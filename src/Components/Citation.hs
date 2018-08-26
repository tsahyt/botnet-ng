{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Components.Citation
    ( Citations
    , citations
    ) where

import Control.Lens (view)
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.Reader
import Data.Array
import Data.Config (HasConfig(..), citationRoot, paths)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..), comparing)
import Data.Semigroup
import Data.Text (Text)
import Data.Vector (Vector)
import Network.Voco
import Network.Yak.Client
import Network.Yak.Types (Channel, Message(..), Nickname)
import System.Directory
import System.FilePath
import Text.Printf

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

-- | Determine the edit distance between two 'Text's
similarity :: Text -> Text -> Int
similarity xs ys = minimum [table ! (m, i) | i <- [0 .. n]]
  where
    (m, n) = (T.length xs, T.length ys)
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds = ((0, 0), (m, n))
    dist (0, _) = 0
    dist (i, 0) = i
    dist (i, j) =
        minimum
            [ table ! (i - 1, j) + 1
            , table ! (i, j - 1) + 1
            , if xs `T.index` (pred i) == ys `T.index` (pred j)
                  then table ! (i - 1, j - 1)
                  else 1 + table ! (i - 1, j - 1)
            ]

search :: Text -> Vector Text -> Maybe Int
search pat xs
    | V.null xs = Nothing
    | otherwise = Just . V.minIndexBy (comparing (similarity pat)) $ xs

{-# INLINE search #-}
newtype Citations =
    Citations [(Text, Vector Text)]
    deriving (Semigroup, Monoid)

loadCites :: MonadIO m => FilePath -> m Citations
loadCites path = do
    files <- liftIO $ listDirectory path
    Citations <$>
        mapM (load . (path </>)) (sortBy (comparing $ Down . length) files)
  where
    load file = do
        let name = dropExtension . takeFileName $ file
        content <- V.fromList . T.lines <$> liftIO (T.readFile file)
        pure (T.pack name, content)

citations ::
       ( MonadChan m
       , MonadRandom m
       , MonadReader r m
       , HasConfig r
       , MonadIO m
       )
    => Bot m Privmsg ()
citations =
    answeringP $ \src -> do
        path <- reader (citationRoot . paths . view config)
        Citations cs <- loadCites path
        allCs src cs <|> (asum $ map (uncurry (cite src)) cs)
  where
    allCs src cs =
        let rs = T.unwords $ map (T.cons ':' . fst) cs
        in filterB (== ":cites") . message' src . Message $
           "Available cites commands are " <> rs

data QuoteCmd
    = Random
    | Numbered Int
    | Search Text

qcmd :: Text -> A.Parser QuoteCmd
qcmd cmd = A.string (T.cons ':' cmd) *> go
  where
    go =
        A.choice
            [ (Numbered <$> (A.space *> A.signed A.decimal))
            , (Search <$> (A.space *> A.takeText))
            , pure Random
            ]

-- | A bot that prints out a citation, given some source (to print it back out
-- to), a name for the citation category, and the data contained in said
-- category.
cite ::
       (MonadChan m, MonadRandom m)
    => Either Channel Nickname
    -> Text
    -> Vector Text
    -> Bot m Message ()
cite src cat content =
    parsedMsg (qcmd cat) $ do
        let n = V.length content
        idx <-
            query >>= \case
                Random -> getRandomR (0, pred n)
                Numbered i -> pure (i `mod` n)
                Search t -> pure . fromMaybe 0 . search t $ content
        let c = T.pack $ printf "[%d/%d] %s" idx n (content V.! idx)
        message' src (Message c)
