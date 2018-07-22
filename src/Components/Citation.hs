{-# LANGUAGE LambdaCase #-}

module Components.Citation
    ( citations
    ) where

import Control.Monad.IO.Class
import Control.Monad.Random
import Data.Array
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
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
loadCites :: MonadIO m => FilePath -> m [(Text, Vector Text)]
loadCites path = do
    files <- liftIO $ listDirectory path
    mapM (load . (path </>)) files
  where
    load file = do
        let name = dropExtension . takeFileName $ file
        content <- V.fromList . T.lines <$> liftIO (T.readFile file)
        pure (T.pack name, content)

citations ::
       (MonadChan m, MonadRandom m, MonadIO m) => FilePath -> Bot m Privmsg ()
citations path =
    answeringP $ \src -> do
        cs <- loadCites path
        asum $ map (uncurry (cite src)) cs

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
