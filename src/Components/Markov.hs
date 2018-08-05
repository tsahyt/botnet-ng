{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Components.Markov
    ( markov
    ) where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.Random hiding (fromList)
import Control.Monad.Reader
import Data.Char
import Data.Config
import Data.Distribution hiding (toList)
import Data.List (group, groupBy, sort, sortBy, tails)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import Network.Voco
import Network.Yak.Client
import Network.Yak.Types hiding (render)
import System.Directory
import System.FilePath

import qualified Data.Function as F
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

markov ::
       (MonadIO m, MonadChan m, MonadReader r m, MonadRandom m, HasConfig r)
    => Bot m Privmsg ()
markov =
    answeringP $ \src -> do
        path <- reader (markovRoot . paths . view config)
        chains <- loadChains path
        asum $ map (uncurry (chainBot src)) chains

chainBot ::
       (MonadChan m, MonadRandom m)
    => Either Channel Nickname
    -> Text
    -> MarkovMap Text
    -> Bot m Message ()
chainBot src cmd mch =
    filterB (== Message (T.cons ':' cmd)) $ do
        start <- randomStart mch
        sents <- getRandomR (2, 3)
        msg <-
            render . takeUntilN (== ".") sents . drop 1 . dropWhile (/= ".") <$>
            runChain 500 start mch
        message' src (Message msg)

loadChains :: MonadIO m => FilePath -> m [(Text, MarkovMap Text)]
loadChains path = do
    files <- liftIO $ listDirectory path
    mapM (load . (path </>)) (sortBy (comparing $ Down . length) files)
  where
    load file = do
        let name = T.pack . dropExtension . takeFileName $ file
        content <- liftIO $ tokenize <$> T.readFile file
        pure (name, buildChain 2 content)

newtype History a = History
    { unHist :: [a]
    } deriving (Eq, Show, Ord)

newtype MarkovMap a = MarkovMap
    { unMM :: Map (History a) (Distribution a)
    } deriving (Eq, Show, Ord)

runChain ::
       forall a m. (Ord a, Show a, MonadRandom m)
    => Int
    -> [a]
    -> MarkovMap a
    -> m [a]
runChain l start (MarkovMap m) =
    if M.member (History start) m
        then fst <$> go ([], (0, History start))
        else return []
  where
    go :: ([a], (Int, History a)) -> m ([a], (Int, History a))
    go (xs, (n, hist))
        | n == l = return (xs, (n, hist))
        | otherwise = do
            let cs =
                    fromMaybe (error $ "Key not in chain " ++ show hist) $
                    M.lookup hist m
            r <- getSample . fromDistribution $ cs
            let History (h:hs) = hist
            go (xs ++ [h], (n + length hs, History $ hs ++ [r]))

randomStart :: MonadRandom m => MarkovMap a -> m [a]
randomStart (MarkovMap m) =
    unHist . (M.keys m !!) <$> getRandomR (0, pred $ M.size m)

separators :: [Char]
separators = ":;,."

tokenize :: Text -> [Text]
tokenize = concatMap (T.foldr go []) . T.words
  where
    go x []
        | x `elem` separators = ["", T.singleton x]
        | otherwise = [T.singleton x]
    go x xs
        | x `elem` separators = "" : T.singleton x : xs
        | otherwise =
            let (y:ys) = xs
            in (T.cons x y) : ys

render :: [Text] -> Text
render = T.dropWhile isSpace . foldr go T.empty
  where
    go x xs
        | x `elem` seps = x <> xs
        | otherwise = " " <> x <> xs
    seps = map T.singleton separators

takeUntilN :: (a -> Bool) -> Int -> [a] -> [a]
takeUntilN _ 0 _ = []
takeUntilN p n x =
    case break p x of
        (l, h:r) -> l ++ h : takeUntilN p (pred n) r
        (l, _) -> l

buildChain ::
       forall a. (Ord a)
    => Int
    -> [a]
    -> MarkovMap a
buildChain n tokens =
    let ngrams =
            map (take (n + 1)) . take (length tokens) . tails . cycle $ tokens
        ndist =
            map (History . fst . head &&& fromList . groupOccurrences) .
            groupBy ((==) `F.on` fst) .
            sortBy (comparing fst) . map (init &&& last) $
            ngrams
    in MarkovMap . M.fromList $ ndist
  where
    groupOccurrences = percentage . group . sort . map snd
    percentage :: [[c]] -> [(c, Probability)]
    percentage xs =
        let total = fromIntegral . length . concat $ xs
        in map (head &&& (/ total) . fromIntegral . length) xs
