{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.DeepSeq (NFData)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Foldable (foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy(Proxy))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Gauge
import Prelude hiding (lookup)

newtype Element = Element Int
  deriving (NFData)

class Environment env where
  name :: String
  empty :: env
  extend :: Element -> env -> env
  lookup :: env -> Int -> Element

instance Environment [Element] where
  name = "Data.List"
  empty = mempty
  extend = (:)
  lookup = (List.!!)

instance Environment (Vector Element) where
  name = "Data.Vector"
  empty = mempty
  extend = Vector.cons
  lookup = (Vector.!)

instance Environment (Map Int Element) where
  name = "Data.Map"
  empty = mempty
  extend x m = Map.insert (Map.size m) x m
  lookup m i = m Map.! (Map.size m - i - 1)

instance Environment (Int, IntMap Element) where
  name = "Data.IntMap"
  empty = (0, mempty)
  extend x (!size, !m) = (1 + size, IntMap.insert size x m)
  lookup (size, m) i = m IntMap.! (size - i - 1)

instance Environment (Int, HashMap Int Element) where
  name = "Data.HashMap.Lazy"
  empty = (0, mempty)
  extend x (!size, !m) = (1 + size, HashMap.insert size x m)
  lookup (size, m) i = m HashMap.! (size - i - 1)

instance Environment (Seq Element) where
  name = "Data.Sequence"
  empty = mempty
  extend = (Seq.<|)
  lookup = Seq.index

fromList :: (Environment env) => [Element] -> env
fromList = foldl' (flip extend) empty

withEnvironmentTypes
  :: forall result
  . (forall env. (Environment env, NFData env) => Proxy env -> result)
  -> [result]
withEnvironmentTypes k =
  [ k (Proxy :: Proxy [Element])
  , k (Proxy :: Proxy (Vector Element))
  , k (Proxy :: Proxy (Map Int Element))
  , k (Proxy :: Proxy (Int, IntMap Element))
  , k (Proxy :: Proxy (Int, HashMap Int Element))
  , k (Proxy :: Proxy (Seq Element))
  ]

iterRange :: Int -> Int -> (Int -> a -> a) -> a -> a
iterRange from to f a = go from to a where
  go from' to' a' | from' >= to' = a'
  go from' to' a' = let !a'' = f from' a' in go (from' + 1) to' a''
{-# inline iterRange #-}


combinedBench :: forall env. (Environment env) => Int -> Proxy env -> Gauge.Benchmark
combinedBench size _ =
  Gauge.bench (name @env) $
    Gauge.whnf
      (\size' ->
        let env :: env = iterRange 0 size' (\_ -> extend (Element 41)) empty
        in
        iterRange 0 (size - 1) (\i acc -> lookup env i `seq` acc) ()
      )
      size

extensionBench :: forall env. (Environment env) => Int -> Proxy env -> Gauge.Benchmark
extensionBench size _ =
  Gauge.bench (name @env) $ Gauge.whnf
    (\size' -> iterRange 0 size' (\_ -> extend (Element 41)) (empty :: env))
    size

lookupBench :: forall env. (Environment env, NFData env) => Int -> Proxy env -> Gauge.Benchmark
lookupBench size _ =
  Gauge.env (pure $ fromList @env $ replicate size $ Element 41) $ \env ->
    Gauge.bench (name @env) $
      Gauge.whnf (\size' -> iterRange 0 (size' - 1) (\i acc -> lookup env i `seq` acc) ())
      size


main :: IO ()
main =
  Gauge.defaultMain
    [ Gauge.bgroup "combined"
      [Gauge.bgroup (show n) $ withEnvironmentTypes $ combinedBench n | n <- sizes]
    , Gauge.bgroup "extension"
      [Gauge.bgroup (show n) $ withEnvironmentTypes $ extensionBench n | n <- sizes]
    , Gauge.bgroup "lookup"
      [Gauge.bgroup (show n) $ withEnvironmentTypes $ lookupBench n | n <- sizes]
    ]
  where
    sizes =
      [1, 3, 5, 7, 10, 15, 20]
