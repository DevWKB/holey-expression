{-|
Module      : NatMap
Description : Map with natural number keys
Copyright   : (c) Harley Eades, 2026
              (c) W⋊B, 2026
Maintainer  : harley.eades@gmail.com

This is a simple wrapper around `Data.IntMap.Lazy.IntMap` restricting the keys to the
natural numbers.
-}
module  Data.NatMap (-- * Map type                     
                     NatMap
                     -- * Natural numbers
                    ,Natural
                    ,naturalToInt
                    ,intToNatural
                    -- * Construction
                    ,empty
                    ,singleton
                    -- ** From unordered lists
                    ,fromList
                    -- * Insertion
                    ,insert
                    -- * Deletion and updating
                    ,delete
                    -- * Query
                    -- ** Lookup
                    ,(!?)
                    ,(!)
                    -- ** Size
                    ,null
                    ,size
                    -- * Combine
                    -- ** Union
                    ,union
                    -- * Traversal
                    -- ** Map
                    ,Data.NatMap.map
                    -- * Conversion        
                    ,keys) where

import GHC.Natural      (Natural)
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as M
import Prelude          hiding (null)

-- | A map of natural numbers to values `f`.
type NatMap f = IntMap f

-- | Convert a natural number into an integer (`Int`).
naturalToInt :: Natural -> Int
naturalToInt = fromInteger . toInteger

-- | Convert an integer (`Int`) into a natural number.
intToNatural :: Int -> Natural
intToNatural = fromInteger . toInteger

-- | Insert a new key/value pair in the map. If the key is already present in
-- the map, the associated value is replaced with the supplied value. See
-- `Data.IntMap.Lazy.insert`.
insert :: Natural -> f -> NatMap f -> NatMap f
insert (naturalToInt->k) = M.insert k

-- | Find the value at a key. Returns Nothing when the element can not be found.
-- See `(Data.IntMap.Lazy.!?)`.
(!?) :: NatMap f -> Natural -> Maybe f
m !? (naturalToInt->k) =  m M.!? k

-- | Find the value at a key. Calls error when the element can not be found. See
-- `(Data.IntMap.Lazy.!)`.
(!) :: NatMap f -> Natural -> f
m ! (naturalToInt->k) = m M.! k

-- | The empty map. 
-- See `Data.IntMap.Lazy.empty`.
empty :: NatMap f
empty = M.empty

-- | Is the map empty? 
-- See `Data.IntMap.Lazy.null`.
null :: NatMap f -> Bool
null = M.null

-- | A map of one element. See `Data.IntMap.Lazy.singleton`.
singleton :: Natural -> f -> NatMap f
singleton (naturalToInt->k)= M.singleton k

-- | Delete a key and its value from the map. When the key is not a member of
-- the map, the original map is returned. See `Data.IntMap.Lazy.delete`.
delete :: Natural -> NatMap f -> NatMap f
delete (naturalToInt->k) = M.delete k

-- | Return all keys of the map in ascending order. 
keys :: NatMap f -> [Natural]
keys = M.foldrWithKey (\k _ r -> intToNatural k : r) []

-- | Create a map from a list of key/value pairs.
fromList :: [(Natural, a)] -> IntMap a
fromList = M.fromList . Prelude.map (\(k,v) -> (naturalToInt k,v))

-- | The (left-biased) union of two maps. It prefers the first map when
-- duplicate keys are encountered.
-- See `Data.IntMap.Lazy.union`.
union :: NatMap a -> NatMap a -> NatMap a
union = M.union

-- | Map a function over all values in the map.
map :: (f1 -> f2) -> IntMap f1 -> IntMap f2
map = M.map

-- | Number of elements in the map.
-- See `Data.IntMap.Lazy.size`.
size :: NatMap f -> Int
size = M.size