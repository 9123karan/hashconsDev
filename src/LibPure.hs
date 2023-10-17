{-# LANGUAGE GADTs #-}

module LibPure(
  HC
, hashCons
, newEmptyHCTable
, getValue
, printTable
, HCTable
) where

import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (forM_)

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}
-- | data instead of newtype to force evaluation for value 
data HC a where
  HC :: (Eq a, Hashable a) => {
    value :: !a
  } -> HC a

-- |  Eq logic of HC
instance Eq (HC a) where
    (HC x) == (HC y) = x == y
  
-- |  Hash logic of HC based on its value
instance Hashable (HC a) where
    hashWithSalt s (HC x) = hashWithSalt s x

-- |  Show logic of HC 
instance Show a => Show (HC a) where
    show (HC x) = "HC: " ++ show x

-- |  Get the raw value from the HC
getValue :: HC a -> a
getValue (HC val) = val

-- | Type alias for a hash consed table, mapping Int hash keys to `HConsed a` values.
type HCTable a = HashMap.HashMap a (HC a)

-- | Create a new, empty `HashconsedTable`.
newEmptyHCTable :: HCTable a
newEmptyHCTable = HashMap.empty

{-|
   Perform a lookup or insert operation on a `HashconsedTable`.

   If `val` is already in the table, return the existing `Hashconsed` value and the same table.
   If `val` is not in the table, insert it, return the new `Hashconsed` value and the modified table.
-}
hashCons :: Hashable a => a -> HCTable a -> (HC a, HCTable a)
hashCons val table =
  case HashMap.lookup val table of
    Just hConsed -> (hConsed, table)  -- Value found, return existing Hashconsed and original table
    Nothing -> 
      let newHC = HC val
      in (newHC, HashMap.insert val newHC table)  -- Value not found, insert and return new table

-- |  Print the contents of the hash-consing table
printTable :: Show a => HCTable a -> IO ()
printTable hcTable = do
  forM_ (HashMap.toList hcTable) $ \(key, hConsed) -> do
      putStrLn $ "Key: " ++ show key ++ " Value: " ++ show hConsed