{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Internal(
  HC -- Data Type of HashConsed Value
, HashCons -- Name of the class
, hashCons -- Function which creates and returns HC value while updating table
, getValue -- Get the value from HashConsed 
, getTable -- Get the table in which the value resides
, printTable -- Print the table in which the value resides
, Table --Table for Pure verision of Hash cons 
, newEmptyPureHCTable -- Returns a Table
, hashConsPure -- Function which creates and returns HC and updated table
, printTablePure -- Print the values of the Pure table passed
, getKey -- Get the key from HashConsed
) where

import System.IO.Unsafe
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import System.Mem.Weak (Weak, mkWeakPtr, deRefWeak)
import Control.Monad (forM_)
import Control.Concurrent.MVar

-- | Data structure to store Hash Consed data 
data HC a where
  HC :: (Eq a, Hashable a) => {
    value :: a
  , hkey :: Int
  } -> HC a

-- |  Eq logic of HC
instance Eq (HC a) where
    (HC x hkeyX) == (HC y hkeyY) = (hkeyX == hkeyY) && (x == y)
  
-- |  Hash logic of HC based on its value
instance Hashable (HC a) where
    hashWithSalt s (HC x _) = hashWithSalt s x

-- |  Show logic of HC 
instance Show a => Show (HC a) where
    show (HC x key) = "HC: " ++ show x ++ " hkey: " ++ show key

-- |  Get the raw value from the HC
getValue :: HC a -> a
getValue (HC val _) = val

-- |  Get the hash key from the HC
getKey :: HC a -> Int
getKey (HC _ key) = key

{- | 
    Table type which holds weak references of HC 
    as values and the value corresponding to the HC as keys
-}
type HCTable a = MVar (HashMap.HashMap a (Weak (HC a)))

-- |  Create a new empty hash-consing table
newEmptyHCTable :: IO (HCTable a)
newEmptyHCTable = newMVar HashMap.empty

-- |  Function to either insert a new HC value or retrieve an existing one
addOrRetrieve :: Hashable a => a -> HCTable a -> IO (HC a)
addOrRetrieve val tableRef = do
  table <- readMVar tableRef
  -- Attempt to retrieve the value from the table
  case HashMap.lookup val table of
    Just weakHC -> do
      -- Dereference the weak pointer to get the actual HC value
      maybeHC <- deRefWeak weakHC
      case maybeHC of
        Just hc -> return hc
        Nothing -> helper val tableRef  -- If value is GC'ed, insert a new one
    Nothing -> helper val tableRef     -- Value not found, insert a new one
    where
      helper valu hcTableRef = do
        let hcValu = HC valu (hash valu)
        -- Create a weak reference to the value, with a finalizer to remove it from the table when GC'ed
        weakRef <- mkWeakPtr hcValu (Just $ finalizer valu hcTableRef)
        hcTable <- takeMVar hcTableRef
        putMVar hcTableRef (HashMap.insert valu weakRef hcTable)
        return hcValu

-- |  Finalizer to remove a value from the table when it's garbage collected
finalizer :: Hashable a => a -> HCTable a -> IO ()
finalizer key tableRef = do
  table <- takeMVar tableRef
  putMVar tableRef (HashMap.delete key table)

-- |  Typeclass for hash-consable values
class (Eq a, Hashable a) => HashCons a where
  hashConsedTable :: HCTable a
  hashConsedTable = unsafePerformIO newEmptyHCTable
  {-# NOINLINE hashConsedTable #-}


-- |  Function to create or retrieve a hash-consed value
hashCons ::  HashCons a  => a -> HC a
hashCons val = unsafePerformIO $ addOrRetrieve val hashConsedTable

-- |  Retrieve the hash-consing table (not dependent on the given value)
getTable :: HashCons a => HC a -> HCTable a
getTable _ = hashConsedTable

-- |  Print the contents of the hash-consing table
printTable :: Show a => HCTable a -> IO ()
printTable hcTable = do
    putStrLn "Printing Table:"
    table <- readMVar hcTable 
    forM_ (HashMap.toList table) $ \(key, weakVal) -> do
        maybeHC <- deRefWeak weakVal
        case maybeHC of
            Just hConsed -> putStrLn $ "Key: " ++ show key ++ " Value: " ++ show hConsed
            Nothing -> putStrLn $ "Reference not found for Key: " ++ show key

-- | Type alias for a hash consed table, mapping Int hash keys to `HConsed a` values.
type Table a = HashMap.HashMap a (HC a)

-- | Create a new, empty `HashconsedTable`.
newEmptyPureHCTable :: Table a
newEmptyPureHCTable = HashMap.empty

{-|
   Perform a lookup or insert operation on a `HashconsedTable`.

   If `val` is already in the table, return the existing `Hashconsed` value and the same table.
   If `val` is not in the table, insert it, return the new `Hashconsed` value and the modified table.
-}
hashConsPure :: Hashable a => a -> Table a -> (HC a, Table a)
hashConsPure val table =
  case HashMap.lookup val table of
    Just hConsed -> (hConsed, table)  -- Value found, return existing Hashconsed and original table
    Nothing -> 
      let newHC = HC val (hash val)
      in (newHC, HashMap.insert val newHC table)  -- Value not found, insert and return new table

-- |  Print the contents of the hash-consing table
printTablePure :: Show a => Table a -> IO ()
printTablePure hcTable = do
  forM_ (HashMap.toList hcTable) $ \(key, hConsed) -> do
      putStrLn $ "Key: " ++ show key ++ " Value: " ++ show hConsed