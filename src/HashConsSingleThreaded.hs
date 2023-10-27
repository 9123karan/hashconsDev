{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module HashConsSingleThreaded(
  HC -- Data Type of HashConsed Value
, HashCons -- Name of the class
, hashCons -- Function which creates and returns HC value while updating table
, getValue -- Get the value from HashConsed 
, getTable -- Get the table in which the value resides
, printTable -- Print the table in which the value resides
, getKey
, HashConsSingleThreaded
) where

import System.IO.Unsafe
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import System.Mem.Weak (Weak, mkWeakPtr, deRefWeak)
import Control.Monad (forM_)
import Data.IORef
import Internal (HC, HashCons, getValue, newHC, getKey)


{- | 
    Table type which holds weak references of HC 
    as values and the value corresponding to the HC as keys
-}
type HCTable a = IORef (HashMap.HashMap a (Weak (HC a)))

-- |  Create a new empty hash-consing table
newEmptyHCTable :: IO (HCTable a)
newEmptyHCTable = newIORef HashMap.empty

-- |  Function to either insert a new HC value or retrieve an existing one
addOrRetrieve :: Hashable a => a -> HCTable a -> IO (HC a)
addOrRetrieve val tableRef = do
  table <- readIORef tableRef
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
        let hcValu = newHC valu (hash valu)
        -- Create a weak reference to the value, with a finalizer to remove it from the table when GC'ed
        weakRef <- mkWeakPtr hcValu (Just $ finalizer valu hcTableRef)
        hcTable <- readIORef hcTableRef
        writeIORef hcTableRef (HashMap.insert valu weakRef hcTable)
        return hcValu

-- |  Finalizer to remove a value from the table when it's garbage collected
finalizer :: Hashable a => a -> HCTable a -> IO ()
finalizer key tableRef = do
  table <- readIORef tableRef
  writeIORef tableRef (HashMap.delete key table)

-- |  Typeclass for hash-consable values
class (Eq a, Hashable a) => HashConsSingleThreaded a where
  hashConsedTable :: HCTable a
  hashConsedTable = unsafePerformIO newEmptyHCTable
  {-# NOINLINE hashConsedTable #-}


-- |  Function to create or retrieve a hash-consed value
hashCons ::  HashConsSingleThreaded a  => a -> HC a
hashCons val = unsafePerformIO $ addOrRetrieve val hashConsedTable

-- |  Retrieve the hash-consing table (not dependent on the given value)
getTable :: HashConsSingleThreaded a => HC a -> HCTable a
getTable _ = hashConsedTable

-- |  Print the contents of the hash-consing table
printTable :: Show a => HCTable a -> IO ()
printTable hcTable = do
    putStrLn "Printing Table:"
    table <- readIORef hcTable 
    forM_ (HashMap.toList table) $ \(key, weakVal) -> do
        maybeHC <- deRefWeak weakVal
        case maybeHC of
            Just hConsed -> putStrLn $ "Key: " ++ show key ++ " Value: " ++ show hConsed
            Nothing -> putStrLn $ "Reference not found for Key: " ++ show key
