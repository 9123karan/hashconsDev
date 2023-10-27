module HashConsPure(
  HC
, hashCons
, newTable
, getValue
, printTable
, Table
, remove
) where

import Data.Hashable
-- Import the required functions from the internal module
import Internal(HC, getValue, Table, newEmptyPureHCTable, hashConsPure, printTablePure, remove)

-- | Create a new, empty `Table`.
newTable :: Table a
newTable = newEmptyPureHCTable

{-|
   Perform a lookup or insert operation on a `Table`.

   If `val` is already in the table, return the existing `HC` value and the same table.
   If `val` is not in the table, insert it, return the new `HC` value and the modified table.
-}
hashCons :: Hashable a => a -> Table a -> (HC a, Table a)
hashCons = hashConsPure

-- |  Print the contents of the hash-consing table
printTable :: Show a => Table a -> IO ()
printTable = printTablePure