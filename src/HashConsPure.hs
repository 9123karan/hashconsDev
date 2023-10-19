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
import Internal(HC, getValue, Table, newEmptyPureHCTable, hashConsPure, printTablePure, remove)

newTable :: Table a
newTable = newEmptyPureHCTable

hashCons :: Hashable a => a -> Table a -> (HC a, Table a)
hashCons = hashConsPure

printTable :: Show a => Table a -> IO ()
printTable = printTablePure