module LibPure(
  HC
, hashCons
, newEmptyHCTable
, getValue
, printTable
, Table
) where

import Data.Hashable
import Internal(HC, getValue, Table, newEmptyPureHCTable, hashConsPure, printTablePure)

newEmptyHCTable :: Table a
newEmptyHCTable = newEmptyPureHCTable

hashCons :: Hashable a => a -> Table a -> (HC a, Table a)
hashCons = hashConsPure

printTable :: Show a => Table a -> IO ()
printTable = printTablePure