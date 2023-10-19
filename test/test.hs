{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, BangPatterns, FlexibleInstances #-}

import Internal (HC, HashCons, hashCons, getValue, getTable, Table, newEmptyPureHCTable, hashConsPure)  --, printTablePure )-- 
import Test.HUnit
-- import Test.QuickCheck
import Control.Concurrent.Async
import Data.List (nub)
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.MVar
import System.Mem
import Data.Hashable
import Data.Bits (xor)
import Control.Concurrent


main :: IO ()
main = do
  _ <- runTestTT unitTests
--   quickCheck prop_hashConsPure
--   quickCheck prop_hashCons
  putStrLn "Testing: "


unitTests :: Test
unitTests = TestList [
    "Test hashCons Pure Version" ~: testHashConsPure,
    "Test multi-threaded hashCons" ~: testMultiThreadedHashCons,
    "Test hashCons" ~: testHashCons,
    "Test Garbage Collection" ~: testGC
  ]

testHashConsPure :: Test
testHashConsPure = TestCase $ do
  let table0 = newEmptyPureHCTable :: Table Int
      (hc1, table1) = hashConsPure 1 table0
      (hc2, _) = hashConsPure 2 table1
  assertEqual "Testing hashCons" 1 (getValue hc1)
  assertEqual "Testing hashCons" 2 (getValue hc2)

testMultiThreadedHashCons :: Test
testMultiThreadedHashCons = TestCase $ do
  let table0 = newEmptyPureHCTable :: Table Int
  asyncs <- mapM (\n -> async $ return $ hashConsPure n table0) [1..1000]
  results <- mapM wait asyncs
  let values = map (getValue . fst) results
  assertEqual "Testing multi-threaded hashCons" (nub [1..1000]) (nub values)


-- prop_hashConsPure :: Property
-- prop_hashConsPure = forAll (listOf arbitrary) $ \(xs :: [Int]) ->
--   let table0 = newEmptyHCTable :: HCTable Int
--       inserts = foldl (\(tbl, _) x -> hashCons x tbl) (table0, undefined) xs
--       finalTable = snd inserts
--       tableSize = HashMap.size finalTable
--   in tableSize == length (nub xs)


type BoolFormula = HC BoolFormula'

data BoolFormula' = 
    Var String 
  | And BoolFormula BoolFormula
  | Or BoolFormula BoolFormula
  deriving (Eq, HashCons, Show)

instance Hashable BoolFormula' where
  hashWithSalt val (Var a) = hashWithSalt val a
  hashWithSalt val (And a1 a2) = (hashWithSalt val a1 `xor` hashWithSalt val a2)`xor` hashWithSalt (val + 10003344) a2
  hashWithSalt val (Or a1 a2) = hashWithSalt val a1 `xor` hashWithSalt val a2

var :: String -> BoolFormula
var str = hashCons (Var str)

_andd :: BoolFormula -> BoolFormula -> BoolFormula
_andd v1 v2 = hashCons (And v1 v2)

orr :: BoolFormula -> BoolFormula -> BoolFormula
orr v1 v2 = hashCons (Or v1 v2)

testHashCons :: Test
testHashCons = TestList
    [ "test hashCons" ~: do
        let formula1 = var "a"
        assertEqual "Testing hashCons" (Var "a") (getValue formula1)
        performMajorGC
    , "test getTable" ~: do
        let !hc1 = var "b"
            table = getTable hc1
        tableContent <- readMVar table
        assertBool "Testing getTable" (not $ HashMap.null tableContent)
        performMajorGC
    ]

testGC :: Test
testGC = TestCase $ do
    let hc1 = var "z"
        hc2 = var "x"
        !_formula = orr hc1 hc2
        key = Or hc1 hc2

    !table1 <- readMVar (getTable hc1)
    assertBool "Entry should exist before GC" (HashMap.member key table1)

    performMajorGC
    threadDelay 2000
 
    let !table = getTable hc1
    table2 <- readMVar table
    assertBool "Entry should not exist after GC" (not $ HashMap.member key table2)
