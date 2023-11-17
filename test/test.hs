{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, BangPatterns, FlexibleInstances #-}

-- import Internal (HC, HashCons, hashCons, getValue, getTable, Table, newEmptyPureHCTable, hashConsPure)  --, printTablePure )-- 
import Test.HUnit
-- import Test.QuickCheck
import Control.Concurrent.Async
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.MVar
import Data.IORef
import System.Mem
import Data.Hashable
import Data.Bits (xor)
import Control.Concurrent
import HashConsSingleThreaded (HC, HashCons, hashCons, getValue, getTable, printTable, getKey, HashConsSingleThreaded ) 


main :: IO ()
main = do
  _ <- runTestTT unitTests
--   quickCheck prop_hashConsPure
--   quickCheck prop_hashCons
  putStrLn "Testing: "


unitTests :: Test
unitTests = TestList [
    -- "Test hashCons Pure Version" ~: testHashConsPure,
    -- "Test multi-threaded hashCons" ~: testMultiThreadedHashCons,
    "Test hashCons" ~: testHashCons,
    "Test Garbage Collection" ~: testGC,
    "Test Boolean Expression" ~: testHashConsNew
  ]

-- testHashConsPure :: Test
-- testHashConsPure = TestCase $ do
--   let table0 = newEmptyPureHCTable :: Table Int
--       (hc1, table1) = hashConsPure 1 table0
--       (hc2, _) = hashConsPure 2 table1
--   assertEqual "Testing hashCons" 1 (getValue hc1)
--   assertEqual "Testing hashCons" 2 (getValue hc2)

-- testMultiThreadedHashCons :: Test
-- testMultiThreadedHashCons = TestCase $ do
--   let table0 = newEmptyPureHCTable :: Table Int
--   asyncs <- mapM (\n -> async $ return $ hashConsPure n table0) [1..1000]
--   results <- mapM wait asyncs
--   let values = map (getValue . fst) results
--   assertEqual "Testing multi-threaded hashCons" (nub [1..1000]) (nub values)


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
  deriving (Eq, HashConsSingleThreaded, Show)

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
        tableContent <- readIORef table
        assertBool "Testing getTable" (not $ HashMap.null tableContent)
        performMajorGC
    ]

testGC :: Test
testGC = TestCase $ do
    let hc1 = var "z"
        hc2 = var "x"
        !_formula = orr hc1 hc2
        key = Or hc1 hc2

    !table1 <- readIORef (getTable hc1)
    assertBool "Entry should exist before GC" (HashMap.member key table1)

    performMajorGC
    threadDelay 2000
 
    let !table = getTable hc1
    table2 <- readIORef table
    assertBool "Entry should not exist after GC" (not $ HashMap.member key table2)


type Expr = HC Expr'

data Expr' 
    = Const Bool
    | VVar String
    | AAnd Expr Expr
    | OOr Expr Expr
    | Not Expr
    deriving (Show, Eq, HashConsSingleThreaded)

instance Hashable Expr' where
  hashWithSalt val (Const True) = hashWithSalt val True
  hashWithSalt val (Const False) = hashWithSalt val False
  hashWithSalt val (VVar a) = hashWithSalt val a
  hashWithSalt val (AAnd a1 a2) = (hashWithSalt val a1 `xor` hashWithSalt val a2)`xor` hashWithSalt (val + 10003344) a2
  hashWithSalt val (OOr a1 a2) = hashWithSalt val a1 `xor` hashWithSalt val a2
  hashWithSalt val (Not a) = hashWithSalt val a `xor` hashWithSalt (val + 10003344) a

constExpr :: Bool -> Expr
constExpr b = hashCons (Const b)

varExpr :: String -> Expr
varExpr str = hashCons (VVar str)

andExpr :: Expr -> Expr -> Expr
andExpr v1 v2 = hashCons (AAnd v1 v2)

orExpr :: Expr -> Expr -> Expr
orExpr v1 v2 = hashCons (OOr v1 v2)

notExpr :: Expr -> Expr
notExpr v1 = hashCons (Not v1)

testHashConsNew :: Test
testHashConsNew = TestList
    [ "test hashCons" ~: do
        let formula1 = varExpr "a"
        assertEqual "Testing hashCons" (VVar "a") (getValue formula1)
        performMajorGC
    , "test getTable" ~: do
        let !hc1 = varExpr "b"
            table = getTable hc1
        tableContent <- readIORef table
        assertBool "Testing getTable" (not $ HashMap.null tableContent)
        performMajorGC
    ]

testingBoolExpr :: Test
testingBoolExpr = TestList
    [ "test hashCons" ~: do
        let !expr1 = constExpr True
        let !expr2 = constExpr False
        let !expr3 = varExpr "x"
        let !expr4 = varExpr "y"
        let !expr5 = notExpr expr1         -- NOT True
        let !expr6 = notExpr expr3         -- NOT x
        let !expr7 = andExpr expr3 expr4   -- x AND y
        let !expr8 = orExpr expr3 expr4    -- x OR y
        let !expr9 = andExpr expr3 (notExpr expr4)  -- x AND (NOT y)
        let !expr10 = orExpr (notExpr expr3) expr4  -- (NOT x) OR y
        let !expr11 = orExpr expr5 (andExpr expr7 expr8)   -- (NOT True) OR ((x AND y) AND (x OR y))
        let !expr12 = andExpr (orExpr expr3 expr4) (notExpr expr2) -- (x OR y) AND (NOT False)
        let !varList = map varExpr ["a", "b", "c", "d", "e"]
        let !combinedExpr = foldl1 andExpr varList  -- a AND b AND c AND d AND e
        let !expr13 = andExpr (varExpr "a") (orExpr (varExpr "a") (notExpr (varExpr "a"))) -- a AND (a OR (NOT a))





