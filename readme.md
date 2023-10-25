# Type-Safe Modular Hash-Consing

The Haskell Hash-Consing Library provides efficient data structures and operations for hash-consing, allowing users to reuse common data structures while preserving maximum sharing. This technique can save memory and speed up equality comparisons.

## Features

- **Hash-Consed Data Structure (`HC`)**
  - A custom data type for hash-consed values.
  
- **Weak Reference Based Table (Using `HashCons` Module)**
  - Allows for potential garbage collection of unused values, freeing up memory.
  
- **Pure Version of Hash-Consing Table (Using `HashConsPure` Module)**
  - An alternative to the weak reference-based approach without garbage collection. Users need to manually maintain the table and ensure values are present in it before utilizing them.
  
- **Extensive Utility Functions**
  - Create, retrieve, and manage hash-consed values and their tables.

## Differences between `HashCons` and `HashConsPure`

Users should be aware of the key differences and the recommended usage patterns of `HashCons` and `HashConsPure`:

- **`HashCons`**: It is a derivable class and it is recommended to use constructors (as shown in the example below) for easy readability and maintainability of the code.

- **`HashConsPure`**: This is more of a table containing wrapped lookup of values. Users need to ensure values are present in it before utilizing hash-consed values. It's more manual in nature, giving users a granular control over hash-consing, at the cost of convenience.

## Installation

To use the weak reference-based version of this library, import the `HashCons` module. For the pure version, import the `HashConsPure` module.


## Quick Start

This section aims to offer users a clearer distinction between the two modules and how they are intended to be used.

### 1. Weak Reference Based Version

```haskell
import HashCons as hc

type Expr = hc.HC Expr'
data Expr' = Lit Int | Add Expr Expr deriving (Eq, Hashable, HashCons)

-- Constructors
lit :: Int -> Expr
lit val = hc.hashcons (Lit val)

add :: Expr -> Expr -> Expr
add exprL exprR = hc.hashcons (Add exprL exprR)

-- Hash-cons the expressions
hcExpr1 = add (lit 2) (lit 5)
hcExpr2 = add (lit 2) (lit 5)

-- Efficiently check for equality
print (hcExpr1 == hcExpr2)  -- True
```

### 2. Pure Version

For the pure version, users need to maintain the hash-consing table and ensure that values are present in it before utilizing hash-consed values. If a value is not present in the table, it will not be hash-consed.

```haskell
import HashConsPure as hcp

type Expr = hcp.HC Expr'
data Expr' = Lit Int | Add Expr Expr deriving (Eq, Hashable)

-- Creating a new table
myTable = hcp.newTable

-- Hash-cons the expressions
(hcExpr1, updatedTable) = hcp.hashCons (Lit 2) myTable
(hcExpr2, updatedTable) = hcp.hashCons (Lit 5) updatedTable
(hcExpr3, updatedTable) = hcp.hashCons (Add hcExpr1 hcExpr2) updatedTable
(hcExpr4, updatedTable) = hcp.hashCons (Add hcExpr1 hcExpr2) updatedTable

-- Efficiently check for equality
print (hcExpr3 == hcExpr4)  -- True
```

#### Caveat:
In the pure version, the table just keeps growing even if certain values are no longer needed in the program. If users are certain that a value is no longer required, they can remove it with:

```haskell
newTable = hcp.remove val updatedTable
```

Be sure to hash-cons the value again using `hashCons` before performing any lookups or using the hash-consed value.

## Documentation

### Functions (For `HashCons` Module)

- `hashCons :: HashCons a => a -> HC a`: 
  - Hash-cons a value. If the value already exists in the table, returns the existing hash-consed value.
  
- `getValue :: HC a -> a`: 
  - Extract the actual value from a hash-consed value.
  
### Functions (For `HashConsPure` Module)

- `newTable :: Table a`: 
  - Create a new empty table for the pure version.

- `getValue :: HC a -> a`: 
  - Extract the actual value from a hash-consed value.

- `getKey :: HC a -> int`: 
  - Extract the hash key from a hash-consed value.
  
- `hashCons :: Hashable a => a -> Table a -> (HC a, Table a)`: 
  - Hash-cons a value using the pure version of the table.

- `remove :: Hashable a => a -> Table a -> Table a`:
  - Remove a value from the pure hash-consing table.
  
- `printTable :: Show a => Table a -> IO ()`: 
  - Print the contents of a hash-consing table for the pure version.

## FAQ

- **What is hash-consing?**
  
  Hash-consing is a technique to maximize structural sharing by ensuring that identical data structures share memory.

- **Why use weak references?**
  
  Weak references allow unused hash-consed values to be garbage collected, freeing up memory.

## Contributing

Feel free to open issues or submit pull requests if you find bugs or have suggestions for improvements.

## License

This library is provided under the BSD 3-Clause License. See `LICENSE` for details.
