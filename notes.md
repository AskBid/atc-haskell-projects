# Notes to self.

## Study on smaller diagonals.

How to get the diagonals that are smaller than the Board lenght?

```
[0 0, O 1, 0 2, 0 3]
[1 0, 1 1, 1 2, 1 3]
[2 0, 2 1, 2 2, 2 3]
[3 0, 3 1, 3 2, 3 3]

[[rows !! i !! n+i, rows !! i+n !! i, rows !! i !! len-i, rows !! i+n !! len-i-n] | i <- [0..len]]
--
-- n = 0
-- len = 3      <-- (length - 1)
--
-- [0 0, 1 1, 2 2, 3 3]              0 1 2 3    0 1 2 3                                 <-- a0 dir
--                                   i          i+n                                     <-- a0 dir
-- [0 0, 1 1, 2 2, 3 3]              0 1 2 3    0 1 2 3                                 <-- a0 dir
--                                   i+n        i                                       <-- a0 dir
--
-- [0 3, 1 2, 2 1, 3 0]              0 1 2 3    3 2 1 0                                 <-- b0 dir
--                                   i          len-i                                   <-- b0 dir
-- [0 3, 1 2, 2 1, 3 0]              0 1 2 3    3 2 1 0                                 <-- b0 dir
--                                   i+n        len-i+n                                 <-- b0 dir
--
--
-- n = 1
-- len = 2
--
-- [0 1, 1 2, 2 3]                   0 1 2      1 2 3                                   <-- aUp side
--                                   i          i+n                                     <-- aUp side
-- [1 0, 2 1, 3 2]                   1 2 3      0 1 2                                   <-- aDw side
--                                   i+n        i                                       <-- aDw side
--
-- [0 2, 1 1, 2 0]                   0 1 2      2 1 0                                   <-- bUp side
--                                   i          len-i                                   <-- bUp side
-- [1 3, 2 2, 3 1]                   1 2 3      3 2 1                                   <-- bDw side
--                                   i+n        len-i+n                                 <-- bDw side
--                                   
--
-- n = 2
-- len = 1
--
-- [0 2, 1 3]                        0 1        2 3                                     <-- aUp side
--                                   i          i+n                                     <-- aUp side 
-- [2 0, 3 1]                        2 3        0 1                                     <-- aDw side
--                                   i+n        i                                       <-- aDw side
--
-- [0 1, 1 0]                        0 1        1 0                                     <-- bUp side
--                                   i          len-i                                   <-- bUp side 
-- [2 3, 3 2]                        2 3        3 2                                     <-- bDw side
--                                   i+n        len-i+n                                 <-- bDw side
--
--
-- n = 3
-- len = 0
--
-- [0 3]                             0          3                                       <-- aUp side
--                                   i          i+n                                     <-- aUp side 
-- [3 0]                             3          0                                       <-- aDw side
--                                   i+n        i                                       <-- aDw side
--
-- [0 0]                             0          0                                       <-- bUp side
--                                   i          len-i                                   <-- bUp side
-- [3 3]                             3          3                                       <-- bDw side
--                                   i+n        len-i+n                                 <-- bDw side
```

## GHCi

`:load` interprets the files loading them into ghci.. but it doesn't place them in the namespace.. although if you launch `:load` during ghci, that files names will be available as the scope of the module is set now on that file.. but they are not in the main upper level namespace.. I know confusing.. but it works like that.. need more understanding on different ghci namespaces.

### GHCi Scopes

In GHCi, “scope” refers to the set of names (functions, types, variables, etc.) that are directly accessible in the interactive prompt without qualification (e.g., mkBoard vs. Board.mkBoard). 

GHCi manages scopes through its module system, and there are several layers of scoping to consider: 

1. Namespace. (top-level scope) 
2. Module context. (current module)
3. Loaded modules.

#### NAMESPACE

The set of names (functions, types, etc.) directly accessible in the GHCi prompt without module qualification (i.e. `Board.mkBoard` can be called just with `mkBoard`)

Only names in the namespace can be used directly (e.g., mkBoard). If a module is loaded but not imported, you must qualify its names (e.g., Board.mkBoard).

- `:import <Module>` or `:module +<Module>` to add a module’s exported symbols.
- `:module -<Module>` removes a module’s symbols.
- `:module` (without arguments) clears the namespace.

#### MODULE CONTEXT (Current Module)

The module that GHCi treats as the “current” module, whose exported symbols are implicitly (tacitly) in the namespace, as if you were typing inside that module’s scope.

When a module is the current context, its exported symbols are in the namespace without needing `:import` or `:m +`.
Typically `cabal repl` typically sets the context to Main (or no specific module).

- `:load <FilePath>` makes the loaded module (or the last one if multiple) the current context.
- `:module *<Module>` explicitly sets the current module.

#### LOADED MODULES

The set of modules that GHCi has compiled and loaded into memory, making them available for imports or use.

Loaded modules are not automatically in the namespace. Their symbols are only accessible with qualification (e.g., `Board.mkBoard`) or after importing via `:import` or `:module +`.

- `cabal repl` via `.cabal` loads modules automatically.
- `:load <FilePath>` to load modules manually.
- `:show modules` to show current loaded modules.

### How Scopes Interact

A module must be loaded (via `:load` or `cabal repl`) before it can be imported (`:import`) or added to the namespace (`:m +`).

The **module context** is a single module whose symbols are in the namespace. The **namespace** can include symbols from multiple modules via `:import` or `:m +`, plus the context’s symbols.

If Board is the context (after `:load src/Board.hs`), `mkBoard` is in the namespace. Adding `:m +Game` keeps Board’s symbols and adds `Game`’s.

#### `:r`

After running `:m +Main Board Game TestGHCI` in GHCi, the modules Main, Board, Game, and TestGHCI will still be in the namespace after you run `:r`, provided that the modules are successfully reloaded without errors and no other commands (e.g., :module without arguments or :m -<Module>) explicitly remove them from the namespace.
The `:r` command reloads all currently loaded modules but does not reset the namespace. The namespace persists across `:r` invocations.

#### Other GHCi Finds:

`:browse <ModuleName>`

```
ghci> :browse Game
type Game :: *
data Game
  = Game {board :: Board,
          turn :: Player,
          countToWin :: Game.CountToWin}
mkGame :: Int -> Game.CountToWin -> Maybe Game
win :: Game -> Maybe Player
ghci> :browse Graphics
lineH :: [Maybe Player] -> String
pawnSpaces :: [Maybe Player] -> String
boardRows :: Board -> [String]
```

`:show modules`

## Errors Language

in the error:

```
Couldn't match expected type ‘[Char]’ with actual type ‘Char’ • In the first argument of ‘(++)’, namely ‘(prefix (i `div` l))’ In the expression: (prefix (i `div` l)) ++ [suffixChar] In an equation for ‘charIndex’: charIndex cs i = (prefix (i `div` l)) ++ [suffixChar] where suffixChar = cs !! (i `mod` l) l = length cs prefix i' | i' < l = cs !! (i' `mod` l) | otherwise = charIndex cs i'
```

`expected type [Char]` it means that the type system expects a **`[Char]`** but your code is actually writing a **`Char`**

