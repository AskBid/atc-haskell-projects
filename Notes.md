# Notes To Self - Lessons Learned During Project.

## `do` block limits `<-` type to the function return type.

In a do block for a function returning `IO a`, the `<-` operator can only bind values from `IO` actions (e.g., `IO` `String`, `IO Task`).

You cannot use `<-` directly with `Maybe`, `List`, or other monads in an `IO do` block because `<-` is specific to the monad of the `do` block (`IO` in this case).

To work with `Maybe Task` in an `IO do` block, you must:
- Handle the Maybe explicitly (e.g., with `case`).
- Convert the `Maybe` into an `IO action` (e.g., `MaybeT` or wrapping with `pure`).
- Change the do block to a different monad (not applicable here since handleInput requires `IO`).

Inside an `IO do` block, you can use `getTask` (which returns `Maybe Task`) by assigning its result to a let variable and then using case to handle the `Maybe`.

Inside the case branches (specifically the Just branch), you can open a nested `do` block that operates in the `Maybe` monad, even though it’s nested within an `IO do` block.

A `do` block's behavior is tied to the monad of the function's return type because it desugars to a sequence of `(>>=)` operations.

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

The `m` in `(>>=)` must be the same monad instance (e.g., `IO` or `Maybe`), so you can't have `m a` be a `Maybe` monad while `m b` is an `IO monad` in the same `(>>=)` chain.

*Key constraint:* The monad `m` must be the same for both the input `m a` and the output `m b`. This is enforced by the type signature, where `m` is a single monad instance (e.g., `IO`, `Maybe`, `[]`, etc.) throughout the operation.

```
handleInput :: [Task] -> String -> IO (Bool, [Task])
```
The return type is `IO (Bool, [Task])`, so the do block operates in the `IO` monad, and all `<-` bindings must involve `IO` actions.


**Why this fails?**

```haskell
handleInput ts "mark" =
  putStrLn "Enter completed task:" >>= \_ ->
  getLine >>= \name ->
  getTask name ts >>= \task ->
  -- ...
```


Haskell can’t apply (>>=) :: IO a -> (a -> IO b) -> IO b to getTask name ts :: Maybe Task because Maybe ≠ IO.


here a possible way to handle it:

```haskell
handleInput ts "mark" = do
  putStrLn "Enter completed task:"
  name <- getLine
  let maybeTask = getTask name ts  -- Maybe Task
  case maybeTask of
    Nothing -> do
      putStrLn $ "Task '" ++ name ++ "' not found."
      pure (True, ts)
    Just task -> do
      let updatedTask = task { completed = True }
      let updatedTasks = map (\t -> if name == name t then updatedTask else t) ts
      pure (True, updatedTasks)
```

The `case .. of` handles the `Maybe Task` purely, and each branch produces an `IO (Bool, [Task])` using `IO` actions (`putStrLn`, `pure`).

By using `let` and `case`, you avoid trying to bind `Maybe Task` with `(>>=)` in the `IO` monad, sidestepping the monad mismatch.
