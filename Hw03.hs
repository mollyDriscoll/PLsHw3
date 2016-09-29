-- Homework 3.0: The "While" programming language
-- Due 2016-09-30

 {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

 {-# OPTIONS_GHC -W #-}
 module Hw03 where

 import qualified Data.Map as Map
 import Data.Map (Map)
 import qualified Data.Set as Set
 import Data.Set (Set)

-- Throughout this homework, we'll be experimenting with our first
-- interpreter for what really is a programming language. We'll need two
-- concepts throughout: variable names (which will just be strings) and
-- stores (a/k/a heaps, where we keep the contents of the variables of
-- our language). All of our variables will store integers.

 type VarName = String

 type Store = Map VarName Int

-- <h3>Problem 1: Interpreting While</h3>

-- We'll define an interpreter for a language that goes beyond the simple
-- WhileNZ language we saw in class.



 data AExp =
     Var VarName
   | Num Int
   | Plus AExp AExp
   | Times AExp AExp
   | Neg AExp
   deriving (Show, Eq, Ord)

-- Write an interpreter for these arithmetic expressions. When evaluating
-- variables, you should return 0 if they're not in the store (such
-- variables are called *unbound* or *undefined*).

 evalA :: Store -> AExp -> Int


 evalA _ (Num x)     =  x
 evalA s (Plus x y)  = (evalA s x) + (evalA s y)
 evalA s (Times x y) = (evalA s x) * (evalA s y)
 evalA s (Neg x)     = -(evalA s x)
 evalA s (Var x)     = case Map.lookup x s of
                         Nothing -> 0
                         Just z -> z


-- We can define boolean expressions similarly. Rather than concretely
-- specifying which arithmetic expressions they're defined over, we just
-- take in a parameter.

 data BExp a =
     Bool Bool
   | Equal a a
   | Lt a a
   | Not (BExp a)
   | Or (BExp a) (BExp a)
   | And (BExp a) (BExp a)
   deriving (Show, Eq, Ord)

-- Write an interpreter for boolean expressions over our prior arithmetic expressions.

 evalB :: Store -> BExp AExp -> Bool
 evalB _ (Bool b) = b
 evalB s (Equal x y) = (evalA s x) == (evalA s y)
 evalB s (Lt x y) = (evalA s x) < (evalA s y)
 evalB s (Not x) = not (evalB s x)
 evalB s (Or x y) = (evalB s x) || (evalB s y)
 evalB s (And x y) = (evalB s x) && (evalB s y)

-- Finally, we'll define a simple programming language. Its abstract
-- syntax tree (AST) takes two type parameters: one identifying the
-- arithmetic expressions we'll use, one identifying the boolean
-- expressions we'll use.

 data Stmt a b =
     Skip
   | Assign VarName a
   | Seq (Stmt a b) (Stmt a b)
   | If (b a) (Stmt a b) (Stmt a b)
   | While (b a) (Stmt a b)
   deriving (Show, Eq, Ord)

-- Write an interpreter for this language.

 eval :: Store -> Stmt AExp BExp -> Store
 eval s Skip = s
 eval s (Assign x y) = Map.insert x (evalA s y) s
 eval s (Seq x y) = eval (eval s x) y
 eval s (If c x y) = if evalB s c then eval s x else eval s y
 eval s (While c x) = if evalB s c then eval (eval s x) (While c x) else s

-- <h3>Problem 2: While, with failures</h3>

-- Here's a new definition for arithmetic expressions, adding division.

 data AExp' =
     Var' VarName
   | Num' Int
   | Plus' AExp' AExp'
   | Times' AExp' AExp'
   | Neg' AExp'
   | Div' AExp' AExp'
   deriving (Show, Eq)

-- Note that division is an operation that can fail. Write another
-- interpreter (defining whatever functions you need). Do not use the
-- `error` function.

-- In the interpreter above, variables not in the store were given the
-- default value of 0. In this version of the interpreter, make it so
-- that unbound variables in arithmetic expressions cause errors, just
-- like division. Here are the two errors that can happen:

 data Error = NoSuchVariable VarName | DivideByZero AExp'

-- When you encounter an unbound variable, the error has a slot for
-- identifying the culpable variable. Similarly, when you try to divide
-- by zero, you should record the entire division expression responsible,
-- not just the divisor. (In a more serious AST, we might keep track of
-- the source file and line number each expression came from, in order to
-- better indicate the source of the problem.)

 evalA' :: Store -> AExp' -> Either Error Int
 evalA' _ (Num' x)     = Right x
 evalA' s (Plus' x y)  = case evalA' s x of
                          Left e -> Left e
                          Right z -> case evalA' s y of
                                      Left e -> Left e
                                      Right v -> Right $ z + v
 evalA' s (Times' x y) = case evalA' s x of
                          Left e -> Left e
                          Right z -> case evalA' s y of
                                      Left e -> Left e
                                      Right v -> Right $ z * v
 evalA' s (Div' x y)   = case evalA' s y of
                          Left e -> Left e
                          Right 0 -> Left (DivideByZero (Div' x y))
                          Right z -> case evalA' s x of
                                      Left e -> Left e
                                      Right v -> Right $ v `div` z
 evalA' s (Neg' x)     = evalA' s (Times' (Neg' (Num' 1)) x)
 evalA' s (Var' x)     = case Map.lookup x s of
                            Nothing -> Left (NoSuchVariable x)
                            Just z -> Right z

 evalB' :: Store -> BExp AExp' -> Either Error Bool
 evalB' _ (Bool b) = Right b
 evalB' s (Equal x y) = case evalA' s x of
                         Left e -> Left e
                         Right z -> case evalA' s y of
                           Left e -> Left e
                           Right v -> Right $ z == v
 evalB' s (Lt x y) = case evalA' s x of
                         Left e -> Left e
                         Right z -> case evalA' s y of
                           Left e -> Left e
                           Right v -> Right $ z < v
 evalB' s (Not x) = case evalB' s x of
                      Left e -> Left e
                      Right z -> Right $ not z
 evalB' s (Or x y) = case evalB' s x of
                         Left e -> Left e
                         Right z -> case evalB' s y of
                           Left e -> Left e
                           Right v -> Right $ z || v
 evalB' s (And x y) = case evalB' s x of
                         Left e -> Left e
                         Right z -> case evalB' s y of
                           Left e -> Left e
                           Right v -> Right $ z && v

 eval' :: Store -> Stmt AExp' BExp -> Either Error Store
 eval' s Skip         = Right s
 eval' s (Assign x y) = case evalA' s y of
                         Right z -> Right $ Map.insert x z s
                         Left e  -> Left e
 eval' s (Seq x y)    = case eval' s x of
                         Right z -> eval' z y
                         Left e  -> Left e
 eval' s (If c x y)   = case evalB' s c of
                         Left e      -> Left e
                         Right True  -> eval' s x
                         Right False -> eval' s y
 eval' s (While c x)  = case evalB' s c of
                         Left e     -> Left e
                         Right True -> case eval' s x of
                                        Left e -> Left e
                                        Right z -> eval' z (While c x)
                         Right False -> Right s

-- <h3>Problem 3: Static analysis</h3>

-- Can we determine in advance whether a given program will try to use an
-- unbound variable if they're run in an initially empty store? This kind
-- of analysis is called "def/use analysis", and it's a common early step
-- in compilation. More generally, this is "static analysis", becuase we
-- inspect our programs before we run them. (*Static* and *dynamic* are
-- opposites; you can read them as "at compile time" and "at run time",
-- respectively.)

-- In some programs, it's easy:

 unboundY = Assign "x" (Var' "y")

-- The program `unboundY` will always fail in an unbound store. It can be
-- more ambiguous, though, as in:

 ambiguous b = Seq (If b (Assign "y" (Num' 0)) Skip) unboundY

-- Depending on what we know about `b`, we may or may not have a problem
-- on our hands. Absent any information about `b`, it *could* happen that
-- `ambiguous b` will try to read from `y` before it's defined.

-- In PL, we tend to stay on the safe side: the general philosophy is
-- that's better to have a false positive (saying a program is unsafe
-- when it's actually fine) than to have a false negative (saying a
-- program is safe when it isn't!). That is, PL prioritizes *soundness*
-- (if we say X, then X is really true) over *completeness* (if X is
-- really true, then we say X). As a side note, observe that it's easy to
-- write a trivial sound analysis (everything's unsafe, please wear a
-- helmet) as it is a trivial complete analysis (everything's safe, take
-- it easy).

-- To get started, write functions that collect all of the variables that
-- appear in given arithmetic and boolean expressions.
--
-- Var' VarName
-- | Num' Int
-- | Plus' AExp' AExp'
-- | Times' AExp' AExp'
-- | Neg' AExp'
-- | Div' AExp' AExp'

 varsA :: AExp' -> Set VarName
 varsA (Var' b) = Set.singleton b
 varsA (Num' _) = Set.empty
 varsA (Plus' a b) = Set.union (varsA a) (varsA b)
 varsA (Times' a b) = Set.union (varsA a) (varsA b)
 varsA (Neg' a) = varsA a
 varsA (Div' a b) = Set.union (varsA a) (varsA b)

-- For example, `varsA (Times' (Plus' (Var' "x") (Var' "y")) (Num' 3)) ==
-- Set.fromList ["x", "y"]`.
-- data BExp a =
--     Bool Bool
--   | Equal a a
--   | Lt a a
--   | Not (BExp a)
--   | Or (BExp a) (BExp a)
--   | And (BExp a) (BExp a)
--   deriving (Show, Eq, Ord)

 varsB :: BExp AExp' -> Set VarName
 varsB (Bool b) = Set.empty
 varsB (Equal a b) = Set.union (varsA a) (varsA b)
 varsB (Lt a b) = Set.union (varsA a) (varsA b)
 varsB (Not b) = varsB b
 varsB (Or a b) = Set.union (varsB a) (varsB b)
 varsB (And a b) = Set.union (varsB a) (varsB b)

-- For example, `varsB (Or (Not (Equal (Var' "foo") (Var' "bar"))) (Bool
-- True)) == Set.fromList ["bar", "foo"]`.

-- Now let's write our analysis: we'll take in a set of variables that we
-- know to be defined, a statement in our language, and we'll return a
-- pair of sets: the set of variables that have been defined and the set
-- of variables that have been used *but not defined*.

 useBeforeDef :: Set VarName -> Stmt AExp' BExp -> (Set VarName, Set VarName)
 useBeforeDef defs Skip = (defs, Set.empty)
 useBeforeDef defs (Assign x a) = (Set.insert x defs, varsA a `Set.difference` defs)


-- What should the other cases do? Remember, you have to be *sound*: the
-- variable in the first part of the pair (the defined variables) must
-- *always* be defined; if it's at all possible for a variable to
-- undefined, it must not appear in the first part. Similarly, if it's at
-- all possible for variable to *ever* be used before it's defined, it
-- must appear in the second part.

-- With these guiding principles, what should we do for `Seq s1 s2`?
-- Everything `s1` defines will be defined for `s2`. The final set of
-- definitions will also include what `s2` defines. What about the the
-- variables that are used before they're defined? If `x` is used in `s1`
-- before it's defined, it doesn't matter if it's later defined in
-- `s2`---it's too late.

-- What about `If b s1 s2`? It's too hard to know anything about the
-- condition `b`. But if we can be certain that both branches define a
-- variable, then we can be certain that it'll be defined at the
-- end. Conversely, if either branch could use a given variable before
-- it's defined, then that variable could potentially be used before
-- being defined.

-- Once you know how `If` and `Seq` works, you should have the general
-- principle for `While`. Sketch it out on the board!

 useBeforeDef _ _ = undefined



-- Be very careful testing your function. Strive for soundness.  The
-- tests below show the results for my `useBeforeDef`---don't feel
-- obligated to do better, but don't do worse. You can modify or delete
-- these tests---my grader ignores them.

 testUnbound, testAmbiguous :: Bool
 testUnbound = useBeforeDef Set.empty unboundY ==
               (Set.singleton "x", Set.singleton "y")

 testAmbiguous = useBeforeDef Set.empty (ambiguous (Bool True)) ==
                 (Set.singleton "x", Set.singleton "y")

-- <h3>Problem 4: Mission Impossible</h3>

-- Your final task is to solve the halting problem. We'll start by
-- writing a function that runs a program a little bit---just one
-- "step". Then we'll look at the *trace* of steps the program takes. If
-- we ever end up in a state we've seen before, then the program
-- diverges. This is a dynamic analysis, since we'll be running our
-- programs.

-- First, fill in the step function below.

 type Config = (Store, Stmt AExp BExp)

 step :: Config -> Maybe Config
 step (_,Skip) = Nothing
 step (st,Assign x a) = Just (Map.insert x (evalA st a) st,Skip)
 step (st,Seq Skip s2) = Just (st,s2)
 step (st,Seq s1 s2) = undefined
 step (st,If b s1 s2) = undefined
 step (st,While b s) = undefined

-- Given a step function, we can compute a trace, i.e., the possibly
-- infinite list of `Config`s that the program will step through. Such a
-- program is safe to write in Haskell because Haskell is *lazy*, i.e.,
-- it will only compute things on demand.

 trace :: (a -> Maybe a) -> a -> [a]
 trace f v =
   case f v of
     Nothing -> [v]
     Just v' -> v:trace f v'

-- I may have gotten excited earlier when I said we'd "solve" the halting
-- problem. We can *try* to solve it, but sometimes we'll have to throw
-- up our hands and say "Who knows?". To facilitate that, we'll use
-- *three-valued logic*, which extends the booleans with a notion of
-- "don't know".

 data TVL = No | Maybe | Yes deriving (Show, Eq, Ord)

-- Write a function `diverges` that checks for loops in a list of
-- configurations. (Note that I've written a much more general type.) The
-- integer paramter should serve as a timeout---a limit as to how far
-- we're willing to look.

-- What counts as a loop? Each element in the list will represent a
-- `Config`, i.e., a pair of a store and a statement currently being
-- executed. If we ever see the same pair twice, we know the program
-- diverges because our programs are *deterministic*, i.e., they do the
-- same thing every time. So your job is to check for duplicate
-- configurations, i.e., elements that appear more than once in the
-- loop. A wise choice of data structure here will make your life easier
-- (and speed up your program).

 diverges :: Ord a => Int -> [a] -> TVL
 diverges limit = undefined


-- Write a function `haltsIn` that takes a starting configuration and a
-- limit and tries to determine whether that configuration ever halts
-- (within the specified limit, from the empty store).

 haltsIn :: Stmt AExp BExp -> Int -> TVL
 haltsIn s limit = undefined


-- Now we have our analysis... let's see what it can do. Write a While
-- program `loop` that diverges and:

-- ```
-- loop `haltsIn` 1000 == No
-- ```

 loop :: Stmt AExp BExp
 loop = undefined

-- Write a While program `long` that converges and:

-- ```
-- long `haltsIn` 1000 == Maybe
-- long `haltsIn` 5000 == Yes
-- ```

 long :: Stmt AExp BExp
 long = undefined

-- Write a While program `tricky` that diverges but for all `n`:

-- ```
-- tricky `haltsIn` n == Maybe
-- ```

 tricky :: Stmt AExp BExp
 tricky = undefined

-- Explain why your `haltsIn` gives an imprecise answer.


-- Do you think you can write a program where `haltsIn` gives a wrong
-- answer? If so, explain your idea---or write it! If not, explain (or
-- prove!) why not.
