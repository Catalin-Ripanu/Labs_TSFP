{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
module Free where

{-
    Required packages:
    
    * mtl
    * deriving-compat
-}

import Control.Monad
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Writer
import Text.Show.Deriving

{-
    In general, a free object of a specific kind (monoid, monad, algebra etc.)
    is the least constrained such object, i.e., it satisfies only the
    definitional properties and nothing more. In addition, any other object of
    the same kind can be recovered from the free object.

    In particular, a free monad over a functor records a sequence of
    computations, without actually performing any specific computation. In other
    words, it constructs computation ASTs, using the functor's data constructors
    as AST nodes. More specifically:
    
    * return produces leaves
    * (>>=) grows the AST by replacing leaves with additional subtrees
    
    In turn, the ASTs can be interpreted within other, more specific monads
    (State, Writer etc.).

    The free monad for functor f is Free f, containing leaves of type a:

    data Free f a = Free (f (Free f a)) | Pure a

    If you ignore the (Pure a) part, you are left with a fixed-point type,
    which fills the holes in functor f's data constructors with the fixed-point
    type itself, in order to get... the fixed-point type. This is precisely
    what allows us to construct arbitrarily nested ASTs using functor f's data
    constructors as nodes.

    An analogy can be made with standard Haskell lists:

    data List a = Cons a (List a) | Null

    * Just as Null means no elements, Pure means no functor f layers
      (0 AST depth)
    * Just as Cons means an additional element, Free means an additional f layer
      (an additional level in the AST)

    instance Functor f => Functor (Free f) where
        fmap :: (a -> b) -> Free f a -> Free f b
        fmap f (Pure a) = Pure $ f a
        fmap f (Free tree) = Free $ fmap (fmap f) tree
    
    instance Functor f => Monad (Free f) where
        return :: a -> Free f a
        return = Pure

        (>>=) :: Free f a -> (a -> Free f b) -> Free f b
        Pure a >>= f = f a
        Free tree >>= f = Free $ fmap (>>= f) tree
    
    An element of type (f a) can be transformed into an elementary AST using:

    liftF :: Functor f => f a -> Free f a
    liftF fa = Free $ fmap Pure fa
-}

{-
    *** TODO 1 ***

    What known monad is the free monad for the Empty functor, Free Empty,
    equivalent to?

    Empty constructs unpopulated types, since there are no data constructors.
-}

-- Free Empty is equivalent to the Identity monad
-- Since Empty has no constructors, Free Empty can only contain Pure values
-- Therefore, it can only contain a single value, just like Identity

-- > freeEmpty
-- Pure 2  -- It just contains the value 2, computed from 1 + 1

data Empty t
    deriving Functor
$(deriveShow1 ''Empty)

freeEmpty :: Free Empty Int
freeEmpty = do
    n <- return 1
    return $ n + 1

{-
    *** TODO 2 ***

    What known monad is the free monad for the Unit functor, Free Unit,
    equivalent to?

    Unit constructs singleton types, since t is unused.
-}

-- Free Unit is equivalent to the Maybe monad
-- Since Unit has one constructor with no data, it represents computation that
-- either produces a value (Pure) or stops (Free Unit)

data Unit t = Unit
    deriving Functor
$(deriveShow1 ''Unit)

unit :: Free Unit a
unit = liftF Unit

freeUnit' :: Free Unit Int
freeUnit' = do
    a <- return 1
    b <- return 2  -- replace (return 2) with unit and reevaluate
    return $ a + b

-- Modified example:
freeUnit :: Free Unit Int
freeUnit = do
    a <- return 1
    unit        -- This will terminate the computation here
    return $ a + 2  -- This never executes

-- > freeUnit
-- Free Unit  -- The computation stops at 'unit'

-- > freeUnit'  -- With original code using return
-- Pure 3     -- Completes the computation

{-
    *** TODO 3 ***

    What known monad is the free monad for the (a,) functor, using () for leaves,
    equivalent to?
-}

-- Free ((,) a) () is equivalent to the List monad -- wrong (think about Writer Monad)
-- It builds a linked list where each node contains a value and points to the rest

-- > pl
-- Free (0,Free (1,Free (2,Free (3,Free (4,Free (5,Free (6,Free (7,Pure ()))))))))
-- This represents the list [0,1,2,3,4,5,6,7]

type PseudoList a = Free ((,) a) ()

singleton :: a -> PseudoList a
singleton a = liftF (a, ())

pl1 :: PseudoList Int
pl1 = do
    singleton 1
    singleton 2
    singleton 3
  
pl2 :: PseudoList Int
pl2 = do
    singleton 5
    singleton 6
  
pl :: PseudoList Int
pl = do
    singleton 0
    pl1
    singleton 4
    pl2
    singleton 7

{-
    *** TODO 4 ***

    If we wanted to define a binary tree as a free monad, what would the
    corresponding functor be?

    Define TreeF and node.

    Use TODO 3 for inspiration.
-}

data TreeF a b = Node a [b]
    deriving Functor
$(deriveShow1 ''TreeF)

type Tree a = Free (TreeF a) ()

-- node :: a -> Tree a
-- node a = liftF $ Node a []

-- Think about PseudoList a = Free ((,) a) () (list of unit values)
-- continue with liftF
node :: a -> [Tree a] -> Tree a
node a children = Free $ Node a children

tree :: Tree Int
tree = 
    node 1 [
        node 2 [
            node 4 [],
            node 5 []
        ],
        node 3 [
            node 6 []
        ]
    ]

{-
    *** TODO 5 ***

    What is the behavior of the free monad for the standard list functor?

    An AST constructed using a free monad can be interpreted within another
    monad m using:

    foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a

    What is the difference between freeList and (foldFree id freeList)?
    The second expression interprets the AST within the standard list monad.
-}

-- The free monad for the list functor creates a computation tree that represents
-- all possible combinations of choices.
-- freeList builds an AST representing the computation
-- foldFree id freeList actually executes it, producing all combinations

freeList :: Free [] (Int, Int, Int)
freeList = do
    x <- liftF [1, 2, 3]
    y <- liftF [4, 5]
    z <- liftF [7,8]
    return (x, y, z)

{-
    *** TODO 6 ***

    StackASTF is the base functor for a simple stack language. Its data
    constructors correspond to available commands:

    * Push places an element on top of the stack
    * Add removes the top two elements from the stack, "adds" them (whatever
      that means) and pushes the result back onto the stack.

    (StackAST a) is a free monad for (StackASTF a) and corresponds to ASTs,
    wherein data constructors produce nodes:

    * a is the type of the elements on the stack
    * b is a whole that the free monad fills with the AST type itself.

    Define the "display" and "execute" interpretations of the AST:

    * display produces a textual description of the AST, by interpreting it
      within the (Writer [String]) monad
    * execute actually runs the computations in the AST using a stack of numeric
      values, by interpreting the AST within the (State [a]) monad.
-}

-- StackASTF represents the basic operations available in our stack language
-- 'a' is the type of elements on the stack
-- 'b' is the continuation type (will be filled by the Free monad)
data StackASTF a b
    = Push a b    -- Push puts a value on top of stack and continues with b
    | Add b       -- Add combines top two values and continues with b
    deriving (Functor)
$(deriveShow1 ''StackASTF)

-- StackAST is our free monad over StackASTF
-- It represents a sequence of stack operations
type StackAST a = Free (StackASTF a)

-- Helper function to create a Push operation
push :: a -> StackAST a ()
push a = liftF $ Push a ()

-- Helper function to create an Add operation
add :: StackAST a ()
add = liftF $ Add ()

-- Example program that demonstrates stack operations
program :: StackAST Int ()
program = do
    push 1    -- Stack: [1]
    push 2    -- Stack: [2,1]
    add       -- Stack: [3]   (2 + 1 = 3)

-- Interpreter that converts the AST to human-readable instructions
-- Uses Writer monad to log each operation
display :: Show a => StackAST a () -> Writer [String] ()
display = foldFree go
  where
    go :: Show a => StackASTF a x -> Writer [String] x
    go (Push a next) = do
        tell ["Push " ++ show a]  -- Log the push operation
        return next
    go (Add next) = do
        tell ["Add"]              -- Log the add operation
        return next

-- Interpreter that actually executes the stack operations
-- Uses State monad to maintain the stack state
execute :: Num a => StackAST a () -> State [a] ()
execute = foldFree go
  where
    go :: Num a => StackASTF a x -> State [a] x
    go (Push n next) = do
        modify (n:)               -- Prepend new value to stack
        return next
    go (Add next) = do
        xs <- get
        case xs of
            -- Pop top two values, add them, push result
            (x:y:rest) -> put ((x + y):rest)
            -- If less than 2 values, do nothing
            _ -> put xs
        return next

-- Run both interpreters to see the operations and final result
main :: IO ()
main = do
    -- Display the sequence of operations
    mapM_ putStrLn $ execWriter (display program)
    -- Show the final stack state after execution
    -- runState returns (result, final state)
    print $ runState (execute program) []