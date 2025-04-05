module NestedList where

import Classes
import List ()

{-
    Define the 'NestedList' type, for handling lists with an arbitrary
    level of intricateness, which would be impossible to specify
    using plain Haskell lists. For instance, the String representation
    of such a nested list might be "[1, [2, 3], 4, 5]".
    
    Instantiate the following classes with the nested list type:
    * 'Show'
    * 'Functor'
    * 'Container'
    * 'Invertible'
    
    The inversion should be performed DEEPLY i.e., for the elements as well.
-}

data NestedList a
    = Elem a               
    | List [NestedList a]
    deriving (Eq, Read)

instance Show a => Show (NestedList a) where
    show (Elem x) = show x
    show (List xs) = "[" ++ showElements xs ++ "]"
      where
        showElements [] = ""
        showElements [y] = show y
        showElements (y:ys) = show y ++ ", " ++ showElements ys

instance Functor NestedList where
    fmap f (Elem x) = Elem (f x)
    fmap f (List xs) = List (map (fmap f) xs)

instance Container NestedList where
    contents (Elem x) = [x]
    contents (List xs) = concatMap contents xs

instance Invertible a => Invertible (NestedList a) where
    invert (Elem x) = Elem (invert x)
    invert (List xs) = List (reverse (map invert xs))
