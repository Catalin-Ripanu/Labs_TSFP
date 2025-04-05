module Typing.Unification where

-- Import required modules
import Typing.Type                -- For Type data type and Substitution type
import qualified Data.Map as M    -- For Map operations
import Control.Monad.State       -- For StateT monad transformer
import Control.Monad.Except      -- For Except monad

{-
    A monad for solving unification constraints. It is composed of:
    * a state monad transformer, for maintaining the substitution
    * an exception monad, for signaling unification errors.
-}
-- Define Unif as a monad stack: StateT for substitution state, wrapped in Except for error handling
type Unif = StateT Substitution (Except String)

-- | Run a unification computation with an initial substitution
-- Returns Either an error message or a tuple of result and final substitution
runUnif :: Unif a -> Substitution -> Either String (a, Substitution)
runUnif ops subst = runExcept $ runStateT ops subst

{-|
    Obtains the end of the binding chain for the given type.
    The search ends when either of the following is reached:
    * an unbound type variable
    * a function type.
-}
chainEnd :: Type       -- ^ Type to look up
         -> Unif Type  -- ^ Chain end
-- If we have a function type, it's already a chain end, return it
chainEnd t@(Arrow _ _) = return t
-- For type variables, we need to check if they're bound
chainEnd t@(TypeVar v) = do
    subst <- get                     -- Get current substitution
    case M.lookup v subst of         -- Look up the variable in substitution
        Nothing -> return t          -- If not bound, it's a chain end
        Just bound -> do
            -- Check for infinite chain
            let occurs = case bound of
                          TypeVar v' -> v == v'
                          Arrow t1 t2 -> v `occursIn` t1 || v `occursIn` t2
            if occurs
                then throwError $ "Infinite type: " ++ v ++ " = " ++ show bound
                else chainEnd bound  -- If bound, follow the chain recursively
  where
    occursIn :: String -> Type -> Bool
    occursIn x (TypeVar y) = x == y
    occursIn x (Arrow t1 t2) = occursIn x t1 || occursIn x t2

{-|
    Returns true if a type variable does NOT appear in the given type.
    The type variable is assumed FREE within the substitution.
-}
occCheck :: String     -- ^ Type variable to check for occurrence
         -> Type       -- ^ Type to look in
         -> Unif Bool  -- ^ True if the type variable does NOT occur
occCheck v t = do
    t' <- chainEnd t                -- First get the chain end of the type
    case t' of
        -- For type variables, check if they're different from our target
        TypeVar v' -> return (v /= v')
        -- For function types, recursively check both sides
        Arrow t1 t2 -> do
            noOcc1 <- occCheck v t1  -- Check first type
            noOcc2 <- occCheck v t2  -- Check second type
            return (noOcc1 && noOcc2) -- Variable must not occur in either

{-|
    Unifies two type expressions.
-}
unify :: Type -> Type -> Unif ()
unify t1 t2 = do
    t1' <- chainEnd t1
    t2' <- chainEnd t2
    case (t1', t2') of
        (TypeVar v1, TypeVar v2) | v1 == v2 -> return ()
        (TypeVar v, t) -> bindToType v t
        (t, TypeVar v) -> bindToType v t
        (Arrow a1 b1, Arrow a2 b2) -> do
            unify a1 a2
            unify b1 b2
  where
    bindToType v t = do
        noOcc <- occCheck v t
        unless noOcc $
            throwError $ "Occurs check failed: " ++ v ++ " in " ++ show t
        modify (M.insert v t)

{-|
    Applies the substitution to a type expression.
-}
applySubst :: Type -> Unif Type
applySubst t = do
    t' <- chainEnd t
    case t' of
        TypeVar _ -> return t'
        Arrow t1 t2 -> Arrow <$> applySubst t1 <*> applySubst t2

            -- Check liftM2 
