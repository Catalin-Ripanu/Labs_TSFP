module Typing.Inference where

import Syntax.Expression
import Typing.Type
import qualified Typing.Unification as U
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import Data.Char (chr, ord)

{-|
    The type of inference state.
    
    Should comprise:
    
    * The global typing context
    
    * The type variable counter.
-}
data TypingState = TypingState
    { context :: TypingContext
    , counter :: Counter
    } deriving Show
    
{-|
    The type of the inference mechanism.
    
    Should expose the following:
    
    * Access to the inference state (State)
    
    * Acces to the local typing context (Reader)

    * A means for storing unification constraints (Writer)
-}
type Infer = ExceptT String (ReaderT TypingContext (WriterT [(Type, Type)] (State TypingState)))

runInfer :: Infer a        -- ^ Expression to type
         -> TypingContext  -- ^ Local context
         -> TypingContext  -- ^ Global context
         -> Counter        -- ^ Current type variable counter
         -> Either String (a, [(Type, Type)])
                           -- ^ The result, along with possible unification
                           --   constraints; otherwise, an error message
runInfer inf loc glob cnt = 
    case evalState (runWriterT $ runReaderT (runExceptT inf) loc) (TypingState glob cnt) of
        (Right a, constraints) -> Right (a, constraints)
        (Left err, _) -> Left err

{-|
    Generates a copy of the given type.
    
    Should rely on 'copyM' below.
-}
copy :: Type -> Type
copy t = case runInfer (copyM t) M.empty M.empty 0 of
    Right (typ, _) -> typ
    Left err -> error err  -- or handle error differently if needed

{-|
    The type inference function, wich synthesizes the type of the given
    expression.
    
    Should rely on 'inferM' below.
-}
infer :: Expression          -- ^ Expression to type
      -> TypingContext       -- ^ Local context
      -> TypingContext       -- ^ Global context
      -> Substitution        -- ^ Substitution
      -> Counter             -- ^ Current type variable counter
      -> Either String Type  -- ^ If the typing succeeds,
                             --   the inferred type; otherwise, an error 
                             --   message.
infer expr loc glob subst cnt =
    case runInfer (inferM expr) loc glob cnt of
        Right (ty, constraints) -> 
            case unifyAll constraints of
                Left err -> Left err 
                Right sub -> 
                    case U.runUnif (U.applySubst ty) (compose sub subst) of
                        Right (result, _) -> Right result
                        Left err -> Left err
        Left err -> Left err

-- Helper functions needed:

unifyAll :: [(Type, Type)] -> Either String Substitution
unifyAll constraints = 
    case U.runUnif (mapM_ (uncurry U.unify) constraints) M.empty of
        Left err -> Left err
        Right ((), sub) -> Right sub

        {- remain in monadic sphere -}

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = M.union (M.map (applyWithSubst s1) s2) s1
  where
    applyWithSubst subst t = 
        case U.runUnif (U.applySubst t) subst of
            Right (t', _) -> t'
            Left err -> error err  -- This should never happen

{-|
    Generates a new type variable using the counter hidden within the state,
    and updates the latter.
-}
newTypeVar :: Infer Type
newTypeVar = do
    state <- get
    let cnt = counter state
    put $ state { counter = cnt + 1 }
    return $ TypeVar [chr cnt]

{-|
    See 'copy'.
-}

copyM :: Type -> Infer Type
copyM ty = do
    let initialSubst = M.empty
    (result, _) <- runStateT (go ty) initialSubst
    return result
  where
    go :: Type -> StateT (M.Map String Type) Infer Type
    {- get, put, modify-}
    go (TypeVar v) = StateT $ \subst -> 
        case M.lookup v subst of
            Just t  -> return (t, subst)
            Nothing -> do
                fresh <- newTypeVar
                return (fresh, M.insert v fresh subst)
    go (Arrow t1 t2) = do
        t1' <- go t1
        t2' <- go t2
        return $ Arrow t1' t2'

{-|
    See 'infer'.
-}
inferM :: Expression -> Infer Type
inferM (Var x) = do
    local <- ask
    state <- get
    case M.lookup x local of
        Just t  -> return t
        Nothing -> case M.lookup x (context state) of
            Just t  -> copyM t
            Nothing -> newTypeVar

inferM (Lambda x e) = do
    tx <- newTypeVar
    te <- local (M.insert x tx) (inferM e)
    return $ Arrow tx te

inferM (Application e1 e2) = do
    t1 <- inferM e1
    t2 <- inferM e2
    tv <- newTypeVar
    tell [(t1, Arrow t2 tv)]
    return tv

inferM (Definition x e) = do
    tv <- newTypeVar
    local (M.insert x tv) $ do
        t <- inferM e
        tell [(tv, t)]
        return t
