{-# LANGUAGE RankNTypes #-}
module FDEx (
    -- Types
    FD,           -- Monad for finite domain constraint solver
    FDVar,        -- Finite domain solver variable

    -- Functions
    runFD,        -- Run the monad and return a list of solutions.
    newVar,       -- Create a new FDVar
    newVars,      -- Create multiple FDVars
    hasValue,     -- Constrain a FDVar to a specific value
    same,         -- Constrain two FDVars to be the same
    different,    -- Constrain two FDVars to be different
    allDifferent, -- Constrain a list of FDVars to be different
    lessThan,     -- Constrain one FDVar to be less than another
    labelling,     -- Backtracking search for all solutions
    zipWithM_',
    mapM_',
    whenM
    ) where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Data.Map ((!), Map)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Control.Monad (MonadPlus, mzero, mplus)
import Control.Applicative (Alternative, empty, (<|>))

-- The FD monad
newtype FD s a = FD { unFD :: (FDState s) -> [] (a, (FDState s)) }
    -- deriving (Monad, Alternative, MonadPlus, MonadState (FDState s))

instance Functor (FD s) where
    fmap f m = FD (\s -> fmap (\(a, s2) -> (f a, s2)) (unFD m s))

instance Applicative (FD s) where
    pure a = FD (\s -> [(a, s)])

    (FD mf) <*> (FD mx) = FD (\s ->
        concatMap (\(f, s1) ->
            concatMap (\(x, s2) -> [(f x, s2)]) (mx s1)
        ) (mf s)
        )

instance Monad (FD s) where
    (FD m) >>= k = FD (\s -> concatMap (\(a, s2) -> unFD (k a) s2) (m s))

instance Alternative (FD s) where
    empty = FD (\_ -> [])
    (FD a) <|> (FD b) = FD (\s -> (a s) `mplus` (b s))

instance MonadPlus (FD s) where
    mzero = FD (\_ -> [])
    mplus (FD l) (FD r) = FD (\s -> (l s) ++ (r s))

evalStateT :: (FDState s -> [] (a, FDState s)) -> FDState s -> [a]
evalStateT m s = concatMap (\(a, _) -> [a]) (m s)

get :: FD s (FDState s)
get = gets id

gets :: (FDState s -> a) -> FD s a
gets f = state (\s -> (f s, s))

put :: FDState s -> FD s ()
put s = state (\_ -> ((), s))

modify :: (FDState s -> FDState s) -> FD s ()
modify f = state (\s -> ((), f s))

state :: (FDState s -> (a, FDState s)) -> FD s a
state f = FD (\s -> [f s])

lift :: [] a -> b -> [(a, b)]
lift m = (\s -> concatMap (\a -> [(a, s)]) m)

replicateM' :: (Monad m) => Int -> m a -> m [a]
replicateM' 0 _ = pure []
replicateM' n m = m >>= \x -> (replicateM' (n - 1) m) >>= \xs -> pure (x : xs)

guard' :: MonadPlus m => Bool -> m ()
guard' True = pure ()
guard' False = mzero

zipWithM_' :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_' _ [] _ = pure ()
zipWithM_' _ _ [] = pure ()
zipWithM_' f (x:xs) (y:ys) = do
    _ <- f x y
    zipWithM_' f xs ys

mapM_' :: Monad m => (a -> m b) -> [a] -> m ()
mapM_' _ [] = pure ()
mapM_' f (x : xs) = (f x) >>= \_ -> mapM_' f xs

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = pure []
mapM' f (x : xs) = (f x) >>= \y -> (mapM' f xs) >>= \ys -> pure (y : ys)

whenM :: Applicative f => Bool -> f () -> f ()
whenM True m = m
whenM False _ = pure ()

-- FD variables
newtype FDVar s = FDVar { unFDVar :: Int } deriving (Ord, Eq)

type VarSupply s = FDVar s
data VarInfo s = VarInfo
     { delayedConstraints :: FD s (), values :: IntSet }
type VarMap s = Map (FDVar s) (VarInfo s)
data FDState s = FDState
     { varSupply :: VarSupply s, varMap :: VarMap s }

-- Run the FD monad and produce a lazy list of possible solutions.
runFD :: (forall s . FD s a) -> [a]
runFD fd = evalStateT (unFD fd) initState

initState :: FDState s
initState = FDState { varSupply = FDVar 0, varMap = Map.empty }


-- Get a new FDVar
newVar :: [Int] -> FD s (FDVar s)
newVar domain= do
    v <- nextVar
    v `isOneOf` domain
    return v
    where
        nextVar :: FD s (FDVar s)
        nextVar = do
            s <- get
            let v = varSupply s
            put $ s { varSupply = FDVar (unFDVar v + 1) }
            return v
        isOneOf :: FDVar s -> [Int] -> FD s ()
        x `isOneOf` dmn =
            modify $ \s ->
                let vm = varMap s
                    vi = VarInfo {
                        delayedConstraints = return (),
                        values = IntSet.fromList dmn}
                in
                s { varMap = Map.insert x vi vm }

newVars :: Int -> [Int] -> FD s [FDVar s]
newVars n domain = replicateM' n (newVar domain)

-- Lookup the current domain of a variable.
lookup :: FDVar s -> FD s IntSet
lookup x = do
    s <- get
    return . values $ varMap s ! x

-- Update the domain of a variable and fire all delayed constraints
-- associated with that variable.
update :: FDVar s -> IntSet -> FD s ()
update x i = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    put $ s { varMap = Map.insert x (vi { values = i}) vm }
    delayedConstraints vi

-- Add a new constraint for a variable to the constraint store.
addConstraint :: FDVar s -> FD s () -> FD s ()
addConstraint x constraint = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    let cs = delayedConstraints vi
    put $ s { varMap =
        Map.insert x (vi { delayedConstraints = cs >> constraint }) vm }
 
-- Useful helper function for adding binary constraints between FDVars.
type BinaryConstraint s = FDVar s -> FDVar s -> FD s ()
addBinaryConstraint :: BinaryConstraint s -> BinaryConstraint s
addBinaryConstraint f x y = do
    let constraint  = f x y
    constraint
    addConstraint x constraint
    addConstraint y constraint

-- Constrain a variable to a particular value.
hasValue :: FDVar s -> Int -> FD s ()
var `hasValue` val = do
    vals <- lookup var
    guard' $ val `IntSet.member` vals
    let i = IntSet.singleton val
    whenM (i /= vals) $ update var i

-- Constrain two variables to have the same value.
same :: FDVar s -> FDVar s -> FD s ()
same = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let i = IntSet.intersection xv yv
    guard' $ not $ IntSet.null i
    whenM (i /= xv) $ update x i
    whenM (i /= yv) $ update y i

-- Constrain two variables to have different values.
different :: FDVar s -> FDVar s -> FD s ()
different = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    guard' $ IntSet.size xv > 1 || IntSet.size yv > 1 || xv /= yv
    whenM (IntSet.size xv == 1 && xv `IntSet.isProperSubsetOf` yv) $
        update y (yv `IntSet.difference` xv)
    whenM (IntSet.size yv == 1 && yv `IntSet.isProperSubsetOf` xv) $
        update x (xv `IntSet.difference` yv)

-- Constrain a list of variables to all have different values.
allDifferent :: [FDVar s] -> FD s ()
allDifferent (x:xs) = do
    mapM_' (different x) xs
    allDifferent xs
allDifferent _ = return ()

-- Constrain one variable to have a value less than the value of another
-- variable.
lessThan :: FDVar s -> FDVar s -> FD s ()
lessThan = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let xv' = IntSet.filter (< IntSet.findMax yv) xv
    let yv' = IntSet.filter (> IntSet.findMin xv) yv
    guard' $ not $ IntSet.null xv'
    guard' $ not $ IntSet.null yv'
    whenM (xv /= xv') $ update x xv'
    whenM (yv /= yv') $ update y yv'

-- Label variables using a depth-first left-to-right search.
labelling :: [FDVar s] -> FD s [Int]
labelling = mapM' label where
    label var = do
        vals <- lookup var
        val <- FD . lift $ IntSet.toList vals
        var `hasValue` val
        return val
