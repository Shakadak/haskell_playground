{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module FD (
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
    labelling     -- Backtracking search for all solutions
    ) where

import Prelude hiding (lookup)
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Map ((!), Map)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Control.Applicative

-- The FD monad
newtype FD s a = FD { unFD :: StateT (FDState s) [] a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadState (FDState s))

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
newVars n domain = replicateM n (newVar domain)

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
    guard $ val `IntSet.member` vals
    let i = IntSet.singleton val
    when (i /= vals) $ update var i

-- Constrain two variables to have the same value.
same :: FDVar s -> FDVar s -> FD s ()
same = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    let i = IntSet.intersection xv yv
    guard $ not $ IntSet.null i
    when (i /= xv) $ update x i
    when (i /= yv) $ update y i

-- Constrain two variables to have different values.
different :: FDVar s -> FDVar s -> FD s ()
different = addBinaryConstraint $ \x y -> do
    xv <- lookup x
    yv <- lookup y
    guard $ IntSet.size xv > 1 || IntSet.size yv > 1 || xv /= yv
    when (IntSet.size xv == 1 && xv `IntSet.isProperSubsetOf` yv) $
        update y (yv `IntSet.difference` xv)
    when (IntSet.size yv == 1 && yv `IntSet.isProperSubsetOf` xv) $
        update x (xv `IntSet.difference` yv)

-- Constrain a list of variables to all have different values.
allDifferent :: [FDVar s] -> FD s ()
allDifferent (x:xs) = do
    mapM_ (different x) xs
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
    guard $ not $ IntSet.null xv'
    guard $ not $ IntSet.null yv'
    when (xv /= xv') $ update x xv'
    when (yv /= yv') $ update y yv'

-- Label variables using a depth-first left-to-right search.
labelling :: [FDVar s] -> FD s [Int]
labelling = mapM label where
    label var = do
        vals <- lookup var
        val <- FD . lift $ IntSet.toList vals
        var `hasValue` val
        return val
