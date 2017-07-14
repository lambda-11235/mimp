
module Exec where

import AST

import qualified Data.Map as M
import Control.Monad.State


data RTState = RTState { getMemory :: [Nat]
                       , getLabelAddrs :: M.Map String Int }
                       deriving (Eq, Show)

emptyRTState :: RTState
emptyRTState = RTState [] M.empty

overMemory :: ([Nat] -> [Nat]) -> RTState -> RTState
overMemory f (RTState mem lbls) = RTState (f mem) lbls

overLblAddrs :: (M.Map String Int -> M.Map String Int) -> RTState -> RTState
overLblAddrs f (RTState mem lbls) = RTState mem (f lbls)

deref :: Int -> RTState -> Nat
deref n rts = deref' n (getMemory rts)
  where
    deref' _ [] = 0
    deref' n (x:xs) = if n == 0 then x else deref' (n - 1) xs

setRef :: Int -> Nat -> RTState -> RTState
setRef n x = overMemory (setRef' n x)
  where
    setRef' n x [] = if n <= 0 then [x] else 0 : (setRef' (n - 1) x [])
    setRef' n x (y:ys) = if n <= 0 then x:ys else y : (setRef' (n - 1) x ys)


type PState = StateT RTState IO


evalValue :: Value -> PState Nat
evalValue (Num n) = return n
evalValue (Ref ptr) = do ptr' <- evalArith ptr
                         rts <- get
                         return (deref (fromIntegral ptr') rts)
evalValue (Location lbl) =
  do s <- get
     case M.lookup lbl (getLabelAddrs s) of
       Nothing -> fail $ "Unbound label " ++ lbl
       Just loc -> return (fromIntegral loc)


evalArith :: Arith -> PState Nat
evalArith (Val v) = evalValue v
evalArith (Add x y) = (+) <$> (evalArith x) <*> (evalArith y)
evalArith (Sub x y) = (-) <$> (evalArith x) <*> (evalArith y)
evalArith (Mult x y) = (*) <$> (evalArith x) <*> (evalArith y)
evalArith (Div x y) = div <$> (evalArith x) <*> (evalArith y)

evalCond :: Cond -> PState Bool
evalCond (And x y) = (&&) <$> (evalCond x) <*> (evalCond y)
evalCond (Or x y) = (||) <$> (evalCond x) <*> (evalCond y)
evalCond (CLT x y) = do x' <- evalArith x
                        y' <- evalArith y
                        return (x' < y')
evalCond (CEQ x y) = do x' <- evalArith x
                        y' <- evalArith y
                        return (x' == y')
evalCond (CGT x y) = do x' <- evalArith x
                        y' <- evalArith y
                        return (x' > y')


exec' :: Int -> Program -> PState ()
exec' pos prog = if pos < length prog then execC (prog !! pos) else return ()
  where
    execC (Label _) = exec' (pos + 1) prog
    execC (Assign ptr expr) =
      do ptr' <- evalArith ptr
         x <- evalArith expr
         modify (setRef (fromIntegral ptr') x)
         exec' (pos + 1) prog
    execC (JMP loc) =
      do pos' <- evalValue loc
         exec' (fromIntegral pos') prog
    execC (JIF cond loc) =
      do b <- evalCond cond
         pos' <- evalValue loc
         if b then exec' (fromIntegral pos') prog else exec' (pos + 1) prog
    execC (Read ptr) =
      do ptr' <- evalArith ptr
         x <- lift (readLn :: IO Int)
         if x < 0 then fail $ "Entered a negative number: " ++ show x else
           do modify (setRef (fromIntegral ptr') (fromIntegral x))
              exec' (pos + 1) prog
    execC (Print x) =
      do x' <- evalValue x
         lift $ print x'
         exec' (pos + 1) prog


exec :: Program -> IO ()
exec prog = let rts = RTState [] (buildLabelAddrs prog M.empty 0) in
              do runStateT (exec' 0 prog) rts
                 return ()

buildLabelAddrs :: Program -> M.Map String Int -> Int -> M.Map String Int
buildLabelAddrs [] lbls _ = lbls
buildLabelAddrs ((Label s) : stmts) lbls pos =
  buildLabelAddrs stmts (M.insert s pos lbls) (pos + 1)
buildLabelAddrs (_:stmts) lbls pos =
  buildLabelAddrs stmts lbls (pos + 1)
