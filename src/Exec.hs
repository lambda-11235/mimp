
module Exec where

import AST

import Data.IORef
import qualified Data.Map as M
import qualified Data.Vector.Mutable as V


data RTState = RTState { getMemory :: IORef (V.IOVector Nat)
                       , getLabelAddrs :: IORef (M.Map String Int) }

emptyRTState :: IO RTState
emptyRTState = do vRef <- V.new 0 >>= newIORef
                  mRef <- newIORef M.empty
                  return (RTState vRef mRef)

deref :: Int -> RTState -> IO Nat
deref n (RTState vRef _) =
  do v <- readIORef vRef
     if n < V.length v then V.read v n else return 0

setRef :: Int -> Nat -> RTState -> IO ()
setRef n x (RTState vRef _) =
  do v <- readIORef vRef
     let l = V.length v
     if n < l then V.write v n x else
       do v' <- V.grow v (n - l + 1)
          mapM_ (\idx -> V.write v' idx 0) [l..n]
          V.write v' n x
          writeIORef vRef v'


evalValue :: Value -> RTState -> IO Nat
evalValue (Num n) _ = return n
evalValue (Ref ptr) rts =
  do ptr' <- evalArith ptr rts
     deref (fromIntegral ptr') rts
evalValue (Location lbl) (RTState _ mRef) =
  do m <- readIORef mRef
     case M.lookup lbl m of
       Nothing -> fail $ "Unbound label " ++ lbl
       Just loc -> return (fromIntegral loc)


evalArith :: Arith -> RTState -> IO Nat
evalArith (Val v) rts = evalValue v rts
evalArith (Add x y) rts = (+) <$> (evalArith x rts) <*> (evalArith y rts)
evalArith (Sub x y) rts = (-) <$> (evalArith x rts) <*> (evalArith y rts)
evalArith (Mult x y) rts = (*) <$> (evalArith x rts) <*> (evalArith y rts)
evalArith (Div x y) rts = div <$> (evalArith x rts) <*> (evalArith y rts)

evalCond :: Cond -> RTState -> IO Bool
evalCond (And x y) rts = (&&) <$> (evalCond x rts) <*> (evalCond y rts)
evalCond (Or x y) rts = (||) <$> (evalCond x rts) <*> (evalCond y rts)
evalCond (Not x) rts = not <$> (evalCond x rts)
evalCond (CLT x y) rts = do x' <- evalArith x rts
                            y' <- evalArith y rts
                            return (x' < y')
evalCond (CEQ x y) rts = do x' <- evalArith x rts
                            y' <- evalArith y rts
                            return (x' == y')
evalCond (CGT x y) rts = do x' <- evalArith x rts
                            y' <- evalArith y rts
                            return (x' > y')


exec' :: Int -> Program -> RTState -> IO ()
exec' pos prog rts =
  if pos < length prog then execC (prog !! pos) else return ()
  where
    execC (Label _) = exec' (pos + 1) prog rts
    execC (Assign ptr expr) =
      do ptr' <- evalArith ptr rts
         x <- evalArith expr rts
         setRef (fromIntegral ptr') x rts
         exec' (pos + 1) prog rts
    execC (JMP loc) =
      do pos' <- evalValue loc rts
         exec' (fromIntegral pos') prog rts
    execC (JIF cond loc) =
      do b <- evalCond cond rts
         pos' <- evalValue loc rts
         if b then exec' (fromIntegral pos') prog rts else exec' (pos + 1) prog rts
    execC (Read ptr) =
      do ptr' <- evalArith ptr rts
         x <- readLn :: IO Int
         if x < 0 then fail $ "Entered a negative number: " ++ show x else
           do setRef (fromIntegral ptr') (fromIntegral x) rts
              exec' (pos + 1) prog rts
    execC (Print x) =
      do x' <- evalValue x rts
         print x'
         exec' (pos + 1) prog rts


exec :: Program -> IO ()
exec prog = do rts <- emptyRTState
               writeIORef (getLabelAddrs rts) (buildLabelAddrs prog M.empty 0)
               exec' 0 prog rts

buildLabelAddrs :: Program -> M.Map String Int -> Int -> M.Map String Int
buildLabelAddrs [] lbls _ = lbls
buildLabelAddrs ((Label s) : stmts) lbls pos =
  buildLabelAddrs stmts (M.insert s pos lbls) (pos + 1)
buildLabelAddrs (_:stmts) lbls pos =
  buildLabelAddrs stmts lbls (pos + 1)
