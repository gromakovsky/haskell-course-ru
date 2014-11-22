{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories

import Prelude (Show)

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat } deriving Show

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State $ \rw -> let RealWorld (Cons x xs) out exit = rw in (RealWorld xs out exit, x)

putNat :: Nat -> IO ()
putNat x = State $ \rw -> let RealWorld input output exit = rw in (RealWorld input (output `append` x) exit, ())

setExitCode :: Nat -> IO ()
setExitCode x = State $ \rw -> let RealWorld input output _ = rw in (RealWorld input output x, ())

