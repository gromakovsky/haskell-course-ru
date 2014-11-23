{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories hiding ((.))
import ITMOPrelude.Categories.FromMonad

import Prelude (Show)

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat } deriving Show

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State $ \rw -> let RealWorld (Cons x xs) out exit = rw in (RealWorld xs out exit, x)

getMaybeNat :: IO (Maybe Nat)
getMaybeNat = State $ \rw -> let RealWorld input output exit = rw in
                                case input of Nil -> (RealWorld Nil output exit, Nothing)
                                              _   -> runState (fmap Just getNat) rw

putNat :: Nat -> IO ()
putNat x = State $ \rw -> let RealWorld input output exit = rw in (RealWorld input (output `append` x) exit, ())

setExitCode :: Nat -> IO ()
setExitCode x = State $ \rw -> let RealWorld input output _ = rw in (RealWorld input output x, ())

testRealWorld = RealWorld testList Nil undefined
-- read number and write incremented, setting exit code to zero
test1 :: IO ()
test1 = fmap Succ getNat >>= putNat >> setExitCode natZero

-- another way to do it
test2 :: IO ()
test2 = getNat >>= (putNat . Succ) >> setExitCode natZero

-- read number and write decrement, but if it is zero don't print anything, just set return code 1
testRealWorld2 = RealWorld (Cons natZero testList) Nil undefined
test3 :: IO ()
test3 = getNat >>= \x -> case x of Zero -> setExitCode natOne
                                   _    -> (putNat $ prev x) >> setExitCode natZero

testGetMaybeRW = RealWorld Nil Nil natZero
test4 = runState getMaybeNat testGetMaybeRW
