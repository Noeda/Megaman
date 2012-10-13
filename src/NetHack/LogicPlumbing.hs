-- This module implements the ugly plumbing DSL for NetHack AI

{-# LANGUAGE ExistentialQuantification #-}

module NetHack.LogicPlumbing
  (newGame, NHStep, isSomewhereOnScreen, ifIn, ifNotIn, answer,
   (=+=), (>=+=), NetHackState(NetHackState, terminal), showStep)
  where

import qualified Terminal as T

data NetHackState = NetHackState { terminal :: T.Terminal }

newGame :: NetHackState
newGame = NetHackState { terminal = T.emptyTerminal 80 24 }

type NHAction a = (NetHackState -> IO (NetHackState, a))

-- Action is currently just like IO monad. Might be more sophisticated in
-- the future. At the moment, its purpose is to forbid mixing IO operations
-- in code with NetHack actions. (liftIO has to be used)
data NHStep a = Nop a |
                Action (NHAction a) |
                Conditional (NHStep Bool) (NHStep ()) |
                Sequence (NHStep ()) (NHStep a) |
                Answer Char |
                ScreenCheck String |
                forall b. BoundSequence (NHStep b) (b -> NHStep a)

instance Functor NHStep where
  fmap f (Nop x) = Nop (f x)
  fmap f (Sequence step1 step2) = Sequence step1 (fmap f step2)
  fmap _ (Answer ch) = Answer ch
  fmap _ (ScreenCheck str) = ScreenCheck str
  fmap _ (Conditional step1 step2) = Conditional step1 step2
  fmap f (BoundSequence step1 fstep2) = BoundSequence step1
                                          (\r -> fmap f $ fstep2 r)

ifIn :: NHStep Bool -> NHStep () -> NHStep ()
ifIn test action = Conditional test action

ifNotIn :: NHStep Bool -> NHStep () -> NHStep ()
ifNotIn test action = Conditional (test >=+= \result -> Nop (not result))
                                  action

isSomewhereOnScreen :: String -> NHStep Bool
isSomewhereOnScreen str = ScreenCheck str

answer :: Char -> NHStep ()
answer ch = Answer ch

voidStep :: NHStep a -> NHStep ()
voidStep action = fmap (\_ -> ()) action

(=+=) :: NHStep a -> NHStep b -> NHStep b
(=+=) step1 step2 = Sequence (voidStep step1) step2

(>=+=) :: NHStep a -> (a -> NHStep b) -> NHStep b
(>=+=) step1 fstep2 =
  BoundSequence step1 fstep2

-- For debugging
showStep :: Show a => NHStep a -> String
showStep (Nop a) = "Nop " ++ show a
showStep (Action ac) = "NHAction <unviewable>"
showStep (Sequence ac1 ac2) = "[" ++ showStep ac1 ++ "," ++ showStep ac2 ++ "]"
showStep (Answer ch) = "Answer " ++ show ch
showStep (ScreenCheck str) = "ScreenCheck " ++ show str
showStep (BoundSequence _ _) = 
  "BoundSequence <unviewable>"
showStep (Conditional test result) =
  "Conditional {" ++ showStep test ++ "} -> {" ++ showStep result ++ "}"

