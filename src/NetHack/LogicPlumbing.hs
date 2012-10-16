-- This module implements the ugly plumbing DSL for NetHack AI

{-# LANGUAGE ExistentialQuantification #-}

module NetHack.LogicPlumbing
  (NetHackState(terminal, currentLevel),
   newGame,
   boolAction,
   sinkAction,
   BAction(),
   NAction(),
   NActionReturn(Answer, Bailout),
   hasSinked,
   isSomewhereOnScreen,
   repeatUntilNoAnswer,
   cursorIsInside,
   bailout,
   ifIn,
   ifNotIn,
   (=~=),
   (=||=),
   (=&&=),
   (=+=),
   answer,
   runSteps)
  where

import qualified Terminal as T
import Control.Monad.Cont
import Control.Monad.State

data NetHackState = NetHackState {
                                   currentLevel :: Int,
                                   terminal :: T.Terminal,
                                   next :: NAction }

newGame :: NAction -> NetHackState
newGame = NetHackState 1 (T.emptyTerminal 80 24)

data NActionReturn = Answer Char |
                     Bailout String

data BAction = BAction NAction NAction
                 (NetHackState -> IO (NetHackState, Bool)) |
               AndAction BAction BAction |
               OrAction BAction BAction |
               NotAction BAction
data NAction = IfAction BAction NAction NAction |
               SeqAction NAction NAction |
               StepOutAction NActionReturn |
               RepeatUntilNoAnswer NAction |
               SinkAction |
               BailoutAction String

boolAction :: NAction -> NAction ->
              (NetHackState -> IO (NetHackState, Bool)) ->
              BAction
boolAction = BAction

repeatUntilNoAnswer :: NAction -> NAction
repeatUntilNoAnswer = RepeatUntilNoAnswer

sinkAction :: NAction
sinkAction = SinkAction

hasSinked :: NetHackState -> Bool
hasSinked (NetHackState { next = n }) = isSinkAction n
  where
    isSinkAction SinkAction        = True
    isSinkAction (BailoutAction _) = True
    isSinkAction _                 = False


cursorIsInside :: (Int, Int) -> (Int, Int) -> BAction
cursorIsInside lefttop rightbottom =
  BAction SinkAction
          SinkAction
          (\ns -> return $ (ns, T.cursorIsInside lefttop rightbottom
                                                 (terminal ns)))

isSomewhereOnScreen :: String -> BAction
isSomewhereOnScreen str =
  BAction SinkAction
          SinkAction
          (\ns -> return $ (ns, T.isSomewhereOnScreen str (terminal ns)))

ifIn :: BAction -> NAction -> NAction
ifIn bac dothis = IfAction bac dothis SinkAction

ifNotIn :: BAction -> NAction -> NAction
ifNotIn bac1 dothis =
  ifIn (NotAction bac1) dothis

(=&&=) :: BAction -> BAction -> BAction
(=&&=) = AndAction

(=~=) :: BAction -> BAction
(=~=) = NotAction

(=||=) :: BAction -> BAction -> BAction
(=||=) = OrAction

(=+=) :: NAction -> NAction -> NAction
(=+=) = SeqAction

answer :: Char -> NAction
answer = StepOutAction . Answer

bailout :: String -> NAction
bailout = BailoutAction

runSteps :: NetHackState -> IO (NetHackState, Maybe NActionReturn)
runSteps ns = runAction (ns { next = SinkAction }) (next ns)

runAction :: NetHackState -> NAction -> IO (NetHackState, Maybe NActionReturn)
runAction ns SinkAction = return (ns, Nothing)
runAction ns b@(BailoutAction str) = return (ns { next = b },
                                             Just $ Bailout str)
runAction ns (StepOutAction ac) = return (ns { next = SinkAction }, Just ac)
runAction ns (SeqAction ac1 ac2) = do
  (ns2, ch) <- runAction ns ac1
  case ch of
    Nothing -> runAction ns2 ac2
    Just _  -> return (ns2 { next = SeqAction (next ns2) ac2 },
                       ch)
runAction ns (IfAction (OrAction bac1 bac2) ac acelse) =
  runAction ns (IfAction bac1 ac (IfAction bac2 ac acelse))
runAction ns (IfAction (AndAction bac1 bac2) ac acelse) =
  runAction ns (IfAction bac1 (IfAction bac2 ac acelse) acelse)
runAction ns (IfAction (NotAction b) ac2 acelse) =
  runAction ns (IfAction b acelse ac2)
runAction ns (IfAction (BAction ac1 cleanup payload) ac2 acelse) = do
  (ns2, ch) <- runAction ns ac1
  case ch of
    Nothing -> do (ns3, result) <- payload ns2
                  if result then runAction ns3 (SeqAction cleanup ac2)
                            else runAction ns3 (SeqAction cleanup acelse)
    Just _  -> return (ns2 { next = IfAction
                                      (BAction (next ns2) cleanup payload)
                                      ac2
                                      acelse },
                       ch)
runAction ns r@(RepeatUntilNoAnswer ac) = do
  (ns2, ch) <- runAction ns ac
  case ch of
    Nothing -> return (ns2, Nothing)
    Just  _ -> return (ns2 { next = SeqAction (next ns2) r }, ch)

