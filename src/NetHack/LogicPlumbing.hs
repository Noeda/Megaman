-- This module implements the ugly plumbing DSL for NetHack AI

{-# LANGUAGE ExistentialQuantification #-}

module NetHack.LogicPlumbing
  (NetHackState(terminal, currentLevel),
   newGame,
   boolAction,
   boolAction2,
   boolAction_,
   boolAction2_,
   sinkAction,
   BAction(),
   NAction(),
   NActionReturn(Answer, Bailout),
   hasSinked,
   isSomewhereOnScreen,
   repeatUntilNoAnswer,
   cursorIsInside,
   messages,
   modAction,
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

import NetHack.More
import NetHack.State

newGame :: NAction -> NetHackState
newGame = NetHackState level (T.emptyTerminal 80 24) [] 1
          where
            (level, _) = newLevel 0

boolAction :: NAction -> NAction ->
              (NetHackState -> IO (NetHackState, Bool)) ->
              BAction
boolAction = BAction

boolAction2 :: NAction -> NAction ->
               (NetHackState -> Bool) ->
               BAction
boolAction2 ac1 ac2 fun = BAction ac1 ac2 $
  (\ns -> return $ (ns, (fun ns)))

boolAction_ :: (NetHackState -> IO (NetHackState, Bool)) ->
               BAction
boolAction_ = BAction SinkAction SinkAction

boolAction2_ :: (NetHackState -> Bool) ->
                BAction
boolAction2_ = boolAction2 SinkAction SinkAction

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

modAction :: (NetHackState -> IO NetHackState) -> NAction
modAction = ModAction

runSteps :: NetHackState -> IO (NetHackState, Maybe NActionReturn)
runSteps ns = runAction (ns { next = SinkAction,
                              messages = stripMessages (terminal ns) })
                        (next ns)

runAction :: NetHackState -> NAction -> IO (NetHackState, Maybe NActionReturn)
runAction ns SinkAction = return (ns, Nothing)
runAction ns b@(BailoutAction str) = return (ns { next = b },
                                             Just $ Bailout str)
runAction ns (StepOutAction ac) = return (ns { next = SinkAction }, Just ac)
runAction ns (ModAction fun) = fun ns >>= (\ns2 -> return (ns2, Nothing))
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

