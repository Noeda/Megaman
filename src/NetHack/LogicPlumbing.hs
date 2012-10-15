-- This module implements the ugly plumbing DSL for NetHack AI

{-# LANGUAGE ExistentialQuantification #-}

module NetHack.LogicPlumbing
  (NetHackState(terminal),
   newGame,
   BAction(),
   NAction(),
   hasSinked,
   isSomewhereOnScreen,
   cursorIsInside,
   bailout,
   ifIn,
   ifNotIn,
   (=&&=),
   (=+=),
   answer,
   runSteps)
  where

import qualified Terminal as T
import Control.Monad.Cont
import Control.Monad.State

data NetHackState = NetHackState { terminal :: T.Terminal,
                                   next :: NAction }

newGame :: NAction -> NetHackState
newGame = NetHackState (T.emptyTerminal 80 24)

data BAction = BAction NAction NAction
                 (NetHackState -> IO (NetHackState, Bool)) |
               AndAction BAction BAction |
               OrAction BAction BAction |
               NotAction BAction
data NAction = IfAction BAction NAction NAction |
               SeqAction NAction NAction |
               AnswerAction Char |
               SinkAction

hasSinked :: NetHackState -> Bool
hasSinked (NetHackState { next = n }) = isSinkAction n
  where
    isSinkAction SinkAction = True
    isSinkAction _          = False


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
  IfAction (NotAction bac1) dothis SinkAction

(=&&=) :: BAction -> BAction -> BAction
(=&&=) = AndAction

(=~=) :: BAction -> BAction
(=~=) = NotAction

(=||=) :: BAction -> BAction -> BAction
(=||=) = OrAction

(=+=) :: NAction -> NAction -> NAction
(=+=) = SeqAction

answer :: Char -> NAction
answer = AnswerAction

bailout :: String -> NAction
bailout _ = SinkAction

runSteps :: NetHackState -> IO (NetHackState, Maybe Char)
runSteps ns = runAction (ns { next = SinkAction }) (next ns)

runAction :: NetHackState -> NAction -> IO (NetHackState, Maybe Char)
runAction ns SinkAction = return (ns, Nothing)
runAction ns (AnswerAction ch) = return (ns, Just ch)
runAction ns (SeqAction ac1 ac2) = do
  (ns2, ch) <- runAction ns ac1
  case ch of
    Nothing -> runAction ns2 ac2
    Just  _ -> return (ns2 { next = SeqAction (next ns2) ac2 }, ch)
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
