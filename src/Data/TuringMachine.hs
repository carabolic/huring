--------------------------------------------------------------------------------
module TuringMachine where

import Alphabet
import State
import Tape as T
import Movement
import Util

-- |Transition function
type Transition s a = State s -> Alphabet a -> (State s, Alphabet a, Movement)

-- |Turingmachine containing the current state, the transition table and the k
-- tapes.
data TuringMachine s a = TM (State s) (Transition s a) (Tape a)

-- |Intitializes a Turing Machine TM with k Tapes. Whereas the first tape is the
-- input tape, the last tape is the output tape. Hence there are k-2 working
-- tapes left.
init :: Tape a -> Transition s a -> TuringMachine s a
init tape transition = TM SStart transition tape

computeStep :: TuringMachine s a -> TuringMachine s a
computeStep (TM Halt transf tape)  = TM Halt transf tape
computeStep (TM state transf tape) = TM state' transf tape'
    where action = transf state (T.read tape)
          state' = fstOf3 action
          symbol' = sndOf3 action
          tape' = moveTape (thdOf3 action) (write symbol' tape)

compute :: TuringMachine s a -> TuringMachine s a
compute (TM Halt transf tape)  = TM Halt transf tape
compute machine = compute . computeStep $ machine

getOutput :: TuringMachine s a -> Tape a
getOutput (TM _ _ tape) = tape

moveTape :: Movement -> Tape a -> Tape a
moveTape L tape = moveLeft tape
moveTape R tape = moveRight tape
moveTape S tape = tape
