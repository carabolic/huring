module State where

data State a = SStart | Halt | State a
             deriving (Eq)

instance Show a => Show (State a) where
    show SStart     = "q_start"
    show Halt       = "q_halt"
    show (State s)  = "q_" ++ show s