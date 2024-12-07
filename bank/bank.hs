module Bank where

newtype BankOp a = BankOp { runBankOp :: Float -> (a, Float) }

deposit :: Float -> BankOp ()
deposit amount = BankOp $ \balance -> ((), balance + amount)

withdraw :: Float -> BankOp Float
withdraw amount = BankOp $ \balance ->
    if balance >= amount
        then (amount, balance - amount)
        else (-(amount - balance), 0)  -- Return negative overdraft amount

runBankOp :: BankOp a -> a
runBankOp op = evalStateT op initialState


