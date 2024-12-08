module Bank where

-- Define the BankOp monad
newtype BankOp a = BankOp { runBankOp :: Float -> (a, Float) }

instance Functor BankOp where
    fmap f (BankOp op) = BankOp $ \balance ->
        let (a, newBalance) = op balance
        in (f a, newBalance)

instance Applicative BankOp where
    pure a = BankOp $ \balance -> (a, balance)
    BankOp f <*> BankOp a = BankOp $ \balance ->
        let (g, balance') = f balance
            (x, balance'') = a balance'
        in (g x, balance'')

instance Monad BankOp where
    return = pure
    BankOp op >>= f = BankOp $ \balance ->
        let (a, balance') = op balance
            BankOp op' = f a
        in op' balance'

-- Deposit operation
deposit :: Float -> BankOp ()
deposit amount = BankOp $ \balance -> ((), balance + amount)

-- Withdraw operation
withdraw :: Float -> BankOp Float
withdraw amount = BankOp $ \balance ->
    let maxOverdraw = -100
        newBalance = balance - amount
        actualWithdrawal = if newBalance >= maxOverdraw
                           then amount
                           else balance - maxOverdraw
    in (actualWithdrawal, balance - actualWithdrawal)

-- Get balance operation
getBalance :: BankOp Float
getBalance = BankOp $ \balance -> (balance, balance)