module Bank where

newtype BankOp a = BankOp (Float -> (a, Float))

deposit :: Float -> BankOp ()
deposit amount = BankOp $ \balance -> ((), balance + amount)

withdraw :: Float -> BankOp Float
withdraw amount = BankOp $ \balance ->
    let newBalance = balance - amount
        actualWithdrawn = if newBalance < -100 then balance + 100 else amount
    in (actualWithdrawn, max newBalance (-100))

getBalance :: BankOp Float
getBalance = BankOp $ \balance -> (balance, balance)


runBankOp :: BankOp a -> a
runBankOp (BankOp f) = fst (f 0)

instance Functor BankOp where
    fmap f (BankOp op) = BankOp $ \balance ->
        let (a, newBalance) = op balance
        in (f a, newBalance)

instance Applicative BankOp where
    pure x = BankOp $ \balance -> (x, balance)
    (BankOp f) <*> (BankOp x) = BankOp $ \balance ->
        let (g, newBalance) = f balance
            (a, finalBalance) = x newBalance
        in (g a, finalBalance)

instance Monad BankOp where
    return = pure
    (BankOp x) >>= f = BankOp $ \balance ->
        let (a, newBalance) = x balance
            BankOp nextOp = f a
        in nextOp newBalance