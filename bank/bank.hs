module Bank where
import Control.Monad.State

newtype BankOp a = BankOp { runBankOp :: State Float a }
    deriving (Functor, Applicative, Monad)

deposit :: Float -> BankOp ()
deposit amount = BankOp $ modify (+ amount)

withdraw :: Float -> BankOp Float
withdraw amount = BankOp $ do
    currentBalance <- get
    let newBalance = currentBalance - amount
    put newBalance
    return amount

getBalance :: BankOp Float
getBalance = BankOp get

runBankOp :: BankOp a -> a
runBankOp (BankOp op) initialBalance = evalState op initialBalance


