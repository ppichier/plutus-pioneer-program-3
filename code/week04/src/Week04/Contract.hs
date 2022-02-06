{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- Contract w s e a
-- EmulatorTrace a

-- w allows a contract to writes log messages of type w (communicate between different contracts). w is visible from the outside
-- s specifies the endpoints
-- type of error message

-- w: () no log message; Empty: no endpoints available; e: Text more efficient that String; the result a is unit(we don't care)
myContract1 :: Contract () Empty Text () 
myContract1 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "hello from the contract" -- atString because we have to tell the compiler the type use because

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (knownWallet 1) myContract1

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

-- here we catch the exception
myContract2 :: Contract () Empty Void () -- Void is different from Unit, Void has no value : This contract can possibly thrown an exception
myContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err) -- unpack: convert Text to String
    myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String -- Here "foo" is a type (DataKinds extension)
                                                             -- .\/ is a type constructor (TypeOperators extension)

myContract3 :: Contract () MySchema Text () -- 
myContract3 = do
    awaitPromise $ endpoint @"foo" Contract.logInfo -- loginfo is type of Int -> Contract
    awaitPromise $ endpoint @"bar" Contract.logInfo

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (knownWallet 1) myContract3 -- start the contract, here we need the handle h. we bind it with arrow left
    callEndpoint @"foo" h 42                                -- call the endpoint from the emulator
    callEndpoint @"bar" h "Haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

myContract4 :: Contract [Int] Empty Text () -- w is a list a INT (Monoid)
myContract4 = do
    void $ Contract.waitNSlots 10
    tell [1]                        -- use the writer feature, tell takes one arg namely the state we want to write
    void $ Contract.waitNSlots 10   
    tell [2]
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (knownWallet 1) myContract4 -- start the contract

    void $ Emulator.waitNSlots 5
    xs <- observableState h         -- observableState look the state of a running contract
    Extras.logInfo $ show xs        -- xs is the state of the contract at that point in time

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
