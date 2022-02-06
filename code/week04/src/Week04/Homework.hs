{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text, unpack)
import Data.Void             (Void)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet


data PayParams = PayParams                               -- record type with 2 fields
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- awaitPromise $ endpoint @"pay" return          -- receives parameter value of type PayParams and then produces a contract that doesn t make use of any side effect
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    Contract.handleError (\err -> Contract.logError $ "caught: " ++ unpack err) $ void $ submitTx tx -- turns constraints above into an actual tx +  submit
    payContract -- recursively call the contract again

payContract2 :: Contract () PaySchema Void ()
payContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err) -- unpack: convert Text to String
    payContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace y z = do  -- IMPLEMENT ME!
            h <- activateContractWallet (knownWallet 1) payContract
            callEndpoint @"pay" h $ PayParams{ppRecipient = mockWalletPaymentPubKeyHash $ knownWallet 2, ppLovelace = y }
            void $ Emulator.waitNSlots 1
            callEndpoint @"pay" h PayParams{ppRecipient = mockWalletPaymentPubKeyHash $ knownWallet 2, ppLovelace = z }
            void $ Emulator.waitNSlots 1
            
payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 10_000_000 20_000_000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000_000_000 20_000_000
