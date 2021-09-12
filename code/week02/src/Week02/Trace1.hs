{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week02.Trace1 where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger
import Ledger.TimeSlot
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Week02.Homework1

-- This file is for testing Homework1 by calling testSuccess or testError

testSuccess :: IO ()
testSuccess = runEmulatorTraceIO myTraceSuccess

testError :: IO ()
testError = runEmulatorTraceIO myTraceError

myTraceSuccess :: EmulatorTrace ()
myTraceSuccess = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"give" h1 $ 10000000
    void $ waitUntilSlot 10
    callEndpoint @"grab" h2 (False, False)
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s

myTraceError :: EmulatorTrace ()
myTraceError = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"give" h1 $ 10000000
    void $ waitUntilSlot 10
    callEndpoint @"grab" h2 (False, True)
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s