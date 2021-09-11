{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week03.TraceHW2 where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger
import Ledger.TimeSlot
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Week03.Homework2

-- This file is for testing Homework2 by calling test

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"give" h1 $ GiveParams (pubKeyHash $ walletPubKey $ Wallet 2) (slotToBeginPOSIXTime def 15) 10000000
    void $ waitUntilSlot 17
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s