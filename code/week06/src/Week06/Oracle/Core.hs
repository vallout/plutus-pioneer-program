{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Core
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleAsset
    , oracleInst
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , runOracle
    , findOracle
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada                as Ada
import           Ledger.Value              as Value
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..))

data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol
    , oOperator :: !PubKeyHash
    , oFee      :: !Value
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''Oracle

data OracleRedeemer = Update | Use
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName
oracleTokenName = TokenName emptyByteString

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator _      _ Use    _   = False
mkOracleValidator oracle _ Update ctx =
    traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&
    traceIfFalse "token missing from input"   inputHasToken                        &&
    traceIfFalse "token missing from output"  outputHasToken                       &&
    traceIfFalse "invalid output datum"       validOutputDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inputHasToken :: Bool
    inputHasToken = case findOwnInput ctx of
        Nothing -> False
        Just i  -> assetClassValueOf (txOutValue $ txInInfoResolved i) (oracleAsset oracle) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    validOutputDatum :: Bool
    validOutputDatum = case txOutDatum ownOutput of
        Nothing -> False
        Just dh -> case findDatum dh info of
            Nothing        -> False
            Just (Datum d) -> case PlutusTx.fromData d of
                Nothing             -> False
                Just (_ :: Integer) -> True

data Oracling
instance Scripts.ScriptType Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

oracleInst :: Oracle -> Scripts.ScriptInstance Oracling
oracleInst oracle = Scripts.validator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . oracleInst

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator

startOracle :: forall w s. HasBlockchainActions s => Integer -> Contract w s Text Oracle
startOracle fees = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    osc <- mapError (pack . show) (forgeContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = Ada.lovelaceValueOf fees
            }
    logInfo @String $ "forged oracle state token for oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (oracleInst oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.scriptInstanceLookups (oracleInst oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x

findOracle :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            dh        <- txOutDatumHash $ txOutTxOut o
            (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o
            x         <- PlutusTx.fromData d
            return (oref, o, x)
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1

type OracleSchema = BlockchainActions .\/ Endpoint "update" Integer

runOracle :: Integer -> Contract (Last Oracle) OracleSchema Text ()
runOracle fees = do
    oracle <- startOracle fees
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle