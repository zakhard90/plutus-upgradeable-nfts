{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module NftUpgraderContract where

import Prelude (IO, Semigroup (..), Show (..), String)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import Control.Monad hiding (fmap)
import Text.Printf (printf)

import Plutus.Contract as Contract
import Plutus.Trace.Emulator as Emulator
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Ledger hiding (mint, singleton)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value

import Wallet.Emulator.Wallet

-- ON-CHAIN

{-# INLINABLE tokenPolicy #-}

-- Defining Minting Validator
tokenPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
tokenPolicy outputRef assetName () scriptContext = 
    traceIfFalse "UTxO not consumed"   hasUTxO &&
    traceIfFalse "Wrong nft amount"  checkIfNotMinted
                    
    where
        info :: TxInfo
        info = scriptContextTxInfo scriptContext
        
        hasUTxO :: Bool
        hasUTxO = any (\utxo -> txInInfoOutRef utxo == outputRef) $ txInfoInputs info       
        
        checkIfNotMinted :: Bool
        checkIfNotMinted = case flattenValue (txInfoMint info) of
          [(_, assetName', amount)] -> assetName' == assetName && amount == 1
          _-> False     

-- Building Policy Script
policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy outputRef assetName = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \outputRef' assetName' -> Scripts.wrapMintingPolicy $ tokenPolicy outputRef' assetName' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode outputRef
    `PlutusTx.applyCode`
    PlutusTx.liftCode assetName        


-- Extracting Policy ID    
tokenSymbol :: TxOutRef -> TokenName -> CurrencySymbol
tokenSymbol outputRef assetName = scriptCurrencySymbol $ policy outputRef assetName

-- OFF-CHAIN

-- Defining minting lambda
mint :: NFTMintParams -> Contract w NFTSchema Text ()
mint np = do
    utxos <- utxosAt $ mAddress np
    case Map.keys utxos of
        []       -> Contract.logError @String "No utxo found"
        outputRef : _ -> do
            let tn      = mToken np
            let val     = Value.singleton (tokenSymbol outputRef tn) tn 1
                lookups = Constraints.mintingPolicy (policy outputRef tn) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput outputRef
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "Forged %s" (show val)


-- Defining minting input parameters
data NFTMintParams = NFTMintParams
    { mToken   :: !TokenName
    , mAddress :: !Address
    , rAddress :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)


-- Defining endpoint input binding
type NFTSchema = Endpoint "mint" NFTMintParams  


-- Defining contract interaction endpoints
endpoints :: Contract () NFTSchema Text ()
endpoints = awaitPromise mint' >> endpoints
  where
    mint' = endpoint @"mint" mint
    

-- SIMULATOR

test :: IO ()
test = runEmulatorTraceIO $ do
    let t1 = "Halloween_0001_L1"
        t2 = "Halloween_0002_L1"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTMintParams
        { mToken   = t1
        , mAddress = mockWalletAddress w1
        , rAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ NFTMintParams
        { mToken   = t2
        , mAddress = mockWalletAddress w1
        , rAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
