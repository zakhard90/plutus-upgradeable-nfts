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
tokenPolicy :: PaymentPubKeyHash -> String -> () -> ScriptContext -> Bool
tokenPolicy pkh collection () sContext = txSignedBy (scriptContextTxInfo sContext) $ unPaymentPubKeyHash pkh

policy :: PaymentPubKeyHash -> String -> Scripts.MintingPolicy Typed
policy pkh collection = mkMintingPolicyScript $
             		$$(PlutusTx.compile [|| \pkh' collection' -> Scripts.wrapMintingPolicy $ tokenPolicy pkh' collection' ||])
             		`PlutusTx.applyCode`
             		PlutusTx.liftCode pkh     
             		`PlutusTx.applyCode`
             		PlutusTx.liftCode collection  


-- Extracting Policy ID    
tokenSymbol :: PaymentPubKeyHash -> String -> CurrencySymbol
tokenSymbol = \pkh' collection' -> scriptCurrencySymbol $ policy pkh' collection'

-- OFF-CHAIN

-- Defining minting lambda
mint :: NFTMintParams -> Contract w NFTSchema Text ()
mint mparams = do              
               pkh <- Contract.ownPaymentPubKeyHash
               let val = Value.singleton (tokenSymbol pkh (mCollection mparams)) (mToken mparams) 1
                   lookups = Constraints.mintingPolicy (policy pkh (mCollection mparams))
                   tx      = Constraints.mustMintValue val
               ledgerTx <- submitTxConstraintsWith @Void lookups tx
               void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
               Contract.logInfo @String $ printf "Forged %s" (show val)


-- Defining minting input parameters
data NFTMintParams = NFTMintParams
    { mToken   :: !TokenName
    , mAddress :: !Address
    , mCollection :: !String
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
    let c = "CDP"
        t1 = "Halloween_0001_L1"
        t2 = "Halloween_0002_L1"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTMintParams
        { mToken   = t1
        , mAddress = mockWalletAddress w1
        , mCollection = c
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ NFTMintParams
        { mToken   = t2
        , mAddress = mockWalletAddress w1
        , mCollection = c
        }
    void $ Emulator.waitNSlots 1
