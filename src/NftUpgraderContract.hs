{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NumericUnderscores     #-}

module NftUpgraderContract where

import           Control.Monad                  hiding (fmap)
import           Data.Aeson                     (ToJSON, FromJSON)
import           Data.Map                       as Map
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           Data.Maybe
import           GHC.Generics                   (Generic)
import           Plutus.Contract                as Contract
import           Plutus.Trace.Emulator          as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude               hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins              as Builtins
import           Ledger                         hiding (mint, singleton)
import           Ledger.Constraints             as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
import           Ledger.Ada                     as Ada
import           Ledger.Value                   as Value
import           Prelude                        (IO, Show (..), String, Semigroup(..))
import           Text.Printf                    (printf)
import           Wallet.Emulator.Wallet
import           Control.Monad.Freer.Extras     as Extras
import           CustomRedeemer

-- ON-CHAIN

{-# INLINABLE tokenPolicy #-}
-- Defining Minting Validator
tokenPolicy :: RedeemerParam -> ScriptContext -> Bool
tokenPolicy (RP outRef tokenName pkHash) sContext = traceIfFalse "Can not give the NFT"  givingPath 
	where
           givingPath :: Bool
           givingPath = traceIfFalse "UTxO not consumed" hasUTxO &&
                        traceIfFalse "Wrong ammount minted" checkMintedAmount &&
                        traceIfFalse "Not original mint" checkIfAuthentic &&
                        traceIfFalse "Reciever did not get the NFT" checkIfRecieved
                        
           info :: TxInfo
           info = scriptContextTxInfo sContext

           hasUTxO :: Bool
           hasUTxO = any (\utxo -> txInInfoOutRef utxo == outRef) $ txInfoInputs info

           checkMintedAmount :: Bool
           checkMintedAmount = case flattenValue (txInfoMint info) of
              [(_, tokenName', amount)]   ->  tokenName' == tokenName && amount == 1
              _                       ->  False  
              
           checkIfAuthentic :: Bool
           checkIfAuthentic = txSignedBy (scriptContextTxInfo sContext) $ unPaymentPubKeyHash pkHash
              
           -- todo
           checkIfRecieved :: Bool
           checkIfRecieved = True        

policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy pkHash = mkMintingPolicyScript $ $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy $ tokenPolicy ||]) 

-- Extracting Policy ID    
tokenSymbol :: PaymentPubKeyHash -> CurrencySymbol
tokenSymbol = \pkHash' -> scriptCurrencySymbol $ policy pkHash'


-- OFF-CHAIN
-- todo concat some unique value
uniqueName :: TokenName -> TxOutRef -> TokenName
uniqueName tn or = tn

-- Defining minting lambda
mint :: NFTParams -> Contract w NFTSchema Text ()
mint mParams = do   
               utxos <- utxosAt $ mAddress mParams  
               pkHash <- Contract.ownPaymentPubKeyHash         
               case Map.keys utxos of
                   []       -> Contract.logError @String "No UTxO found on the provied Address!" 
                   outRef : _ -> do 
                                   let tokenName  = uniqueName (mToken mParams) outRef
                                       redeemer   = (RP outRef tokenName pkHash)
                                       nft        = Value.singleton (tokenSymbol pkHash) tokenName 1 
                                       val        = (Ada.lovelaceValueOf 2_000_000) <> nft
                                       lookups    = Constraints.mintingPolicy (policy pkHash)  <>
                                                    Constraints.unspentOutputs utxos
                                       tx         = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData redeemer) nft <>
                                                    Constraints.mustSpendPubKeyOutput outRef <> 
                                                    Constraints.mustPayToPubKey (PaymentPubKeyHash $ fromJust $ toPubKeyHash $ mReceiver mParams) val
                                   ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                   void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                   Contract.logInfo @String $ printf "Forged %s" (show val)             

-- Defining minting and dropping input parameters
data NFTParams = NFTParams
    { mToken    :: !TokenName
    , mAddress  :: !Address
    , mReceiver :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)


-- Defining endpoint input binding
type NFTSchema = Endpoint "mint" NFTParams  


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
    callEndpoint @"mint" h1 $ NFTParams
        { mToken   = t1
        , mAddress = mockWalletAddress w1
        , mReceiver = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ NFTParams
        { mToken   = t2
        , mAddress = mockWalletAddress w1
        , mReceiver = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
