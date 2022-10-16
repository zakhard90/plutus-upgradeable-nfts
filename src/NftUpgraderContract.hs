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

-- TODO
-- [x] Working minting policy for NFTs with modular asset name
-- [x] Minting policy for a collection, i.e. same CurrencySymbol for all minted NFTs
-- [x] Enforce uniqueness on TokenName (Modular asset name + id + level + slice of hashed txid)
-- [x] Mintable Serum token with a custom amount
-- [ ] Traverse tx inputs to get utxos with genesis NFT and Serum
-- [ ] Add minting condition to allow users to mint upgraded NFT with genesis NFT and Serum
-- [ ] Verify that input tokens meet the required condition, i.e. L1 + S1 = L2
-- [ ] Burn the input tokens
-- [ ] Generalize for L2, L3, etc..
-- [?] Payment for the genesis mint

module NftUpgraderContract where

import           Control.Monad                  hiding (fmap)
import           Data.Aeson                     (ToJSON, FromJSON)
import           Data.Map                       as Map
import           Data.Text                      (Text, pack)
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
import           CustomRedeemer

-- ON-CHAIN

{-# INLINABLE tokenPolicy #-}
-- Defining Minting Validator
tokenPolicy :: RedeemerParam -> ScriptContext -> Bool
tokenPolicy (RP outRef tkName mintAmount pkHash) sContext = traceIfFalse "Can not give the NFT"  givingPath 
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
              [(_, tkName', amount)]   ->  tkName' == tkName && amount == mintAmount
              _                        ->  False  
              
           checkIfAuthentic :: Bool
           checkIfAuthentic = txSignedBy (scriptContextTxInfo sContext) $ unPaymentPubKeyHash pkHash
              
           -- todo
           checkIfRecieved :: Bool
           checkIfRecieved = True        

policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy pkHash = mkMintingPolicyScript $ 
                $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy $ tokenPolicy ||]) 
                -- `PlutusTx.applyCode`
                -- PlutusTx.liftCode pkHash

-- Extracting policy ID    
tokenSymbol :: PaymentPubKeyHash -> CurrencySymbol
tokenSymbol = scriptCurrencySymbol . policy

-- OFF-CHAIN

-- Enforce a really unique value for the token name
uniqueName :: TokenName -> TxOutRef -> TokenName
uniqueName tn ou = TokenName $ Builtins.appendByteString (unTokenName tn) fingerprint
        where
           -- Slice of the hash
           fingerprint :: BuiltinByteString
           fingerprint = Builtins.sliceByteString 0 8 digest
           
           -- Hash of tx id and tx index
           digest :: BuiltinByteString
           digest = Builtins.blake2b_256 $ Builtins.appendByteString tx idx
           
           tx :: BuiltinByteString
           tx = getTxId $ txOutRefId ou
           
           idx :: BuiltinByteString
           idx = Builtins.encodeUtf8 $ Builtins.toBuiltin $ pack $ show $ txOutRefIdx ou
           
-- Defining minting function
mintDrop :: NFTParams -> Contract w NFTSchema Text ()
mintDrop mParams = do   
               utxos <- utxosAt $ mAddress mParams  
               pkHash <- Contract.ownPaymentPubKeyHash   
               Contract.logInfo @String $ printf "PkHASH %s" (show pkHash)      
               case Map.keys utxos of
                 []       -> Contract.logError @String "No UTxO found on the provied Address!" 
                 outRef : _ -> do 
                                let tkName     = uniqueName (mToken mParams) outRef
                                    mAmt       = mAmount mParams
                                    redeemer   = (RP outRef tkName mAmt pkHash)
                                    nft        = Value.singleton (tokenSymbol pkHash) tkName mAmt 
                                    val        = (Ada.lovelaceValueOf 2_000_000) <> nft
                                    lookups    = Constraints.mintingPolicy (policy pkHash)  <>
                                                 Constraints.unspentOutputs utxos
                                    tx         = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData redeemer) nft <>                                               Constraints.mustSpendPubKeyOutput outRef <> 
                                                 Constraints.mustPayToPubKey (PaymentPubKeyHash $ fromJust $ toPubKeyHash $ mReceiver mParams) val
                                ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                Contract.logInfo @String $ printf "Forged %s" (show val)             

-- Defining minting and dropping input parameters
data NFTParams = NFTParams
    { mToken    :: !TokenName
    , mAmount   :: !Integer
    , mAddress  :: !Address
    , mReceiver :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)


-- Defining endpoint input binding
type NFTSchema = Endpoint "mint" NFTParams  


-- Defining contract interaction endpoints
endpoints :: Contract () NFTSchema Text ()
endpoints = awaitPromise mint' >> endpoints
  where
    mint' = endpoint @"mint" mintDrop    

-- SIMULATOR

test :: IO ()
test = runEmulatorTraceIO $ do

    let t1 = "Halloween_0001_L1_"
        q1 = 1
        t2 = "Halloween_0002_L1_"
        q2 = 1
        t3 = "Halloween_S1_"
        q3 = 1000
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTParams
        { mToken    = t1
        , mAmount   = q1
        , mAddress  = mockWalletAddress w1
        , mReceiver = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h2 $ NFTParams
        { mToken   = t2
        , mAmount   = q2
        , mAddress = mockWalletAddress w2
        , mReceiver = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ NFTParams
        { mToken   = t3
        , mAmount   = q3
        , mAddress = mockWalletAddress w1
        , mReceiver = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
