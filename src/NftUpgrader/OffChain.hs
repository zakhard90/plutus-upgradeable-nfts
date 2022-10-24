{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE NumericUnderscores     #-}

module NftUpgrader.OffChain where
     
import           NftUpgrader.PolicyRedeemer  
import           NftUpgrader.OnChain 
import           NftUpgrader.Utility     
      
import           Control.Monad                  hiding (fmap)
import           Data.Aeson                     (ToJSON, FromJSON)
import           Data.Map                       as Map
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import qualified Data.Maybe                     as Maybe
import           GHC.Generics                   (Generic)
import           Plutus.Contract                as Contract
import qualified PlutusTx
import           PlutusTx.Prelude               hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins              as Builtins
import           Ledger                         hiding (mint, singleton)
import           Ledger.Constraints             as Constraints
import           Ledger.Ada                     as Ada
import           Ledger.Value                   as Value
import           Prelude                        (Show (..), String, Semigroup(..))
import           Text.Printf                    (printf)
                                                        
-- Defining minting function
mintDrop :: NFTMintParams -> Contract w NFTSchema Text ()
mintDrop mParams = do   
               utxos <- utxosAt $ mAddress mParams  
               pkHash <- Contract.ownPaymentPubKeyHash   
  
               case Map.keys utxos of
                 []       -> Contract.logError @String "No UTxO found on the provied Address!" 
                 oref : _ -> do 
                                let tkName     = uniqueName (mToken mParams) oref
                                    policyId   = tokenSymbol pkHash redeemer
                                    mintPolicy = (policy pkHash redeemer)
                                    
                                    -- Test purpose info
                                    tkId       = Builtins.sliceByteString 10 7 $ unTokenName tkName
                                    tkIndex    = Builtins.sliceByteString 0 4 tkId
                                    tkType     = Builtins.indexByteString tkId 5
                                    tkLevel    = Builtins.indexByteString tkId 6
                                    
                                    mAmt       = mAmount mParams
                                    redeemer   = (RP oref tkName mAmt)
                                    nft        = Value.singleton policyId tkName mAmt 
                                    val        = (Ada.lovelaceValueOf 2_000_000) <> nft
                                    lookups    = Constraints.mintingPolicy mintPolicy  <>
                                                 Constraints.unspentOutputs utxos
                                    tx         = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData redeemer) nft <>                                               Constraints.mustSpendPubKeyOutput oref <> 
                                                 Constraints.mustPayToPubKey (PaymentPubKeyHash $ Maybe.fromJust $ toPubKeyHash $ mReceiver mParams) val
                                
                                Contract.logInfo @String $ printf "MINT"
                                Contract.logInfo @String $ printf "TokenName length %d" (Builtins.lengthOfByteString $ unTokenName tkName) 
                                Contract.logInfo @String $ printf "ID %s" (show $ tkId)
                                Contract.logInfo @String $ printf "Index %s" (show $ tkIndex)
                                Contract.logInfo @String $ printf "Type %s" (show $ tkType)
                                Contract.logInfo @String $ printf "Level %s" (show $ tkLevel)
                                
                                ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

                                Contract.logInfo @String $ printf "Forged %s" (show val)             

-- Defining upgrading function
mintUpgrade :: NFTUpgradeParams -> Contract w NFTSchema Text ()
mintUpgrade uParams = do   
               utxos <- utxosAt $ uAddress uParams 
               Contract.logInfo @String $ printf "UTxOs count %s" (show $ length (Map.keys utxos))
               
               case Map.assocs utxos of
                 []                        -> Contract.logError @String "No UTxO found on the provied Address!" 
                 [_]                       -> Contract.logError @String "Only one UTxO found on the provied Address!" 
                 [_,_]                     -> Contract.logError @String "Only two UTxOs found on the provied Address!"
                 (or1,ot1) : (or2,ot2) : (or3,ot3) : _ -> do 
                                let tkName     = uniqueName (uToken uParams) or1
                                    pkhMinter  = PaymentPubKeyHash $ Maybe.fromJust $ toPubKeyHash $ uMinter uParams
                                    prevNft    = buildPreviousLevelString tkName
                                    serum      = buildSerumString tkName                  
                                    
                                    policyId   = tokenSymbol pkhMinter redeemer
                                    mintPolicy = (policy pkhMinter redeemer)
                                    
                                    
                                    -- Test purpose info
                                    tkId       = Builtins.sliceByteString 10 7 $ unTokenName tkName
                                    tkIndex    = Builtins.sliceByteString 0 4 tkId
                                    tkType     = Builtins.indexByteString (unTokenName tkName) 9
                                    tkLevel    = Builtins.indexByteString tkId 6                                    
                                    
                                    mAmt       = 1
                                    redeemer   = (RP or1 tkName mAmt)
                                    nft        = Value.singleton policyId tkName mAmt 
                                    val        = (Ada.lovelaceValueOf 2_000_000) <> nft
                                    lookups    = Constraints.mintingPolicy mintPolicy <>
                                                 Constraints.unspentOutputs utxos
                                                 
                                    tx         = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData redeemer) nft <>                                               Constraints.mustSpendPubKeyOutput or1 <>
                                                 Constraints.mustSpendPubKeyOutput or2 <>
                                                 Constraints.mustSpendPubKeyOutput or3
                                    
                                Contract.logInfo @String $ printf "UPGRADE"
                                Contract.logInfo @String $ printf "TokenName length %d" (Builtins.lengthOfByteString $ unTokenName tkName) 
                                Contract.logInfo @String $ printf "Prev. Level %s" (show $ prevNft) 
                                Contract.logInfo @String $ printf "Serum %s" (show $ serum) 
                                Contract.logInfo @String $ printf "ID %s" (show $ tkId)
                                Contract.logInfo @String $ printf "Index %s" (show $ tkIndex)
                                Contract.logInfo @String $ printf "Type %s" (show $ tkType)
                                Contract.logInfo @String $ printf "Level %s" (show $ tkLevel)
                               
                                Contract.logInfo @String $ printf "UTxO 1 has token %s" (show $ hasTokenAtUtxo (toTxOut ot1) policyId prevNft)
                                Contract.logInfo @String $ printf "UTxO 1 has serum %s" (show $ hasTokenAtUtxo (toTxOut ot1) policyId serum)
                                Contract.logInfo @String $ printf "UTxO 2 has token %s" (show $ hasTokenAtUtxo (toTxOut ot2) policyId prevNft)
                                Contract.logInfo @String $ printf "UTxO 2 has serum %s" (show $ hasTokenAtUtxo (toTxOut ot2) policyId serum)
                                Contract.logInfo @String $ printf "UTxO 3 has token %s" (show $ hasTokenAtUtxo (toTxOut ot3) policyId prevNft)
                                Contract.logInfo @String $ printf "UTxO 3 has serum %s" (show $ hasTokenAtUtxo (toTxOut ot3) policyId serum)

                                ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                Contract.logInfo @String $ printf "Forged %s" (show val) 

-- Defining minting and dropping input parameters
data NFTMintParams = NFTMintParams
    { mToken    :: !TokenName
    , mAmount   :: !Integer
    , mAddress  :: !Address
    , mReceiver :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)

-- Defining nft upgrading input parameters
data NFTUpgradeParams = NFTUpgradeParams
    { uToken    :: !TokenName
    , uAddress  :: !Address
    , uMinter   :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)

-- Defining endpoint input binding
type NFTSchema = Endpoint "mint" NFTMintParams .\/ Endpoint "upgrade" NFTUpgradeParams 


-- Defining contract interaction endpoints
endpoints :: Contract () NFTSchema Text ()
endpoints = awaitPromise (mint' `select` upgrade') >> endpoints
  where
    mint' = endpoint @"mint" mintDrop
    upgrade' = endpoint @"upgrade" mintUpgrade 
    
    
