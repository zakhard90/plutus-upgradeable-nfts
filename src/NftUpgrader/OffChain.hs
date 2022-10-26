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
               
               Contract.logInfo @String $ printf "MINT"
  
               case Map.keys utxos of
                 []       -> Contract.logError @String "No UTxO found on the provied Address!" 
                 oref : _ -> do 
                                let tkName     = uniqueName (mToken mParams) oref
                                    policyId   = tokenSymbol pkHash redeemer
                                    mintPolicy = (policy pkHash redeemer)
                                    
                                    -- Test purpose info
                                    tkId       = Builtins.sliceByteString 10 7 $ unTokenName tkName
                                    tkIndex    = Builtins.sliceByteString 0 4 tkId
                                    
                                    mAmt       = mAmount mParams
                                    redeemer   = (RP oref tkName mAmt)
                                    nft        = Value.singleton policyId tkName mAmt 
                                    val        = (Ada.lovelaceValueOf 2_000_000) <> nft
                                    lookups    = Constraints.mintingPolicy mintPolicy  <>
                                                 Constraints.unspentOutputs utxos
                                    tx         = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData redeemer) nft <>                                               Constraints.mustSpendPubKeyOutput oref <> 
                                                 Constraints.mustPayToPubKey (PaymentPubKeyHash $ Maybe.fromJust $ toPubKeyHash $ mReceiver mParams) val
                                
                                Contract.logInfo @String $ printf "TokenName length --> %d" (Builtins.lengthOfByteString $ unTokenName tkName) 
                                Contract.logInfo @String $ printf "ID               --> %s" (show $ tkId)
                                Contract.logInfo @String $ printf "Index            --> %s" (show $ tkIndex)
                                
                                ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

                                Contract.logInfo @String $ printf "Forged %s" (show val)             

-- Defining upgrading function
mintUpgrade :: NFTUpgradeParams -> Contract w NFTSchema Text ()
mintUpgrade uParams = do   
             utxos <- utxosAt $ uAddress uParams 
             
             Contract.logInfo @String $ printf "UPGRADE"
             Contract.logInfo @String $ printf "UTxOs count      --> %s" (show $ length (Map.keys utxos))
                              
             case Map.assocs utxos of
               []    -> Contract.logError @String "No UTxO found on the provied Address!" 
               [_]   -> Contract.logError @String "Only one UTxO found on the provied Address!" 
               [_,_] -> Contract.logError @String "Only two UTxOs found on the provied Address!"
               xs  -> do
                        let (or1,ot1) = PlutusTx.Prelude.head $ 
                                        PlutusTx.Prelude.filter (\(_,x) -> hasLovelaveAtUtxo (toTxOut x)) xs
                            (or2,ot2) = PlutusTx.Prelude.head $ 
                                        PlutusTx.Prelude.filter (\(_,x) -> hasTokenAtUtxo (toTxOut x) policyId prevNftStr) xs
                            (or3,ot3) = PlutusTx.Prelude.head $ 
                                        PlutusTx.Prelude.filter (\(_,x) -> hasTokenAtUtxo (toTxOut x) policyId serumStr) xs

                            inTkName  = getTokenNameFromValue $ txOutValue $ toTxOut ot2
                            inSrName  = getTokenNameFromValue $ txOutValue $ toTxOut ot3

                            pkhMinter  = PaymentPubKeyHash $ Maybe.fromJust $ toPubKeyHash $ uMinter uParams
                            tkName     = uniqueName (uToken uParams) or1
                            prevNftStr = buildPreviousLevelString tkName
                            serumStr   = buildSerumString tkName                  
                                     
                            policyId   = tokenSymbol pkhMinter redeemer
                            mintPolicy = (policy pkhMinter redeemer)
                                    
                            -- Test purpose info
                            tkId       = Builtins.sliceByteString 10 7 $ unTokenName tkName
                            tkIndex    = Builtins.sliceByteString 0 4 tkId                                  
                           
                            mAmt       = 1
                            redeemer   = (RP or1 tkName mAmt)
                            upNft      = Value.singleton policyId tkName mAmt 
                            inNft      = Value.singleton policyId inTkName $ negate mAmt
                            inSerum    = Value.singleton policyId inSrName $ negate mAmt  
                            val        = (Ada.lovelaceValueOf 2_000_000) <> upNft
                            lookups    = Constraints.mintingPolicy mintPolicy <>
                                         Constraints.unspentOutputs utxos
                                                 
                            tx         = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData redeemer) upNft <>                                       Constraints.mustSpendPubKeyOutput or1 <>
                                         Constraints.mustSpendPubKeyOutput or2 <>
                                         Constraints.mustSpendPubKeyOutput or3 <>
                                         Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData redeemer) inNft <>
                                         Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData redeemer) inSerum
                        
                        Contract.logInfo @String $ printf "TokenName length --> %d" (Builtins.lengthOfByteString $ unTokenName tkName) 
                        Contract.logInfo @String $ printf "ID               --> %s" (show $ tkId)
                        Contract.logInfo @String $ printf "Index            --> %s" (show $ tkIndex)
                        Contract.logInfo @String $ printf "Prev. Level      --> %s" (show $ prevNftStr) 
                        Contract.logInfo @String $ printf "Serum            --> %s" (show $ serumStr) 
                               
                        Contract.logInfo @String $ printf "UTxO 1 has ada   --> %s" (show $ hasLovelaveAtUtxo (toTxOut ot1))
                        Contract.logInfo @String $ printf "UTxO 2 has token --> %s" (show $ hasTokenAtUtxo (toTxOut ot2) policyId prevNftStr)
                        Contract.logInfo @String $ printf "UTxO 3 has serum --> %s" (show $ hasTokenAtUtxo (toTxOut ot3) policyId serumStr)

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
    
    
