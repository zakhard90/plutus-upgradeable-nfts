{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module NftUpgrader.OnChain where

import           NftUpgrader.PolicyRedeemer  
import           NftUpgrader.Utility

import qualified PlutusTx
import           PlutusTx.Prelude               hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins              as Builtins
import           Ledger                         hiding (mint, singleton)
import qualified Ledger.Typed.Scripts           as Scripts
import           Ledger.Value                   as Value
import qualified Ledger.Contexts                as Contexts

-- Defining Minting Validator
{-# INLINABLE tokenPolicy #-}
tokenPolicy :: PaymentPubKeyHash -> RedeemerParam -> ScriptContext -> Bool
tokenPolicy pkHash (RP outRef tkName mintAmount) sContext = traceIfFalse "Can not give the NFT"  givingPath 
       where
           givingPath :: Bool
           givingPath = traceIfFalse "UTxO not consumed" hasUTxO &&
                        traceIfFalse "Wrong ammount minted" checkMintedAmount &&
                        traceIfFalse "Not original or upgrade mint" checkIfOriginalOrUpgrade && 
                        traceIfFalse "Produced outputs are not correct" checkProducedOutput
                        
           info :: TxInfo
           info = scriptContextTxInfo sContext
           
           utxoInputs :: [TxInInfo]
           utxoInputs = txInfoInputs info

           curSymbol :: CurrencySymbol
           curSymbol = Contexts.ownCurrencySymbol sContext

           utxoOutputs :: [TxOut]
           utxoOutputs = txInfoOutputs info

           hasUTxO :: Bool
           hasUTxO = any (\utxo -> txInInfoOutRef utxo == outRef) utxoInputs
  
           checkMintedAmount :: Bool
           checkMintedAmount = case flattenValue (txInfoMint info) of
              [(_, tkName', amount)]   ->  tkName' == tkName && amount == mintAmount
              _                        ->  False  
           
           checkIfOriginalOrUpgrade :: Bool
           checkIfOriginalOrUpgrade = checkIfOriginal || checkIfUpgrade          
              
           checkIfOriginal :: Bool
           checkIfOriginal = checkIfSigned && checkIfGenesis
              
           checkIfSigned :: Bool
           checkIfSigned = txSignedBy (scriptContextTxInfo sContext) $ unPaymentPubKeyHash pkHash
           
           tokenId :: BuiltinByteString
           tokenId = Builtins.sliceByteString 10 7 $ unTokenName tkName 
           
           tokenType :: Integer
           tokenType = Builtins.indexByteString tokenId 5
           
           tokenLevel :: Integer
           tokenLevel = Builtins.indexByteString tokenId 6
                     
           isSerum :: Bool
           isSerum = (==) tokenType 83

           isPlayable :: Bool
           isPlayable =  (==) tokenType 76
           
           isNft :: Bool
           isNft =  isPlayable && (==) mintAmount 1           
           
           isFirstLevel :: Bool
           isFirstLevel =  (==) tokenLevel 49   
           
           isNextLevel :: Bool
           isNextLevel =  (>) tokenLevel 49    
           
           isFirstLevelNft :: Bool
           isFirstLevelNft = isNft && isFirstLevel 
           
           checkIfGenesis :: Bool
           checkIfGenesis = isSerum || isFirstLevelNft
                                   
           checkIfUpgrade :: Bool
           checkIfUpgrade = isNextLevel &&
                            traceIfFalse "Not enough UTxO inputs"   hasEnoughInputs &&
                            traceIfFalse "UTxO has no nft input"    hasNftInput &&
                            traceIfFalse "UTxO has no serum input"  hasSerumInput   
           
           ptkString :: BuiltinByteString
           ptkString =  buildPreviousLevelString tkName
           
           serumString :: BuiltinByteString
           serumString =  buildSerumString tkName
                      
           hasEnoughInputs :: Bool
           hasEnoughInputs = (>) (length utxoInputs) 2
                        
           hasNftInput :: Bool
           hasNftInput = any (\utxo -> isInputNftCurrency utxo) utxoInputs
           
           isInputNftCurrency :: TxInInfo -> Bool
           isInputNftCurrency = (\tx ->  hasTokenAtUtxo tx curSymbol ptkString) . txInInfoResolved 
           
           hasSerumInput :: Bool
           hasSerumInput = any (\utxo -> isInputSerum utxo) utxoInputs
           
           isInputSerum :: TxInInfo -> Bool
           isInputSerum = (\tx ->  hasTokenAtUtxo tx curSymbol serumString) . txInInfoResolved 
           
           -- todo
           checkProducedOutput :: Bool
           checkProducedOutput = (>) (length utxoOutputs) 0        

-- Defining policy script
policy :: PaymentPubKeyHash -> RedeemerParam -> Scripts.MintingPolicy
policy pkHash redeemer = mkMintingPolicyScript $ 
                $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . tokenPolicy ||]) 
                  `PlutusTx.applyCode`
                    PlutusTx.liftCode pkHash

-- Extracting policy ID    
{-# INLINABLE tokenSymbol #-}
tokenSymbol :: PaymentPubKeyHash -> RedeemerParam -> CurrencySymbol
tokenSymbol pkHash = scriptCurrencySymbol . policy pkHash

-- Byte values

-- Type
-- L -> 76
-- S -> 83

-- Level
-- 1 -> 49
-- 2 -> 50

-- _ -> 95
                   
