{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module NftUpgrader.OnChain where

import           NftUpgrader.PolicyRedeemer  
import           NftUpgrader.Utility

import qualified PlutusTx
import           PlutusTx.Prelude                              hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins                             as Builtins
import           Ledger.Address                                ()
import           Plutus.V2.Ledger.Api                          (PubKeyHash, ScriptContext, TxInfo, TxInInfo, TxOutRef, scriptContextTxInfo, txInfoInputs, txInInfoOutRef, txInfoMint, mkMintingPolicyScript)
import           Plutus.Script.Utils.V2.Typed.Scripts          (MintingPolicy, mkUntypedMintingPolicy)
import           Plutus.Script.Utils.V2.Scripts                (scriptCurrencySymbol)
import           Ledger.Value                                  as Value
import           Plutus.V2.Ledger.Contexts                     (ownCurrencySymbol, txSignedBy, txInInfoResolved)

-- Defining Minting Validator
{-# INLINABLE tokenPolicy #-}
tokenPolicy :: PubKeyHash -> RedeemerParam -> ScriptContext -> Bool
tokenPolicy pkHash (RP outRef tkName mintAmount) sContext = traceIfFalse "Can not mint the NFT"  givingPath 
       where
           givingPath :: Bool
           givingPath = traceIfFalse "UTxO not consumed" hasUTxO &&
                        traceIfFalse "Wrong ammount minted" checkMintedToken &&
                        traceIfFalse "Not original or upgrade mint" checkIfOriginalOrUpgrade 
                        
           info :: TxInfo
           info = scriptContextTxInfo sContext
           
           utxoInputs :: [TxInInfo]
           utxoInputs = txInfoInputs info

           curSymbol :: CurrencySymbol
           curSymbol = ownCurrencySymbol sContext

           hasUTxO :: Bool
           hasUTxO = any (\utxo -> txInInfoOutRef utxo == outRef) utxoInputs
  
           checkMintedToken :: Bool
           checkMintedToken = case getMintedTokenValue $ flattenValue (txInfoMint info) of
              (_, tkName', amount)   ->  checkUniqueTokenName tkName' && amount == mintAmount              
           
           checkIfOriginalOrUpgrade :: Bool
           checkIfOriginalOrUpgrade = checkIfOriginal || checkIfUpgrade          
              
           checkIfOriginal :: Bool
           checkIfOriginal = checkIfSigned && checkIfGenesis
              
           checkIfSigned :: Bool
           checkIfSigned = txSignedBy (scriptContextTxInfo sContext) pkHash          
           
           checkUniqueTokenName :: TokenName -> Bool
           checkUniqueTokenName tn = tn == regenerateUniqueTokenName outRef tn
           
           regenerateUniqueTokenName :: TxOutRef -> TokenName -> TokenName
           regenerateUniqueTokenName oref tn = uniqueName (TokenName (Builtins.sliceByteString 0 18 $ unTokenName tn)) oref
           
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

-- Defining policy script
policy :: PubKeyHash -> RedeemerParam -> MintingPolicy
policy pkHash redeemer = mkMintingPolicyScript $ 
                $$(PlutusTx.compile [|| mkUntypedMintingPolicy . tokenPolicy ||]) 
                  `PlutusTx.applyCode`
                    PlutusTx.liftCode pkHash

-- Extracting policy ID    
{-# INLINABLE tokenSymbol #-}
tokenSymbol :: PubKeyHash -> RedeemerParam -> CurrencySymbol
tokenSymbol pkHash = scriptCurrencySymbol . policy pkHash

-- Byte values

-- Type
-- L -> 76
-- S -> 83

-- Level
-- 1 -> 49
-- 2 -> 50

-- _ -> 95
                   
