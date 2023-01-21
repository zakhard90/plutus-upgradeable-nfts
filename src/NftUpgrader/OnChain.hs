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
import           Ledger.Address
import qualified Plutus.V2.Ledger.Api                          as PlutusV2
import qualified Plutus.Script.Utils.V1.Typed.Scripts          as TypedScripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts          as TypedScriptsV2
import qualified Plutus.Script.Utils.V2.Scripts                as ScriptsV2
import           Ledger.Value                                  as Value
import           Plutus.V2.Ledger.Contexts                     (ownCurrencySymbol, txSignedBy, txInInfoResolved)

-- Defining Minting Validator
{-# INLINABLE tokenPolicy #-}
tokenPolicy :: PaymentPubKeyHash -> RedeemerParam -> PlutusV2.ScriptContext -> Bool
tokenPolicy pkHash (RP outRef tkName mintAmount) sContext = traceIfFalse "Can not mint the NFT"  givingPath 
       where
           givingPath :: Bool
           givingPath = traceIfFalse "UTxO not consumed" hasUTxO &&
                        traceIfFalse "Wrong ammount minted" checkMintedToken &&
                        traceIfFalse "Not original or upgrade mint" checkIfOriginalOrUpgrade 
                        
           info :: PlutusV2.TxInfo
           info = PlutusV2.scriptContextTxInfo sContext
           
           utxoInputs :: [PlutusV2.TxInInfo]
           utxoInputs = PlutusV2.txInfoInputs info

           curSymbol :: CurrencySymbol
           curSymbol = ownCurrencySymbol sContext

           hasUTxO :: Bool
           hasUTxO = any (\utxo -> PlutusV2.txInInfoOutRef utxo == outRef) utxoInputs
  
           checkMintedToken :: Bool
           checkMintedToken = case getMintedTokenValue $ flattenValue (PlutusV2.txInfoMint info) of
              (_, tkName', amount)   ->  checkUniqueTokenName tkName' && amount == mintAmount              
           
           checkIfOriginalOrUpgrade :: Bool
           checkIfOriginalOrUpgrade = checkIfOriginal || checkIfUpgrade          
              
           checkIfOriginal :: Bool
           checkIfOriginal = checkIfSigned && checkIfGenesis
              
           checkIfSigned :: Bool
           checkIfSigned = txSignedBy (PlutusV2.scriptContextTxInfo sContext) $ unPaymentPubKeyHash pkHash
           
           checkUniqueTokenName :: TokenName -> Bool
           checkUniqueTokenName tn = tn == regenerateUniqueTokenName outRef tn
           
           regenerateUniqueTokenName :: PlutusV2.TxOutRef -> TokenName -> TokenName
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
           
           isInputNftCurrency :: PlutusV2.TxInInfo -> Bool
           isInputNftCurrency = (\tx ->  hasTokenAtUtxo tx curSymbol ptkString) . txInInfoResolved 
           
           hasSerumInput :: Bool
           hasSerumInput = any (\utxo -> isInputSerum utxo) utxoInputs
           
           isInputSerum :: PlutusV2.TxInInfo -> Bool
           isInputSerum = (\tx ->  hasTokenAtUtxo tx curSymbol serumString) . txInInfoResolved       

-- Defining policy script
policy :: PaymentPubKeyHash -> RedeemerParam -> TypedScripts.MintingPolicy
policy pkHash redeemer = PlutusV2.mkMintingPolicyScript $ 
                $$(PlutusTx.compile [|| TypedScriptsV2.mkUntypedMintingPolicy . tokenPolicy ||]) 
                  `PlutusTx.applyCode`
                    PlutusTx.liftCode pkHash

-- Extracting policy ID    
{-# INLINABLE tokenSymbol #-}
tokenSymbol :: PaymentPubKeyHash -> RedeemerParam -> CurrencySymbol
tokenSymbol pkHash = ScriptsV2.scriptCurrencySymbol . policy pkHash

-- Byte values

-- Type
-- L -> 76
-- S -> 83

-- Level
-- 1 -> 49
-- 2 -> 50

-- _ -> 95
                   
