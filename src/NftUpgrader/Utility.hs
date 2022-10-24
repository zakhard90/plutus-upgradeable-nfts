module NftUpgrader.Utility where

import           Data.Text                      (pack)
import           PlutusTx.Prelude               hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins              as Builtins
import           Ledger                         hiding (mint, singleton)
import           Ledger.Value                   as Value
import           Prelude                        (Show (..))

-- Enforce a really unique value for the token name
{-# INLINABLE uniqueName #-}
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

-- Verify if a Tx output has a specific currency and token                                 
{-# INLINABLE hasTokenAtUtxo #-}
hasTokenAtUtxo :: TxOut -> CurrencySymbol -> BuiltinByteString -> Bool
hasTokenAtUtxo tx policyId tkString = hasTokenAtFlatValue (flattenValue $ txOutValue $ tx) policyId tkString  

-- Verify if a flat value has a specific currency and token 
{-# INLINABLE hasTokenAtFlatValue #-}
hasTokenAtFlatValue :: [(CurrencySymbol, TokenName, Integer)] -> CurrencySymbol -> BuiltinByteString -> Bool
hasTokenAtFlatValue sets policyId tkString = case sets of
                                                 []         -> False
                                                 [v]        -> isMatchingFlatValue v policyId tkString
                                                 (v : vs)   -> if isMatchingFlatValue v policyId tkString
                                                               then True
                                                               else hasTokenAtFlatValue vs policyId tkString         

-- Verify if a flat value entry has a specific currency and token 
isMatchingFlatValue :: (CurrencySymbol, TokenName, Integer) -> CurrencySymbol -> BuiltinByteString -> Bool
isMatchingFlatValue (cs, tkn, amt) policyId tkString = cs == policyId && amt > 0 && isMatchingToken tkn tkString

-- Verify if the initial part of a token name matches a desired string value                                               
isMatchingToken :: TokenName -> BuiltinByteString -> Bool
isMatchingToken tkName tkString = Builtins.equalsByteString (Builtins.sliceByteString 0 17 $ unTokenName tkName) tkString                                   

-- Compose a string corresponding to a specific serum required to perform an nft upgrade
{-# INLINABLE buildSerumString #-}
buildSerumString :: TokenName -> BuiltinByteString
buildSerumString tkName = Builtins.appendByteString base $ 
                          Builtins.appendByteString tIndex $
                          Builtins.appendByteString tType tLevel
   where 
     base   = Builtins.sliceByteString 0 10 $ unTokenName tkName
     tIndex = Builtins.consByteString 48 $ 
              Builtins.consByteString 48 $
              Builtins.consByteString 48 $
              Builtins.consByteString 48 Builtins.emptyByteString
     tType  = Builtins.consByteString 95 $ 
              Builtins.consByteString 83 Builtins.emptyByteString 
     tLevel = Builtins.consByteString ((Builtins.indexByteString (Builtins.sliceByteString 16 1 $ unTokenName tkName) 0) - 1) Builtins.emptyByteString  
 
 -- Compose a string corresponding to a the token name of the previous level token required to perform an nft upgrade  
{-# INLINABLE buildPreviousLevelString #-}     
buildPreviousLevelString :: TokenName -> BuiltinByteString
buildPreviousLevelString tkName = Builtins.appendByteString base tLevel
   where 
     base   = Builtins.sliceByteString 0 16 $ unTokenName tkName 
     tLevel = Builtins.consByteString ((Builtins.indexByteString (Builtins.sliceByteString 16 1 $ unTokenName tkName) 0) - 1) Builtins.emptyByteString               
