{-# LANGUAGE NumericUnderscores     #-}

module NftUpgrader.Utility where

import           PlutusTx.Prelude               hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins              as Builtins
import           Ledger                         hiding (mint, singleton)
import           Ledger.Ada                     as Ada
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.Scripts    (Language (..))
import qualified Plutus.Script.Utils.Scripts    as Scripts
import           Ledger.Value                   as Value
import           Prelude                        (Show)

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
           idx = Builtins.consByteString (txOutRefIdx ou) Builtins.emptyByteString

-- Verify if a Tx output has a some lovelace value                                 
{-# INLINABLE hasLovelaveAtUtxo #-}
hasLovelaveAtUtxo :: PlutusV2.TxOut -> Bool
hasLovelaveAtUtxo tx = case flattenValue $ PlutusV2.txOutValue tx of
                        [(cur, _, amount)]  ->  cur == Ada.adaSymbol && amount > 5_000_000
                        _                   ->  False

-- Verify if a Tx output has a specific currency and token                                 
{-# INLINABLE hasTokenAtUtxo #-}
hasTokenAtUtxo :: PlutusV2.TxOut -> CurrencySymbol -> BuiltinByteString -> Bool
hasTokenAtUtxo tx policyId tkString = hasTokenAtFlatValue (flattenValue $ PlutusV2.txOutValue tx) policyId tkString  

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

-- Get custom token name from value                                               
{-# INLINABLE getTokenNameFromValue #-}
getTokenNameFromValue :: Value -> TokenName
getTokenNameFromValue val = case flattenValue val of
                             [] -> TokenName $ Builtins.emptyByteString
                             vs -> getTokenNameFromFlatValue vs 

-- Get custom token name from flat value
getTokenNameFromFlatValue :: [(CurrencySymbol, TokenName, Integer)] -> TokenName
getTokenNameFromFlatValue vals = case vals of
                                  []                 -> TokenName $ Builtins.emptyByteString
                                  [(_, tkn, _)]     -> tkn
                                  (v@(_, tkn, _):vs) -> if isTokenFlatValue v 
                                                        then tkn
                                                        else getTokenNameFromFlatValue vs

-- Verify if flat value entry is from a custom token
isTokenFlatValue :: (CurrencySymbol, TokenName, Integer) -> Bool
isTokenFlatValue (cs, _, _) = cs /= Ada.adaSymbol

-- Get entry with custom token from flat value
{-# INLINABLE getMintedTokenValue #-}
getMintedTokenValue :: [(CurrencySymbol, TokenName, Integer)] -> (CurrencySymbol, TokenName, Integer)
getMintedTokenValue vals = case vals of 
                             []                       -> (Ada.adaSymbol, Ada.adaToken, 0)
                             [v]                      -> v
                             (v@(_, _, amount): vs)   -> if amount > 0 then v else getMintedTokenValue vs 

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
     
getVersionedScript :: a -> Scripts.Versioned a
getVersionedScript s = Scripts.Versioned s PlutusV1
