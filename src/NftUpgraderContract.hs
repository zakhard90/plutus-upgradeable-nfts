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
-- [x] Traverse tx inputs to get utxos with genesis NFT and Serum
-- [x] Add minting condition to allow users to mint upgraded NFT with genesis NFT and Serum
-- [x] Verify that input tokens meet the required condition, i.e. L1 + S1 = L2
-- [ ] Verify that the correct token is minted on upgrade
-- [ ] Generalize upgrade Tx construction to handle a different number of input UTxOs
-- [ ] Burn the input tokens
-- [x] Generalize for L2, L3, etc..
-- [?] Payment for the genesis mint
-- [?] Multisig account for admin wallet minting genesis collection

module NftUpgraderContract where

import           Control.Monad                  hiding (fmap)
import           Data.Aeson                     (ToJSON, FromJSON)
import           Data.Map                       as Map
import           Data.Text                      (Text, pack)
import           Data.Void                      (Void)
import qualified Data.Maybe                     as Maybe
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
import qualified Ledger.Contexts                as Contexts
import           Prelude                        (IO, Show (..), String, Semigroup(..))
import           Text.Printf                    (printf)
import           Wallet.Emulator.Wallet
import           CustomRedeemer

-- ON-CHAIN

-- Types
-- L -> 76
-- S -> 83

-- Level
-- 1 -> 49
-- 2 -> 50

-- _ -> 95

-- Defining Minting Validator  

{-# INLINABLE tokenPolicy #-}
tokenPolicy :: PaymentPubKeyHash -> RedeemerParam -> ScriptContext -> Bool
tokenPolicy pkHash (RP outRef tkName mintAmount) sContext = traceIfFalse "Can not give the NFT"  givingPath 
       where
           givingPath :: Bool
           givingPath = traceIfFalse "UTxO not consumed" hasUTxO &&
                        traceIfFalse "Wrong ammount minted" checkMintedAmount &&
                        traceIfFalse "Not original or upgrade mint" checkIfOriginalOrUpgrade && 
                        traceIfFalse "Reciever did not get the NFT" checkIfRecieved
                        
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
           isSerum = (==) tokenType serumType

           isPlayable :: Bool
           isPlayable =  (==) tokenType playableType     
           
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
           checkIfRecieved :: Bool
           checkIfRecieved = True        


{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

serumType :: Integer
serumType = 83
           
playableType :: Integer
playableType = 76

policy :: PaymentPubKeyHash -> RedeemerParam -> Scripts.MintingPolicy
policy pkHash redeemer = mkMintingPolicyScript $ 
                $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . tokenPolicy ||]) 
                  `PlutusTx.applyCode`
                    PlutusTx.liftCode pkHash

-- Extracting policy ID    
{-# INLINABLE tokenSymbol #-}
tokenSymbol :: PaymentPubKeyHash -> RedeemerParam -> CurrencySymbol
tokenSymbol pkHash = scriptCurrencySymbol . policy pkHash

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
                                 
{-# INLINABLE hasTokenAtUtxo #-}
hasTokenAtUtxo :: TxOut -> CurrencySymbol -> BuiltinByteString -> Bool
hasTokenAtUtxo tx policyId tkString = hasTokenAtFlatValue (flattenValue $ txOutValue $ tx) policyId tkString  

hasTokenAtFlatValue :: [(CurrencySymbol, TokenName, Integer)] -> CurrencySymbol -> BuiltinByteString -> Bool
hasTokenAtFlatValue sets policyId tkString = case sets of
                                                 []         -> False
                                                 [v]        -> isMatchingFlatValue v policyId tkString
                                                 (v : vs)   -> if isMatchingFlatValue v policyId tkString
                                                               then True
                                                               else hasTokenAtFlatValue vs policyId tkString         

isMatchingFlatValue :: (CurrencySymbol, TokenName, Integer) -> CurrencySymbol -> BuiltinByteString -> Bool
isMatchingFlatValue (cs, tkn, amt) policyId tkString = cs == policyId && amt > 0 && isMatchingToken tkn tkString
                                                
isMatchingToken :: TokenName -> BuiltinByteString -> Bool
isMatchingToken tkName tkString = Builtins.equalsByteString (Builtins.sliceByteString 0 17 $ unTokenName tkName) tkString                                   


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
     
buildPreviousLevelString :: TokenName -> BuiltinByteString
buildPreviousLevelString tkName = Builtins.appendByteString base tLevel
   where 
     base   = Builtins.sliceByteString 0 16 $ unTokenName tkName 
     tLevel = Builtins.consByteString ((Builtins.indexByteString (Builtins.sliceByteString 16 1 $ unTokenName tkName) 0) - 1) Builtins.emptyByteString               
                                                        
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

-- SIMULATOR

test :: IO ()
test = runEmulatorTraceIO $ do

    let s1 = "Halloween_0000_S1_"
        qs = 5
        t1 = "Halloween_0001_L1_"
        q1 = 1
        t2 = "Halloween_0002_L1_"
        q2 = 1
        u1 = "Halloween_0001_L2_"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    
    callEndpoint @"mint" h1 $ NFTMintParams
        { mToken    = t1
        , mAmount   = q1
        , mAddress  = mockWalletAddress w1
        , mReceiver = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ NFTMintParams
        { mToken    = s1
        , mAmount   = qs
        , mAddress  = mockWalletAddress w1
        , mReceiver = mockWalletAddress w2
        }   
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ NFTMintParams
        { mToken    = t2
        , mAmount   = q2
        , mAddress  = mockWalletAddress w1
        , mReceiver = mockWalletAddress w1
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"upgrade" h2 $ NFTUpgradeParams
        { uToken    = u1
        , uAddress  = mockWalletAddress w2
        , uMinter   = mockWalletAddress w1
        }
    void $ Emulator.waitNSlots 1               
