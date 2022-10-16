{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module CustomRedeemer where

import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger             (TxOutRef, TokenName)

-- Defining custom redeemer
data RedeemerParam = RP
                   { rOutRef     :: TxOutRef
                   , rTokenName  :: TokenName
                   , rMintAmount :: Integer
                   }  

PlutusTx.makeIsDataIndexed ''RedeemerParam [('RP,0)]
