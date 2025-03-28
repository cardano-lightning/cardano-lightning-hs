module CardanoLightning.Transaction.AUTxO where

import Cardano.Api as C
import qualified Data.List as List
import Data.Map.Strict as M

data AUTxO era = AUTxO {aTxIn :: C.TxIn, aTxOut :: C.TxOut C.CtxUTxO era}
    deriving (Show)

toUTxO :: AUTxO era -> C.UTxO era
toUTxO (AUTxO txin txout) = C.UTxO . M.fromList $ [(txin, txout)]

foldToUTxO :: (Foldable f) => f (AUTxO era) -> C.UTxO era
foldToUTxO =
    C.UTxO
        . foldMap (fromList . List.singleton . \(AUTxO txin txout) -> (txin, txout))
