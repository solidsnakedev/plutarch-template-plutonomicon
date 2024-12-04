module Main (main) where

import Plutarch.Internal.Term (compile)
import Plutarch.Prelude

import Plutarch.LedgerApi.V3 (PScriptContext)
import Plutarch.Script (serialiseScript)

import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as SBS
import Data.Text.Encoding qualified as Text

myScript :: forall (s :: S). Term s (PScriptContext :--> PUnit)
myScript = plam $ \_ctx -> unTermCont $ do
  pure perror

main :: IO ()
main = do
  let
    compiled =
      case compile mempty myScript of
        Right script -> script
        Left err -> error $ show err

  putStr "Raw script hex: "
  print $ Text.decodeUtf8 $ Base16.encode $ SBS.fromShort $ serialiseScript compiled
