module Main(main) where

import Plutarch.Prelude
import Plutarch (compile)
import Plutarch.Script (serialiseScript)
import Plutarch.LedgerApi.V3 (PScriptContext)

import Data.ByteString.Short qualified as SBS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as Text

myScript :: forall (s :: S). Term s (PScriptContext :--> PUnit)
myScript = plam $ \_ctx -> unTermCont $ do
  pure $ perror

main :: IO ()
main = do
  let
    compiled =
      case compile mempty myScript of
        Right script -> script
        Left err -> error $ show err

  putStr "Raw script hex: "
  print $ Text.decodeUtf8 $ Base16.encode $ SBS.fromShort $ serialiseScript $ compiled
