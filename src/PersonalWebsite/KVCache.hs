module PersonalWebsite.KVCache (
    runKVWithCache,
) where

import Data.ByteString.Base58
import Data.Cache
import qualified OpenTelemetry.Trace as Otel
import PersonalWebsite.Tracing
import Polysemy
import Polysemy.KVStore
import Relude hiding (Reader, ask, runReader)

spanWithKey ::
    (Members '[Tracing] r) => Text -> ByteString -> Sem r x -> Sem r x
spanWithKey name hashedKey =
    inSpan'
        name
        Otel.defaultSpanArguments
            { Otel.attributes =
                one
                    ( "key"
                    , Otel.toAttribute
                        . decodeUtf8 @Text
                        . encodeBase58 bitcoinAlphabet
                        $ hashedKey
                    )
            }

runKVWithCache ::
    forall k v r a.
    (Members [Embed IO, Tracing] r) =>
    (k -> ByteString) ->
    Cache ByteString v ->
    Int ->
    Sem (KVStore k v : r) a ->
    Sem r a
runKVWithCache hash cache maxSize = interpret \case
    LookupKV k -> spanWithHashedKey "LookupKV" k (embed . lookup cache)
    UpdateKV k Nothing -> spanWithHashedKey "DeleteKV" k (embed . delete cache)
    UpdateKV k (Just v) -> spanWithHashedKey "InsertKV" k \hashedKey -> embed do
        insert cache hashedKey v
        s <- size cache
        when (s > maxSize) (purgeExpired cache)
  where
    spanWithHashedKey :: forall x. Text -> k -> (ByteString -> Sem r x) -> Sem r x
    spanWithHashedKey name k f =
        let hashedKey = hash k
         in spanWithKey name hashedKey (f hashedKey)
