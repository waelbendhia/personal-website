module PersonalWebsite.KVCache (
    runKVWithCache,
) where

import Data.ByteString.Base58
import Data.Cache
import qualified OpenTelemetry.Trace as Otel
import PersonalWebsite.Tracing
import Polysemy
import Polysemy.KVStore
import Relude

runKVWithCache ::
    forall k v r a.
    Members [Embed IO, Tracing] r =>
    (k -> ByteString) ->
    Cache ByteString v ->
    Int ->
    Sem (KVStore k v : r) a ->
    Sem r a
runKVWithCache hash cache maxSize = interpret \case
    LookupKV k -> spanWithKey "LookupKV" k (embed . lookup cache)
    UpdateKV k Nothing -> spanWithKey "DeleteKV" k (embed . delete cache)
    UpdateKV k (Just v) -> spanWithKey "InsertKV" k \hashedKey -> embed $ do
        insert cache hashedKey v
        s <- size cache
        when (s > maxSize) (purgeExpired cache)
  where
    spanWithKey :: forall x. Text -> k -> (ByteString -> Sem r x) -> Sem r x
    spanWithKey name k f =
        let hashedKey = hash k
         in inSpan'
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
                $ f hashedKey
