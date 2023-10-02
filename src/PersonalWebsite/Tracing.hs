{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module PersonalWebsite.Tracing (
    traceHandler,
    withTracing,
    runTracing,
    getTracerProvider,
    Traced,
    Tracing (..),
    TracedAPI,
    inSpan',
) where

import Data.Vault.Lazy
import GHC.Exception
import GHC.TypeLits
import Network.Wai
import qualified OpenTelemetry.Context as Otel
import qualified OpenTelemetry.Context.ThreadLocal as Otel
import qualified OpenTelemetry.Trace as Otel
import qualified OpenTelemetry.Trace.Core as Otel
import qualified OpenTelemetry.Trace.Monad as Otel
import Polysemy
import Polysemy.Input
import Polysemy.Resource
import Relude
import Servant
import Servant.Server.Internal.Delayed

data Tracing m a where
    InSpan :: CallStack -> Text -> Otel.SpanArguments -> m a -> Tracing m a
    WithSpan :: Otel.Span -> m a -> Tracing m a
    CurrentSpan :: Tracing m (Maybe Otel.Span)
    WithTracer :: Otel.Tracer -> m a -> Tracing m a
    GetTracer :: Tracing m Otel.Tracer
    GetTracerProvider :: Tracing m Otel.TracerProvider

makeSem ''Tracing

instance (Member (Input Otel.Tracer) r) => Otel.MonadTracer (Sem r) where getTracer = input

data Traced

type family TracedAPI api where
    TracedAPI (a :<|> b) = TracedAPI a :<|> TracedAPI b
    TracedAPI a = Traced :> a

instance
    (HasServer api ctx, HasContextEntry ctx (Key (Otel.Tracer, Otel.Span))) =>
    HasServer (Traced :> api) ctx
    where
    type ServerT (Traced :> api) m = Maybe (Otel.Tracer, Otel.Span) -> ServerT api m
    route _ ctx del =
        route
            (Proxy @api)
            ctx
            (passToServer del (lookup (getContextEntry ctx) . vault))
    hoistServerWithContext _ pc f a t = hoistServerWithContext (Proxy @api) pc f (a t)

class Nameable api where
    name :: Text

instance (KnownSymbol sym, Nameable api) => Nameable (sym :> api) where
    name = toText (symbolVal @sym Proxy) <> "/" <> name @api

instance Nameable api => Nameable (ReqBody types body :> api) where
    name = name @api

instance Nameable (Post types resp) where
    name = " POST"

instance Nameable (Get types resp) where
    name = " GET"

instance Nameable (Put types resp) where
    name = " PUT"

instance Nameable (Delete types resp) where
    name = " DELETE"

instance Nameable api => Nameable (QueryParam' mods sym t :> api) where
    name = name @api

instance (KnownSymbol sym, Nameable api) => Nameable (Capture' mods sym t :> api) where
    name = "{" <> toText (symbolVal @sym Proxy) <> "}/" <> name @api

traceHandler ::
    forall api r a.
    Nameable api =>
    Proxy api ->
    Members [Tracing, Embed IO] r =>
    Maybe (Otel.Tracer, Otel.Span) ->
    Sem r a ->
    Sem r a
traceHandler _ (Just (t, p)) a = do
    Otel.updateName p (name @api)
    withSpan p $ withTracer t a
traceHandler _ Nothing a = inSpan' (name @api) (Otel.defaultSpanArguments{Otel.kind = Otel.Server}) a

withTracing ::
    forall api r.
    ( Members '[Tracing, Embed IO] r
    , HasServer api '[]
    , Nameable api
    ) =>
    ServerT api (Sem r) ->
    ServerT (Traced :> api) (Sem r)
withTracing s mt =
    hoistServer @api @(Sem r) @(Sem r) Proxy (traceHandler @api Proxy mt) s

inSpan' :: Members '[Tracing] r => Text -> Otel.SpanArguments -> Sem r a -> Sem r a
inSpan' = inSpan callStack

runTracing ::
    forall r a.
    Members [Resource, Embed IO] r =>
    Otel.TracerProvider ->
    Sem (Tracing : r) a ->
    Sem r a
runTracing tp = runTracing' Nothing Nothing
  where
    runTracing' :: forall b. Maybe Otel.Tracer -> Maybe Otel.Span -> Sem (Tracing : r) b -> Sem r b
    runTracing' mt ms =
        interpretH \case
            GetTracerProvider -> pureT tp
            WithTracer t m -> do
                mm <- runT m
                raise $ runTracing' (Just t) ms mm
            WithSpan sp m -> do
                mm <- runT m
                raise $ runTracing' mt (Just sp) mm
            GetTracer ->
                pureT $
                    fromMaybe
                        (Otel.makeTracer tp "opentelemetry-haskell" (Otel.TracerOptions Nothing))
                        mt
            CurrentSpan -> pureT ms
            InSpan cs n args m -> do
                mm <- runT m
                t <- raise $ runTracing' mt ms getTracer
                bracket
                    startSpan
                    (uncurry endSpan)
                    \(_, s) -> raise (runTracing' (Just t) (Just s) mm) `onException` onError s
              where
                startSpan = do
                    ctx <- Otel.getContext
                    t <- raise $ runTracing' mt ms getTracer
                    s <- Otel.createSpanWithoutCallStack t ctx n args
                    Otel.adjustContext (Otel.insertSpan s)
                    Otel.whenSpanIsRecording s $ case getCallStack cs of
                        [] -> pass
                        (fn, loc) : _ -> do
                            Otel.addAttributes
                                s
                                $ fromList
                                    [ ("code.function", Otel.toAttribute $ toText fn)
                                    , ("code.namespace", Otel.toAttribute $ toText $ srcLocModule loc)
                                    , ("code.filepath", Otel.toAttribute $ toText $ srcLocFile loc)
                                    , ("code.lineno", Otel.toAttribute $ srcLocStartLine loc)
                                    , ("code.package", Otel.toAttribute $ toText $ srcLocPackage loc)
                                    ]
                    mParent <- raise $ runTracing' mt ms currentSpan
                    pure (mParent <|> Otel.lookupSpan ctx, s)
                onError s = Otel.setStatus s $ Otel.Error ""
                endSpan parent s = do
                    Otel.endSpan s Nothing
                    Otel.adjustContext \ctx -> maybe (Otel.removeSpan ctx) (`Otel.insertSpan` ctx) parent
