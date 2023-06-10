{-
   ElevenLabs API Documentation

   This is the documentation for the ElevenLabs API. You can use this API to use our service programmatically, this is done by using your xi-api-key. <br/> You can view your xi-api-key using the 'Profile' tab on https://beta.elevenlabs.io. Our API is experimental so all endpoints are subject to change.

   OpenAPI Version: 3.0.2
   ElevenLabs API Documentation API version: 1.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : ElevenLabsAPIDocumentation.Core
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module ElevenLabsAPIDocumentation.Core where

import ElevenLabsAPIDocumentation.MimeTypes
import ElevenLabsAPIDocumentation.Logging

import qualified Control.Arrow as P (left)
import qualified Control.DeepSeq as NF
import qualified Control.Exception.Safe as E
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.Lazy as BL64
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.CaseInsensitive as CI
import qualified Data.Data as P (Data, Typeable, TypeRep, typeRep)
import qualified Data.Foldable as P
import qualified Data.Ix as P
import qualified Data.Kind as K (Type)
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as TI
import qualified Data.Time.ISO8601 as TI
import qualified GHC.Base as P (Alternative)
import qualified Lens.Micro as L
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Types as NH
import qualified Prelude as P
import qualified Text.Printf as T
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Control.Applicative ((<|>))
import Control.Applicative (Alternative)
import Control.Monad.Fail (MonadFail)
import Data.Function ((&))
import Data.Foldable(foldlM)
import Data.Monoid ((<>))
import Data.Text (Text)
import Prelude (($), (.), (&&), (<$>), (<*>), Maybe(..), Bool(..), Char, String, fmap, mempty, pure, return, show, IO, Monad, Functor, maybe)

-- * ElevenLabsAPIDocumentationConfig

-- |
data ElevenLabsAPIDocumentationConfig = ElevenLabsAPIDocumentationConfig
  { configHost  :: BCL.ByteString -- ^ host supplied in the Request
  , configUserAgent :: Text -- ^ user-agent supplied in the Request
  , configLogExecWithContext :: LogExecWithContext -- ^ Run a block using a Logger instance
  , configLogContext :: LogContext -- ^ Configures the logger
  , configAuthMethods :: [AnyAuthMethod] -- ^ List of configured auth methods
  , configValidateAuthMethods :: Bool -- ^ throw exceptions if auth methods are not configured
  , configQueryExtraUnreserved :: B.ByteString -- ^ Configures additional querystring characters which must not be URI encoded, e.g. '+' or ':'
  }

-- | display the config
instance P.Show ElevenLabsAPIDocumentationConfig where
  show c =
    T.printf
      "{ configHost = %v, configUserAgent = %v, ..}"
      (show (configHost c))
      (show (configUserAgent c))

-- | constructs a default ElevenLabsAPIDocumentationConfig
--
-- configHost:
--
-- @http://localhost@
--
-- configUserAgent:
--
-- @"elevenlabs-api-documentation/0.1.0.0"@
--
newConfig :: IO ElevenLabsAPIDocumentationConfig
newConfig = do
    logCxt <- initLogContext
    return $ ElevenLabsAPIDocumentationConfig
        { configHost = "http://localhost"
        , configUserAgent = "elevenlabs-api-documentation/0.1.0.0"
        , configLogExecWithContext = runDefaultLogExecWithContext
        , configLogContext = logCxt
        , configAuthMethods = []
        , configValidateAuthMethods = True
        , configQueryExtraUnreserved = ""
        }

-- | updates config use AuthMethod on matching requests
addAuthMethod :: AuthMethod auth => ElevenLabsAPIDocumentationConfig -> auth -> ElevenLabsAPIDocumentationConfig
addAuthMethod config@ElevenLabsAPIDocumentationConfig {configAuthMethods = as} a =
  config { configAuthMethods = AnyAuthMethod a : as}

-- | updates the config to use stdout logging
withStdoutLogging :: ElevenLabsAPIDocumentationConfig -> IO ElevenLabsAPIDocumentationConfig
withStdoutLogging p = do
    logCxt <- stdoutLoggingContext (configLogContext p)
    return $ p { configLogExecWithContext = stdoutLoggingExec, configLogContext = logCxt }

-- | updates the config to use stderr logging
withStderrLogging :: ElevenLabsAPIDocumentationConfig -> IO ElevenLabsAPIDocumentationConfig
withStderrLogging p = do
    logCxt <- stderrLoggingContext (configLogContext p)
    return $ p { configLogExecWithContext = stderrLoggingExec, configLogContext = logCxt }

-- | updates the config to disable logging
withNoLogging :: ElevenLabsAPIDocumentationConfig -> ElevenLabsAPIDocumentationConfig
withNoLogging p = p { configLogExecWithContext =  runNullLogExec}

-- * ElevenLabsAPIDocumentationRequest

-- | Represents a request.
--
--   Type Variables:
--
--   * req - request operation
--   * contentType - 'MimeType' associated with request body
--   * res - response model
--   * accept - 'MimeType' associated with response body
data ElevenLabsAPIDocumentationRequest req contentType res accept = ElevenLabsAPIDocumentationRequest
  { rMethod  :: NH.Method   -- ^ Method of ElevenLabsAPIDocumentationRequest
  , rUrlPath :: [BCL.ByteString] -- ^ Endpoint of ElevenLabsAPIDocumentationRequest
  , rParams   :: Params -- ^ params of ElevenLabsAPIDocumentationRequest
  , rAuthTypes :: [P.TypeRep] -- ^ types of auth methods
  }
  deriving (P.Show)

-- | 'rMethod' Lens
rMethodL :: Lens_' (ElevenLabsAPIDocumentationRequest req contentType res accept) NH.Method
rMethodL f ElevenLabsAPIDocumentationRequest{..} = (\rMethod -> ElevenLabsAPIDocumentationRequest { rMethod, ..} ) <$> f rMethod
{-# INLINE rMethodL #-}

-- | 'rUrlPath' Lens
rUrlPathL :: Lens_' (ElevenLabsAPIDocumentationRequest req contentType res accept) [BCL.ByteString]
rUrlPathL f ElevenLabsAPIDocumentationRequest{..} = (\rUrlPath -> ElevenLabsAPIDocumentationRequest { rUrlPath, ..} ) <$> f rUrlPath
{-# INLINE rUrlPathL #-}

-- | 'rParams' Lens
rParamsL :: Lens_' (ElevenLabsAPIDocumentationRequest req contentType res accept) Params
rParamsL f ElevenLabsAPIDocumentationRequest{..} = (\rParams -> ElevenLabsAPIDocumentationRequest { rParams, ..} ) <$> f rParams
{-# INLINE rParamsL #-}

-- | 'rParams' Lens
rAuthTypesL :: Lens_' (ElevenLabsAPIDocumentationRequest req contentType res accept) [P.TypeRep]
rAuthTypesL f ElevenLabsAPIDocumentationRequest{..} = (\rAuthTypes -> ElevenLabsAPIDocumentationRequest { rAuthTypes, ..} ) <$> f rAuthTypes
{-# INLINE rAuthTypesL #-}

-- * HasBodyParam

-- | Designates the body parameter of a request
class HasBodyParam req param where
  setBodyParam :: forall contentType res accept. (Consumes req contentType, MimeRender contentType param) => ElevenLabsAPIDocumentationRequest req contentType res accept -> param -> ElevenLabsAPIDocumentationRequest req contentType res accept
  setBodyParam req xs =
    req `_setBodyLBS` mimeRender (P.Proxy :: P.Proxy contentType) xs & _setContentTypeHeader

-- * HasOptionalParam

-- | Designates the optional parameters of a request
class HasOptionalParam req param where
  {-# MINIMAL applyOptionalParam | (-&-) #-}

  -- | Apply an optional parameter to a request
  applyOptionalParam :: ElevenLabsAPIDocumentationRequest req contentType res accept -> param -> ElevenLabsAPIDocumentationRequest req contentType res accept
  applyOptionalParam = (-&-)
  {-# INLINE applyOptionalParam #-}

  -- | infix operator \/ alias for 'addOptionalParam'
  (-&-) :: ElevenLabsAPIDocumentationRequest req contentType res accept -> param -> ElevenLabsAPIDocumentationRequest req contentType res accept
  (-&-) = applyOptionalParam
  {-# INLINE (-&-) #-}

infixl 2 -&-

-- | Request Params
data Params = Params
  { paramsQuery :: NH.Query
  , paramsHeaders :: NH.RequestHeaders
  , paramsBody :: ParamBody
  }
  deriving (P.Show)

-- | 'paramsQuery' Lens
paramsQueryL :: Lens_' Params NH.Query
paramsQueryL f Params{..} = (\paramsQuery -> Params { paramsQuery, ..} ) <$> f paramsQuery
{-# INLINE paramsQueryL #-}

-- | 'paramsHeaders' Lens
paramsHeadersL :: Lens_' Params NH.RequestHeaders
paramsHeadersL f Params{..} = (\paramsHeaders -> Params { paramsHeaders, ..} ) <$> f paramsHeaders
{-# INLINE paramsHeadersL #-}

-- | 'paramsBody' Lens
paramsBodyL :: Lens_' Params ParamBody
paramsBodyL f Params{..} = (\paramsBody -> Params { paramsBody, ..} ) <$> f paramsBody
{-# INLINE paramsBodyL #-}

-- | Request Body
data ParamBody
  = ParamBodyNone
  | ParamBodyB B.ByteString
  | ParamBodyBL BL.ByteString
  | ParamBodyFormUrlEncoded WH.Form
  | ParamBodyMultipartFormData [NH.Part]
  deriving (P.Show)

-- ** ElevenLabsAPIDocumentationRequest Utils

_mkRequest :: NH.Method -- ^ Method
          -> [BCL.ByteString] -- ^ Endpoint
          -> ElevenLabsAPIDocumentationRequest req contentType res accept -- ^ req: Request Type, res: Response Type
_mkRequest m u = ElevenLabsAPIDocumentationRequest m u _mkParams []

_mkParams :: Params
_mkParams = Params [] [] ParamBodyNone

setHeader ::
     ElevenLabsAPIDocumentationRequest req contentType res accept
  -> [NH.Header]
  -> ElevenLabsAPIDocumentationRequest req contentType res accept
setHeader req header =
  req `removeHeader` P.fmap P.fst header
  & (`addHeader` header)

addHeader ::
     ElevenLabsAPIDocumentationRequest req contentType res accept
  -> [NH.Header]
  -> ElevenLabsAPIDocumentationRequest req contentType res accept
addHeader req header = L.over (rParamsL . paramsHeadersL) (header P.++) req

removeHeader :: ElevenLabsAPIDocumentationRequest req contentType res accept -> [NH.HeaderName] -> ElevenLabsAPIDocumentationRequest req contentType res accept
removeHeader req header =
  req &
  L.over
    (rParamsL . paramsHeadersL)
    (P.filter (\h -> cifst h `P.notElem` P.fmap CI.mk header))
  where
    cifst = CI.mk . P.fst


_setContentTypeHeader :: forall req contentType res accept. MimeType contentType => ElevenLabsAPIDocumentationRequest req contentType res accept -> ElevenLabsAPIDocumentationRequest req contentType res accept
_setContentTypeHeader req =
    case mimeType (P.Proxy :: P.Proxy contentType) of
        Just m -> req `setHeader` [("content-type", BC.pack $ P.show m)]
        Nothing -> req `removeHeader` ["content-type"]

_setAcceptHeader :: forall req contentType res accept. MimeType accept => ElevenLabsAPIDocumentationRequest req contentType res accept -> ElevenLabsAPIDocumentationRequest req contentType res accept
_setAcceptHeader req =
    case mimeType (P.Proxy :: P.Proxy accept) of
        Just m -> req `setHeader` [("accept", BC.pack $ P.show m)]
        Nothing -> req `removeHeader` ["accept"]

setQuery ::
     ElevenLabsAPIDocumentationRequest req contentType res accept
  -> [NH.QueryItem]
  -> ElevenLabsAPIDocumentationRequest req contentType res accept
setQuery req query =
  req &
  L.over
    (rParamsL . paramsQueryL)
    (P.filter (\q -> cifst q `P.notElem` P.fmap cifst query)) &
  (`addQuery` query)
  where
    cifst = CI.mk . P.fst

addQuery ::
     ElevenLabsAPIDocumentationRequest req contentType res accept
  -> [NH.QueryItem]
  -> ElevenLabsAPIDocumentationRequest req contentType res accept
addQuery req query = req & L.over (rParamsL . paramsQueryL) (query P.++)

addForm :: ElevenLabsAPIDocumentationRequest req contentType res accept -> WH.Form -> ElevenLabsAPIDocumentationRequest req contentType res accept
addForm req newform =
    let form = case paramsBody (rParams req) of
            ParamBodyFormUrlEncoded _form -> _form
            _ -> mempty
    in req & L.set (rParamsL . paramsBodyL) (ParamBodyFormUrlEncoded (newform <> form))

_addMultiFormPart :: ElevenLabsAPIDocumentationRequest req contentType res accept -> NH.Part -> ElevenLabsAPIDocumentationRequest req contentType res accept
_addMultiFormPart req newpart =
    let parts = case paramsBody (rParams req) of
            ParamBodyMultipartFormData _parts -> _parts
            _ -> []
    in req & L.set (rParamsL . paramsBodyL) (ParamBodyMultipartFormData (newpart : parts))

_setBodyBS :: ElevenLabsAPIDocumentationRequest req contentType res accept -> B.ByteString -> ElevenLabsAPIDocumentationRequest req contentType res accept
_setBodyBS req body =
    req & L.set (rParamsL . paramsBodyL) (ParamBodyB body)

_setBodyLBS :: ElevenLabsAPIDocumentationRequest req contentType res accept -> BL.ByteString -> ElevenLabsAPIDocumentationRequest req contentType res accept
_setBodyLBS req body =
    req & L.set (rParamsL . paramsBodyL) (ParamBodyBL body)

_hasAuthType :: AuthMethod authMethod => ElevenLabsAPIDocumentationRequest req contentType res accept -> P.Proxy authMethod -> ElevenLabsAPIDocumentationRequest req contentType res accept
_hasAuthType req proxy =
  req & L.over rAuthTypesL (P.typeRep proxy :)

-- ** Params Utils

toPath
  :: WH.ToHttpApiData a
  => a -> BCL.ByteString
toPath = BB.toLazyByteString . WH.toEncodedUrlPiece

toHeader :: WH.ToHttpApiData a => (NH.HeaderName, a) -> [NH.Header]
toHeader x = [fmap WH.toHeader x]

toForm :: WH.ToHttpApiData v => (BC.ByteString, v) -> WH.Form
toForm (k,v) = WH.toForm [(BC.unpack k,v)]

toQuery :: WH.ToHttpApiData a => (BC.ByteString, Maybe a) -> [NH.QueryItem]
toQuery x = [(fmap . fmap) toQueryParam x]
  where toQueryParam = T.encodeUtf8 . WH.toQueryParam

toPartialEscapeQuery :: B.ByteString -> NH.Query -> NH.PartialEscapeQuery
toPartialEscapeQuery extraUnreserved query = fmap (\(k, v) -> (k, maybe [] go v)) query
  where go :: B.ByteString -> [NH.EscapeItem]
        go v = v & B.groupBy (\a b -> a `B.notElem` extraUnreserved && b `B.notElem` extraUnreserved)
                 & fmap (\xs -> if B.null xs then NH.QN xs
                                  else if B.head xs `B.elem` extraUnreserved
                                          then NH.QN xs -- Not Encoded
                                          else NH.QE xs -- Encoded
                        )

-- *** OpenAPI `CollectionFormat` Utils

-- | Determines the format of the array if type array is used.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. This is valid only for parameters in "query" ('NH.Query') or "formData" ('WH.Form')

toHeaderColl :: WH.ToHttpApiData a => CollectionFormat -> (NH.HeaderName, [a]) -> [NH.Header]
toHeaderColl c xs = _toColl c toHeader xs

toFormColl :: WH.ToHttpApiData v => CollectionFormat -> (BC.ByteString, [v]) -> WH.Form
toFormColl c xs = WH.toForm $ fmap unpack $ _toColl c toHeader $ pack xs
  where
    pack (k,v) = (CI.mk k, v)
    unpack (k,v) = (BC.unpack (CI.original k), BC.unpack v)

toQueryColl :: WH.ToHttpApiData a => CollectionFormat -> (BC.ByteString, Maybe [a]) -> NH.Query
toQueryColl c xs = _toCollA c toQuery xs

_toColl :: P.Traversable f => CollectionFormat -> (f a -> [(b, BC.ByteString)]) -> f [a] -> [(b, BC.ByteString)]
_toColl c encode xs = fmap (fmap P.fromJust) (_toCollA' c fencode BC.singleton (fmap Just xs))
  where fencode = fmap (fmap Just) . encode . fmap P.fromJust
        {-# INLINE fencode #-}

_toCollA :: (P.Traversable f, P.Traversable t, P.Alternative t) => CollectionFormat -> (f (t a) -> [(b, t BC.ByteString)]) -> f (t [a]) -> [(b, t BC.ByteString)]
_toCollA c encode xs = _toCollA' c encode BC.singleton xs

_toCollA' :: (P.Monoid c, P.Traversable f, P.Traversable t, P.Alternative t) => CollectionFormat -> (f (t a) -> [(b, t c)]) -> (Char -> c) -> f (t [a]) -> [(b, t c)]
_toCollA' c encode one xs = case c of
  CommaSeparated -> go (one ',')
  SpaceSeparated -> go (one ' ')
  TabSeparated -> go (one '\t')
  PipeSeparated -> go (one '|')
  MultiParamArray -> expandList
  where
    go sep =
      [P.foldl1 (\(sk, sv) (_, v) -> (sk, (combine sep <$> sv <*> v) <|> sv <|> v)) expandList]
    combine sep x y = x <> sep <> y
    expandList = (P.concatMap encode . (P.traverse . P.traverse) P.toList) xs
    {-# INLINE go #-}
    {-# INLINE expandList #-}
    {-# INLINE combine #-}

-- * AuthMethods

-- | Provides a method to apply auth methods to requests
class P.Typeable a =>
      AuthMethod a  where
  applyAuthMethod
    :: ElevenLabsAPIDocumentationConfig
    -> a
    -> ElevenLabsAPIDocumentationRequest req contentType res accept
    -> IO (ElevenLabsAPIDocumentationRequest req contentType res accept)

-- | An existential wrapper for any AuthMethod
data AnyAuthMethod = forall a. AuthMethod a => AnyAuthMethod a deriving (P.Typeable)

instance AuthMethod AnyAuthMethod where applyAuthMethod config (AnyAuthMethod a) req = applyAuthMethod config a req

-- | indicates exceptions related to AuthMethods
data AuthMethodException = AuthMethodException String deriving (P.Show, P.Typeable)

instance E.Exception AuthMethodException

-- | apply all matching AuthMethods in config to request
_applyAuthMethods
  :: ElevenLabsAPIDocumentationRequest req contentType res accept
  -> ElevenLabsAPIDocumentationConfig
  -> IO (ElevenLabsAPIDocumentationRequest req contentType res accept)
_applyAuthMethods req config@(ElevenLabsAPIDocumentationConfig {configAuthMethods = as}) =
  foldlM go req as
  where
    go r (AnyAuthMethod a) = applyAuthMethod config a r

-- * Utils

-- | Removes Null fields.  (OpenAPI-Specification 2.0 does not allow Null in JSON)
#if MIN_VERSION_aeson(2,0,0)
_omitNulls :: [(A.Key, A.Value)] -> A.Value
#else
_omitNulls :: [(Text, A.Value)] -> A.Value
#endif
_omitNulls = A.object . P.filter notNull
  where
    notNull (_, A.Null) = False
    notNull _ = True

-- | Encodes fields using WH.toQueryParam
_toFormItem :: (WH.ToHttpApiData a, Functor f) => t -> f a -> f (t, [Text])
_toFormItem name x = (name,) . (:[]) . WH.toQueryParam <$> x

-- | Collapse (Just "") to Nothing
_emptyToNothing :: Maybe String -> Maybe String
_emptyToNothing (Just "") = Nothing
_emptyToNothing x = x
{-# INLINE _emptyToNothing #-}

-- | Collapse (Just mempty) to Nothing
_memptyToNothing :: (P.Monoid a, P.Eq a) => Maybe a -> Maybe a
_memptyToNothing (Just x) | x P.== P.mempty = Nothing
_memptyToNothing x = x
{-# INLINE _memptyToNothing #-}

-- * DateTime Formatting

newtype DateTime = DateTime { unDateTime :: TI.UTCTime }
  deriving (P.Eq,P.Data,P.Ord,P.Typeable,NF.NFData)
instance A.FromJSON DateTime where
  parseJSON = A.withText "DateTime" (_readDateTime . T.unpack)
instance A.ToJSON DateTime where
  toJSON (DateTime t) = A.toJSON (_showDateTime t)
instance WH.FromHttpApiData DateTime where
  parseUrlPiece = P.maybe (P.Left "parseUrlPiece @DateTime") P.Right . _readDateTime . T.unpack
instance WH.ToHttpApiData DateTime where
  toUrlPiece (DateTime t) = T.pack (_showDateTime t)
instance P.Show DateTime where
  show (DateTime t) = _showDateTime t
instance MimeRender MimeMultipartFormData DateTime where
  mimeRender _ = mimeRenderDefaultMultipartFormData

-- | @_parseISO8601@
_readDateTime :: (MonadFail m, Alternative m) => String -> m DateTime
_readDateTime s =
  DateTime <$> _parseISO8601 s
{-# INLINE _readDateTime #-}

-- | @TI.formatISO8601Millis@
_showDateTime :: (t ~ TI.UTCTime, TI.FormatTime t) => t -> String
_showDateTime =
  TI.formatISO8601Millis
{-# INLINE _showDateTime #-}

-- | parse an ISO8601 date-time string
_parseISO8601 :: (TI.ParseTime t, MonadFail m, Alternative m) => String -> m t
_parseISO8601 t =
  P.asum $
  P.flip (TI.parseTimeM True TI.defaultTimeLocale) t <$>
  ["%FT%T%QZ", "%FT%T%Q%z", "%FT%T%Q%Z"]
{-# INLINE _parseISO8601 #-}

-- * Date Formatting

newtype Date = Date { unDate :: TI.Day }
  deriving (P.Enum,P.Eq,P.Data,P.Ord,P.Ix,NF.NFData)
instance A.FromJSON Date where
  parseJSON = A.withText "Date" (_readDate . T.unpack)
instance A.ToJSON Date where
  toJSON (Date t) = A.toJSON (_showDate t)
instance WH.FromHttpApiData Date where
  parseUrlPiece = P.maybe (P.Left "parseUrlPiece @Date") P.Right . _readDate . T.unpack
instance WH.ToHttpApiData Date where
  toUrlPiece (Date t) = T.pack (_showDate t)
instance P.Show Date where
  show (Date t) = _showDate t
instance MimeRender MimeMultipartFormData Date where
  mimeRender _ = mimeRenderDefaultMultipartFormData

-- | @TI.parseTimeM True TI.defaultTimeLocale "%Y-%m-%d"@
_readDate :: MonadFail m => String -> m Date
_readDate s = Date <$> TI.parseTimeM True TI.defaultTimeLocale "%Y-%m-%d" s
{-# INLINE _readDate #-}

-- | @TI.formatTime TI.defaultTimeLocale "%Y-%m-%d"@
_showDate :: TI.FormatTime t => t -> String
_showDate =
  TI.formatTime TI.defaultTimeLocale "%Y-%m-%d"
{-# INLINE _showDate #-}

-- * Byte/Binary Formatting


-- | base64 encoded characters
newtype ByteArray = ByteArray { unByteArray :: BL.ByteString }
  deriving (P.Eq,P.Data,P.Ord,P.Typeable,NF.NFData)

instance A.FromJSON ByteArray where
  parseJSON = A.withText "ByteArray" _readByteArray
instance A.ToJSON ByteArray where
  toJSON = A.toJSON . _showByteArray
instance WH.FromHttpApiData ByteArray where
  parseUrlPiece = P.maybe (P.Left "parseUrlPiece @ByteArray") P.Right . _readByteArray
instance WH.ToHttpApiData ByteArray where
  toUrlPiece = _showByteArray
instance P.Show ByteArray where
  show = T.unpack . _showByteArray
instance MimeRender MimeMultipartFormData ByteArray where
  mimeRender _ = mimeRenderDefaultMultipartFormData

-- | read base64 encoded characters
_readByteArray :: MonadFail m => Text -> m ByteArray
_readByteArray = P.either P.fail (pure . ByteArray) . BL64.decode . BL.fromStrict . T.encodeUtf8
{-# INLINE _readByteArray #-}

-- | show base64 encoded characters
_showByteArray :: ByteArray -> Text
_showByteArray = T.decodeUtf8 . BL.toStrict . BL64.encode . unByteArray
{-# INLINE _showByteArray #-}

-- | any sequence of octets
newtype Binary = Binary { unBinary :: BL.ByteString }
  deriving (P.Eq,P.Data,P.Ord,P.Typeable,NF.NFData)

instance A.FromJSON Binary where
  parseJSON = A.withText "Binary" _readBinaryBase64
instance A.ToJSON Binary where
  toJSON = A.toJSON . _showBinaryBase64
instance WH.FromHttpApiData Binary where
  parseUrlPiece = P.maybe (P.Left "parseUrlPiece @Binary") P.Right . _readBinaryBase64
instance WH.ToHttpApiData Binary where
  toUrlPiece = _showBinaryBase64
instance P.Show Binary where
  show = T.unpack . _showBinaryBase64
instance MimeRender MimeMultipartFormData Binary where
  mimeRender _ = unBinary

_readBinaryBase64 :: MonadFail m => Text -> m Binary
_readBinaryBase64 = P.either P.fail (pure . Binary) . BL64.decode . BL.fromStrict . T.encodeUtf8
{-# INLINE _readBinaryBase64 #-}

_showBinaryBase64 :: Binary -> Text
_showBinaryBase64 = T.decodeUtf8 . BL.toStrict . BL64.encode . unBinary
{-# INLINE _showBinaryBase64 #-}

-- * Lens Type Aliases

type Lens_' s a = Lens_ s s a a
type Lens_ s t a b = forall (f :: K.Type -> K.Type). Functor f => (a -> f b) -> s -> f t
