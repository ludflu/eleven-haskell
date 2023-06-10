{-
   ElevenLabs API Documentation

   This is the documentation for the ElevenLabs API. You can use this API to use our service programmatically, this is done by using your xi-api-key. <br/> You can view your xi-api-key using the 'Profile' tab on https://beta.elevenlabs.io. Our API is experimental so all endpoints are subject to change.

   OpenAPI Version: 3.0.2
   ElevenLabs API Documentation API version: 1.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : ElevenLabsAPIDocumentation.API.TextToSpeech
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module ElevenLabsAPIDocumentation.API.TextToSpeech where

import ElevenLabsAPIDocumentation.Core
import ElevenLabsAPIDocumentation.MimeTypes
import ElevenLabsAPIDocumentation.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** TextToSpeech

-- *** textToSpeechV1TextToSpeechVoiceIdPost

-- | @POST \/v1\/text-to-speech\/{voice_id}@
-- 
-- Text To Speech
-- 
-- Converts text into speech using a voice of your choice and returns audio.
-- 
textToSpeechV1TextToSpeechVoiceIdPost
  :: (Consumes TextToSpeechV1TextToSpeechVoiceIdPost MimeJSON, MimeRender MimeJSON BodyTextToSpeechV1TextToSpeechVoiceIdPost)
  => BodyTextToSpeechV1TextToSpeechVoiceIdPost -- ^ "bodyTextToSpeechV1TextToSpeechVoiceIdPost"
  -> VoiceId -- ^ "voiceId" -  Voice ID to be used, you can use https://api.elevenlabs.io/v1/voices to list all the available voices.
  -> ElevenLabsAPIDocumentationRequest TextToSpeechV1TextToSpeechVoiceIdPost MimeJSON NoContent MimeNoContent
textToSpeechV1TextToSpeechVoiceIdPost bodyTextToSpeechV1TextToSpeechVoiceIdPost (VoiceId voiceId) =
  _mkRequest "POST" ["/v1/text-to-speech/",toPath voiceId]
    `setBodyParam` bodyTextToSpeechV1TextToSpeechVoiceIdPost

data TextToSpeechV1TextToSpeechVoiceIdPost 
instance HasBodyParam TextToSpeechV1TextToSpeechVoiceIdPost BodyTextToSpeechV1TextToSpeechVoiceIdPost 

-- | /Optional Param/ "optimize_streaming_latency" - You can turn on latency optimizations at some cost of quality. The best possible final latency varies by model. Possible values: 0 - default mode (no latency optimizations) 1 - normal latency optimizations (about 50% of possible latency improvement of option 3) 2 - strong latency optimizations (about 75% of possible latency improvement of option 3) 3 - max latency optimizations 4 - max latency optimizations, but also with text normalizer turned off for even more latency savings (best latency, but can mispronounce eg numbers and dates). 
instance HasOptionalParam TextToSpeechV1TextToSpeechVoiceIdPost OptimizeStreamingLatency where
  applyOptionalParam req (OptimizeStreamingLatency xs) =
    req `addQuery` toQuery ("optimize_streaming_latency", Just xs)

-- | /Optional Param/ "xi-api-key" - Your API key. This is required by most endpoints to access our API programatically. You can view your xi-api-key using the 'Profile' tab on the website.
instance HasOptionalParam TextToSpeechV1TextToSpeechVoiceIdPost XiApiKey where
  applyOptionalParam req (XiApiKey xs) =
    req `addHeader` toHeader ("xi-api-key", xs)

-- | @application/json@
instance Consumes TextToSpeechV1TextToSpeechVoiceIdPost MimeJSON

instance Produces TextToSpeechV1TextToSpeechVoiceIdPost MimeNoContent


-- *** textToSpeechV1TextToSpeechVoiceIdStreamPost

-- | @POST \/v1\/text-to-speech\/{voice_id}\/stream@
-- 
-- Text To Speech
-- 
-- Converts text into speech using a voice of your choice and returns audio as an audio stream.
-- 
textToSpeechV1TextToSpeechVoiceIdStreamPost
  :: (Consumes TextToSpeechV1TextToSpeechVoiceIdStreamPost MimeJSON, MimeRender MimeJSON BodyTextToSpeechV1TextToSpeechVoiceIdStreamPost)
  => BodyTextToSpeechV1TextToSpeechVoiceIdStreamPost -- ^ "bodyTextToSpeechV1TextToSpeechVoiceIdStreamPost"
  -> VoiceId -- ^ "voiceId" -  Voice ID to be used, you can use https://api.elevenlabs.io/v1/voices to list all the available voices.
  -> ElevenLabsAPIDocumentationRequest TextToSpeechV1TextToSpeechVoiceIdStreamPost MimeJSON NoContent MimeNoContent
textToSpeechV1TextToSpeechVoiceIdStreamPost bodyTextToSpeechV1TextToSpeechVoiceIdStreamPost (VoiceId voiceId) =
  _mkRequest "POST" ["/v1/text-to-speech/",toPath voiceId,"/stream"]
    `setBodyParam` bodyTextToSpeechV1TextToSpeechVoiceIdStreamPost

data TextToSpeechV1TextToSpeechVoiceIdStreamPost 
instance HasBodyParam TextToSpeechV1TextToSpeechVoiceIdStreamPost BodyTextToSpeechV1TextToSpeechVoiceIdStreamPost 

-- | /Optional Param/ "optimize_streaming_latency" - You can turn on latency optimizations at some cost of quality. The best possible final latency varies by model. Possible values: 0 - default mode (no latency optimizations) 1 - normal latency optimizations (about 50% of possible latency improvement of option 3) 2 - strong latency optimizations (about 75% of possible latency improvement of option 3) 3 - max latency optimizations 4 - max latency optimizations, but also with text normalizer turned off for even more latency savings (best latency, but can mispronounce eg numbers and dates). 
instance HasOptionalParam TextToSpeechV1TextToSpeechVoiceIdStreamPost OptimizeStreamingLatency where
  applyOptionalParam req (OptimizeStreamingLatency xs) =
    req `addQuery` toQuery ("optimize_streaming_latency", Just xs)

-- | /Optional Param/ "xi-api-key" - Your API key. This is required by most endpoints to access our API programatically. You can view your xi-api-key using the 'Profile' tab on the website.
instance HasOptionalParam TextToSpeechV1TextToSpeechVoiceIdStreamPost XiApiKey where
  applyOptionalParam req (XiApiKey xs) =
    req `addHeader` toHeader ("xi-api-key", xs)

-- | @application/json@
instance Consumes TextToSpeechV1TextToSpeechVoiceIdStreamPost MimeJSON

instance Produces TextToSpeechV1TextToSpeechVoiceIdStreamPost MimeNoContent

