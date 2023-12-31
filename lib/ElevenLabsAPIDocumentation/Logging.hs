{-
   ElevenLabs API Documentation

   This is the documentation for the ElevenLabs API. You can use this API to use our service programmatically, this is done by using your xi-api-key. <br/> You can view your xi-api-key using the 'Profile' tab on https://beta.elevenlabs.io. Our API is experimental so all endpoints are subject to change.

   OpenAPI Version: 3.0.2
   ElevenLabs API Documentation API version: 1.0
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : ElevenLabsAPIDocumentation.Logging
Logging functions
-}
{-# LANGUAGE CPP #-}

#ifdef USE_KATIP

module ElevenLabsAPIDocumentation.Logging
  ( module ElevenLabsAPIDocumentation.LoggingKatip
  ) where

import ElevenLabsAPIDocumentation.LoggingKatip

#else

module ElevenLabsAPIDocumentation.Logging
  ( module ElevenLabsAPIDocumentation.LoggingMonadLogger
  ) where

import ElevenLabsAPIDocumentation.LoggingMonadLogger

#endif
