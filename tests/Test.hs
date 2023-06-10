{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import ElevenLabsAPIDocumentation.Model
import ElevenLabsAPIDocumentation.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy AddVoiceResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy BodyDownloadHistoryItemsV1HistoryDownloadPost)
      propMimeEq MimeJSON (Proxy :: Proxy BodyTextToSpeechV1TextToSpeechVoiceIdPost)
      propMimeEq MimeJSON (Proxy :: Proxy BodyTextToSpeechV1TextToSpeechVoiceIdStreamPost)
      propMimeEq MimeJSON (Proxy :: Proxy ExtendedSubscriptionResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy FeedbackResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy FineTuningResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy GetHistoryResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy GetVoicesResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy HTTPValidationError)
      propMimeEq MimeJSON (Proxy :: Proxy HistoryItemResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy InvoiceResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy LanguageResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy LocationInner)
      propMimeEq MimeJSON (Proxy :: Proxy ModelResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy RecordingResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy SampleResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy Settings)
      propMimeEq MimeJSON (Proxy :: Proxy SubscriptionResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy UserResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy ValidationError)
      propMimeEq MimeJSON (Proxy :: Proxy VerificationAttemptResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy VoiceResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy VoiceSettings)
      propMimeEq MimeJSON (Proxy :: Proxy VoiceSettingsResponseModel)
      propMimeEq MimeJSON (Proxy :: Proxy VoiceSharingResponseModel)
      
