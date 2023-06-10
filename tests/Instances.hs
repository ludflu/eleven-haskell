{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import ElevenLabsAPIDocumentation.Model
import ElevenLabsAPIDocumentation.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary AddVoiceResponseModel where
  arbitrary = sized genAddVoiceResponseModel

genAddVoiceResponseModel :: Int -> Gen AddVoiceResponseModel
genAddVoiceResponseModel n =
  AddVoiceResponseModel
    <$> arbitrary -- addVoiceResponseModelVoiceId :: Text
  
instance Arbitrary BodyDownloadHistoryItemsV1HistoryDownloadPost where
  arbitrary = sized genBodyDownloadHistoryItemsV1HistoryDownloadPost

genBodyDownloadHistoryItemsV1HistoryDownloadPost :: Int -> Gen BodyDownloadHistoryItemsV1HistoryDownloadPost
genBodyDownloadHistoryItemsV1HistoryDownloadPost n =
  BodyDownloadHistoryItemsV1HistoryDownloadPost
    <$> arbitrary -- bodyDownloadHistoryItemsV1HistoryDownloadPostHistoryItemIds :: [Text]
  
instance Arbitrary BodyTextToSpeechV1TextToSpeechVoiceIdPost where
  arbitrary = sized genBodyTextToSpeechV1TextToSpeechVoiceIdPost

genBodyTextToSpeechV1TextToSpeechVoiceIdPost :: Int -> Gen BodyTextToSpeechV1TextToSpeechVoiceIdPost
genBodyTextToSpeechV1TextToSpeechVoiceIdPost n =
  BodyTextToSpeechV1TextToSpeechVoiceIdPost
    <$> arbitrary -- bodyTextToSpeechV1TextToSpeechVoiceIdPostText :: Text
    <*> arbitraryReducedMaybe n -- bodyTextToSpeechV1TextToSpeechVoiceIdPostModelId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bodyTextToSpeechV1TextToSpeechVoiceIdPostVoiceSettings :: Maybe VoiceSettings
  
instance Arbitrary BodyTextToSpeechV1TextToSpeechVoiceIdStreamPost where
  arbitrary = sized genBodyTextToSpeechV1TextToSpeechVoiceIdStreamPost

genBodyTextToSpeechV1TextToSpeechVoiceIdStreamPost :: Int -> Gen BodyTextToSpeechV1TextToSpeechVoiceIdStreamPost
genBodyTextToSpeechV1TextToSpeechVoiceIdStreamPost n =
  BodyTextToSpeechV1TextToSpeechVoiceIdStreamPost
    <$> arbitrary -- bodyTextToSpeechV1TextToSpeechVoiceIdStreamPostText :: Text
    <*> arbitraryReducedMaybe n -- bodyTextToSpeechV1TextToSpeechVoiceIdStreamPostModelId :: Maybe Text
    <*> arbitraryReducedMaybe n -- bodyTextToSpeechV1TextToSpeechVoiceIdStreamPostVoiceSettings :: Maybe VoiceSettings
  
instance Arbitrary ExtendedSubscriptionResponseModel where
  arbitrary = sized genExtendedSubscriptionResponseModel

genExtendedSubscriptionResponseModel :: Int -> Gen ExtendedSubscriptionResponseModel
genExtendedSubscriptionResponseModel n =
  ExtendedSubscriptionResponseModel
    <$> arbitrary -- extendedSubscriptionResponseModelTier :: Text
    <*> arbitrary -- extendedSubscriptionResponseModelCharacterCount :: Int
    <*> arbitrary -- extendedSubscriptionResponseModelCharacterLimit :: Int
    <*> arbitrary -- extendedSubscriptionResponseModelCanExtendCharacterLimit :: Bool
    <*> arbitrary -- extendedSubscriptionResponseModelAllowedToExtendCharacterLimit :: Bool
    <*> arbitrary -- extendedSubscriptionResponseModelNextCharacterCountResetUnix :: Int
    <*> arbitrary -- extendedSubscriptionResponseModelVoiceLimit :: Int
    <*> arbitrary -- extendedSubscriptionResponseModelProfessionalVoiceLimit :: Int
    <*> arbitrary -- extendedSubscriptionResponseModelCanExtendVoiceLimit :: Bool
    <*> arbitrary -- extendedSubscriptionResponseModelCanUseInstantVoiceCloning :: Bool
    <*> arbitrary -- extendedSubscriptionResponseModelCanUseProfessionalVoiceCloning :: Bool
    <*> arbitrary -- extendedSubscriptionResponseModelCurrency :: E'Currency
    <*> arbitrary -- extendedSubscriptionResponseModelStatus :: E'Status
    <*> arbitraryReduced n -- extendedSubscriptionResponseModelNextInvoice :: InvoiceResponseModel
    <*> arbitrary -- extendedSubscriptionResponseModelHasOpenInvoices :: Bool
  
instance Arbitrary FeedbackResponseModel where
  arbitrary = sized genFeedbackResponseModel

genFeedbackResponseModel :: Int -> Gen FeedbackResponseModel
genFeedbackResponseModel n =
  FeedbackResponseModel
    <$> arbitrary -- feedbackResponseModelThumbsUp :: Bool
    <*> arbitrary -- feedbackResponseModelFeedback :: Text
    <*> arbitrary -- feedbackResponseModelEmotions :: Bool
    <*> arbitrary -- feedbackResponseModelInaccurateClone :: Bool
    <*> arbitrary -- feedbackResponseModelGlitches :: Bool
    <*> arbitrary -- feedbackResponseModelAudioQuality :: Bool
    <*> arbitrary -- feedbackResponseModelOther :: Bool
    <*> arbitraryReducedMaybe n -- feedbackResponseModelReviewStatus :: Maybe Text
  
instance Arbitrary FineTuningResponseModel where
  arbitrary = sized genFineTuningResponseModel

genFineTuningResponseModel :: Int -> Gen FineTuningResponseModel
genFineTuningResponseModel n =
  FineTuningResponseModel
    <$> arbitrary -- fineTuningResponseModelModelId :: Text
    <*> arbitrary -- fineTuningResponseModelLanguage :: Text
    <*> arbitrary -- fineTuningResponseModelIsAllowedToFineTune :: Bool
    <*> arbitrary -- fineTuningResponseModelFineTuningRequested :: Bool
    <*> arbitrary -- fineTuningResponseModelFinetuningState :: E'FinetuningState
    <*> arbitraryReduced n -- fineTuningResponseModelVerificationAttempts :: [VerificationAttemptResponseModel]
    <*> arbitrary -- fineTuningResponseModelVerificationFailures :: [Text]
    <*> arbitrary -- fineTuningResponseModelVerificationAttemptsCount :: Int
    <*> arbitrary -- fineTuningResponseModelSliceIds :: [Text]
  
instance Arbitrary GetHistoryResponseModel where
  arbitrary = sized genGetHistoryResponseModel

genGetHistoryResponseModel :: Int -> Gen GetHistoryResponseModel
genGetHistoryResponseModel n =
  GetHistoryResponseModel
    <$> arbitraryReduced n -- getHistoryResponseModelHistory :: [HistoryItemResponseModel]
    <*> arbitrary -- getHistoryResponseModelLastHistoryItemId :: Text
    <*> arbitrary -- getHistoryResponseModelHasMore :: Bool
  
instance Arbitrary GetVoicesResponseModel where
  arbitrary = sized genGetVoicesResponseModel

genGetVoicesResponseModel :: Int -> Gen GetVoicesResponseModel
genGetVoicesResponseModel n =
  GetVoicesResponseModel
    <$> arbitraryReduced n -- getVoicesResponseModelVoices :: [VoiceResponseModel]
  
instance Arbitrary HTTPValidationError where
  arbitrary = sized genHTTPValidationError

genHTTPValidationError :: Int -> Gen HTTPValidationError
genHTTPValidationError n =
  HTTPValidationError
    <$> arbitraryReducedMaybe n -- hTTPValidationErrorDetail :: Maybe [ValidationError]
  
instance Arbitrary HistoryItemResponseModel where
  arbitrary = sized genHistoryItemResponseModel

genHistoryItemResponseModel :: Int -> Gen HistoryItemResponseModel
genHistoryItemResponseModel n =
  HistoryItemResponseModel
    <$> arbitrary -- historyItemResponseModelHistoryItemId :: Text
    <*> arbitrary -- historyItemResponseModelRequestId :: Text
    <*> arbitrary -- historyItemResponseModelVoiceId :: Text
    <*> arbitrary -- historyItemResponseModelVoiceName :: Text
    <*> arbitrary -- historyItemResponseModelText :: Text
    <*> arbitrary -- historyItemResponseModelDateUnix :: Int
    <*> arbitrary -- historyItemResponseModelCharacterCountChangeFrom :: Int
    <*> arbitrary -- historyItemResponseModelCharacterCountChangeTo :: Int
    <*> arbitrary -- historyItemResponseModelContentType :: Text
    <*> arbitrary -- historyItemResponseModelState :: E'State
    <*> arbitraryReduced n -- historyItemResponseModelSettings :: A.Value
    <*> arbitraryReduced n -- historyItemResponseModelFeedback :: FeedbackResponseModel
  
instance Arbitrary InvoiceResponseModel where
  arbitrary = sized genInvoiceResponseModel

genInvoiceResponseModel :: Int -> Gen InvoiceResponseModel
genInvoiceResponseModel n =
  InvoiceResponseModel
    <$> arbitrary -- invoiceResponseModelAmountDueCents :: Int
    <*> arbitrary -- invoiceResponseModelNextPaymentAttemptUnix :: Int
  
instance Arbitrary LanguageResponseModel where
  arbitrary = sized genLanguageResponseModel

genLanguageResponseModel :: Int -> Gen LanguageResponseModel
genLanguageResponseModel n =
  LanguageResponseModel
    <$> arbitrary -- languageResponseModelLanguageId :: Text
    <*> arbitrary -- languageResponseModelName :: Text
  
instance Arbitrary LocationInner where
  arbitrary = sized genLocationInner

genLocationInner :: Int -> Gen LocationInner
genLocationInner n =
  
  pure LocationInner
   
instance Arbitrary ModelResponseModel where
  arbitrary = sized genModelResponseModel

genModelResponseModel :: Int -> Gen ModelResponseModel
genModelResponseModel n =
  ModelResponseModel
    <$> arbitrary -- modelResponseModelModelId :: Text
    <*> arbitrary -- modelResponseModelName :: Text
    <*> arbitrary -- modelResponseModelCanBeFinetuned :: Bool
    <*> arbitrary -- modelResponseModelCanDoTextToSpeech :: Bool
    <*> arbitrary -- modelResponseModelCanDoVoiceConversion :: Bool
    <*> arbitrary -- modelResponseModelTokenCostFactor :: Double
    <*> arbitrary -- modelResponseModelDescription :: Text
    <*> arbitraryReduced n -- modelResponseModelLanguages :: [LanguageResponseModel]
  
instance Arbitrary RecordingResponseModel where
  arbitrary = sized genRecordingResponseModel

genRecordingResponseModel :: Int -> Gen RecordingResponseModel
genRecordingResponseModel n =
  RecordingResponseModel
    <$> arbitrary -- recordingResponseModelRecordingId :: Text
    <*> arbitrary -- recordingResponseModelMimeType :: Text
    <*> arbitrary -- recordingResponseModelSizeBytes :: Int
    <*> arbitrary -- recordingResponseModelUploadDateUnix :: Int
    <*> arbitrary -- recordingResponseModelTranscription :: Text
  
instance Arbitrary SampleResponseModel where
  arbitrary = sized genSampleResponseModel

genSampleResponseModel :: Int -> Gen SampleResponseModel
genSampleResponseModel n =
  SampleResponseModel
    <$> arbitrary -- sampleResponseModelSampleId :: Text
    <*> arbitrary -- sampleResponseModelFileName :: Text
    <*> arbitrary -- sampleResponseModelMimeType :: Text
    <*> arbitrary -- sampleResponseModelSizeBytes :: Int
    <*> arbitrary -- sampleResponseModelHash :: Text
  
instance Arbitrary Settings where
  arbitrary = sized genSettings

genSettings :: Int -> Gen Settings
genSettings n =
  Settings
    <$> arbitrary -- settingsStability :: Double
    <*> arbitrary -- settingsSimilarityBoost :: Double
  
instance Arbitrary SubscriptionResponseModel where
  arbitrary = sized genSubscriptionResponseModel

genSubscriptionResponseModel :: Int -> Gen SubscriptionResponseModel
genSubscriptionResponseModel n =
  SubscriptionResponseModel
    <$> arbitrary -- subscriptionResponseModelTier :: Text
    <*> arbitrary -- subscriptionResponseModelCharacterCount :: Int
    <*> arbitrary -- subscriptionResponseModelCharacterLimit :: Int
    <*> arbitrary -- subscriptionResponseModelCanExtendCharacterLimit :: Bool
    <*> arbitrary -- subscriptionResponseModelAllowedToExtendCharacterLimit :: Bool
    <*> arbitrary -- subscriptionResponseModelNextCharacterCountResetUnix :: Int
    <*> arbitrary -- subscriptionResponseModelVoiceLimit :: Int
    <*> arbitrary -- subscriptionResponseModelProfessionalVoiceLimit :: Int
    <*> arbitrary -- subscriptionResponseModelCanExtendVoiceLimit :: Bool
    <*> arbitrary -- subscriptionResponseModelCanUseInstantVoiceCloning :: Bool
    <*> arbitrary -- subscriptionResponseModelCanUseProfessionalVoiceCloning :: Bool
    <*> arbitrary -- subscriptionResponseModelCurrency :: E'Currency
    <*> arbitrary -- subscriptionResponseModelStatus :: E'Status
  
instance Arbitrary UserResponseModel where
  arbitrary = sized genUserResponseModel

genUserResponseModel :: Int -> Gen UserResponseModel
genUserResponseModel n =
  UserResponseModel
    <$> arbitraryReduced n -- userResponseModelSubscription :: SubscriptionResponseModel
    <*> arbitrary -- userResponseModelIsNewUser :: Bool
    <*> arbitrary -- userResponseModelXiApiKey :: Text
    <*> arbitrary -- userResponseModelCanUseDelayedPaymentMethods :: Bool
  
instance Arbitrary ValidationError where
  arbitrary = sized genValidationError

genValidationError :: Int -> Gen ValidationError
genValidationError n =
  ValidationError
    <$> arbitraryReduced n -- validationErrorLoc :: [LocationInner]
    <*> arbitrary -- validationErrorMsg :: Text
    <*> arbitrary -- validationErrorType :: Text
  
instance Arbitrary VerificationAttemptResponseModel where
  arbitrary = sized genVerificationAttemptResponseModel

genVerificationAttemptResponseModel :: Int -> Gen VerificationAttemptResponseModel
genVerificationAttemptResponseModel n =
  VerificationAttemptResponseModel
    <$> arbitrary -- verificationAttemptResponseModelText :: Text
    <*> arbitrary -- verificationAttemptResponseModelDateUnix :: Int
    <*> arbitrary -- verificationAttemptResponseModelAccepted :: Bool
    <*> arbitrary -- verificationAttemptResponseModelSimilarity :: Double
    <*> arbitrary -- verificationAttemptResponseModelLevenshteinDistance :: Double
    <*> arbitraryReduced n -- verificationAttemptResponseModelRecording :: RecordingResponseModel
  
instance Arbitrary VoiceResponseModel where
  arbitrary = sized genVoiceResponseModel

genVoiceResponseModel :: Int -> Gen VoiceResponseModel
genVoiceResponseModel n =
  VoiceResponseModel
    <$> arbitrary -- voiceResponseModelVoiceId :: Text
    <*> arbitrary -- voiceResponseModelName :: Text
    <*> arbitraryReduced n -- voiceResponseModelSamples :: [SampleResponseModel]
    <*> arbitrary -- voiceResponseModelCategory :: Text
    <*> arbitraryReduced n -- voiceResponseModelFineTuning :: FineTuningResponseModel
    <*> arbitrary -- voiceResponseModelLabels :: (Map.Map String Text)
    <*> arbitrary -- voiceResponseModelDescription :: Text
    <*> arbitrary -- voiceResponseModelPreviewUrl :: Text
    <*> arbitrary -- voiceResponseModelAvailableForTiers :: [Text]
    <*> arbitraryReduced n -- voiceResponseModelSettings :: VoiceSettingsResponseModel
    <*> arbitraryReduced n -- voiceResponseModelSharing :: VoiceSharingResponseModel
  
instance Arbitrary VoiceSettings where
  arbitrary = sized genVoiceSettings

genVoiceSettings :: Int -> Gen VoiceSettings
genVoiceSettings n =
  VoiceSettings
    <$> arbitrary -- voiceSettingsStability :: Double
    <*> arbitrary -- voiceSettingsSimilarityBoost :: Double
  
instance Arbitrary VoiceSettingsResponseModel where
  arbitrary = sized genVoiceSettingsResponseModel

genVoiceSettingsResponseModel :: Int -> Gen VoiceSettingsResponseModel
genVoiceSettingsResponseModel n =
  VoiceSettingsResponseModel
    <$> arbitrary -- voiceSettingsResponseModelStability :: Double
    <*> arbitrary -- voiceSettingsResponseModelSimilarityBoost :: Double
  
instance Arbitrary VoiceSharingResponseModel where
  arbitrary = sized genVoiceSharingResponseModel

genVoiceSharingResponseModel :: Int -> Gen VoiceSharingResponseModel
genVoiceSharingResponseModel n =
  VoiceSharingResponseModel
    <$> arbitrary -- voiceSharingResponseModelStatus :: Text
    <*> arbitrary -- voiceSharingResponseModelHistoryItemSampleId :: Text
    <*> arbitrary -- voiceSharingResponseModelOriginalVoiceId :: Text
    <*> arbitrary -- voiceSharingResponseModelPublicOwnerId :: Text
    <*> arbitrary -- voiceSharingResponseModelLikedByCount :: Int
    <*> arbitrary -- voiceSharingResponseModelClonedByCount :: Int
  



instance Arbitrary E'Currency where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'FinetuningState where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

