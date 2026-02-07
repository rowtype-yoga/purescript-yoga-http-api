module Test.Operators where

import Prelude
import Effect (Effect)
import ViTest (ViTest, viTest)
import OperatorTest.Spec as OperatorTest
import ParserTest.Spec as ParserTest
import VariantResponseTest.Spec as VariantResponseTest
import MetadataValidationTest.Spec as MetadataValidationTest
import APIRecordTest.Spec as APIRecordTest
import SchemaComponentTest.Spec as SchemaComponentTest
import BearerAuthTest.Spec as BearerAuthTest
-- import CallbackTest.Spec as CallbackTest
import CookieAuthTest.Spec as CookieAuthTest
import OpenAPIInfoTest.Spec as OpenAPIInfoTest
import ContentTypeTest.Spec as ContentTypeTest
import ExamplesTest.Spec as ExamplesTest

spec :: Effect ViTest
spec = do
  _ <- OperatorTest.testRendering
  _ <- ParserTest.testSegmentParsing
  _ <- ParserTest.testCaptureParsing
  _ <- ParserTest.testPathParsing
  _ <- ParserTest.testOptionalQueryParams
  _ <- ParserTest.testRequiredQueryParams
  _ <- ParserTest.testErrorCases
  _ <- VariantResponseTest.testStatusCodeMapping
  _ <- VariantResponseTest.testStatusCodeToString
  _ <- VariantResponseTest.testRespondNoHeaders
  _ <- VariantResponseTest.testRespondWith
  _ <- VariantResponseTest.testRespond
  _ <- VariantResponseTest.testSimpleVariantOpenAPI
  _ <- VariantResponseTest.testComplexVariantOpenAPI
  _ <- VariantResponseTest.testVariantWithHeaders
  _ <- VariantResponseTest.testVariantPatternMatching
  _ <- MetadataValidationTest.testPatternValidation
  _ <- MetadataValidationTest.testMinLengthValidation
  _ <- MetadataValidationTest.testMaxLengthValidation
  _ <- MetadataValidationTest.testMinimumValidation
  _ <- MetadataValidationTest.testMaximumValidation
  _ <- MetadataValidationTest.testComposedValidation
  _ <- APIRecordTest.testCollectOperations
  _ <- APIRecordTest.testOperationIds
  _ <- APIRecordTest.testBuildOpenAPISpec
  _ <- APIRecordTest.testAPIHandlers
  _ <- BearerAuthTest.testBearerTokenAuth
  _ <- BearerAuthTest.testBasicAuth
  _ <- BearerAuthTest.testApiKeyAuth
  _ <- BearerAuthTest.testDigestAuth
  _ <- BearerAuthTest.testMultipleAuthTypes
  _ <- CookieAuthTest.testApiKeyCookieAuth
  _ <- CookieAuthTest.testRegularCookieParams
  _ <- CookieAuthTest.testMixedCookies
  _ <- OpenAPIInfoTest.testInfoDescription
  _ <- OpenAPIInfoTest.testInfoContact
  _ <- OpenAPIInfoTest.testInfoLicense
  _ <- OpenAPIInfoTest.testInfoAllFields
  _ <- ContentTypeTest.testRequestContentTypes
  _ <- ContentTypeTest.testResponseContentTypes
  _ <- ContentTypeTest.testMixedContentTypes
  _ <- ExamplesTest.testExamplesMetadataExtraction
  _ <- ExamplesTest.testExamplesInParameters
  _ <- ExamplesTest.testExamplesInRequestBody
  _ <- ExamplesTest.testExamplesWithSummary
  _ <- ExamplesTest.testExamplesBackwardCompatibility
  _ <- ExamplesTest.testExamplesInQueryParams
  _ <- ExamplesTest.testComplexExampleObject
  -- _ <- CallbackTest.testPaymentCallback
  -- _ <- CallbackTest.testMultipleCallbacks
  -- _ <- CallbackTest.testCallbackMethods
  -- _ <- CallbackTest.testAPISpecWithCallbacks
  -- _ <- CallbackTest.testRealWorldWebhooks
  SchemaComponentTest.testSchemaComponents

main :: ViTest
main = viTest spec
