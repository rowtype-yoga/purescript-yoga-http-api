module Test.Operators where

import Prelude
import Effect (Effect)
import ViTest (ViTest, viTest)
import Test.OperatorTest as OperatorTest
import Test.ParserTest as ParserTest
import Test.VariantResponseTest as VariantResponseTest
import Test.MetadataValidationTest as MetadataValidationTest
import Test.APIRecordTest as APIRecordTest
import Test.SchemaComponentTest as SchemaComponentTest
import Test.BearerAuthTest as BearerAuthTest
import Test.CookieAuthTest as CookieAuthTest
import Test.OpenAPIInfoTest as OpenAPIInfoTest
import Test.ContentTypeTest as ContentTypeTest
import Test.ExamplesTest as ExamplesTest

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
  SchemaComponentTest.testSchemaComponents

main :: ViTest
main = viTest spec
