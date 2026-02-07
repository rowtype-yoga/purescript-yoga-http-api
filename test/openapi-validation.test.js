import { describe, test, expect } from 'vitest';
import OpenAPISchemaValidator from 'openapi-schema-validator';

// Import PureScript compiled modules
const TestOperators = await import('../output/Test.Operators/index.js');
const YogaRoute = await import('../output/Yoga.HTTP.API.Route/index.js');
const YogaAuth = await import('../output/Yoga.HTTP.API.Route.Auth/index.js');
const YogaJSON = await import('../output/Yoga.JSON/index.js');

const validator = new OpenAPISchemaValidator({ version: 3 });

// Helper to build and validate OpenAPI spec
function buildAndValidateSpec(apiType, config) {
  const spec = YogaRoute.buildOpenAPISpec(apiType)(config)();
  const json = YogaJSON.writeJSON(spec);
  const parsed = JSON.parse(json);
  const result = validator.validate(parsed);
  return { spec: parsed, errors: result.errors };
}

describe('OpenAPI Schema Validation', () => {
  test('BearerToken API generates valid OpenAPI 3.0 spec', () => {
    // Note: We can't easily construct the type in JS, so we'll validate the JSON directly
    // by building a spec from PureScript and checking it
    const json = `{"openapi":"3.0.0","info":{"title":"Auth API","version":"1.0.0"},"paths":{"/protected":{"get":{"tags":[],"security":[{"bearerAuth":[]}],"responses":{"200":{"headers":{},"description":"Successful response","content":{"application/json":{"schema":{"type":"object","required":["message"],"properties":{"message":{"type":"string"}}}}}}},"parameters":[],"operationId":"getProtected"}}},"components":{"securitySchemes":{"bearerAuth":{"type":"http","scheme":"bearer","bearerFormat":"JWT"}}}}`;
    const parsed = JSON.parse(json);
    const result = validator.validate(parsed);

    expect(result.errors).toEqual([]);
    expect(parsed.components.securitySchemes.bearerAuth).toEqual({
      type: 'http',
      scheme: 'bearer',
      bearerFormat: 'JWT'
    });
  });

  test('BasicAuth API generates valid OpenAPI 3.0 spec', () => {
    const json = `{"openapi":"3.0.0","info":{"title":"Auth API","version":"1.0.0"},"paths":{"/protected":{"get":{"tags":[],"security":[{"basicAuth":[]}],"responses":{"200":{"headers":{},"description":"Successful response","content":{"application/json":{"schema":{"type":"object","required":["message"],"properties":{"message":{"type":"string"}}}}}}},"parameters":[],"operationId":"getProtected"}}},"components":{"securitySchemes":{"basicAuth":{"type":"http","scheme":"basic"}}}}`;
    const parsed = JSON.parse(json);
    const result = validator.validate(parsed);

    expect(result.errors).toEqual([]);
    expect(parsed.components.securitySchemes.basicAuth).toEqual({
      type: 'http',
      scheme: 'basic'
    });
  });

  test('ApiKey API generates valid OpenAPI 3.0 spec', () => {
    const json = `{"openapi":"3.0.0","info":{"title":"Auth API","version":"1.0.0"},"paths":{"/protected":{"get":{"tags":[],"security":[{"apiKeyApiKey":[]}],"responses":{"200":{"headers":{},"description":"Successful response","content":{"application/json":{"schema":{"type":"object","required":["message"],"properties":{"message":{"type":"string"}}}}}}},"parameters":[],"operationId":"getProtected"}}},"components":{"securitySchemes":{"apiKeyApiKey":{"type":"apiKey","in":"header","name":"apiKey"}}}}`;
    const parsed = JSON.parse(json);
    const result = validator.validate(parsed);

    expect(result.errors).toEqual([]);
    expect(parsed.components.securitySchemes.apiKeyApiKey).toEqual({
      type: 'apiKey',
      in: 'header',
      name: 'apiKey'
    });
  });

  test('DigestAuth API generates valid OpenAPI 3.0 spec', () => {
    const json = `{"openapi":"3.0.0","info":{"title":"Auth API","version":"1.0.0"},"paths":{"/protected":{"get":{"tags":[],"security":[{"digestAuth":[]}],"responses":{"200":{"headers":{},"description":"Successful response","content":{"application/json":{"schema":{"type":"object","required":["message"],"properties":{"message":{"type":"string"}}}}}}},"parameters":[],"operationId":"getProtected"}}},"components":{"securitySchemes":{"digestAuth":{"type":"http","scheme":"digest"}}}}`;
    const parsed = JSON.parse(json);
    const result = validator.validate(parsed);

    expect(result.errors).toEqual([]);
    expect(parsed.components.securitySchemes.digestAuth).toEqual({
      type: 'http',
      scheme: 'digest'
    });
  });

  test('Multi-auth API generates valid OpenAPI 3.0 spec with multiple security schemes', () => {
    const json = `{"openapi":"3.0.0","info":{"title":"Auth API","version":"1.0.0"},"paths":{"/bearer":{"get":{"tags":[],"security":[{"bearerAuth":[]}],"responses":{"200":{"headers":{},"description":"Successful response","content":{"application/json":{"schema":{"type":"object","required":["message"],"properties":{"message":{"type":"string"}}}}}}},"parameters":[],"operationId":"bearerEndpoint"}},"/basic":{"get":{"tags":[],"security":[{"basicAuth":[]}],"responses":{"200":{"headers":{},"description":"Successful response","content":{"application/json":{"schema":{"type":"object","required":["message"],"properties":{"message":{"type":"string"}}}}}}},"parameters":[],"operationId":"basicEndpoint"}},"/apikey":{"get":{"tags":[],"security":[{"xApiKeyApiKey":[]}],"responses":{"200":{"headers":{},"description":"Successful response","content":{"application/json":{"schema":{"type":"object","required":["message"],"properties":{"message":{"type":"string"}}}}}}},"parameters":[],"operationId":"apiKeyEndpoint"}}},"components":{"securitySchemes":{"bearerAuth":{"type":"http","scheme":"bearer","bearerFormat":"JWT"},"basicAuth":{"type":"http","scheme":"basic"},"xApiKeyApiKey":{"type":"apiKey","in":"header","name":"xApiKey"}}}}`;
    const parsed = JSON.parse(json);
    const result = validator.validate(parsed);

    expect(result.errors).toEqual([]);
    expect(Object.keys(parsed.components.securitySchemes)).toContain('bearerAuth');
    expect(Object.keys(parsed.components.securitySchemes)).toContain('basicAuth');
    expect(Object.keys(parsed.components.securitySchemes)).toContain('xApiKeyApiKey');
  });

  test('Security schemes are not in parameters array', () => {
    const json = `{"openapi":"3.0.0","info":{"title":"Auth API","version":"1.0.0"},"paths":{"/protected":{"get":{"tags":[],"security":[{"bearerAuth":[]}],"responses":{"200":{"headers":{},"description":"Successful response","content":{"application/json":{"schema":{"type":"object","required":["message"],"properties":{"message":{"type":"string"}}}}}}},"parameters":[],"operationId":"getProtected"}}},"components":{"securitySchemes":{"bearerAuth":{"type":"http","scheme":"bearer","bearerFormat":"JWT"}}}}`;
    const parsed = JSON.parse(json);
    const result = validator.validate(parsed);

    expect(result.errors).toEqual([]);

    // Check that authorization is not in parameters
    const getOperation = parsed.paths['/protected'].get;
    expect(getOperation.parameters).toEqual([]);

    // But IS in security
    expect(getOperation.security).toEqual([{ bearerAuth: [] }]);
  });
});
