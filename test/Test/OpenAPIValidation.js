import OpenAPISchemaValidator from "openapi-schema-validator";

const validator = new OpenAPISchemaValidator({ version: 3 });

export const validateImpl = (spec) => validator.validate(spec);
