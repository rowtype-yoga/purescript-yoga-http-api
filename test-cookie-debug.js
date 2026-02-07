import { buildOpenAPISpec } from './output/Yoga.HTTP.API.Route/index.js';
import { writeJSON } from './output/Yoga.JSON/index.js';

// ApiKeyCookieAPI type equivalent
const spec = buildOpenAPISpec()({ title: "Cookie Auth API", version: "1.0.0", description: null, contact: null, license: null });
const json = writeJSON(spec);
console.log("Full spec:");
console.log(json);
console.log("\nSearching for sessionId in parameters:");
const hasSessionIdParam = json.includes('"name":"sessionId"') && json.includes('"in":"cookie"');
console.log("Has sessionId param:", hasSessionIdParam);
console.log("\nSearching for sessionIdCookie in security:");
const hasSessionIdCookie = json.includes("sessionIdCookie");
console.log("Has sessionIdCookie:", hasSessionIdCookie);
