export const securityShape = (source) => {
  const security = JSON.parse(source).paths["/combined"].get.security;
  return {
    requirementCount: security.length,
    schemeCount: Object.keys(security[0] ?? {}).length,
  };
};
