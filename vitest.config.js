import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: [
      "output/Test.Operators/index.js",
      "test-compile-fail/compile-fail.test.js",
    ],
    globals: true,
    environment: "jsdom",
  },
});
