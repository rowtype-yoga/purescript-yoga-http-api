import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["output/Test.Operators/index.js"],
    globals: true,
    environment: "jsdom",
  },
});
