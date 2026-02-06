import { describe, it, expect, beforeAll } from "vitest";
import { execSync } from "child_process";
import { readdirSync, readFileSync } from "fs";
import { join } from "path";

const casesDir = join(__dirname, "cases");
const outputDir = "output-compile-fail";

function getSourceGlobs() {
  const raw = execSync("npx spago sources", {
    encoding: "utf-8",
    cwd: join(__dirname, ".."),
    stdio: ["pipe", "pipe", "pipe"],
  });
  return raw
    .split("\n")
    .filter((l) => l.trim() && !l.startsWith("test/"))
    .map((g) => `"${g}"`)
    .join(" ");
}

let sourceGlobs;

beforeAll(() => {
  sourceGlobs = getSourceGlobs();
  try {
    execSync(
      `npx purs compile --output ${outputDir} ${sourceGlobs}`,
      {
        cwd: join(__dirname, ".."),
        encoding: "utf-8",
        stdio: ["pipe", "pipe", "pipe"],
      }
    );
  } catch {
    // Warming cache - ignore errors
  }
}, 120_000);

const cases = readdirSync(casesDir)
  .filter((f) => f.endsWith(".purs"))
  .sort();

describe("Compile-fail tests", () => {
  for (const file of cases) {
    const filePath = join(casesDir, file);
    const firstLine = readFileSync(filePath, "utf-8").split("\n")[0];
    const expectMatch = firstLine.match(/^-- EXPECT:\s*(.+)/);
    const expectedSubstring = expectMatch ? expectMatch[1].trim() : null;
    const testName = file.replace(".purs", "");

    it(testName, () => {
      let output = "";
      let exitCode = 0;
      try {
        execSync(
          `npx purs compile --output ${outputDir} ${sourceGlobs} "${filePath}"`,
          {
            cwd: join(__dirname, ".."),
            encoding: "utf-8",
            stdio: ["pipe", "pipe", "pipe"],
          }
        );
      } catch (err) {
        exitCode = err.status;
        output = (err.stdout || "") + (err.stderr || "");
      }

      expect(exitCode, `Expected ${file} to fail compilation but it succeeded`).not.toBe(0);

      if (expectedSubstring) {
        expect(output, `Expected error to contain "${expectedSubstring}"`).toContain(expectedSubstring);
      }
    }, 30_000);
  }
});
