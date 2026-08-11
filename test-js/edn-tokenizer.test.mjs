import test from "node:test";
import assert from "node:assert/strict";
import { ednTokens } from "../src-js/edn-tokenizer.mjs";

test("matching EDN brackets share their nesting depth", () => {
  const brackets = ednTokens("[:a {:b [1 (2)]}]").filter(token => token.type === "bracket");
  assert.deepEqual(brackets.map(({ text, depth }) => ({ text, depth })), [
    { text: "[", depth: 0 },
    { text: "{", depth: 1 },
    { text: "[", depth: 2 },
    { text: "(", depth: 3 },
    { text: ")", depth: 3 },
    { text: "]", depth: 2 },
    { text: "}", depth: 1 },
    { text: "]", depth: 0 }
  ]);
});

test("tokenizer recognizes EDN data types", () => {
  const tokens = ednTokens("{:name \"Ada\" :age 37 :active true :missing nil :id #uuid \"abc\" :initial \\A}");
  const byText = new Map(tokens.map(token => [token.text, token.type]));
  assert.equal(byText.get(":name"), "keyword");
  assert.equal(byText.get("\"Ada\""), "string");
  assert.equal(byText.get("37"), "number");
  assert.equal(byText.get("true"), "literal");
  assert.equal(byText.get("nil"), "literal");
  assert.equal(byText.get("#uuid"), "tag");
  assert.equal(byText.get("\\A"), "character");
});

test("brackets inside strings and character literals are not structural", () => {
  const tokens = ednTokens("{:text \"[not brackets]\" :character \\[}");
  const brackets = tokens.filter(token => token.type === "bracket");
  assert.deepEqual(brackets.map(token => token.text), ["{", "}"]);
});

test("unbalanced input remains renderable", () => {
  assert.deepEqual(ednTokens("]"), [{ text: "]", type: "bracket", depth: 0 }]);
});
