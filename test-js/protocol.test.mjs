import test from "node:test";
import assert from "node:assert/strict";
import * as transit from "transit-js";
import { bridgeExpression, compatibilityMessage, PROTOCOL_VERSION, transitToPlain } from "../src-js/protocol.mjs";

test("bridge expressions keep DOM expressions in the inspected context", () => {
  const expression = bridgeExpression("selectElement", "$0");
  assert.match(expression, /selectElement/);
  assert.match(expression, /f\(\$0\)/);
  assert.doesNotMatch(expression, /eval\(/);
});

test("missing and incompatible bridges are actionable", () => {
  assert.match(compatibilityMessage(null), /Preload missing/);
  assert.match(compatibilityMessage({ protocol: 99 }), /Upgrade the preload and extension together/);
  assert.equal(compatibilityMessage({ protocol: PROTOCOL_VERSION, "registration-hook": true, "react-supported": true }), "Connected");
});

test("late preload and unsupported React have distinct status", () => {
  assert.match(compatibilityMessage({ protocol: 1, "registration-hook": false }), /loaded too late/);
  assert.match(compatibilityMessage({ protocol: 1, "registration-hook": true, "react-supported": false, "react-major": 19 }), /React 19/);
});

test("Transit maps become plain panel payloads", () => {
  const decoded = transit.reader("json").read('["^ ","protocol",1,"warnings",[["^ ","message","partial"]]]');
  assert.deepEqual(transitToPlain(decoded, transit), { protocol: 1, warnings: [{ message: "partial" }] });
});
