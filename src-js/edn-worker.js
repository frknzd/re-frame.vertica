import { ednTokens } from "./edn-tokenizer.mjs";

self.addEventListener("message", event => {
  const { id, text } = event.data;
  self.postMessage({ id, tokens: ednTokens(text) });
});
