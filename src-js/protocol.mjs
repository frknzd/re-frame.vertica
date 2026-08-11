export const PROTOCOL_VERSION = 1;

export function transitToPlain(value, transitApi) {
  const api = transitApi.isMap ? transitApi : transitApi.default;
  if (api.isMap(value)) {
    const result = {};
    value.forEach((entryValue, entryKey) => { result[String(entryKey)] = transitToPlain(entryValue, api); });
    return result;
  }
  if (api.isSet(value)) return Array.from(value.values(), item => transitToPlain(item, api));
  if (Array.isArray(value)) return value.map(item => transitToPlain(item, api));
  return value;
}

export function bridgeExpression(method, argumentExpression = "") {
  const key = JSON.stringify(method);
  return `(()=>{const b=globalThis.__RE_FRAME_VERTICA__;if(!b)return null;const f=b[${key}];if(typeof f!=="function")throw new Error("Bridge method unavailable: "+${key});return f(${argumentExpression});})()`;
}

export function compatibilityMessage(capabilities) {
  if (!capabilities) return "Preload missing. Add re-frame.vertica.preload before application namespaces and reload the page.";
  if (capabilities.protocol !== PROTOCOL_VERSION) {
    return `Bridge protocol ${capabilities.protocol ?? "unknown"} is incompatible with extension protocol ${PROTOCOL_VERSION}. Upgrade the preload and extension together.`;
  }
  if (!capabilities["registration-hook"]) return "Preload loaded too late: subscription registration could not be instrumented.";
  if (!capabilities["react-supported"]) return `React ${capabilities["react-major"] ?? "unknown"} is unsupported; React 17 or 18 is required.`;
  return "Connected";
}
