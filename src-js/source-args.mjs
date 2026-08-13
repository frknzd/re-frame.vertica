const OPEN_TO_CLOSE = { "(": ")", "[": "]", "{": "}" };

function tokenize(source) {
  const tokens = [];
  let index = 0;
  while (index < source.length) {
    const start = index;
    const char = source[index];
    if (/\s|,/.test(char)) {
      index += 1;
      continue;
    }
    if (char === ";") {
      while (index < source.length && source[index] !== "\n") index += 1;
      continue;
    }
    if (char === '"') {
      index += 1;
      while (index < source.length) {
        if (source[index] === "\\") index += 2;
        else if (source[index++] === '"') break;
      }
      tokens.push({ type: "string", start, end: index });
      continue;
    }
    if (OPEN_TO_CLOSE[char] || Object.values(OPEN_TO_CLOSE).includes(char)) {
      tokens.push({ type: "delimiter", value: char, start, end: ++index });
      continue;
    }
    while (index < source.length && !/[\s,;()\[\]{}"]/.test(source[index])) index += 1;
    tokens.push({ type: "atom", value: source.slice(start, index), start, end: index });
  }
  return tokens;
}

function parseForms(source) {
  const tokens = tokenize(source);
  const root = { type: "root", children: [], start: 0, end: source.length };
  const stack = [root];
  for (const token of tokens) {
    const parent = stack[stack.length - 1];
    if (OPEN_TO_CLOSE[token.value]) {
      const form = {
        type: token.value === "(" ? "list" : token.value === "[" ? "vector" : "map",
        open: token.value,
        close: OPEN_TO_CLOSE[token.value],
        children: [],
        start: token.start,
        end: token.end
      };
      parent.children.push(form);
      stack.push(form);
    } else if (token.type === "delimiter") {
      if (stack.length > 1 && stack[stack.length - 1].close === token.value) {
        stack[stack.length - 1].end = token.end;
        stack.pop();
      }
    } else {
      parent.children.push(token);
    }
  }
  return root;
}

function atomValue(form) {
  return form?.type === "atom" ? form.value : null;
}

function formText(source, form) {
  return source.slice(form.start, form.end).replace(/\s+/g, " ").trim();
}

function sourcePosition(source, offset) {
  const before = source.slice(0, offset);
  const lines = before.split("\n");
  return { line: lines.length, column: lines[lines.length - 1].length + 1 };
}

function definitionNameIndex(children, start = 1) {
  let metadataPending = false;
  for (let index = start; index < children.length; index += 1) {
    const child = children[index];
    const value = atomValue(child);
    if (metadataPending) {
      metadataPending = false;
      continue;
    }
    if (value === "^") {
      metadataPending = true;
      continue;
    }
    if (value?.startsWith("^")) continue;
    if (value) return index;
  }
  return -1;
}

function signatureVectors(definition, nameIndex) {
  const tail = definition.children.slice(nameIndex + 1);
  const direct = tail.find(form => form.type === "vector");
  if (direct) return [direct];
  return tail
    .filter(form => form.type === "list")
    .map(form => form.children.find(child => child.type === "vector"))
    .filter(Boolean);
}

function nestedFnSignatures(definition, nameIndex) {
  const stack = definition.children.slice(nameIndex + 1);
  while (stack.length) {
    const form = stack.shift();
    if (form.type === "list" && ["fn", "fn*"].includes(atomValue(form.children[0]))) {
      let offset = 1;
      if (atomValue(form.children[offset]) && form.children[offset + 1]?.type !== "vector") offset += 1;
      const direct = form.children.slice(offset).find(child => child.type === "vector");
      if (direct) return [direct];
      return form.children.slice(offset)
        .filter(child => child.type === "list")
        .map(child => child.children.find(grandchild => grandchild.type === "vector"))
        .filter(Boolean);
    }
    if (form.children) stack.push(...form.children);
  }
  return [];
}

function argumentLabel(source, form) {
  const value = atomValue(form);
  if (value) return value.replace(/^\^\S+\s*/, "");
  return formText(source, form);
}

function parameterForms(vector) {
  const parameters = [];
  for (let index = 0; index < vector.children.length; index += 1) {
    const form = vector.children[index];
    const value = atomValue(form);
    if (value === "^") {
      index += 1;
      continue;
    }
    if (value?.startsWith("^")) continue;
    parameters.push(form);
  }
  return parameters;
}

function expandSignature(source, vector, arity) {
  const forms = parameterForms(vector);
  const restIndex = forms.findIndex(form => atomValue(form) === "&");
  const fixed = restIndex < 0 ? forms : forms.slice(0, restIndex);
  const rest = restIndex < 0 ? null : forms[restIndex + 1];
  if ((!rest && fixed.length !== arity) || (rest && arity < fixed.length)) return null;
  const labels = fixed.map(form => argumentLabel(source, form));
  if (rest) {
    const restLabel = argumentLabel(source, rest);
    for (let index = fixed.length; index < arity; index += 1) {
      labels.push(`${restLabel}[${index - fixed.length}]`);
    }
  }
  return labels;
}

function namespaceName(root) {
  for (const form of root.children) {
    if (form.type !== "list" || atomValue(form.children[0]) !== "ns") continue;
    const nameIndex = definitionNameIndex(form.children);
    if (nameIndex >= 0) return atomValue(form.children[nameIndex]);
  }
  return null;
}

export function indexClojureScriptSource(source, index = new Map(), url = "") {
  const root = parseForms(source);
  const namespace = namespaceName(root);
  if (!namespace) return index;

  const visit = form => {
    if (!form?.children) return;
    if (form.type === "list") {
      const operator = atomValue(form.children[0]);
      if (["defn", "defn-", "def", "defonce"].includes(operator)) {
        const nameIndex = definitionNameIndex(form.children);
        const localName = atomValue(form.children[nameIndex]);
        if (localName) {
          const signatures = operator.startsWith("defn")
            ? signatureVectors(form, nameIndex)
            : nestedFnSignatures(form, nameIndex);
          index.set(`${namespace}/${localName}`, {
            source,
            signatures,
            url,
            ...sourcePosition(source, form.start)
          });
        }
      }
    }
    for (const child of form.children) visit(child);
  };
  visit(root);
  return index;
}

export function resolveArgumentNames(index, componentName, arity) {
  const definition = index.get(componentName);
  if (!definition || !Number.isInteger(arity) || arity < 0) return null;
  for (const signature of definition.signatures) {
    const labels = expandSignature(definition.source, signature, arity);
    if (labels) return labels;
  }
  return null;
}

export function resolveSourceLocation(index, componentName) {
  const definition = index.get(componentName);
  if (!definition?.url) return null;
  return {
    componentName,
    url: definition.url,
    line: definition.line,
    column: definition.column
  };
}

function absoluteSourceUrl(source, sourceRoot, mapUrl) {
  const rooted = sourceRoot ? `${sourceRoot.replace(/\/$/, "")}/${source.replace(/^\//, "")}` : source;
  try {
    return new URL(rooted, mapUrl).href;
  } catch (_) {
    return rooted;
  }
}

function collectMap(map, result, mapUrl) {
  if (!map || typeof map !== "object") return;
  if (Array.isArray(map.sections)) {
    for (const section of map.sections) collectMap(section?.map, result, mapUrl);
  }
  if (Array.isArray(map.sourcesContent)) {
    map.sourcesContent.forEach((content, index) => {
      const source = map.sources?.[index] || "";
      if (typeof content === "string" && /\.clj[sc]?(?:$|[?#])/i.test(source)) {
        result.push({ url: absoluteSourceUrl(source, map.sourceRoot, mapUrl), content });
      }
    });
  }
}

export function embeddedSources(sourceMap, mapUrl = "") {
  const parsed = typeof sourceMap === "string" ? JSON.parse(sourceMap) : sourceMap;
  const result = [];
  collectMap(parsed, result, mapUrl);
  return result;
}

export function addSourceResource(index, url, content) {
  if (/\.map(?:$|[?#])/i.test(url)) {
    for (const source of embeddedSources(content, url)) {
      if (/\.clj[sc]?(?:$|[?#])/i.test(source.url)) {
        indexClojureScriptSource(source.content, index, source.url);
      }
    }
  } else if (/\.clj[sc]?(?:$|[?#])/i.test(url)) {
    indexClojureScriptSource(content, index, url);
  }
  return index;
}
