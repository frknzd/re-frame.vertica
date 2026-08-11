const OPENERS = new Set(["(", "[", "{"]);
const CLOSERS = new Set([")", "]", "}"]);
const MATCHING_OPENER = { ")": "(", "]": "[", "}": "{" };

function delimiter(character) {
  return character == null || /[\s,()[\]{}";]/.test(character);
}

function tokenType(atom) {
  if (atom.startsWith(":")) return "keyword";
  if (atom.startsWith("#")) return "tag";
  if (atom.startsWith("\\")) return "character";
  if (/^(?:nil|true|false)$/.test(atom)) return "literal";
  if (/^[+-]?(?:\d+r[0-9A-Za-z]+|0[xX][0-9A-Fa-f]+|\d+(?:\.\d+)?(?:[eE][+-]?\d+)?[MN]?|\d+\/\d+)$/.test(atom)) return "number";
  return "symbol";
}

export function ednTokens(input) {
  const text = String(input ?? "");
  const tokens = [];
  const stack = [];

  function push(value, type, depth = null) {
    if (!value) return;
    const previous = tokens.at(-1);
    if (previous && previous.type === type && previous.depth === depth && type === "plain") previous.text += value;
    else tokens.push({ text: value, type, depth });
  }

  for (let index = 0; index < text.length;) {
    const character = text[index];
    if (/\s|,/.test(character)) {
      const start = index++;
      while (index < text.length && /\s|,/.test(text[index])) index += 1;
      push(text.slice(start, index), "plain");
      continue;
    }
    if (character === ";") {
      const start = index++;
      while (index < text.length && text[index] !== "\n") index += 1;
      push(text.slice(start, index), "comment");
      continue;
    }
    if (character === "\"") {
      const start = index++;
      let escaped = false;
      while (index < text.length) {
        const current = text[index++];
        if (escaped) escaped = false;
        else if (current === "\\") escaped = true;
        else if (current === "\"") break;
      }
      push(text.slice(start, index), "string");
      continue;
    }
    if (character === "\\") {
      const start = index++;
      if (index < text.length) index += 1;
      while (index < text.length && !delimiter(text[index])) index += 1;
      push(text.slice(start, index), "character");
      continue;
    }
    if (OPENERS.has(character)) {
      const depth = stack.length;
      stack.push(character);
      push(character, "bracket", depth);
      index += 1;
      continue;
    }
    if (CLOSERS.has(character)) {
      let depth = Math.max(0, stack.length - 1);
      if (stack.at(-1) === MATCHING_OPENER[character]) stack.pop();
      else depth = stack.length;
      push(character, "bracket", depth);
      index += 1;
      continue;
    }

    const start = index++;
    while (index < text.length && !delimiter(text[index])) index += 1;
    const atom = text.slice(start, index);
    push(atom, tokenType(atom));
  }
  return tokens;
}
