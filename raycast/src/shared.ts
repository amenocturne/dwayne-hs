import { getPreferenceValues } from "@raycast/api";
import { execSync, exec } from "child_process";
import { promisify } from "util";
import { readFileSync, writeFileSync } from "fs";
import { homedir, userInfo } from "os";
import { basename } from "path";

const execAsync = promisify(exec);

interface Preferences {
  configPath: string;
  dwaynePath: string;
}

export function expandHome(path: string): string {
  if (path.startsWith("~/")) {
    return path.replace("~", homedir());
  }
  return path;
}

function getPrefs(): Preferences {
  return getPreferenceValues<Preferences>();
}

function getUser(): string {
  if (process.env.USER) return process.env.USER;
  try {
    const u = userInfo().username;
    if (u) return u;
  } catch {
    // fall through
  }
  return basename(homedir());
}

function makeDwayneEnv(): NodeJS.ProcessEnv {
  const prefs = getPrefs();
  const home = homedir();
  const user = getUser();
  const env: NodeJS.ProcessEnv = {
    ...process.env,
    USER: user,
    PATH: `${home}/.local/bin:${home}/.cabal/bin:${home}/.nix-profile/bin:/etc/profiles/per-user/${user}/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:${process.env.PATH || ""}`,
  };
  if (prefs.configPath) {
    env.DWAYNE_CONFIG = expandHome(prefs.configPath);
  }
  return env;
}

export function runDwayne(args: string[]): string {
  const prefs = getPrefs();
  const binary = expandHome(prefs.dwaynePath || "dwayne");
  const escaped = args.map((a) => `'${a.replace(/'/g, "'\\''")}'`).join(" ");
  return execSync(`${binary} ${escaped}`, {
    env: makeDwayneEnv(),
    encoding: "utf-8",
    timeout: 15000,
  }).trim();
}

export async function runDwayneAsync(args: string[]): Promise<string> {
  const prefs = getPrefs();
  const binary = expandHome(prefs.dwaynePath || "dwayne");
  const escaped = args.map((a) => `'${a.replace(/'/g, "'\\''")}'`).join(" ");
  const { stdout } = await execAsync(`${binary} ${escaped}`, {
    env: makeDwayneEnv(),
    encoding: "utf-8",
    timeout: 15000,
  });
  return stdout.trim();
}

export function getInboxFilePath(): string {
  const prefs = getPrefs();
  const configPath = expandHome(prefs.configPath || "~/.config/dwayne/config.yml");
  const content = readFileSync(configPath, "utf-8");
  const match = content.match(/^inboxFile:\s*(.+)$/m);
  if (!match) {
    throw new Error("inboxFile not found in dwayne config");
  }
  return expandHome(match[1].trim());
}

export interface LastEntry {
  keyword: string;
  titleLine: string;
  body: string;
  startOffset: number;
  endOffset: number;
  propertiesBlock: string;
}

export function readLastEntry(): LastEntry | null {
  const filePath = getInboxFilePath();
  const content = readFileSync(filePath, "utf-8");

  // Find the last top-level heading
  const headingRegex = /^\* (\S+) (.+)$/gm;
  let lastMatch: RegExpExecArray | null = null;
  let match: RegExpExecArray | null;
  while ((match = headingRegex.exec(content)) !== null) {
    lastMatch = match;
  }
  if (!lastMatch) return null;

  const startOffset = lastMatch.index;
  const keyword = lastMatch[1];
  const titleLine = lastMatch[2];

  // Find where this entry ends (next top-level heading or EOF)
  const rest = content.substring(startOffset + lastMatch[0].length);
  const nextHeading = rest.search(/^\* /m);
  const endOffset =
    nextHeading === -1
      ? content.length
      : startOffset + lastMatch[0].length + nextHeading;

  const entryText = content.substring(startOffset, endOffset);

  // Extract PROPERTIES block
  const propsMatch = entryText.match(/:PROPERTIES:\n[\s\S]*?:END:/);
  const propertiesBlock = propsMatch ? propsMatch[0] : "";

  // Extract body (everything after :END: line)
  const endMatch = entryText.match(/:END:\n?([\s\S]*)/);
  const body = endMatch ? endMatch[1].trim() : "";

  return { keyword, titleLine, body, startOffset, endOffset, propertiesBlock };
}

export function formatForMarkdown(text: string): string {
  return text.replace(
    /(?<!\]\()https?:\/\/[^\s)\]]+/g,
    (url) => {
      try {
        const parsed = new URL(url);
        const host = parsed.hostname.replace(/^www\./, "");
        const path = parsed.pathname === "/" ? "" : parsed.pathname;
        let display = host;
        if (path) {
          const segments = path.split("/").filter(Boolean);
          if (segments.length <= 2) {
            display += "/" + segments.join("/");
          } else {
            display += "/\u2026/" + segments.slice(-1)[0];
          }
        }
        if (parsed.hash) display += parsed.hash;
        return `[${display}](${url})`;
      } catch {
        return url;
      }
    }
  );
}

export function shortenUrls(text: string): string {
  return text.replace(/https?:\/\/[^\s)\]]+/g, (url) => {
    try {
      const parsed = new URL(url);
      return parsed.hostname.replace(/^www\./, "");
    } catch {
      return url;
    }
  });
}

export function saveLastEntry(
  entry: LastEntry,
  newTitle: string,
  newBody: string
): void {
  const filePath = getInboxFilePath();
  const content = readFileSync(filePath, "utf-8");

  let replacement = `* ${entry.keyword} ${newTitle}`;
  if (entry.propertiesBlock) {
    replacement += `\n${entry.propertiesBlock}`;
  }
  if (newBody.trim()) {
    replacement += `\n\n${newBody.trim()}`;
  }
  replacement += "\n";

  const before = content.substring(0, entry.startOffset);
  const after = content.substring(entry.endOffset);
  writeFileSync(filePath, before + replacement + after, "utf-8");
}
