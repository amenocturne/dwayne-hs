import {
  Detail,
  LaunchProps,
  popToRoot,
  ActionPanel,
  Action,
} from "@raycast/api";
import { usePromise } from "@raycast/utils";
import { runDwayneAsync, readLastEntry } from "./shared";

function orgToMarkdown(text: string): string {
  return text.replace(/\[\[([^\]]+)\]\[([^\]]+)\]\]/g, "[$2]($1)");
}

// Module-level singleton so re-renders / strict-mode double-mounts /
// hot-reload all share one capture+read pipeline. Each Raycast command
// launch is a fresh process, so this resets per invocation.
let capturePromise: Promise<{ title: string; body: string }> | null = null;

export default function Inbox(
  props: LaunchProps<{ arguments: { text: string } }>
) {
  const { text } = props.arguments;

  const { isLoading, data, error } = usePromise(async () => {
    if (!capturePromise) {
      capturePromise = (async () => {
        await runDwayneAsync(["capture", text]);
        const entry = readLastEntry();
        return entry
          ? { title: entry.titleLine, body: entry.body }
          : { title: text, body: "" };
      })();
    }
    return capturePromise;
  });

  if (error) {
    return <Detail markdown={`## Capture Failed\n\n${String(error)}`} />;
  }

  if (isLoading || !data) {
    return <Detail isLoading markdown="" />;
  }

  let markdown = `## Captured\n\n**${orgToMarkdown(data.title)}**`;
  if (data.body) {
    markdown += `\n\n${orgToMarkdown(data.body)}`;
  }

  return (
    <Detail
      markdown={markdown}
      actions={
        <ActionPanel>
          <Action title="Done" onAction={popToRoot} />
        </ActionPanel>
      }
    />
  );
}
