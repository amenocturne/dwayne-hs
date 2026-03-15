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

export default function Inbox(
  props: LaunchProps<{ arguments: { text: string } }>
) {
  const { text } = props.arguments;

  const { isLoading, data, error } = usePromise(async () => {
    await runDwayneAsync(["capture", text]);
    const entry = readLastEntry();
    return entry
      ? { title: entry.titleLine, body: entry.body }
      : { title: text, body: "" };
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
