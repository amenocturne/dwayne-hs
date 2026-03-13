import {
  Form,
  ActionPanel,
  Action,
  showToast,
  Toast,
  popToRoot,
  Detail,
} from "@raycast/api";
import { useState } from "react";
import { runDwayne, formatForMarkdown } from "./shared";

export default function Capture() {
  const [captured, setCaptured] = useState<{
    title: string;
    body: string;
  } | null>(null);

  async function handleSubmit(values: { title: string; body: string }) {
    if (!values.title.trim()) {
      await showToast({
        style: Toast.Style.Failure,
        title: "Title is required",
      });
      return;
    }

    const text = values.body.trim()
      ? `${values.title}\n${values.body}`
      : values.title;

    try {
      runDwayne(["capture", text]);
      setCaptured({ title: values.title, body: values.body });
    } catch (error) {
      await showToast({
        style: Toast.Style.Failure,
        title: "Capture failed",
        message: String(error),
      });
    }
  }

  if (captured) {
    let markdown = `## Captured\n\n**${captured.title}**`;
    if (captured.body.trim()) {
      markdown += `\n\n${formatForMarkdown(captured.body)}`;
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

  return (
    <Form
      actions={
        <ActionPanel>
          <Action.SubmitForm title="Capture" onSubmit={handleSubmit} />
        </ActionPanel>
      }
    >
      <Form.TextField
        id="title"
        title="Title"
        placeholder="What's on your mind?"
        autoFocus
      />
      <Form.TextArea
        id="body"
        title="Body"
        placeholder="Optional details, links..."
      />
    </Form>
  );
}
