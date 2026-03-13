import {
  Form,
  ActionPanel,
  Action,
  showToast,
  Toast,
  popToRoot,
} from "@raycast/api";
import { runDwayne } from "./shared";

export default function Capture() {
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
      runDwayne(["capture", "--no-enrich", text]);
      await showToast({
        style: Toast.Style.Success,
        title: "Captured",
        message: values.title,
      });
      await popToRoot();
    } catch (error) {
      await showToast({
        style: Toast.Style.Failure,
        title: "Capture failed",
        message: String(error),
      });
    }
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
