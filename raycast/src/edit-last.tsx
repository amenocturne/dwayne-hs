import {
  Form,
  ActionPanel,
  Action,
  showToast,
  Toast,
  popToRoot,
} from "@raycast/api";
import { useState, useEffect } from "react";
import { readLastEntry, saveLastEntry, LastEntry } from "./shared";

export default function EditLast() {
  const [entry, setEntry] = useState<LastEntry | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [title, setTitle] = useState("");
  const [body, setBody] = useState("");

  useEffect(() => {
    try {
      const last = readLastEntry();
      if (!last) {
        setError("No entries found in inbox file");
        return;
      }
      setEntry(last);
      setTitle(last.titleLine);
      setBody(last.body);
    } catch (e) {
      setError(String(e));
    }
  }, []);

  useEffect(() => {
    if (error) {
      showToast({ style: Toast.Style.Failure, title: "Error", message: error });
    }
  }, [error]);

  async function handleSubmit(values: { title: string; body: string }) {
    if (!entry) return;
    if (!values.title.trim()) {
      await showToast({
        style: Toast.Style.Failure,
        title: "Title is required",
      });
      return;
    }

    try {
      saveLastEntry(entry, values.title, values.body);
      await showToast({
        style: Toast.Style.Success,
        title: "Updated",
        message: values.title,
      });
      await popToRoot();
    } catch (e) {
      await showToast({
        style: Toast.Style.Failure,
        title: "Save failed",
        message: String(e),
      });
    }
  }

  if (error) {
    return (
      <Form>
        <Form.Description text={`Error: ${error}`} />
      </Form>
    );
  }

  if (!entry) {
    return (
      <Form>
        <Form.Description text="Loading..." />
      </Form>
    );
  }

  return (
    <Form
      actions={
        <ActionPanel>
          <Action.SubmitForm title="Save" onSubmit={handleSubmit} />
        </ActionPanel>
      }
    >
      <Form.Description
        title="Editing"
        text={`Last ${entry.keyword} entry`}
      />
      <Form.TextField
        id="title"
        title="Title"
        value={title}
        onChange={setTitle}
        autoFocus
      />
      <Form.TextArea
        id="body"
        title="Body"
        value={body}
        onChange={setBody}
      />
    </Form>
  );
}
