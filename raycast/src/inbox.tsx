import { showToast, Toast, LaunchProps, showHUD } from "@raycast/api";
import { runDwayne, shortenUrls } from "./shared";

export default async function Inbox(
  props: LaunchProps<{ arguments: { text: string } }>
) {
  const { text } = props.arguments;

  try {
    runDwayne(["capture", text]);
    await showHUD(`Captured: ${shortenUrls(text)}`);
  } catch (error) {
    await showToast({
      style: Toast.Style.Failure,
      title: "Capture failed",
      message: String(error),
    });
  }
}
