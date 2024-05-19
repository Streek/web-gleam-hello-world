import * as $lustre from "../lustre/lustre.mjs";
import * as $element from "../lustre/lustre/element.mjs";
import { makeError } from "./gleam.mjs";

export function main() {
  let app = $lustre.element($element.text("Hello, world!"));
  let $ = $lustre.start(app, "#app", undefined);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "web_gleam_hello_world",
      6,
      "main",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  return undefined;
}
