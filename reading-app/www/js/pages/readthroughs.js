import { querySelector } from "../dom.js";

export default null;

const createButton = querySelector(
  HTMLButtonElement,
  ".create-new-readthrough",
);

async function onClickCreate() {
  const name = prompt("New Read-through name");
  if (name !== null) {
    createButton.setAttribute("disabled", "");

    const url = `/readthrough/create/${self.encodeURIComponent(name)}`;

    const res = await fetch(url, {
      method: "POST",
      body: JSON.stringify(name),
    });
    if (res.status !== 200) throw new Error(res.status.toString());

    self.location.reload();
  }
}

createButton.addEventListener("click", onClickCreate);
