import { parentElements, querySelector } from "../../dom.js";

export default null;

const inputElem = querySelector(HTMLInputElement, `input[name="tokenize"]`);

const choicesElem = querySelector(HTMLElement, ".tokenize-choices");

/** @type {undefined | number} */
let debounce;

function onInput() {
  clearTimeout(debounce);
  debounce = setTimeout(tokenize, 500);
}

function getId() {
  const rtId = inputElem.getAttribute("data-rtid");
  if (rtId === null) throw new Error(`rtid`);
  return rtId;
}

async function tokenize() {
  const rtId = getId();
  const search = self.encodeURIComponent(inputElem.value);
  const resultsHtml = await fetch(
    `/readthrough/${rtId}/tokenize-choices?search=${search}`,
  ).then((res) => res.text());

  choicesElem.innerHTML = resultsHtml;
}

/** @argument e {Event} */
async function onChoiceClick(e) {
  const button = parentElements(e.target).find(
    (x) => x instanceof HTMLButtonElement,
  );
  if (!(button instanceof HTMLButtonElement)) throw new Error();

  const chtToken = button.getAttribute("data-token");
  const chtRest = button.getAttribute("data-rest");
  const chtSkipped = button.classList.contains("skip");
  if (chtToken === null || chtSkipped === null) throw new Error();

  inputElem.setAttribute("disabled", "");
  choicesElem
    .querySelectorAll("button")
    .forEach((button) => button.setAttribute("disabled", ""));

  const rtId = getId();

  const result = await fetch(`/readthrough/${rtId}/choose`, {
    method: "POST",
    body: JSON.stringify({ chtToken, chtRest, chtSkipped }),
    headers: {
      "Content-Type": "application/json",
    },
  });
  if (result.status !== 200) throw new Error();

  self.location.reload();
}

inputElem.addEventListener("input", onInput);
// choicesElem.addEventListener("click", onChoiceClick);
