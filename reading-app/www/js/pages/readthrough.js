import { parentElements, querySelector } from "../dom.js";

export default null;

const inputElem = querySelector(HTMLInputElement, `input[name="tokenize"]`);

const choicesElem = querySelector(HTMLElement, ".tokenize-choices");

/** @type {undefined | number} */
let debounce;

function onInput() {
  clearTimeout(debounce);
  debounce = setTimeout(tokenize, 500);
}

async function tokenize() {
  const rtId = inputElem.getAttribute("data-rtid");
  if (rtId === null) throw new Error(`rtid`);

  const search = self.encodeURIComponent(inputElem.value);
  const resultsHtml = await fetch(
    `/readthrough/${rtId}/tokenize-choices?search=${search}`,
  ).then((res) => res.text());

  choicesElem.innerHTML = resultsHtml;
}

/** @argument e {Event} */
function onChoiceClick(e) {
  const button = parentElements(e.target).find(
    (x) => x instanceof HTMLButtonElement,
  );
  if (!(button instanceof HTMLButtonElement)) throw new Error();

  const token = button.getAttribute("data-token");
  const rest = button.getAttribute("data-rest");
  const skip = button.classList.contains("skip");
  if (token === null || rest === null) throw new Error();

  inputElem.setAttribute("disabled", "");
  choicesElem
    .querySelectorAll("button")
    .forEach((button) => button.setAttribute("disabled", ""));
}

inputElem.addEventListener("input", onInput);
choicesElem.addEventListener("click", onChoiceClick);
