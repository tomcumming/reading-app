import { querySelector } from "../dom.js";

export default null;

const inputElem = querySelector(HTMLInputElement, `input[name="tokenize"]`);

const resultsElem = querySelector(HTMLElement, ".tokenize-choices");

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

  resultsElem.innerHTML = resultsHtml;
}

inputElem.addEventListener("input", onInput);
