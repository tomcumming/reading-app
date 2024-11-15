/** @import {} from './types.d.ts' */
/** @import { Choice } from '../../api-types.d.ts' */

export class Tokenizer extends HTMLElement {
  /** @type {undefined | number} */
  debounce = undefined;

  constructor() {
    super();

    this.render();
  }

  render() {
    this.innerHTML = `
      <input />
      <div data-choices></div>
    `;

    this.querySelector(":scope > input")?.addEventListener("input", () =>
      this.onChange(),
    );
  }

  onChange() {
    self.clearTimeout(this.debounce);
    this.debounce = self.setTimeout(() => this.onChangeDelay(), 500);
    this.setChoices("loading");
  }

  readThroughId() {
    const rtId = this.getAttribute("data-id");
    if (rtId === null) throw new Error(`ReadThrough ID was not provided`);
    return rtId;
  }

  async onChangeDelay() {
    /** @type {null | HTMLInputElement} */
    const input = this.querySelector(":scope > input");
    if (input === null) throw new Error(`Cant find input`);

    const trimmed = input.value.trim();
    const encoded = self.encodeURIComponent(trimmed);
    const rtId = this.readThroughId();
    /** @type {Choice[]} */
    const choices = await fetch(
      `/readthrough/${rtId}/tokenize?s=${encoded}`,
    ).then((res) => res.json());
    this.setChoices(choices);
  }

  /** @arg choicesOrLoading {'loading' | Choice[]} */
  setChoices(choicesOrLoading) {
    const listElem = this.querySelector(":scope > div[data-choices]");
    if (listElem === null) throw new Error(`Could not find list`);

    if (choicesOrLoading === "loading") {
      listElem.innerHTML = `<p class="loading-message">Loading...</p>`;
    } else {
      // TODO make this button.choice display:flex with gap
      listElem.innerHTML = choicesOrLoading
        .map(
          ({ choText, choRest }) => `<button class="choice">
            <span>${choText}</span>
            ${choRest.map((s) => `<span>${s}</span>`).join("")}
            </button>`,
        )
        .join("");
    }
  }
}

self.customElements.define("reading-tokenizer", Tokenizer);
