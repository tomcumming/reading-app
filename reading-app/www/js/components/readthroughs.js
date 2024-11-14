/** @import * as api from '../api-types.d.ts' */

const createButtonSelector = "reading-readthroughs > button[data-create-new]";

const style = `<style>
reading-readthroughs {
  display: grid;
}

reading-readthroughs > button[data-create-new] {
  justify-self: end;
}

reading-readthroughs > .readthrough-list {
display: grid;
justify-self: start;
}
</style>`;

export class ReadThroughs extends HTMLElement {
  /** @type {'loading' | api.ReadThs } */
  readthroughs = "loading";

  constructor() {
    super();

    this.render();
  }

  render() {
    const rths =
      this.readthroughs === "loading"
        ? `<div class="loading">Loading...</div>`
        : renderList(this.readthroughs);

    this.innerHTML = `
      ${style}
      <h1>Read-Throughs</h1>
      <button data-create-new>➕ Create new readthough</button>
      ${rths}
    `;
    this.querySelector(createButtonSelector)?.addEventListener("click", () =>
      this.onCreateNewClicked(),
    );
  }

  async refreshReadThroughs() {
    const response = await fetch("/readthrough");
    this.readthroughs = await response.json();
    this.render();
  }

  async onCreateNewClicked() {
    const name = prompt("Name of reading material");

    if (name === null) return;

    document.querySelector(createButtonSelector)?.setAttribute("disabled", "");

    const response = await fetch("/readthrough/create", {
      method: "post",
      body: JSON.stringify(name),
      headers: {
        Accept: "application/json",
        "Content-Type": "application/json",
      },
    });
    await response.json();

    document.querySelector(createButtonSelector)?.removeAttribute("disabled");
    this.refreshReadThroughs();
  }

  connectedCallback() {
    this.refreshReadThroughs();
  }
}

self.customElements.define("reading-readthroughs", ReadThroughs);

// TODO sort

/** @type {(rths: api.ReadThs) => string} */
const renderList = (rths) => `<div class="readthrough-list">
  ${rths.map(renderReadThroughLink).join("")}
</div>`;

/** @type {(rth: [number, api.ReadTh]) => string} */
const renderReadThroughLink = ([rtId, rth]) =>
  `<a class="button" href="#readthrough/${rtId}">${rth.rthName}</a>`;
