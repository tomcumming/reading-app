/** @import {State} from './readthrough-types.d.ts' */
/** @import * as api from '../api-types.d.ts' */

export class ReadThrough extends HTMLElement {
  /** @type {State} */
  state = { loading: true };

  constructor() {
    super();

    this.loadInitial();
  }

  currentId() {
    const rtId = this.getAttribute("data-id");
    if (rtId === null) throw new Error(`Could not read readthrough id`);
    return rtId;
  }

  async loadInitial() {
    const rtId = this.currentId();
    /** @type {api.ReadTh} */
    const json = await fetch(`/readthrough/${rtId}`).then((res) => res.json());
    this.state = {
      tokenize: {
        readTh: json,
      },
    };
    this.render();
  }

  render() {
    if ("loading" in this.state) {
      this.innerHTML = "<p> Loading... </p>";
    } else {
      this.innerHTML = "<p> TODO tokenize </p>";
    }
  }
}

self.customElements.define("reading-readthrough", ReadThrough);
