/** @type {{
  pattern: RegExp;
  render: (match: RegExpMatchArray) => string;
}[]} */
const routes = [
  {
    pattern: /^$/,
    render: () => `<reading-readthroughs />`,
  },
  {
    pattern: /^#readthrough\/(\d+)$/,
    render: ([_, rtId]) => `<reading-readthrough data-id="${rtId}" />`,
  },
];

class App extends HTMLElement {
  constructor() {
    super();

    this.render();

    self.addEventListener("hashchange", () => this.onHashChange());
  }

  render() {
    const hash = self.location.hash;

    for (const { pattern, render } of routes) {
      const match = pattern.exec(hash);
      if (match !== null) {
        this.innerHTML = render(match);
        return;
      }
    }

    throw new Error(`Unhandled route '${hash}'`);
  }

  connectedCallback() {}

  onHashChange() {
    this.render();
  }
}

customElements.define("reading-app", App);
