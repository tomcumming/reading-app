class App extends HTMLElement {
  constructor() {
    super();

    this.render();

    self.addEventListener("hashchange", () => this.onHashChange());
  }

  render() {
    const hash = self.location.hash;

    {
      const match = /^#readthrough\/(\d+)$/.exec(hash);
      if (match !== null) throw new Error(`Todo readthrough ${match[1]}`);
    }

    if (hash === "") {
      this.innerHTML = `<reading-readthroughs />`;
      return;
    }

    throw new Error(`Unhandled route '${hash}'`);
  }

  connectedCallback() {}

  onHashChange() {
    this.render();
  }
}

customElements.define("reading-app", App);
