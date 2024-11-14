class App extends HTMLElement {
  constructor() {
    super();

    this.render();
  }

  render() {
    // TODO handle route change
    // console.log(self.location.hash);

    this.innerHTML = `<reading-readthroughs />`;
  }

  connectedCallback() {
    // TODO hash change handler
  }
}

customElements.define("reading-app", App);
