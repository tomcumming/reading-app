/** @template Elem 
@argument type {new () => Elem}
@argument query {string}
@returns {Elem}
*/
export function querySelector(type, query) {
  const element = document.querySelector(query);
  if (element === null) throw new Error(`Can't find element '${query}'`);
  if (element instanceof type) return element;
  throw new Error(`Element wrong type '${query}'`);
}
