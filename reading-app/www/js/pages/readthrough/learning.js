/** @import HanziWriter from "hanzi-writer" */
/** @import { HanziWriterOptions } from "hanzi-writer" */

/** @type {typeof HanziWriter} */
// @ts-ignore
const hanziWriter = self.HanziWriter;

const hanziWriterTarget = document.querySelector(".hanzi-writer-target");

if (hanziWriterTarget instanceof HTMLElement) {
  /** @type {Partial<HanziWriterOptions>} */
  const options = {
    showHintAfterMisses: 1,
    charDataLoader: function (char, onComplete) {
      fetch(`/hanzi-writer/${char}.json`)
        .then((res) => res.json())
        .then(onComplete);
    },
    onComplete: (summaryData) => console.log(summaryData),
  };

  const char = hanziWriterTarget.getAttribute("data-char");
  if (char === null) throw new Error();

  const hw = hanziWriter.create(hanziWriterTarget, char, options);

  hw.quiz();
}
