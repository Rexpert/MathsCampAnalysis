/**
 * Parse HTML doc from markdown files in /static/markdown, then
 * attach the parsed markdown to HTML div
 *
 * @param {string} path path to markdown file
 * @returns div populated with parsed markdown
 */
const generateHTMLFromMarkdown = async path => {
  const markedDiv = document.createElement("div");
  const res = await fetch(path);
  const data = await res.text();

  markedDiv.innerHTML = marked(data);

  return markedDiv;
};

/**
 * Let iframe responsive by using bootstrap 4 embed responsive classes.
 * Attach responsive div as iframe parent
 *
 * @param {HTMLIFrameElement} iframe iframe element
 */
const generateResponsiveIframe = iframe => {
  const parent = iframe.parentNode;
  const wrapper = document.createElement("div");
  wrapper.classList.add("embed-responsive", "embed-responsive-16by9");
  iframe.classList.add("embed-responsive-item");

  parent.replaceChild(wrapper, iframe);
  wrapper.appendChild(iframe);
};

window.addEventListener("load", async () => {
  const homeMainElem = document.querySelector("#home-page-main");
  const markedDiv = await generateHTMLFromMarkdown(
    "./static/markdown/homepage.md"
  );

  // attach parsed markdown to home page main content
  homeMainElem.appendChild(markedDiv);

  const iframes = document.querySelectorAll("iframe");
  for (const iframe of iframes) {
    generateResponsiveIframe(iframe);
  }
});
