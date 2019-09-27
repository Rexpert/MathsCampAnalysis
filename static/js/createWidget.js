/**
 * Parse HTML doc from markdown files in /static/markdown, then
 * attach the parsed markdown to HTML div
 *
 * @param {string} path path to markdown file
 * @returns promise with parsed markdown
 */
function generateHTMLFromMarkdown(path) {
  var markedDiv = document.createElement("div");
  var data;

  return fetch(path)
    .then(function(res) {
      return res.text();
    })
    .then(function(md) {
      data = md;
      markedDiv.innerHTML = marked(data);
      return markedDiv;
    });
}

/**
 * Let iframe responsive by using bootstrap 4 embed responsive classes.
 * Attach responsive div as iframe parent
 *
 * @param {HTMLIFrameElement} iframe iframe element
 */
function generateResponsiveIframe(iframe) {
  var parent = iframe.parentNode;
  var wrapper = document.createElement("div");
  wrapper.className += "embed-responsive embed-responsive-16by9";
  iframe.className += "embed-responsive-item";

  parent.replaceChild(wrapper, iframe);
  wrapper.appendChild(iframe);
}

window.addEventListener("load", function() {
  var homeMainElem = document.querySelector("#home-page-main");

  generateHTMLFromMarkdown("./static/markdown/homepage.md").then(function(
    markedDiv
  ) {
    // attach parsed markdown to home page main content
    homeMainElem.appendChild(markedDiv);

    var iframes = document.querySelectorAll("iframe");
    for (var i = 0; i < iframes.length; i++) {
      generateResponsiveIframe(iframes[i]);
    }
  });
});
