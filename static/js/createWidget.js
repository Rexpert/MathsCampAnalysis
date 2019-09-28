/**
 * Let iframe responsive by using bootstrap 4 embed responsive classes.
 * Attach responsive div as iframe parent
 *
 * @param {HTMLIFrameElement} iframe iframe element
 */
function generateResponsiveIframe(iframe) {
  var parent = iframe.parentNode;
  var wrapper = document.createElement("div");
  wrapper.className += " embed-responsive";

  // using aspect ratio of 1by1 and no border for
  // liquid chart
  if (iframe.src.indexOf("liquid") > -1) {
    wrapper.className += " embed-responsive-1by1";
  } else {
    wrapper.className += " embed-responsive-16by9";
    iframe.style.border = "2px solid black";
    iframe.style.padding = "1.5rem";
  }

  iframe.className += " embed-responsive-item";

  parent.replaceChild(wrapper, iframe);
  wrapper.appendChild(iframe);

  iframe.style.display = "block";
}

/**
 * simple function to avoid repetition of adding
 * bootstrap mt-4 class to section
 */
function addSectionMargin() {
  const sections = document.querySelectorAll("section");

  for (var i = 0; i < sections.length; i++) {
    var section = sections[i];
    section.className += " mt-4";
  }
}

window.addEventListener("load", function() {
  // making iframe responsive
  var iframes = document.querySelectorAll("iframe");
  for (var i = 0; i < iframes.length; i++) {
    generateResponsiveIframe(iframes[i]);
  }
});

window.addEventListener("DOMContentLoaded", function() {
  // add margin to sections
  addSectionMargin();
});
