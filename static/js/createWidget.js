/**
 * Remove placeholder image from html
 *
 * This function is to support IE since child.remove
 * method does not included
 *
 * @param {HTMLImageElement} img Placeholder image
 */
function removePlaceholderImg(img) {
  var parent = img.parentNode;

  if (parent) {
    parent.removeChild(img);
  }
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
  var iframes = document.querySelectorAll("iframe");
  var placeholderImgs = document.querySelectorAll(".widget-placeholder");

  // delay iframe show up
  // display placeholder for slightly longer time
  setTimeout(function() {
    // remove placeholder first, then show iframe
    for (var i = 0; i < iframes.length; i++) {
      removePlaceholderImg(placeholderImgs[i]);
      generateResponsiveIframe(iframes[i]);
    }
  }, 1500);
});

window.addEventListener("DOMContentLoaded", function() {
  // add margin to sections
  addSectionMargin();
});
