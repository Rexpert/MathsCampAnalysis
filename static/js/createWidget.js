const createResponsiveEmbed = url => {
  const div = document.createElement("div");
  const iframe = document.createElement("iframe");
  div.classList.add("embed-responsive", "embed-responsive-16by9");

  iframe.src = url;
  iframe.classList.add("embed-responsive-item");
  div.appendChild(iframe);
  return div;
};

window.addEventListener("load", async () => {
  const scoresChartDiv = document.querySelector("#scores-chart");
  const moneyAllocationChartDiv = document.querySelector(
    "#money-allocation-chart"
  );
  const scoresResponsiveDiv = createResponsiveEmbed("./static/scores.html");
  const moneyAllocationResponsiveDiv = createResponsiveEmbed(
    "./static/MoneyAllocation.html"
  );

  scoresChartDiv.appendChild(scoresResponsiveDiv);
  moneyAllocationChartDiv.appendChild(moneyAllocationResponsiveDiv);
});
