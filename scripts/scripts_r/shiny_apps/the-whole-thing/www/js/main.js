function logifySlider(el) {
  // regular number style
  $(el).data("ionRangeSlider").update({
    "prettify": function (num) {
      return (Math.pow(10, num).toLocaleString());
    }
  });
}

/* on load */
$(function() {
  $(".menu .item").tab({
    history: false
  });
});

$(document).on("shiny:sessioninitialized", function(e) {
  $(".js-range-slider").each(function() {
    logifySlider(this);
  });
});
