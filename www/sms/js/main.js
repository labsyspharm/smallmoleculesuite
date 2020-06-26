function logifySlider(el, base=2, precision=2) {
  $(el).data("ionRangeSlider").update({
    "prettify": function (num) {
      var exp = Math.pow(base, num);
      if (precision === null)
        return exp.toString();
      return exp.toPrecision(precision);
    }
  });
}

$(function() {
  $(document).on("shiny:sessioninitialized", function(e) {

    $(".logify-slider .js-range-slider").each(function() {
      logifySlider(this, base=2, precision=null);
    });

    $(".logify-slider-10 .js-range-slider").each(function() {
      logifySlider(this, base=10, precision=null);
    });
  });
});
