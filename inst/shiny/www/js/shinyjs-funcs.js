shinyjs.scrollLogger = function(params) {
  var $logger = $('#wallaceLog');
  $logger.scrollTop($logger[0].scrollHeight);
}

shinyjs.removeModule = function(params) {
  var defaultParams = {
    component : null,
    module : null
  }
  params = shinyjs.getParams(params, defaultParams);

  params.component = params.component + "Sel";
  var $radio = $("input[type='radio'][name='" + params.component + "']" +
    "[value='" + params.module + "']");
  var $parent = $radio.closest(".shiny-input-radiogroup");
  var checked = $radio.prop('checked');
  $radio.closest(".radio").remove();
  if (checked) {
    $parent.find(".radio input[type='radio']").first().prop('checked', true);
    $parent.trigger("change");
  }
}
