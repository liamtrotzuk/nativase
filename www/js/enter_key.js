shinyjs.setbutton = function(params) {
  var defaultParams = {
    id: null,
    button: null
  };
  params = shinyjs.getParams(params, defaultParams);
  var el = $("#" + params.id);
  var button = $("#" + params.button);
  el.keyup(function(event) {
    if (event.keyCode === 13) {
      button.click();
    }
  })
};'