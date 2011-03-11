$(document).ready(function() {
  $('#configuration-select').change(function () {
        javascript:window.location='/?conf=' + $('#configuration-select').value());
  });   			     
});