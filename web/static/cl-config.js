$(function(){
  // choose either the full version
  $(".multiselect").multiselect();
  // or disable some features
  $(".multiselect").multiselect({sortable: true, searchable: true});
  $(".button").button();
  $("input:submit").button();
});