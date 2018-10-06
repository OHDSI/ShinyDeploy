searchButton <- function(inputId, label, queryInput, ...) {
  script <- "
  var link=document.createElement('a');
  link.id = 'searchLink';
  link.href='?term='+encodeURI(document.getElementById('%id%').value);
  document.body.appendChild(link);
  document.getElementById('searchLink').click();
  return false;
  "
  script <- gsub("%id%", queryInput, script)
  tags$button(type = "button", onclick = script, label, ...)
}
