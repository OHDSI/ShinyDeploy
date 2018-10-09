searchButton <- function(inputId, label, structured) {
  if (structured) {
    script <- "
    var link = document.createElement('a');
    link.id = 'searchLink';
    link.href = '?structured=true'
    // link.href = link.href + '&indication=' + encodeURI(document.getElementById('indication').value);
    link.href = link.href + '&exposureGroup=' + encodeURI(document.getElementById('exposureGroup').value);
    link.href = link.href + '&target=' + encodeURI(document.getElementById('target').value);
    link.href = link.href + '&comparator=' + encodeURI(document.getElementById('comparator').value);
    link.href = link.href + '&outcome=' + encodeURI(document.getElementById('outcome').value);
    link.href = link.href + '&database=' + encodeURI(document.getElementById('database').value);
    document.body.appendChild(link);
    document.getElementById('searchLink').click();
    return false;
    "
  } else {
    script <- "
    var link = document.createElement('a');
    link.id = 'searchLink';
    link.href = '?term=' + encodeURI(document.getElementById('query').value);
    document.body.appendChild(link);
    document.getElementById('searchLink').click();
    return false;
    "
  }
  tags$button(type = "button", onclick = script, label)
}
