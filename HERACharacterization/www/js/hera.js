setInputValue = function(params) {
  Shiny.setInputValue("jscookie", params);  
}

setCookie = function(params) {
  Cookies.set("HERACharacterizationTermsOfUse", escape(params), { expires: 30 });
}

Shiny.addCustomMessageHandler("setCookie", function(params) {
  setCookie(params);
  setInputValue(params);
});

Shiny.addCustomMessageHandler("rmCookie", function(params) {
  Cookies.remove("HERACharacterizationTermsOfUse");
  setInputValue(params);
});

Shiny.addCustomMessageHandler("alert", function(params) {
  alert(params)
});

$(document).on('shiny:sessioninitialized', function(event) {
  var cookie = Cookies.get("HERACharacterizationTermsOfUse");
  if (typeof cookie !== "undefined") {
    setInputValue(cookie);
  } else {
    setCookie("");
    setInputValue("");
  }
});
