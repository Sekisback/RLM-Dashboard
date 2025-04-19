Shiny.addCustomMessageHandler("updateNavbarDate", function(message) {
  document.getElementById("navbar_date").textContent = message;
});