
Shiny.addCustomMessageHandler('redirect-to-url', function(message) {
    window.location.href = message;
});
