$( document ).ready(function() {
  $( ".navbar .container-fluid" ).append( '<img src="osp_logo.png" height = 50, width = 295 align="right">' );
});

// This recieves messages of type "testmessage" from the server.
// See http://shiny.rstudio.com/gallery/server-to-client-custom-messages.html
// for details
Shiny.addCustomMessageHandler("testmessage",
  function(message) {
    alert(JSON.stringify(message));
  }
);