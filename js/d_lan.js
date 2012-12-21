// All D-LAN is contained in this object.
var dlan = {};
   
$(function() {
   $(".gallery a").colorbox();   

   var canvas = $("#canvas-snow")[0];
   canvas.height = 125; 
   var setCanvasSize = function() { canvas.width = window.innerWidth; };
   $(window).resize(setCanvasSize);   
   setCanvasSize();

   (new Snow(canvas)).start();
});
