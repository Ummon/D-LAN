// All D-LAN is contained in this object.
var dlan = {};
   
$(function() {
   $(".gallery a").colorbox();   

   var canvas = $("#canvas-snow")[0];
   canvas.height = 85; 
   var setCanvasSize = function() { canvas.width = window.innerWidth; };
   $(window).resize(setCanvasSize);   
   setCanvasSize();

   var snow = new Snow(canvas);
   snow.start();
});
