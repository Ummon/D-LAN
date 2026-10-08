// All D-LAN is contained in this object.
var dlan = {};

dlan.isMobile = function () {
   // Chromium based browsers only.
   if (navigator.userAgentData)
      return navigator.userAgentData.mobile;
   // The primary input is a finger (also true for iPad which pretends to be a Mac in its user agent).
   return window.matchMedia("(pointer: coarse)").matches;
}

$(function () {
   $(".gallery a").colorbox({
      maxWidth: "100%",
      maxHeight: "100%",
      scalePhotos: true
   });

   // Reload the current page with the chosen language. The server then sets a
   // one year 'lang' cookie, so the choice persists on the next pages.
   $("#langs").on("change", function () {
      var url = new URL(window.location.href);
      url.searchParams.set("lang", this.value);
      window.location.href = url.href;
   });

   $("#file").on("change", function () {
      var url = new URL(window.location.href);
      url.searchParams.set("file", this.value);
      window.location.href = url.href;
   });


   var canvas = $("#canvas-menu")[0];
   var snow;
   // The canvas is stretched over the menu by the CSS, its drawing buffer has to match its displayed size.
   var setCanvasSize = function () {
      if (canvas.width === canvas.clientWidth && canvas.height === canvas.clientHeight)
         return;

      canvas.width = canvas.clientWidth;
      canvas.height = canvas.clientHeight;

      // Changing the size of a canvas clears it: the flakes are redrawn right away to avoid a blank frame.
      if (snow)
         snow.draw();
   };
   if (window.ResizeObserver)
      new ResizeObserver(setCanvasSize).observe(canvas);
   else
      $(window).resize(setCanvasSize);
   setCanvasSize();

   // It snows from the begining of december to the end of january.
   var currentMonth = (new Date()).getMonth();
   if ((currentMonth >= 11 || currentMonth <= 0) && !dlan.isMobile()) {
      snow = new Snow(canvas);

      /*
      snow.p.flakeSpeedFactor = 4;
      snow.p.flakeSizeFactor = 5;
      snow.p.blur = 0.5;
      snow.p.flakeAngularVelocityFactor = 3;
      snow.p.transparencyFactorFromPosition = function(y, r) { return 1; }
      snow.p.flakeColorFromPosition = function(x, y) {
         return { r: Math.floor(x * 255) % 255, g: 210, b: 255 - Math.floor(y * 255 * 4) % 255 };
      }
      */

      snow.start();
   }
});
