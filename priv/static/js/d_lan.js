// All D-LAN is contained in this object.
var dlan = {};

dlan.isMobile = function () {
   return (/webos|iphone|ipod|android|ie|blackberry|windows phone/).test(navigator.userAgent.toLowerCase());
}

$(function () {
   $(".gallery a").colorbox();

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
   canvas.height = 120;
   var setCanvasSize = function () { canvas.width = window.innerWidth; };
   $(window).resize(setCanvasSize);
   setCanvasSize();

   // It snows from the begining of december to the end of february.
   var currentMonth = (new Date()).getMonth();
   if (currentMonth >= 11 || currentMonth <= 0 && !dlan.isMobile()) {
      var snow = new Snow(canvas);

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
