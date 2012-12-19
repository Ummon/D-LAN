/**
  * Usage:
  *  a) Default parameters:
  *   var snow = new Snow(canvas);
  *   snow.start();
  *
  *  b) Predefined parameters:
  *   var snow = new Snow(canvas, SnowNS.bigFlakes);
  *   snow.start();
  *
  *  c) Change parameters:
  *   var snow = new Snow(canvas);
  *   snow.p.flakePerSecond = 25;
  *   snow.p.maxFlakes = 1000;
  *   snow.start();
  */
  
var SnowNS = {   
   littleFlakes : {
      flakePerSecond : 15,
      maxFlakes : 500,
      flakeSpeedFactor : 1,
      flakeSizeFactor : 1,
      
      threshold : 0.8, // threshold for the default transparency factor function.   
      // This function may be replaced, for instance by (function(y, r) { return 1; }) if you don't want any flake fade.   
      // Value of y is 0 (top) to 1 (bottom).
      transparencyFactorFromPosition : function(y, r) {      
         if (y >= this.threshold)
         {
            var slope = 1 / (1 - r - this.threshold);
            var b = 1 + this.threshold * slope;
            return -y * slope + b;
         }
         else
            return 1;
      },
      
      flakeColor : { r: 255, g: 255, b: 255 }
   },

   clone : function(obj) {
      if(obj == null || typeof(obj) != 'object')
         return obj;
      var temp = obj.constructor();
      for(var key in obj)
         temp[key] = this.clone(obj[key]);
      return temp;
   }
}

SnowNS.bigFlakes = SnowNS.clone(SnowNS.littleFlakes)
SnowNS.bigFlakes.flakeSizeFactor = 4;
SnowNS.bigFlakes.threshold = 0.6;
  
var Snow = function(canvas, parameters) {   
   if (!parameters)
      parameters = SnowNS.littleFlakes;
   
   var self = this
   
   this.p = parameters;
   
   this.running = false;
   this.canvas = canvas; 
   this.ct = this.canvas.getContext("2d");
   
   this.flakes = new Array();
   this.timeSinceLastFlakesAdd = 0;
   
   var buildColor = function(r, g, b, a) {
      var normalize = function(v, max) {
         if (!max)
            max = 255;
         return v < 0 ? 0 : (v > max ? max : v);
      }
      return "rgba(" + normalize(r) + "," + normalize(g) + "," + normalize(b) + "," + normalize(a, 1) + ")";
   }
   
   this.Flake = function(x, y) {
      this.snow = self;
      this.distance = 1 - Math.sin(Math.random() * Math.PI / 2); // 1 means the nearest and 0 the farest. We use 'sin' to have more farest flakes.
      this.radius = (this.distance * 3 + 1) * this.snow.p.flakeSizeFactor;
      this.transparency = this.distance / 1.5 + 0.1;
      this.x = x;
      this.y = y - this.radius;
      this.angle = Math.random() * Math.PI / 4;
      this.verticalVelocity = this.distance * 8 + 2; // [px/s].
      this.angularVelocity = (Math.random() - 0.5) * Math.PI / 2; // [rad/s].
      this.offset = Math.random() * 2 * Math.PI;
      this.type = Math.floor(Math.random() * 2); // Two types of flake: 0 and 1.
   }

   this.Flake.prototype.getX = function() {
      return this.x + Math.sin(this.y / (8 * this.distance + 2) + this.offset) * (8 * this.distance + 2);
   }

   this.Flake.prototype.getY = function() {
      return this.y;
   }

   this.Flake.prototype.update = function(dt) {
      this.y += this.snow.p.flakeSpeedFactor * dt * this.verticalVelocity / 1000
      this.angle += (dt * this.angularVelocity / 1000) % (2 * Math.PI);
   }

   this.Flake.prototype.draw = function() {
      this.snow.ct.save();
      this.snow.ct.translate(this.getX(), this.getY());
      this.snow.ct.rotate(this.angle);

      this.snow.ct.strokeStyle = buildColor(
         this.snow.p.flakeColor.r,
         this.snow.p.flakeColor.g,
         this.snow.p.flakeColor.b,
         this.transparency * this.snow.p.transparencyFactorFromPosition(this.y / this.snow.canvas.height, this.radius / this.snow.canvas.height)
      );
      
      this.snow.ct.lineWidth = this.snow.p.flakeSizeFactor;

      this.snow.ct.beginPath();

      if (this.type == 0) {
         var x = this.radius * Math.cos(Math.PI / 6);
         var y = this.radius * Math.sin(Math.PI / 6);
         
         this.snow.ct.moveTo(0, -this.radius);
         this.snow.ct.lineTo(0, this.radius);
         
         this.snow.ct.moveTo(-x, -y);
         this.snow.ct.lineTo(x, y);
         
         this.snow.ct.moveTo(-x, y);
         this.snow.ct.lineTo(x, -y);
      } else {      
         this.snow.ct.moveTo(0, -this.radius);
         this.snow.ct.lineTo(0, this.radius);
         this.snow.ct.moveTo(-this.radius, 0);
         this.snow.ct.lineTo(this.radius, 0);

         this.snow.ct.moveTo(-this.radius / 2, -this.radius / 2);
         this.snow.ct.lineTo(this.radius / 2, this.radius / 2);
         this.snow.ct.moveTo(-this.radius / 2, this.radius / 2);
         this.snow.ct.lineTo(this.radius / 2, -this.radius / 2);
      }

      this.snow.ct.stroke();
      this.snow.ct.restore();
   }
}

Snow.prototype.update = function(dt) {
   // Remove hidden flakes. O(n^2)?
   for (var i = 0; i < this.flakes.length; i++)
      if (this.flakes[i].y - this.flakes[i].radius > this.canvas.height)
      {
         this.flakes.splice(i, 1);
         i--;
      }
      
   // Add some new flakes.
   this.timeSinceLastFlakesAdd += dt;
      
   var flakePeriodMs = 1000 / this.p.flakePerSecond;
   if (this.timeSinceLastFlakesAdd >= flakePeriodMs) {
      for (var i = 0; i < Math.floor(this.timeSinceLastFlakesAdd / flakePeriodMs) && this.flakes.length < this.p.maxFlakes; i++)
         this.flakes.push(new this.Flake(Math.random() * this.canvas.width, 0));
            
      this.timeSinceLastFlakesAdd = this.timeSinceLastFlakesAdd % flakePeriodMs;
   }
   
   for (var i = 0; i < this.flakes.length; i++)
      this.flakes[i].update(dt);
}

Snow.prototype.draw = function() {
   this.ct.clearRect(0, 0, this.canvas.width, this.canvas.height);
   
   for (var i = 0; i < this.flakes.length; i++)
      this.flakes[i].draw(); 
}

Snow.prototype.start = function() {   
   var self = this;
      
   var requestAnimFrame = 
      window.requestAnimationFrame || 
      window.webkitRequestAnimationFrame || 
      window.mozRequestAnimationFrame || 
      window.oRequestAnimationFrame || 
      window.msRequestAnimationFrame || 
      function(callback){ window.setTimeout(callback, 1000 / 60); };
      
   var getCurrentTime = function() { return window.performance.now ? window.performance.now() : Date.now(); }; // [ms].

   this.running = true;
   var lastUpdateTime = getCurrentTime();
   
   var tick = function() {   
      if (!self.running)
         return;
      
      requestAnimFrame(tick);
      
      var now = getCurrentTime();
      
      self.update(now - lastUpdateTime);
      lastUpdateTime = now;
      
      self.draw();
   };
   tick();
}

Snow.prototype.stop = function() {
   this.running = false;
}