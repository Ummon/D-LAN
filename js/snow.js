var Snow = function(canvas) {
   ///// Public parameters, can be changed at any time.
   this.flakePerSecond = 15;
   this.maxFlakes = 500;
   this.t = 0.8; // threshold for the transparency factor function.
   
   // This function may be replaced, for instance by (function(y, r) { return 1; }) if you don't want any flake fade.   
   // Value of y is 0 (top) to 1 (bottom).
   this.transparencyFactorFromPosition = function(y, r) {      
      if (y >= this.t)
      {
         var slope = 1 / (1 - r - this.t);
         var b = 1 + this.t * slope;
         return -y * slope + b;
      }
      else
         return 1;
   }
   
   this.flakeColor = { r: 255, g: 255, b: 255 }
   /////
   
   var self = this
   
   this.running = false;
   this.canvas = canvas; 
   this.ct = this.canvas.getContext("2d");   
   this.ct.lineWidth = 1;
   
   this.flakes = new Array();
   this.timeSinceLastFlakesAdd = 0;
   
   var buildColor = function(r, g, b, a) {
      return "rgba(" + r + "," + g + "," + b + "," + a + ")";
   }
   
   this.Flake = function(x, y) {
      this.snow = self;
      this.distance = 1 - Math.sin(Math.random() * Math.PI / 2); // 1 means the nearest and 0 the farest. We use 'sin' to have more farest flakes.
      this.radius = this.distance * 3 + 1;
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
      this.y += dt * this.verticalVelocity / 1000
      this.angle += (dt * this.angularVelocity / 1000) % (2 * Math.PI);
   }

   this.Flake.prototype.draw = function() {
      this.snow.ct.save();
      this.snow.ct.translate(this.getX(), this.getY());
      this.snow.ct.rotate(this.angle);

      this.snow.ct.strokeStyle = buildColor(
         this.snow.flakeColor.r,
         this.snow.flakeColor.g,
         this.snow.flakeColor.b,
         this.transparency * this.snow.transparencyFactorFromPosition(this.y / this.snow.canvas.height, this.radius / this.snow.canvas.height)
      );

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
   var flakePeriodMs = 1000 / this.flakePerSecond;
   if (this.timeSinceLastFlakesAdd >= flakePeriodMs) {
      for (var i = 0; i < Math.floor(this.timeSinceLastFlakesAdd / flakePeriodMs) && this.flakes.length < this.maxFlakes; i++)
          this.flakes.push(new this.Flake(Math.random() * this.canvas.width, 0));
            
      this.timeSinceLastFlakesAdd = this.timeSinceLastFlakesAdd - flakePeriodMs;
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