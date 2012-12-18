var Snow = function(canvas) {   
   // Parameters.
   this.flakePerSecond = 20;
   this.maxFlakes = 5000;
   this.fps = 25;
   //////////

   this.canvas = canvas; 
   this.ct = this.canvas.getContext("2d");
   this.dt = 1000 / this.fps; // [ms].
      
   this.flakes = new Array();
   this.timeSinceLastFlakesAdd = 0;
   
   var buildColor = function(r, g, b, a) {
      return "rgba(" + r + "," + g + "," + b + "," + a + ")";
   }
   
   var self = this
   
   this.Flake = function(x, y) {
      this.snow = self;
      this.distance = 1 - Math.sin(Math.random() * Math.PI / 2); // 1 means the nearest and 0 the farest. We use 'sin' to have more farest flakes.
      this.radius = this.distance * 3 + 1;
      this.transparency = this.distance / 1.5 + 0.1;
      this.x = x;
      this.y = y - this.radius;
      this.angle = Math.random() * Math.PI / 4;
      this.verticalSpeed = this.distance * 8 + 2; // px/s.
      this.offset = Math.random() * 2 * Math.PI;
   }

   this.Flake.prototype.getX = function() {
      return this.x + Math.sin(this.y / (8 * this.distance + 2) + this.offset) * (8 * this.distance + 2);
   }

   this.Flake.prototype.getY = function() {
      return this.y;
   }

   this.Flake.prototype.update = function() {
      this.y += this.snow.dt * this.verticalSpeed / 1000
   }

   this.Flake.prototype.draw = function() {
      this.snow.ct.save();
      this.snow.ct.translate(this.getX(), this.getY());
      this.snow.ct.rotate(this.angle);

      this.snow.ct.lineWidth = 1;
      this.snow.ct.strokeStyle = buildColor(255, 255, 255, this.transparency);

      this.snow.ct.beginPath();

      this.snow.ct.moveTo(0, -this.radius);
      this.snow.ct.lineTo(0, this.radius);
      this.snow.ct.moveTo(-this.radius, 0);
      this.snow.ct.lineTo(this.radius, 0);

      this.snow.ct.moveTo(-this.radius / 2, -this.radius / 2);
      this.snow.ct.lineTo(this.radius / 2, this.radius / 2);
      this.snow.ct.moveTo(-this.radius / 2, this.radius / 2);
      this.snow.ct.lineTo(this.radius / 2, -this.radius / 2);

      this.snow.ct.stroke();
      this.snow.ct.restore();
   }
}

Snow.prototype.update = function() {
   // Remove hidden flakes. O(n^2)?
   for (var i = 0; i < this.flakes.length; i++)
      if (this.flakes[i].y - this.flakes[i].radius > this.canvas.height)
      {
         this.flakes.splice(i, 1);
         i--;
      }
      
   // Add some new flakes.
   this.timeSinceLastFlakesAdd += this.dt;
   var flakePeriodMs = 1000 / this.flakePerSecond;
   if (this.timeSinceLastFlakesAdd >= flakePeriodMs) {
      for (var i = 0; i < Math.floor(this.timeSinceLastFlakesAdd / flakePeriodMs) && this.flakes.length < this.maxFlakes; i++)
          this.flakes.push(new this.Flake(Math.random() * this.canvas.width, 0));
            
      this.timeSinceLastFlakesAdd = this.timeSinceLastFlakesAdd - flakePeriodMs;
   }

   for (var i = 0; i < this.flakes.length; i++)
      this.flakes[i].update();
}

Snow.prototype.draw = function() {
   this.ct.clearRect(0, 0, this.canvas.width, this.canvas.height);

   for (var i = 0; i < this.flakes.length; i++)
      this.flakes[i].draw(); 
}

Snow.prototype.start = function() {
   var self = this;
   this.setIntervalID = setInterval(function() {
      self.update();
      self.draw();
   }, this.dt);
}

Snow.prototype.stop = function() {
   clearInterval(this.setIntervalID);
}