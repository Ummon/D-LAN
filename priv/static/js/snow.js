/*jslint white: true, indent: 3 */

/**
  * Usage:
  *  a) Default parameters:
  *   (new Snow(canvas)).start();
  *
  *  b) Predefined parameters:
  *   (new Snow(canvas, SnowNS.bigFlakes)).start();
  *
  *  c) Changing some parameters, see the default parameter object 'SnowNS.littleFlakes' below:
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
      flakeAngularVelocityFactor : 1,
      flakeSizeFactor : 1,
      flakeSinWidthFactor : 1,
      flakeSinPeriodFactor : 1,
      
      blur : 0, // 0 = no blur, 1 = full blur
      
      threshold : 0.8, // threshold for the default transparency factor function.
      // This function may be replaced, for instance by (function(y, r) { return 1; }) if you don't want any flake fade.   
      // Value of y is 0 (top) to 1 (bottom).
      transparencyFactorFromPosition : function(y, r) {      
         if (y >= this.threshold) {
            var slope = 1 / (1 - r - this.threshold),
                b = 1 + this.threshold * slope;
            return -y * slope + b;
         }
         return 1;
      },
      
      flakeColor : { r: 255, g: 255, b: 255 },
      flakeColorFromPosition : undefined
   },

   clone : function(obj) {
      if(obj === null || typeof obj !== 'object')
         return obj;
      var temp = obj.constructor();
      for(var key in obj)
         temp[key] = this.clone(obj[key]);
      return temp;
   }
}

SnowNS.bigFlakes = SnowNS.clone(SnowNS.littleFlakes);
SnowNS.bigFlakes.flakeSizeFactor = 4;
SnowNS.bigFlakes.threshold = 0.6;
  
var Snow = function(canvas, parameters) {
   parameters = typeof parameters !== 'undefined' ? parameters : SnowNS.littleFlakes;
   
   var self = this
   
   this.p = parameters;
   
   this.running = false;
   this.canvas = canvas; 
   this.ct = this.canvas.getContext("2d");
   
   this.flakes = new Array();
   this.timeSinceLastFlakesAdd = 0;
   
   var buildColor = function(r, g, b, a) {
      var normalize = function(v, max) {
         max = typeof max !== 'undefined' ? max : 255;
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
      this.angle = Math.random() * 2 * Math.PI;
      this.verticalVelocity = this.distance * 8 + 2; // [px/s].
      this.angularVelocity = this.snow.p.flakeAngularVelocityFactor * (Math.random() - 0.5) * Math.PI / 2; // [rad/s].
      this.offset = Math.random() * 2 * Math.PI;
      this.type = Math.floor(Math.random() * 2); // Two types of flake: 0 and 1.
   }

   this.Flake.prototype.getX = function() {
      return this.x + Math.sin((this.y / (8 * this.distance + 2) + this.offset) / this.snow.p.flakeSinPeriodFactor) * this.snow.p.flakeSinWidthFactor * (8 * this.distance + 2);
   }

   this.Flake.prototype.getY = function() {
      return this.y;
   }

   this.Flake.prototype.update = function(dt) {
      this.y += this.snow.p.flakeSpeedFactor * dt * this.verticalVelocity / 1000
      this.angle += (dt * this.angularVelocity / 1000) % (2 * Math.PI);
   }

   this.Flake.prototype.draw = function() {    
      var x = this.getX(),
          y = this.getY();
      this.snow.ct.save();
      this.snow.ct.translate(x, y);
      this.snow.ct.rotate(this.angle);
      
      var normalizedY = y / this.snow.canvas.height;

      var color = this.snow.p.flakeColorFromPosition ? this.snow.p.flakeColorFromPosition(x / this.snow.canvas.width, normalizedY) : this.snow.p.flakeColor;
      
      this.snow.ct.strokeStyle = buildColor(color.r, color.g, color.b,
         this.transparency * this.snow.p.transparencyFactorFromPosition(normalizedY, this.radius / this.snow.canvas.height)
      );
      
      this.snow.ct.lineWidth = this.snow.p.flakeSizeFactor;

      this.snow.ct.beginPath();

      if (this.type === 0) {
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
   // Apply the gaussian blur if 'this.p.blur' isn't 0.
   if (this.p.blur !== 0)
      this.drawBlur();
   else   
      this.ct.clearRect(0, 0, this.canvas.width, this.canvas.height);
   
   for (var i = 0; i < this.flakes.length; i++)
      this.flakes[i].draw(); 
}

/**
  * Apply a gaussian blur depending of 'this.p.blur' (rho) to the whole canvas.
  * See http://en.wikipedia.org/wiki/Gaussian_blur for more information.
  */
Snow.prototype.drawBlur = function() {
   var w = this.canvas.width,
       h = this.canvas.height,
       transparentFading = 0.93;
   
   var matrixSize = 0;
   
   // Create the gaussian matrix if 'this.p.rho' hasn't been defined or if it has been modified.
   if (!this.rho || this.rho != this.p.blur) {
      this.rho = this.p.blur;
      
      var rowSquare = Math.pow(this.rho, 2);
      var a = 1 / (2 * Math.PI * rowSquare);
      
      matrixSize = Math.floor(this.rho * 6);
      if (matrixSize % 2 == 0)
         matrixSize += 1;
      if (matrixSize < 3)
         matrixSize = 3;
      
      this.gaussianBlurMatrix = new Array(matrixSize);
      this.gaussianBlurMatrixSum = 0;
      
      for (var i = 0; i < matrixSize; i++) {
         var y = i - Math.floor(matrixSize / 2);
         for (var j = 0; j < matrixSize; j++) {
            var x = j - Math.floor(matrixSize / 2);
            this.gaussianBlurMatrix[i * matrixSize + j] = a * Math.pow(Math.E, (-Math.pow(x,2) - Math.pow(y,2)) / (2 * rowSquare));
            this.gaussianBlurMatrixSum += this.gaussianBlurMatrix[i * matrixSize + j];
         }
      }
   } else {
      matrixSize = Math.floor(this.rho * 6);
   }
   
   var currentImage = this.ct.getImageData(0, 0, w, h),
       bluredImage = this.ct.createImageData(w, h),
       currentImageData = currentImage.data,
       bluredImageData = bluredImage.data,
       s2 = Math.floor(matrixSize / 2),
       pixelValue = new Array(4);
   
   for (var y = 0; y < h; y++) { 
      for (var x = 0; x < w; x++) {
         var pixelPosition = (y * w + x) * 4;
         pixelValue[0] = 0.0;
         pixelValue[1] = 0.0;
         pixelValue[2] = 0.0;
         pixelValue[3] = 0.0;
         
         for (var i = 0; i < matrixSize; i++) {
            for (var j = 0; j < matrixSize; j++) {     
               var x2 = x + (j - s2),
                   y2 = y + (i - s2),
                   factor = this.gaussianBlurMatrix[i * matrixSize + j];
               
               if (x2 < 0 || y2 < 0 || x2 > w || y2 > h) {               
                  pixelValue[0] += factor * currentImageData[pixelPosition];
                  pixelValue[1] += factor * currentImageData[pixelPosition + 1];
                  pixelValue[2] += factor * currentImageData[pixelPosition + 2];
               } else {
                  var pixelPosition2 = (y2 * w + x2) * 4;
                  pixelValue[0] += factor * currentImageData[pixelPosition2];
                  pixelValue[1] += factor * currentImageData[pixelPosition2 + 1];
                  pixelValue[2] += factor * currentImageData[pixelPosition2 + 2];
                  pixelValue[3] += factor * currentImageData[pixelPosition2 + 3];                  
               }               
            }
         }
         
         bluredImageData[pixelPosition] = pixelValue[0] / this.gaussianBlurMatrixSum;
         bluredImageData[pixelPosition + 1] = pixelValue[1] / this.gaussianBlurMatrixSum;
         bluredImageData[pixelPosition + 2] = pixelValue[2] / this.gaussianBlurMatrixSum;
         bluredImageData[pixelPosition + 3] = transparentFading * pixelValue[3] / this.gaussianBlurMatrixSum;
      }
   }
   
   this.ct.putImageData(bluredImage, 0, 0);
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