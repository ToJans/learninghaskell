"use strict";

function ImageRange(imgOrRange) {
  if (imgOrRange && imgOrRange.right && imgOrRange.bottom) {
    this.left = imgOrRange.left;
    this.right = imgOrRange.right;
    this.top = imgOrRange.top;
    this.bottom = imgOrRange.bottom;
    this.img = imgOrRange.img;
  } else if (imgOrRange && imgOrRange.width && imgOrRange.height) {
    this.left = 0;
    this.right = imgOrRange.width;
    this.top = 0;
    this.bottom = imgOrRange.height;
    this.img = imgOrRange;
  } else {
    this.left = 0;
    this.right = 0;
    this.top = 0;
    this.bottom = 0;
    this.img = {width:0,height:0,data:new Uint8ClampedArray(4)};
  }
  this.width = this.right - this.left;
  this.height = this.bottom -  this.top;
  this.area = this.width * this.height;
  this.center = {x:this.left+this.width/2,y:this.top+this.height/2};
}

// folds over an image
// takes an accFunc(acc,index,x,y,{r,g,b}) that returns the acc
ImageRange.prototype.foldImgTopDownLeftRight = function(accFunc,acc,stepDown,stepRight) {
  var x,y,xi,yi,d=this.img.data,rgb;
  stepDown = stepDown || 1;
  stepRight = stepRight || 1;
  var sStepDown = stepDown * 4 * this.img.width;
  var sStepRight = stepRight * 4;
  yi = this.top*4*this.img.width + this.left*4;
  for (y = this.top;y<=this.bottom;y+=stepDown,yi+=sStepDown) {
    xi = yi;
    for (var x = this.left;x<=this.right;x+=stepRight,xi+=sStepRight) {
      rgb = {r:d[xi],g:d[xi+1],b:d[xi+2]};
      acc = accFunc.call(this,acc,xi,x,y,rgb);
    }
  }
  return acc;
};

// modify an image's colors
// takes a modFunc(index,x,y,{r,g,b}) that returns an {r,g,b};
ImageRange.prototype.mapColor = function(modfunc,stepDown,stepRight) {
  var x,y,xi,yi,rgb,d,nd = new Uint8ClampedArray(this.img.data);
  d= this.img.data;
  stepDown = stepDown || 1;
  stepRight = stepRight || 1;
  var sStepDown = stepDown * 4 * this.img.width;
  var sStepRight = stepRight * 4;
  yi = this.top*4*this.img.width + this.left*4;
  for (y = this.top;y<=this.bottom;y+=stepDown,yi+=sStepDown) {
    xi = yi;
    for (var x = this.left;x<=this.right;x+=stepRight,xi+=sStepRight) {
      rgb = {r:d[xi],g:d[xi+1],b:d[xi+2]};
      rgb = modfunc.call(this,xi,x,y,rgb);
      nd[xi] = rgb.r;
      nd[xi+1] = rgb.g;
      nd[xi+2] = rgb.b;
    }
  }
  return new ImageRange({
    left: this.left,
    right: this.right,
    top: this.top,
    bottom: this.bottom,
    img: {
      data: nd,
      width: this.img.width,
      height: this.img.height
    }
  });
};

ImageRange.prototype.rangeFromColor=function(imgRange,r,g,b) {
  var initial = { left: bounds.right,
                  right: bounds.left,
                  top: bounds.bottom,
                  bottom: bounds.top};
  var d = img.data;
  var scanAcc = function(acc,i,x,y,rgb) {
    if (rgb.r!=r || rgb.g != g || rgb.b!= b) return acc;
    return { left:   x < acc.left   ? x : acc.left
           , right:  x > acc.right  ? x : acc.right
           , top:    y < acc.top    ? y : acc.top
           , bottom: y > acc.bottom ? y : acc.bottom
           };
  };
  var acc = this.foldImgTopDownLeftRight(scanAcc,initial);
  acc.img = this.img;
  return new ImageRange(acc);
};

ImageRange.prototype.grayScale = function() {
  var v;
  return this.mapColor(function(i,x,y,rgb) {
    v = 0.2126*rgb.r + 0.7152*rgb.g + 0.0722*rgb.b;
    return {r:v,g:v,b:v}
  });
};

ImageRange.prototype.shrinkBound = function(size) {
  var nb = new ImageRange(this);
  nb.left += size;
  nb.right -= size;
  nb.top += size;
  nb.bottom -= size;
  return new ImageRange(nb);
};

// folds over a square range of size n*2+1, only gets acc,x,y,rgb as inputs
ImageRange.prototype.foldSquare = function(foldAcc,acc,x,y,size) {
  var xs,ys,yi,xi,rgb,downStep = this.img.width * 4,rightStep = 4;
  var d = this.img.data;
  yi = (y - size) * downStep + (x - size) * rightStep;
  for (ys = y-size;ys<y+size;ys++,yi+=downStep) {
    for (xs = x-size,xi = yi;xs<x+ size;xs++,xi+=rightStep) {
      rgb = {r:d[xi],g:d[xi+1],b:d[xi+2]};
      acc = foldAcc(acc,xs,ys,rgb)
    }
  }
  return acc;
};

ImageRange.accSumRGB = function(acc,x,y,rgb){
  return {
    r: acc.r + rgb.r,
    g: acc.g + rgb.g,
    b: acc.b + rgb.b
  };
};

ImageRange.prototype.sumSquare = function(x,y,size) {
  return this.foldSquare(ImageRange.accSumRGB,{r:0,g:0,b:0},x,y,size)
};

ImageRange.prototype.blur = function(size) {
  size = size || 1;
  var total = (size*2+1) * (size*2+1);
  return this.shrinkBound(size).mapColor(function(i,x,y,rgb) {
    rgb = this.sumSquare(x,y,size);
    return {
      r: rgb.r/total,
      g: rgb.g/total,
      b: rgb.b/total
    }
  });
  return nr;
};

// assumes we work with Red component only, so convert to grayscale first
ImageRange.prototype.adaptiveTreshold = function(size,tresholdValue) {
  var total = (size*2+1) * (size*2+1) - 1;
  return this.shrinkBound(size).mapColor(function(i,x,y,rgb) {
    var sums = this.sumSquare(x,y,size).r - rgb.r;
    if ( sums == 0 || rgb.r * total / sums > tresholdValue) {
      return {r:255,g:255,b:255};
    } else {
      return {r:0,g:0,b:0};
    }
  });
  return nr;
};

// floodfill at position, returns the bounded range
ImageRange.prototype.floodFill = function(x,y,rgbCondition,rgb) {
  if (rgbCondition(rgb)) {
    throw "the fill color should not match the rgbCondition";
  }
  var bnds = {
    left: this.right,
    right:this.left,
    top:this.bottom,
    bottom: this.top,
  };
  var d = this.img.data,rgbv;
  var xs = [{x:x,y:y,i:x*4+y*this.img.width*4}],v;
  while (xs.length>0) {
    v = xs.pop();
    if (v.x<this.left || v.x> this.right || v.y < this.top || v.y > this.bottom)
      continue;
    rgbv = {r:d[v.i],g:d[v.i+1],b:d[v.i+2]};
    if (!rgbCondition(rgbv)) continue;
    this.img.data[v.i  ] = rgb.r;
    this.img.data[v.i+1] = rgb.g;
    this.img.data[v.i+2] = rgb.b;
    bnds = {
      left:   v.x < bnds.left   ? v.x : bnds.left,
      right:  v.x > bnds.right  ? v.x : bnds.right,
      top:    v.y < bnds.top    ? v.y : bnds.top,
      bottom: v.y > bnds.bottom ? v.y : bnds.bottom,
    }
    xs.push({x:v.x-1,y:v.y,i:v.i-4});
    xs.push({x:v.x+1,y:v.y,i:v.i+4});
    xs.push({x:v.x,y:v.y-1,i:v.i-4*this.img.width});
    xs.push({x:v.x,y:v.y+1,i:v.i+4*this.img.width});
  }
  bnds.img = this.img;
  return new ImageRange(bnds);
}

ImageRange.prototype.replaceColor=function(fromRGB,toRGB) {
  return this.mapColor(function(i,x,y,rgb) {
    if (fromRGB.r == rgb.r && fromRGB.g == rgb.g && fromRGB.b == rgb.b) {
      return toRGB;
    } else {
      return rgb;
    }
  });
}

ImageRange.prototype.thickenColor = function(fromRGB,size) {
  size = size || 1;
  var d= this.img.data;
  var nd= new Uint8ClampedArray(d);
  var w = this.img.width
  for (var i=4*(w*size+size);i<d.length-4*(w*size+size);i+=4) {
    if (d[i]=fromRGB.r && d[i+1]==fromRGB.g && d[i+2] ==fromRGB.b) {
      var yi = i - (size*w + size) * 4;
      for (var j=-size;j<=size;j++,yi+=4*w) {
        var xi = yi;
        for (var k=-size;k<=size;k++,xi+=4) {
          nd[xi] = fromRGB.r;
          nd[xi+1] = fromRGB.g;
          nd[xi+2] = fromRGB.b;
        }
      }
    }
  }
  return new ImageRange({
    left: this.left - size,
    right: this.right + size,
    top: this.top - size,
    bottom: this.bottom + size,
    img: {
      width: this.img.width,
      height: this.img.height,
      data: nd
    }
  });
}

ImageRange.blobColors = {
  current: {r:255,g:  1,b:  1},
  largest: {r:  1,g:255,b:  1},
  other:   {r:  1,g:  1,b:  1}
};

// find the largest blob that satisfies the predicate
// modifies the image
// if you define a callback, it will call it for every blob filled
ImageRange.prototype.largestBlob = function(minSize, blobPredicate, callback) {
  var stepRight= minSize / 2;

  var stepDown = 1;
  var maxy = this.img.height;
  var self=this;

  var largestBlob = new ImageRange(this);
  largestBlob.area = 0;

  var isMatch = function(rgb){return rgb.r == 0;};

  var accFunc = function(acc,index,x,y,rgb) {
    if (!isMatch(rgb)) return acc;
    var ff = acc.floodFill(x,y,isMatch,ImageRange.blobColors.current);
    if (ff.area > largestBlob.area && blobPredicate(ff)) {
      ff = ff.replaceColor(ImageRange.blobColors.current,ImageRange.blobColors.largest);
      largestBlob = ff;
    } else {
      ff = ff.replaceColor(ImageRange.blobColors.current,ImageRange.blobColors.other);
    }
    acc.img = ff.img;
    if (callback) callback(acc);
    return acc;
  }

  this.foldImgTopDownLeftRight(accFunc,largestBlob,stepDown,stepRight);

  return largestBlob;
}

// find lines in image; returs lines array
// only works on pixels colored in matchColor
// requires the lines to cover at least minPixelCount
ImageRange.prototype.houghLines = function(matchColor,minPixelCount,minAngleDiff,minDistanceDiff,callback) {
  minAngleDiff = minAngleDiff || 5;
  minDistanceDiff = minDistanceDiff || 5;
  var maxAngles = 180;
  var sines = [];
  var da = Math.PI/maxAngles;
  var maxSize = Math.sqrt( this.width * this.width + this.height * this.height);
  // cache sines & cosines for performance reasons
  for (var i=0,a=0;i<maxAngles;i++,a+=da) {
    sines[i] = Math.sin(a);
  }
  var sin = function(a) {
    return sines[a];
  }
  var cos = function(a) {
    return sin((a-maxAngles/2) % maxAngles);
  };

  // transform will count the occurances of pixels mapped
  // to normal space with angle theta and distance rho

  // initialize the normal space to zero
  var transformed = new Uint32Array(maxAngles*maxSize);

  // convert the image into normal space for matching pixels
  this.foldImgTopDownLeftRight(function(acc,index,x,y,rgb) {
    if (rgb.r != matchColor.r || rgb.g != matchColor.g || rgb.b != matchColor.b)
      return;
    if (callback) {
      callback("tracing image for all angles at "+x+", "+y);
    }

    for (var theta=0;theta<maxAngles;theta++) {
      var rho = x * cos(theta) + y * sin(theta);
      transformed[theta*maxSize+Math.trunc(rho)]++;
    }
  });

  var img = this.img;

  var self = this;
  var isMatch = function(i) {
    return img.data[i] == matchColor.r &&
           img.data[i+1] == matchColor.g &&
           img.data[i+2] == matchColor.b;
  }

  // convert normal space into a series of lines, and it's count
  // of matching pixels
  // i.e. {cnt:20,lines:[{x:10,y:1,x2:20,y2:1},{x:30,y:1,x2:40,y:1}]}
  var rhoThetaToLines = function(rho,theta,minLineSize) {
    var a = cos(theta), b = sin(theta);
    var x = a*rho, y = b*rho;
    x +=b*maxSize, y-= a*maxSize;
    var insideLen = 0,current;
    var result = {cnt:0,lines:[],rho:rho,theta:theta};

    for (var i = -maxSize;i<=maxSize;i++,x-=b,y+=a) {
      if (x>self.left && x<self.right && y>self.top && y < self.bottom) {
        var sy= Math.round(y);
        var sx= Math.round(x);
        var si = sy*img.width+sx;
        si*=4;
        if (insideLen > 0 ) {
          if (isMatch(si)) {
            insideLen++;
            current.x2 = sx;current.y2 = sy;
          } else {
            if (insideLen>minLineSize) {
              result.cnt += insideLen;
              result.lines.push(current);
            }
            insideLen = 0;
          }
        } else if (insideLen == 0 && isMatch(si)) {
          insideLen = 1;
          current = {x:sx,y:sy}
        }
      }
    }
    return result;
  }

  var selected = [];

  // check every pixel in normal space, find the ones
  // with the largest amounts of pixels on a line,
  // locality for theta = minAngleDiff
  // locality for rho = minDistanceDiff
  for (var theta=0;theta<maxAngles;theta++) {
    if (callback) {
      callback("tracing angle "+(theta+1) + " from "+maxAngles);
    }
    for (var rho = 0; rho < maxSize;rho++ ) {
      var cnt = transformed[theta*maxSize+rho];
      if (cnt<minPixelCount) continue;
      // get actual matching pixel counts and lines that match
      var res = rhoThetaToLines(rho,theta,minDistanceDiff);
      var cnt = res.cnt;
      if (cnt == 0) {
        continue;
      }
      var shouldPush = true;
      // only add if locally largest
      for (var i=0;i<selected.length;i++) {
        var x = selected[i];
        var dRho = Math.abs(x.rho - rho);
        var dTheta =  Math.abs(x.theta - theta);
        if ( dRho > minAngleDiff || dTheta > minDistanceDiff) continue;
        // a close record found, so we either replace it or do nothing
        shouldPush = false;
        // if the current count of pixels is larger than the previous,
        // replace the item in the list
        if ( cnt > x.cnt) {
          selected[i] = res;
          break;
        }
      }
      if (shouldPush) selected.push(res);
    }
  };

  var min = function() {
    return new Array(arguments).reduce(function(acc,x){return x<acc?x:acc;});
  }
  var max = function() {
    return new Array(arguments).reduce(function(acc,x){return x>acc?x:acc;});
  }

  // we are only interested in the endpoints of the lines
  var lines = selected.map(function(r) {
    return r.lines.reduce(function(acc,l){
      return {
        x: min(acc.x,l.x,l.x2),
        y: min(acc.y,l.y,l.y2),
        x2: max(acc.x,l.x,l.x2),
        y2: max(acc.y,l.y,l.y2)
      };
    });
  });

  return lines;

}

var evts = {};

evts.imageUpdated = function (imgRange) {
  self.postMessage({msgType:'imageUpdated',data:imgRange.img});
}

evts.lines = function(lines){
  self.postMessage({msgType:'lines',data:lines});
}

evts.error = function(msg) {
  self.postMessage({msgType:'error',data:msg});
}

evts.info = function(msg) {
  self.postMessage({msgType:'info',data:msg});
}

var cmds = {};

function isValidSudokuBlob (imageRange) {
  var dx = imageRange.img.width - imageRange.img.height;
  var dy = imageRange.img.height - imageRange.img.width;
  dx = dx > 0 ? dx/2 : 0;
  dy = dy > 0 ? dy/2 : 0;
  return (imageRange.width+dx*2) > imageRange.img.width /3 &&
         (imageRange.height+dy*2) > imageRange.img.height / 3 &&
         imageRange.center.x + dx > imageRange.img.width/3 &&
         imageRange.center.y + dy > imageRange.img.height/3 &&
         imageRange.center.x - dx < imageRange.img.width*2/3 &&
         imageRange.center.y - dy < imageRange.img.height*2/3 &&
         imageRange.left > 10 &&
         imageRange.right < imageRange.img.width - 10 &&
         imageRange.top > 10 &&
         imageRange.bottom < imageRange.img.height - 10;
}

cmds.processImage = function (e) {
  var img = e.data;
  var result = new ImageRange(img)
  evts.info('processing image');
  evts.imageUpdated(result);
  evts.info('converting to grayscale');
  result = result.grayScale();
  evts.imageUpdated(result);
  evts.info('converting to black and white');
  result = result.adaptiveTreshold(10,0.9);
  evts.imageUpdated(result);
  evts.info('locating sudoku');
  var minsize = (img.width < img.height ? img.width:img.height)/2;
  result = result.largestBlob(minsize, isValidSudokuBlob,evts.imageUpdated);
  evts.imageUpdated(result);
  if (result.area == 0) {
    evts.info('unable to locate sudoku');
    return;
  }
  evts.info('thickening sudoku edges');
  result = result.thickenColor(ImageRange.blobColors.largest,6);
  evts.imageUpdated(result);
  evts.info('finding lines');
  // result = result.findRectangleCorners();
  var minLength = result.width<result.height?result.width:result.height;
  var minSegLength = minLength*3/4;
  var minPixelCount = minSegLength; // at least 90% of pixels need to be on the line
  var minAngleDiff = 45; // minimum angles between hough lines in degrees
  var minDistanceDiff = minSegLength/2; // min distance between hough lines in pixels
  var matchColor = ImageRange.blobColors.largest; // the color we will match
  var cb = function(s) {
    evts.info('finding lines: '+s);
  }
  var lines = result.houghLines(matchColor,minPixelCount,minAngleDiff,minDistanceDiff,cb);
  evts.lines(lines);
  evts.imageUpdated(result);

  evts.info('done');
}

self.addEventListener('message', function(e) {
  var handler = cmds[e.data.msgType];
  if (handler) {
    handler(e.data);
  } else {
    evts.error('unknown message type: ' + e.data.msgType)
  };
}, false);
