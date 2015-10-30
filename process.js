"use strict";

var Filters = {};

Filters.grayScale = function(img) {
    var d = img.data;
    var nd = new Uint8ClampedArray(d.length)
    for (var i = 0; i<d.length;i+=4) {
        var r = d[i];
        var g = d[i+1];
        var b = d[i+2];
        var v = 0.2126*r + 0.7152*g + 0.0722*b;
        nd[i] = v;
        nd[i+1] = v;
        nd[i+2] = v;
    }
    return {data:nd,width:img.width,height:img.height}
}

Filters.blur = function(img) {
  var d = img.data;
  var nd = new Uint8ClampedArray(d.length)
  var fsize = 3, middle = 1;

  for (var i = 0; i<d.length - (fsize * 4);i+=4) {
      var vcenter,vsum = 0,ix,iy;
      for (var x = 0,ix = i;x<fsize;x++,ix += 4) {
        for (var y = 0,iy = ix;y<fsize;y++,iy += 4 * img.width) {
            vsum += d[iy];
        }
      }
      var v = vsum /9;
      nd[i] = v;
      nd[i+1] = v;
      nd[i+2] = v;
  }
  return {data:nd,width:img.width,height:img.height}
}

Filters.adaptiveTreshold = function(img) {
    var d = img.data;
    var nd = new Uint8ClampedArray(d.length)
    var fsize = 11, middle = 5;

    for (var i = 0; i<d.length - (fsize * 4);i+=4) {
        var vcenter,vsum=0,ix,iy;
        for (var x = 0,ix = i;x<fsize;x++,ix += 4) {
          for (var y = 0,iy = ix;y<fsize;y++,iy += 4 * img.width) {
            var val = d[iy]
            if (x == middle && y == middle) {
              vcenter = val * (fsize * fsize - 1);
            } else {
              vsum += val;
            }
          }
        }
        var v = vcenter > vsum * 0.97 ? 255 : 0;
        nd[i] = v;
        nd[i+1] = v;
        nd[i+2] = v;
    }
    return {data:nd,width:img.width,height:img.height}
}

var m1R = 255, m1G =   1, m1B =   1; // floodfill color
var m2R =   1, m2G = 255, m2B =   1; // color of largest blob
var m3R =   1, m3G =   1, m3B = 255; // color of other blobs

function createNoneWrappingBoarders(img) {
  for (var y=0,i=0;y<img.height;y++,i+=img.width * 4) {
    img.data[i] = img.data[i + img.width*4 - 4] = 128;
  }
}

function floodFill(xs,img) {
  var i;
  while (xs.length>0) {
    i = xs.pop();
    if (i<0 || i>= img.data.length - img.width * 4 * 10) continue;
    if (img.data[i] != 0) continue;
    img.data[i  ] = m1R;
    img.data[i+1] = m1G;
    img.data[i+2] = m1B;
    xs.push(i-4);
    xs.push(i+4);
    xs.push(i - img.width * 4);
    xs.push(i + img.width * 4);
  }
}

// wraps around borders for speed, image requires borders that don't match
function scanlineFill (index, img) {
  var isMatch = function(i) { return img.data[i] == 0;};
  var isNoMatch = function(i) { return !isMatch(i);};
  var isInBounds = function(i) { return i>=0 && i < img.data.length;};
  var fillPixel = function(i) {
    img.data[i  ] = m1R;
    img.data[i+1] = m1G;
    img.data[i+2] = m1B;
  };
  var isFilled = function(i) {
    return img.data[i] == m1R && img.data[i+1] == m1G && img.data[i+2] == m1B;
  }

  var prevPixel = function(i) { return i - 4;};
  var nextPixel = function(i) { return i + 4;};
  var prevLine = function(i) { return i - img.width*4;};
  var nextLine = function(i) { return i - img.width*4;};

  function scanLeftRight(oleft,oright) {
    var left = oleft,right = oright;

    if (!isInBounds(left) || ! isInBounds(right)) return;

    if (isFilled(left) && isFilled(right)) return;

    var leftScanBound = left - (left/4 % img.width)*4;
    var rightScanBound = leftScanBound + (img.width-1) * 4;

    var isInScanBound = function(i) {
      return i >= leftScanBound && i <= rightScanBound;
    }

    // find matching pixel in the range.
    if (isMatch(left)) {
      while (isInScanBound(prevPixel(left)) && isMatch(prevPixel(left))) {
        left =prevPixel(left);
      }
    } else {
      while (left <= right && isNoMatch(left)) {
        left = nextPixel(left);
      }
    }
    while (isMatch(left) && left < right) {
      var i = left;
      while (i <= right && isMatch(i) && isInScanBound(i)) {
        fillPixel(i);
        i = nextPixel(i);
      }
      scanLeftRight(prevLine(left),prevLine(prevPixel(i)));
      scanLeftRight(nextLine(left),nextLine(prevPixel(i)));
      left = i;
      while (left<right && isNoMatch(left)) {
        left=nextPixel(left);
      }
    }
  }

  scanLeftRight(index,index);
}

var findColorBounds=function(r,g,b,img) {
  var minx = img.width, maxx = 0, miny = img.height,maxy = 0,d=img.data;
  for (var x = 0,xi = 0;x<img.width;x++,xi+=4) {
    for (var y = 0,xy = xi; y<img.height;y++,xy+=4*img.width) {
      if (d[xy]!=r || d[xy+1] != g || d[xy+2]!= b) continue;
      if (x<minx) minx = x;
      if (x>maxx) maxx = x;
      if (y<miny) miny = y;
      if (y>maxy) maxy = y;
    }
  }
  return {left:minx,right:maxx,top:miny,bottom:maxy};
}

var boundArea = function(bounds) {
  return (bounds.bottom-bounds.top) * (bounds.right- bounds.left);
}

var isValidSudokuBlob = function (bounds,img) {
  var size = {width:bounds.right-bounds.left,height:bounds.bottom - bounds.top};
  var center = {x:bounds.left+size.height/2,y:bounds.top + size.height/2};
  var dx = img.width - img.height;
  var dy = img.height - img.width;
  dx = dx > 0 ? dx/2 : 0;
  dy = dy > 0 ? dy/2 : 0;
  return (size.width+dx*2) > img.width /3 &&
         (size.height+dy*2) > img.height / 3 &&
         center.x + dx > img.width/3 &&
         center.y + dy > img.height/3 &&
         center.x - dx < img.width*2/3 &&
         center.y - dy < img.height*2/3 &&
         bounds.left > 25 &&
         bounds.right < img.width - 25 &&
         bounds.top > 25 &&
         bounds.bottom < img.height - 25;
}

var replaceColor=function(r,g,b,rr,gg,bb,img) {
  var d = img.data;
  for (var x = 0,xi = 0;x<img.width;x++,xi+=4) {
    for (var y = 0,xy = xi; y<img.height;y++,xy+=4*img.width) {
      if (d[xy]!=r || d[xy+1] != g || d[xy+2]!= b) continue;
      d[xy]=rr,d[xy+1]=gg,d[xy+2]=bb;
    }
  }
}

Filters.largestBlob = function(img,callback) {
  var oldD = img.data;
  var d = new Uint8ClampedArray(oldD);
  img.data = d;

  var minWidthOfBlob =  img.width/2;

  var skipWidth = minWidthOfBlob / 2;
  var skipheight = 3;
  var maxy = img.height;

  var largestBlobArea = 0;
  var selectedBound = null;

  for (var i = 0; i<d.length - img.width * 4 * 10;i+=4*skipWidth) {
    var ii = i
    if (d[ii]!=0) continue;
    floodFill([ii],img);
    var bounds = findColorBounds(m1R,m1G,m1B,img);
    var myArea = boundArea(bounds);
    if (isValidSudokuBlob(bounds,img) && myArea > largestBlobArea) {

      largestBlobArea = myArea;
      selectedBound = bounds;

      replaceColor(m2R,m2G,m2B,m3R,m3G,m3B,img);
      replaceColor(m1R,m1G,m1B,m2R,m2G,m2B,img);
      if (myArea > img.width * img.height * 0.5)
        break;
    } else {
      replaceColor(m1R,m1G,m1B,m3R,m3G,m3B,img);
    }
    if (callback) callback({width: img.width,height:img.height,data:d});
  }
  if (selectedBound) {
    return {data:d,width:img.width,height:img.height}
  } else {
    return null;
  }
}

Filters.findRectangleCorners = function(img) {
  // var points;
  // var d = img.data;
  // var isMatch = function(i){ return d[i]==m2R && d[i+1] == m2G && d[i+2]== m2B;}
  // for (var x=1,ix = 4;x<img.width-1;x++,ix+=4) {
  //   for (var y=1,iy = ix+4*img.width;y<img.height-1;y++,iy+=4*img.width) {
  //     if (isMatch(iy)) {
  //       var lr = if (!isMatch(d[iy] && isMatch)
  //     }
  //   }
  // }
  return img;
};

var evts = {};

evts.imageUpdated = function (imgData) {
  self.postMessage({msgType:'imageUpdated',data:imgData});
}

evts.error = function(msg) {
  self.postMessage({msgType:'error',data:msg});
}

evts.info = function(msg) {
  self.postMessage({msgType:'info',data:msg});
}

var cmds = {};

cmds.processImage = function (e) {
  var imgData = e.data;
  evts.info('converting to grayscale');
  var result = Filters.grayScale(imgData);
  evts.imageUpdated(result);
  evts.info('converting to black and white');
  result = Filters.adaptiveTreshold(result);
  createNoneWrappingBoarders(result)
  evts.imageUpdated(result);
  evts.info('locating sudoku');
  var result = Filters.largestBlob(result,evts.imageUpdated);
  if (result === null) {
    evts.info('unable to locate sudoku');
    return;
  }
  evts.info('finding corners');
  result = Filters.findRectangleCorners(result);
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
