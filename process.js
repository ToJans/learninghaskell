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

var setColor = function(i,img) {
  img.data[i] = 255;
  img.data[i+1] = 0;
  img.data[i+2] = 0;
}

function drawMarker(i,img) {
  for (var y= -5,iy=i-5*4*img.width;y<5;y++,iy+=4*img.width) {
    for (var x= -5,ix=iy-5*4;x<5;x++,ix+=4) {
      setColor(ix,img);
    }
  }
}

Filters.findRectangleCorners = function(img) {
  var points;
  var d = img.data;
  var isMatch = function(i){ return d[i]==m2R && d[i+1] == m2G && d[i+2]== m2B;}
  var isBoundMatch = function(i) {
    return i>0 && i<img.width * 4 * img.height && isMatch(i);
  }

  var i = 0;

  var getCoord = function(i,l,ca,sa) {
    var dx = Math.trunc(l*ca)*4;
    var dy = Math.trunc(l*sa)*4*img.width;
    return i+dx+dy;
  }

  var findLongestLength = function(i,minAngle, maxAngle,angleStep,minLength,otherPoints) {
    var match = null;
    for (var angle=minAngle;angle<=minAngle+maxAngle;angle+=angleStep) {
      var ca = Math.cos(angle),sa = Math.sin(angle);
      var dx = 0,dy=0;
      var ll=0,lr=0;
      while (isBoundMatch(getCoord(i,ll+minLength/10,ca,sa))) ll+=minLength/10;
      // while (isBoundMatch(getCoord(i,-lr-minLength/10,ca,sa))) lr+=minLength/10;
      if ((lr+ll) > minLength-(minLength/10)) {
        while (isBoundMatch(getCoord(i,ll+4,ca,sa))) ll++;
        // while (isBoundMatch(getCoord(i,-lr-4,ca,sa))) lr++;
        if ((lr+ll) > minLength &&
            (match == null || (match.lr+match.ll < lr+ll))) {
              var li =getCoord(i,ll,ca,sa);
              var ri =getCoord(i,-lr,ca,sa);
              if (otherPoints.indexOf(ri) == -1 || otherPoints.indexOf(li) == -1) {
                match = { ll:ll,lr:lr,ca:ca,sa:sa,angle:angle}
              }
        }
      }
    }
    evts.imageUpdated(img)
    if (match) {
      var li =getCoord(i,match.ll,match.ca,match.sa);
      var ri =getCoord(i,-match.lr,match.ca,match.sa);
      return {li:li,ri:ri};
    } else {
      return null;
    }
  }

  // find first point
  while (!isMatch(i) ) i+=4;

  i+=img.width*4*2;

  // drawMarker(i,img);

  evts.imageUpdated(img);

  var minlen = img.width>img.height?img.height:img.width;
  minlen /= 20;

  var p = [i];

  var startAngle = 0;
  var l = findLongestLength(i,startAngle,Math.PI,Math.PI/180,minlen,p)
  var isClosed = false;
  if (l) {
    while (l!== null && p.length<40) {
      if (p.indexOf(l.ri) != -1 && p.indexOf(l.li) != -1) {
        isClosed = true;
        break;
      }
      if (p.indexOf(l.ri) == -1) p.push(l.ri);
      if (p.indexOf(l.li) == -1) p.push(l.li);
      startAngle+=l.angle-Math.PI/4
      l = findLongestLength(l.ri,startAngle,Math.PI*1.5,Math.PI/180,minlen,p)
    }
  }

  p.map(function(i){
    drawMarker(i,img);
  });

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
