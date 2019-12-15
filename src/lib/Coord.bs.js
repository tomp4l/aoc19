// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Relude_Int = require("relude/src/Relude_Int.bs.js");
var Relude_Map = require("relude/src/Relude_Map.bs.js");
var Relude_Set = require("relude/src/Relude_Set.bs.js");
var Relude_List = require("relude/src/Relude_List.bs.js");
var Relude_Option = require("relude/src/Relude_Option.bs.js");
var Relude_List_Specializations = require("relude/src/list/Relude_List_Specializations.bs.js");

function compare(a, b) {
  var i = Curry._2(Relude_Int.compare, a[0], b[0]);
  if (i !== -718572442) {
    return i;
  } else {
    return Curry._2(Relude_Int.compare, a[1], b[1]);
  }
}

function eq(a, b) {
  return compare(a, b) === /* equal_to */-718572442;
}

var CoordOrd = {
  compare: compare,
  eq: eq
};

var CoordSet = Relude_Set.WithOrd({
      eq: eq,
      compare: compare
    });

var CoordMap = Relude_Map.WithOrd({
      eq: eq,
      compare: compare
    });

function add(param, param$1) {
  return /* tuple */[
          param[0] + param$1[0] | 0,
          param[1] + param$1[1] | 0
        ];
}

function sub(param, param$1) {
  return /* tuple */[
          param[0] - param$1[0] | 0,
          param[1] - param$1[1] | 0
        ];
}

function div(param, c) {
  return /* tuple */[
          Caml_int32.div(param[0], c),
          Caml_int32.div(param[1], c)
        ];
}

function output(toString, $$default, map) {
  var points = Curry._1(CoordMap.keys, map);
  var match = Relude_List.unzip(points);
  var ys = match[1];
  var xs = match[0];
  var minX = Relude_Option.getOrElse(0, Curry._1(Relude_List_Specializations.Int.min, xs));
  var maxX = Relude_Option.getOrElse(0, Curry._1(Relude_List_Specializations.Int.max, xs));
  var minY = Relude_Option.getOrElse(0, Curry._1(Relude_List_Specializations.Int.min, ys));
  var maxY = Relude_Option.getOrElse(0, Curry._1(Relude_List_Specializations.Int.max, ys));
  var _y = minY;
  while(true) {
    var y = _y;
    if (y <= maxY) {
      var loopX = (function(y){
      return function loopX(_x) {
        while(true) {
          var x = _x;
          if (x <= maxX) {
            process.stdout.write(Curry._1(toString, Relude_Option.getOrElse($$default, Curry._2(CoordMap.get, /* tuple */[
                              x,
                              y
                            ], map))));
            _x = x + 1 | 0;
            continue ;
          } else {
            return 0;
          }
        };
      }
      }(y));
      loopX(minX);
      process.stdout.write("\n");
      _y = y + 1 | 0;
      continue ;
    } else {
      return 0;
    }
  };
}

exports.CoordOrd = CoordOrd;
exports.CoordSet = CoordSet;
exports.CoordMap = CoordMap;
exports.add = add;
exports.sub = sub;
exports.div = div;
exports.output = output;
/* CoordSet Not a pure module */
