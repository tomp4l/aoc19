// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Relude_Int = require("relude/src/Relude_Int.bs.js");
var Relude_Map = require("relude/src/Relude_Map.bs.js");
var Relude_Set = require("relude/src/Relude_Set.bs.js");

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

exports.CoordOrd = CoordOrd;
exports.CoordSet = CoordSet;
exports.CoordMap = CoordMap;
exports.add = add;
exports.sub = sub;
exports.div = div;
/* CoordSet Not a pure module */