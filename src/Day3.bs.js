// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Future = require("reason-future/src/Future.bs.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Relude_Int = require("relude/src/Relude_Int.bs.js");
var Relude_Map = require("relude/src/Relude_Map.bs.js");
var Relude_Set = require("relude/src/Relude_Set.bs.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Relude_List = require("relude/src/Relude_List.bs.js");
var Relude_Option = require("relude/src/Relude_Option.bs.js");
var Relude_String = require("relude/src/Relude_String.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Relude_Function = require("relude/src/Relude_Function.bs.js");
var InputLoader$Aoc19 = require("./lib/InputLoader.bs.js");
var Relude_List_Specializations = require("relude/src/list/Relude_List_Specializations.bs.js");

var InvalidDirection = Caml_exceptions.create("Day3-Aoc19.InvalidDirection");

var $great$great = Relude_Function.Infix.$great$great;

function stringToDirection(s) {
  var split = Relude_String.splitAt(1, s);
  var l = split[0];
  switch (l) {
    case "D" :
        return /* Down */Block.__(3, [Caml_format.caml_int_of_string(split[1])]);
    case "L" :
        return /* Left */Block.__(2, [Caml_format.caml_int_of_string(split[1])]);
    case "R" :
        return /* Right */Block.__(0, [Caml_format.caml_int_of_string(split[1])]);
    case "U" :
        return /* Up */Block.__(1, [Caml_format.caml_int_of_string(split[1])]);
    default:
      throw [
            InvalidDirection,
            l
          ];
  }
}

function distance(d) {
  return d[0];
}

function pathFromDirection(direction, coord) {
  var _path = /* [] */0;
  var _dir = direction;
  var _coord$prime = coord;
  while(true) {
    var coord$prime = _coord$prime;
    var dir = _dir;
    var path = _path;
    var y = coord$prime[1];
    var x = coord$prime[0];
    var match = dir[0] > 0;
    if (match) {
      var nextDistance = dir[0] - 1 | 0;
      var match$1;
      switch (dir.tag | 0) {
        case /* Right */0 :
            match$1 = /* tuple */[
              /* tuple */[
                x + 1 | 0,
                y
              ],
              /* Right */Block.__(0, [nextDistance])
            ];
            break;
        case /* Up */1 :
            match$1 = /* tuple */[
              /* tuple */[
                x,
                y + 1 | 0
              ],
              /* Up */Block.__(1, [nextDistance])
            ];
            break;
        case /* Left */2 :
            match$1 = /* tuple */[
              /* tuple */[
                x - 1 | 0,
                y
              ],
              /* Left */Block.__(2, [nextDistance])
            ];
            break;
        case /* Down */3 :
            match$1 = /* tuple */[
              /* tuple */[
                x,
                y - 1 | 0
              ],
              /* Down */Block.__(3, [nextDistance])
            ];
            break;
        
      }
      var nextCoord = match$1[0];
      _coord$prime = nextCoord;
      _dir = match$1[1];
      _path = /* :: */[
        nextCoord,
        path
      ];
      continue ;
    } else {
      return path;
    }
  };
}

function pathFromDirections(directions) {
  return Relude_List.foldLeft((function (acc, dir) {
                  var current = Relude_List.head(acc);
                  var path = Curry._1(Relude_List.flatten, Curry._1(Relude_Option.toList, Relude_Option.map((function (param) {
                                  return pathFromDirection(dir, param);
                                }), current)));
                  return Relude_List.concat(path, acc);
                }), /* :: */[
                /* tuple */[
                  0,
                  0
                ],
                /* [] */0
              ])(directions);
}

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

function getCrossings(a, b) {
  var aSet = Curry._1(CoordSet.fromList, a);
  var bSet = Curry._1(CoordSet.fromList, b);
  return Curry._2(CoordSet.remove, /* tuple */[
              0,
              0
            ], Curry._2(CoordSet.intersect, aSet, bSet));
}

function manhattenDistance(coord) {
  return Curry._1(Relude_Int.abs, coord[0]) + Curry._1(Relude_Int.abs, coord[1]) | 0;
}

function getClosestToOrigin(coords) {
  return Relude_Option.getOrThrow(Relude_List.head(Relude_List.sortBy((function (a, b) {
                        return Curry._2(Relude_Int.compare, manhattenDistance(a), manhattenDistance(b));
                      }), coords)));
}

function addDistanceToPath(path) {
  var pathLength = Curry._1(Relude_List.length, path);
  return Relude_List.mapWithIndex((function (v, i) {
                return /* tuple */[
                        v,
                        (pathLength - i | 0) - 1 | 0
                      ];
              }), path);
}

function addDistancesToMap(path, map) {
  return Relude_List.foldLeft((function (map, coordWithDistance) {
                  var coord = coordWithDistance[0];
                  var match = Curry._2(CoordMap.get, coord, map);
                  if (match !== undefined) {
                    return Curry._3(CoordMap.set, coord, match + coordWithDistance[1] | 0, map);
                  } else {
                    return map;
                  }
                }), map)(path);
}

function findClosestIntersection(pathA, pathB) {
  var intersections = Curry._1(CoordSet.toList, getCrossings(pathA, pathB));
  var intersectionsWithDistance = Belt_List.zip(intersections, Belt_List.make(Belt_List.length(intersections), 0));
  var intersectionsMap = Curry._1(CoordMap.fromList, intersectionsWithDistance);
  var pathADistances = addDistanceToPath(pathA);
  var pathBDistances = addDistanceToPath(pathB);
  var intersectionsMap$1 = addDistancesToMap(pathADistances, intersectionsMap);
  var intersectionsMap$2 = addDistancesToMap(pathBDistances, intersectionsMap$1);
  return Curry._1(Relude_List_Specializations.Int.min, Curry._1(CoordMap.values, intersectionsMap$2));
}

var partial_arg = Relude_List.map(pathFromDirections);

var partial_arg$1 = Relude_List.map(stringToDirection);

var partial_arg$2 = Relude_List.map((function (param) {
        return $great$great((function (param) {
                      return Relude_String.splitList(",", param);
                    }), partial_arg$1, param);
      }));

var input = Future.map(InputLoader$Aoc19.newlineSeparated(3), (function (param) {
        return $great$great(partial_arg$2, partial_arg, param);
      }));

Future.tap(input, (function (a) {
        if (a) {
          var match = a[1];
          if (match && !match[1]) {
            console.log("Closest to origin", manhattenDistance(getClosestToOrigin(Curry._1(CoordSet.toList, getCrossings(a[0], match[0])))));
            return /* () */0;
          } else {
            return /* () */0;
          }
        } else {
          return /* () */0;
        }
      }));

Future.tap(input, (function (a) {
        if (a) {
          var match = a[1];
          if (match && !match[1]) {
            console.log("Closest paths", findClosestIntersection(a[0], match[0]));
            return /* () */0;
          } else {
            return /* () */0;
          }
        } else {
          return /* () */0;
        }
      }));

exports.InvalidDirection = InvalidDirection;
exports.$great$great = $great$great;
exports.stringToDirection = stringToDirection;
exports.distance = distance;
exports.pathFromDirection = pathFromDirection;
exports.pathFromDirections = pathFromDirections;
exports.CoordOrd = CoordOrd;
exports.CoordSet = CoordSet;
exports.CoordMap = CoordMap;
exports.getCrossings = getCrossings;
exports.manhattenDistance = manhattenDistance;
exports.getClosestToOrigin = getClosestToOrigin;
exports.addDistanceToPath = addDistanceToPath;
exports.addDistancesToMap = addDistancesToMap;
exports.findClosestIntersection = findClosestIntersection;
exports.input = input;
/* CoordSet Not a pure module */
