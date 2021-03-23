// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Relude_Set = require("relude/src/Relude_Set.bs.js");
var Relude_List = require("relude/src/Relude_List.bs.js");
var Relude_Tuple = require("relude/src/Relude_Tuple.bs.js");
var Relude_Option = require("relude/src/Relude_Option.bs.js");
var Relude_String = require("relude/src/Relude_String.bs.js");
var Relude_Function = require("relude/src/Relude_Function.bs.js");
var Relude_StringMap = require("relude/src/Relude_StringMap.bs.js");
var InputLoader$Aoc19 = require("./lib/InputLoader.bs.js");
var StackSafeFuture$Aoc19 = require("./lib/StackSafeFuture.bs.js");
var Relude_List_Specializations = require("relude/src/list/Relude_List_Specializations.bs.js");

function entryToTuple(entry) {
  return Relude_Option.getOrThrow(Relude_Tuple.fromList2(Relude_String.splitList(")", entry)));
}

var StringSet = Relude_Set.WithOrd(Relude_String.Ord);

function addEntry(param, param$1) {
  var orbiters = param$1._1;
  var point = param$1._0;
  var o = param[1];
  var p = param[0];
  if (p === point) {
    return /* Orbit */{
            _0: point,
            _1: {
              hd: /* Orbit */{
                _0: o,
                _1: /* [] */0
              },
              tl: orbiters
            }
          };
  }
  var partial_arg = [
    p,
    o
  ];
  return /* Orbit */{
          _0: point,
          _1: Relude_List.map(function (param) {
                  return addEntry(partial_arg, param);
                })(orbiters)
        };
}

function entriesToMap(entries) {
  return Relude_List.foldLeft((function (map, param) {
                  var orbiter = param[1];
                  var point = param[0];
                  var existing = Curry._2(Relude_StringMap.get, point, map);
                  if (existing !== undefined) {
                    return Curry._3(Relude_StringMap.set, point, {
                                hd: orbiter,
                                tl: existing
                              }, map);
                  } else {
                    return Curry._3(Relude_StringMap.set, point, {
                                hd: orbiter,
                                tl: /* [] */0
                              }, map);
                  }
                }), Curry._1(Relude_StringMap.make, undefined))(entries);
}

function addEntry$1(param, param$1) {
  return Relude_Function.flip(addEntry, param, param$1);
}

function mapToOrbit(map) {
  var _map = map;
  var _orbit = /* Orbit */{
    _0: "COM",
    _1: /* [] */0
  };
  while(true) {
    var orbit = _orbit;
    var map$1 = _map;
    var orbittingSet = Curry._1(StringSet.fromList, Curry._1(Relude_List.flatten, Curry._1(Relude_StringMap.values, map$1)));
    var orbittedSet = Curry._1(StringSet.fromList, Curry._1(Relude_StringMap.keys, map$1));
    var planetsNotOrbitting = Curry._2(StringSet.diff, orbittedSet, orbittingSet);
    var addOrbitAndRemoveFromMap = function (param, key) {
      var map = param[1];
      var values = Relude_List.map(function (v) {
              return [
                      key,
                      v
                    ];
            })(Relude_Option.getOrThrow(Curry._2(Relude_StringMap.get, key, map)));
      return [
              Relude_List.foldLeft(addEntry$1, param[0])(values),
              Curry._2(Relude_StringMap.remove, key, map)
            ];
    };
    var match = Curry._3(StringSet.foldLeft, addOrbitAndRemoveFromMap, [
          orbit,
          map$1
        ], planetsNotOrbitting);
    var newMap = match[1];
    var newOrbit = match[0];
    if (Curry._1(Relude_StringMap.isEmpty, newMap)) {
      return newOrbit;
    }
    _orbit = newOrbit;
    _map = newMap;
    continue ;
  };
}

function totalOrbitDistance(orbit) {
  var loop = function (count, orbit) {
    var rest = orbit._1;
    if (!rest) {
      return count;
    }
    var partial_arg = count + 1 | 0;
    return count + Curry._1(Relude_List_Specializations.Int.sum, Relude_List.map(function (param) {
                      return loop(partial_arg, param);
                    })(rest)) | 0;
  };
  return loop(0, orbit);
}

function findPathContaining(paths, planet) {
  return Relude_Option.getOrElse(/* [] */0, Curry._2(Relude_List.find, (function (a) {
                    return Curry._3(Relude_List.contains, Relude_String.Eq, planet, a);
                  }), paths));
}

function distanceBetweenYouAndSanta(orbit) {
  var allPaths = function (param) {
    var orbiters = param._1;
    var point = param._0;
    if (!orbiters) {
      return {
              hd: {
                hd: point,
                tl: /* [] */0
              },
              tl: /* [] */0
            };
    }
    var partial_arg = Relude_List.map(function (ps) {
          return {
                  hd: point,
                  tl: ps
                };
        });
    return Curry._2(Relude_List.flatMap, (function (param) {
                  return Relude_Function.flipCompose(allPaths, partial_arg, param);
                }), orbiters);
  };
  var allPaths$1 = allPaths(orbit);
  var santaPath = Curry._1(StringSet.fromList, findPathContaining(allPaths$1, "SAN"));
  var yourPath = Curry._1(StringSet.fromList, findPathContaining(allPaths$1, "YOU"));
  var differenceSantas = Curry._2(StringSet.diff, santaPath, yourPath);
  var differenceYours = Curry._2(StringSet.diff, yourPath, santaPath);
  return (Curry._1(StringSet.length, differenceSantas) + Curry._1(StringSet.length, differenceYours) | 0) - 2 | 0;
}

var input = InputLoader$Aoc19.newlineSeparated(6);

var orbits = StackSafeFuture$Aoc19.map((function (i) {
        return mapToOrbit(entriesToMap(Relude_List.map(entryToTuple)(i)));
      }), input);

StackSafeFuture$Aoc19.tap(function (i) {
        console.log("Total path is: ", totalOrbitDistance(i));
        
      })(orbits);

StackSafeFuture$Aoc19.tap(function (i) {
        console.log("Distance: ", distanceBetweenYouAndSanta(i));
        
      })(orbits);

var $great$great = Relude_Function.flipCompose;

var StringMap;

exports.$great$great = $great$great;
exports.entryToTuple = entryToTuple;
exports.StringMap = StringMap;
exports.StringSet = StringSet;
exports.entriesToMap = entriesToMap;
exports.addEntry = addEntry$1;
exports.mapToOrbit = mapToOrbit;
exports.totalOrbitDistance = totalOrbitDistance;
exports.findPathContaining = findPathContaining;
exports.distanceBetweenYouAndSanta = distanceBetweenYouAndSanta;
exports.input = input;
exports.orbits = orbits;
/* StringSet Not a pure module */
