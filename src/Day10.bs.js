// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Relude_Int = require("relude/src/Relude_Int.bs.js");
var Coord$Aoc19 = require("./lib/Coord.bs.js");
var Relude_List = require("relude/src/Relude_List.bs.js");
var Relude_Float = require("relude/src/Relude_Float.bs.js");
var Relude_Option = require("relude/src/Relude_Option.bs.js");
var Relude_String = require("relude/src/Relude_String.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Relude_Function = require("relude/src/Relude_Function.bs.js");
var InputLoader$Aoc19 = require("./lib/InputLoader.bs.js");
var StackSafeFuture$Aoc19 = require("./lib/StackSafeFuture.bs.js");

var UnknownArea = Caml_exceptions.create("Day10-Aoc19.Space.UnknownArea");

function fromString(s) {
  var partial_arg = Relude_List.map(function ($$char) {
        switch ($$char) {
          case "#" :
              return /* Asteroid */0;
          case "." :
              return /* EmptySpace */1;
          default:
            throw {
                  RE_EXN_ID: UnknownArea,
                  _1: $$char,
                  Error: new Error()
                };
        }
      });
  return Curry._1(Coord$Aoc19.CoordMap.fromList, Coord$Aoc19.addCoordinates(Relude_List.map(function (param) {
                        return Relude_Function.flipCompose((function (param) {
                                      return Relude_String.splitList("", param);
                                    }), partial_arg, param);
                      })(Relude_String.splitList("\n", s))));
}

var size = Coord$Aoc19.CoordMap.length;

function get(v) {
  var partial_arg = Curry._1(Coord$Aoc19.CoordMap.get, v);
  return function (param) {
    return Relude_Function.flipCompose(partial_arg, (function (param) {
                  return Relude_Option.getOrElse(/* OutOfBounds */2, param);
                }), param);
  };
}

function vaporise(v) {
  return Curry._1(Coord$Aoc19.CoordMap.remove, v);
}

function asteroids(space) {
  return Curry._1(Coord$Aoc19.CoordMap.keys, Curry._2(Coord$Aoc19.CoordMap.filter, (function (param, v) {
                    return v === /* Asteroid */0;
                  }), space));
}

var Space = {
  UnknownArea: UnknownArea,
  fromString: fromString,
  size: size,
  get: get,
  vaporise: vaporise,
  asteroids: asteroids
};

function gcm(param) {
  var b = param[1];
  var a = param[0];
  if (a === 0) {
    return Pervasives.abs(b);
  }
  if (b === 0) {
    return Pervasives.abs(a);
  }
  var a$1 = Pervasives.abs(a);
  var b$1 = Pervasives.abs(b);
  var min = a$1 < b$1 ? a$1 : b$1;
  return Relude_List.foldLeft((function (gcm, i) {
                  if (Caml_int32.mod_(a$1, i) === 0 && Caml_int32.mod_(b$1, i) === 0) {
                    return i;
                  } else {
                    return gcm;
                  }
                }), 1)(Relude_List.makeWithIndex(min, (function (param) {
                    return 1 + param | 0;
                  })));
}

function isVisible(origin, space, p) {
  if (Caml_obj.caml_equal(p, origin)) {
    return false;
  }
  var vector = Coord$Aoc19.sub(p, origin);
  var gcm$1 = gcm(vector);
  var grad = Coord$Aoc19.div(vector, gcm$1);
  var _p = Coord$Aoc19.sub(p, grad);
  while(true) {
    var p$1 = _p;
    if (Caml_obj.caml_equal(p$1, origin)) {
      return true;
    }
    var area = get(p$1)(space);
    if (area === /* Asteroid */0) {
      return false;
    }
    _p = Coord$Aoc19.sub(p$1, grad);
    continue ;
  };
}

function visibleCount(space, p) {
  var asteroids$1 = asteroids(space);
  var isVisible$1 = function (param) {
    return isVisible(p, space, param);
  };
  return Curry._1(Relude_List.length, Relude_List.filter(isVisible$1, asteroids$1));
}

function findBestAsteroid(space) {
  var asteroids$1 = asteroids(space);
  return Relude_Option.getOrThrow(Relude_List.head(Relude_List.map(function (param) {
                        return param[0];
                      })(Relude_List.sortBy((function (param, param$1) {
                            return Curry._2(Relude_Int.compare, param$1[1], param[1]);
                          }), Relude_List.zip(asteroids$1, Relude_List.map(function (param) {
                                    return visibleCount(space, param);
                                  })(asteroids$1))))));
}

function vectorLength(param) {
  var y = param[1];
  var x = param[0];
  return Math.sqrt(Math.imul(x, x) + Math.imul(y, y) | 0);
}

function angle(param, param$1) {
  var y2 = param$1[1];
  var x2 = param$1[0];
  var y1 = param[1];
  var x1 = param[0];
  var determinent = Math.imul(x1, y2) - Math.imul(y1, x2) | 0;
  var dotProduct = Math.imul(x1, x2) + Math.imul(y1, y2) | 0;
  var a = Math.atan2(determinent, dotProduct);
  if (a < 0) {
    return a + 2 * Math.PI;
  } else {
    return a;
  }
}

function minimumAngle(origin, vector, asteroids) {
  return Relude_Option.getOrThrow(Relude_List.head(Relude_List.map(function (param) {
                        return param[0];
                      })(Relude_List.sortBy((function (param, param$1) {
                            return Curry._2(Relude_Float.compare, param[1], param$1[1]);
                          }), Relude_List.zip(asteroids, Relude_List.map(function (param) {
                                    return angle(vector, param);
                                  })(Relude_List.map(function (c) {
                                        return Coord$Aoc19.sub(c, origin);
                                      })(asteroids)))))));
}

function findFirstTarget(origin, space) {
  var asteroids$1 = asteroids(space);
  var visible = Relude_List.filter((function (param) {
          return isVisible(origin, space, param);
        }), asteroids$1);
  return minimumAngle(origin, [
              0,
              -1
            ], visible);
}

function findNextTarget(origin, space, current) {
  var asteroids$1 = asteroids(space);
  var vector = Coord$Aoc19.sub(current, origin);
  var visible = Relude_List.filter((function (param) {
          return Relude_Function.flipCompose((function (param) {
                        return Caml_obj.caml_equal(current, param);
                      }), (function (prim) {
                        return !prim;
                      }), param);
        }), Relude_List.filter((function (param) {
              return isVisible(origin, space, param);
            }), asteroids$1));
  return minimumAngle(origin, vector, visible);
}

function bigFuckingLaser(origin, space, desiredCount) {
  var firstTarget = findFirstTarget(origin, space);
  var _space = space;
  var _current = firstTarget;
  var _count = 1;
  while(true) {
    var count = _count;
    var current = _current;
    var space$1 = _space;
    if (count === desiredCount) {
      return current;
    }
    var nextTarget = findNextTarget(origin, space$1, current);
    var destroyedTarget = Curry._2(Coord$Aoc19.CoordMap.remove, current, space$1);
    _count = count + 1 | 0;
    _current = nextTarget;
    _space = destroyedTarget;
    continue ;
  };
}

var input = InputLoader$Aoc19.loadDay(10);

StackSafeFuture$Aoc19.tap(function (param) {
        var match = bigFuckingLaser(param[1], param[0], 200);
        console.log("200th to be boomed", Math.imul(match[0], 100) + match[1] | 0);
        
      })(StackSafeFuture$Aoc19.tap(function (param) {
            console.log("Best asteroid", visibleCount(param[0], param[1]));
            
          })(StackSafeFuture$Aoc19.map((function (space) {
                return [
                        space,
                        findBestAsteroid(space)
                      ];
              }), StackSafeFuture$Aoc19.map(fromString, input))));

var $great$great = Relude_Function.flipCompose;

exports.$great$great = $great$great;
exports.Space = Space;
exports.gcm = gcm;
exports.isVisible = isVisible;
exports.visibleCount = visibleCount;
exports.findBestAsteroid = findBestAsteroid;
exports.vectorLength = vectorLength;
exports.angle = angle;
exports.minimumAngle = minimumAngle;
exports.findFirstTarget = findFirstTarget;
exports.findNextTarget = findNextTarget;
exports.bigFuckingLaser = bigFuckingLaser;
exports.input = input;
/* input Not a pure module */
