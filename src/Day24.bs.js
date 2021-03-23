// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Relude_Int = require("relude/src/Relude_Int.bs.js");
var Coord$Aoc19 = require("./lib/Coord.bs.js");
var Relude_List = require("relude/src/Relude_List.bs.js");
var Relude_Option = require("relude/src/Relude_Option.bs.js");
var Relude_String = require("relude/src/Relude_String.bs.js");
var Relude_Function = require("relude/src/Relude_Function.bs.js");
var Relude_List_Specializations = require("relude/src/list/Relude_List_Specializations.bs.js");

function environmentFromString(param) {
  switch (param) {
    case "#" :
        return /* Bug */1;
    case "." :
        return /* Empty */0;
    default:
      return ;
  }
}

function fromString(s) {
  return Curry._1(Coord$Aoc19.CoordMap.fromList, Coord$Aoc19.addCoordinates(Relude_List.map(function (param) {
                        return Relude_Function.flipCompose((function (param) {
                                      return Relude_String.splitAsList("", param);
                                    }), (function (param) {
                                      return Relude_List.mapOption(environmentFromString, param);
                                    }), param);
                      })(Relude_String.splitAsList("\n", s))));
}

var empty = fromString(".....\n  .....\n  .....\n  .....\n  .....");

function countBugs(neighbours) {
  return Curry._2(Relude_List.countBy, (function (param) {
                if (param) {
                  return true;
                } else {
                  return false;
                }
              }), neighbours);
}

function nextGenerationCell(cell, neighbours) {
  var bugCount = countBugs(neighbours);
  if (cell) {
    if (bugCount === 1) {
      return /* Bug */1;
    } else {
      return /* Empty */0;
    }
  } else if (bugCount === 1 || bugCount === 2) {
    return /* Bug */1;
  } else {
    return /* Empty */0;
  }
}

function nextGeneration(env, neighbours) {
  return Curry._2(Coord$Aoc19.CoordMap.mapWithKey, (function (coord, c) {
                return nextGenerationCell(c, Curry._1(neighbours, coord));
              }), env);
}

function mapAllCoords(f) {
  return Relude_List.map(f)({
              hd: 0,
              tl: {
                hd: 1,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 3,
                    tl: {
                      hd: 4,
                      tl: /* [] */0
                    }
                  }
                }
              }
            });
}

function neighbours(param, l) {
  var y = param[1];
  var x = param[0];
  if (x === 2 && y === 2) {
    return /* [] */0;
  }
  var left = x === 0 ? ({
        hd: [
          1,
          2,
          l - 1 | 0
        ],
        tl: /* [] */0
      }) : (
      x === 3 && y === 2 ? mapAllCoords(function (y) {
              return [
                      4,
                      y,
                      l + 1 | 0
                    ];
            }) : ({
            hd: [
              x - 1 | 0,
              y,
              l
            ],
            tl: /* [] */0
          })
    );
  var right = x === 4 ? ({
        hd: [
          3,
          2,
          l - 1 | 0
        ],
        tl: /* [] */0
      }) : (
      x === 1 && y === 2 ? mapAllCoords(function (y) {
              return [
                      0,
                      y,
                      l + 1 | 0
                    ];
            }) : ({
            hd: [
              x + 1 | 0,
              y,
              l
            ],
            tl: /* [] */0
          })
    );
  var up = y === 0 ? ({
        hd: [
          2,
          1,
          l - 1 | 0
        ],
        tl: /* [] */0
      }) : (
      x === 2 && y === 3 ? mapAllCoords(function (x) {
              return [
                      x,
                      4,
                      l + 1 | 0
                    ];
            }) : ({
            hd: [
              x,
              y - 1 | 0,
              l
            ],
            tl: /* [] */0
          })
    );
  var down = y === 4 ? ({
        hd: [
          2,
          3,
          l - 1 | 0
        ],
        tl: /* [] */0
      }) : (
      x === 2 && y === 1 ? mapAllCoords(function (x) {
              return [
                      x,
                      0,
                      l + 1 | 0
                    ];
            }) : ({
            hd: [
              x,
              y + 1 | 0,
              l
            ],
            tl: /* [] */0
          })
    );
  return Curry._1(Relude_List.flatten, {
              hd: left,
              tl: {
                hd: right,
                tl: {
                  hd: down,
                  tl: {
                    hd: up,
                    tl: /* [] */0
                  }
                }
              }
            });
}

function retrieveRecursiveNeighbours(neighbours, env) {
  return Relude_List.mapOption((function (param) {
                var y = param[1];
                var x = param[0];
                return Curry._2(Relude_Option.flatMap, (function (level) {
                              return Curry._2(Coord$Aoc19.CoordMap.get, [
                                          x,
                                          y
                                        ], level);
                            }), Curry._2(Relude_Int.$$Map.get, param[2], env));
              }), neighbours);
}

function coordToInt(param) {
  var shift = param[0] + Math.imul(param[1], 5) | 0;
  return (1 << shift);
}

function envHealth(env) {
  return Relude_List.foldLeft((function (h, param) {
                  if (param[1]) {
                    return h + coordToInt(param[0]) | 0;
                  } else {
                    return h;
                  }
                }), 0)(Curry._1(Coord$Aoc19.CoordMap.toList, env));
}

function duplicateHealth(env) {
  var startHealths = Curry._1(Relude_Int.$$Set.fromList, {
        hd: envHealth(env),
        tl: /* [] */0
      });
  var _healths = startHealths;
  var _lastEnv = env;
  while(true) {
    var lastEnv = _lastEnv;
    var healths = _healths;
    var nextEnv = nextGeneration(lastEnv, (function(lastEnv){
        return function (param) {
          var y = param[1];
          var x = param[0];
          return Relude_List.mapOption((function (c) {
                        return Curry._2(Coord$Aoc19.CoordMap.get, c, lastEnv);
                      }), {
                      hd: [
                        x + 1 | 0,
                        y
                      ],
                      tl: {
                        hd: [
                          x - 1 | 0,
                          y
                        ],
                        tl: {
                          hd: [
                            x,
                            y + 1 | 0
                          ],
                          tl: {
                            hd: [
                              x,
                              y - 1 | 0
                            ],
                            tl: /* [] */0
                          }
                        }
                      }
                    });
        }
        }(lastEnv)));
    var health = envHealth(nextEnv);
    if (Curry._2(Relude_Int.$$Set.contains, health, healths)) {
      return health;
    }
    _lastEnv = nextEnv;
    _healths = Curry._2(Relude_Int.$$Set.update, health, healths);
    continue ;
  };
}

function nextGenerationRecursive(env) {
  var definedLevels = Curry._1(Relude_Int.$$Map.keys, env);
  var min = Relude_Option.getOrThrow(Curry._1(Relude_List_Specializations.Int.min, definedLevels));
  var max = Relude_Option.getOrThrow(Curry._1(Relude_List_Specializations.Int.max, definedLevels));
  var _remaining = {
    hd: min - 1 | 0,
    tl: {
      hd: max + 1 | 0,
      tl: definedLevels
    }
  };
  var _lastEnv = env;
  while(true) {
    var lastEnv = _lastEnv;
    var remaining = _remaining;
    if (!remaining) {
      return lastEnv;
    }
    var current = remaining.hd;
    var level = Curry._3(Relude_Int.$$Map.getOrElse, current, empty, env);
    var nextLevel = nextGeneration(level, (function(current){
        return function (c) {
          var neighbourCoords = neighbours(c, current);
          return retrieveRecursiveNeighbours(neighbourCoords, env);
        }
        }(current)));
    var nextEnv = envHealth(nextLevel) > 0 || Curry._2(Relude_Int.$$Map.contains, current, lastEnv) ? Curry._3(Relude_Int.$$Map.set, current, nextLevel, lastEnv) : lastEnv;
    _lastEnv = nextEnv;
    _remaining = remaining.tl;
    continue ;
  };
}

function countBugsRecursive(env) {
  var definedLevels = Curry._1(Relude_Int.$$Map.values, env);
  return Curry._1(Relude_List_Specializations.Int.sum, Relude_List.map(function (level) {
                    return countBugs(Curry._1(Coord$Aoc19.CoordMap.values, level));
                  })(definedLevels));
}

function drawEnvRecursive(gen, env) {
  console.log([
        "Generation",
        gen
      ]);
  console.log("");
  return Curry._2(Relude_List.forEach, (function (param) {
                console.log([
                      "Level",
                      param[0]
                    ]);
                Coord$Aoc19.output((function (param) {
                        if (param) {
                          return "#";
                        } else {
                          return ".";
                        }
                      }), /* Empty */0, param[1]);
                console.log("");
                
              }), Curry._1(Relude_Int.$$Map.toList, env));
}

function runRecusive(generations, start) {
  var startEnv = Curry._1(Relude_Int.$$Map.fromList, {
        hd: [
          0,
          start
        ],
        tl: /* [] */0
      });
  var _n = generations;
  var _lastEnv = startEnv;
  while(true) {
    var lastEnv = _lastEnv;
    var n = _n;
    if (n === 0) {
      return countBugsRecursive(lastEnv);
    }
    var nextEnv = nextGenerationRecursive(lastEnv);
    _lastEnv = nextEnv;
    _n = n - 1 | 0;
    continue ;
  };
}

var Bugs = {
  environmentFromString: environmentFromString,
  fromString: fromString,
  empty: empty,
  countBugs: countBugs,
  nextGenerationCell: nextGenerationCell,
  nextGeneration: nextGeneration,
  mapAllCoords: mapAllCoords,
  neighbours: neighbours,
  retrieveRecursiveNeighbours: retrieveRecursiveNeighbours,
  coordToInt: coordToInt,
  envHealth: envHealth,
  duplicateHealth: duplicateHealth,
  nextGenerationRecursive: nextGenerationRecursive,
  countBugsRecursive: countBugsRecursive,
  drawEnvRecursive: drawEnvRecursive,
  show: false,
  runRecusive: runRecusive
};

var input = "##.##\n.#.##\n##..#\n#.#..\n.###.";

console.log(duplicateHealth(fromString(input)));

console.log(runRecusive(200, fromString(input)));

var $great$great = Relude_Function.flipCompose;

exports.$great$great = $great$great;
exports.Bugs = Bugs;
exports.input = input;
/* empty Not a pure module */
