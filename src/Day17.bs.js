// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Char = require("bs-platform/lib/js/char.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Coord$Aoc19 = require("./lib/Coord.bs.js");
var Relude_List = require("relude/src/Relude_List.bs.js");
var Intcode$Aoc19 = require("./Intcode.bs.js");
var Relude_Option = require("relude/src/Relude_Option.bs.js");
var Relude_String = require("relude/src/Relude_String.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Relude_Function = require("relude/src/Relude_Function.bs.js");
var InputLoader$Aoc19 = require("./lib/InputLoader.bs.js");
var StackSafeFuture$Aoc19 = require("./lib/StackSafeFuture.bs.js");
var Relude_List_Specializations = require("relude/src/list/Relude_List_Specializations.bs.js");

function spriteToDisplay(param) {
  switch (param) {
    case /* Empty */0 :
        return ".";
    case /* Scaffold */1 :
        return "#";
    case /* UpRobot */2 :
        return "^";
    case /* DownRobot */3 :
        return "v";
    case /* LeftRobot */4 :
        return "<";
    case /* RightRobot */5 :
        return ">";
    case /* FuckedRobot */6 :
        return "X";
    
  }
}

var draw = Coord$Aoc19.CoordMap.set;

var make = Coord$Aoc19.CoordMap.make;

function get(c, s) {
  return Curry._3(Coord$Aoc19.CoordMap.getOrElse, c, /* Empty */0, s);
}

var getCoords = Coord$Aoc19.CoordMap.keys;

function findUpRobot(screen) {
  return Relude_Option.getOrThrow(Relude_List.head(Curry._1(Coord$Aoc19.CoordMap.keys, Curry._2(Coord$Aoc19.CoordMap.filter, (function (param, v) {
                            return v === /* UpRobot */2;
                          }), screen))));
}

function output(param) {
  return Coord$Aoc19.output(spriteToDisplay, /* Empty */0, param);
}

var $$Screen = {
  draw: draw,
  make: make,
  get: get,
  getCoords: getCoords,
  findUpRobot: findUpRobot,
  output: output
};

var InvalidCodePoint = Caml_exceptions.create("Day17-Aoc19.Camera.InvalidCodePoint");

function takeImage(input) {
  var position = {
    contents: [
      0,
      0
    ]
  };
  var screen = {
    contents: Curry._1(make, undefined)
  };
  var nextOutput = function (string) {
    var v = Caml_format.caml_int_of_string(string);
    var match = position.contents;
    var y = match[1];
    var nextSprite;
    if (v >= 60) {
      if (v >= 94) {
        if (v !== 118) {
          if (v >= 95) {
            throw {
                  RE_EXN_ID: InvalidCodePoint,
                  _1: v,
                  Error: new Error()
                };
          }
          nextSprite = /* UpRobot */2;
        } else {
          nextSprite = /* DownRobot */3;
        }
      } else if (v !== 88) {
        if (v >= 63) {
          throw {
                RE_EXN_ID: InvalidCodePoint,
                _1: v,
                Error: new Error()
              };
        }
        switch (v - 60 | 0) {
          case 0 :
              nextSprite = /* LeftRobot */4;
              break;
          case 1 :
              throw {
                    RE_EXN_ID: InvalidCodePoint,
                    _1: v,
                    Error: new Error()
                  };
          case 2 :
              nextSprite = /* RightRobot */5;
              break;
          
        }
      } else {
        nextSprite = /* FuckedRobot */6;
      }
    } else if (v !== 10) {
      if (v !== 35) {
        if (v !== 46) {
          throw {
                RE_EXN_ID: InvalidCodePoint,
                _1: v,
                Error: new Error()
              };
        }
        nextSprite = /* Empty */0;
      } else {
        nextSprite = /* Scaffold */1;
      }
    } else {
      nextSprite = undefined;
    }
    if (nextSprite !== undefined) {
      screen.contents = Curry._3(draw, position.contents, nextSprite, screen.contents);
      position.contents = [
        match[0] + 1 | 0,
        y
      ];
    } else {
      position.contents = [
        0,
        y + 1 | 0
      ];
    }
    
  };
  return StackSafeFuture$Aoc19.map((function (param) {
                return screen.contents;
              }), Intcode$Aoc19.run(undefined, nextOutput, input));
}

var Camera = {
  InvalidCodePoint: InvalidCodePoint,
  takeImage: takeImage
};

var NoMoreInput = Caml_exceptions.create("Day17-Aoc19.Robot.NoMoreInput");

function run(param, input) {
  var input$1 = Relude_List.replaceAt(0, "2", input);
  var fullInput = Curry._1(Relude_List.flatten, Relude_List.intersperse({
            hd: /* "\n" */10,
            tl: /* [] */0
          }, {
            hd: param[0],
            tl: {
              hd: param[1],
              tl: {
                hd: param[2],
                tl: {
                  hd: param[3],
                  tl: {
                    hd: {
                      hd: /* "n" */110,
                      tl: /* [] */0
                    },
                    tl: {
                      hd: /* [] */0,
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          }));
  var remainingInput = {
    contents: fullInput
  };
  var out = {
    contents: 0
  };
  var nextOutput = function (s) {
    out.contents = Caml_format.caml_int_of_string(s);
    
  };
  var nextInput = function (param) {
    var match = remainingInput.contents;
    if (match) {
      remainingInput.contents = match.tl;
      return StackSafeFuture$Aoc19.pure(String(match.hd));
    }
    throw {
          RE_EXN_ID: NoMoreInput,
          Error: new Error()
        };
  };
  return StackSafeFuture$Aoc19.map((function (param) {
                return out.contents;
              }), Intcode$Aoc19.run(nextInput, nextOutput, input$1));
}

var Robot = {
  NoMoreInput: NoMoreInput,
  run: run
};

function findIntersection(screen) {
  var coords = Curry._1(getCoords, screen);
  return Relude_List.filter((function (c) {
                var y = c[1];
                var x = c[0];
                if (get(c, screen) !== /* Scaffold */1) {
                  return false;
                }
                var u = get([
                      x,
                      y - 1 | 0
                    ], screen);
                var d = get([
                      x,
                      y + 1 | 0
                    ], screen);
                var l = get([
                      x - 1 | 0,
                      y
                    ], screen);
                var r = get([
                      x + 1 | 0,
                      y
                    ], screen);
                if (u === /* Scaffold */1 && d === /* Scaffold */1 && l === /* Scaffold */1) {
                  return r === /* Scaffold */1;
                } else {
                  return false;
                }
              }), coords);
}

var partial_arg = Relude_List.map(function (param) {
      return Math.imul(param[0], param[1]);
    });

function intersectionsToAlignmentParameters(param) {
  return Relude_Function.flipCompose(partial_arg, Relude_List_Specializations.Int.sum, param);
}

function nextPosition(param, param$1) {
  var y = param[1];
  var x = param[0];
  switch (param$1) {
    case /* Up */0 :
        return [
                x,
                y - 1 | 0
              ];
    case /* Down */1 :
        return [
                x,
                y + 1 | 0
              ];
    case /* Left */2 :
        return [
                x - 1 | 0,
                y
              ];
    case /* Right */3 :
        return [
                x + 1 | 0,
                y
              ];
    
  }
}

function leftRight(param) {
  switch (param) {
    case /* Up */0 :
        return [
                /* Left */2,
                /* Right */3
              ];
    case /* Down */1 :
        return [
                /* Right */3,
                /* Left */2
              ];
    case /* Left */2 :
        return [
                /* Down */1,
                /* Up */0
              ];
    case /* Right */3 :
        return [
                /* Up */0,
                /* Down */1
              ];
    
  }
}

var InvalidState = Caml_exceptions.create("Day17-Aoc19.InvalidState");

function findPath(screen) {
  var start = findUpRobot(screen);
  var _pos = start;
  var _direction = /* Up */0;
  var _instructions = /* [] */0;
  while(true) {
    var instructions = _instructions;
    var direction = _direction;
    var pos = _pos;
    var np = nextPosition(pos, direction);
    var match = leftRight(direction);
    var r = match[1];
    var l = match[0];
    var lp = nextPosition(pos, l);
    var rp = nextPosition(pos, r);
    if (get(np, screen) === /* Scaffold */1) {
      if (instructions) {
        var match$1 = instructions.hd;
        _instructions = {
          hd: [
            match$1[0],
            match$1[1] + 1 | 0
          ],
          tl: instructions.tl
        };
        _pos = np;
        continue ;
      }
      throw {
            RE_EXN_ID: InvalidState,
            Error: new Error()
          };
    }
    if (get(lp, screen) === /* Scaffold */1) {
      _instructions = {
        hd: [
          /* LeftTurn */0,
          1
        ],
        tl: instructions
      };
      _direction = l;
      _pos = lp;
      continue ;
    }
    if (get(rp, screen) !== /* Scaffold */1) {
      return Relude_List.reverse(instructions);
    }
    _instructions = {
      hd: [
        /* RightTurn */1,
        1
      ],
      tl: instructions
    };
    _direction = r;
    _pos = rp;
    continue ;
  };
}

function xToChars(render, path) {
  return Relude_List.map(function (s) {
                return Caml_string.get(s, 0);
              })(Curry._1(Relude_List.flatten, Relude_List.intersperse({
                      hd: ",",
                      tl: /* [] */0
                    }, Relude_List.map(render)(path))));
}

function pathToChars(param) {
  return xToChars((function (param) {
                return {
                        hd: param[0] ? "R" : "L",
                        tl: {
                          hd: ",",
                          tl: Relude_String.splitList("", String(param[1]))
                        }
                      };
              }), param);
}

var charsToAscii = Relude_List.map(function (prim) {
      return prim;
    });

function compressedToChars(param) {
  return xToChars((function (c) {
                if (c.TAG === /* Raw */0) {
                  return {
                          hd: c._0 ? "R" : "L",
                          tl: {
                            hd: ",",
                            tl: Relude_String.splitList("", String(c._1))
                          }
                        };
                } else {
                  return {
                          hd: Char.escaped(c._0),
                          tl: /* [] */0
                        };
                }
              }), param);
}

function replace(ident, sub, path) {
  var _subRemaining = sub;
  var _removed = /* [] */0;
  var _remaining = path;
  var _replaced = /* [] */0;
  while(true) {
    var replaced = _replaced;
    var remaining = _remaining;
    var removed = _removed;
    var subRemaining = _subRemaining;
    var exit = 0;
    if (subRemaining) {
      if (remaining) {
        var r = remaining.hd;
        var match = subRemaining.hd;
        if (r.TAG === /* Raw */0) {
          if (match[0] === r._0 && match[1] === r._1) {
            _remaining = remaining.tl;
            _removed = {
              hd: r,
              tl: removed
            };
            _subRemaining = subRemaining.tl;
            continue ;
          }
          exit = 2;
        } else {
          exit = 2;
        }
      } else {
        exit = 2;
      }
    } else {
      _replaced = {
        hd: {
          TAG: /* Compressed */1,
          _0: ident
        },
        tl: replaced
      };
      _removed = /* [] */0;
      _subRemaining = sub;
      continue ;
    }
    if (exit === 2) {
      if (remaining) {
        var n = remaining.hd;
        if (n.TAG === /* Raw */0) {
          if (removed === /* [] */0) {
            _replaced = {
              hd: n,
              tl: replaced
            };
            _remaining = remaining.tl;
            _removed = /* [] */0;
            _subRemaining = sub;
            continue ;
          }
          
        } else {
          var replaced_1 = Relude_List.concat(removed, replaced);
          var replaced$1 = {
            hd: n,
            tl: replaced_1
          };
          _replaced = replaced$1;
          _remaining = remaining.tl;
          _removed = /* [] */0;
          _subRemaining = sub;
          continue ;
        }
      } else if (removed === /* [] */0) {
        return Relude_List.reverse(replaced);
      }
      
    }
    var replaced$2 = Relude_List.concat(removed, replaced);
    _replaced = replaced$2;
    _removed = /* [] */0;
    _subRemaining = sub;
    continue ;
  };
}

function compress(path) {
  var pathLength = Curry._1(Relude_List.length, path);
  var lengths = Relude_List.makeWithIndex(5, (function (param) {
          return 1 + param | 0;
        }));
  var attempts = Relude_List.filter((function (param) {
          if (Curry._1(Relude_List.length, pathToChars(param[0])) <= 20) {
            return Curry._1(Relude_List.length, pathToChars(param[1])) <= 20;
          } else {
            return false;
          }
        }), Curry._3(Relude_List.map2, (function (s, e) {
              var start = Relude_List.take(s, path);
              var end_ = Relude_List.drop(pathLength - e | 0, path);
              return [
                      start,
                      end_
                    ];
            }), lengths, lengths));
  var raw = Relude_List.map(function (param) {
          return {
                  TAG: /* Raw */0,
                  _0: param[0],
                  _1: param[1]
                };
        })(path);
  var compressed = Relude_List.map(function (param) {
          var e = param[1];
          var s = param[0];
          var a = replace(/* "A" */65, s, raw);
          var b = replace(/* "B" */66, e, a);
          var remaining = Relude_List.mapOption((function (param) {
                  if (param.TAG === /* Raw */0) {
                    return [
                            param._0,
                            param._1
                          ];
                  }
                  
                }), Relude_List.takeWhile((function (param) {
                      if (param.TAG === /* Raw */0) {
                        return true;
                      } else {
                        return false;
                      }
                    }), Relude_List.dropWhile((function (param) {
                          if (param.TAG === /* Raw */0) {
                            return false;
                          } else {
                            return true;
                          }
                        }), b)));
          var c = replace(/* "C" */67, remaining, b);
          return [
                  s,
                  e,
                  remaining,
                  c
                ];
        })(attempts);
  var valid = Relude_List.filter((function (param) {
          return Curry._2(Relude_List.all, (function (param) {
                        if (param.TAG === /* Raw */0) {
                          return false;
                        } else {
                          return true;
                        }
                      }), param[3]);
        }), compressed);
  var match = Relude_Option.getOrThrow(Relude_List.head(valid));
  return [
          charsToAscii(compressedToChars(match[3])),
          charsToAscii(pathToChars(match[0])),
          charsToAscii(pathToChars(match[1])),
          charsToAscii(pathToChars(match[2]))
        ];
}

var input = InputLoader$Aoc19.commaSeparated(17);

var image = Curry._2(StackSafeFuture$Aoc19.flatMap, takeImage, input);

StackSafeFuture$Aoc19.tap(function (param) {
        return Relude_Function.flipCompose((function (param) {
                      return Relude_Function.flipCompose(findIntersection, intersectionsToAlignmentParameters, param);
                    }), (function (param) {
                      console.log("sum of the alignment parameters", param);
                      
                    }), param);
      })(image);

var compressed = StackSafeFuture$Aoc19.map((function (param) {
        return Relude_Function.flipCompose(findPath, compress, param);
      }), image);

StackSafeFuture$Aoc19.tap(function (param) {
        console.log("Dust", param);
        
      })(Curry._1(StackSafeFuture$Aoc19.flatten, Curry._3(StackSafeFuture$Aoc19.map2, (function (i, c) {
                return run(c, i);
              }), input, compressed)));

var CoordMap;

var $great$great = Relude_Function.flipCompose;

exports.CoordMap = CoordMap;
exports.$great$great = $great$great;
exports.spriteToDisplay = spriteToDisplay;
exports.$$Screen = $$Screen;
exports.Camera = Camera;
exports.Robot = Robot;
exports.findIntersection = findIntersection;
exports.intersectionsToAlignmentParameters = intersectionsToAlignmentParameters;
exports.nextPosition = nextPosition;
exports.leftRight = leftRight;
exports.InvalidState = InvalidState;
exports.findPath = findPath;
exports.xToChars = xToChars;
exports.pathToChars = pathToChars;
exports.charsToAscii = charsToAscii;
exports.compressedToChars = compressedToChars;
exports.replace = replace;
exports.compress = compress;
exports.input = input;
exports.image = image;
exports.compressed = compressed;
/* partial_arg Not a pure module */
