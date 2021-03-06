// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Int64 = require("bs-platform/lib/js/int64.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_int64 = require("bs-platform/lib/js/caml_int64.js");
var Relude_Map = require("relude/src/Relude_Map.bs.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Relude_List = require("relude/src/Relude_List.bs.js");
var Relude_Option = require("relude/src/Relude_Option.bs.js");
var Relude_String = require("relude/src/Relude_String.bs.js");
var Readline$Aoc19 = require("./lib/Readline.bs.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Relude_Function = require("relude/src/Relude_Function.bs.js");
var Relude_Ordering = require("relude/src/Relude_Ordering.bs.js");
var StackSafeFuture$Aoc19 = require("./lib/StackSafeFuture.bs.js");

function compare(a, b) {
  return Relude_Ordering.fromInt(Int64.compare(a, b));
}

var eq = Caml_obj.caml_equal;

var $$Map = Relude_Map.WithOrd({
      eq: eq,
      compare: compare
    });

function fromList(list) {
  return Relude_List.foldLeft((function (map, param) {
                  return Curry._3($$Map.set, Caml_int64.of_int32(param[1]), param[0], map);
                }), Curry._1($$Map.make, undefined))(Relude_List.zipWithIndex(list));
}

function get(i) {
  return Curry._2(Relude_Function.flipCompose, Curry._1($$Map.get, i), (function (param) {
                return Relude_Option.getOrElse("0", param);
              }));
}

var set = $$Map.set;

function getFromMemory(m) {
  return Curry._2(Relude_Function.flipCompose, Curry._1($$Map.get, Caml_format.caml_int64_of_string(m)), (function (param) {
                return Relude_Option.getOrElse("0", param);
              }));
}

var UnknownMode = Caml_exceptions.create("Intcode-Aoc19.ComputerState.UnknownMode");

var asNumber = Caml_format.caml_int64_of_string;

function fromList$1(list) {
  return {
          memory: fromList(list),
          pointer: Caml_int64.zero,
          relativeBase: Caml_int64.zero
        };
}

function get$1(param) {
  var memory = param.memory;
  var pointer = param.pointer;
  return Curry._1(get(pointer), memory);
}

function increment(state) {
  state.pointer = Caml_int64.add(state.pointer, Caml_int64.one);
  
}

function incrementAndGet(state) {
  var v = get$1(state);
  increment(state);
  return v;
}

function incrementAndGetWithMode(mode, state) {
  var v = incrementAndGet(state);
  switch (mode) {
    case "0" :
        return Curry._1(getFromMemory(v), state.memory);
    case "1" :
        return v;
    case "2" :
        var memory = state.memory;
        var relativeBase = state.relativeBase;
        return Curry._1(get(Caml_int64.add(Caml_format.caml_int64_of_string(v), relativeBase)), memory);
    default:
      throw {
            RE_EXN_ID: UnknownMode,
            Error: new Error()
          };
  }
}

function incrementAndSetWithMode(mode, value, state) {
  var key = Caml_format.caml_int64_of_string(incrementAndGet(state));
  var cell;
  switch (mode) {
    case "0" :
        cell = key;
        break;
    case "2" :
        cell = Caml_int64.add(key, state.relativeBase);
        break;
    default:
      throw {
            RE_EXN_ID: UnknownMode,
            Error: new Error()
          };
  }
  state.memory = Curry._3(set, cell, value, state.memory);
  
}

function doOp3(op, param, state) {
  var m2 = param[1];
  var m1 = param[0];
  var l = Relude_Function.flipCompose((function (param) {
          return incrementAndGetWithMode(m1, param);
        }), asNumber, state);
  var r = Relude_Function.flipCompose((function (param) {
          return incrementAndGetWithMode(m2, param);
        }), asNumber, state);
  var value = Int64.to_string(Curry._2(op, l, r));
  incrementAndSetWithMode(param[2], value, state);
  return /* Continue */1;
}

function halt(param) {
  return /* Halt */0;
}

function jumpIf(param, nonZero, state) {
  var m2 = param[1];
  var v = incrementAndGetWithMode(param[0], state);
  var p = Relude_Function.flipCompose((function (param) {
          return incrementAndGetWithMode(m2, param);
        }), asNumber, state);
  if (nonZero) {
    if (v !== "0") {
      state.pointer = p;
    }
    
  } else if (v === "0") {
    state.pointer = p;
  }
  return /* Continue */1;
}

function defaultNextInput(param) {
  var readline = Readline$Aoc19.make(undefined);
  return StackSafeFuture$Aoc19.tap(function (param) {
                readline.close();
                
              })(Readline$Aoc19.question(readline, "input dear human\n"));
}

function defaultNextOutput(param) {
  console.log("Output: ", param);
  
}

function run(nextInputOpt, nextOutputOpt, intcode) {
  var nextInput = nextInputOpt !== undefined ? nextInputOpt : defaultNextInput;
  var nextOutput = nextOutputOpt !== undefined ? nextOutputOpt : defaultNextOutput;
  var state = fromList$1(intcode);
  var program = function (_param) {
    while(true) {
      var op = incrementAndGet(state);
      var padded = Relude_String.repeat(4, "0") + op;
      var split = Relude_List.reverse(Relude_String.splitList("", padded));
      var nextOp;
      var exit = 0;
      if (split) {
        switch (split.hd) {
          case "1" :
              var match = split.tl;
              if (match && match.hd === "0") {
                var match$1 = match.tl;
                if (match$1) {
                  var match$2 = match$1.tl;
                  if (match$2) {
                    var match$3 = match$2.tl;
                    if (match$3) {
                      var partial_arg_0 = match$1.hd;
                      var partial_arg_1 = match$2.hd;
                      var partial_arg_2 = match$3.hd;
                      var partial_arg = [
                        partial_arg_0,
                        partial_arg_1,
                        partial_arg_2
                      ];
                      nextOp = (function(partial_arg){
                      return function (param) {
                        return doOp3(Caml_int64.add, partial_arg, param);
                      }
                      }(partial_arg));
                    } else {
                      exit = 1;
                    }
                  } else {
                    exit = 1;
                  }
                } else {
                  exit = 1;
                }
              } else {
                exit = 1;
              }
              break;
          case "2" :
              var match$4 = split.tl;
              if (match$4 && match$4.hd === "0") {
                var match$5 = match$4.tl;
                if (match$5) {
                  var match$6 = match$5.tl;
                  if (match$6) {
                    var match$7 = match$6.tl;
                    if (match$7) {
                      var partial_arg_0$1 = match$5.hd;
                      var partial_arg_1$1 = match$6.hd;
                      var partial_arg_2$1 = match$7.hd;
                      var partial_arg$1 = [
                        partial_arg_0$1,
                        partial_arg_1$1,
                        partial_arg_2$1
                      ];
                      nextOp = (function(partial_arg$1){
                      return function (param) {
                        return doOp3(Caml_int64.mul, partial_arg$1, param);
                      }
                      }(partial_arg$1));
                    } else {
                      exit = 1;
                    }
                  } else {
                    exit = 1;
                  }
                } else {
                  exit = 1;
                }
              } else {
                exit = 1;
              }
              break;
          case "3" :
              var match$8 = split.tl;
              if (match$8 && match$8.hd === "0") {
                var match$9 = match$8.tl;
                if (match$9) {
                  var mode = match$9.hd;
                  nextOp = (function(mode){
                  return function (param) {
                    var input = Curry._1(nextInput, undefined);
                    var processed = StackSafeFuture$Aoc19.map((function (v) {
                            return incrementAndSetWithMode(mode, v, param);
                          }), input);
                    return /* AwaitInput */{
                            _0: processed
                          };
                  }
                  }(mode));
                } else {
                  exit = 1;
                }
              } else {
                exit = 1;
              }
              break;
          case "4" :
              var match$10 = split.tl;
              if (match$10 && match$10.hd === "0") {
                var match$11 = match$10.tl;
                if (match$11) {
                  var mode$1 = match$11.hd;
                  nextOp = (function(mode$1){
                  return function (param) {
                    var v = incrementAndGetWithMode(mode$1, param);
                    Curry._1(nextOutput, v);
                    return /* Continue */1;
                  }
                  }(mode$1));
                } else {
                  exit = 1;
                }
              } else {
                exit = 1;
              }
              break;
          case "5" :
              var match$12 = split.tl;
              if (match$12 && match$12.hd === "0") {
                var match$13 = match$12.tl;
                if (match$13) {
                  var match$14 = match$13.tl;
                  if (match$14) {
                    var partial_arg_0$2 = match$13.hd;
                    var partial_arg_1$2 = match$14.hd;
                    var partial_arg$2 = [
                      partial_arg_0$2,
                      partial_arg_1$2
                    ];
                    nextOp = (function(partial_arg$2){
                    return function (param) {
                      return jumpIf(partial_arg$2, true, param);
                    }
                    }(partial_arg$2));
                  } else {
                    exit = 1;
                  }
                } else {
                  exit = 1;
                }
              } else {
                exit = 1;
              }
              break;
          case "6" :
              var match$15 = split.tl;
              if (match$15 && match$15.hd === "0") {
                var match$16 = match$15.tl;
                if (match$16) {
                  var match$17 = match$16.tl;
                  if (match$17) {
                    var partial_arg_0$3 = match$16.hd;
                    var partial_arg_1$3 = match$17.hd;
                    var partial_arg$3 = [
                      partial_arg_0$3,
                      partial_arg_1$3
                    ];
                    nextOp = (function(partial_arg$3){
                    return function (param) {
                      return jumpIf(partial_arg$3, false, param);
                    }
                    }(partial_arg$3));
                  } else {
                    exit = 1;
                  }
                } else {
                  exit = 1;
                }
              } else {
                exit = 1;
              }
              break;
          case "7" :
              var match$18 = split.tl;
              if (match$18 && match$18.hd === "0") {
                var match$19 = match$18.tl;
                if (match$19) {
                  var match$20 = match$19.tl;
                  if (match$20) {
                    var match$21 = match$20.tl;
                    if (match$21) {
                      var partial_arg_0$4 = match$19.hd;
                      var partial_arg_1$4 = match$20.hd;
                      var partial_arg_2$2 = match$21.hd;
                      var partial_arg$4 = [
                        partial_arg_0$4,
                        partial_arg_1$4,
                        partial_arg_2$2
                      ];
                      nextOp = (function(partial_arg$4){
                      return function (param) {
                        return doOp3((function (a, b) {
                                      if (Int64.compare(a, b) < 0) {
                                        return Caml_int64.one;
                                      } else {
                                        return Caml_int64.zero;
                                      }
                                    }), partial_arg$4, param);
                      }
                      }(partial_arg$4));
                    } else {
                      exit = 1;
                    }
                  } else {
                    exit = 1;
                  }
                } else {
                  exit = 1;
                }
              } else {
                exit = 1;
              }
              break;
          case "8" :
              var match$22 = split.tl;
              if (match$22 && match$22.hd === "0") {
                var match$23 = match$22.tl;
                if (match$23) {
                  var match$24 = match$23.tl;
                  if (match$24) {
                    var match$25 = match$24.tl;
                    if (match$25) {
                      var partial_arg_0$5 = match$23.hd;
                      var partial_arg_1$5 = match$24.hd;
                      var partial_arg_2$3 = match$25.hd;
                      var partial_arg$5 = [
                        partial_arg_0$5,
                        partial_arg_1$5,
                        partial_arg_2$3
                      ];
                      nextOp = (function(partial_arg$5){
                      return function (param) {
                        return doOp3((function (a, b) {
                                      if (Caml_int64.eq(a, b)) {
                                        return Caml_int64.one;
                                      } else {
                                        return Caml_int64.zero;
                                      }
                                    }), partial_arg$5, param);
                      }
                      }(partial_arg$5));
                    } else {
                      exit = 1;
                    }
                  } else {
                    exit = 1;
                  }
                } else {
                  exit = 1;
                }
              } else {
                exit = 1;
              }
              break;
          case "9" :
              var match$26 = split.tl;
              if (match$26) {
                switch (match$26.hd) {
                  case "0" :
                      var match$27 = match$26.tl;
                      if (match$27) {
                        var partial_arg$6 = match$27.hd;
                        nextOp = (function(partial_arg$6){
                        return function (param) {
                          var v = Relude_Function.flipCompose((function (param) {
                                  return incrementAndGetWithMode(partial_arg$6, param);
                                }), asNumber, param);
                          param.relativeBase = Caml_int64.add(param.relativeBase, v);
                          return /* Continue */1;
                        }
                        }(partial_arg$6));
                      } else {
                        exit = 1;
                      }
                      break;
                  case "9" :
                      nextOp = halt;
                      break;
                  default:
                    exit = 1;
                }
              } else {
                exit = 1;
              }
              break;
          default:
            exit = 1;
        }
      } else {
        exit = 1;
      }
      if (exit === 1) {
        console.error("Unknown op", op);
        nextOp = halt;
      }
      var $$continue = Curry._1(nextOp, state);
      if (typeof $$continue !== "number") {
        return Curry._2(StackSafeFuture$Aoc19.flatMap, program, $$continue._0);
      }
      if ($$continue === 0) {
        return StackSafeFuture$Aoc19.pure(undefined);
      }
      _param = undefined;
      continue ;
    };
  };
  var done_ = program(undefined);
  return StackSafeFuture$Aoc19.map((function (param) {
                state.pointer = Caml_int64.zero;
                return get$1(state);
              }), done_);
}

exports.run = run;
/* Map Not a pure module */
