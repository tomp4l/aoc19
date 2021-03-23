// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Relude_Int = require("relude/src/Relude_Int.bs.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Relude_List = require("relude/src/Relude_List.bs.js");
var Relude_Array = require("relude/src/Relude_Array.bs.js");
var Intcode$Aoc19 = require("./Intcode.bs.js");
var Relude_Function = require("relude/src/Relude_Function.bs.js");
var InputLoader$Aoc19 = require("./lib/InputLoader.bs.js");
var StackSafeFuture$Aoc19 = require("./lib/StackSafeFuture.bs.js");

function nextInput(halt, sendNet, i, nextY, inputs, idleNetworks, lastNatY, param) {
  if (halt.contents) {
    return StackSafeFuture$Aoc19.never(undefined);
  }
  if (sendNet.contents) {
    sendNet.contents = false;
    return StackSafeFuture$Aoc19.pure(String(i));
  }
  var y = nextY.contents;
  if (y !== undefined) {
    nextY.contents = undefined;
    return StackSafeFuture$Aoc19.pure(y);
  }
  var nextInput$1 = Curry._2(Relude_Int.$$Map.get, i, inputs.contents);
  if (nextInput$1 !== undefined && nextInput$1) {
    var match = nextInput$1.tl;
    if (match) {
      Relude_Array.setAt(i, false, idleNetworks);
      nextY.contents = match.hd;
      inputs.contents = Curry._3(Relude_Int.$$Map.set, i, match.tl, inputs.contents);
      return StackSafeFuture$Aoc19.pure(nextInput$1.hd);
    }
    
  }
  if (i === 0 && Curry._2(Relude_Array.all, Relude_Function.identity, idleNetworks)) {
    var nextInput$2 = Curry._2(Relude_Int.$$Map.get, 255, inputs.contents);
    if (nextInput$2 === undefined) {
      return StackSafeFuture$Aoc19.pure("-1");
    }
    if (!nextInput$2) {
      return StackSafeFuture$Aoc19.pure("-1");
    }
    var match$1 = nextInput$2.tl;
    if (!match$1) {
      return StackSafeFuture$Aoc19.pure("-1");
    }
    var y$1 = match$1.hd;
    Relude_Array.setAt(i, false, idleNetworks);
    nextY.contents = y$1;
    inputs.contents = Curry._3(Relude_Int.$$Map.set, 255, /* [] */0, inputs.contents);
    if (Caml_obj.caml_equal(lastNatY.contents, y$1)) {
      halt.contents = true;
      console.log([
            "double nat",
            y$1
          ]);
    }
    lastNatY.contents = y$1;
    return StackSafeFuture$Aoc19.pure(nextInput$2.hd);
  }
  Relude_Array.setAt(i, true, idleNetworks);
  return StackSafeFuture$Aoc19.pure("-1");
}

function nextOutput(currentOut, inputs, xOut, useNat, halt, o) {
  var recipient = currentOut.contents;
  if (recipient === undefined) {
    currentOut.contents = Caml_format.caml_int_of_string(o);
    return ;
  }
  var is = Curry._3(Relude_Int.$$Map.getOrElse, recipient, /* [] */0, inputs.contents);
  var newIs = Relude_List.append(o, is);
  inputs.contents = Curry._3(Relude_Int.$$Map.set, recipient, newIs, inputs.contents);
  if (xOut.contents) {
    xOut.contents = false;
    return ;
  }
  if (recipient === 255) {
    if (useNat) {
      var ow = Relude_List.drop(Curry._1(Relude_List.length, newIs) - 2 | 0, newIs);
      inputs.contents = Curry._3(Relude_Int.$$Map.set, recipient, ow, inputs.contents);
    } else {
      halt.contents = true;
      console.log([
            "255 sent",
            o
          ]);
    }
  }
  xOut.contents = true;
  currentOut.contents = undefined;
  
}

function runNetwork(useNat, intCode) {
  var idleNetworks = Relude_Array.makeWithIndex(50, (function (param) {
          return Relude_Function.$$const(true, param);
        }));
  var inputs = {
    contents: Curry._1(Relude_Int.$$Map.make, undefined)
  };
  var halt = {
    contents: false
  };
  var _i = 0;
  while(true) {
    var i = _i;
    if (i === 50) {
      return ;
    }
    var nextY = {
      contents: undefined
    };
    var currentOut = {
      contents: undefined
    };
    var xOut = {
      contents: true
    };
    var sendNet = {
      contents: true
    };
    var lastNatY = {
      contents: undefined
    };
    Intcode$Aoc19.run((function(i,nextY,sendNet,lastNatY){
        return function (param) {
          return nextInput(halt, sendNet, i, nextY, inputs, idleNetworks, lastNatY, param);
        }
        }(i,nextY,sendNet,lastNatY)), (function(currentOut,xOut){
        return function (param) {
          return nextOutput(currentOut, inputs, xOut, useNat, halt, param);
        }
        }(currentOut,xOut)), intCode);
    _i = i + 1 | 0;
    continue ;
  };
}

var Network = {
  nextInput: nextInput,
  nextOutput: nextOutput,
  runNetwork: runNetwork
};

var input = InputLoader$Aoc19.commaSeparated(23);

StackSafeFuture$Aoc19.tap(function (param) {
        return runNetwork(false, param);
      })(input);

StackSafeFuture$Aoc19.tap(function (param) {
        return runNetwork(true, param);
      })(input);

var $great$great = Relude_Function.flipCompose;

exports.$great$great = $great$great;
exports.Network = Network;
exports.input = input;
/* input Not a pure module */
