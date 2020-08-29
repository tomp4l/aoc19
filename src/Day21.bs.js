// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Char = require("bs-platform/lib/js/char.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Intcode$Aoc19 = require("./Intcode.bs.js");
var Relude_String = require("relude/src/Relude_String.bs.js");
var InputLoader$Aoc19 = require("./lib/InputLoader.bs.js");
var StackSafeFuture$Aoc19 = require("./lib/StackSafeFuture.bs.js");

function output(s) {
  var i = Caml_format.caml_int_of_string(s);
  if (i < 256) {
    process.stdout.write($$String.make(1, Char.chr(i)));
  } else {
    console.log(i);
  }
  
}

function assessHull(code, intcode) {
  var remainingCode = {
    contents: Relude_String.splitAsList("", code)
  };
  var nextInput = function (param) {
    var match = remainingCode.contents;
    if (match) {
      remainingCode.contents = match.tl;
      return StackSafeFuture$Aoc19.pure(String(Caml_string.get(match.hd, 0)));
    } else {
      return StackSafeFuture$Aoc19.pure("10");
    }
  };
  return Intcode$Aoc19.run(nextInput, output, intcode);
}

var SpringDroid = {
  output: output,
  assessHull: assessHull
};

var input = InputLoader$Aoc19.commaSeparated(21);

var partial_arg = "OR D J\nNOT C T\nAND T J\nNOT A T\nOR T J\nWALK";

StackSafeFuture$Aoc19.tap(function (prim) {
        console.log(prim);
        
      })(Curry._2(StackSafeFuture$Aoc19.flatMap, (function (param) {
            return assessHull(partial_arg, param);
          }), input));

var partial_arg$1 = "OR D J\nAND H J\nOR B T\nAND C T\nNOT T T\nAND T J\nNOT A T\nOR T J\nRUN";

StackSafeFuture$Aoc19.tap(function (prim) {
        console.log(prim);
        
      })(Curry._2(StackSafeFuture$Aoc19.flatMap, (function (param) {
            return assessHull(partial_arg$1, param);
          }), input));

exports.SpringDroid = SpringDroid;
exports.input = input;
/* input Not a pure module */