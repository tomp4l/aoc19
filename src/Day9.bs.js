// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Intcode$Aoc19 = require("./Intcode.bs.js");
var InputLoader$Aoc19 = require("./lib/InputLoader.bs.js");
var StackSafeFuture$Aoc19 = require("./lib/StackSafeFuture.bs.js");

var input = InputLoader$Aoc19.commaSeparated(9);

var partial_arg = (function (param) {
    return StackSafeFuture$Aoc19.pure("1");
  });

StackSafeFuture$Aoc19.map((function (eta) {
        var param;
        return Intcode$Aoc19.run(partial_arg, param, eta);
      }), input);

var partial_arg$1 = (function (param) {
    return StackSafeFuture$Aoc19.pure("2");
  });

StackSafeFuture$Aoc19.map((function (eta) {
        var param;
        return Intcode$Aoc19.run(partial_arg$1, param, eta);
      }), input);

exports.input = input;
/* input Not a pure module */
