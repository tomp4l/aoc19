// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Future = require("reason-future/src/Future.bs.js");
var Intcode$Aoc19 = require("./Intcode.bs.js");
var Relude_Function = require("relude/src/Relude_Function.bs.js");
var InputLoader$Aoc19 = require("./InputLoader.bs.js");

var $great$great = Relude_Function.Infix.$great$great;

var input = InputLoader$Aoc19.commaSeparatedInts(5);

Future.map(input, Intcode$Aoc19.run);

exports.$great$great = $great$great;
exports.input = input;
/* input Not a pure module */
