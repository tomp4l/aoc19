// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Readline = require("readline");
var StackSafeFuture$Aoc19 = require("./StackSafeFuture.bs.js");

var defaultInput = {
  input: process.stdin,
  output: process.stdout
};

function make(param) {
  return Readline.createInterface(defaultInput);
}

function question(readline, query) {
  return StackSafeFuture$Aoc19.make(function (param) {
              readline.question(query, param);
              
            });
}

exports.defaultInput = defaultInput;
exports.make = make;
exports.question = question;
/* defaultInput Not a pure module */
