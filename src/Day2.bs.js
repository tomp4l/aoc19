// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Future = require("reason-future/src/Future.bs.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Relude_List = require("relude/src/Relude_List.bs.js");
var Intcode$Aoc19 = require("./Intcode.bs.js");
var Relude_Function = require("relude/src/Relude_Function.bs.js");
var InputLoader$Aoc19 = require("./lib/InputLoader.bs.js");

function runWithInput(i, a, b) {
  return Intcode$Aoc19.run(Relude_List.replaceAt(2, b, Relude_List.replaceAt(1, a, i)));
}

var viableInput = Relude_List.makeWithIndex(100, Relude_Function.identity);

function findInput(input, desired) {
  var search = function (_nouns, _verbs, _acc) {
    while(true) {
      var acc = _acc;
      var verbs = _verbs;
      var nouns = _nouns;
      if (nouns) {
        if (verbs) {
          var verb = verbs[0];
          var noun = nouns[0];
          var futureInput = Future.map2(runWithInput(input, noun, verb), Future.value(Caml_int32.imul(noun, 100) + verb | 0), (function (a, b) {
                  return /* tuple */[
                          a,
                          b
                        ];
                }));
          _acc = /* :: */[
            futureInput,
            acc
          ];
          _verbs = /* :: */[
            verb,
            verbs[1]
          ];
          _nouns = nouns[1];
          continue ;
        } else {
          return acc;
        }
      } else if (verbs) {
        _verbs = verbs[1];
        _nouns = viableInput;
        continue ;
      } else {
        return acc;
      }
    };
  };
  return Relude_List.foldLeft((function (acc, f) {
                  return Future.flatMap(acc, (function (acc) {
                                return Future.map(f, (function (param) {
                                              var a = param[0];
                                              if (a !== undefined && a === desired) {
                                                return param[1];
                                              } else {
                                                return acc;
                                              }
                                            }));
                              }));
                }), Future.value(undefined))(search(viableInput, viableInput, /* [] */0));
}

var input = InputLoader$Aoc19.commaSeparatedInts(2);

Future.tap(Future.flatMap(input, (function (i) {
            return runWithInput(i, 12, 2);
          })), (function (param) {
        console.log("Processed to:", param);
        return /* () */0;
      }));

Future.tap(Future.flatMap(input, (function (i) {
            return findInput(i, 19690720);
          })), (function (param) {
        console.log("Desired at:", param);
        return /* () */0;
      }));

exports.runWithInput = runWithInput;
exports.viableInput = viableInput;
exports.findInput = findInput;
exports.input = input;
/* viableInput Not a pure module */
