// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Relude_Int = require("relude/src/Relude_Int.bs.js");
var Relude_List = require("relude/src/Relude_List.bs.js");
var Relude_Function = require("relude/src/Relude_Function.bs.js");
var InputLoader$Aoc19 = require("./lib/InputLoader.bs.js");
var StackSafeFuture$Aoc19 = require("./lib/StackSafeFuture.bs.js");
var Relude_List_Specializations = require("relude/src/list/Relude_List_Specializations.bs.js");

function fuelRequired(mass) {
  return Curry._2(Relude_Int.max, 0, (mass / 3 | 0) - 2 | 0);
}

function totalFuelRequired(masses) {
  return Curry._1(Relude_List_Specializations.Int.sum, Relude_List.map(fuelRequired)(masses));
}

function fuelForFuel(mass) {
  var extraFuel = fuelRequired(mass);
  if (extraFuel === 0) {
    return 0;
  } else {
    return extraFuel + fuelForFuel(extraFuel) | 0;
  }
}

function fuelRequiredIncludingFuel(mass) {
  var fuelReqired = fuelRequired(mass);
  return fuelReqired + fuelForFuel(fuelReqired) | 0;
}

function totalFuelRequiredIncludingFuel(masses) {
  return Curry._1(Relude_List_Specializations.Int.sum, Relude_List.map(fuelRequiredIncludingFuel)(masses));
}

var input = InputLoader$Aoc19.newlineSeparatedInts(1);

var $great$great = Relude_Function.Infix.$great$great;

StackSafeFuture$Aoc19.tap((function (param) {
          return $great$great(totalFuelRequired, (function (param) {
                        console.log("Fuel without extra fuel", param);
                        return /* () */0;
                      }), param);
        }))(input);

StackSafeFuture$Aoc19.tap((function (param) {
          return $great$great(totalFuelRequiredIncludingFuel, (function (param) {
                        console.log("Fuel with fuel for fuel", param);
                        return /* () */0;
                      }), param);
        }))(input);

exports.fuelRequired = fuelRequired;
exports.totalFuelRequired = totalFuelRequired;
exports.fuelForFuel = fuelForFuel;
exports.fuelRequiredIncludingFuel = fuelRequiredIncludingFuel;
exports.totalFuelRequiredIncludingFuel = totalFuelRequiredIncludingFuel;
exports.input = input;
exports.$great$great = $great$great;
/* input Not a pure module */
