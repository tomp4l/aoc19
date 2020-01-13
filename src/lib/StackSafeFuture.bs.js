// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Relude_Array = require("relude/src/Relude_Array.bs.js");
var Relude_Extensions_Apply = require("relude/src/extensions/Relude_Extensions_Apply.bs.js");
var Relude_Extensions_Monad = require("relude/src/extensions/Relude_Extensions_Monad.bs.js");
var Relude_Extensions_Functor = require("relude/src/extensions/Relude_Extensions_Functor.bs.js");
var Relude_Extensions_Applicative = require("relude/src/extensions/Relude_Extensions_Applicative.bs.js");

function mapState(f, state) {
  var value = state[0];
  var match = value.contents;
  if (match !== undefined) {
    var v = Caml_option.valFromOption(match);
    return Curry._1(state[2], (function (param) {
                  return Curry._1(f, v);
                }));
  } else {
    var callbacks = state[1];
    callbacks.contents = Relude_Array.append((function (param) {
            var match = value.contents;
            if (match !== undefined) {
              return Curry._1(f, Caml_option.valFromOption(match));
            } else {
              return /* () */0;
            }
          }), callbacks.contents);
    return /* () */0;
  }
}

var inFlight = {
  contents: /* array */[]
};

var running = {
  contents: false
};

function defaultExecutionContext(run) {
  var match = running.contents;
  if (match) {
    inFlight.contents = Relude_Array.append(run, inFlight.contents);
    return /* () */0;
  } else {
    var _run = run;
    while(true) {
      var run$1 = _run;
      running.contents = true;
      Curry._1(run$1, /* () */0);
      var match$1 = Relude_Array.head(inFlight.contents);
      if (match$1 !== undefined) {
        inFlight.contents = Relude_Array.tailOrEmpty(inFlight.contents);
        _run = match$1;
        continue ;
      } else {
        running.contents = false;
        return /* () */0;
      }
    };
  }
}

function make(resolver) {
  var value = {
    contents: undefined
  };
  var callbacks = {
    contents: /* array */[]
  };
  var resolve = function (v) {
    value.contents = Caml_option.some(v);
    var _param = /* () */0;
    while(true) {
      var match = Relude_Array.head(callbacks.contents);
      if (match !== undefined) {
        defaultExecutionContext(match);
        callbacks.contents = Relude_Array.tailOrEmpty(callbacks.contents);
        _param = /* () */0;
        continue ;
      } else {
        return /* () */0;
      }
    };
  };
  Curry._1(resolver, resolve);
  return /* Future */[/* tuple */[
            value,
            callbacks,
            defaultExecutionContext
          ]];
}

function map(f, param) {
  var state = param[0];
  return make((function (resolve) {
                return mapState((function (v) {
                              return Curry._1(resolve, Curry._1(f, v));
                            }), state);
              }));
}

function flatMap(f, param) {
  var state = param[0];
  return make((function (resolve) {
                return mapState((function (v) {
                              var match = Curry._1(f, v);
                              return mapState(Curry.__1(resolve), match[0]);
                            }), state);
              }));
}

function tap(f) {
  return (function (param) {
      return map((function (v) {
                    Curry._1(f, v);
                    return v;
                  }), param);
    });
}

function pure(v) {
  return make((function (resolve) {
                return Curry._1(resolve, v);
              }));
}

function never(param) {
  return make((function (param) {
                return /* () */0;
              }));
}

function delay(t, f) {
  return make((function (resolve) {
                setTimeout((function (param) {
                        return Curry._1(resolve, Curry._1(f, /* () */0));
                      }), t);
                return /* () */0;
              }));
}

var Functor = {
  map: map
};

var include = Relude_Extensions_Functor.FunctorExtensions(Functor);

function apply(f, a) {
  return flatMap((function (f$prime) {
                return map(Curry.__1(f$prime), a);
              }), f);
}

var Apply = {
  map: map,
  apply: apply
};

var include$1 = Relude_Extensions_Apply.ApplyExtensions(Apply);

var Applicative = {
  map: map,
  apply: apply,
  pure: pure
};

var include$2 = Relude_Extensions_Applicative.ApplicativeExtensions(Applicative);

function bind(x, f) {
  return flatMap(f, x);
}

var Monad = {
  map: map,
  apply: apply,
  pure: pure,
  flat_map: bind
};

var include$3 = Relude_Extensions_Monad.MonadExtensions(Monad);

var flipMap = include.flipMap;

var $$void = include.$$void;

var voidRight = include.voidRight;

var voidLeft = include.voidLeft;

var flap = include.flap;

var applyFirst = include$1.applyFirst;

var applySecond = include$1.applySecond;

var map2 = include$1.map2;

var map3 = include$1.map3;

var map4 = include$1.map4;

var map5 = include$1.map5;

var tuple2 = include$1.tuple2;

var tuple3 = include$1.tuple3;

var tuple4 = include$1.tuple4;

var tuple5 = include$1.tuple5;

var mapTuple2 = include$1.mapTuple2;

var mapTuple3 = include$1.mapTuple3;

var mapTuple4 = include$1.mapTuple4;

var mapTuple5 = include$1.mapTuple5;

var liftA1 = include$2.liftA1;

var all = include$2.all;

var flatMap$1 = include$3.flatMap;

var flatten = include$3.flatten;

var composeKleisli = include$3.composeKleisli;

var flipComposeKleisli = include$3.flipComposeKleisli;

var liftM1 = include$3.liftM1;

var when_ = include$3.when_;

var unless = include$3.unless;

exports.Apply = Apply;
exports.Applicative = Applicative;
exports.Functor = Functor;
exports.Monad = Monad;
exports.make = make;
exports.map = map;
exports.tap = tap;
exports.pure = pure;
exports.never = never;
exports.delay = delay;
exports.flipMap = flipMap;
exports.$$void = $$void;
exports.voidRight = voidRight;
exports.voidLeft = voidLeft;
exports.flap = flap;
exports.apply = apply;
exports.applyFirst = applyFirst;
exports.applySecond = applySecond;
exports.map2 = map2;
exports.map3 = map3;
exports.map4 = map4;
exports.map5 = map5;
exports.tuple2 = tuple2;
exports.tuple3 = tuple3;
exports.tuple4 = tuple4;
exports.tuple5 = tuple5;
exports.mapTuple2 = mapTuple2;
exports.mapTuple3 = mapTuple3;
exports.mapTuple4 = mapTuple4;
exports.mapTuple5 = mapTuple5;
exports.liftA1 = liftA1;
exports.all = all;
exports.bind = bind;
exports.flatMap = flatMap$1;
exports.flatten = flatten;
exports.composeKleisli = composeKleisli;
exports.flipComposeKleisli = flipComposeKleisli;
exports.liftM1 = liftM1;
exports.when_ = when_;
exports.unless = unless;
/* include Not a pure module */
