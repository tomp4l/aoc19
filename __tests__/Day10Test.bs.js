// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Day10$Aoc19 = require("../src/Day10.bs.js");

Jest.describe("loading", (function (param) {
        var input = ".#\n..";
        Jest.test("load a whole input", (function (param) {
                return Jest.Expect.toBe(4, Jest.Expect.expect(Curry._1(Day10$Aoc19.Space.size, Day10$Aoc19.Space.fromString(input))));
              }));
        Jest.test("load an asteroid", (function (param) {
                return Jest.Expect.toBe(/* Asteroid */0, Jest.Expect.expect(Day10$Aoc19.Space.get([
                                      1,
                                      0
                                    ])(Day10$Aoc19.Space.fromString(input))));
              }));
        Jest.test("load empty space", (function (param) {
                return Jest.Expect.toBe(/* EmptySpace */1, Jest.Expect.expect(Day10$Aoc19.Space.get([
                                      0,
                                      1
                                    ])(Day10$Aoc19.Space.fromString(input))));
              }));
        Jest.test("return out of bounds", (function (param) {
                return Jest.Expect.toBe(/* OutOfBounds */2, Jest.Expect.expect(Day10$Aoc19.Space.get([
                                      0,
                                      3
                                    ])(Day10$Aoc19.Space.fromString(input))));
              }));
        
      }));

Jest.describe("gcm", (function (param) {
        Jest.test("of 1,1 is 1", (function (param) {
                return Jest.Expect.toBe(1, Jest.Expect.expect(Day10$Aoc19.gcm([
                                    1,
                                    1
                                  ])));
              }));
        Jest.test("of 1,0 is 1", (function (param) {
                return Jest.Expect.toBe(1, Jest.Expect.expect(Day10$Aoc19.gcm([
                                    1,
                                    0
                                  ])));
              }));
        Jest.test("of 4,0 is 4", (function (param) {
                return Jest.Expect.toBe(4, Jest.Expect.expect(Day10$Aoc19.gcm([
                                    4,
                                    0
                                  ])));
              }));
        Jest.test("of -1,1 is 1", (function (param) {
                return Jest.Expect.toBe(1, Jest.Expect.expect(Day10$Aoc19.gcm([
                                    -1,
                                    1
                                  ])));
              }));
        Jest.test("of -1,-1 is 1", (function (param) {
                return Jest.Expect.toBe(1, Jest.Expect.expect(Day10$Aoc19.gcm([
                                    -1,
                                    -1
                                  ])));
              }));
        Jest.test("of 7,9 is 1", (function (param) {
                return Jest.Expect.toBe(1, Jest.Expect.expect(Day10$Aoc19.gcm([
                                    7,
                                    9
                                  ])));
              }));
        Jest.test("of 9,7 is 1", (function (param) {
                return Jest.Expect.toBe(1, Jest.Expect.expect(Day10$Aoc19.gcm([
                                    9,
                                    7
                                  ])));
              }));
        Jest.test("of 2,4 is 2", (function (param) {
                return Jest.Expect.toBe(2, Jest.Expect.expect(Day10$Aoc19.gcm([
                                    2,
                                    4
                                  ])));
              }));
        Jest.test("of 2,-4 is 2", (function (param) {
                return Jest.Expect.toBe(2, Jest.Expect.expect(Day10$Aoc19.gcm([
                                    2,
                                    -4
                                  ])));
              }));
        Jest.test("of 5,5 is 5", (function (param) {
                return Jest.Expect.toBe(5, Jest.Expect.expect(Day10$Aoc19.gcm([
                                    5,
                                    5
                                  ])));
              }));
        
      }));

Jest.describe("space", (function (param) {
        var space = Day10$Aoc19.Space.fromString(".#..#\n.....\n#####\n....#\n...##");
        Jest.test("can find an asteroid", (function (param) {
                return Jest.Expect.toEqual([
                            0,
                            2
                          ], Jest.Expect.expect(List.hd(Day10$Aoc19.Space.asteroids(space))));
              }));
        Jest.test("can find all the asteroids", (function (param) {
                return Jest.Expect.toBe(10, Jest.Expect.expect(List.length(Day10$Aoc19.Space.asteroids(space))));
              }));
        Jest.test("can find a visible", (function (param) {
                return Jest.Expect.toBe(true, Jest.Expect.expect(Day10$Aoc19.isVisible([
                                    4,
                                    0
                                  ], space, [
                                    1,
                                    0
                                  ])));
              }));
        Jest.test("can find a blocked", (function (param) {
                return Jest.Expect.toBe(false, Jest.Expect.expect(Day10$Aoc19.isVisible([
                                    2,
                                    2
                                  ], space, [
                                    0,
                                    2
                                  ])));
              }));
        Jest.test("can count visible", (function (param) {
                return Jest.Expect.toBe(8, Jest.Expect.expect(Day10$Aoc19.visibleCount(space, [
                                    3,
                                    4
                                  ])));
              }));
        Jest.test("best asteroid", (function (param) {
                return Jest.Expect.toEqual([
                            3,
                            4
                          ], Jest.Expect.expect(Day10$Aoc19.findBestAsteroid(space)));
              }));
        Jest.test("first target", (function (param) {
                return Jest.Expect.toEqual([
                            1,
                            0
                          ], Jest.Expect.expect(Day10$Aoc19.findFirstTarget([
                                    0,
                                    2
                                  ], space)));
              }));
        Jest.test("first target", (function (param) {
                return Jest.Expect.toEqual([
                            3,
                            2
                          ], Jest.Expect.expect(Day10$Aoc19.findFirstTarget([
                                    3,
                                    4
                                  ], space)));
              }));
        Jest.test("first target", (function (param) {
                return Jest.Expect.toEqual([
                            4,
                            2
                          ], Jest.Expect.expect(Day10$Aoc19.findFirstTarget([
                                    4,
                                    0
                                  ], space)));
              }));
        
      }));

Jest.describe("vectors", (function (param) {
        Jest.test("vector length can be calculated", (function (param) {
                return Jest.Expect.toEqual(5, Jest.Expect.expect(Day10$Aoc19.vectorLength([
                                    3,
                                    4
                                  ])));
              }));
        Jest.test("0 angle can be calculated", (function (param) {
                return Jest.Expect.toEqual(0, Jest.Expect.expect(Day10$Aoc19.angle([
                                    0,
                                    1
                                  ], [
                                    0,
                                    1
                                  ])));
              }));
        Jest.test("1/4 angle can be calculated", (function (param) {
                return Jest.Expect.toBeCloseTo(1.57, Jest.Expect.expect(Day10$Aoc19.angle([
                                    0,
                                    -1
                                  ], [
                                    1,
                                    0
                                  ])));
              }));
        Jest.test("1/4 angle can be calculated with different lengths", (function (param) {
                return Jest.Expect.toBeCloseTo(1.57, Jest.Expect.expect(Day10$Aoc19.angle([
                                    0,
                                    -4
                                  ], [
                                    2,
                                    0
                                  ])));
              }));
        Jest.test("angle", (function (param) {
                return Jest.Expect.toBeCloseTo(0.46, Jest.Expect.expect(Day10$Aoc19.angle([
                                    0,
                                    -1
                                  ], [
                                    1,
                                    -2
                                  ])));
              }));
        
      }));

Jest.describe("laser", (function (param) {
        var space = Day10$Aoc19.Space.fromString(".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##");
        Jest.test("first target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            8,
                            1
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    8,
                                    3
                                  ], space, 1)));
              }));
        Jest.test("second target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            9,
                            0
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    8,
                                    3
                                  ], space, 2)));
              }));
        Jest.test("third target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            9,
                            1
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    8,
                                    3
                                  ], space, 3)));
              }));
        Jest.test("fourth target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            10,
                            0
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    8,
                                    3
                                  ], space, 4)));
              }));
        Jest.test("fifth target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            9,
                            2
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    8,
                                    3
                                  ], space, 5)));
              }));
        Jest.test("sixth target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            11,
                            1
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    8,
                                    3
                                  ], space, 6)));
              }));
        Jest.test("seventh target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            12,
                            1
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    8,
                                    3
                                  ], space, 7)));
              }));
        Jest.test("eighth target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            11,
                            2
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    8,
                                    3
                                  ], space, 8)));
              }));
        Jest.test("ninth target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            15,
                            1
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    8,
                                    3
                                  ], space, 9)));
              }));
        var space$1 = Day10$Aoc19.Space.fromString(".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##");
        Jest.test("first target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            11,
                            12
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    11,
                                    13
                                  ], space$1, 1)));
              }));
        Jest.test("second target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            12,
                            1
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    11,
                                    13
                                  ], space$1, 2)));
              }));
        Jest.test("third target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            12,
                            2
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    11,
                                    13
                                  ], space$1, 3)));
              }));
        Jest.test("tenth target aquired", (function (param) {
                return Jest.Expect.toEqual([
                            12,
                            8
                          ], Jest.Expect.expect(Day10$Aoc19.bigFuckingLaser([
                                    11,
                                    13
                                  ], space$1, 10)));
              }));
        
      }));

/*  Not a pure module */
