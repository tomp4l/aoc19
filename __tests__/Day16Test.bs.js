// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.bs.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Day16$Aoc19 = require("../src/Day16.bs.js");
var Relude_List = require("relude/src/Relude_List.bs.js");
var Relude_String = require("relude/src/Relude_String.bs.js");

function strToList(s) {
  return Relude_List.map(Caml_format.caml_int_of_string)(Relude_String.splitList("", s));
}

Jest.describe("masks", (function (param) {
        Jest.test("pass 1 size 8", (function (param) {
                return Jest.Expect.toEqual({
                            hd: 1,
                            tl: {
                              hd: 0,
                              tl: {
                                hd: -1,
                                tl: {
                                  hd: 0,
                                  tl: {
                                    hd: 1,
                                    tl: {
                                      hd: 0,
                                      tl: {
                                        hd: -1,
                                        tl: {
                                          hd: 0,
                                          tl: /* [] */0
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }, Jest.Expect.expect(Day16$Aoc19.mask(8, 1)));
              }));
        Jest.test("pass 2 size 8", (function (param) {
                return Jest.Expect.toEqual({
                            hd: 0,
                            tl: {
                              hd: 1,
                              tl: {
                                hd: 1,
                                tl: {
                                  hd: 0,
                                  tl: {
                                    hd: 0,
                                    tl: {
                                      hd: -1,
                                      tl: {
                                        hd: -1,
                                        tl: {
                                          hd: 0,
                                          tl: /* [] */0
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }, Jest.Expect.expect(Day16$Aoc19.mask(8, 2)));
              }));
        
      }));

Jest.describe("pass", (function (param) {
        var number = {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: {
                  hd: 5,
                  tl: {
                    hd: 6,
                    tl: {
                      hd: 7,
                      tl: {
                        hd: 8,
                        tl: /* [] */0
                      }
                    }
                  }
                }
              }
            }
          }
        };
        Jest.test("pass 1 size 8", (function (param) {
                return Jest.Expect.toEqual(4, Jest.Expect.expect(Day16$Aoc19.pass(8, 1, number)));
              }));
        Jest.test("pass 2 size 8", (function (param) {
                return Jest.Expect.toEqual(8, Jest.Expect.expect(Day16$Aoc19.pass(8, 2, number)));
              }));
        Jest.test("pass 3 size 8", (function (param) {
                return Jest.Expect.toEqual(2, Jest.Expect.expect(Day16$Aoc19.pass(8, 3, number)));
              }));
        
      }));

Jest.describe("phase", (function (param) {
        var number = {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: {
                  hd: 5,
                  tl: {
                    hd: 6,
                    tl: {
                      hd: 7,
                      tl: {
                        hd: 8,
                        tl: /* [] */0
                      }
                    }
                  }
                }
              }
            }
          }
        };
        Jest.test("phase 1 size 8", (function (param) {
                return Jest.Expect.toEqual({
                            hd: 4,
                            tl: {
                              hd: 8,
                              tl: {
                                hd: 2,
                                tl: {
                                  hd: 2,
                                  tl: {
                                    hd: 6,
                                    tl: {
                                      hd: 1,
                                      tl: {
                                        hd: 5,
                                        tl: {
                                          hd: 8,
                                          tl: /* [] */0
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }, Jest.Expect.expect(Day16$Aoc19.phase(8, number)));
              }));
        Jest.test("phase 2 size 8", (function (param) {
                return Jest.Expect.toEqual({
                            hd: 3,
                            tl: {
                              hd: 4,
                              tl: {
                                hd: 0,
                                tl: {
                                  hd: 4,
                                  tl: {
                                    hd: 0,
                                    tl: {
                                      hd: 4,
                                      tl: {
                                        hd: 3,
                                        tl: {
                                          hd: 8,
                                          tl: /* [] */0
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }, Jest.Expect.expect(Day16$Aoc19.phase(8, Day16$Aoc19.phase(8, number))));
              }));
        Jest.test("phase 4 size 8", (function (param) {
                return Jest.Expect.toEqual({
                            hd: 0,
                            tl: {
                              hd: 1,
                              tl: {
                                hd: 0,
                                tl: {
                                  hd: 2,
                                  tl: {
                                    hd: 9,
                                    tl: {
                                      hd: 4,
                                      tl: {
                                        hd: 9,
                                        tl: {
                                          hd: 8,
                                          tl: /* [] */0
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }, Jest.Expect.expect(Day16$Aoc19.phases(4, 8, number)));
              }));
        Jest.test("phase 100 size 8", (function (param) {
                var number = strToList("80871224585914546619083218645595");
                return Jest.Expect.toEqual({
                            hd: 2,
                            tl: {
                              hd: 4,
                              tl: {
                                hd: 1,
                                tl: {
                                  hd: 7,
                                  tl: {
                                    hd: 6,
                                    tl: {
                                      hd: 1,
                                      tl: {
                                        hd: 7,
                                        tl: {
                                          hd: 6,
                                          tl: /* [] */0
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }, Jest.Expect.expect(Relude_List.take(8, Day16$Aoc19.phases(100, Curry._1(Relude_List.length, number), number))));
              }));
        
      }));

Jest.describe("repeat list", (function (param) {
        return Jest.test("repeat", (function (param) {
                      return Jest.Expect.toEqual({
                                  hd: 1,
                                  tl: {
                                    hd: 2,
                                    tl: {
                                      hd: 1,
                                      tl: {
                                        hd: 2,
                                        tl: {
                                          hd: 1,
                                          tl: {
                                            hd: 2,
                                            tl: /* [] */0
                                          }
                                        }
                                      }
                                    }
                                  }
                                }, Jest.Expect.expect(Day16$Aoc19.repeatList(3, {
                                          hd: 1,
                                          tl: {
                                            hd: 2,
                                            tl: /* [] */0
                                          }
                                        })));
                    }));
      }));

Jest.describe("offset", (function (param) {
        Jest.test("1 loop simple", (function (param) {
                return Jest.Expect.toEqual({
                            hd: 0,
                            tl: {
                              hd: 9,
                              tl: {
                                hd: 8,
                                tl: {
                                  hd: 7,
                                  tl: {
                                    hd: 6,
                                    tl: {
                                      hd: 5,
                                      tl: {
                                        hd: 4,
                                        tl: {
                                          hd: 3,
                                          tl: /* [] */0
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }, Jest.Expect.expect(Relude_List.take(8, Day16$Aoc19.digitsAtOffset(1, 100, 50, {
                                        hd: 1,
                                        tl: /* [] */0
                                      }))));
              }));
        var number = strToList("03036732577212944063491565474664");
        Jest.test("03036732577212944063491565474664", (function (param) {
                return Jest.Expect.toEqual({
                            hd: 8,
                            tl: {
                              hd: 4,
                              tl: {
                                hd: 4,
                                tl: {
                                  hd: 6,
                                  tl: {
                                    hd: 2,
                                    tl: {
                                      hd: 0,
                                      tl: {
                                        hd: 2,
                                        tl: {
                                          hd: 6,
                                          tl: /* [] */0
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }, Jest.Expect.expect(Relude_List.take(8, Day16$Aoc19.digitsAtOffset(100, 10000, 303673, number))));
              }));
        
      }));

exports.strToList = strToList;
/*  Not a pure module */
