open Jest
open Day10

let () =
  describe "loading" (fun () ->
      let open Expect in
      let input = ".#\n.." in
      test "load a whole input" (fun () ->
          expect (Space.fromString input |> Space.size) |> toBe 4);
      test "load an asteroid" (fun () ->
          expect (Space.fromString input |> Space.get (1, 0)) |> toBe Asteroid);
      test "load empty space" (fun () ->
          expect (Space.fromString input |> Space.get (0, 1)) |> toBe EmptySpace);
      test "return out of bounds" (fun () ->
          expect (Space.fromString input |> Space.get (0, 3))
          |> toBe OutOfBounds);
      ())

let () =
  describe "gcm" (fun () ->
      let open Expect in
      test "of 1,1 is 1" (fun () -> expect (gcm (1, 1)) |> toBe 1);
      test "of 1,0 is 1" (fun () -> expect (gcm (1, 0)) |> toBe 1);
      test "of 4,0 is 4" (fun () -> expect (gcm (4, 0)) |> toBe 4);
      test "of -1,1 is 1" (fun () -> expect (gcm (-1, 1)) |> toBe 1);
      test "of -1,-1 is 1" (fun () -> expect (gcm (-1, -1)) |> toBe 1);
      test "of 7,9 is 1" (fun () -> expect (gcm (7, 9)) |> toBe 1);
      test "of 9,7 is 1" (fun () -> expect (gcm (9, 7)) |> toBe 1);
      test "of 2,4 is 2" (fun () -> expect (gcm (2, 4)) |> toBe 2);
      test "of 2,-4 is 2" (fun () -> expect (gcm (2, -4)) |> toBe 2);
      test "of 5,5 is 5" (fun () -> expect (gcm (5, 5)) |> toBe 5);
      ())

let () =
  describe "space" (fun () ->
      let open Expect in
      let space = Space.fromString {j|.#..#
.....
#####
....#
...##|j} in
      test "can find an asteroid" (fun () ->
          expect (Space.asteroids space |> List.hd) |> toEqual (0, 2));
      test "can find all the asteroids" (fun () ->
          expect (Space.asteroids space |> List.length) |> toBe 10);
      test "can find a visible" (fun () ->
          expect (isVisible (4, 0) space (1, 0)) |> toBe true);
      test "can find a blocked" (fun () ->
          expect (isVisible (2, 2) space (0, 2)) |> toBe false);
      test "can count visible" (fun () ->
          expect (visibleCount space (3, 4)) |> toBe 8);
      test "best asteroid" (fun () ->
          expect (findBestAsteroid space) |> toEqual (3, 4));
      test "first target" (fun () ->
          expect (findFirstTarget (0, 2) space) |> toEqual (1, 0));
      test "first target" (fun () ->
          expect (findFirstTarget (3, 4) space) |> toEqual (3, 2));
      test "first target" (fun () ->
          expect (findFirstTarget (4, 0) space) |> toEqual (4, 2));
      ())

let () =
  describe "vectors" (fun () ->
      let open Expect in
      test "vector length can be calculated" (fun () ->
          expect (vectorLength (3, 4)) |> toEqual 5.);
      test "0 angle can be calculated" (fun () ->
          expect (angle (0, 1) (0, 1)) |> toEqual 0.);
      test "1/4 angle can be calculated" (fun () ->
          expect (angle (0, -1) (1, 0)) |> toBeCloseTo 1.57);
      test "1/4 angle can be calculated with different lengths" (fun () ->
          expect (angle (0, -4) (2, 0)) |> toBeCloseTo 1.57);
      test "angle" (fun () ->
          expect (angle (0, -1) (1, -2)) |> toBeCloseTo 0.46);
      ())

let () =
  describe "laser" (fun () ->
      let open Expect in
      let space =
        Space.fromString
          {j|.#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....#...###..
..#.#.....#....##|j}
      in
      test "first target aquired" (fun () ->
          expect (bigFuckingLaser (8, 3) space 1) |> toEqual (8, 1));
      test "second target aquired" (fun () ->
          expect (bigFuckingLaser (8, 3) space 2) |> toEqual (9, 0));
      test "third target aquired" (fun () ->
          expect (bigFuckingLaser (8, 3) space 3) |> toEqual (9, 1));
      test "fourth target aquired" (fun () ->
          expect (bigFuckingLaser (8, 3) space 4) |> toEqual (10, 0));
      test "fifth target aquired" (fun () ->
          expect (bigFuckingLaser (8, 3) space 5) |> toEqual (9, 2));
      test "sixth target aquired" (fun () ->
          expect (bigFuckingLaser (8, 3) space 6) |> toEqual (11, 1));
      test "seventh target aquired" (fun () ->
          expect (bigFuckingLaser (8, 3) space 7) |> toEqual (12, 1));
      test "eighth target aquired" (fun () ->
          expect (bigFuckingLaser (8, 3) space 8) |> toEqual (11, 2));
      test "ninth target aquired" (fun () ->
          expect (bigFuckingLaser (8, 3) space 9) |> toEqual (15, 1));
      let space =
        Space.fromString
          {j|.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##|j}
      in
      test "first target aquired" (fun () ->
          expect (bigFuckingLaser (11, 13) space 1) |> toEqual (11, 12));
      test "second target aquired" (fun () ->
          expect (bigFuckingLaser (11, 13) space 2) |> toEqual (12, 1));
      test "third target aquired" (fun () ->
          expect (bigFuckingLaser (11, 13) space 3) |> toEqual (12, 2));
      test "tenth target aquired" (fun () ->
          expect (bigFuckingLaser (11, 13) space 10) |> toEqual (12, 8));
      ())
