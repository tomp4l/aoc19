open Jest
open Day18

let () =
  let mazeString =
    {j|#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################|j}
  in
  let maze = Maze.fromString mazeString false in
  describe "local djikstras" (fun () ->
      let open Expect in
      test "collects all keys with none collected" (fun () ->
          expect (Maze.nextKeys KeySet.empty (Start 0) maze)
          |> toEqual
               [
                 ("a", 3);
                 ("b", 3);
                 ("f", 3);
                 ("g", 3);
                 ("c", 5);
                 ("d", 5);
                 ("e", 5);
                 ("h", 5);
               ]);
      test "ignores a collected key" (fun () ->
          expect (Maze.nextKeys (KeySet.fromList [ "a" ]) (Start 0) maze)
          |> toEqual
               [
                 ("b", 3);
                 ("f", 3);
                 ("g", 3);
                 ("c", 5);
                 ("d", 5);
                 ("e", 5);
                 ("h", 5);
               ]);
      test "collect through open door" (fun () ->
          expect (Maze.nextKeys (KeySet.fromList [ "a"; "e" ]) (Start 0) maze)
          |> toEqual
               [
                 ("b", 3);
                 ("f", 3);
                 ("g", 3);
                 ("c", 5);
                 ("d", 5);
                 ("h", 5);
                 ("k", 8);
               ]);
      test "all near collected" (fun () ->
          expect
            (Maze.nextKeys
               (KeySet.fromList [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ])
               (Start 0) maze)
          |> toEqual
               [
                 ("j", 8);
                 ("k", 8);
                 ("n", 8);
                 ("o", 8);
                 ("i", 10);
                 ("l", 10);
                 ("m", 10);
                 ("p", 10);
               ]);
      test "all collected" (fun () ->
          expect
            (Maze.nextKeys
               (KeySet.fromList
                  [
                    "a";
                    "b";
                    "c";
                    "d";
                    "e";
                    "f";
                    "g";
                    "h";
                    "i";
                    "j";
                    "k";
                    "l";
                    "m";
                    "n";
                    "o";
                    "p";
                  ])
               (Start 0) maze)
          |> toEqual []);
      ());
  describe "count of keys" (fun () ->
      let open Expect in
      test "can count the keys" (fun () ->
          expect (Maze.numberOfKeys maze) |> toBe 16));
  describe "finds the shortest way to collect all keys" (fun () ->
      let open Expect in
      test "can count the keys" (fun () ->
          expect (Maze.collectAllKeys mazeString) |> toBe 136))
