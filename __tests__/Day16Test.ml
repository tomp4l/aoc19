open Jest
open Day16

let strToList s =
  Relude.String.splitList ~delimiter:"" s |> Relude.List.map int_of_string

let () =
  describe "masks" (fun () ->
      let open Expect in
      test "pass 1 size 8" (fun () ->
          expect (mask 8 1) |> toEqual [ 1; 0; -1; 0; 1; 0; -1; 0 ]);
      test "pass 2 size 8" (fun () ->
          expect (mask 8 2) |> toEqual [ 0; 1; 1; 0; 0; -1; -1; 0 ]);
      ());

  describe "pass" (fun () ->
      let open Expect in
      let number = [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
      test "pass 1 size 8" (fun () -> expect (pass 8 1 number) |> toEqual 4);
      test "pass 2 size 8" (fun () -> expect (pass 8 2 number) |> toEqual 8);
      test "pass 3 size 8" (fun () -> expect (pass 8 3 number) |> toEqual 2);

      ());

  describe "phase" (fun () ->
      let open Expect in
      let number = [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
      test "phase 1 size 8" (fun () ->
          expect (phase 8 number) |> toEqual [ 4; 8; 2; 2; 6; 1; 5; 8 ]);
      test "phase 2 size 8" (fun () ->
          expect (phase 8 number |> phase 8)
          |> toEqual [ 3; 4; 0; 4; 0; 4; 3; 8 ]);
      test "phase 4 size 8" (fun () ->
          expect (phases 4 8 number) |> toEqual [ 0; 1; 0; 2; 9; 4; 9; 8 ]);
      test "phase 100 size 8" (fun () ->
          let number = strToList "80871224585914546619083218645595" in
          expect
            (phases 100 (Relude.List.length number) number |> Relude.List.take 8)
          |> toEqual [ 2; 4; 1; 7; 6; 1; 7; 6 ]);
      ());

  describe "repeat list" (fun () ->
      let open Expect in
      test "repeat" (fun () ->
          expect (repeatList 3 [ 1; 2 ]) |> toEqual [ 1; 2; 1; 2; 1; 2 ]));

  describe "offset" (fun () ->
      let open Expect in
      test "1 loop simple" (fun () ->
          expect (digitsAtOffset 1 100 50 [ 1 ] |> Relude.List.take 8)
          |> toEqual [ 0; 9; 8; 7; 6; 5; 4; 3 ]);
      let number = strToList "03036732577212944063491565474664" in
      test "03036732577212944063491565474664" (fun () ->
          expect (digitsAtOffset 100 10000 303673 number |> Relude.List.take 8)
          |> toEqual [ 8; 4; 4; 6; 2; 0; 2; 6 ]);
      ())
