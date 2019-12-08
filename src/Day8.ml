let ( >> ) = Relude.Function.flipCompose

let width = 25

let height = 6

let imageSize = width * height

let input = InputLoader.loadDay 8

let chunk string size =
  let rec loop remaining acc =
    match Relude.String.splitAt size remaining with
    | next, "" -> next :: acc
    | next, remaining -> loop remaining (next :: acc)
  in
  loop string [] |> Relude.List.reverse

let images string =
  chunk string imageSize
  |> Relude.List.map (Relude.String.splitList ~delimiter:"")

let digitCount d = Relude.List.filter (( = ) d) >> Relude.List.length

module ZeroCountOrd : BsAbstract.Interface.ORD with type t = string list =
struct
  let zeroCount = digitCount "0"

  type t = string list

  let compare a b = Relude.Int.Ord.compare (zeroCount a) (zeroCount b)

  let eq a b = zeroCount a = zeroCount b
end

let leastZeroes (images : string list list) =
  Relude.List.sort (module ZeroCountOrd) images
  |> Relude.List.head |> Relude.Option.getOrThrow

let onesTimesTwos image = digitCount "1" image * digitCount "2" image

exception DecodingFailure of string

let displayImage image =
  chunk image width
  |> Relude.List.String.joinWith "\n"
  |> Relude.String.replaceEach ~replaceWith:" " ~search:"0"

let decodedImage imageStack =
  let rec pixelLoop remainingStacks nextStack image =
    match remainingStacks with
    | [] -> raise (DecodingFailure "All transparent")
    | [] :: _ -> image |> Relude.List.reverse
    | ("2" :: next) :: rest -> pixelLoop rest (next :: nextStack) image
    | (p :: next) :: rest ->
        let image = p :: image in
        let dropRest = Relude.List.map Relude.List.tailOrEmpty rest in
        let stack =
          Relude.List.concat (Relude.List.reverse (next :: nextStack)) dropRest
        in
        pixelLoop stack [] image
  in
  pixelLoop imageStack [] [] |> Relude.List.String.join

let _ =
  input
  |> StackSafeFuture.tap
       (images >> leastZeroes >> onesTimesTwos >> Js.log2 "Least zeroes")

let _ =
  input
  |> StackSafeFuture.tap
       (images >> decodedImage >> displayImage >> ( ^ ) "\nDecoded:\n" >> Js.log)
