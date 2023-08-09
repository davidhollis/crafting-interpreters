type t = Clock [@@deriving show]

let to_string = function Clock -> "clock"
