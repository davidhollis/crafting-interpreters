open Base
open Result

let eval f args =
  match (f, args) with
  | Native.Clock, [] -> return (Runtime.Value.Number (Unix.gettimeofday ()))
  | _ -> fail `BadNativeCall
