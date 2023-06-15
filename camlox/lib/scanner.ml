type t = { mutable source : string }

let create source = { source }
let scan_all _scanner : Token.t list = []
