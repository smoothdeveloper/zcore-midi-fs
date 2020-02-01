module ZMidi.Internal.ExtraTypes
open FSharpPlus.Math.Generic



// --------------------------------------------------------------------------------
// -- Helper for varlen
// --------------------------------------------------------------------------------

/// Space efficient representation of length fields.
/// 
/// This data type is not used directly in the syntax tree where
/// it would be cumbersome. But it is used as an intermediate type
/// in the parser and emitter.
///
type Varlen = V1 of byte
            | V2 of byte * byte
            | V3 of byte * byte * byte
            | V4 of byte * byte * byte * byte


let inline up v = 0x7fuy &&& v
let inline left7  v = v <<< 7
let inline left14 v = v <<< 14
let inline left21 v = v <<< 21
let fromVarlen = 
  function | V1 a            -> uint32 (up a)
           | V2 (a, b)       -> (left7  (uint32 (up a))) + (uint32 (up b))
           | V3 (a, b, c)    -> (left14 (uint32 (up a))) + (left7  (uint32 (up b))) + uint32 (up c)
           | V4 (a, b, c, d) -> (left21 (uint32 (up a))) + (left14 (uint32 (up b))) + (left7 (uint32 (up c))) + uint32 (up d)
let inline encodeVarlen (myValue) =
    let inline initMask nBits =
        [|0 .. nBits - 1|]
        |> Array.map (fun shift -> 1G <<< shift)
        |> Array.fold ((|||)) 0G
    let nBits = 7
    let maxBits =
        let nMaxBytes = System.Runtime.InteropServices.Marshal.SizeOf(myValue.GetType())
        nMaxBytes * nBits
    let maxValue = initMask maxBits
    if maxValue < myValue then
        failwithf "can't encode %i: to high, max being %i" myValue maxValue
    let shiftAnd7Bits =
        [|0 .. nBits .. maxBits - 1|]
        |> Array.map (fun shift ->
            let mask = initMask nBits <<< shift
            let value =byte ((myValue &&& mask) >>> shift)
            value
            )
        
    shiftAnd7Bits
    |> Array.rev
    |> Array.skipWhile ((=) 0G)
    |> function | [||] -> [|0G|]
                | bytes -> bytes 

