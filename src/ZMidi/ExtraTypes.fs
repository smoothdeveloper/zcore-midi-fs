module ZMidi.Internal.ExtraTypes



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