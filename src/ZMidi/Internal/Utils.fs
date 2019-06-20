namespace ZMidi.Internal

module Utils = 
    open ZMidi.DataTypes
    open System.IO
    
    /// Builds a Word16 (big endian).
    let uint16be (a : byte) (b : byte) : uint16 = 
        let a = uint16 a
        let b = uint16 b
        (a <<< 8) + b

    /// Builds a Word14 (big endian).
    let word14be a b = 
      let a = uint16 a
      let b = uint16 b
      word14((a <<< 7) + b)
      