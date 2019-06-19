namespace ZMidi.Internal

module Utils = 

    open System.IO
    

    /// Build a Word16 (big endian).
    let uint16be (a : byte) (b : byte) : uint16 = 
        let a1 = uint16 a
        let b1 = uint16 b
        (a1 <<< 8) + b1