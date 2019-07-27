namespace ZMidi.Internal

module Utils = 
    open ZMidi.DataTypes
    open System.IO
    
    /// Builds a Word16 (big endian).
    let word16be (a : byte) (b : byte) : uint16 = 
        let a = uint16 a
        let b = uint16 b
        (a <<< 8) + b

    /// Builds a Word14 (big endian).
    let word14be (a: byte) (b: byte) : word14 = 
        let a = uint16 a
        let b = uint16 b
        word14((a <<< 7) + b)

    let word24be (a: byte) (b: byte) (c: byte) : word24 =
        ((uint32 a) <<< 16)
        + ((uint32 b) <<< 8)
        + (uint32 c)

    let word32be (a: byte) (b: byte) (c: byte) (d: byte) : uint32 =
        ((uint32 a) <<< 24)
        + ((uint32 b) <<< 16)
        + ((uint32 c) <<< 8)
        + (uint32 d)
        