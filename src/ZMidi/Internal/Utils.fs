namespace ZMidi.Internal
open ZMidi.DataTypes

module Evil =
    let inline uncurry4 f = fun (a,b,c,d) -> f a b c d
module DataTypes =
    module FromBytes =

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

        let word32be (a: byte) (b: byte) (c: byte) (d: byte) : word32 =
              ((uint32 a) <<< 24)
            + ((uint32 b) <<< 16)
            + ((uint32 c) <<< 08)
            + ((uint32 d) <<< 00)
    module ToBytes =
        let word32be (v: word32) =
              (v &&& 0x000000ffu) >>> 00 |> byte
            , (v &&& 0x0000ff00u) >>> 08 |> byte
            , (v &&& 0x00ff0000u) >>> 16 |> byte
            , (v &&& 0xff000000u) >>> 24 |> byte
    module Isomorphisms =
        type Iso<'a,'b> = ('a -> 'b) * ('b -> 'a)
        module Iso =
            let reverse iso = snd iso, fst iso
            
        let word32be : Iso<_,_> = (ToBytes.word32be), (Evil.uncurry4 FromBytes.word32be)
        
module Utils = 
    open System.IO

    let inline (|TestBit|_|) (bit: int) (i: ^T) =
      let mask = LanguagePrimitives.GenericOne <<< bit
      if mask &&& i = mask then Some () else None

    let inline clearBit (bit: int) (i: ^T) =
      let mask = ~~~ (LanguagePrimitives.GenericOne <<< bit)
      i &&& mask
      
    let inline setBit (bit: int) (i: ^T) =
      let mask = (LanguagePrimitives.GenericOne <<< bit)
      i ||| mask
    let inline msbHigh i =
      match i with
      | TestBit 7 -> true
      | _ -> false


    module Text =
        let prettyBytes (bytes : byte array) =
          bytes 
          |> Array.chunkBySize 32
          |> Array.map (
            fun bytesChunk ->
              let bits =
                bytesChunk 
                |> Array.chunkBySize 16
                |> Array.map (fun items ->
                  items
                  |> Array.map (sprintf "%02x")
                  |> String.concat " "
                )
                |> String.concat "  -  "
                
              bits
          )

          |> String.concat System.Environment.NewLine
        let inline prettyBits number =
            let maxSize = 8 * System.Runtime.InteropServices.Marshal.SizeOf (number.GetType())
            [|0 .. (maxSize - 1)|]
            |> Array.rev
            |> Array.map (fun shift ->
                let mask = LanguagePrimitives.GenericOne <<< shift
                if (number &&& mask <> LanguagePrimitives.GenericZero) then "■" else " "
                )
            |> String.concat ""
            |> sprintf "[%s]"

    module PreventPrintF =
        open System
        let [<Obsolete("please do not use printfn in this file", true)>] printfn () = () 
        let [<Obsolete("please do not use printf in this file", true)>] printf () = () 
