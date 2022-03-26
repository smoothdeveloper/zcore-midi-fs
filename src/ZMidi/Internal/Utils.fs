namespace ZMidi.Internal
open ZMidi.DataTypes

module Evil =
    let inline uncurry2 f = fun (a,b) -> f a b
    let inline uncurry3 f = fun (a,b,c) -> f a b c
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
            + ((uint32 b) <<< 08)
            + (uint32 c)

        let word32be (a: byte) (b: byte) (c: byte) (d: byte) : word32 =
              ((uint32 a) <<< 24)
            + ((uint32 b) <<< 16)
            + ((uint32 c) <<< 08)
            + ((uint32 d) <<< 00)
    module ToBytes =
        let word24be (v: word32) =
              (v &&& 0x00ff0000u) >>> 16 |> byte
            , (v &&& 0x0000ff00u) >>> 08 |> byte
            , (v &&& 0x000000ffu) >>> 00 |> byte
            
        let word32be (v: word32) =
              (v &&& 0xff000000u) >>> 24 |> byte
            , (v &&& 0x00ff0000u) >>> 16 |> byte
            , (v &&& 0x0000ff00u) >>> 08 |> byte
            , (v &&& 0x000000ffu) >>> 00 |> byte
        let word14be (v: word14) =
            let v = v.Value in
              (v &&& 0b11111110000000us) >>> 07 |> byte
            , (v &&& 0b00000001111111us) >>> 00 |> byte
        let word16be (v: word16) =
              (v &&& 0b11111110000000us) >>> 08 |> byte
            , (v &&& 0b00000001111111us) >>> 00 |> byte
        
        let midiVoiceEvent x =
            
            match x with
            | NoteOff(status, byte1, byte2)        -> [|status ||| 0x80uy; byte1; byte2|]
            | NoteOn(status, byte1, byte2)         -> [|status ||| 0x90uy; byte1; byte2|]
            | NoteAfterTouch(status, byte1, byte2) -> [|status ||| 0xa0uy; byte1; byte2|]
            | Controller(status, byte1, byte2)     -> [|status ||| 0xb0uy; byte1; byte2|] 
            | ProgramChange(status, byte1)         -> [|status ||| 0xc0uy; byte1|]
            | ChannelAftertouch(status, byte1)     -> [|status ||| 0xd0uy; byte1|]
            | PitchBend(status, bend) ->
                let byte1, byte2 = word14be bend
                [|status ||| 0xe0uy; byte1; byte2|]
           
        let deltaTime (time: DeltaTime) =
            time.Value |> ExtraTypes.encodeVarlen
        let midiMetaEvent x =
            match x with
            | MidiMetaEvent.TextEvent(midiTextType, text) ->
                let b =
                    match midiTextType with
                    | GenericText     -> 01uy
                    | CopyrightNotice -> 02uy
                    | SequenceName    -> 03uy
                    | InstrumentName  -> 04uy
                    | Lyrics          -> 05uy
                    | Marker          -> 06uy
                    | CuePoint        -> 07uy
                let varLen = ExtraTypes.encodeVarlen text.Length
                [|0xffuy; b; yield! varLen; for c in text do byte c|]
            | MidiMetaEvent.ChannelPrefix b -> [|0xffuy; 0x20uy; 0x01uy; b|]
            | MidiMetaEvent.MidiPort b      -> [|0xffuy; 0x21uy; 0x01uy; b|]
            | MidiMetaEvent.EndOfTrack      -> [|0xffuy; 0x2fuy; 0x00uy|]
            | MidiMetaEvent.TimeSignature(b1, b2, b3, b4) -> [|0xffuy; 0x58uy; 0x04uy; b1; b2; b3; b4|]
            | MidiMetaEvent.KeySignature(b, midiScaleType) ->
                let a = byte b
                let b =
                    match midiScaleType with
                    | MidiScaleType.Major -> 0uy
                    | MidiScaleType.Minor -> 1uy
                    | MidiScaleType.OtherScale b -> b
                    
                [|0xffuy; 0x59uy; 0x02uy; a; b|]
            | MidiMetaEvent.SetTempo i      ->
                let a,b,c = word24be i
                [|0xffuy; 0x51uy; 0x03uy; a; b; c|]
            | MidiMetaEvent.SequenceNumber s ->
                let a,b = word16be s
                [|0xffuy; 0x00uy; 0x02uy; a; b|]
            | MidiMetaEvent.SMPTEOffset(a, b, c, d, e) ->
                [|0xffuy; 0x54uy; 0x05uy; a;b;c;d;e|]
            | MidiMetaEvent.MetaOther(otherType, bytes) ->
                [|0xffuy; otherType; yield! ExtraTypes.encodeVarlen bytes.Length; yield! bytes|]
            | MidiMetaEvent.SSME bytes ->
                [|0xffuy; 0x7fuy; yield! ExtraTypes.encodeVarlen bytes.Length; yield! bytes|]
            
        let midiSysCommonEvent midiSysCommonEvent =
            match midiSysCommonEvent with
            | MidiSysCommonEvent.QuarterFrame b       -> [|0xf1uy; b|]
            | MidiSysCommonEvent.SongPosPointer(a, b) -> [|0xf2uy; a; b|]
            | MidiSysCommonEvent.SongSelect b         -> [|0xf3uy; b|]
            | MidiSysCommonEvent.UndefinedF4          -> [|0xf4uy|]
            | MidiSysCommonEvent.UndefinedF5          -> [|0xf5uy|]
            | MidiSysCommonEvent.TuneRequest          -> [|0xf6uy|]
            | MidiSysCommonEvent.EOX                  -> [|0xf7uy|]
            
        let putSysexContPacket packets =
            [|
                for (MidiSysExContPacket(delta, bytes)) in packets do
                    yield! deltaTime delta
                    0xf7uy
                    yield! ExtraTypes.encodeVarlen bytes.Length
                    yield! bytes
            |]
        let midiSysexEvent midiSysexEvent =
            match midiSysexEvent with
            | MidiSysExEvent.SysExSingle bytes ->
                [|0xf0uy; yield! ExtraTypes.encodeVarlen bytes.Length ; yield! bytes |]
            | MidiSysExEvent.SysExEscape bytes ->
                [|0xf7uy; yield! ExtraTypes.encodeVarlen bytes.Length ; yield! bytes |]
            | MidiSysExEvent.SysExCont(bytes, midiSysExContPackets) ->
                [|0xf0uy; yield! ExtraTypes.encodeVarlen bytes.Length ; yield! bytes; yield! putSysexContPacket midiSysExContPackets |]
            
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
