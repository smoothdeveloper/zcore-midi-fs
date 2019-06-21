namespace ZMidi

open ZMidi.DataTypes

module ReadFile =
    //let readMidi filename =
    //let midiFile : Parser = ()
    open ZMidi.Internal.ParserMonad
    open ZMidi.Internal.Utils
    

    /// Apply parse then apply the check, if the check fails report
    /// the error message. 
    let postCheck parser isOutputValid errorMessage =
      parseMidi {
        let! answer = parser
        if isOutputValid answer then 
          return answer 
        else 
          return! fatalError errorMessage
      }

    let inline (|TestBit|_|) (bit: int) (i: uint16) =
      let mask = uint16(1 <<< bit)
      if mask &&& i = mask then Some () else None
    let inline clearBit (bit: int) (i:uint16) =
      let mask = ~~~ (uint16(1 <<< bit))
      i &&& mask
    let assertString (s: string) =
      postCheck (readString s.Length) ((=) s) (Other (sprintf "assertString: expected '%s'" s))

    let assertWord32 i =
      postCheck readUInt32be ((=) i) (Other (sprintf "assertWord32: expected '%i'" i))

    let fileFormat =
        parseMidi {
            match! readUInt16be with
            | 0us -> return MidiFormat0
            | 1us -> return MidiFormat1
            | 2us -> return MidiFormat2
            | x   -> return! (fatalError (Other (sprintf "fileFormat: Unrecognized file format %i" x)))
            }
    let timeDivision =
        parseMidi {
          match! readUInt16be with
          | TestBit 15 as x -> return FramePerSecond (clearBit 15 x)
          | x               -> return TicksPerBeat x
          }
    let header = 
        parseMidi {
            let! _ = assertString "MThd"
            let! _ = assertWord32 6u 
            let! format = fileFormat
            let! trackCount = readUInt16be
            let! timeDivision = timeDivision
            return { trackCount = trackCount
                     timeDivision = timeDivision
                     format = format }
            }
    //let midiFile =
    //  parseMidi {
    //    let! header = P.header
    //
    //  }

    //let readMidi filename =
    //  ParserMonad.runParseMidi 
    //let pitchBend ch = "pitch bend" <??> (PitchBend ch) <$> P.readWord14be

    