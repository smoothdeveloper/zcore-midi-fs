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

    let inline (|TestBit|_|) (bit: int) (i: ^T) =
      let mask = LanguagePrimitives.GenericOne <<< bit
      if mask &&& i = mask then Some () else None

    let inline clearBit (bit: int) (i: ^T) =
      let mask = ~~~ (LanguagePrimitives.GenericOne <<< bit)
      i &&& mask

    let assertString (s: string) =
      postCheck (readString s.Length) ((=) s) (Other (sprintf "assertString: expected '%s'" s))

    let assertWord32 i =
      postCheck readUInt32be ((=) i) (Other (sprintf "assertWord32: expected '%i'" i))

    let assertWord8 i =
      postCheck readByte ((=) i) (Other (sprintf "assertWord8: expected '%i'" i))

    let getVarlen : ParserMonad<word32> =
      parseMidi {
        failwith "getVarlen: not imple"
      }
    
    let getVarlenText = gencount getVarlen readChar (fun _ b -> System.String b)

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
    let trackHeader =
        parseMidi {
          let! _ = assertString "MTrk"
          return! readUInt32be
        }
    let textEvent textType =
      parseMidi {
        let! text = getVarlenText
        return TextEvent(textType, text)
      }
    let metaEventSequenceNumber =
      parseMidi {
        let! a = assertWord8 2uy
        let! b = peek
        return SequenceNumber(word16be a b)
      }
      
    let metaEventGenericText =
      parseMidi {
        let! a = assertWord8 2uy
        let! b = peek
        return! textEvent GenericText
      }
    let metaEvent i =
      parseMidi {
        match i with
        | 0x00 -> return metaEventSequenceNumber
        | 0x01 -> return metaEventGenericText
      }
    let event : ParserMonad<MidiEvent> = 
      parseMidi {
        return! fatalError (Other "event: not implemented") }


    let deltaTime = 
      parseMidi {
          return! getVarlen
      } <??> (fun p -> "delta time")

    let message = 
        parseMidi {
          let! deltaTime = deltaTime
          let! event = event
          return { timestamp = DeltaTime(deltaTime); event = event }
        }
    let messages i = 
      parseMidi {
          return! boundRepeat (int i) message
      }
    let track : ParserMonad<MidiTrack> =
        parseMidi {
            let! length = trackHeader
            return! messages length
        }
    //let midiFile =
    //  parseMidi {
    //    let! header = P.header
    //
    //  }

    //let readMidi filename =
    //  ParserMonad.runParseMidi 
    //let pitchBend ch = "pitch bend" <??> (PitchBend ch) <$> P.readWord14be

    