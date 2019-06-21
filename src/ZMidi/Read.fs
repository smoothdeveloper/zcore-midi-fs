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
    let event : ParserMonad<MidiMessage> = 
      parseMidi {
        return! fatalError (Other "not implemented") }
    let getVarLen : ParserMonad<uint32> = 
      parseMidi {
        return! fatalError (Other "not implemented")
        //getVarlen :: ParserM Word32
        //getVarlen = liftM fromVarlen step1
        //  where
        //    step1     = word8 >>= \a -> if msbHigh a then step2 a else return (V1 a)
        //    step2 a   = word8 >>= \b -> if msbHigh b then step3 a b else return (V2 a b)
        //    step3 a b = word8 >>= \c -> if msbHigh c then do { d <- word8
        //                                                     ; return (V4 a b c d) }
        //                                             else return (V3 a b c)  
        
        
      }


    let deltaTime = 
      parseMidi {
          return! getVarLen
      } <??> (fun p -> "delta time")

    let message = 
        parseMidi {
          let! deltaTime = deltaTime
          let! event = event
          return deltaTime, event
        }
    let messages i = 
      parseMidi {
          return! fatalError (Other "not implemented")
          //let! messages = boundRepeat i message
      }
    let track : ParserMonad<MidiTrack> =
        parseMidi {
            let header = trackHeader
            return! fatalError (Other "not implemented")
           // let! messages = messages
        }
    //let midiFile =
    //  parseMidi {
    //    let! header = P.header
    //
    //  }

    //let readMidi filename =
    //  ParserMonad.runParseMidi 
    //let pitchBend ch = "pitch bend" <??> (PitchBend ch) <$> P.readWord14be

    