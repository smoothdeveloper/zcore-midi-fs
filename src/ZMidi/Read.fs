namespace ZMidi

open ZMidi.DataTypes

module ReadFile =
    open ZMidi.Internal.ParserMonad
    open ZMidi.Internal.Utils
    open ZMidi.Internal.ExtraTypes

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
    let inline msbHigh i =
      match i with
      | TestBit 7 -> true
      | _ -> false
    let assertString (s: string) =
      postCheck (readString s.Length) ((=) s) (Other (sprintf "assertString: expected '%s'" s))

    let assertWord32 i =
      postCheck readUInt32be ((=) i) (Other (sprintf "assertWord32: expected '%i'" i))

    let assertWord8 i =
      postCheck readByte ((=) i) (Other (sprintf "assertWord8: expected '%i'" i))

    let getVarlen : ParserMonad<word32> =
      let rec loop acc =
        parseMidi {
          let! b = readByte
          let acc = acc <<< 7
          if msbHigh b then
            let result = uint64 (b &&& 0b01111111uy)
            return! loop (acc + result)
          else
            return (acc + (uint64 b)) }  
      parseMidi {
        let! result = loop 0UL
        return (uint32 result) }
      
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
        let! a = assertWord8 2uy
        let! b = peek
        let! text = getVarlenText
        return TextEvent(textType, text)
      }

    let metaEventSequenceNumber =
      parseMidi {
        let! a = assertWord8 2uy
        let! b = peek
        return SequenceNumber(word16be a b)
      }

    let metaEvent i =
      parseMidi {
        match i with
        | 0x00 -> return! metaEventSequenceNumber
        | 0x01 -> return! textEvent GenericText
        | 0x02 -> return! textEvent CopyrightNotice
        | 0x03 -> return! textEvent SequenceName
        | 0x04 -> return! textEvent InstrumentName
        | 0x05 -> return! textEvent Lyrics
        | 0x06 -> return! textEvent Marker
        | 0x07 -> return! textEvent CuePoint
      }
//    let (<*>) af ma =


    let runningStatus : ParserMonad<MidiRunningStatus> = 
      parseMidi {
      
        return MidiRunningStatus.ON
      }
    //let metaEvent n = //: ParserMonad<MetaEvent> =
    //  match n with
    //  ///| 0x00uy -> ( SequenceNumber <~> (assertWord8 2uy *> word16be)) <??> (sprintf "sequence number: failed at %i" )
    //  | 0x01uy -> (textEvent GenericText)     <??> (sprintf "generic text: failed at %i")
    //  | 0x02uy -> (textEvent CopyrightNotice) <??> (sprintf "generic text: failed at %i")
    //  | 0x03uy -> (textEvent SequenceName)    <??> (sprintf "generic text: failed at %i")
    //  | 0x04uy -> (textEvent InstrumentName)  <??> (sprintf "generic text: failed at %i")
    //  | 0x05uy -> (textEvent Lyrics)          <??> (sprintf "generic text: failed at %i")
    //  | 0x06uy -> (textEvent Marker)          <??> (sprintf "generic text: failed at %i")
    //  | 0x07uy -> (textEvent CuePoint)        <??> (sprintf "generic text: failed at %i")
    //  //| 0x20uy -> (textEvent GenericText)     <??> (sprintf "generic text: failed at %i")
    //  | _ -> failwithf "metaEvent %i" n

      //parseMidi {
      //  
      //}
    let event : ParserMonad<MidiEvent> = 
      let step n : ParserMonad<MidiMetaEvent>= 
        //parseMidi {
          match n with
          | 0xffuy -> failwithf "" //MetaEvent <~> (dropByte *> (readByte >>= metaEvent))
          //| 0xf7uy -> ()//SysexEvent
          //| 0xf0uy -> ()//SysexEvent
          //| 0x80uy -> ()//VoiceEvent
          //| n when n >= 0xf8uy -> ()//SysRealtimeEvent
          //| n when n >= 0xf1uy -> ()//SysCommonEvent
          //| n when n >= 0x80uy -> ()//VoiceEvent
          //| _      -> getRunningEvent >>= runningStatus
        //}
      parseMidi {
        let! p = peek
        step p
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

    let midiFile =
      parseMidi {
        let! header = header
        let! tracks = count (header.trackCount) track
        return { header = header; tracks = tracks }
      }