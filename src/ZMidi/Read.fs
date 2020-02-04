namespace ZMidi

open FSharpPlus
open ZMidi.DataTypes

module ReadFile =
    open ZMidi.Internal.ParserMonad
    open ZMidi.Internal.Utils
    open ZMidi.Internal.DataTypes.FromBytes

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
    let getVarlenBytes = gencount getVarlen readByte (fun _ b -> b)
        
    let deltaTime = 
      parseMidi {
          let! v = getVarlen
          return DeltaTime(v)
      } <??> (fun p -> "delta time")
    
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
      
    let scale =
      parseMidi {
        match! readByte with
        | 0uy -> return MidiScaleType.Major
        | 1uy -> return MidiScaleType.Minor
        | other -> return MidiScaleType.OtherScale other
      }

    let metaEventSequenceNumber =
      parseMidi {
        let! a = assertWord8 2uy
        let! b = peek
        return SequenceNumber(word16be a b)
      }
      
    let metaEventChannelPrefix =
      parseMidi {
        let! _ = assertWord8 0x01uy
        let! b = readByte
        return ChannelPrefix b        
      }

    let metaEventMidiPort =
      parseMidi {
        let! _ = assertWord8 0x01uy
        let! b = readByte
        return MidiPort b   
      }
    let metaEventEndOfTrack =
      parseMidi {
        let! _ = assertWord8 0uy
        return EndOfTrack
      }
    let metaEventSetTempo =
      parseMidi {
        let! _ = assertWord8 3uy
        let! a = readWord24be
        return SetTempo a
      }
      
    let metaEventSmpteOffset =
      parseMidi {
        let! _ = assertWord8 5uy
        let! a = readByte
        let! b = readByte
        let! c = readByte
        let! d = readByte
        let! e = readByte
        return SMPTEOffset(a,b,c,d,e)
      }
    let metaEventTimeSignature =
      parseMidi {
        let! _ = assertWord8 4uy
        let! a = readByte
        let! b = readByte
        let! c = readByte
        let! d = readByte
        return TimeSignature(a,b,c,d)
      }
      
    let metaEventKeySignature =
      parseMidi {
        let! _ = assertWord8 2uy
        let! a = readByte
        let a = int8 a
        let! b = scale
        return KeySignature(a,b)
      }
      
    let metaEvent i =
      parseMidi {
        match i with
        | 0x00uy -> return! metaEventSequenceNumber    <??> (konst "sequence number")
        | 0x01uy -> return! textEvent GenericText      <??> (konst "generic text")
        | 0x02uy -> return! textEvent CopyrightNotice  <??> (konst "copyright notice")
        | 0x03uy -> return! textEvent SequenceName     <??> (konst "sequence name")
        | 0x04uy -> return! textEvent InstrumentName   <??> (konst "instrument name")
        | 0x05uy -> return! textEvent Lyrics           <??> (konst "lyrics")
        | 0x06uy -> return! textEvent Marker           <??> (konst "marker")
        | 0x07uy -> return! textEvent CuePoint         <??> (konst "cue point")
        | 0x20uy -> return! metaEventChannelPrefix     <??> (konst "channel prefix")
        | 0x21uy -> return! metaEventMidiPort          <??> (konst "midi port")
        | 0x2fuy -> return! metaEventEndOfTrack        <??> (konst "end of track")
        | 0x51uy -> return! metaEventSetTempo          <??> (konst "set tempo")
        | 0x54uy -> return! metaEventSmpteOffset       <??> (konst "smpte offset")
        | 0x58uy -> return! metaEventTimeSignature     <??> (konst "time signature")
        | 0x59uy -> return! metaEventKeySignature      <??> (konst "key signature")
        | 0x7fuy -> let! bytes = getVarlenBytes <??> (konst "system specific meta event")
                    return SSME bytes
        | other  -> let! bytes = getVarlenBytes <??> (konst (sprintf "meta other %x" other))
                    return MetaOther(other, bytes)
      }

    let isTerminated bytes =
      bytes 
      |> Array.tryFind ((=) 0xf7uy) 
      |> function | Some i -> true
                  | None -> false

    let rec sysExContPackets =
      parseMidi {
        let! d = deltaTime
        let! b = getVarlenBytes
        let answer = MidiSysExContPacket (d, b)
        if isTerminated b then return List.singleton answer
                          else 
                            let! answer2 = sysExContPackets
                            return (answer :: answer2)
      }
    let sysExEvent =
      parseMidi {
        let! b = getVarlenBytes
        if isTerminated b then
          return SysExSingle b
        else
          let! cont = sysExContPackets
          return SysExCont(b, cont)
      } <??> (fun _ -> "sysExEvent")
    let sysExEscape = 
      parseMidi {
        let! bytes = getVarlenBytes
        return SysExEscape bytes
      } <??> (fun _ -> "sysExEscape")
    let impossibleMatch text =
      fatalError (ErrMsg.Other (sprintf "impossible match: %s" text))

    let sysCommonEvent n =
      match n with
      | 0xf1uy -> readByte >>= (QuarterFrame >> mreturn) <??> (fun p -> "quarter frame")
      | 0xf2uy -> 
        parseMidi {
          let! a = readByte
          let! b = readByte
          return SongPosPointer(a,b)
        } <??> (fun p -> "song pos. pointer")
      | 0xf3uy -> readByte >>= (QuarterFrame >> mreturn) <??> (fun p -> "song select")
      | 0xf4uy -> mreturn UndefinedF4
      | 0xf5uy -> mreturn UndefinedF5
      | 0xf6uy -> mreturn TuneRequest
      | 0xf7uy -> mreturn EOX
      | tag -> impossibleMatch (sprintf "sysCommonEvent %x" tag)

    let sysRealtimeEvent n =
      match n with
      | 0xf8uy -> mreturn TimingClock
      | 0xf9uy -> mreturn TimingClock
      | 0xfauy -> mreturn TimingClock
      | 0xfbuy -> mreturn TimingClock
      | 0xfcuy -> mreturn TimingClock
      | 0xfduy -> mreturn TimingClock
      | 0xfeuy -> mreturn TimingClock
      | 0xffuy -> mreturn TimingClock
      | tag ->  impossibleMatch (sprintf "sysRealtimeEvent %x" tag)

    let inline (|SB|) b =
      b &&& 0xf0uy, b &&& 0x0fuy

    let noteOff ch =
      parseMidi {
        let! a = readByte
        let! b = readByte
        return MidiVoiceEvent.NoteOff(ch, a, b)
      } <??> (fun p -> "note-off")

    let noteOn ch =
      parseMidi {
        let! a = readByte
        let! b = readByte
        return MidiVoiceEvent.NoteOn(ch, a, b)
      } <??> (fun p -> "note-on")

    let noteAftertouch ch =
      parseMidi {
        let! a = readByte
        let! b = readByte
        return MidiVoiceEvent.NoteAfterTouch(ch, a, b)
      } <??> (fun p -> "noteAftertouch")

    let controller ch =
      parseMidi {
        let! a = readByte
        let! b = readByte
        return MidiVoiceEvent.Controller(ch, a, b)
      } <??> (fun p -> "controller")

    let programChange ch =
      parseMidi {
        let! a = readByte
        return MidiVoiceEvent.ProgramChange(ch, a)
      } <??> (fun p -> "controller")

    let channelAftertouch ch =
      parseMidi {
        let! a = readByte
        return MidiVoiceEvent.ChannelAftertouch(ch, a)
      } <??> (fun p -> "channelAftertouch")
    let pitchBend ch =
      parseMidi {
        let! a = readWord14be
        return MidiVoiceEvent.PitchBend(ch, a)
      } <??> (fun p -> "pitchBend")


    let voiceEvent n =
      parseMidi {
        match n with
        | SB(0x80uy, ch) -> do! setRunningEvent (NoteOff ch); 
                            return! noteOff ch
        | SB(0x90uy, ch) -> do! setRunningEvent (NoteOn ch)
                            return! noteOn ch
        | SB(0xa0uy, ch) -> do! setRunningEvent (NoteAftertoucuh ch)
                            return! noteAftertouch ch
        | SB(0xb0uy, ch) -> do! setRunningEvent (Control ch)
                            return! controller ch
        | SB(0xc0uy, ch) -> do! setRunningEvent (Program ch)
                            return! programChange ch
        | SB(0xd0uy, ch) -> do! setRunningEvent (ChannelAftertouch ch)
                            return! channelAftertouch ch
        | SB(0xe0uy, ch) -> do! setRunningEvent (PitchBend ch)
                            return! pitchBend ch
        | otherwise      -> return! impossibleMatch (sprintf "voiceEvent: %x" otherwise)
      }

    let runningStatus (event: VoiceEvent) : ParserMonad<MidiEvent> = 
      let mVoiceEvent e = mreturn (VoiceEvent(MidiRunningStatus.ON, e))
      match event with
      | NoteOff           ch -> (noteOff ch)           >>= mVoiceEvent
      | NoteOn            ch -> (noteOn ch)            >>= mVoiceEvent
      | NoteAftertoucuh   ch -> (noteAftertouch ch)    >>= mVoiceEvent
      | Control           ch -> (controller ch)        >>= mVoiceEvent
      | Program           ch -> (programChange ch)     >>= mVoiceEvent
      | ChannelAftertouch ch -> (channelAftertouch ch) >>= mVoiceEvent
      | PitchBend         ch -> (pitchBend ch)         >>= mVoiceEvent
      | StatusOff            -> readByte >>= (MidiEventOther >> mreturn)

    /// Parse an event - for valid input this function should parse
    /// without error (i.e all cases of event types are fully 
    /// enumerated). 
    ///
    /// Malformed input (syntactically bad events, or truncated data) 
    /// can cause fatal parse errors.
    
    let event : ParserMonad<MidiEvent> = 
      parseMidi {
        match! peek with
        | 0xffuy -> 
          do! dropByte
          let! event = readByte >>= metaEvent
          return MetaEvent event
        | 0xf7uy -> 
          do! dropByte
          let! sysexEvent = sysExEscape
          return SysExEvent sysexEvent
        | 0xf0uy ->
          do! dropByte
          let! sysexEvent = sysExEvent
          return SysExEvent sysexEvent
        | x when x >= 0xf8uy ->
          do! dropByte
          let! event = sysRealtimeEvent x
          return SysRealtimeEvent event
        | x when x >= 0xf1uy ->
          do! dropByte
          let! event = sysCommonEvent x
          return SysCommonEvent event
        | x when x >= 0x80uy ->
          do! dropByte
          let! voiceEvent = voiceEvent x
          return VoiceEvent(MidiRunningStatus.OFF, voiceEvent)
        | otherwise ->
          return! (getRunningEvent >>= runningStatus)
      }

    let message = 
        parseMidi {
          let! deltaTime = deltaTime
          let! event = event
          return { timestamp = deltaTime; event = event }
        }
    let messages i = 
      parseMidi {
          
          let! lastPos = getPos
          let maxPos = lastPos + int i
          return! repeatTillPosition maxPos message
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