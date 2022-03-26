namespace ZMidi
open ZMidi.DataTypes
open ZMidi.Internal.DataTypes
open ZMidi.Internal.Utils
open ZMidi.Internal.WriterMonad
open System.Text
module WriteFile =
    module PutOps =
        
        let putAscii (text: string) = text |> Encoding.ASCII.GetBytes |> PutBytes
        
        let putWord32be (value: uint32) = PutBytes [| byte (value >>> 24)
                                                      byte (value >>> 16)
                                                      byte (value >>> 8)
                                                      byte (value >>> 0) |]
        
        
        let putWord16be (value: uint16) = PutBytes [| byte (value >>> 8)
                                                      byte (value >>> 0) |]
        
        let putFormat = putWord16be << function | MidiFormat0 -> 0us
                                                | MidiFormat1 -> 1us
                                                | MidiFormat2 -> 2us
             
        let putTimeDivision timeDivision =
            match timeDivision with
            | FramePerSecond frame -> putWord16be (setBit 15 frame) 
            | TicksPerBeat ticks   -> putWord16be (clearBit 15 ticks)

        let putHeader (header: MidiHeader) =
            [|
                putAscii "MThd"
                putWord32be 6u
                putFormat header.format
                putWord16be header.trackCount
                putTimeDivision header.timeDivision
            |]
            |> PutOp.concat
        let putVarLen varLen =
            PutBytes (ZMidi.Internal.ExtraTypes.encodeVarlen varLen)
        let putDeltaTime (timestamp: DeltaTime) =
            putVarLen timestamp.Value
        let putMidiEvent event =
            match event with
            | MidiEvent.VoiceEvent(midiRunningStatus, midiVoiceEvent) ->
                PutBytes (ToBytes.midiVoiceEvent midiVoiceEvent)                
            | MetaEvent midiMetaEvent ->
                PutBytes (ToBytes.midiMetaEvent midiMetaEvent)
            | MidiEvent.SysExEvent midiSysExEvent ->
                PutBytes (ToBytes.midiSysexEvent midiSysExEvent)
            | MidiEventOther b ->
                PutByte b
            | SysRealtimeEvent midiSysRealtimeEvent ->
                PutByte
                    (
                    match midiSysRealtimeEvent with
                    | TimingClock      -> 0xf8uy
                    | UndefinedF9      -> 0xf9uy
                    | StartSequence    -> 0xfauy
                    | ContinueSequence -> 0xfbuy
                    | StopSequence     -> 0xfcuy
                    | UndefinedFD      -> 0xfduy
                    | ActiveSensing    -> 0xfeuy
                    | SystemReset      -> 0xffuy
                    )
            | SysCommonEvent midiSysCommonEvent ->
                PutBytes (ToBytes.midiSysCommonEvent midiSysCommonEvent)
            
        let putTrack events =
            let eventsAsBytes =
                [|
                  for e in events do
                      putDeltaTime e.timestamp
                      putMidiEvent e.event
                |]
                |> PutOp.concat
                |> PutOp.toBytes
            
            [|
              putAscii "MTrk"
              putWord32be (uint32 (Array.length eventsAsBytes))
              PutBytes eventsAsBytes
            |]
            |> PutOp.concat
                
        let putMidiFile (midiFile: MidiFile) =
            [|
                putHeader midiFile.header
                for t in midiFile.tracks do
                    putTrack t
            |]                            
            |> PutOp.concat
            