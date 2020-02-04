namespace ZMidi
open ZMidi.DataTypes
open ZMidi.Internal.Utils
open ZMidi.Internal.WriterMonad
open System.Text
open FSharpPlus

module WriteFile =
    module PutOps =
        
        let putAscii (text: string) = String.getBytes Encoding.ASCII text |> PutBytes
        
        let putWord32be (value: uint32) = PutBytes (toBytesBE value)
        
        
        let putWord16be (value: uint16) = PutBytes (toBytesBE value)
        
        let putFormat = putWord16be << function | MidiFormat0 -> 0us
                                                | MidiFormat1 -> 1us
                                                | MidiFormat2 -> 2us
             
        let putTimeDivision timeDivision =
            match timeDivision with
            | FramePerSecond frame -> putWord16be (setBit 15 frame) 
            | TicksPerBeat ticks   -> putWord16be (clearBit 15 ticks)

        