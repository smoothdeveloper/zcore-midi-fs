#r "../build/Debug/AnyCPU/net462/zmidi-fs-core.dll"
open ZMidi.Internal
open System.IO
open ZMidi
open ZMidi.WriteFile
open ZMidi.DataTypes
open ZMidi.Internal.Utils
open ZMidi.Internal.WriterMonad
open ZMidi.Internal.DataTypes
let rec putter op =
    
    match op with
    | PutByte b -> System.Console.WriteLine (Text.prettyBits b)
    | PutBytes bytes -> bytes |> Array.map Text.prettyBits |> Array.iter System.Console.WriteLine 
    | NoOp -> ()
    | Combined(op1,op2) ->
        putter op1
        putter op2
open PutOps

let putVarlen (varlen: word32) = PutBytes (ExtraTypes.encodeVarlen varlen)
    
let putDeltaTime (deltaTime: DeltaTime) =
    putVarlen deltaTime.Value

let putEvent event =
    match event with
    | MidiEvent.MetaEvent _        -> PutByte 0xffuy 
    | MidiEvent.SysExEvent _       -> PutByte 0xf7uy 
    | MidiEvent.VoiceEvent(_, e)   -> PutByte e.Status 
    | MidiEvent.MidiEventOther b   -> PutByte b 
    | MidiEvent.SysCommonEvent e   -> PutByte e.Status
    | MidiEvent.SysRealtimeEvent e -> PutByte e.Status
    
let putMessage (message: MidiMessage) =
    seq {
        yield putDeltaTime message.timestamp
        yield putEvent message.event
    }
let writeHeader header =
    writeBytes putter {
        do! putAscii "MThd";
        do! putWord32be 6u
        do! putFormat header.format
        do! putWord16be header.trackCount
        do! putTimeDivision header.timeDivision
    }
let writeTrack track =
    writeBytes putter {
        do! putAscii "MTrk"
        do! putWord32be (uint32 (Array.length track))
        for m in track do
            do! putMessage m
    }
    
let folder = 
  Path.Combine(__SOURCE_DIRECTORY__ , ".." , "data", "midifiles")
  |> DirectoryInfo

for file in folder.EnumerateFiles() do
  let buffer = File.ReadAllBytes file.FullName
  match ParserMonad.runParser ReadFile.midiFile buffer ZMidi.Internal.ParserMonad.State.initial with
  | Ok midiFile ->
      try
        writeTrack midiFile.tracks.[0]
      with e ->
          printfn "%A" e
  | Error e -> printfn "%A" e
