module ZMidi.Tests.Tests

open System
open System.Diagnostics
open System.IO
open Expecto
open ZMidi.Internal.ParserMonad
open ZMidi.Tests.Infrastructure
open ZMidi.Tests.ExpectedFailures
open ZMidi
open ZMidi.ReadFile
open ZMidi.DataTypes

type Fileset = Fileset of name: string * ArchiveKind * uri: string 

let fileSets = [
  Fileset("maestro-v2.0.0-midi", Zip  , "https://storage.googleapis.com/magentadata/datasets/maestro/v2.0.0/maestro-v2.0.0-midi.zip")
  Fileset("magentadata-groove" , Zip  , "https://storage.googleapis.com/magentadata/datasets/groove/groove-v1.0.0-midionly.zip")
  Fileset("magentadata-egmd"   , Zip  , "https://storage.googleapis.com/magentadata/datasets/e-gmd/v1.0.0/e-gmd-v1.0.0-midi.zip")
  Fileset("lmd_full"           , TarGz, "http://hog.ee.columbia.edu/craffel/lmd/lmd_full.tar.gz")
]
      
let dir = DirectoryInfo(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "data"))
let errortext = Path.Combine(dir.FullName, "errortext.txt")
let enumerateFilesetMidiFiles sort =
    seq {
      yield! dir.EnumerateFiles("*.midi", SearchOption.AllDirectories)
      yield! dir.EnumerateFiles("*.mid", SearchOption.AllDirectories) 
    }
    |> Seq.indexed
    |> Seq.sortBy sort

module Parse =
    let zmidiParse bytes =
        try
          runParser ReadFile.midiFile bytes State.initial
        with
          e -> Error(ParseError.ParseError(Pos.MinValue, ErrMsg.Exn e))

    let drywetMidiParse (bytes: _ array) =
        use ms = new MemoryStream(bytes)
        try
            let midi = Melanchall.DryWetMidi.Core.MidiFile.Read(ms)
            Ok midi
        with
          e -> Error e
    let naudioParse (bytes: _ array) =
        use ms = new MemoryStream(bytes)
        try
            let midi = NAudio.Midi.MidiFile(ms, true)
            Ok midi
        with
          e -> Error e
type DisposableStopwatch(stopwatch:Stopwatch) =
  interface IDisposable with
    member x.Dispose() = stopwatch.Stop()
  member x.Elapsed = stopwatch.Elapsed
  
  
type MidiFileParseTestOutcome =
  | RoundTripOk
  | RoundTripDiff
  | RoundTripParseError
 // | ParseFailure
  | ExpectedError
  | UnexpectedError
  | ExpectedErrorMismatch

[<Tests>]
let tests =
  testList "unit" [

  for Fileset(name, kind, uri) in fileSets do
    downloadFileSet dir uri name kind

  let enumFiles () = enumerateFilesetMidiFiles (fun (i,_) ->i)
  let tasks =
    [|
      for i, file in enumFiles() do
        async {
          return
              (i,file), (File.ReadAllBytes file.FullName) 
        }
    |]
  let files =
        seq {
          for f in tasks |> Seq.chunkBySize 16 do
             let results =
               f
               |> Async.Parallel
               |> Async.RunSynchronously
             yield! results
        }
    
  let timingScope () =
    let stopwatch = Stopwatch()
    stopwatch.Start()
    new DisposableStopwatch(stopwatch)
  
  let performForFile i (f: FileInfo) bytes =
      let strippedFilename = f.FullName.Replace(dir.FullName,"").Replace("\\","/").Substring(1)
      match runParser midiFile bytes State.initial with
      | Ok(midiFile, parseState) ->
        let readMidiFile = runParser ReadFile.midiFile bytes State.initial
        match readMidiFile with
        | Error(ParseError(position, errMsg)) ->
          printfn $"couldn't read back {f.Directory.Name} {f.Name}: {position}/{bytes.Length} {errMsg}"
          RoundTripParseError
        | Ok (readMidiFile, parserState) ->
          if readMidiFile.header <> midiFile.header then
            printfn $"{f.Directory.Name} {f.Name} headers differs!"
            RoundTripDiff
          elif readMidiFile.tracks.Length <> midiFile.tracks.Length then
            printfn $"{f.Directory.Name} {f.Name} tracks count differs!"
            RoundTripDiff
          else
            let differences =
              [|
                use scope = timingScope()
                for (i,(t1, t2)) in Seq.zip readMidiFile.tracks midiFile.tracks |> Seq.indexed do
                  let differences =
                    Seq.zip t1 t2
                    |> Seq.indexed
                    |> Seq.filter (fun (i,(a,b)) ->
                     if a <> b then
                       let a = a.event
                       let b = b.event
                       match a,b with
                       | (VoiceEvent(_,a)), (VoiceEvent(_,b)) -> a <> b
                       | a, b -> a <> b
                     else
                       false
                    )
                    |> Seq.truncate 1
                    |> Seq.toArray
                  for d in differences do
                    i, d
                if scope.Elapsed > TimeSpan.FromMilliseconds 50 then
                  printfn $"spent {scope.Elapsed.TotalMilliseconds}ms comparing {f.FullName}"
              |]
            if differences.Length = 0 then
              RoundTripOk
            else
              let message =
                differences
                |> Array.map (fun (track, (i,(a,b))) -> $"{f.Directory.Name} {f.Name}at track {track} index {i}:\n{a}\n<>\n{b}")
                |> String.concat Environment.NewLine
              printfn $"parse roundtrip diff:-------------------------------------------////////\n{message}"
              RoundTripDiff
      | Error error ->
        match expectedFailures.TryGetValue strippedFilename with
        | true, expected ->
          if expected <> error then
            printfn "file %s had error: %A" f.FullName error
            ExpectedErrorMismatch
          else
            ExpectedError
        | false, _ ->
          printfn $"failed to parse unexpectedly {i} {f.FullName}."
          let bytes = File.ReadAllBytes f.FullName
          let naudio = Parse.naudioParse bytes
          let drywet = Parse.drywetMidiParse bytes
          match naudio, drywet with
          | Ok _, _ | _, Ok _ ->
            printfn $"file %s{f.FullName} had error: %A{error}\ndrywet:%A{drywet}\nnaudio:{(string naudio) |> Seq.truncate 1000 |> Seq.toArray |> String}"
            UnexpectedError
          | Error naudio, Error drywet ->
            File.AppendAllLines(errortext, Array.singleton $"\"{strippedFilename}\", %A{error}")
            printfn $"please add for {f.FullName} as both naudio and drywet parser also failed; error: {(string naudio) |> Seq.truncate 1000 |> Seq.toArray |> String}"
            ExpectedError
  
  let failedParses = ResizeArray()
  let failedRoundTrips = ResizeArray()
  
  for (i,f), bytes in files do
    let outcome = performForFile i f bytes
    match outcome with
    | ExpectedError
    | RoundTripOk -> ()
    | RoundTripDiff | RoundTripParseError ->
      failedRoundTrips.Add ((i,f,outcome))
    | ExpectedErrorMismatch
    | UnexpectedError
      ->
      failedParses.Add ((i,f,outcome))
    
  test "parse all files" {
    let errors =
      [|
        for i,f,outcome in failedParses do
          let strippedFilename = f.FullName.Replace(dir.FullName,"").Substring(1)
          yield $"{i} {strippedFilename} {outcome}"
      |]    
    if errors.Length > 0 then
      failtest $"{failedParses.Count} failed file(s) present:\n {errors |> String.concat Environment.NewLine}"
  }

  test "parsed files written and parsed back gives equal object" {
    
    let issues = [|
      for i,f,outcome in failedRoundTrips do
        let strippedFilename = f.FullName.Replace(dir.FullName,"").Substring(1)
        yield $"{i} {strippedFilename} {outcome}"
    |]
    if issues <> Array.empty then
      failtest (String.concat Environment.NewLine issues)
    
  }
  test "parseVarlen" {
      let cases =
        
        [|
          // http://www.music.mcgill.ca/~ich/classes/mumt306/StandardMIDIfileformat.html
          {| expected = 0x00000000u; input = [|0x00uy|] |}
          {| expected = 0x00000040u; input = [|0x40uy|] |}
          {| expected = 0x0000007fu; input = [|0x7fuy|] |}
          {| expected = 0x00000080u; input = [|0x81uy; 0x00uy|] |}
          {| expected = 0x00002000u; input = [|0xc0uy; 0x00uy|] |}
          {| expected = 0x00003fffu; input = [|0xffuy; 0x7fuy|] |}
          {| expected = 0x00004000u; input = [|0x81uy; 0x80uy; 0x00uy|] |}
          {| expected = 0x00100000u; input = [|0xc0uy; 0x80uy; 0x00uy|] |}
          {| expected = 0x001fffffu; input = [|0xffuy; 0xffuy; 0x7fuy|] |}
          {| expected = 0x00200000u; input = [|0x81uy; 0x80uy; 0x80uy; 0x00uy|] |}
          {| expected = 0x08000000u; input = [|0xc0uy; 0x80uy; 0x80uy; 0x00uy|] |}
          {| expected = 0x0fffffffu; input = [|0xffuy; 0xffuy; 0xffuy; 0x7fuy|] |}
          // not a good sample, the initial implementation passed it but failed some other values (480...)
          
        |]

      let state = State.initial

      let failures =
        [|
          for case in cases do
        
          match runParser getVarlen case.input state with
          | Ok (result,_) ->
            if result <> case.expected then
              case, result
          | Error e ->
            failwith $"{e}"
        |]

      if failures.Length > 0 then
        let message = 
          sprintf "%i failure(s):%s" 
            failures.Length
            (
              System.Environment.NewLine
              + (failures 
                    |> Array.map 
                        (fun (e, result) ->
                          sprintf "exptected: %0x for %s, got %A" 
                            e.expected
                            (e.input |> Array.map (sprintf "%0x") |> String.concat " ")
                            result
                        ) |> String.concat System.Environment.NewLine
              
                    )
                  )
        failwithf "%s" message
  }

  test "isomorphisms" {
    
    let testIso name (a,b) testValue =
      let ra,rb = a testValue, a testValue |> b |> a
      if ra <> rb then
        Error (name, $"%A{ra} <> %A{rb}")
      else
        Ok ()
        
    let results =
      [|
        // remark: need to do it with FsCheck?
        testIso "word32be" Isomorphisms.word32be 1000u
        testIso "word24be" Isomorphisms.word24be 1000u
        testIso "word16be" Isomorphisms.word16be 1000us
        testIso "word14be" Isomorphisms.word14be (word14(1000us))
        testIso "midiVoiceEvent" Isomorphisms.midiVoiceEvent (MidiVoiceEvent.PitchBend(0uy, word14(64us)))
        testIso "midiVoiceEvent" Isomorphisms.midiVoiceEvent (MidiVoiceEvent.PitchBend(1uy, word14(1200us)))
        testIso "midiMetaEvent" Isomorphisms.metaEvent MidiMetaEvent.EndOfTrack
        testIso "midiMetaEvent" Isomorphisms.metaEvent (SequenceNumber 514us)
        testIso "deltatime" Isomorphisms.deltaTime (DeltaTime 480u)
      |]
    
    let failures = results |> Array.choose (function Error (name, values) -> Some ($"{name}: {values}") | _ -> None)
    if failures.Length > 0 then
      failtest (String.concat Environment.NewLine failures)
  
  }
  ]

[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args tests