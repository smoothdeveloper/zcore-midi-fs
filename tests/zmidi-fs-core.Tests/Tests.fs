module ZMidi.Tests.Tests

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Threading
open CsvHelper
open CsvHelper.Configuration
open Expecto
open ZMidi.Internal
open ZMidi.Internal.ParserMonad
open ZMidi.Tests.Infrastructure
open ZMidi.Tests.ExpectedFailures
open ZMidi
open ZMidi.ReadFile
open ZMidi.DataTypes
open ZMidi.WriteFile


let csvFilename =
  let now = DateTime.Now
  FileInfo(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "tests", "runs", $"parsealltests/%04i{now.Year}.%02i{now.Month}.%02i{now.Day}.%02i{now.Hour}.%02i{now.Minute}.%02i{now.Second}/outcome.csv"))



type TestCsvReportRecord =
  {
    relativeFilename: string
    parseMillisecondsZMidi: decimal
    parseMillisecondsDryWet: decimal
    parseMillisecondsNAudio: decimal
    parsedStatusZMidi: bool
    parsedStatusDryWet: bool
    parsedStatusNAudio: bool
  }
  

type TestCsvReport() =
  let writer =
    csvFilename.Directory.Create()
    new StreamWriter(csvFilename.FullName)
  let config = CsvConfiguration CultureInfo.InvariantCulture
  let csvWriter = new CsvWriter(writer, config)
  let gate = obj()
  do
    [| "file"; "parse ms ZMidi"; "parse ms DryWet"; "parse ms NAudio" |]
    |> Array.iter csvWriter.WriteField
    csvWriter.NextRecord()
    
  let mutable recCount = 0
  member x.AppendResult resultRecord =
    let csvRecord =
      [|
          resultRecord.relativeFilename
          string resultRecord.parseMillisecondsZMidi
          string resultRecord.parseMillisecondsDryWet
          string resultRecord.parseMillisecondsNAudio
          string resultRecord.parsedStatusZMidi
          string resultRecord.parsedStatusDryWet
          string resultRecord.parsedStatusNAudio
        |]
  
    let recCount = Interlocked.Increment(&recCount)
    csvRecord
    |> Array.iter csvWriter.WriteField
    csvWriter.NextRecord()
    
    if recCount % 100 = 0 then
      csvWriter.Flush()
  
  
    
  interface IDisposable with
    member x.Dispose () =
      csvWriter.Dispose()
      writer.Dispose()

type Fileset = Fileset of name: string * ArchiveKind * uri: string 

let fileSets = [
  Fileset("maestro-v2.0.0-midi", Zip  , "https://storage.googleapis.com/magentadata/datasets/maestro/v2.0.0/maestro-v2.0.0-midi.zip")
  Fileset("magentadata-groove" , Zip  , "https://storage.googleapis.com/magentadata/datasets/groove/groove-v1.0.0-midionly.zip")
  Fileset("magentadata-egmd"   , Zip  , "https://storage.googleapis.com/magentadata/datasets/e-gmd/v1.0.0/e-gmd-v1.0.0-midi.zip")
  Fileset("lmd_full"           , TarGz, "http://hog.ee.columbia.edu/craffel/lmd/lmd_full.tar.gz")
]
      
let dir = DirectoryInfo(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "data"))
type DisposableStopwatch(stopwatch:Stopwatch) =
  interface IDisposable with
    member x.Dispose() = stopwatch.Stop()
  member x.Elapsed = stopwatch.Elapsed
let timingScope () =
  let stopwatch = Stopwatch()
  stopwatch.Start()
  new DisposableStopwatch(stopwatch)

let errortext = Path.Combine(dir.FullName, "errortext.txt")
let enumerateFilesetMidiFiles sort =
    seq {
      yield! dir.EnumerateFiles("*.midi", SearchOption.AllDirectories)
      yield! dir.EnumerateFiles("*.mid", SearchOption.AllDirectories) 
    }
    |> Seq.indexed
    |> Seq.sortBy sort

module Parse =
    let inline tryTimeScoped f =
      use scope = timingScope ()
      try
        f () |> Ok, scope.Elapsed
      with 
        e ->
          Error e, scope.Elapsed
          
    let zmidiParse bytes =
        try
          runParser ReadFile.midiFile bytes State.initial
        with
          e -> Error(ParseError.ParseError(Pos.MinValue, ErrMsg.Exn e))

    let drywetMidiParse (bytes: _ array) =
        use ms = new MemoryStream(bytes)
        tryTimeScoped <| fun () -> Melanchall.DryWetMidi.Core.MidiFile.Read(ms)
    let naudioParse (bytes: _ array) =
        use ms = new MemoryStream(bytes)
        tryTimeScoped <| fun () -> NAudio.Midi.MidiFile(ms, true)
  
  
type Lib = ZMidi | NAudio | DryWet
type ElapsedTimeKind =
  | ParseToBytes of bytes: byte array
  | ParseToMidi of Lib
  | WriteBackToMidi
  | ReadBackToMidi
  | CompareWrittenBack
  
type MidiFileParseTestOutcome =
  | RoundTripOk
  | RoundTripDiff
  | RoundTripParseError
  | ExpectedError
  | UnexpectedError
  | ExpectedErrorMismatch

[<Tests>]
let tests =
  testList "unit" [
  use csv = new TestCsvReport()
  for Fileset(name, kind, uri) in fileSets do
    downloadFileSet dir uri name kind
  
  
  let resultR = ResizeArray()
  let inline pushProcessEntry valueKind timedScope result =
    resultR.Add((valueKind, timedScope))
    result, timedScope
    
  let enumFiles () = enumerateFilesetMidiFiles (fun (i,_) ->i)
  let files =
    seq {
      for i, file in enumFiles() do
        let timingScope = timingScope ()
        let bytes = File.ReadAllBytes file.FullName
        (i,file), (bytes, timingScope.Elapsed) 
    }
  
  let compareResult midiFile readMidiFile filename =
    if readMidiFile.header <> midiFile.header then
      printfn $"{filename} headers differs!"
      RoundTripDiff
    elif readMidiFile.tracks.Length <> midiFile.tracks.Length then
      printfn $"{filename} tracks count differs!"
      RoundTripDiff
    else
      let differences =
        [|
          use scope = timingScope()
          for i,(t1, t2) in Seq.zip readMidiFile.tracks midiFile.tracks |> Seq.indexed do
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
            printfn $"spent {scope.Elapsed.TotalMilliseconds}ms comparing {filename}"
        |]
      if differences.Length = 0 then
        RoundTripOk
      else
        let message =
          differences
          |> Array.map (fun (track, (i,(a,b))) -> $"{filename}at track {track} index {i}:\n{a}\n<>\n{b}")
          |> String.concat Environment.NewLine
        printfn $"parse roundtrip diff:-------------------------------------------////////\n{message}"
        RoundTripDiff

  let performForFile i (f: FileInfo) bytes =
    let strippedFilename = f.FullName.Replace(dir.FullName,"").Replace("\\","/").Substring(1)
    let parseResult, initialParseElapsed =
      use timedScope = timingScope ()
      runParser midiFile bytes State.initial
      |> pushProcessEntry (ParseToMidi ZMidi, f) timedScope.Elapsed
    let result =
      { relativeFilename = strippedFilename
        parsedStatusDryWet = false
        parsedStatusNAudio = false
        parsedStatusZMidi = false
        parseMillisecondsDryWet = 0m
        parseMillisecondsNAudio = 0m
        parseMillisecondsZMidi = decimal initialParseElapsed.TotalMilliseconds
      }
    let naudio, parseNaudio = Parse.naudioParse bytes
    let drywet, parseDryWet = Parse.drywetMidiParse bytes
          
    let result =
      let result =
        match naudio with
        | Error _ -> { result with parsedStatusNAudio = false }
        | Ok _ -> { result with parsedStatusNAudio = true }
      let result =
        match drywet with
        | Error _ -> { result with parsedStatusDryWet = false }
        | Ok _ -> { result with parsedStatusDryWet = true }
      let result = { result with parseMillisecondsDryWet = decimal parseDryWet.TotalMilliseconds }
      let result = { result with parseMillisecondsNAudio = decimal parseNaudio.TotalMilliseconds }
      result
  
    match parseResult with
    | Ok(midiFile, parseState) ->
      let result =
        { result with
            parsedStatusZMidi = true
        }
      let bytes, _ =
          use timingScope = timingScope()
          PutOps.putMidiFile midiFile
          |> WriterMonad.PutOp.toBytes
          |> pushProcessEntry (WriteBackToMidi,f) timingScope.Elapsed
          
      let readMidiFile, elapsed =
        use timingScope = timingScope ()
        runParser ReadFile.midiFile bytes State.initial
        |> pushProcessEntry (ReadBackToMidi,f) timingScope.Elapsed
        
      match readMidiFile with
      | Error(ParseError(position, errMsg)) ->
        printfn $"couldn't read back {strippedFilename}: {position}/{bytes.Length} {errMsg}"
        RoundTripParseError, result
      | Ok (readMidiFile, parserState) ->
        use timingScope = timingScope ()
        (compareResult midiFile readMidiFile strippedFilename
        |> pushProcessEntry (CompareWrittenBack,f) timingScope.Elapsed
        |> fst), result
        
    | Error error ->
        
      match expectedFailures.TryGetValue strippedFilename with
      | true, expected ->
        if expected <> error then
          printfn $"file %s{strippedFilename} had error: %A{error}"
          ExpectedErrorMismatch, result
        else
          ExpectedError, result
      | false, _ ->
        printfn $"failed to parse unexpectedly {i} {strippedFilename}."
        match naudio, drywet with
        | Ok _, Error drywet ->
          printfn $"%s{strippedFilename} DryWet had error: %A{drywet}"
          UnexpectedError, result
        | Error naudio, Ok _ ->
          printfn $"%s{strippedFilename} NAudio had error: %A{naudio}"
          UnexpectedError, result
        | Error naudio, Error drywet ->
          File.AppendAllLines(errortext, Array.singleton $"\"{strippedFilename}\", %A{error}")
          printfn $"please add for {strippedFilename} as both naudio and drywet parser also failed; error: drywet=============\n %A{drywet}\n naudio==========\n %A{naudio}"
          ExpectedError, result
        | Ok _, Ok _ ->
          UnexpectedError, result
  let failedParses = ResizeArray()
  let failedRoundTrips = ResizeArray()

  for (i,f), (bytes, ioreadtobytestimespan) in files do
    if ioreadtobytestimespan > TimeSpan.FromMilliseconds 500 then
      printfn $"taking {ioreadtobytestimespan.TotalMilliseconds}ms to read {f.FullName}"
    let outcome, result = performForFile i f bytes
      
    csv.AppendResult(result)
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