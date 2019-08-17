

//#load "../src/zmidi/datatypes.fs"
//#load "../src/zmidi/extratypes.fs"
//#load "../src/zmidi/internal/utils.fs"
//#load "../src/zmidi/internal/parsermonad.fs"
//#load "../src/zmidi/read.fs"
#r "../build/Release/AnyCPU/net45/zmidi-fs-core.dll"
open System.IO
open ZMidi.Internal.ParserMonad
open ZMidi
//ZMidi.Internal.ParserMonad.debug <- true
(*


 
let pp = parseMidi {
  return boundRepeat 10 readByte
}


let d = Seq.initInfinite (fun i -> byte (i % 255)) |> Seq.truncate 10 |> Seq.toArray 
let s = State.initial

let (Ok(ParserMonad f)) = (ZMidi.Internal.ParserMonad.runParser pp d s)
f d s
*)

let folder = 
  Path.Combine(__SOURCE_DIRECTORY__ , ".." , "data", "midifiles")
  |> DirectoryInfo

for file in folder.EnumerateFiles() do
  let buffer = File.ReadAllBytes file.FullName
  printfn "======================= %s" file.FullName 
  let watch = System.Diagnostics.Stopwatch.StartNew()
  let parseResult = 
    ZMidi.Internal.ParserMonad.runParser
      ZMidi.ReadFile.midiFile
      buffer
      State.initial
  printfn "ellapsed parse time : %A" watch.Elapsed
  match parseResult with 
  | Ok result -> 
    
    printfn "%i tracks" result.tracks.Length
    for t in result.tracks do
      printfn "track: %A events" t.Length
    
  | Error something -> printfn "ERR: %s %A" file.FullName something

(*
let cases = 
  [|
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
  |]
for case in cases do
  let state = State.initial

  let result = runParser ZMidi.ReadFile.getVarlen case.input state
  printfn "%A" result

do 
  printfn "hello"; 
  let a = 1; 
  printfn "world %i" a;

//do printfn "hello"; let a = 1; printfn "world %i" a;








let d = [|    0xffuy
              0x08uy
              0x0euy
              0x41uy
              0x6Duy
              0x61uy
              0x7Auy
              0x69uy
              0x6euy
              0x67uy
              0x20uy
              0x47uy
              0x72uy
              0x61uy
              0x63uy
              0x65uy
              0x00uy
              |]

let p = ZMidi.ReadFile.event 
let s = State.initial

let (Ok(ParserMonad f)) = (ZMidi.Internal.ParserMonad.runParser p d s)
f d s


open ZMidi.ReadFile
open ZMidi.DataTypes
let p = parseMidi {
  let! _ = assertString "MThd"
  let! _ = assertWord32 6u
  let! format = fileFormat
  let! trackCount = readUInt16be
  let! timeDiv = timeDivision
  let header =
         { trackCount = trackCount
           timeDivision = timeDiv
           format = format }
         
  return! track
}    
  
  

for file in folder.EnumerateFiles() do
  let buffer = File.ReadAllBytes file.FullName

  let parseResult = 
    ZMidi.Internal.ParserMonad.runParser
      p
      buffer
      State.initial


  printfn "%s" file.FullName 

  match parseResult with 
  | Ok result -> 
    printfn "%A" result
  | Error something -> printfn "ERR: %s %A" file.FullName something



let pp = parseMidi {
  return boundRepeat 5 readByte
}

let (Ok(ParserMonad f)) = (ZMidi.Internal.ParserMonad.runParser pp [|1uy..5uy|] State.initial)

f [|1uy..5uy|] State.initial*)