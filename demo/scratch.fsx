//#load "../src/zmidi/datatypes.fs"
//#load "../src/zmidi/extratypes.fs"
//#load "../src/zmidi/internal/utils.fs"
//#load "../src/zmidi/internal/parsermonad.fs"
//#load "../src/zmidi/read.fs"
#r "../build/Debug/AnyCPU/net45/zmidi-fs-core.dll"


open System.IO
open ZMidi.Internal.ParserMonad
open ZMidi

let folder = 
  Path.Combine(__SOURCE_DIRECTORY__ , ".." , "data", "midifiles")
  |> DirectoryInfo

for file in folder.EnumerateFiles() do
  let buffer = File.ReadAllBytes file.FullName

  let parseResult = 
    ZMidi.Internal.ParserMonad.runParser
      ZMidi.ReadFile.midiFile
      buffer
      State.initial


  printfn "%s" file.FullName 

  match parseResult with 
  | Ok result -> 
    
    printfn "%i tracks" result.tracks.Length
    for t in result.tracks do
      t.Length
      printfn "track: %A" t
    ()
  | Error something -> printfn "ERR: %s %A" file.FullName something


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
         
  let! _ = assertString "MTrk"
  let! l = readUInt32be
  let! t = deltaTime
  let! e = event
  return (l,t,e)
  //let! track1 = track
  //return track1
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
