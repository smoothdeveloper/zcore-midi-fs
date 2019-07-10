#load "../src/zmidi/datatypes.fs"
#load "../src/zmidi/extratypes.fs"
#load "../src/zmidi/internal/utils.fs"
#load "../src/zmidi/internal/parsermonad.fs"
#load "../src/zmidi/read.fs"
//#r "../build/Debug/AnyCPU/net45/zmidi-fs-core.dll"
open System.IO
open ZMidi.Internal.ParserMonad
(*
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

  *)
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

msbHigh 0x80uy &&& 0x7fuy