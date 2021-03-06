module ZMidi.Tests
open System.IO
open Expecto
open ZMidi.Internal.ParserMonad
open ZMidi.ReadFile
open ZMidi.DataTypes
open System.Net
open System.IO.Compression

let downloadFileSet (directory: DirectoryInfo) =
    directory.Create()
    let archive = Path.Combine(directory.FullName, "maestro-v2.0.0-midi.zip")
    let client = new WebClient()
    
    client.DownloadFile(@"https://storage.googleapis.com/magentadata/datasets/maestro/v2.0.0/maestro-v2.0.0-midi.zip", archive)
    ZipFile.ExtractToDirectory(archive, Path.Combine(directory.FullName, "maestro-v2.0.0-midi"))
    File.Delete archive

[<Tests>]
let tests =
  testList "unit" [

  test "parse all files" {
      let dir = DirectoryInfo(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "data"))
      if not dir.Exists then
        downloadFileSet dir
      let files = 
          [|
              yield! dir.GetFiles("*.midi", SearchOption.AllDirectories)
              yield! dir.GetFiles("*.mid", SearchOption.AllDirectories)
          |]
      let okFiles, errorFiles =
        [|
          for f in files do
            let bytes = File.ReadAllBytes f.FullName
            f,runParser
                midiFile
                bytes
                State.initial
        |]
        |> Array.partition (function (_,Ok _) -> true | (_,Error _) -> false)
      printfn "%i files OK, %i files not OK" okFiles.Length errorFiles.Length
      for f, error in errorFiles do
          printfn "file %s had error: %A" f.FullName error
      if errorFiles.Length > 0 then
        failtest "failed files present"
  }

  test "parseVarlen" {
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

      let state = State.initial

      let failures =
        [|
          for case in cases do
        
          let result = runParser getVarlen case.input state
          if result <> (Ok (case.expected)) then
            yield case, result
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

  ]

[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args tests