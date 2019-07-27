module ZMidi.Tests
open Expecto
open ZMidi.Internal.ParserMonad
open ZMidi.ReadFile
open ZMidi.DataTypes

let tests =
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


[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args tests