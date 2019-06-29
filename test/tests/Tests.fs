module ZMidi.Tests
open Expecto
open ZMidi.Internal.ParserMonad
open ZMidi.ReadFile

let tests =
  test "parseVarlen" {
      let cases = 
        [|
          {| input = [| 0x00uy |]; expected = 0u |}
          {| input = [| 0x7fuy |]; expected = 127u |}
          {| input = [| 0x80uy |]; expected = 128u |}
          {| input = [| 0x03uy; 0xe8uy |]; expected = 1000u |}
          {| input = [| 0x3fuy; 0xffuy |]; expected = 16383u |}
          {| input = [| 0x0fuy; 0x42uy; 0x40uy |]; expected = 100000u |}
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