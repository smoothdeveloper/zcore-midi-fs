module Program

open System.IO
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Engines
open BenchmarkDotNet.Running
open ZMidi
open ZMidi.Tests
open ZMidi.Tests.Tests
    
[<MemoryDiagnoser>]
[<MarkdownExporter; AsciiDocExporter; HtmlExporter; CsvExporter; RPlotExporter>]
[<SimpleJob(RunStrategy.ColdStart, launchCount= 1)>]

type ReadFilesBench() =
    let ff =
        let r = System.Random(1)
        printfn "reading files..."
        let files = Tests.enumerateFilesetMidiFiles(fun _ -> r.Next()) |> Seq.truncate 108
        printfn "reading files done."
        
        files
    
    [<Benchmark(Baseline=true)>]
    member _.BuiltIn () =
        for _,f in ff do
            Parse.zmidiParse (File.ReadAllBytes f.FullName) |> ignore
    [<Benchmark>]
    member _.DryWetMidi () =
        for _,f in ff do
            Parse.drywetMidiParse (File.ReadAllBytes f.FullName) |> ignore

    [<Benchmark>]
    member _.NAudio () =
        for _,f in ff do
            Parse.naudioParse (File.ReadAllBytes f.FullName) |> ignore

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<ReadFilesBench>() |> ignore
    0
    

