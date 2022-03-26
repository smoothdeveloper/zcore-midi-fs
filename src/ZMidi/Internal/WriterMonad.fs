namespace ZMidi.Internal

open System.IO

module WriterMonad = // "monad"
    let debug = false
    let inline logf format =
        if debug then
            printfn format
        else
            // is there a way to do noop here?
            fprintf StreamWriter.Null format
 
    type PutOp =
        | PutByte of byte
        | PutBytes of byte array
        | Combined of PutOp * PutOp
        | NoOp
        static member concat putOps =
            Seq.foldBack (fun a nextOp -> Combined(a, nextOp)) putOps NoOp
        static member toBytes putOp =
            let items = ResizeArray()
            let rec write =
                function
                    | NoOp -> ()
                    | PutByte b -> items.Add [| b |]
                    | PutBytes b -> items.Add b
                    | Combined (a,b) ->
                        write a
                        write b

            write putOp
            
            items.ToArray()
            |> Array.concat
            
        static member toWriter (writer: BinaryWriter) putOp =
            
            let rec write =
                function
                | NoOp -> ()
                | PutByte b -> writer.Write b
                | PutBytes b -> writer.Write b
                | Combined (a,b) ->
                    write a
                    write b
                    
            write putOp
    let inline bind putter toWrite f =
        logf "builder: bind"
        f (putter toWrite)

    let inline zero v =
        logf "builder: zero"
        v
 
    let inline delay f =
        logf "builder: delay"
        f ()

    let inline combine writer1 writer2 f =
        logf "builder: combine"
        f (writer1,writer2)
       
    let inline forLoop (inputs : 'a seq) (f : 'a -> 'b) putOp getResult =
        logf "builder: forloop"
        for x in inputs do
            delay (fun () -> f x |> putOp |> ignore) // ignore here makes the for loop returning unit
        getResult ()
                
    type WriteBytesBuilder<'foo,'a,'b> (putter : 'a -> 'b) =
        let putOp (op) =
            logf "builder: put operation %A" op
            putter op

        let mutable retVal = Unchecked.defaultof<'foo>
        member inline _.Zero ()             = zero None
        member inline _.Zero a              = logf "builder: Zero %A" a; failwith "oooosp0000"
        member inline _.Delay f             = delay f
        member _.Combine (writer1, writer2) = combine writer1 writer2 Combined
        member _.Bind (writer, f)           = bind putOp writer f
        member _.Bind (writer: 'a seq, f)           =
            [for w in writer -> bind putOp w f] |> List.last
        member _.For (inputs, f)            = forLoop inputs f putOp (fun _ -> NoOp)
        member _.Return _x                  =
            // not yet monadic
            logf "builder: Return %A" _x 
            retVal <- _x
            NoOp
        member this.Run a =
            logf "builder: Run %A" a
            retVal
            
    let writeBytes = WriteBytesBuilder
    
    let runWriter (writer: WriteBytesBuilder<_,_,_>) =
        writer.Run ()
    