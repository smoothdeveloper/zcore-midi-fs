
namespace ZMidi.Internal

module ParserMonad = 

    open System.IO

    open ZMidi.Internal.Utils

    /// Status is either OFF or the previous VoiceEvent * Channel.
    type VoiceEvent = 
        | StatusOff
        | NoteOn            of byte
        | NoteOff           of byte
        | NoteAftertoucuh   of byte
        | Control           of byte
        | Program           of byte
        | ChannelAftertouch of byte
        | PitchBend         of byte
        override x.ToString() =
          match x with
          | StatusOff           -> "StatusOff"
          | NoteOn            o -> "NoteOn "            + string o
          | NoteOff           o -> "NoteOff "           + string o
          | NoteAftertoucuh   o -> "NoteAftertoucuh "   + string o
          | Control           o -> "Control "           + string o
          | Program           o -> "Program "           + string o
          | ChannelAftertouch o -> "ChannelAftertouch " + string o
          | PitchBend         o -> "PitchBend "         + string o

    type MidiData = byte array

    type Pos = int

    type ErrMsg = 
      | EOF of where: string
      | Other of error: string
      | Exn of error: exn
      
    type State = 
        { Position: Pos 
          RunningStatus: VoiceEvent
          #if DEBUG_LASTPARSE
          LastParse : obj
          #endif
        }
        static member initial =
            { Position = 0
              RunningStatus = VoiceEvent.StatusOff
              #if DEBUG_LASTPARSE
              LastParse = null
              #endif
            }
        override x.ToString () =
            #if DEBUG_LASTPARSE
            sprintf "(Pos:%i;Status:%30s;LastParse:%50s)" x.Position (sprintf "%A" x.RunningStatus)
                
                (sprintf "%A" x.LastParse)
            #else
            System.String.Format("(Pos:{0};Status:{1})", x.Position, string x.RunningStatus)
            #endif
    type ParseError = 
      ParseError of 
        position: Pos 
        * message: ErrMsg 
        #if DEBUG_LASTPARSE
        * lastToken : obj // need top level type, picking System.Object for now
        #endif

    let inline mkOtherParseError st (genMessage : Pos -> string) =
      ParseError(
        st.Position
        , Other (genMessage st.Position)
        #if DEBUG_LASTPARSE
        , st.LastParse
        #endif
      )

    let inline mkParseError st (errMsg: ErrMsg) =
      ParseError(
        st.Position
        , errMsg
        #if DEBUG_LASTPARSE
        , st.LastParse
        #endif
      )

    type ParserMonad<'a> = 
        ParserMonad of (MidiData -> State -> Result<'a * State, ParseError> )

    let nullOut = new StreamWriter(Stream.Null) :> TextWriter
    let mutable debug = false
    let logf format = //format: Printf.TextWriterFormat<'a>) =
          if debug then
            printfn format
          else
            fprintfn nullOut format
        
    let inline private apply1 (parser : ParserMonad<'a>) 
                       (midiData : byte[])
                       (state : State)  :  Result<'a * State, ParseError> = 
        let (ParserMonad fn) = parser 
        try
            let result = fn midiData state
            let oldState = state
            match result with
            | Ok (r, state) ->
              if debug then
                if state <> oldState then
                    logf "parse ok: %O" state
              #if DEBUG_LASTPARSE
              let state = { state with LastParse = r }
              #endif
              Ok (r, state)
            | Error e ->
                logf "parse error: %50s %O" (sprintf "%A" e) state
                Error e
        with e ->
            logf "parse FATAL error: %50s %O" (sprintf "%A" e) state
            Error (
                      
                      ParseError(
                                    state.Position
                                    , ErrMsg.Exn e
                                    #if DEBUG_LASTPARSE
                                    , state.LastParse
                                    #endif
                                )
                  )
            
    let inline mreturn (x:'a) : ParserMonad<'a> = 
        ParserMonad <| fun _ st -> Ok (x, st)

    let inline private bindM (parser : ParserMonad<'a>) 
                      (next : 'a -> ParserMonad<'b>) : ParserMonad<'b> = 
        ParserMonad <| fun input state -> 
            match apply1 parser input state with
            | Error msg -> Error msg
            | Ok (ans, st1) -> apply1 (next ans) input st1

    let mzero () : ParserMonad<'a> = 
        ParserMonad <| fun _ state -> Error (mkParseError state (EOF "mzero"))

    let inline mplus (parser1 : ParserMonad<'a>) (parser2 : ParserMonad<'a>) : ParserMonad<'a> = 
        ParserMonad <| fun input state -> 
            match apply1 parser1 input state with
            | Error _ -> apply1 parser2 input state
            | Ok res -> Ok res

    let inline private delayM (fn:unit -> ParserMonad<'a>) : ParserMonad<'a> = 
        bindM (mreturn ()) fn 

    let inline mfor (items: #seq<'a>) (fn: 'a -> ParserMonad<'b>) : ParserMonad<seq<'b>> = failwithf ""


    let (>>=) (m: ParserMonad<'a>) (k: 'a -> ParserMonad<'b>) : ParserMonad<'b> =
      bindM m k
   
    type ParserBuilder() = 
        member inline self.ReturnFrom (ma:ParserMonad<'a>) : ParserMonad<'a> = ma
        member inline self.Return x         = mreturn x
        member inline self.Bind (p,f)       = bindM p f
        member inline self.Zero a          = ParserMonad (fun input state -> Ok(a, state))
        //member self.Combine (ma, mb) = ma >>= mb

        // inspired from http://www.fssnip.net/7UJ/title/ResultBuilder-Computational-Expression
        // probably broken
        member inline self.TryFinally(m, compensation) =
             try self.ReturnFrom(m)
             finally compensation()
        
        //member self.Delay(f: unit -> ParserMonad<'a>) : ParserMonad<'a> = f ()
        //member self.Using(res:#System.IDisposable, body) =
        //      self.TryFinally(body res, fun () -> if not (isNull res) then res.Dispose())
        //member self.While(guard, f) =
        //       if not (guard()) then self.Zero () else
        //       do f() |> ignore
        //       self.While(guard, f)
        //member self.For(sequence:seq<_>, body) =
        //       self.Using(sequence.GetEnumerator(), fun enum -> self.While(enum.MoveNext, fun () -> self.Delay(fun () -> body enum.Current)))

    let (parseMidi:ParserBuilder) = new ParserBuilder()

    let runParser (ma:ParserMonad<'a>) input initialState =
      apply1 ma input initialState
      |> Result.map fst

    /// Run the parser on a file.
    let runParseMidi (ma : ParserMonad<'a>) (inputPath : string) : Result<'a, ParseError> = 
        runParser ma (File.ReadAllBytes inputPath) State.initial


    /// Throw a parse error
    let parseError (genMessage : Pos -> string) : ParserMonad<'a> = 
        ParserMonad <| fun _ st -> Error (mkOtherParseError st genMessage)

    /// Run the parser, if it fails swap the error message.
    let inline ( <??> ) (parser : ParserMonad<'a>) (genMessage : Pos -> string) : ParserMonad<'a> = 
        ParserMonad <| fun input st -> 
            match apply1 parser input st with
            | Ok result -> Ok result
            | Error e ->
                logf "oops <??>: e:%A" e
                Error(mkOtherParseError st genMessage)

    ///
    let fmap (f: 'a -> 'b) (p: ParserMonad<'a>) : ParserMonad<'b> =
      parseMidi {
        let! a = p
        return (f a)
      }
    let inline ( <~> (* <$> *) ) (a) b = fmap a b
    let ( *> ) (a: ParserMonad<'a>) (b: 'a -> ParserMonad<'b>) : ParserMonad<'b> = 
      parseMidi {
        let! a = a
        return! (b a)
      }

    // http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#%3C%24
    /// Replace all locations in the input with the same value.
    /// The default definition is @'fmap' . 'const'@, but this may be
    /// overridden with a more efficient version.
    let inline ( <~ (* <$ *) ) (a: 'a) (b: ParserMonad<'b>) : ParserMonad<'a> =
      (*let konst k _ = k
      let x = fmap a b
      konst x b*)
      failwithf ""
      //(fmap >> konst) a b
    
    /// Sequence actions, discarding the value of the first argument.
    //let liftA2 f x = (<*>) (fmap f x)

    //let ( <*> ) = liftA2 id
    //let ( *> ) a1 a2 =
    //
    //  (id <~ a1) <*> a2
    //  
      

    let fatalError err =
      ParserMonad <| fun _ st -> Error (mkParseError st err)

    let getRunningEvent : ParserMonad<VoiceEvent> = 
        ParserMonad <| fun _ st -> Ok (st.RunningStatus , st)

    let inline setRunningEvent (runningStatus : VoiceEvent) : ParserMonad<unit> = 
        ParserMonad <| fun _ st -> Ok ((),  { st with RunningStatus = runningStatus })

    let getPos : ParserMonad<int> =
        ParserMonad <| fun _ st -> Ok (st.Position, st)

    let inline private (|PositionValid|PositionInvalid|) (input: MidiData, state: State) =
      if state.Position >= 0 && state.Position < input.Length then
        PositionValid
      else
        PositionInvalid

    let inline private checkedParseM (name: string) (f: MidiData -> State -> Result<('a * State), ParseError>) =
        ParserMonad 
            (fun input state -> 
                try 
                  match input,state with
                  | PositionValid -> f input state
                  | PositionInvalid -> Error (mkParseError state (EOF name))
                with
                | e -> Error (mkParseError state (Other (sprintf "%s %A" name e)))
            )

    let peek : ParserMonad<byte> =
        checkedParseM "peek" <|
            fun input st -> Ok (input.[st.Position], st)

    /// Conditionally gets a byte (word8). Fails if input is finished.
    /// Consumes data on if predicate succeeds, does not consume if
    /// predicate fails.
    let cond (test : byte -> bool) : ParserMonad<byte option> = 
        checkedParseM "cond" <|
            fun input st ->
                let a1 = input.[st.Position]
                if test a1 then 
                  Ok (Some a1, st) 
                else Ok (None, st)

    /// Repeats a given <see paramref="parser"/> <see paramref="length"/> times.
    /// Fails with accumulated errors when any encountered.
    let inline count (length : ^T) (parser : ParserMonad<'a>) : ParserMonad<'a []> =
        ParserMonad <| fun input state ->
            let rec work (i : 'T) 
                         (st : State) 
                         (fk : ParseError -> Result<'a list * State, ParseError>) 
                         (sk : State -> 'a list  -> Result<'a list * State, ParseError>) = 
                if i <= LanguagePrimitives.GenericZero then 
                    sk st []
                else 
                    match apply1 parser input st with
                    | Error msg -> fk msg
                    | Ok (a1, st1) -> 
                        work (i -  LanguagePrimitives.GenericOne) st1 fk (fun st2 ac -> 
                        sk st2 (a1 :: ac))
            work length state (fun msg -> Error msg) (fun st ac -> Ok (ac, st)) 
                |> Result.map (fun (ans, st) -> (List.toArray ans, st))


    /// Run a parser within a bounded section of the input stream.
    let repeatTillPosition (maxPosition: Pos) (p: ParserMonad<'a>) : ParserMonad<'a array> =
        ParserMonad(fun data state ->
            let results = ResizeArray()
            let mutable firstError = Ok (Array.empty, state) 
            let mutable lastState = state
            let rec loop () =
                if lastState.Position < int maxPosition then
                    match apply1 p data lastState with
                    | Ok (result, state) ->
                        lastState <- state
                        results.Add result
                        loop ()
                    | Error e ->
                        firstError <- Error e

            loop ()
            match firstError with
            | Ok _ ->
                Ok(results.ToArray(), lastState)
            | Error _ ->
                firstError
        )
                        
    let inline boundRepeat (n: ^T) (p: ParserMonad<'a>) : ParserMonad<'a array> =
        ParserMonad(fun data state ->
            let result = Array.zeroCreate (int n)
            let mutable lastState = state
// revisit with a fold?
            let mutable error = Ok (Unchecked.defaultof<_>,lastState)
            let mutable i = LanguagePrimitives.GenericZero
            let mutable errorOccured = false
            logf "bound repeat %i" n

            while i < n && not errorOccured do
                logf "bound repeat %i/%i" i n
  
                match apply1 p data lastState with
                | Ok (item,state) ->
                    lastState <- state
                    result.[int i] <- item
                | (Error e) ->
                    error <- Error e
                    errorOccured <- true
                i <- i + LanguagePrimitives.GenericOne
            if errorOccured then
                error
            else
                Ok (result, lastState)
            )

    /// Apply the parser for /count/ times, derive the final answer
    /// from the intermediate list with the supplied function.
    let inline gencount (plen: ParserMonad<'T>) (p: ParserMonad<'a>) (constr: ^T -> 'a array -> 'answer) : ParserMonad<'answer> =
        parseMidi {
          let! l = plen
          logf "gen count: l: %i" l
          let! items = boundRepeat l p
          return constr l items
        }

    /// Drop a byte (word8).
    let dropByte : ParserMonad<unit> = 
        checkedParseM "dropByte" <| 
            fun input st -> Ok ((), { st with Position = st.Position + 1 })

    /// Parse a byte (Word8).
    let readByte : ParserMonad<byte> = 
        checkedParseM "readByte" <| 
            fun input st ->
                let a1 = input.[st.Position]
                Ok (a1, { st with Position = st.Position + 1 })
              
    /// Parse a single byte char.
    let readChar : ParserMonad<char> = 
        parseMidi { 
            let! a = readByte
            return (char a)
        }

    /// Parse a string of the given length.
    let readString (length : int) : ParserMonad<string> = 
        parseMidi { 
            let! arr = count length readChar
            return (System.String arr)
        }
        <??> sprintf "readString failed at %i"

    /// Parse a uint16 (big endian).
    let readUInt16be : ParserMonad<uint16>= 
        parseMidi { 
            let! a = readByte
            let! b = readByte
            return word16be a b
            }
        <??> sprintf "uint16be: failed at %i"

    /// Parse a word14 (big endian) from 2 consecutive bytes.
    let readWord14be = 
        parseMidi {
            let! a = readByte
            let! b = readByte
            return (word14be a b)
            }
        <??> sprintf "word14be: failed at %i"

    /// Parse a word32 (big endian).
    let readUInt32be =
      parseMidi {
        let! a = readByte
        let! b = readByte
        let! c = readByte
        let! d = readByte
        return (word32be a b c d)
        }

    /// Parse a word24 (big endian).
    let readWord24be =
        parseMidi {
            let! a = readByte
            let! b = readByte
            let! c = readByte
            return (word24be a b c)
        }