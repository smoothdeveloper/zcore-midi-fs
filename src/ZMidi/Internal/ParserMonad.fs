
namespace ZMidi.Internal

module ParserMonad = 

    open System.IO
    open FSharpPlus
    open FSharpPlus.Data
    open FSharpPlus.Math.Generic

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
          Input: MidiData
          #if DEBUG_LASTPARSE
          LastParse : obj
          #endif
        }
        static member initial =
            { Position = 0
              RunningStatus = VoiceEvent.StatusOff
              Input = [||]
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
    with
      static member (+) (_: ParseError, y: ParseError) = y

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

    type ParserMonad<'a> = StateT<State, Result<'a * State, ParseError>>

    let nullOut = new StreamWriter(Stream.Null) :> TextWriter
    let mutable debug = false
    let logf format =
          if debug then
            printfn format
          else
            fprintfn nullOut format
            //Unchecked.defaultof<_>
        
    let inline private apply1 (parser : ParserMonad<'a>)
                       (state : State)  :  Result<'a * State, ParseError> = 
        let (StateT fn) = parser 
        try
            let result = fn state
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
            
    let inline mreturn (x:'a) : ParserMonad<'a> = result x

    let mzero () : ParserMonad<'a> = 
        StateT <| fun state -> Error (mkParseError state (EOF "mzero"))

    let inline mplus (parser1 : ParserMonad<'a>) (parser2 : ParserMonad<'a>) : ParserMonad<'a> = parser1 <|> parser2

    let inline mfor (items: #seq<'a>) (fn: 'a -> ParserMonad<'b>) : ParserMonad<seq<'b>> = failwithf ""
   
    let parseMidi = monad

    let runParser (ma:ParserMonad<'a>) input initialState =
      apply1 ma { initialState with Input = input}
      |> Result.map fst

    /// Run the parser on a file.
    let runParseMidi (ma : ParserMonad<'a>) (inputPath : string) : Result<'a, ParseError> = 
        runParser ma (File.ReadAllBytes inputPath) State.initial


    /// Throw a parse error
    let parseError (genMessage : Pos -> string) : ParserMonad<'a> = 
        StateT <| fun st -> Error (mkOtherParseError st genMessage)

    let fmapM (modify: 'a -> 'b) (parser : ParserMonad<'a>) : ParserMonad<'b> = 
        StateT <| fun state -> 
            match apply1 parser state with
            | Error err -> Error err
            | Ok (a, st2) -> Ok (modify a, st2)

    /// Operator for fmapM
    let ( <<| ) (modify: 'a -> 'b) (parser : ParserMonad<'a>) : ParserMonad<'b> = 
       fmapM modify parser

    /// Run the parser, if it fails swap the error message.
    let inline ( <??> ) (parser : ParserMonad<'a>) (genMessage : Pos -> string) : ParserMonad<'a> = 
        StateT <| fun st -> 
            match apply1 parser st with
            | Ok result -> Ok result
            | Error e ->
                logf "oops <??>: e:%A" e
                Error(mkOtherParseError st genMessage)

    ///
    let fmap (f: 'a -> 'b) (p: ParserMonad<'a>) : ParserMonad<'b> = map f p
    let inline ( <~> (* <$> *) ) (a) b = fmap a b

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
      StateT <| fun st -> Error (mkParseError st err)

    let getRunningEvent : ParserMonad<VoiceEvent> = 
        StateT <| fun st -> Ok (st.RunningStatus , st)

    let inline setRunningEvent (runningStatus : VoiceEvent) : ParserMonad<unit> = 
        StateT <| fun st -> Ok ((),  { st with RunningStatus = runningStatus })

    let getPos : ParserMonad<int> =
        StateT <| fun st -> Ok (st.Position, st)

    let inline private (|PositionValid|PositionInvalid|) (input: MidiData, state: State) =
      if state.Position >= 0 && state.Position < input.Length then
        PositionValid
      else
        PositionInvalid

    let inline private checkedParseM (name: string) (f: State -> Result<('a * State), ParseError>) =
        StateT 
            (fun state -> 
                try 
                  match state.Input, state with
                  | PositionValid -> f state
                  | PositionInvalid -> Error (mkParseError state (EOF name))
                with
                | e -> Error (mkParseError state (Other (sprintf "%s %A" name e)))
            )

    let peek : ParserMonad<byte> =
        checkedParseM "peek" <|
            fun st -> Ok (st.Input.[st.Position], st)

    /// Conditionally gets a byte (word8). Fails if input is finished.
    /// Consumes data on if predicate succeeds, does not consume if
    /// predicate fails.
    let cond (test : byte -> bool) : ParserMonad<byte option> = 
        checkedParseM "cond" <|
            fun st ->
                let a1 = st.Input.[st.Position]
                if test a1 then 
                  Ok (Some a1, st) 
                else Ok (None, st)

    /// Repeats a given <see paramref="parser"/> <see paramref="length"/> times.
    /// Fails with accumulated errors when any encountered.
    let inline count (length : ^T) (parser : ParserMonad<'a>) : ParserMonad<'a []> =
        StateT <| fun state ->
            let rec work (i : 'T) 
                         (st : State) 
                         (fk : ParseError -> Result<'a list * State, ParseError>) 
                         (sk : State -> 'a list  -> Result<'a list * State, ParseError>) = 
                if i <= 0G then 
                    sk st []
                else 
                    match apply1 parser st with
                    | Error msg -> fk msg
                    | Ok (a1, st1) -> 
                        work (i - 1G) st1 fk (fun st2 ac -> 
                        sk st2 (a1 :: ac))
            work length state (fun msg -> Error msg) (fun st ac -> Ok (ac, st)) 
                |> Result.map (fun (ans, st) -> (List.toArray ans, st))

    /// Run a parser within a bounded section of the input stream.
    let repeatTillPosition (maxPosition: Pos) (parser: ParserMonad<'a>) : ParserMonad<'a array> =
        StateT <| fun state -> 
            let limit = maxPosition
            let rec work (st : State) 
                        (fk : ParseError -> Result<'a list * State, ParseError>) 
                        (sk : State -> 'a list  -> Result<'a list * State, ParseError>) = 
                match apply1 parser st with
                | Error a -> fk a
                | Ok(a1, st1) -> 
                    match compare st1.Position limit with
                    | 0 -> sk st1 [a1]
                    | t when t > 0 -> fk (ParseError(st1.Position, Other "repeatTillPosition - too far"))
                    | _ -> 
                        work st1 fk (fun st2 ac ->
                            sk st2 (a1 :: ac))
            work state (fun msg -> Error msg) (fun st ac -> Ok (ac, st)) 
                |> Result.map (fun (ans, st) -> (List.toArray ans, st))

    /// Apply the parser for /count/ times, derive the final answer
    /// from the intermediate list with the supplied function.
    let inline gencount (plen: ParserMonad<'T>) (p: ParserMonad<'a>) (constr: ^T -> 'a array -> 'answer) : ParserMonad<'answer> =
        parseMidi {
          let! times = plen
          let! arr = count (int times) p
          return (constr times arr)
        }

    /// Drop a byte (word8).
    let dropByte : ParserMonad<unit> = 
        checkedParseM "dropByte" <| 
            fun st -> Ok ((), { st with Position = st.Position + 1 })

    /// Parse a byte (Word8).
    let readByte : ParserMonad<byte> = 
        checkedParseM "readByte" <| 
            fun st ->
                let a1 = st.Input.[st.Position]
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
    open ZMidi.Internal.DataTypes.FromBytes
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