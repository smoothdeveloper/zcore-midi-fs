module Isomorphisms

open ZMidi
open ZMidi.Internal
open ZMidi.Internal.DataTypes
open ZMidi.Internal.ParserMonad

type Iso<'a,'b> = ('a -> 'b) * ('b -> 'a)
module Iso =
    let reverse iso = snd iso, fst iso
    
let word32be : Iso<_,_> = (ToBytes.word32be), (Evil.uncurry4 FromBytes.word32be)
let word24be : Iso<_,_> = (ToBytes.word24be), (Evil.uncurry3 FromBytes.word24be)

let word16be : Iso<_,_> = ToBytes.word16be, (Evil.uncurry2 FromBytes.word16be)
let word14be : Iso<_,_> = ToBytes.word14be, (Evil.uncurry2 FromBytes.word14be)

module Result =
    let internal get = function | Ok r -> r | Error e -> failwith $"{e}"
let midiVoiceEvent : Iso<_,_> = ToBytes.midiVoiceEvent, (fun (meb: _ array) -> runParser (ReadFile.voiceEvent meb.[0]) meb.[1..] State.initial |> Result.get |> fst)
let metaEvent : Iso<_,_> = ToBytes.midiMetaEvent, (fun (meb: _ array) -> runParser (ReadFile.metaEvent meb.[1]) meb.[2..] State.initial |> Result.get |> fst)
let deltaTime : Iso<_,_> = ToBytes.deltaTime, (fun (meb: _ array) -> runParser (ReadFile.deltaTime) meb State.initial |> Result.get |> fst)