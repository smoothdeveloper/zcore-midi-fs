namespace ZMidi.DataTypes

type midiportnumber = byte
type word8 = byte
type word16 = uint16
type word32 = uint32
type bits7 = byte
type midichannel = byte
type [<Struct>] DeltaTime(value: int32) =
    member x.Value = value

type [<Struct>] word14(value: word16) =
    member x.Value = value


/// The file format - in a MIDI file this is a big-endian 
/// word16 with 0,1 or 2 being the only valid values. 
type MidiFormat =
/// Format 0 file - single multi-channel track.
| MidiFormat0
/// Format 1 file - 1 or more tracks, played simultaneously.
| MidiFormat1
/// Format 2 file - 1 or more independent tracks.
| MidiFormat2
type MidiTimeDivision =
    | FramePerSecond of frame: word16
    | TicksPerBeat of ticks: word16

type MidiHeader = {
    format: MidiFormat
    trackCount: word16
    timeDivision: MidiTimeDivision
}

/// Running Status.
///
/// MIDI allows a compact representation of voice events where
/// consecutive events (same event, same channel) only need to
/// include the first event-channel byte - subsequent events 
/// only send payload until the next event or channel change.
///
/// Including @MidiRunningStatus@ in the data representation is 
/// important for ZMidi as an aim is to allow round-tripping
/// of exisiting MIDI files. However it makes MIDI generation
/// more complicated (there is more scope to generate bad 
/// output) - if you are only generating MIDI it is wise to always 
/// set @MidiRunningStatus@ to @OFF@.
[<RequireQualifiedAccess>]
type MidiRunningStatus = ON | OFF
type TagByte = word8
type MidiDataOther = TagByte
type MidiEvent =
    | MidiEventOther of MidiDataOther
    | VoiceEvent of MidiRunningStatus * MidiVoiceEvent
    | SysExEvent of MidiSysExEvent
    | SysCommonEvent of MidiSysCommonEvent
    | SysRealtimeEvent of MidiSysRealtimeEvent
    | MetaEvent of MidiMetaEvent

and MidiVoiceEvent =
    /// Note off.
    /// 
    /// > 80 to 8F (0 to F is channel number) * note * velocity
    /// 
    ///  Turn off a sounding note.
    | NoteOff of status: bits7 * note: bits7 * velocity: bits7

    /// Note on.
    /// 
    /// > 90 to 9F (0 to F is channel number) * note * velocity
    /// 
    /// Start playing a note.
    | NoteOn of status: bits7 * note: bits7 * velocity: bits7

    /// Polyphonic key pressure.
    ///
    /// > A0 to AF (0 to F is channel number) * note * pressure_value
    ///
    /// Change of pressure applied to the synthesizer key.
    | NoteAfterTouch of status: bits7 * note: bits7 * pressure: bits7

    /// Set a controller.
    ///
    /// > B0 to BF (0 to F is channel number) * controller_number * value 
    ///
    /// Controller change, e.g. by a footswitch.
    | Controller of status: bits7 * controller: bits7 * value: bits7

    ///  Set the program.
    /// 
    /// > C0 to CF (0 to F is channel number) * program_number 
    /// 
    /// Change the instrument 
    /// playing on the specified channel. For playback on 
    /// computers (rather than synthesizers) the program numbers
    /// will correspond to the /General MIDI/ instrument numbers.
    | ProgramChange of status: bits7 * programNumber: bits7

    /// Channel pressure.
    ///
    /// > D0 to DF (0 to F is channel number) * pressure_value
    ///    
    | ChannelAfterTouch of status: bits7 * pressure: bits7

    /// Pitch bend 
    /// 
    /// > E0 to EF (0 to F is channel number) * value
    /// 
    /// Change the pitch of a sounding note. Often used to 
    /// approximate microtonal tunings.
    /// 
    /// NOTE - as of v0.9.0 the value is interpreted.
    /// This is a Word14 value, the range is (0..16383).
    | PitchBend of status: bits7 * bend: word14


and MidiTextType =
    | GenericText
    | CopyrightNotice
    | SequenceName
    | InstrumentName
    | Lyrics
    | Marker
    | CuePoint


/// Sequential messages with delta time 0 are played simultaneously.  
and MidiMessage = {
    timestamp: DeltaTime
    event: MidiEvent
}

and MidiTrack = MidiMessage array

and MidiFile = {
     header : MidiHeader
     tracks : MidiTrack array
 }

and MidiSysExEvent =
  /// Single SysEx event.
  ///
  /// > F0 * length * data
  /// 
  /// An uninterpreted sys-ex event.
  | SysExSingle of byte array

  /// SysEx sent as (non-standard) multiple continuation 
  /// packets.
  ///
  /// > F0 * length * packet1 ... [SysExContPacket]
  ///
  | SysExCont of byte array * MidiSysExContPacket array

  /// Escape sequence of one-or-more SysEx events.
  ///
  /// > F7 * length * data
  ///
  | SysExEscape of byte array

/// Continuation packet for a (non-standard) multi-part SysEx 
/// event.
/// 
/// Apprently this format is use by Casio.
and MidiSysExContPacket = MidiSysExContPacket of DeltaTime * byte array



and MidiMetaEvent =

    ///  Text / copywright etc.
    /// 
    /// > FF * text_type * contents
    /// 
    /// Free text field (e.g. copyright statement). The contents 
    /// can notionally be any length.
    ///
    | TextEvent of MidiTextType * string

    ///  Sequence Number 
    /// 
    /// > FF 00 02 * value
    /// 
    /// Format 1 files - only track 1 should have a sequence 
    /// number. 
    ///
    /// Format 2 files - a sequence number should identify each 
    /// track.
    ///  
    /// The sequence number event should occur at the start of a 
    /// track, before any non-zero time events.
    ///
    | SequenceNumber of word16

    ///  Channel prefix 
    /// 
    /// > FF 20 01 * channel
    /// 
    /// Relay all meta and sys-ex events to the given channel.
    ///
    | ChannelPrefix of midichannel

    ///  Midi port
    /// 
    /// > FF 21 01 * port
    /// 
    /// Midi port number - used to workaround 16 channel limit...
    /// 
    | MidiPort of midiportnumber

    ///  End-of-track event. 
    ///
    /// > FF 2F 00
    ///
    | EndOfTrack

    ///  Set tempo
    ///
    /// > FF 51 03 * microseconds_per_quarter_note
    ///
    | SetTempo of word32

    ///  SMPTE offest
    /// 
    /// > FF 54 05 * hour * minute * second * frac * subfrac
    /// 
    /// The SMPTE time when a track should start. This event 
    /// should occur at the start of a track, before any non-zero 
    /// time events.
    ///
    | SMPTEOffset of word8 * word8 * word8 * word8 * word8
    
    ///  Time signature 
    /// 
    /// > FF 58 04 * numerator * denominator * metro * num_32nd_notes
    ///
    | TimeSignature of word8 * word8 * word8 * word8
    
    ///  Key signature 
    ///
    /// > FF 59 02 * key_type * scale_type
    ///
    /// @key_type@ is the number of sharps (postive numbers) or 
    /// flats (negative numbers), e.g. (-1) is 1 flat.
    ///
    /// @scale_type@ indicates major or minor.  
    ///
    | KeySignature of int8 * MidiScaleType
    
    ///  SSME 
    /// 
    /// > FF 7F * length * data
    /// 
    /// Sequencer specific meta-event - uninterpreted.
    ///
    | SSME of byte array 
    ///  Unrecognized Meta Event
    ///
    /// > FF * type * length * data 
    ///
    | MetaOther of otherType: word8 * byte array


/// System common event.
///
/// Common information for all channels in a system. 
///
/// These events may not be pertinent to MIDI files generated on a 
/// computer (as opposed to MIDI generated by a synthesizer or 
/// sequencer).
///
and MidiSysCommonEvent =
    /// Time code quarter frame.
    /// 
    /// > F1 * payload
    /// 
    /// Note the payload is really a byte split into two 4-bit 
    /// values, however here it is uninterpreted.
    ///
    | QuarterFrame of word8
    
    /// Song position pointer.
    ///
    /// > F2 * lsb * msb
    ///
    | SongPosPointer of word8 * word8

    /// Song number.
    /// 
    /// > F3 * song_number
    ///
    /// Song number should be in the range 0..127.
    ///
    | SongSelect of word8

    /// Undefined system common event.
    /// 
    /// > F4
    ///
    | UndefinedF4

    /// Undefined system common event.
    /// 
    /// > F5
    ///
    | UndefinedF5

    /// Tune request.
    /// 
    /// > F6
    /// 
    /// Tune request message for analogue synthesizers.
    ///
    | TuneRequest

    /// End-of-system-exclusive message.
    /// 
    /// > F7
    /// 
    | EOX

/// System real-time event.
///
/// These events may not be pertinent to MIDI files generated on a 
/// computer (as opposed to MIDI generated by a synthesizer or 
/// sequencer).
and MidiSysRealtimeEvent =
    /// Timing signal.
    /// 
    /// > F8 
    ///      
    | TimingClock
    
    /// Undefined real time event.
    /// 
    /// > F9
    ///
    ///
    | UndefinedF9

    
    /// Start playing a sequence.
    /// 
    /// > FA
    /// 
    | StartSequence

    
    /// Continue playing a stopped sequence.
    /// 
    /// > FB
    ///
    | ContinueSequence


    /// Stop playing a sequence.
    /// 
    /// > FC
    ///
    | StopSequence


    /// Undefined real time event.
    /// 
    /// > FD
    ///
    ///
    | UndefinedFD

    /// Active sensing
    /// 
    /// > FE
    /// 
    /// Synchronization pulse...
    /// 
    | ActiveSensing


    /// Reset to power-up status.
    /// 
    /// > FF
    ///
    | SystemReset



and [<RequireQualifiedAccess>] MidiScaleType = Major | Minor | OtherScale of word8
