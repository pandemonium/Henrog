namespace Henrog.Core.Model

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Thoth.Json.Net

open NodaTime


module String =
  let stripMargin (text : string) =
    let strip =
      String.trimStart " "
      >> String.trimStart "|"

    in strip <!> String.split [| "\n" |] text
       |> String.concat "\n"

(* Move this stuff to a more primordial layer. *)
type Error = Wtf               of string 
           | Json              of string
           | DatabaseException of exn

type 'a Out = Result<'a, Error>

module Out =
  let inline error (cons : ^a -> Error) (ps : ^a) =
    cons ps |> Error |> lift

type Timestamp = Instant


type Configuration =
  { Persistence : DatabaseConfiguration }

and DatabaseConfiguration =
  { ConnectionString : string }


type UniqueIdentifier = Uuid of System.Guid

module UniqueIdentifier =
  let make = Uuid

  let asUuid (Uuid id) = id

  let asString =
    asUuid >> fun x -> x.ToString "N"

  let encode : UniqueIdentifier Encoder = fun (Uuid id) ->
    Encode.guid id

  let decode : UniqueIdentifier Decoder =
    Decode.guid |> Decode.map Uuid


type PersonalIdentity = 
  private Personnummer      of int64
        | Samordningsnummer of int64

module PersonalIdentity =
  let unsafePersonnummer = Personnummer

  let number = function Personnummer      pin -> pin
                      | Samordningsnummer sam -> sam

  let asString = 
    number >> string

  let encode : PersonalIdentity Encoder =
    function Personnummer      n -> [ "personnummer",      Encode.int64 n ]
           | Samordningsnummer n -> [ "samordningsnummer", Encode.int64 n ]
    >> Encode.object

  let decode : PersonalIdentity Decoder =
    let read name constructor =
      Decode.field name Decode.int64 |> Decode.map constructor
    in [ read "personnummer"      Personnummer
         read "samordningsnummer" Samordningsnummer ]
       |> Decode.oneOf


type Municipality =
  { Code : int
    Seat : string }

module Municipality =
  let encode : Municipality Encoder = fun it ->
    [ "code", Encode.int    it.Code
      "seat", Encode.string it.Seat ]
    |> Encode.object

  let decode : Municipality Decoder =
    Decode.object <| fun get ->
      { Code = get.Required.Field "code" Decode.int
        Seat = get.Required.Field "seat" Decode.string }


type Address =
  { Street        : string
    PostalCode    : int
    City          : string 
    Municipality  : Municipality }

module Address =
  let encode : Address Encoder = fun it ->
    [ "street",        Encode.string       it.Street
      "postalCode",    Encode.int          it.PostalCode
      "city",          Encode.string       it.City
      "municipality",  Municipality.encode it.Municipality ]
    |> Encode.object

  let decode : Address Decoder =
    Decode.object <| fun get ->
      { Street        = get.Required.Field "street"       Decode.string
        PostalCode    = get.Required.Field "postalCode"   Decode.int
        City          = get.Required.Field "city"         Decode.string 
        Municipality  = get.Required.Field "municipality" Municipality.decode }


type LocationId = LocationId of UniqueIdentifier

type Contract = Code of int

type Location =
  { Name    : string
    Address : Address }

type LocationInfo = LocationId * Location

module Location =
  let id (id, _) = id

  let encodeId : LocationId Encoder = fun (LocationId uuid) ->
    UniqueIdentifier.encode uuid

  let encodeContract : Contract Encoder = fun (Code it) ->
    Encode.int it

  let encode : LocationInfo Encoder = fun (id, it) ->
    [ "id",       encodeId       id
      "name",     Encode.string  it.Name
      "address",  Address.encode it.Address  ]
    |> Encode.object

  let decodeId : LocationId Decoder =
    UniqueIdentifier.decode |> Decode.map LocationId

  let decodeContract : Contract Decoder =
    Decode.int |> Decode.map Code

  let decode : LocationInfo Decoder =
    let it =
      Decode.object <| fun get ->
        { Name     = get.Required.Field "name"     Decode.string
          Address  = get.Required.Field "address"  Address.decode }
    in Decode.map2 tuple2 (Decode.field "id" decodeId) it


type ContactId = ContactId of UniqueIdentifier

type BinaryGender = Male | Female

module BinaryGender =
  let fromPin =
    PersonalIdentity.number 
    >> function n when n / 10L % 2L = 0L -> Female 
              | otherwise                -> Male

  let encode =
    function Male   -> Encode.string "M"
           | Female -> Encode.string "F"

  let decode : BinaryGender Decoder =
    let it =
      function "M"       -> Decode.succeed Male
             | "F"       -> Decode.succeed Female
             | otherwise -> Decode.fail $"No such gender `{otherwise}`."
    in Decode.string
       |> Decode.andThen it

type Contact =
  { Pin      : PersonalIdentity
    To       : LocationId
    Gender   : BinaryGender
    FullName : string 
    Address  : Address  }

type ContactInfo = ContactId * Contact

module Contact =
  let id (ContactId id, _) = id

  let location (_, { To = to' }) = to'

  let encodeId : ContactId Encoder = fun (ContactId uuid) ->
    UniqueIdentifier.encode uuid

  let encode : ContactInfo Encoder = fun (id, it) ->
    [ "id",       encodeId                id
      "pin",      PersonalIdentity.encode it.Pin
      "to",       Location.encodeId       it.To
      "gender",   BinaryGender.encode     it.Gender
      "fullName", Encode.string           it.FullName 
      "address",  Address.encode          it.Address ]
    |> Encode.object

  let decodeId : ContactId Decoder =
    UniqueIdentifier.decode |> Decode.map ContactId

  let decode : ContactInfo Decoder =
    let it =
      Decode.object <| fun get ->
        { Pin      = get.Required.Field "pin"      PersonalIdentity.decode
          To       = get.Required.Field "to"       Location.decodeId
          Gender   = get.Required.Field "gender"   BinaryGender.decode
          FullName = get.Required.Field "fullName" Decode.string 
          Address  = get.Required.Field "address"  Address.decode }
    in Decode.map2 tuple2 (Decode.field "id" decodeId) it


type CaregiverId = CaregiverId of UniqueIdentifier

(* Combine Caregiver and Establiment. *)
type Caregiver =
  { Self     : ContactId
    With     : LocationId 
    Contract : Contract }

type CaregiverInfo = CaregiverId * Caregiver

module Caregiver =
  let location (_, info) = info.With

  let private encodeContract : Contract Encoder = fun (Code code) ->
    Encode.int code

  let private decodeContract : Contract Decoder =
    Decode.int |> Decode.map Code

  let encodeId : CaregiverId Encoder = fun (CaregiverId uuid) ->
    UniqueIdentifier.encode uuid

  let encode : CaregiverInfo Encoder = fun (id, it) ->
    [ "id",       encodeId          id
      "self",     Contact.encodeId  it.Self
      "with",     Location.encodeId it.With
      "contract", encodeContract    it.Contract ]
    |> Encode.object

  let decodeId : CaregiverId Decoder =
    UniqueIdentifier.decode |> Decode.map CaregiverId

  let decode : CaregiverInfo Decoder =
    let it =
      Decode.object <| fun get ->
        { Self     = get.Required.Field "self"     Contact.decodeId
          With     = get.Required.Field "with"     Location.decodeId
          Contract = get.Required.Field "contract" decodeContract }
    in Decode.map2 tuple2 (Decode.field "id" decodeId) it


type JournalId = JournalId of UniqueIdentifier

type Journal =
  { Subject   : ContactId
    Caregiver : CaregiverId 
    Location  : LocationId }

type JournalInfo = JournalId * Journal

module Journal =
  let location (_, { Location = l }) = l

  let encodeId : JournalId Encoder = fun (JournalId uuid) ->
    UniqueIdentifier.encode uuid

  let encode : JournalInfo Encoder = fun (id, it) ->
    [ "id",        encodeId           id
      "subject",   Contact.encodeId   it.Subject 
      "caregiver", Caregiver.encodeId it.Caregiver 
      "location",  Location.encodeId  it.Location ]
    |> Encode.object

  let decodeId : JournalId Decoder =
    UniqueIdentifier.decode |> Decode.map JournalId

  let decode : JournalInfo Decoder =
    let it =
      Decode.object <| fun get ->
        { Subject   = get.Required.Field "subject"   Contact.decodeId
          Caregiver = get.Required.Field "caregiver" Caregiver.decodeId 
          Location  = get.Required.Field "location"  Location.decodeId }
    in Decode.map2 tuple2 (Decode.field "id" decodeId) it


type Heading = HeadingKey of string

type NoteBody = Text     of string
              | Document of string

type NoteId = NoteId of UniqueIdentifier

type Note =
  { Journal : JournalId
    Heading : Heading
    Body    : NoteBody }

type NoteSignature =
  { Journal  : JournalId
    Note     : NoteId
    SignedBy : CaregiverId }

type NoteInfo = NoteId * Note

module Note =
  let journal (_, it : Note) = it.Journal

  let encodeId : NoteId Encoder = fun (NoteId uuid) ->
    UniqueIdentifier.encode uuid

  let encodeHeading : Heading Encoder = fun (HeadingKey key) ->
    Encode.string key

  let encodeNoteBody : NoteBody Encoder =
    function Text     t -> [ "text",     Encode.string t ]
           | Document d -> [ "document", Encode.string d ]
    >> Encode.object

  let encodeNoteSignature : NoteSignature Encoder = fun it ->
    [ "journal",  Journal.encodeId   it.Journal
      "note",     encodeId           it.Note
      "signedBy", Caregiver.encodeId it.SignedBy ]
    |> Encode.object

  let encode : NoteInfo Encoder = fun (id, it) ->
    [ "id",      encodeId         id
      "journal", Journal.encodeId it.Journal
      "heading", encodeHeading    it.Heading
      "body",    encodeNoteBody   it.Body ]
    |> Encode.object

  let decodeId : NoteId Decoder =
    UniqueIdentifier.decode |> Decode.map NoteId
  
  let decodeHeading : Heading Decoder =
    Decode.string |> Decode.map HeadingKey

  let decodeNoteBody : NoteBody Decoder =
    let read name constructor =
      Decode.field name Decode.string |> Decode.map constructor
    in [ read "text"     Text
         read "document" Document ]
       |> Decode.oneOf

  let decodeNoteSignature : NoteSignature Decoder =
    Decode.object <| fun get ->
      { Journal  = get.Required.Field "journal"  Journal.decodeId
        Note     = get.Required.Field "note"     decodeId
        SignedBy = get.Required.Field "signedBy" Caregiver.decodeId }

  let decode : NoteInfo Decoder =
    let it =
      Decode.object <| fun get ->
        { Journal = get.Required.Field "journal" Journal.decodeId
          Heading = get.Required.Field "heading" decodeHeading
          Body    = get.Required.Field "body"    decodeNoteBody }
    in Decode.map2 tuple2 (Decode.field "id" decodeId) it

(* There are several kinds of visits, each type implies a number of Headings. *)
type VisitationType = New | Recurring

type VisitationId = VisitationId of UniqueIdentifier

type Visitation =
  { At        : LocationId
    Caregiver : CaregiverId
    Subject   : ContactId
    Type      : VisitationType }

type VisitationNote =
  { Visitation : VisitationId
    Journal    : JournalId }

type VisitationInfo = VisitationId * Visitation

module Visitation =
  let caregiver (_, it : Visitation) = it.Caregiver

  let encodeId : VisitationId Encoder = fun (VisitationId uuid) ->
    UniqueIdentifier.encode uuid

  let encodeVisitationType : VisitationType Encoder =
    function New       -> "new"
           | Recurring -> "recurring"
    >> Encode.string

  let encodeVisitationNote : VisitationNote Encoder = fun it ->
    [ "visitation", encodeId         it.Visitation
      "journal",    Journal.encodeId it.Journal ]
    |> Encode.object

  let encode : VisitationInfo Encoder = fun (id, it) ->
    [ "id",        encodeId               id
      "at",        Location.encodeId it.At
      "caregiver", Caregiver.encodeId     it.Caregiver
      "subject",   Contact.encodeId       it.Subject
      "type",      encodeVisitationType   it.Type ]
    |> Encode.object

  let decodeId : VisitationId Decoder =
    UniqueIdentifier.decode |> Decode.map VisitationId

  let decodeVisitationType : VisitationType Decoder =
    function "new"       -> Decode.succeed New
           | "recurring" -> Decode.succeed Recurring
           | otherwise   -> Decode.fail $"No such VisitationType `{otherwise}`."
    |> flip Decode.andThen Decode.string

  let decodeVisitationNote : VisitationNote Decoder =
    Decode.object <| fun get ->
      { Visitation = get.Required.Field "visitation" decodeId
        Journal    = get.Required.Field "journal"    Journal.decodeId }

  let decode : VisitationInfo Decoder =
    let it =
      Decode.object <| fun get ->
        { At        = get.Required.Field "at"        Location.decodeId
          Caregiver = get.Required.Field "caregiver" Caregiver.decodeId
          Subject   = get.Required.Field "subject"   Contact.decodeId
          Type      = get.Required.Field "type"      decodeVisitationType }
    in Decode.map2 tuple2 (Decode.field "id" decodeId) it


type Event = ContactAdded       of Timestamp * ContactInfo
           | LocationRegistered of Timestamp * LocationInfo
           | CaregiverAdded     of Timestamp * CaregiverInfo
           | JournalCreated     of Timestamp * JournalInfo
           | NoteAdded          of Timestamp * NoteInfo
           | NoteSigned         of Timestamp * NoteSignature
           | CaregiverVisited   of Timestamp * VisitationInfo
           | VisitationNoted    of Timestamp * VisitationNote

module Event =
  type Kind = ContactAdded
            | LocationRegistered
            | CaregiverAdded
            | JournalCreated
            | NoteAdded
            | NoteSigned
            | CaregiverVisited
            | VisitationNoted

  module Kind =
    let ofEvent =
      function Event.ContactAdded       _ -> ContactAdded
             | Event.LocationRegistered _ -> LocationRegistered
             | Event.CaregiverAdded     _ -> CaregiverAdded
             | Event.JournalCreated     _ -> JournalCreated
             | Event.NoteAdded          _ -> NoteAdded
             | Event.NoteSigned         _ -> NoteSigned
             | Event.CaregiverVisited   _ -> CaregiverVisited
             | Event.VisitationNoted    _ -> VisitationNoted

    let name =
      function ContactAdded       -> "ContactAdded"
             | LocationRegistered -> "LocationRegistered"
             | CaregiverAdded     -> "CaregiverAdded"
             | JournalCreated     -> "JournalCreated"
             | NoteAdded          -> "NoteAdded"
             | NoteSigned         -> "NoteSigned"
             | CaregiverVisited   -> "CaregiverVisited"
             | VisitationNoted    -> "VisitationNoted"

    let tryFromName =
      function "ContactAdded"       -> Ok ContactAdded
             | "LocationRegistered" -> Ok LocationRegistered
             | "CaregiverAdded"     -> Ok CaregiverAdded
             | "JournalCreated"     -> Ok JournalCreated
             | "NoteAdded"          -> Ok NoteAdded
             | "NoteSigned"         -> Ok NoteSigned
             | "CaregiverVisited"   -> Ok CaregiverVisited
             | "VisitationNoted"    -> Ok VisitationNoted
             | otherwise            -> throw <| Wtf $"Unknown EventKind `{otherwise}`." 

  let private locationIdText (LocationId uuid) =
    UniqueIdentifier.asString uuid

  let private journalIdText (JournalId uuid) =
    UniqueIdentifier.asString uuid

  let private caregiverIdText (CaregiverId uuid) =
    UniqueIdentifier.asString uuid

  (* This function is not nice. *)
  let externalRepresentation = function
    | Event.ContactAdded (at, info) ->
      at, Contact.location info |> locationIdText, Contact.encode info
    | Event.LocationRegistered (at, info) ->
      at, Location.id info |> locationIdText, Location.encode info
    | Event.CaregiverAdded (at, info) ->
      at, Caregiver.location info |> locationIdText, Caregiver.encode info
    | Event.JournalCreated (at, info) ->
      (* This is why a journal has a location (formerly known as establishment.) *)
      at, Journal.location info |> locationIdText, Journal.encode info
    | Event.NoteAdded (at, info) ->
      at, Note.journal info |> journalIdText, Note.encode info
    | Event.NoteSigned (at, info) ->
      at, info.Journal |> journalIdText, Note.encodeNoteSignature info
    | Event.CaregiverVisited (at, info) ->
      at, Visitation.caregiver info |> caregiverIdText, Visitation.encode info
    | Event.VisitationNoted (at, info) ->
      at, info.Journal |> journalIdText, Visitation.encodeVisitationNote info

  let fromExternalRepresentation at : Kind -> Event Decoder =
    let produce constructor decoder =
      Decode.map (curry constructor at) decoder
    in function ContactAdded       -> produce Event.ContactAdded       Contact.decode
              | LocationRegistered -> produce Event.LocationRegistered Location.decode
              | CaregiverAdded     -> produce Event.CaregiverAdded     Caregiver.decode
              | JournalCreated     -> produce Event.JournalCreated     Journal.decode
              | NoteAdded          -> produce Event.NoteAdded          Note.decode
              | NoteSigned         -> produce Event.NoteSigned         Note.decodeNoteSignature
              | CaregiverVisited   -> produce Event.CaregiverVisited   Visitation.decode
              | VisitationNoted    -> produce Event.VisitationNoted    Visitation.decodeVisitationNote