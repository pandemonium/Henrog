namespace Henrog.Domain.Model

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Thoth.Json.Net

open NodaTime


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
  let asUuid (Uuid id) = id

  let asString =
    asUuid >> fun x -> x.ToString "N"

  let encode : UniqueIdentifier Encoder = fun (Uuid id) ->
    Encode.guid id

  let decode : UniqueIdentifier Decoder =
    Decode.guid |> Decode.map Uuid


type PersonalIdentity = 
  private Personnummer      of int
        | Samordningsnummer of int

module PersonalIdentity =
  let number = function Personnummer      pin -> pin
                      | Samordningsnummer sam -> sam

  let asString = 
    number >> string

  let encode : PersonalIdentity Encoder =
    function Personnummer      n -> [ "personnummer",      Encode.int n ]
           | Samordningsnummer n -> [ "samordningsnummer", Encode.int n ]
    >> Encode.object

  let decode : PersonalIdentity Decoder =
    let read name constructor =
      Decode.field name Decode.int |> Decode.map constructor
    in [ read "personnummer"      Personnummer
         read "samordningsnummer" Samordningsnummer ]
       |> Decode.oneOf


type Address =
  { Street     : string
    PostalCode : int
    City       : string }

module Address =
  let encode : Address Encoder = fun it ->
    [ "street",     Encode.string it.Street
      "postalCode", Encode.int it.PostalCode
      "city",       Encode.string it.City ]
    |> Encode.object

  let decode : Address Decoder =
    Decode.object <| fun get ->
      { Street     = get.Required.Field "street"     Decode.string
        PostalCode = get.Required.Field "postalCode" Decode.int
        City       = get.Required.Field "city"       Decode.string }


type EstablishmentId = EstablishmentId of UniqueIdentifier

type Establishment =
  { Name    : string
    Address : Address }

type EstablishmentInfo = EstablishmentId * Establishment

module Establishment =
  let id (id, _) = id

  let encodeId : EstablishmentId Encoder = fun (EstablishmentId uuid) ->
    UniqueIdentifier.encode uuid

  let encode : EstablishmentInfo Encoder = fun (id, it) ->
    [ "id",   encodeId          id
      "name", Encode.string     it.Name 
      "address", Address.encode it.Address  ]
    |> Encode.object

  let decodeId : EstablishmentId Decoder =
    UniqueIdentifier.decode |> Decode.map EstablishmentId

  let decode : EstablishmentInfo Decoder =
    let it =
      Decode.object <| fun get ->
        { Name    = get.Required.Field "name"    Decode.string
          Address = get.Required.Field "address" Address.decode }
      in Decode.tuple2 decodeId it


type ContactId = ContactId of UniqueIdentifier

type Contact =
  { Pin      : PersonalIdentity
    To       : EstablishmentId
    FullName : string 
    Address  : Address  }

type ContactInfo = ContactId * Contact

module Contact =
  let id (ContactId id, _) = id

  let establishment (_, { To = to' }) = to'

  let encodeId : ContactId Encoder = fun (ContactId uuid) ->
    UniqueIdentifier.encode uuid

  let encode : ContactInfo Encoder = fun (id, it) ->
    [ "id",       encodeId                id
      "pin",      PersonalIdentity.encode it.Pin
      "to",       Establishment.encodeId  it.To
      "fullName", Encode.string           it.FullName 
      "address",  Address.encode          it.Address ]
    |> Encode.object

  let decodeId : ContactId Decoder =
    UniqueIdentifier.decode |> Decode.map ContactId

  let decode : ContactInfo Decoder =
    let it =
      Decode.object <| fun get ->
        { Pin      = get.Required.Field "pin"      PersonalIdentity.decode
          To       = get.Required.Field "to"       Establishment.decodeId
          FullName = get.Required.Field "fullName" Decode.string 
          Address  = get.Required.Field "address"  Address.decode }
    in Decode.tuple2 decodeId it

type Contract = private Code of int

type CaregiverId = CaregiverId of UniqueIdentifier

type Caregiver =
  { Self     : ContactId
    With     : EstablishmentId
    Contract : Contract }

type CaregiverInfo = CaregiverId * Caregiver

module Caregiver =
  let establishment (_, info) = info.With

  let private encodeContract : Contract Encoder = fun (Code code) ->
    Encode.int code

  let private decodeContract : Contract Decoder =
    Decode.int |> Decode.map Code

  let encodeId : CaregiverId Encoder = fun (CaregiverId uuid) ->
    UniqueIdentifier.encode uuid

  let encode : CaregiverInfo Encoder = fun (id, it) ->
    [ "id",       encodeId               id
      "self",     Contact.encodeId       it.Self
      "with",     Establishment.encodeId it.With
      "contract", encodeContract         it.Contract ]
    |> Encode.object

  let decodeId : CaregiverId Decoder =
    UniqueIdentifier.decode |> Decode.map CaregiverId

  let decode : CaregiverInfo Decoder =
    let it =
      Decode.object <| fun get ->
        { Self     = get.Required.Field "self"     Contact.decodeId
          With     = get.Required.Field "with"     Establishment.decodeId
          Contract = get.Required.Field "contract" decodeContract }
    in Decode.tuple2 decodeId it


type JournalId = JournalId of UniqueIdentifier

type Journal =
  { Subject       : ContactId
    Caregiver     : CaregiverId
    Establishment : EstablishmentId }

type JournalInfo = JournalId * Journal

module Journal =
  let establishment (_, it) = it.Establishment

  let encodeId : JournalId Encoder = fun (JournalId uuid) ->
    UniqueIdentifier.encode uuid

  let encode : JournalInfo Encoder = fun (id, it) ->
    [ "id",            encodeId               id
      "subject",       Contact.encodeId       it.Subject 
      "caregiver",     Caregiver.encodeId     it.Caregiver
      "establishment", Establishment.encodeId it.Establishment ]
    |> Encode.object

  let decodeId : JournalId Decoder =
    UniqueIdentifier.decode |> Decode.map JournalId

  let decode : JournalInfo Decoder =
    let it =
      Decode.object <| fun get ->
        { Subject       = get.Required.Field "subject"       Contact.decodeId
          Caregiver     = get.Required.Field "caregiver"     Caregiver.decodeId
          Establishment = get.Required.Field "establishment" Establishment.decodeId }
    in Decode.tuple2 decodeId it


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

  let decode : NoteInfo Decoder =
    let it =
      Decode.object <| fun get ->
        { Journal = get.Required.Field "journal" Journal.decodeId
          Heading = get.Required.Field "heading" decodeHeading
          Body    = get.Required.Field "body"    decodeNoteBody }
    in Decode.tuple2 decodeId it

(* There are several kinds of visits, each type implies a number of Headings. *)
type VisitationType = New | Recurring

type VisitationId = VisitationId of UniqueIdentifier

type Visitation =
  { At        : EstablishmentId
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
      "at",        Establishment.encodeId it.At
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
        { At        = get.Required.Field "at"        Establishment.decodeId
          Caregiver = get.Required.Field "caregiver" Caregiver.decodeId
          Subject   = get.Required.Field "subject"   Contact.decodeId
          Type      = get.Required.Field "type"      decodeVisitationType }
    in Decode.tuple2 decodeId it


type Event = ContactAdded         of Timestamp * ContactInfo
           | EstablishmentCreated of Timestamp * EstablishmentInfo
           | CaregiverAdded       of Timestamp * CaregiverInfo
           | JournalCreated       of Timestamp * JournalInfo
           | NoteAdded            of Timestamp * NoteInfo
           | NoteSigned           of Timestamp * NoteSignature
           | CaregiverVisited     of Timestamp * VisitationInfo
           | VisitationNoted      of Timestamp * VisitationNote

module Event =
  type Kind = ContactAdded
            | EstablishmentCreated
            | CaregiverAdded
            | JournalCreated
            | NoteAdded
            | NoteSigned
            | CaregiverVisited
            | VisitationNoted

  module Kind =
    let ofEvent =
      function Event.ContactAdded         _ -> ContactAdded
             | Event.EstablishmentCreated _ -> EstablishmentCreated
             | Event.CaregiverAdded       _ -> CaregiverAdded
             | Event.JournalCreated       _ -> JournalCreated
             | Event.NoteAdded            _ -> NoteAdded
             | Event.NoteSigned           _ -> NoteSigned
             | Event.CaregiverVisited     _ -> CaregiverVisited
             | Event.VisitationNoted      _ -> VisitationNoted

    let name =
      function ContactAdded         -> "ContactAdded"
             | EstablishmentCreated -> "EstablishmentCreated"
             | CaregiverAdded       -> "CaregiverAdded"
             | JournalCreated       -> "JournalCreated"
             | NoteAdded            -> "NoteAdded"
             | NoteSigned           -> "NoteSigned"
             | CaregiverVisited     -> "CaregiverVisited"
             | VisitationNoted      -> "VisitationNoted"

  let private establishmentIdText (EstablishmentId uuid) =
    UniqueIdentifier.asString uuid

  let private journalIdText (JournalId uuid) =
    UniqueIdentifier.asString uuid

  let private caregiverIdText (CaregiverId uuid) =
    UniqueIdentifier.asString uuid

  let externalRepresentation = function
  | Event.ContactAdded (at, info) ->
    at, Contact.establishment info |> establishmentIdText, Contact.encode info
  | Event.EstablishmentCreated (at, info) ->
    at, Establishment.id info |> establishmentIdText, Establishment.encode info
  | Event.CaregiverAdded (at, info) ->
    at, Caregiver.establishment info |> establishmentIdText, Caregiver.encode info
  | Event.JournalCreated (at, info) ->
    at, Journal.establishment info |> establishmentIdText, Journal.encode info
  | Event.NoteAdded (at, info) ->
    at, Note.journal info |> journalIdText, Note.encode info
  | Event.NoteSigned (at, info) ->
    at, info.Journal |> journalIdText, Note.encodeNoteSignature info
  | Event.CaregiverVisited (at, info) ->
    at, Visitation.caregiver info |> caregiverIdText, Visitation.encode info
  | Event.VisitationNoted (at, info) ->
    at, info.Journal |> journalIdText, Visitation.encodeVisitationNote info
