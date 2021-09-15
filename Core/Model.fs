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


type PersonalIdentification = 
  private Personnummer      of int
        | Samordningsnummer of int

module PersonalIdentification =
  let number = function Personnummer      pin -> pin
                      | Samordningsnummer sam -> sam

  let asString = 
    number >> string

  let encode : PersonalIdentification Encoder =
    function Personnummer      n -> [ "personnummer",      Encode.int n ]
           | Samordningsnummer n -> [ "samordningsnummer", Encode.int n ]
    >> Encode.object

  let decode : PersonalIdentification Decoder =
    let read name constructor =
      Decode.field name Decode.int |> Decode.map constructor
    in [ read "personnummer"      Personnummer
         read "samordningsnummer" Samordningsnummer ]
       |> Decode.oneOf

type ContactId = ContactId of UniqueIdentifier

type Contact =
  { Pin      : PersonalIdentification
    FullName : string }

type ContactInfo = ContactId * Contact

module Contact =
  let id (ContactId id, _) = id

  let encodeId : ContactId Encoder = fun (ContactId uuid) ->
    UniqueIdentifier.encode uuid

  let encode : ContactInfo Encoder = fun (id, it) ->
    [ "id",       encodeId                      id
      "pin",      PersonalIdentification.encode it.Pin
      "fullName", Encode.string                 it.FullName ]
    |> Encode.object

  let decodeId : ContactId Decoder =
    UniqueIdentifier.decode |> Decode.map ContactId

  let decode : ContactInfo Decoder =
    let data =
      Decode.object <| fun get ->
        { Pin      = get.Required.Field "pin"      PersonalIdentification.decode
          FullName = get.Required.Field "fullName" Decode.string }
    in Decode.tuple2 decodeId data


type Event = ContactAdded of Timestamp * ContactInfo

module Event =
  type Kind = ContactAdded

  module Kind =
    let ofEvent =
      function Event.ContactAdded _ -> ContactAdded

    let name = 
      function ContactAdded -> "ContactAdded"

  let externalRepresentation =
      function Event.ContactAdded (at, data) as event ->
                at, Kind.ofEvent event, Contact.id data |> UniqueIdentifier.asString, Contact.encode data
