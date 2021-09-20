namespace Henrog.Domain.Archive

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Thoth.Json.Net

open Henrog.Infrastructure
open Henrog.Domain.Model


type Memorandum =
  { Id          : UniqueIdentifier
    At          : Timestamp
    Kind        : Event.Kind
    AggregateId : string
    Data        : JsonValue }

module Memorandum =
  open Newtonsoft.Json
  open System.IO
  module Query = Persistence.Query

  let make id at aggregateId data kind =
    { Id          = id
      At          = at
      Kind        = kind
      AggregateId = aggregateId
      Data        = data }

  let ofEvent id event =
    let at, aggregateId, json = 
      Event.externalRepresentation event
    in make id at aggregateId json 
       <| Event.Kind.ofEvent event

  let parseJsonText text : JsonValue Out =
    try use reader = new JsonTextReader (new StringReader (text), DateParseHandling = DateParseHandling.None)
        Ok <| Newtonsoft.Json.Linq.JValue.ReadFrom reader
    with :? JsonReaderException as e ->
      throw <| Json $"Given an invalid JSON: {e.Message}."

  let decodeEvent at kind value : Event Out =
    let resolveDecoder =
      Event.fromExternalRepresentation at kind
    in Decode.fromValue "$" resolveDecoder value
       |> Result.mapError Json

  let read : Memorandum Out Persistence.Query = 
    monad { let! id          = UniqueIdentifier.make     <!> Query.getGuid
            let! at          = Timestamp.FromDateTimeUtc <!> Query.getDateTime
            let! kind        = Event.Kind.tryFromName    <!> Query.getString
            let! aggregateId = Query.getString
            let! data        = parseJsonText             <!> Query.getString

            return make id at aggregateId <!> data <*> kind }

  let write correlationId (memo : Memorandum) : unit Persistence.UnitOfWork =
    let insert : string = failwith "hi"
    let record =
      {| Id            = memo.Id       |> UniqueIdentifier.asUuid
         CorrelationId = correlationId |> UniqueIdentifier.asUuid
         At            = memo.At
         Kind          = memo.Kind     |> Event.Kind.name
         AggregateId   = memo.AggregateId
         Data          = memo.Data     |> Encode.toString 0 |}
      |> Persistence.Parameters.object
    in ignore <!> Persistence.UnitOfWork.write insert record


type Revision =
  { Id       : UniqueIdentifier
    Contents : Memorandum NonEmptyList }

module Revision =
  let summon id memorandum =
    { Id = id; Contents = NonEmptyList.singleton memorandum}

  let combine lhs rhs =
    { rhs with Contents = lhs.Contents ++ rhs.Contents }

type Revision with
  static member (+) (p, q) = Revision.combine p q


type StreamCommit = 
  private Commit of Revision * StreamCommit | Nil

module StreamCommit =
  let add correlationId memo =
    Commit (Revision.summon correlationId memo, Nil)

  (* Verify that these commits linearize correctly.. *)
  let rec join p q =
    match p, q with
    | Commit (lhs, p'), Commit (rhs, q') when lhs.Id = rhs.Id -> 
      Commit (lhs ++ rhs, join p' q')
    | Commit (lhs, p), Commit (rhs, q) ->
      Commit (rhs, Commit (lhs, join q p))
    | Nil, rhs -> rhs
    | lhs, Nil -> lhs

type StreamCommit with
  static member Zero       = Nil
  static member (+) (p, q) = StreamCommit.join p q


(* Archive too? Rename ofStreamCommit to add? *)
module PersistentRepresentation =
  let ofStreamCommit commit : unit Persistence.UnitOfWork =
    let memoranda { Id = id; Contents = cs } = 
      tuple2 id <!> cs |> NonEmptyList.toList

    let rec linearize =
      function Commit (revision, parent) -> memoranda revision ++ linearize parent
             | Nil                       -> []

    let xs : _ Persistence.UnitOfWork =
      traverse <| uncurry Memorandum.write
               <| linearize commit
    in ignore <!> xs


module Archive =
  type Term =
    { Id            : UniqueIdentifier Predicate
      At            : Timestamp        Predicate
      Kind          : Event.Kind       Predicate
      AggregateId   : string           Predicate 
      CorrelationId : UniqueIdentifier Predicate }

  and 'a Predicate = This   of 'a
                   | OneOf  of 'a (* How to solve this? *)
                   | After  of 'a
                   | Before of 'a
                   | Any

  and Expression = Literal of Term
                 | Or      of Expression * Expression
                 | And     of Expression * Expression

  module Predicate =
    let apply formal actual =
      function This   x -> $"{formal} = {actual}"
             | OneOf  x -> ""
             | After  x -> ""
             | Before x -> ""
             | Any      -> ""

  let (|Predicate|_|) =
    function This x | OneOf x | After x | Before x -> Some x | Any -> None

  module Term =
    let any =
      { Id = Any; At = Any; Kind = Any; AggregateId = Any; CorrelationId = Any }

    let select (summon : Term -> Term) : Term =
      summon any

    let inline put name adapt =
        function Predicate x ->
                  adapt x
                  |> Persistence.Parameter.put name
                  |> Some
               | otherwise -> None

    let inline parameters term =
      [ term.Id            |> put "id"           UniqueIdentifier.asUuid 
        term.At            |> put "timestamp"    id
        term.Kind          |> put "kind"         Event.Kind.name
        term.AggregateId   |> put "aggregateId"  id 
        term.CorrelationId |> put "correlatonId" UniqueIdentifier.asUuid ]
      |> choose id
      |> Persistence.Parameters.list

    let renderQuery =
      function x -> ""


  module Expression =
    let this = Literal
    let and' = curry And
    let or'  = curry Or

    let rec parameters =
      function Literal t  -> Term.parameters t
             | And (p, q) -> parameters p ++ parameters q
             | Or (p, q)  -> parameters p ++ parameters q

    let rec renderConditions =
      function Literal t  -> Term.renderQuery t
             | And (p, q) -> $"{renderConditions p} AND {renderConditions q}"
             | Or (p, q)  -> $"{renderConditions p} OR {renderConditions q}"

  let query (filter : Expression) : Event list Persistence.UnitOfWork = monad {
    let renderQuery : string =
      failwith "hi"

    let text       = renderQuery
    let parameters = Expression.parameters filter

    let decodeMemorandum (memo : Memorandum) =
      Memorandum.decodeEvent memo.At memo.Kind memo.Data

    let! xs =
      bind decodeMemorandum <!> Memorandum.read
      |> fun q -> sequence <!> Persistence.Query.list q
      |> Persistence.UnitOfWork.read text parameters

    return! lift xs
  }