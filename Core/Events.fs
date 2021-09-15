namespace Henrog.Domain.Events

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
    AggregateId : string  (* Should this be a UUID? *)
    Data        : JsonValue }

module Memorandum =
  let ofEvent id event =
    let at, aggregateId, json = 
      Event.externalRepresentation event
    in { Id          = id
         At          = at
         Kind        = Event.Kind.ofEvent event
         AggregateId = aggregateId
         Data        = json }

  let write correlationId (memo : Memorandum) : unit Persistence.UnitOfWork =
    let insert : string = failwith "hi"
    let record =
      {| Id            = memo.Id       |> UniqueIdentifier.asUuid
         CorrelationId = correlationId |> UniqueIdentifier.asUuid
         At            = memo.At
         Kind          = memo.Kind     |> Event.Kind.name
         AggregateId   = memo.AggregateId
         Data          = memo.Data     |> Encode.toString 0 |}
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
