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
    let insert = 
      """INSERT
           INTO journal (event_id, correlation_id, at, kind, aggregate_id, data)
         VALUES (@id, @correlationId, @at, @kind, @aggregateId, @data)"""
    let mkRecord =
      {| Id            = memo.Id       |> UniqueIdentifier.asUuid
         CorrelationId = correlationId |> UniqueIdentifier.asUuid
         At            = memo.At
         Kind          = memo.Kind     |> Event.Kind.name
         AggregateId   = memo.AggregateId
         Data          = memo.Data     |> Encode.toString 0 |}
      |> Persistence.Parameters.object
    in ignore <!> Persistence.UnitOfWork.write insert mkRecord


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


module Query =
  type Expression = Literal of Term
                  | Or     of Expression * Expression
                  | And    of Expression * Expression
  and Term =
    { Id            : UniqueIdentifier Predicate
      At            : Timestamp        Predicate
      Kind          : Event.Kind       Predicate
      AggregateId   : string           Predicate 
      CorrelationId : UniqueIdentifier Predicate }

  and 'a Predicate = This   of 'a
                   | OneOf  of 'a list (* How to solve this? *)
                   | After  of 'a
                   | Before of 'a
                   | Any

  module Predicate =
    let apply formal actual =
      let applyOp op = $"{formal} {op} @{actual}"
      let parameters =
        mapi (fun i _ -> sprintf "@%s%d" actual i)
        >> String.concat ", "
      in function This   x -> applyOp "="
                | OneOf xs -> $"{formal} IN ({parameters xs})"
                | After  x -> applyOp ">"
                | Before x -> applyOp "<"
                | Any      -> ""

  let (|One|_|) =
    function This x | After x | Before x -> Some x | OneOf _ | Any -> None

  let (|Many|_|) =
    function OneOf x -> Some x | otherwise -> None

  module Term =
    let any =
      { Id = Any; At = Any; Kind = Any; AggregateId = Any; CorrelationId = Any }

    let select (summon : Term -> Term) parent : Term =
      summon parent

//    let id 

    let inline put name convert =
      function One x ->
                convert x
                |> Persistence.Parameter.put name
                |> Some
             | otherwise -> None

    let inline putMany name convert =
      (* This name will collide with other branches using a OneOf with the same
         base parameter name. *)
      let freshName i =
        Persistence.Parameter.put $"{name}{i}"
      in function Many xs ->
                   convert <!> xs
                   |> mapi freshName 
                   |> Persistence.Parameters.list
                   |> Some
                | otherwise -> None

    (* Term -> Persistence.Parameters State. *)
    (* The State can contain a base for fresh identifiers for array parameters. *)
    let inline parameters term : Persistence.Parameters =
      let ones = 
        [ term.Id            |> put "id"           UniqueIdentifier.asUuid 
          term.At            |> put "at"           id
          term.Kind          |> put "kind"         Event.Kind.name
          term.AggregateId   |> put "aggregateId"  id 
          term.CorrelationId |> put "correlatonId" UniqueIdentifier.asUuid ]
        |> choose id
        |> Persistence.Parameters.list

      let manies =
        [ term.Id            |> putMany "id"           UniqueIdentifier.asUuid 
          term.At            |> putMany "at"           id
          term.Kind          |> putMany "kind"         Event.Kind.name
          term.AggregateId   |> putMany "aggregateId"  id 
          term.CorrelationId |> putMany "correlatonId" UniqueIdentifier.asUuid ]
        |> choose id
        |> sum

      in ones ++ manies

    let translate (prefix : string) term =
      let apply field =
        Predicate.apply $"{prefix}.{field}"
      in [ term.Id            |> apply "event_id"       "id"
           term.At            |> apply "at"             "at"
           term.Kind          |> apply "kind"           "kind"
           term.AggregateId   |> apply "aggregate_id"   "aggregateId"
           term.CorrelationId |> apply "correlation_id" "correlationId" ]
         |> sum


  module Expression =
    let literal = Literal
    let and'    = curry And
    let or'     = curry Or

    let rec parameters =
      function Literal t  -> Term.parameters t
             | And (p, q) -> parameters p ++ parameters q
             | Or  (p, q) -> parameters p ++ parameters q

    let query alias =
      let where (clause : string) =
        if clause.Length <> 0
        then $"WHERE {clause}" else ""

      let select whereClause =
        $"""|SELECT *
            |  FROM journal {alias}
            | {whereClause}
            | ORDER BY {alias}.At"""
        |> String.stripMargin

      let rec reifyConditions =
        function Literal t  -> Term.translate alias t
               | And (p, q) -> $"({reifyConditions p}) AND ({reifyConditions q})"
               | Or  (p, q) -> $"({reifyConditions p}) OR ({reifyConditions q})"
      in select << where << reifyConditions

  let evaluate (filter : Expression) : Event list Persistence.UnitOfWork = 
    monad { let text       = Expression.query "e" filter
            let parameters = Expression.parameters filter
        
            let decodeMemorandum (memo : Memorandum) =
              Memorandum.decodeEvent memo.At memo.Kind memo.Data
        
            let! xs =
              bind decodeMemorandum <!> Memorandum.read
              |> fun q -> sequence <!> Persistence.Query.list q
              |> Persistence.UnitOfWork.read text parameters
        
            return! lift xs }