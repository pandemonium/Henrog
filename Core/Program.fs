open System

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Henrog.Domain.Model
open Henrog.Domain.Archive

module Make =
  let id x =
    Query.Term.select (fun p -> { p with Id = Query.This x }) Query.Term.any
    |> Query.Expression.literal
  
  let kind x =
    Query.Term.select (fun p -> { p with Kind = Query.This x }) Query.Term.any
    |> Query.Expression.literal

  let before x =
    Query.Term.select (fun p -> { p with At = Query.Before x }) Query.Term.any
    |> Query.Expression.literal

[<EntryPoint>]
let main argv =
  let id   = UniqueIdentifier.make <| System.Guid.NewGuid ()
  let kind = Event.Kind.CaregiverAdded

  let query = Query.Expression.or' <| Make.id id
                                   <| Make.kind kind

  let now = NodaTime.SystemClock.Instance.GetCurrentInstant()

  let query' = Query.Expression.and' query <| Make.before now

  query'
  |> Query.Expression.parameters
  |> printfn "%A"

  query'
  |> Query.Expression.query "e"
  |> printfn "%A"

  0 // return an integer exit code-