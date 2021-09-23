open System

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Henrog.Domain.Model
open Henrog.Domain.Control
open Henrog.Infrastructure.Persistence
open Henrog.Domain.Archive

open NodaTime


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


module Postgres =
  open Npgsql

  let connector configuration : Connector = 
    NpgsqlConnection.GlobalTypeMapper.UseNodaTime ()
    |> ignore

    let connect () =
      new NpgsqlConnection (configuration.ConnectionString) 
      :> Data.IDbConnection
    in { Apply = connect }

let makeRuntime configuration =
  let connector = Postgres.connector configuration.Persistence
  { new IRuntime with
      member __.RunUnitOfWork action = UnitOfWork.apply connector action
      member __.FreshIdentifier   () = Guid.NewGuid () |> UniqueIdentifier.make
      member __.CurrentTime       () = SystemClock.Instance.GetCurrentInstant () }

let queries =
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


[<EntryPoint>]
let main argv =
  let run script =
    let configuration =
      { Persistence = { ConnectionString = "User ID=patrik;Password=;Host=localhost;Port=5432;Database=henrog;Pooling=true;" } }
    in Environment.make (makeRuntime configuration) configuration
       |> ScriptingHost.execute script

  monad { let! id = UniqueIdentifier.fresh
          let! at = Timestamp.now
          let! establishmentId = UniqueIdentifier.makeFresh EstablishmentId
          let! contactId       = UniqueIdentifier.makeFresh ContactId
          let address =
            { Street     = "Lindhöjden sommarbofallet"
              PostalCode = 29472
              City       = "Sölvesborg" }
          let contact =
            { Pin      = PersonalIdentity.unsafePersonnummer 9103095148L
              To       = establishmentId
              FullName = "Victoria Lundholm"
              Address  = address }
          do! EventStream.emit <| ContactAdded (at, (contactId, contact))
          return id }
  |> run (* Error condition if there is a non-empty Event Stream here? *)
  |> printfn "%A"

  0 // return an integer exit code-