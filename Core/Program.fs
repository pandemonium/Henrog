open System

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Henrog.Domain.Model
open Henrog.Domain.Control
open Henrog.Domain.Archive

open NodaTime


module Make =
  let id x =
    Query.Expression.SuchThat.id Query.This x
    |> Query.Expression.select
  
  let kind x =
    Query.Expression.SuchThat.kind Query.This x
    |> Query.Expression.select

  let before x =
    Query.Expression.SuchThat.at Query.Before x
    |> Query.Expression.select

module Bootstrap =
  open Henrog.Infrastructure.Persistence
  open Npgsql

  let connector configuration : Connector = 
    NpgsqlConnection.GlobalTypeMapper.UseNodaTime ()
    |> ignore

    let connect () =
      new NpgsqlConnection (configuration.ConnectionString) 
      :> Data.IDbConnection
    in { Apply = connect }

  let makeRuntime configuration =
    let connector = connector configuration.Persistence
    { new IRuntime with
        member __.RunUnitOfWork action = UnitOfWork.apply connector action
        member __.FreshIdentifier   () = Guid.NewGuid () |> UniqueIdentifier.make
        member __.CurrentTime       () = SystemClock.Instance.GetCurrentInstant () }

module Test =
  let queries =
    let id   = UniqueIdentifier.make <| Guid.NewGuid ()
    let kind = Event.Kind.CaregiverAdded

    let query = Query.Expression.or' <| Make.id id
                                     <| Make.kind kind

    let now = SystemClock.Instance.GetCurrentInstant ()

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
    in Environment.make (Bootstrap.makeRuntime configuration) configuration
       |> ScriptingHost.execute script

  let emitPerson : unit Script =
    monad { let! at = Timestamp.now
            let! establishmentId = UniqueIdentifier.makeFresh EstablishmentId
            let! contactId       = UniqueIdentifier.makeFresh ContactId
            let address =
              { Street     = "Kastvindsgatan"
                PostalCode = 41134
                City       = "Göteborg" }
            let contact =
              { Pin      = PersonalIdentity.unsafePersonnummer 7505264510L
                To       = establishmentId
                FullName = "Oskar Mossberg"
                Address  = address }
            do! EventStream.emit <| ContactAdded (at, (contactId, contact)) }

  monad { let! id = UniqueIdentifier.fresh
//          do! emitPerson

          let! events = Query.Expression.star
                        |> Query.evaluate
                        |> Script.liftUnitOfWork

          return events }
  |> run (* Error condition if there is a non-empty Event Stream here? *)
  |> printfn "%A"

  0