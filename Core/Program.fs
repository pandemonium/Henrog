open System

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Henrog.Core
open Henrog.Core.Model
open Henrog.Core.Control
open Henrog.Core.Archive

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
  open Henrog.Infrastructure.Database
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
  let queries () =
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

  let mkAddress (person : Fejk.Person) =
    { Street        = person.Street
      PostalAddress = person.PostalAddress }

  let generateContact establishmentId : _ Script =
    monad { let! at        = Timestamp.now
            let! contactId = UniqueIdentifier.makeFresh ContactId
            let! person    = Fejk.Scrape.parse

            let parsePin (text : string) =
              text.Split "-"
              |> fun xs -> $"{xs.[0]}{xs.[1]}"
              |> Int64.Parse

            let contact =
              { Pin      = parsePin person.Pin
                           |> PersonalIdentity.unsafePersonnummer
                To       = establishmentId
                FullName = person.Name
                Address  = mkAddress person }
            do! EventStream.emit <| ContactAdded (at, (contactId, contact)) 
            return contactId }

  let generateCaregiver establishmentId : _ Script = 
    monad { let! at        = Timestamp.now
            let! id        = UniqueIdentifier.makeFresh CaregiverId
            let! contactId = generateContact establishmentId
           
            let caregiver =
              { Self     = contactId
                With     = establishmentId
                Contract = Contract.unsafeContract 1257 }
        
            do! EventStream.emit <| CaregiverAdded (at, (id, caregiver))
            return id }

  let generateEstablishment : _ Script = 
    monad { let! at = Timestamp.now
            let! id = UniqueIdentifier.makeFresh EstablishmentId
            let! x  = Fejk.Scrape.parse
            let establishment =
              { Name    = $"{x.Name}s etablering"
                Address = mkAddress x }
        
            do! EventStream.emit <| EstablishmentCreated (at, (id, establishment))
        
            return id }

  let generateJournal establishmentId caregiverId : _ Script = 
    monad { let! at      = Timestamp.now
            let! id      = UniqueIdentifier.makeFresh JournalId
            let! subject = generateContact establishmentId
        
            let journal =
              { Subject       = subject
                Caregiver     = caregiverId
                Establishment = establishmentId }
        
            do! EventStream.emit <| JournalCreated (at, (id, journal))
        
            return id }

  let rec performN (theThing : 'a Script ) : int -> 'a list Script =
    function 0 -> result []
           | n -> monad { let! x = theThing
                          let! xs = performN theThing (n - 1)
                          return x::xs }

  let generateEstablishmentWithData count : _ Script = monad {
    let! establishmentId = generateEstablishment
    let! caregiverId     = generateCaregiver establishmentId

    return! performN <| generateJournal establishmentId caregiverId
                     <| count
  }

  let doQueries : _ Script =
    monad { let! id = UniqueIdentifier.fresh
            let q = Query.Expression.SuchThat.kind Query.This Event.Kind.ContactAdded
                    |> Query.Expression.select

            let! events = Query.Expression.star
                          |> Query.evaluate
                          |> Script.liftUnitOfWork

            return events }

[<EntryPoint>]
let main argv =
  let run script =
    let configuration =
      { Persistence = { ConnectionString = "User ID=patrik;Password=;Host=localhost;Port=5432;Database=henrog;Pooling=true;" } }
    in Environment.make (Bootstrap.makeRuntime configuration) configuration
       |> ScriptingHost.execute script

//  Scrape.load
  (Test.generateEstablishmentWithData 10 |> EventStream.flush)
  *> Test.doQueries
  |> run (* Error condition if there is a non-empty Event Stream here? *)
  |> printfn "%A"

  0