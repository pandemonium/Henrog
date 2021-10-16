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
    let postalCode, city = 
      person.PostalAddress.Split(" ")
      |> fun xs -> Int32.Parse $"{xs.[0]}{xs.[1]}", xs.[2]
    in { Street        = person.Street
         PostalCode    = postalCode
         City          = city
         Municipality = { Code = 1497; Seat = ""} }

  let generateContact locationId : _ Script =
    monad { let! at        = Timestamp.now
            let! contactId = UniqueIdentifier.makeFresh ContactId
            let! person    = Fejk.Scrape.parse

            let parseAge (text : string) =
              text.Split(" ").[0]
              |> Int64.Parse

            let parsePin (text : string) =
              text.Split "-"
              |> fun xs -> $"{xs.[0]}{xs.[1]}"
              |> Int64.Parse

            let century (age : int64) (pin : string) =
              let thisYear = int64 (at.ToDateTimeUtc().Year)
              let pinYear  = Int64.Parse pin.[0..1]

              if pinYear + age < thisYear 
                then $"20{pin}"
                else $"19{pin}"

            let pin = person.Pin
                      |> century (parseAge person.Age)
                      |> parsePin
                      |> PersonalIdentity.unsafePersonnummer

            let contact =
              { Pin      = pin
                To       = locationId
                Gender   = BinaryGender.fromPin pin
                FullName = person.Name
                Address  = mkAddress person }
            do! EventStream.emit <| ContactAdded (at, (contactId, contact)) 
            return contactId }

  let generateCaregiver locationId : _ Script = 
    monad { let! at        = Timestamp.now
            let! id        = UniqueIdentifier.makeFresh CaregiverId
            let! contactId = generateContact locationId
           
            let caregiver =
              { Self     = contactId
                With     = locationId
                Contract = Code 1257 }
        
            do! EventStream.emit <| CaregiverAdded (at, (id, caregiver))
            return id }

  let generateLocation : _ Script = 
    monad { let! at = Timestamp.now
            let! id = UniqueIdentifier.makeFresh LocationId
            let! x  = Fejk.Scrape.parse
            let location =
              { Name     = $"{x.Name}s etablering"
                Address  = mkAddress x }

            do! EventStream.emit <| LocationRegistered (at, (id, location))
            return id }

  let generateJournal locationId caregiverId : _ Script = 
    monad { let! at      = Timestamp.now
            let! id      = UniqueIdentifier.makeFresh JournalId
            let! subject = generateContact locationId
        
            let journal =
              { Subject   = subject
                Caregiver = caregiverId
                Location  = locationId }
        
            do! EventStream.emit <| JournalCreated (at, (id, journal))
        
            return id }

  let random = System.Random ()

  let noteBodies =
    Text <!> [| "Ramlat och slagit i huvudet."
                "Fullt utvecklad ruptur på Levator Anii."
                "Har ätit för mycket korv och blivit obes."
                "Misstänkt bruk av Kalle Anka-tidningar." |]

  let randomNoteBody : _ Script =
    monad { return 99. * (random.NextDouble ()) % (float noteBodies.Length)
                   |> fun x -> noteBodies.[int x] }

  let generateNote journalId : _ Script = 
    monad { let! at   = Timestamp.now
            let! id   = UniqueIdentifier.makeFresh NoteId
            let! body = randomNoteBody
        
            let note =
              { Journal = journalId
                Heading = HeadingKey "Anmanes"
                Body    = body }
        
            do! EventStream.emit <| NoteAdded (at, (id, note))
        
            return id }

  let rec replicateM (m : _ Script) : int -> _ list Script =
    function 0     -> result []
           | count -> List.cons <!> m <*> replicateM m (count - 1)

  let signNote journalId caregiverId noteId : _ Script = 
    monad { let! at = Timestamp.now
            let! id = UniqueIdentifier.makeFresh NoteId
            let signature =
              { Journal  = journalId
                Note     = noteId
                SignedBy = caregiverId }

            return! EventStream.emit <| NoteSigned (at, signature) }

  let generateLocationWithData count : _ Script = 
    monad { let! locationId  = generateLocation
            let! caregiverId = generateCaregiver locationId

            let makeJournal = 
              monad { let! journalId = generateJournal locationId caregiverId
                      let! noteId    = generateNote journalId
                      do! signNote journalId caregiverId noteId
      
                      return journalId }

            return! makeJournal |> flip replicateM count }

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
  (Test.generateLocationWithData 100 |> EventStream.flush)
  *> Test.doQueries
  |> run (* Error condition if there is a non-empty Event Stream here? *)
  |> printfn "%A"

  0