open System

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

type Context =
  { Name : string }

type Error = Wtf of string | Unclean

type 'a Out = Result<'a, Error>

type 'a EventScript = 'a * string list

type 'a Eventful = ReaderT<Context, 'a EventScript Out WriterT>

type 'a Program = ReaderT<Context, 'a Out>

type 'a Db = Make of 'a

module Eventful =
  let foo (a : 'a Eventful) = monad {
    let! p, q = listen a
    return! a
  }

  let liftDb (db : 'a Db) : 'a Program =
    failwith "hi"

  let liftDb' (Make a) : 'a Eventful =
    printfn "liftDb': %A" a
    result a

  let publish events : unit Db =
    printfn "publish: %A" events
    Make ()

  let emit x : unit Eventful = tell [x]

  let make (x : int) : int Eventful = monad {
    let! ctx = ask
    do! emit $"Added {x} to {ctx.Name}"    
    return x
  }
 
  let callGod (m : _ Eventful) : _ Eventful = monad {
    let! x = m
    do! emit "Calling God!"
    return x
  }

  let context : Context Eventful = ask

  let run program = 
    ReaderT.run program >> WriterT.run

  let prepare (block : _ Eventful) : _ Eventful = 
    monad { let! _, ev = listen block
            return! if List.isEmpty ev then block 
                                       else throw Unclean }

  let commit (eventful : 'a Eventful) : 'a Eventful = 
    let write : 'a Eventful = 
      monad { let! a, events = listen eventful
              do! publish events |> liftDb'
              return a }
    let flush =
      run write >> map fst >> WriterT.lift >> ReaderT.lift
    in ask >>= flush

[<EntryPoint>]
let main argv =
  let ctx = { Name = "Planet Earth" }
  Eventful.make 10
  |> Eventful.callGod
  |> Eventful.commit
  |> flip Eventful.run ctx
  |> printfn "%A"
  0 // return an integer exit code-