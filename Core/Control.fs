namespace Henrog.Core.Control

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Henrog.Core.Model
open Henrog.Core.Archive
open Henrog.Infrastructure.Database


type IRuntime =
  (* What is the point of this? *)
  abstract RunUnitOfWork   : 'a UnitOfWork -> 'a Out
  abstract FreshIdentifier : unit -> UniqueIdentifier
  abstract CurrentTime     : unit -> Timestamp

type Environment =
  { Runtime       : IRuntime 
    Configuration : Configuration }

module Environment =
  let make interpreter configuration =
    { Runtime       = interpreter
      Configuration = configuration }

type 'a EventStream = 'a * StreamCommit

type 'a Effect = ReaderT<Environment, 'a EventStream Out WriterT>

module Effect =
  let unsafeRun effect environment =
    ReaderT.run effect environment

type Context =
  { Environment   : Environment
    CorrelationId : UniqueIdentifier
    Started       : Timestamp }

type 'a Script = ReaderT<Context, 'a EventStream Out WriterT>

module Script =
  let context : Context Script = ask
  
  let liftOut out : 'a Script =
    out |> lift |> lift

  let liftUnitOfWork work : 'a Script = 
    monad { let! context = context
            return! context.Environment.Runtime.RunUnitOfWork work
                    |> liftOut }

  let makeContext env : Context =
    { Environment   = env
      CorrelationId = env.Runtime.FreshIdentifier ()
      Started       = env.Runtime.CurrentTime () }

  (* Does this keep the Event Stream? *)
  (* Is this usable somehow? *)
  let evaluate (app : 'a Script) : 'a Effect =
    local makeContext app

  let runWithContext (script : 'a Script) context =
    ReaderT.run script context
    |> WriterT.run

  let run script env =
    evaluate script
    |> flip ReaderT.run env
    |> WriterT.run


module Timestamp =
  let now : Timestamp Script =
    monad { let! run = ask
            return run.Environment.Runtime.CurrentTime () }


module UniqueIdentifier =
  let fresh : UniqueIdentifier Script =
    monad { let! context = Script.context
            return context.Environment.Runtime.FreshIdentifier () }

  let makeFresh constructor =
    constructor <!> fresh

  let correlationId : UniqueIdentifier Script =
    monad { let! context = Script.context
            return context.CorrelationId }


module EventStream =
  open Script

  let private push commit : unit Script =
    tell commit

  let emit event : unit Script = 
    monad { let! eventId       = UniqueIdentifier.fresh
            let! correlationId = UniqueIdentifier.correlationId
            do! Memorandum.ofEvent eventId event
                |> StreamCommit.add correlationId
                |> push }

  let flush (eventful : 'a Script) : 'a Script =
      let write : 'a Script =
          monad { let! x, commit = listen eventful
                  do! PersistentRepresentation.ofStreamCommit commit
                      |> liftUnitOfWork
                  return x }
      let commit =
          runWithContext write >> map fst >> liftOut
      in context >>= commit

module ScriptingHost =
  let execute script environment =
    EventStream.flush script
    |> flip Script.run environment
    |> (<!>) fst