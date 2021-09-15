namespace Henrog.Domain.Control

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Henrog.Domain.Model
open Henrog.Domain.Events
open Henrog.Infrastructure.Persistence


type IRuntime =
  (* What is the point of this? *)
  abstract RunUnitOfWork   : 'a UnitOfWork -> 'a Out
  abstract FreshIdentifier : unit -> UniqueIdentifier
  abstract CurrentTime     : unit -> Timestamp

type Environment =
  { Interpreter   : IRuntime 
    Configuration : Configuration }

type 'a EventStream = 'a * StreamCommit

type 'a Effect = ReaderT<Environment, 'a EventStream Out WriterT>

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
            return! context.Environment.Interpreter.RunUnitOfWork work
                    |> lift
                    |> lift }

  let makeContext env : Context =
    { Environment   = env
      CorrelationId = env.Interpreter.FreshIdentifier ()
      Started       = env.Interpreter.CurrentTime () }

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
            return run.Environment.Interpreter.CurrentTime () }


module UniqueIdentifier =
  let fresh : UniqueIdentifier Script =
    monad { let! context = Script.context
            return context.Environment.Interpreter.FreshIdentifier () }

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