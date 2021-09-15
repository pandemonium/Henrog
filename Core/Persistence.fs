namespace Henrog.Infrastructure.Persistence

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open System.Data
open System.Reflection

open Microsoft.Data.SqlClient

open Henrog.Infrastructure
open Henrog.Domain.Model

type Connector =
  { Apply : unit -> IDbConnection }

module TransactionContext =
  let withConnection { Apply = connector } (delta : IDbConnection -> 'a) : 'a =
    let connection = connector ()
    try connection.Open ()
        delta connection
    finally connection.Close ()

  let demarcation connector (unitOfWork : IDbTransaction -> 'a Out) : 'a Out =
    try withConnection connector <| fun c ->
      let tx     = c.BeginTransaction ()
      let output = unitOfWork tx 
      tx.Commit ()
      output
    with e -> throw <| DatabaseException e


type ReadState =
  private { Reader  : IDataReader
            Ordinal : int
            CanRead : bool }

module ReadState =
  let make reader =
    { Reader  = reader
      Ordinal = 0
      CanRead = false }

  let getField (extract : IDataReader -> int -> 'a) state : 'a =
    extract state.Reader state.Ordinal

  let nextField state =
    { state with Ordinal = state.Ordinal + 1 }

  let resetOrdinal state =
    { state with Ordinal = 0 }

  let isNull state : bool =
    state.Reader.IsDBNull state.Ordinal

  let canRead state : bool =
    state.CanRead

  let nextRow state : ReadState =
    { resetOrdinal state with CanRead = state.Reader.Read () }


type 'a Query = State<ReadState, 'a>

module Query =
  let field extract : 'a Query = monad {
    let! value = State.gets <| ReadState.getField extract
    do! State.modify ReadState.nextField
    return value
  }

  let nullable (query : 'a Query) : 'a option Query = monad {
    let! isNull = State.gets ReadState.isNull
    if isNull
      then return None
      else return! map Some query
  }

  let getString   = field (fun r -> r.GetString)
  let getInt      = field (fun r -> r.GetInt32)
  let getGuid     = field (fun r -> r.GetGuid)
  let getDateTime = field (fun r -> r.GetDateTime)
  let getDecimal  = field (fun r -> r.GetDecimal)
  let getFloat    = field (fun r -> r.GetFloat)
  let getDouble   = field (fun r -> r.GetDouble)
  let getBoolean  = field (fun r -> r.GetBoolean)

  let rec enumerate (query : 'a Query) (iteratee : Iteratee<'a, 'b>) : 'b Query = monad {
    match iteratee with
    | Return x ->
      return x
    | Continue f ->
      do! State.modify ReadState.nextRow
      let! canRead = State.gets ReadState.canRead
      
      if canRead
        then let! x = query
             let xs = f <| Emit x
             return! enumerate query xs
        else return! enumerate query <| f End
  }

  let optional query =
    enumerate query Iteratee.tryHead

  let list query =
    enumerate query Iteratee.list

  let select<'a> = monad<'a>

  let apply (query : 'a Query) =
    State.eval query


module Command =
  let parameterize (command : IDbCommand) =
    let converter = System.ComponentModel.TypeDescriptor.GetConverter typeof<DbType>

    let resolveDbType (property : PropertyInfo) =
      let typeName = property.PropertyType.Name
      if typeName = "Instant" then DbType.DateTime2
      else downcast converter.ConvertFrom typeName

    let mkParam (property : PropertyInfo) value =
      let initialize (param : IDbDataParameter) =
        param.ParameterName <- property.Name
        param.DbType        <- resolveDbType property
        param.Value         <- value
      in command.CreateParameter ()
         |> tap initialize

    let addParam (collection : IDataParameterCollection) (prop, value) =
      mkParam prop value
      |> collection.Add
      |> ignore
      collection
    in command.Parameters
       |> Reflect.linearize addParam

type 'a UnitOfWork = ReaderT<IDbTransaction, 'a Out>

module UnitOfWork =
  let createCommand text data : IDbCommand UnitOfWork = 
    monad { let! tx = ask
            let initialze c =
              Command.parameterize c data |> ignore
              c.Transaction <- tx
            return tx.Connection.CreateCommand ()
                   |> tap initialze }

  let write text data : int UnitOfWork = 
    monad { use! command = createCommand text data
            try return command.ExecuteNonQuery ()
            with e -> 
              return! Out.error DatabaseException e }

  let read text data query : 'a UnitOfWork = 
    monad { use! command = createCommand text data    
            try use dataReader = command.ExecuteReader ()
                return dataReader
                       |> ReadState.make
                       |> Query.apply query
            with e -> 
              return! Out.error DatabaseException e }

  let apply connector work : 'a Out =
    TransactionContext.demarcation connector
    <| ReaderT.run work
