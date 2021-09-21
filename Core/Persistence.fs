namespace Henrog.Infrastructure.Persistence

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open System
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


(* StateT instead? *)
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
      do! modify ReadState.nextRow
      let! canRead = gets ReadState.canRead

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


type Parameters = Object    of obj 
                | List      of Parameter list
                | Composite of Parameters * Parameters

and Parameter = PutString    of string    Put
              | PutInt       of int       Put
              | PutGuid      of Guid      Put
              | PutDateTime  of DateTime  Put
              | PutTimestamp of Timestamp Put
              | PutDecimal   of decimal   Put
              | PutFloat     of float     Put
              | PutDouble    of double    Put
              | PutBool      of bool      Put

and 'a Put = Put of 'a * string

type Put = ApplyPut with
  static member inline ($) (ApplyPut, x : Guid)      = curry Put x >> PutGuid
  static member inline ($) (ApplyPut, x : string)    = curry Put x >> PutString
  static member inline ($) (ApplyPut, x : int)       = curry Put x >> PutInt
  static member inline ($) (ApplyPut, x : DateTime)  = curry Put x >> PutDateTime
  static member inline ($) (ApplyPut, x : Timestamp) = curry Put x >> PutTimestamp

module Parameter =
  type Flat = string * Type * obj 

  let flatten : Parameter -> Flat =
    function PutString (Put (x, name)) -> name, typeof<string>, x :> obj
           | otherwise                 -> failwith "hi"

  let name  = flatten >> fun (x, _, _) -> x
  let type' = flatten >> fun (_, x, _) -> x
  let value = flatten >> fun (_, _, x) -> x

  let inline put name value = (ApplyPut $ value) name

module Command =

  (* This stuff is not good. *)
  let parameterize (command : IDbCommand) (parameters : Parameter list) =
    let converter = System.ComponentModel.TypeDescriptor.GetConverter typeof<DbType>

    let resolveDbType (tpe : Type) =
      let typeName = tpe.Name
      if typeName = "Instant" then DbType.DateTime2
      else downcast converter.ConvertFrom typeName 
    
    let mkParam (parameter : Parameter) =
      let initialize (param : IDbDataParameter) =
        param.ParameterName <- Parameter.name parameter
        param.DbType        <- Parameter.type' parameter |> resolveDbType 
        param.Value         <- Parameter.value parameter
      in command.CreateParameter ()
         |> tap initialize

    let addAll (collection : IDataParameterCollection) =
      List.iter (mkParam >> collection.Add >> ignore) parameters
    in addAll command.Parameters

  let introspect (command : IDbCommand) =
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

module Parameters =
  let inline object x = 
    x :> obj |> Object

  let inline list xs =
    List xs

  let rec apply command =
    function Object object    -> Command.introspect command object |> ignore
           | List parameters  -> Command.parameterize command parameters
           | Composite (p, q) -> apply command p; apply command q

type Parameters with
  static member Zero       = List []
  static member (+) (p, q) = Composite (p, q)

module UnitOfWork =
  let createCommand text parameters : IDbCommand UnitOfWork =
    monad { let! tx = ask
            let initialze (command : IDbCommand) =
              Parameters.apply command parameters
              command.CommandText <- text
              command.Transaction <- tx
            return tx.Connection.CreateCommand ()
                   |> tap initialze }

  let write text data : int UnitOfWork =
    monad { use! command = createCommand text data
            try return command.ExecuteNonQuery ()
            with e ->
              return! throw <| DatabaseException e }

  let read text data query : 'a UnitOfWork =
    monad { use! command = createCommand text data
            try use dataReader = command.ExecuteReader ()
                return dataReader
                       |> ReadState.make
                       |> Query.apply query
            with e ->
              return! throw <| DatabaseException e }

  let apply connector work : 'a Out =
    TransactionContext.demarcation connector
    <| ReaderT.run work
