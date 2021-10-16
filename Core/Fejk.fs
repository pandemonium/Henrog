namespace Henrog.Core.Fejk

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open FSharp.Data

open Henrog.Core.Control


type Person =
  { Name          : string
    Street        : string
    PostalAddress : string
    Phone         : string
    Pin           : string 
    Age           : string }

module Person =
  let parse (fields : Map<string, string>) : Person =
    { Name          = Map.find "Namn:"         fields
      Street        = Map.find "Gata:"         fields
      PostalAddress = Map.find "Postort:"      fields
      Phone         = Map.find "Telefon:"      fields
      Pin           = Map.find "Personnummer:" fields 
      Age           = Map.find "Ålder:"        fields }

module Scrape =
  let (|Cons|Nil|) (xs : _ seq) =
    if Seq.isEmpty xs then Nil 
    else Cons (Seq.head xs, Seq.tail xs)

  let private parseRow (row : HtmlNode) : string * string =
    match row.Descendants "td" with
    | Cons (key, Cons (value, _)) -> key.InnerText (), value.InnerText ()
    | otherwise                   -> failwith "hi"

  let parse : Person Script = 
    monad {
      return parseRow <!> HtmlDocument.Load("https://fejk.se").Descendants "tr"
             |> Map.ofSeq
             |> Person.parse
    }
