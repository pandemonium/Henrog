namespace Henrog.Core.Fejk

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open FSharp.Data


type Person =
  { Name   : string
    Street : string
    City   : string
    Phone  : string
    Pin    : string }

module Person =
  let parse (fields : Map<string, string>) : Person =
    { Name   = Map.find "Namn:"         fields
      Street = Map.find "Gata:"         fields
      City   = Map.find "Postort:"      fields
      Phone  = Map.find "Telefon:"      fields
      Pin    = Map.find "Personnummer:" fields }

module Scrape =
  let (|Cons|Nil|) (xs : _ seq) =
    if Seq.isEmpty xs then Nil 
    else Cons (Seq.head xs, Seq.tail xs)

  let parseRow (row : HtmlNode) : string * string =
    match row .Descendants "td" with
    | Cons (key, Cons (value, _)) -> key.InnerText (), value.InnerText ()
    | otherwise                   -> failwith "hi"

  let load =
    parseRow <!> HtmlDocument.Load("https://fejk.se").Descendants "tr"
    |> Map.ofSeq
    |> Person.parse