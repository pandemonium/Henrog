namespace Henrog.Xml

open System
open System.Xml.Linq

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Henrog.Core.Model


module Encode =
  type Document =
    { Encoding : Text.Encoding
      Root     : Node }

  and Node = Element of Element 
           | Text    of string

  and Element =
    { Name       : Name
      Attributes : Attribute list
      Children   : Node list }

  and Attribute = Attribute of Name * string | Empty

  and Name = Namespaced of Namespace * localName : string

  and Namespace = WithPrefix of uri : string * prefix : string
                | Uri        of uri : string
                | Default

  let element name attributes children : Node =
    { Name       = name
      Attributes = attributes
      Children   = children }
    |> Element

  let text = Text

  let attribute =
    curry Attribute

  module Name =
    let inSpace = curry Namespaced

  module Namespace =
    let withPrefix uri prefix = WithPrefix (uri, prefix)
    let uri                   = Uri

    let declare =
      let ns = Name.inSpace Default
      in function WithPrefix (uri, prefix) -> Attribute (ns $"xmlns:{prefix}", uri)
                | Uri uri                  -> Attribute (ns "xmlns", uri)
                | Default                  -> Empty

  module Document =
    let root encoding root =
      { Encoding = encoding
        Root     = root }

    let asString { Encoding = e; Root = r } : string =
      let name =
        function Namespaced (WithPrefix (_, prefix), name) -> $"{prefix}:{name}"
               | Namespaced (_, name)                      -> name

      let attribute =
        function Attribute (n, v) -> $"{name n}=\"{v}\"" | Empty -> ""

      let rec node =
        function Element { Name = n; Attributes = atts; Children = cs } -> 
                 let elName     = name n
                 let children   = node <!> cs |> String.concat ""
                 let attributes = 
                   if atts.IsEmpty then ""
                   else attribute <!> atts
                        |> fun xs -> $""" {String.concat " " xs}"""
                 in $"<{elName}{attributes}>{children}</{elName}>"
               | Text t -> t

      in $"""|<?xml version='1.0' encoding='{e.EncodingName}'?>
             |{node r}"""
         |> String.stripMargin


type Parser<'a, 'e> = ReaderT<'e Automaton, 'a ParseResult>

and 'a Decoder = Parser<'a, XElement>

and 'a Read    = Parser<'a, string>

and 'a Automaton =
  { History : Went list
    Locus   : 'a }

and Went = Child     of XName
         | Attribute of XName
         | Text

and 'a ParseResult = Result<'a, ParseError>

and ParseError = NoSuchElement   of history : Went list * unknown : XName
               | NoSuchAttribute of history : Went list * unknown : XName
               | BadFormat       of history : Went list * expected : string * was : string

module private Automaton =  
  let make locus =
    { History = []; Locus = locus }

  let locus { Locus = f } = f

  module Goto =
    let inline private went locus went (context : _ Automaton) : _ Automaton =
      { History = went :: context.History
        Locus   = locus }

    let inline child name locus =
      local (went locus <| Child name)

    let inline attribute name locus =
      local (went locus <| Attribute name)

    let inline text locus =
      local (went locus Text)

module Shows =
  let xname (xn : XName) =
    $"`{xn.LocalName}` ({xn.NamespaceName})"

  let went =
    function Child name     -> $"Child {xname name}"
           | Attribute name -> $"Attribute {xname name}"
           | Text           -> $"Text"

  let history (h : Went list) =
    went <!> h |> fun xs -> $"""-> {String.concat "\n-> " xs}"""

  let parseError =
    function NoSuchElement (h, unknown)   -> 
              $"History:\n{history h}\n-> Child `{xname unknown}` was not found."
           | NoSuchAttribute (h, unknown) -> 
              $"History:\n{history h}\n-> Attribute `{xname unknown}` was not found."
           | BadFormat (h, expected, was) -> 
              $"History:\n{history h}\nRead `{was}`, expected {expected}."

module Decode =
  (* Could have parser have XDocument as env instead. Add a root combinator
     and others to read other interesting stuff in the document. *)

  let name x = XName.Get(x)

  module Read =
    open NodaTime

    let context : string Automaton Read = ask

    let inline private read spec : ^a Read = 
      context >>= fun self ->
        match tryParse self.Locus with
        | Some i    -> result i
        | otherwise -> throw <| BadFormat (self.History, spec, self.Locus)

    let int : int Read = read "int"

    let string : string Read = Automaton.locus <!> context

    let structured (pattern : 'a Text.IPattern) : 'a Read =
      context >>= fun self ->
        match pattern.Parse self.Locus with
        | r when r.Success -> result r.Value
        | otherwise        -> throw <| BadFormat (self.History, pattern.ToString (), self.Locus)
  
  let context : XElement Automaton Decoder = ask

  let child name parser : _ Decoder =
    context >>= fun self ->
      match self.Locus.Element name with
                     (* This should move into Automaton. *)
      | null  -> throw <| NoSuchElement (self.History, name)
      | child -> Automaton.Goto.child name child parser

  let attribute name read : _ Decoder =
    context >>= fun self ->
      match self.Locus.Attribute name with
                     (* This should move into Automaton. *)
      | null      -> throw <| NoSuchAttribute (self.History, name)
      | attribute -> Automaton.Goto.attribute name attribute.Value read

  let text (read : _ Read) : _ Decoder =
    context >>= fun self -> 
      Automaton.Goto.text self.Locus.Value read

  let textChild name =
    child name << text

  let fromRootElement (parser : _ Decoder) root =
    Automaton.make root
    |> ReaderT.run parser


module ParseRunner =
  open System.Xml.Linq

  let run =
    let xml = 
      """|<?xml version='1.0' encoding='utf-8'?>
         |<html:html xmlns:html="http://www.w3.org/TR/html4/"><html:head><html:title>Hello, world</html:title></html:head><html:body html:onload="alert('Foofoo');"><html:p html:style="font-family='Verdana';" html:data="2021-12- 29">Jahaja?</html:p></html:body></html:html>"""
      |> String.stripMargin

    let NS = "http://www.w3.org/TR/html4/"
    let html n = XName.Get(n, NS)

    let getSome = 
      monad { let! style = Decode.attribute (html "style") Decode.Read.string
              let! date  = Decode.attribute (html "data") 
                           <| Decode.Read.structured NodaTime.Text.LocalDatePattern.Iso
              return style, date }

    let inline at path =
      Decode.child <!> path |> List.reduce (<<)

    let parser : _ Decoder = 
      monad { let! x = at [ html "body"; html "p" ] getSome
              return x }

    XDocument.Parse(xml).Root
    |> Decode.fromRootElement parser
    |> Result.mapError Shows.parseError

module Runner =
  open Encode

  let ns    = Default
  let xmlns = Namespace.withPrefix "http://www.w3.org/TR/html4/" "html"
  let name  = Name.inSpace xmlns
  let el    = name >> element
  let attr =
    name >> attribute

  let run =
    el "html" [ Namespace.declare xmlns ] 
              [ el "head" [] [ el "title" [] [ text "Hello, world" ] ]
                el "body" [ attr "onload" "alert('Foofoo');" ] [ el "p" [] [ text "Jahaja?"] ]
              ]
    |> Document.root Text.Encoding.UTF8
    |> Document.asString