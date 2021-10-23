namespace Henrog.Xml

open System

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Henrog.Core.Model

module Output =
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

  module Node =
    let element name attributes children : Node =
      { Name       = name
        Attributes = attributes
        Children   = children }
      |> Element

    let text = Text

  module Attribute =
    let make =
      curry Attribute

  module Name =
    let inSpace space = curry Namespaced space
    
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

      let strings = String.concat

      let rec node =
        function Element { Name = n; Attributes = atts; Children = cs } -> 
                 let elName     = name n
                 let children   = node <!> cs |> strings ""
                 let attributes = 
                   if atts.IsEmpty then ""
                   else attribute <!> atts
                        |> fun xs -> $""" {strings " " xs}"""
                 in $"<{elName}{attributes}>{children}</{elName}>"
               | Text t -> t

      in $"""|<?xml version='1.0' encoding='{e.EncodingName}'?>
             |{node r}"""
         |> String.stripMargin


module Parsing =
  open System.Xml.Linq

  (* Could have parser have XDocument as env instead. Add a root combinator
     and others to read other interesting stuff in the document. *)

  type 'a Parser = ReaderT<XElement Context, 'a ParseResult>

  and 'a Read    = ReaderT<string Context, 'a ParseResult>

  and 'a Context =
    { History : Goto list
      Focus   : 'a }

  and Goto = Child     of XName
           | Attribute of XName
           | Text

  and 'a ParseResult = Result<'a, ParseError>

  and ParseError = NoSuchElement   of history : Goto list * unknown : XName
                 | NoSuchAttribute of history : Goto list * unknown : XName
                 | BadFormat       of history : Goto list * expeced : string * was : string

  module Context =  
    let make focus =
      { History = []; Focus = focus }

    let focus { Focus = f } = f

    let inline private goto (x : 'b) push (context : 'a Context) : 'b Context =
      { History = push :: context.History
        Focus   = x }

    let inline child name it (p : 'a Parser) : 'a Parser =
      local (goto it <| Child name) p

    let inline attribute name it (r : _ Read) : _ Parser =
      local (goto it <| Attribute name) r

    let inline text it =
      local (goto it Text)

  let name x = XName.Get(x)

  module Read =
    open NodaTime

    let context : string Context Read = ask

    let inline private read spec : ^a Read = 
      monad { let! self = context
              match tryParse self.Focus with
              | Some i    -> return i
              | otherwise -> return! throw <| BadFormat (self.History, spec, self.Focus) }

    let int : int Read = read "int"

    let string : string Read = Context.focus <!> context

    let structured (pattern : 'a Text.IPattern) : 'a Read = 
      monad { let! self = context
              match pattern.Parse self.Focus with
              | r when r.Success -> return r.Value
              | otherwise        -> return! throw <| BadFormat (self.History, pattern.ToString (), self.Focus) }

  
  module Element =
    let context : XElement Context Parser = ask

    let child name parser : _ Parser =
      context >>= fun self ->
        match self.Focus.Element name with
        | null  -> throw <| NoSuchElement (self.History, name)
        | child -> Context.child name child parser

    let attribute name read : _ Parser =
      context >>= fun self ->
        match self.Focus.Attribute name with
        | null      -> throw <| NoSuchAttribute (self.History, name)
        | attribute -> Context.attribute name attribute.Value read

    let text (read : _ Read) : _ Parser =
      context >>= fun self -> 
        Context.text self.Focus.Value read

    let textChild name =
      child name << text

  let parse (parser : _ Parser) root =
    Context.make root
    |> ReaderT.run parser


module ParseRunner =
  open System.Xml.Linq
  open Parsing

  let run =
    let xml = 
      """|<?xml version='1.0' encoding='utf-8'?>
         |<html:html xmlns:html="http://www.w3.org/TR/html4/"><html:head><html:title>Hello, world</html:title></html:head><html:body html:onload="alert('Foofoo');"><html:p html:style="font-family='Verdana';" html:data="2021-12-29">Jahaja?</html:p></html:body></html:html>"""
      |> String.stripMargin

    let NS = "http://www.w3.org/TR/html4/"
    let html n = XName.Get(n, NS)

    let getSome = 
      monad { let! style = Element.attribute (html "style") Read.string
              let! date  = Element.attribute (html "data") 
                           <| Read.structured NodaTime.Text.LocalDatePattern.Iso
              return style, date }

    let inline at path =
      Element.child <!> path |> List.reduce (<<)

    let parser : _ Parser = monad {
      let! x = at [ html "body"; html "p" ] getSome

      return x
    }

    XDocument.Parse(xml).Root
    |> Parsing.parse parser

module Runner =
  open Output

  let ns    = Default
  let xmlns = Namespace.withPrefix "http://www.w3.org/TR/html4/" "html"
  let name  = Name.inSpace xmlns
  let el    = name >> Node.element
  let text  = Node.text
  let attr =
    name >> Attribute.make

  let run =
    el "html" [ Namespace.declare xmlns ] 
              [ el "head" [] [ el "title" [] [ text "Hello, world" ] ]
                el "body" [ attr "onload" "alert('Foofoo');" ] [ el "p" [] [ text "Jahaja?"] ]
              ]
    |> Document.root Text.Encoding.UTF8
    |> Document.asString