namespace Henrog.Core

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

type Visit =
  { At      : Model.Timestamp
    Subject : Model.Contact }

// De grejer jag kan få betalt för är:
// 
// 
// Telefonkontakt 60-ish kr
// Gruppbesök - 150-ish kr
// "Enskilt besök normalarvode" 381 kr
// Digitalt besök 381 kr
// Parallellbesök (max tre på en timme) 381 kr
// "Enskilt besök särskilt arvode" 900 kr

type VisitationsFileInfo =
  { Caregiver : Model.Caregiver
    Visits    : Visit list }