% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SlagFigGjsnGrVar.R
\name{SlagFigGjsnGrVar}
\alias{SlagFigGjsnGrVar}
\title{Søylediagram med gjennomsnitt/median for hver grupperingsenhet (sykehus, RHF, ...)}
\usage{
SlagFigGjsnGrVar(RegData, valgtVar, valgtMaal = "Gjsn",
  datoFra = "2012-04-01", datoTil = "2050-12-31", minald = 0,
  maxald = 130, erMann = "", diagnose = "", innl4t = "",
  NIHSSinn = "", enhetsUtvalg = 0, reshID = 0, outfile = "",
  preprosess = 1, hentData = 0)
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}

\item{valgtVar}{Hvilken variabel som skal visualiseres
         Alder: alder
         AntDagerInnl: liggetid
         TidSymptInnlegg: tid fra symptom til innleggelse
         TidSymptTrombolyse: tid fra symptom til trombolyse
         TidInnleggTrombolyse: tid fra innleggelse til trombolyse
         NIHSSinnkomst: NIHSS ved innkomst
         NIHSSpreTrombolyse: NIHSS før trombolyse
         NIHSSetterTrombolyse: NIHSS etter trombolyse
         NIHSSpreTrombektomi: NIHSS før trombektomi
         NIHSSetterTrombektomi: NIHSS etter trombektomi
         NIHSSendrTrombolyse: Nedgang i NIHSS etter trombolyse
         NIHSSendrTrombektomi: nedgang i NIHSS etter trombektomi

Detajer: Her bør man liste opp hvilke variable funksjonen benytter.}

\item{valgtMaal}{'Med' = median. Alt annet gir gjennomsnitt}

\item{datoFra}{Tidligste dato i utvalget (vises alltid i figuren).}

\item{datoTil}{Seneste dato i utvalget (vises alltid i figuren).}

\item{minald}{Alder, fra og med (Standardverdi: 0)}

\item{maxald}{Alder, til og med (Standardverdi: 130)}

\item{erMann}{Kjønn, standard: alt annet enn 0/1 gir begge kjønn
0: Kvinner
1: Menn}

\item{diagnose}{1: Infarkt(I61), 
2: Blødning(I63), 
3: definert(I64), 
standard: alle (ikke spesifisert)}

\item{innl4t}{innlagt innen 4 timer. 
0: nei
1: ja, standard:'' (alt annet)}

\item{NIHSSinn}{NIHSS ved innkomst. Registreringer hvor NIHSS ikke er utførtblir tatt bort. 
Kategorier: 0-5,6-10,11-15,...}

\item{enhetsUtvalg}{Gjør gruppeutvalg for
0: Hele landet
     7: Egen region}

\item{reshID}{Parameter følger fra innlogging helseregister.no og angir
hvilken enhet i spesialisthelsetjenesten brukeren tilhører}

\item{outfile}{Navn på fil figuren skrives til. Standard: '' (Figur skrives
til systemets standard utdataenhet (som regel skjerm))}

\item{preprosess}{Preprosesser data
FALSE: Nei
TRUE: Ja (Standard)}

\item{hentData}{Gjør spørring mot database
0: Nei, RegData gis som input til funksjonen (Standard)
1: Ja}
}
\value{
Søylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
}
\description{
Funksjon som genererer en figur med gjennomsnitt/median
for hvert sykehus og kan ta inn ulike numeriske variable.
Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
}
\details{
Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
}

