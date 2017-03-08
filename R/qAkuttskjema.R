#' Henter data registrert for Hjerneslag
#'
#' Henter data for Hjerneslagregisteret fra "staging"
#'
#' @inheritParams SlagFigAndeler
#'
#' @return Henter dataramma RegData for Hjerneslag
#' @export
#'
#'
qAkuttskjema <- function(datoFra = '2013-01-01', datoTil = '2099-01-01') {
  
  registryName <- "Hjerneslag"
  dbType <- "mysql"
  
  query <- paste0('SELECT
AkutteFokaleutfallPosBilleddiag,
AndreFokaleSympt,
AntDagerInnl,
Armparese,
Ataksi,
Atrieflimmer,
AvdForstInnlagt,
AvdForstInnlagtHvilken,
AvdUtskrFra,
AvdUtskrFraHvilken,
Beinparese,
BevissthetsgradInnleggelse,
BildediagnostikkEkstrakranKar,
BildediagnostikkHjerne,
BildediagnostikkHjerte,
BildediagnostikkIntraraniell,
BoligforholdPre,
BosituasjonPre,
CerebralCTInn,
DeathDate,
Dobbeltsyn,
Dysartri,
Facialisparese,
ForflytningPre,
ForloepID,
Helseenhet AS Avdeling,
Hemikraniektomi,
HjerneblInnen36timer,
HjerneblodningsstoppBeh,
HvorOppstoHjerneslaget,
Innleggelsestidspunkt,
MindreEnn4tSymptInnlegg,
MobiliseringInnen24Timer,
MRSPre,
Neglekt,
NIHSSetterTrombektomi,
NIHSSetterTrombolyse,
NIHSSikkeUtfort,
NIHSSinnkomst,
NIHSSpreTrombektomi,
NIHSSpreTrombolyse,
PaakledningPre,
PatientAge AS Alder,
PatientGender,
PostMedikBehHoytBT,
PreA2Antagonist,
PreACEhemmer,
PreAndreEnnWarfarin,
PreASA,
PreBetablokker,
PreDiabetes,
PreDipyridamol,
PreDiuretica,
PreIngenMedikam,
PreKalsiumantagonist,
PreKlopidogrel,
PreMedHoytBT,
PreStatinerLipid,
PreWarfarin,
RegistreringHjerterytme,
RoykerPre,
Sensibilitetsutfall,
SivilstatusPre,
Slagdiagnose,
Spraakproblem,
SvelgtestUtfort,
Symptomdebut,
Synsfeltutfall,
TidlHjerneslag,
TidlHjerneslagType,
TidlHjerteinfarkt,
TidlTIA,
ToalettbesokPre,
Transportmetode,
Trombektomi,
TrombektomiStarttidspunkt,
Trombolyse,
TrombolyseStarttid,
TverrfagligVurdering,
UnitId AS ReshId,
UtA2Antagonist,
UtACEhemmer,
UtAndreEnnWarfarin,
UtASA,
UtBetablokker,
UtDipyridamol,
UtDiuretica,
UtKalsiumantagonist,
UtKlopidogrel,
UtskrTil,
UtStatinerLipid,
UtWarfarin,
VaaknetMedSymptom,
Vertigo,
UPPER(SkjemaGUID),
FROM
     Akuttskjema
	 
	 WHERE cast(Innleggelsestidspunkt AS date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
	   RegData <- rapbase::LoadRegData(registryName, query, dbType)
  
  return(RegData)
}

