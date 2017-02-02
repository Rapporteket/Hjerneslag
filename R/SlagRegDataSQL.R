
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
SlagRegDataSQL <- function(datoFra = '2013-01-01', datoTil = '2099-01-01') {
  
  registryName <- "Hjerneslag"
  dbType <- "mysql"
  
  query <- paste0('SELECT
o.AarsakManglendeOppf,
a.AkutteFokaleutfallPosBilleddiag,
a.AndreFokaleSympt,
a.AntDagerInnl,
a.Armparese,
a.Ataksi,
a.Atrieflimmer,
a.AvdForstInnlagt,
a.AvdForstInnlagtHvilken,
a.AvdUtskrFra,
a.AvdUtskrFraHvilken,
a.Beinparese,
a.BevissthetsgradInnleggelse,
a.BildediagnostikkEkstrakranKar,
a.BildediagnostikkHjerne,
a.BildediagnostikkHjerte,
a.BildediagnostikkIntraraniell,
o.Boligforhold3mnd,
a.BoligforholdPre,
o.Bosituasjon3mnd,
a.BosituasjonPre,
a.CerebralCTInn,
o.DagerInnleggelseTilDod,
o.DagerSymptDebutTilOppf,
a.DeathDate,
a.Dobbeltsyn,
a.Dysartri,
a.Facialisparese,
o.Forflytning3mnd,
a.ForflytningPre,
a.ForloepID,
a.Helseenhet AS Avdeling,
a.Hemikraniektomi,
a.HjerneblInnen36timer,
a.HjerneblodningsstoppBeh,
UPPER(o.HovedskjemaGUID),
a.HvorOppstoHjerneslaget,
a.Innleggelsestidspunkt,
o.KjorerBilNaa,
o.KjorteBilForHjerneslag,
a.MindreEnn4tSymptInnlegg,
a.MobiliseringInnen24Timer,
o.MRS3mnd,
a.MRSPre,
a.Neglekt,
a.NIHSSetterTrombektomi,
a.NIHSSetterTrombolyse,
a.NIHSSikkeUtfort,
a.NIHSSinnkomst,
a.NIHSSpreTrombektomi,
a.NIHSSpreTrombolyse,
o.OperertHalspulsaare,
o.OppfolgUtf,
o.Paakledning3mnd,
a.PaakledningPre,
a.PatientAge AS Alder,
a.PatientGender,
a.PreA2Antagonist,
a.PreACEhemmer,
a.PreAndreEnnWarfarin,
a.PreASA,
a.PreBetablokker,
a.PreDiabetes,
a.PreDipyridamol,
a.PreDiuretica,
a.PreIngenMedikam,
a.PreKalsiumantagonist,
a.PreKlopidogrel,
a.PreMedHoytBT,
a.PreStatinerLipid,
a.PreWarfarin,
a.PostMedikBehHoytBT,
a.RegistreringHjerterytme,
o.ReinnlagtTypeSlag,
a.UnitId AS ReshId,
o.Royker3mnd,
a.RoykerPre,
a.Sensibilitetsutfall,
o.Sivilstatus3mnd,
a.SivilstatusPre,
UPPER(a.SkjemaGUID),
a.Slagdiagnose,
a.Spraakproblem,
o.SpraakTaleproblEtterHjslag,
a.SvelgtestUtfort,
a.Symptomdebut,
a.Synsfeltutfall,
o.SynsproblEtterHjslag,
a.TidlHjerneslag,
a.TidlHjerneslagType,
a.TidlHjerteinfarkt,
a.TidlTIA,
o.Tilfredshet,
o.Toalettbesok3mnd,
a.ToalettbesokPre,
a.Transportmetode,
a.Trombektomi,
a.TrombektomiStarttidspunkt,
a.Trombolyse,
a.TrombolyseStarttid,
a.TverrfagligVurdering,
a.UtA2Antagonist,
a.UtACEhemmer,
a.UtAndreEnnWarfarin,
a.UtASA,
a.UtBetablokker,
a.UtDipyridamol,
a.UtDiuretica,
a.UtKalsiumantagonist,
a.UtKlopidogrel,
a.UtskrTil,
a.UtStatinerLipid,
a.UtWarfarin,
a.VaaknetMedSymptom,
a.Vertigo,
o.YrkesaktivNaa,
o.YrkesaktivUnderHjerneslag2
FROM
     Akuttskjema a
LEFT JOIN AkuttskjemaOppfolging o
ON a.SkjemaGUID = o.HovedskjemaGUID

WHERE cast(Innleggelsestidspunkt AS date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
 
  RegData <- rapbase::LoadRegData(registryName, query, dbType)
  
  return(RegData)
}
