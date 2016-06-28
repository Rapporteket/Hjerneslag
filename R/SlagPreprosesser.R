#' Preprosesser data fra Hjerneslag
#'
#' Denne funksjonen definerer variabler og fjerner ikke-ferdigstilte registreringer
#'
#' @inheritParams SlagFigAndeler
#'
#' @return Data En list med det filtrerte datasettet og sykehusnavnet som tilsvarer reshID
#'
#' @export
#'
SlagPreprosess <- function(RegData=RegData, reshID=reshID)
{
  #Definerer operator
  "%u%" <- union
  
  #Kun ferdigstilte registreringer:
  # Rapporteket får kun levert ferdigstilte registreringer
  #Kjønn
  RegData$erMann[RegData$Kjonn == 'K'] <- 0
  RegData$erMann[RegData$Kjonn == 'M'] <- 1
  #RegData$erMann <- 0
	#RegData$erMann[RegData$Kjonn == 'M'] <- 1	#kjVar <- 'Kjonn'
  
  #Riktig navn på regions-variabel:
  RegData$Region <- RegData$RHF
  
  #-------Riktig format på datovariable, samt identifisere registreringer som ikke har gyldig tidspunkt
	RegData$InnDato <- as.Date(RegData$Innleggelsestidspunkt, format="%Y-%m-%d") # %H:%M:%S" )	#"%d.%m.%Y"	"%Y-%m-%d"
	
	RegData$Innleggelsestidspunkt <- strptime(RegData$Innleggelsestidspunkt, "%Y-%m-%d %H:%M:%S")
	SjekkTidsPktInnlegg <- format(RegData$Innleggelsestidspunkt, "%H:%M")
	indUtInnlegg <- which(SjekkTidsPktInnlegg == '00:00')	
	
	RegData$Symptomdebut <- as.POSIXlt(RegData$Symptomdebut, format="%Y-%m-%d %H:%M:%S" )
	SjekkTidsPktSymptomdebut <- format(RegData$Symptomdebut, "%H:%M")
	#Ta ut ugyldige tidspunkt og oppvåkningsslag
	indUtSymptomdebut <- (which(SjekkTidsPktSymptomdebut == '00:00')	%u% which(RegData$VaaknetMedSymptom!=2))
	
	RegData$TrombolyseStarttid <- as.POSIXlt(RegData$TrombolyseStarttid, format="%Y-%m-%d %H:%M:%S" )
	SjekkTidsPktTrombStart <- format(RegData$TrombolyseStarttid, "%H:%M")
	#Ta ut ugyldige tidspunkt og filtrere bort de som ikke har fått trombolyse
	indUtTrombStart <- which(SjekkTidsPktTrombStart == '00:00') %u% which(!(RegData$Trombolyse %in% c(1,3)))
	 
	
	#------Definere nye tidsvariable. Ugyldige tidspunkt settes til NA.
	RegData$TidSymptInnlegg <- as.numeric(difftime(RegData$Innleggelsestidspunkt, RegData$Symptomdebut, units='hours'))
	RegData$TidSymptInnlegg[indUtInnlegg %u% indUtSymptomdebut %u% which(RegData$TidSymptInnlegg<0)] <- NA
	
	RegData$TidInnleggTrombolyse <- as.numeric(difftime(RegData$TrombolyseStarttid, RegData$Innleggelsestidspunkt, units='mins'))
	RegData$TidInnleggTrombolyse[indUtInnlegg %u% indUtTrombStart %u% which(RegData$TidInnleggTrombolyse<0)] <- NA
	
	RegData$TidSymptTrombolyse <- as.numeric(difftime(RegData$TrombolyseStarttid, RegData$Symptomdebut, units='hours'))
	RegData$TidSymptTrombolyse[indUtSymptomdebut %u% indUtTrombStart %u% which(RegData$TidSymptTrombolyse<0)] <- NA
	
	#Antall dager fra innleggelse til død
	RegData$TidDeath <- as.numeric(difftime(as.POSIXlt(RegData$DeathDate, format = "%Y-%m-%d"),
	          as.POSIXlt(RegData$Innleggelsestidspunkt, format = "%Y-%m-%d"), units='days'))
	RegData$TidDeath[which(RegData$TidDeath<0)] <- NA

	#Tidspunktet 00:00 angir at tidspunkt er ukjent. Disse må tas ut når man skal se på differanser til andre tidspunkt.
	#Dette kan gjøres på flere måter:
	#RegData$Tidsvariabel <- strptime(RegData$Tidsvariabel, "%Y-%m-%d %H:%M:%S")
	#MangelSjekk <- as.numeric(format(RegData$Tidsvariabel, "%H"))  +  as.numeric(format(RegData$Tidsvariabel, "%M"))/60
	#indMed <- which(MangelSjekk>0)
	
	#RegData$Tidsvariabel <- strptime(RegData$Tidsvariabel, "%Y-%m-%d %H:%M:%S")
	#MangelSjekk2 <- format(RegData$Tidsvariabel, "%H:%M")
	#indMed2 <- which(MangelSjekk2 != '00:00')
	#Eventuelt kan man snu utvalget, lage en dvs. lage 
	#indUT <- which(MangelSjekk2 == '00:00')
	#og sette disse lik ‘NA’.
	
	
	
	
	#Div. variabel"mapping"
	RegData$PreMedikBehHoytBT <- RegData$PreMedHoytBT 
	RegData$PreKalsiumanatgonist <- RegData$PreKalsiumantagonist
	
	indAfasi <- which(RegData$Afasi %in% c(1,2,9))
	RegData$SpraakTaleproblem[indAfasi] <- RegData$Afasi[indAfasi]

	#Da har vi ikke sikret oss mot evt. «villregistrering» i SpraakTaleproblem.
	#Evt. kan vi gjøre sånn:
#	indAfasi <- which(RegData$Afasi %in% c(1,2,9))
#	indSprTale <- which(RegData$ SpraakTaleproblem %in% c(1,2,9))
#	RegData$Spraakproblem <- -1
#	RegData$Spraakproblem[indSprTale] <- RegData$SpraakTaleproblem[indSprTale]
	#	RegData$Spraakproblem[indAfasi] <- RegData$Afasi[indAfasi]
	

  return(invisible(RegData))
}


# RegData$ShNavn <- RegData$Avdeling	#factor(RegData$ReshId)

