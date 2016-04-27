#' Preprosesser data fra Hjerneslag
#'
#' Denne funksjonen definerer variabler og fjerner ikke-ferdigstilte registreringer
#'
#' @inheritParams FigAndeler
#'
#' @return Data En list med det filtrerte datasettet og sykehusnavnet som tilsvarer reshID
#'
#' @export
#'
SlagPreprosess <- function(RegData=RegData, reshID=reshID)
{
  #Kun ferdigstilte registreringer:
  # Rapporteket får kun levert ferdigstilte registreringer
  #Kjønn
  RegData$erMann[RegData$Kjonn == 'K'] <- 0
  RegData$erMann[RegData$Kjonn == 'M'] <- 1
  #RegData$erMann <- 0
	#RegData$erMann[RegData$Kjonn == 'M'] <- 1	#kjVar <- 'Kjonn'

  #Riktig format på datovariable:
	RegData$InnDato <- as.Date(RegData$Innleggelsestidspunkt, format="%Y-%m-%d") # %H:%M:%S" )	#"%d.%m.%Y"	"%Y-%m-%d"
	RegData <- RegData[which(RegData$Innleggelsestidspunkt!=''),]	#Tar ut registreringer som ikke har innleggelsesdato
	RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$Innleggelsestidspunkt, format="%Y-%m-%d %H:%M:%S" )
	RegData$Symptomdebut <- as.POSIXlt(RegData$Symptomdebut, format="%Y-%m-%d %H:%M:%S" )
	RegData$TidSymptInnlegg <- as.numeric(difftime(RegData$Innleggelsestidspunkt, RegData$Symptomdebut,  
			units='hours'))
	#RegData$TimerSymptomdebutInnlegg <- as.numeric(difftime(RegData$Innleggelsestidspunkt, RegData$Symptomdebut,  
	#                                                         units='hours'))
	#RegData$TimerSymptomdebutInnlegg[which(RegData$VaaknetMedSymptom!=2)] <- NA
	RegData$TrombolyseStarttid <- as.POSIXlt(RegData$TrombolyseStarttid, format="%Y-%m-%d %H:%M:%S" )
	RegData$TidInnTrombolyse <- as.numeric(difftime(RegData$TrombolyseStarttid, RegData$Innleggelsestidspunkt,   
		units='mins'))
	RegData$PreMedikBehHoytBT <- RegData$PreMedHoytBT 
	RegData$PreKalsiumanatgonist <- RegData$PreKalsiumantagonist

  #Riktig navn på regions-variabel:
	RegData$Region <- RegData$RHF


  return(invisible(RegData))
}


# RegData$ShNavn <- RegData$Avdeling	#factor(RegData$ReshId)

