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
  #Kun ferdigstilte registreringer:
  # Rapporteket får kun levert ferdigstilte registreringer
  #Kjønn
  RegData$erMann[RegData$Kjonn == 'K'] <- 0
  RegData$erMann[RegData$Kjonn == 'M'] <- 1
  #RegData$erMann <- 0
	#RegData$erMann[RegData$Kjonn == 'M'] <- 1	#kjVar <- 'Kjonn'
  
  #Riktig navn på regions-variabel:
  RegData$Region <- RegData$RHF
  
  #Riktig format på datovariable:
	RegData$InnDato <- as.Date(RegData$Innleggelsestidspunkt, format="%Y-%m-%d") # %H:%M:%S" )	#"%d.%m.%Y"	"%Y-%m-%d"
	RegData$Innleggelsestidspunkt <- strptime(RegData$Innleggelsestidspunkt, "%Y-%m-%d %H:%M:%S")
	#Tar ut registreringer som ikke har innleggelsesdato
	MangelSjekk2 <- format(RegData$Innleggelsestidspunkt, "%H:%M")
	indMed2 <- which(MangelSjekk2 != '00:00')	
	#MangelSjekk <- as.numeric(format(RegData$Innleggelsestidspunkt, "%H"))  +  as.numeric(format(RegData$Symptomdebut, "%M"))/60
	#RegData <- RegData[which(MangelSjekk>0)

	RegData$Symptomdebut <- as.POSIXlt(RegData$Symptomdebut, format="%Y-%m-%d %H:%M:%S" )
	
	RegData$TidSymptInnlegg <- as.numeric(difftime(RegData$Innleggelsestidspunkt, RegData$Symptomdebut,  
			units='hours'))
	#RegData$TimerSymptomdebutInnlegg <- as.numeric(difftime(RegData$Innleggelsestidspunkt, RegData$Symptomdebut,  
	#                                                         units='hours'))
	RegData$TrombolyseStarttid <- as.POSIXlt(RegData$TrombolyseStarttid, format="%Y-%m-%d %H:%M:%S" )
	RegData$TidInnTrombolyse <- as.numeric(difftime(RegData$TrombolyseStarttid, RegData$Innleggelsestidspunkt,   
		units='mins'))
	
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

