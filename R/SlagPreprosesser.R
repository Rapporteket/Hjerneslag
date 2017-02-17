#' Preprosesser data fra Hjerneslag
#'
#' Denne funksjonen definerer variabler og fjerner ikke-ferdigstilte registreringer
#'
#' @inheritParams SlagFigAndeler
#'
#' @return Data Dataramme med beregnede og "omnavnede" variable
#'
#' @export
#'
SlagPreprosess <- function(RegData=RegData)
{
  #Definerer operator
  "%u%" <- union
  
  #-----Kun ferdigstilte registreringer:
  # Rapporteket får kun levert ferdigstilte registreringer
  #Kjønn
  RegData$erMann[RegData$PatientGender == 'Female'] <- 0
  RegData$erMann[RegData$PatientGender == 'Male'] <- 1

	#-----Navneendringer gjøres i spørring... Vanskelig for lokal kjøring...
  names(RegData)[which(names(RegData)=='PatientAge')] <- 'Alder'
  names(RegData)[which(names(RegData)=='UnitId')] <-  'ReshId'
  names(RegData)[which(names(RegData)=='Helseenhet')] <- 'Avdeling'

  #-----Endre format 
  var <- c('Ataksi', 'Dysartri','Dobbeltsyn','Neglekt','NIHSSikkeUtfort',
                            'PreIngenMedikam', 'Sensibilitetsutfall','Synsfeltutfall','Vertigo')
  for (k in var) {RegData[ ,k] <- as.logical(RegData[ ,k]) }
  #Bruke sapply?
  
  #-----Riktig format på datovariable, samt identifisere registreringer som ikke har gyldig tidspunkt
	RegData$InnDato <- as.Date(RegData$Innleggelsestidspunkt, format="%Y-%m-%d") # %H:%M:%S" )	#"%d.%m.%Y"	"%Y-%m-%d"
	
	RegData$Innleggelsestidspunkt <- strptime(RegData$Innleggelsestidspunkt, "%Y-%m-%d %H:%M:%S")
	SjekkTidsPktInnlegg <- format(RegData$Innleggelsestidspunkt, "%H:%M")
	indUtInnlegg <- which(SjekkTidsPktInnlegg == '00:00')	
	
	RegData$Symptomdebut <- as.POSIXlt(RegData$Symptomdebut, format="%Y-%m-%d %H:%M:%S" )
	SjekkTidsPktSymptomdebut <- format(RegData$Symptomdebut, "%H:%M")
	#Ta ut ugyldige tidspunkt og oppvåkningsslag
	indUtSymptomdebut <- (which(SjekkTidsPktSymptomdebut == '00:00')	
	                      %u% which(RegData$VaaknetMedSymptom!=2))
	
	RegData$TrombolyseStarttid <- as.POSIXlt(RegData$TrombolyseStarttid, format="%Y-%m-%d %H:%M:%S" )
	SjekkTidsPktTrombStart <- format(RegData$TrombolyseStarttid, "%H:%M")
	#Ta ut ugyldige tidspunkt og filtrere bort de som ikke har fått trombolyse
	indUtTrombStart <- which(SjekkTidsPktTrombStart == '00:00') %u% which(!(RegData$Trombolyse %in% c(1,3)))  #Bare de som har fått trombolyse
#	                %u% which(RegData$Slagdiagnose!=2)  #Bare de med hjerneinfarkt (ikke fra sept.2016)
	
	
	
	#------Definere nye tidsvariable. Ugyldige tidspunkt settes til NA.
	RegData$TidSymptInnlegg <- as.numeric(difftime(RegData$Innleggelsestidspunkt, 
	                                               RegData$Symptomdebut, units='hours'))
	RegData$TidSymptInnlegg[indUtInnlegg %u% indUtSymptomdebut] <- NA   # %u% which(RegData$TidSymptInnlegg<0)
	
	RegData$TidInnleggTrombolyse <- as.numeric(difftime(RegData$TrombolyseStarttid, 
	                                                    RegData$Innleggelsestidspunkt, units='mins'))
	RegData$TidInnleggTrombolyse[indUtInnlegg %u% indUtTrombStart] <- NA  # %u% which(RegData$TidInnleggTrombolyse<0)
	
	RegData$TidSymptTrombolyse <- as.numeric(difftime(RegData$TrombolyseStarttid, 
	                                                  RegData$Symptomdebut, units='hours'))
	RegData$TidSymptTrombolyse[indUtSymptomdebut %u% indUtTrombStart] <- NA   # %u% which(RegData$TidSymptTrombolyse<0)
	
	#Antall dager fra innleggelse til død
	RegData$TidDeath <- as.numeric(difftime(as.POSIXlt(RegData$DeathDate, format = "%Y-%m-%d"),
	          as.POSIXlt(RegData$Innleggelsestidspunkt, format = "%Y-%m-%d"), units='days'))
	RegData$Dod98 <- 0
	RegData$Dod98[RegData$TidDeath <= 98] <- 1
	
	#RegData$TidDeath[which(RegData$TidDeath<0)] <- NA

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
	
	
	
#-------BT-senkende ved innkomst
#Variabelen PreMedikBehHoytBT er verdiløs, dvs. feil ift hva som er reg. i de enkelte BT-variable
#Ny variabel(?): PreMedHoytBT - denne ser også ut til å være fullstendig feil...
NavnBTsenk <- c('PreDiuretica','PreACEhemmer', 'PreA2Antagonist', 'PreBetablokker', 'PreKalsiumantagonist')
RegData$PreBTsenk <- 9
TF <- (RegData[ ,NavnBTsenk])==1
RegData$PreBTsenk[rowSums(TF, na.rm=T)>0] <- 1
Nei <- (RegData[ ,NavnBTsenk])==2
RegData$PreBTsenk[rowSums(Nei, na.rm=T) == length(NavnBTsenk)] <- 2
indFor2016<- which(RegData$InnDato < as.Date('2016-01-01'))
RegData$PreMedikHoytBT[indFor2016] <- RegData$PreBTsenk[indFor2016]

#TestData <- RegData[ , c(NavnBTsenk, 'PreBTsenk', 'PreMedHoytBT')]


#BT-senkende ved utreise
NavnBTsenkUt <- c('UtDiuretica','UtACEhemmer', 'UtA2Antagonist', 'UtBetablokker', 'UtKalsiumantagonist')
RegData$PostBTsenk <- 9
TF <- (RegData[ ,NavnBTsenkUt])==1  #TRUE for de som har minst en "ja"
RegData$PostBTsenk[rowSums(TF, na.rm=T)>0] <- 1
Nei <- (RegData[ ,NavnBTsenkUt])==2  #Nei for alle
RegData$PostBTsenk[rowSums(Nei, na.rm=T) == length(NavnBTsenkUt)] <- 2
RegData$PostMedikBehHoytBT[indFor2016] <- RegData$PostBTsenk[indFor2016] 

#TestData <- RegData[ , c(NavnBTsenkUt, 'PostBTsenk', 'PostMedikBehHoytBT')]


#---Platehemmende ved utreise: UtPlatehem
NavnUtPlate <- c('UtASA', 'UtDipyridamol', 'UtKlopidogrel') 
RegData$UtPlatehem <- 9
TF <- (RegData[ ,NavnUtPlate])==1  #TRUE for de som har minst en "ja"
RegData$UtPlatehem[rowSums(TF, na.rm=T)>0] <- 1
Nei <- (RegData[ ,NavnUtPlate])==2  #Nei for alle
RegData$UtPlatehem[rowSums(Nei, na.rm=T) == length(NavnUtPlate)] <- 2


#---Antikoagulanter ved utreise: UtAntikoag
#UtAntikoag	Kommer fra UtWarfarin og UtAndreEnnWarfarin
NavnUtAntikoag <- c('UtWarfarin', 'UtAndreEnnWarfarin') 
RegData$UtAntikoag <- 9
TF <- (RegData[ ,NavnUtAntikoag])==1  #TRUE for de som har minst en "ja"
RegData$UtAntikoag[rowSums(TF, na.rm=T)>0] <- 1
Nei <- (RegData[ ,NavnUtAntikoag])==2  #Nei for alle
RegData$UtAntikoag[rowSums(Nei, na.rm=T) == length(NavnUtAntikoag)] <- 2


#Tidligere beregnet hos HN-IKT:
#UtPlatehem	Kommer fra UtASA, UtDipyridamol og UtKlopidogrel: 
#         Brukt i KvalInd, AndelGrVar og AndelTid. Def selv i Samledokumentene.
#UtAntikoag	Kommer fra UtWarfarin og UtAndreEnnWarfarin
#         Brukt i KvalInd, AndelGrVar og AndelTid, samt SlagSamleDok og -Land.



  return(invisible(RegData))
}