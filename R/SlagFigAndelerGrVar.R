#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en grupperingsvariabelen sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams SlagFigAndeler 
#' @param valgtVar Hvilken variabel som skal visualiseres
#'          BehSlagenhet: Behandlet i slagenhet
#'          InnlInnen4eSymptom: Innlagt innen 4t etter symptomdebut
#'          InnlSlagenh: Innlagt direkte i slagehet
#'          LipidI63u80: Utskrevet med lipidsenkning
#'          SvelgtestUtfort: Svelgfunksjon vurdert
#'          TidInnTrombolyse40min:  Trombolyse innen 30 min. etter innleggelse
#'          TrombolyseI63: Hjerneinfarktpasienter som har fått trombolyse
#'          UtBT: Blodtrykksmedikament ved utskriving
#'          UtAntitrombotiskI63: Utskrevet med antitrombotisk behandling. Innleggelser etter 31.12.2013.
#'          UtAntikoagI63atrie: Utskrevet med antikoagulasjon (hjerneinfarktpasienter med atrieflimmer)
#' @param enhetsUtvalg Gjør gruppeutvalg for
#'                 0: Hele landet
#'				   7: Egen region 
#' @return Søylediagram med andeler av valgt variabel for hvert sykehus
#'
#' @export

SlagFigAndelerGrVar <- function(RegData, valgtVar, datoFra='2012-04-01', datoTil='2050-12-31', enhetsUtvalg=0,
		minald=0, maxald=130, erMann='', diagnose='', innl4t='', NIHSSinn='', hentData=0, preprosess=1, reshID=0, outfile='') {

  if (hentData == 1) {		
    RegData <- SlagRegDataSQL(datoFra, datoTil)
  }
  
# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
  if (preprosess==1){
    RegData <- SlagPreprosess(RegData=RegData, reshID=reshID)
  }
  

cexShNavn <- 0.85

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne, 
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
smltxt <- 'Hele landet'
if (enhetsUtvalg == 7) {	
		smltxt <- as.character(RegData$Region[indEgen1])
		RegData <- RegData[which(RegData$Region == smltxt), ]	#kun egen region
		cexShNavn <- 1
	}

grVar <- 'Avdeling'
RegData[ ,grVar] <- factor(RegData[ ,grVar])
Ngrense <- 10		#Minste antall registreringer for at ei gruppe skal bli vist


RegData$Variabel <- 0		

#Tar ut de med manglende registrering av valgt variabel og gjør utvalg
SlagUtvalg <- SlagUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
		erMann=erMann, diagnose=diagnose, innl4t=innl4t, NIHSSinn=NIHSSinn)
RegData <- SlagUtvalg$RegData
utvalgTxt <- SlagUtvalg$utvalgTxt


if (valgtVar == 'BehSlagenhet') {
	RegData$Variabel[union(which(RegData$AvdForstInnlagt==1), which(RegData$AvdUtskrFra==1))] <- 1 
}
if (valgtVar == 'InnlSlagenh') {
	indDirInnlSlag <- union(which(RegData$AvdForstInnlagt==1), 
						which(RegData$AvdForstInnlagtHvilken %in% 3:4))
	RegData$Variabel[indDirInnlSlag] <- 1 
}
if (valgtVar == 'InnlInnen4eSymptom') {
#	Tar ut oppvåkningsslag
	RegData <- RegData[which(RegData$VaaknetMedSymptom==2), ]
	RegData$TidSymptInnlegg <- as.numeric(difftime(RegData$Innleggelsestidspunkt, RegData$Symptomdebut,  units='hours'))
	RegData$Variabel[RegData$TidSymptInnlegg <= 4] <- 1 
}
if (valgtVar == 'LipidI63u80') {
	#		'Hjerneinfarkt (I63) <= 80 år, levende utskrevet 
	RegData <- RegData[which(RegData$UtskrTil != 10), ] # RegData$Slagdiagnose==2 & RegData$Alder <=80
	diagnose <- 2	#'I63'
	minald <- 18
	maxald <- 80
	RegData$Variabel[RegData$UtStatinerLipid==1] <- 1
	}
if (valgtVar == 'SvelgtestUtfort') {
#Av alle. Andel er  de som helt sikkert fått utf. svelgtest, samt de det ikke er relevant for
	RegData$Variabel[which(RegData$SvelgtestUtfort %in% c(1,3))] <- 1
}
if (valgtVar == 'TidInnTrombolyse40min') {	
	diagnose <- 2	#'I63'
	RegData <- RegData[which(RegData$Trombolyse %in% c(1,3)), ]
	RegData$TidInnleggTromb <- as.numeric(difftime(RegData$TrombolyseStarttid,
			RegData$Innleggelsestidspunkt, units='mins'))
	RegData$Variabel[RegData$TidInnleggTromb <= 40] <- 1 
}
if (valgtVar == 'TrombolyseI63') {
	#RegData <- RegData[which(RegData$Slagdiagnose==2), ]		#Slagdiagnose I63
	diagnose <- 2	#'I63'
	RegData$Variabel[which(RegData$Trombolyse %in% c(1,3))] <- 1
}
if (valgtVar == 'UtAntitrombotiskI63') {
	#Slagdiagnose I63 og levende utskrevet
	RegData <- RegData[which(RegData$UtskrTil != 10), ]	#& RegData$Slagdiagnose==2 
	diagnose <- 2	#'I63'
	datoFra <- '2014-01-01' 
	#Antitrombotisk = Platehemmende eller antikoagulerende
	Platehemmere <- c('UtASA','UtKlopidogrel','UtDipyridamol')
	Antikoag <- c('UtWarfarin','UtAndreEnnWarfarin')
	indAntitrombotisk <- which(RegData[ ,c('UtPlatehem','UtAntikoag')]==1, arr.ind=T)[,1]
	RegData$Variabel[indAntitrombotisk] <- 1
}
if (valgtVar == 'UtAntikoagI63atrie') {
	#Slagdiagnose I63, atrieflimmer og levende utskrevet
	RegData <- RegData[which(RegData$UtskrTil != 10 & RegData$Atrieflimmer==1), ]	
	diagnose <- 2	#'I63' 	#RegData$Slagdiagnose==2 & 
	Antikoag <- c('UtWarfarin','UtAndreEnnWarfarin')
	RegData$Variabel[unique(which(RegData[Antikoag]==1, arr.ind=T)[,1])] <- 1
}
if (valgtVar == 'UtBT') {
#Bare levende pasienter
#PostMedBehHoytBT ny fra 1.1.2016
	RegData <- RegData[which(RegData$UtskrTil != 10), ]
	NavnBTsenkUt <- c('UtDiuretica','UtACEhemmer', 'UtA2Antagonist', 'UtBetablokker', 'UtKalsiumantagonist', 'PostMedHoytBT')
	indBTsenkUt <- which(RegData[ ,NavnBTsenkUt]==1, arr.ind=T)[,1]
	RegData$Variabel[indBTsenkUt] <- 1
	#RegData$Variabel[which(RegData$PostMedHoytBT ] <- 1	#Denne kan benyttes hvis variabelen oppdateres bakover i tid
}

#Tar ut de med manglende registrering av valgt variabel og gjør utvalg
SlagUtvalg <- SlagUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
		erMann=erMann, diagnose=diagnose, innl4t=innl4t, NIHSSinn=NIHSSinn)
RegData <- SlagUtvalg$RegData
utvalgTxt <- SlagUtvalg$utvalgTxt

tittel <- switch(valgtVar, InnlSlagenh = 'Innlagt direkte i slagehet' ,
			BehSlagenhet = 'Behandlet i slagenhet',
			InnlInnen4eSymptom= 'Innlagt innen 4t etter symptomdebut',
			SvelgtestUtfort = 'Svelgfunksjon vurdert el. ikke relevant',
			TidInnTrombolyse40min = 'Trombolyse innen 40 min. etter innleggelse',
			TrombolyseI63 = 'Hjerneinfarktpasienter som har fått trombolyse',	
			UtAntitrombotiskI63 = c('Utskrevet med antitrombotisk behandling', 
									'(Innleggelser etter 31.12.2013.)'),
			UtAntikoagI63atrie = c('Utskrevet med antikoagulasjon',
								'(hjerneinfarktpasienter med atrieflimmer)'),
			LipidI63u80 = 'Utskrevet med lipidsenkning',
			UtBT = 'Blodtrykksmedikament ved utskriving')


	N <- dim(RegData)[1]
	Nvar <- tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)
	if(N > 0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}
	AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
	AndelerGr <- round(100*Nvar/Ngr,2)
	
	indGrUt <- as.numeric(which(Ngr < Ngrense))
	if (length(indGrUt)==0) { indGrUt <- 0}
	AndelerGr[indGrUt] <- -0.001
	sortInd <- order(as.numeric(AndelerGr), decreasing=TRUE) 
	Ngrtxt <- paste('N=', as.character(Ngr), sep='')	#
	Ngrtxt[indGrUt] <- paste('N<', Ngrense,sep='')	#paste(' (<', Ngrense,')',sep='')	#
	
	AndelerGrSort <- AndelerGr[sortInd]
	AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
	GrNavnSort <- paste(names(Ngr)[sortInd], ', ',Ngrtxt[sortInd], sep='')
	
	andeltxt <- paste(sprintf('%.1f',AndelerGrSort), '%',sep='') 	#round(as.numeric(AndelerGrSort),1)
	if (length(indGrUt)>0) {andeltxt[(AntGr+1):(AntGr+length(indGrUt))] <- ''}
			
			
#-----------Figur---------------------------------------
if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	if (dim(RegData)[1]>0) {
	tekst <- paste('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene', sep='')
	} else {tekst <- 'Ingen registrerte data for dette utvalget'}
	title(main=tittel)
	text(0.5, 0.6, tekst, cex=1.2)	
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
if ( outfile != '') {dev.off()}

} else {
	
#--------------------------FIGUR---------------------------------------------------
#Innparametre: ...


FigTypUt <- figtype(outfile, height=3*800, fargepalett=SlagUtvalg$fargepalett)	
farger <- FigTypUt$farger
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.7)
		#NB: strwidth oppfører seg ulikt avh. av device...
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

xmax <- max(AndelerGrSort*1.15,100)
pos <- barplot(as.numeric(AndelerGrSort), horiz=T, border=NA, col=farger[3], #main=tittel,
	xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='Andel (%)', las=1, cex.names=0.7)
ybunn <- 0.1
ytopp <- pos[AntGr]+1	#-length(indGrUt)]
lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2) 
legend('topright', xjust=1, , cex=1, lwd=2, col=farger[2], 
	legend=paste(smltxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N,sep='' ), 
	bty='o', bg='white', box.col='white')
mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
title(tittel, line=1, font.main=1, cex.main=1.2)

text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt, 
		las=1, cex=0.8, adj=0, col=farger[1])	#Andeler, hvert sykehus

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

	
par('fig'=c(0, 1, 0, 1)) 
if ( outfile != '') {dev.off()}
#----------------------------------------------------------------------------------
}
}
