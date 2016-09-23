#' Søylediagram med gjennomsnitt/median for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med gjennomsnitt/median
#' for hvert sykehus og kan ta inn ulike numeriske variable.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams SlagFigAndeler
#' @param valgtMaal 'Med' = median. Alt annet gir gjennomsnitt 
#' @param enhetsUtvalg Gjør gruppeutvalg for
#'                 0: Hele landet
#'				         7: Egen region 
#' @param valgtVar Hvilken variabel som skal visualiseres
#'          Alder: alder
#'          AntDagerInnl: liggetid
#'          TidSymptInnlegg: tid fra symptom til innleggelse
#'          TidSymptTrombolyse: tid fra symptom til trombolyse
#'          TidInnleggTrombolyse: tid fra innleggelse til trombolyse
#'          NIHSSinnkomst: NIHSS ved innkomst
#'          NIHSSpreTrombolyse: NIHSS før trombolyse
#'          NIHSSetterTrombolyse: NIHSS etter trombolyse
#'          NIHSSpreTrombektomi: NIHSS før trombektomi
#'          NIHSSetterTrombektomi: NIHSS etter trombektomi
#'          NIHSSendrTrombolyse: Nedgang i NIHSS etter trombolyse
#'          NIHSSendrTrombektomi: nedgang i NIHSS etter trombektomi
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#'
#' @return Søylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
#'
#' @export

SlagFigGjsnGrVar <- function(RegData, valgtVar, valgtMaal='Gjsn', datoFra='2012-04-01', datoTil='2050-12-31', 
		minald=0, maxald=130, erMann='', diagnose='', innl4t='', NIHSSinn='', enhetsUtvalg=0, reshID=0, 
		outfile='', preprosess=1, hentData=0) {


  if (hentData == 1) {		
    RegData <- SlagRegDataSQL(datoFra, datoTil)
  }
  
  # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
  if (preprosess == 1){
    RegData <- SlagPreprosess(RegData=RegData, reshID=reshID)
  }
  
#Når bare skal sammenlikne med region trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
smltxt <- 'alle sykehus'
if (enhetsUtvalg == 7) {	
		smltxt <- as.character(RegData$Region[indEgen1])
		RegData <- RegData[which(RegData$Region == smltxt), ]	#kun egen region
	}

grVar <- 'Avdeling'
RegData[ ,grVar] <- factor(RegData[ ,grVar])
Ngrense <- 10		#Minste antall registreringer for at ei gruppe skal bli vist


RegData$TrombolyseStarttid <- as.POSIXlt(RegData$TrombolyseStarttid, format="%Y-%m-%d %H:%M:%S" )

#if (valgtVar == 'TidSymptTrombolyse') {
#Uten oppvåkningsslag
	#RegData <- RegData[which(RegData$Trombolyse %in% c(1,3) & RegData$VaaknetMedSymptom==2), ] Nå i preprosess
#	RegData$Variabel <- RegData$TidSymptTrombolyse
	  #as.numeric(difftime(RegData$TrombolyseStarttid, RegData$Symptomdebut,  
#			units='hours'))
#	}
#if (valgtVar == 'TidSymptInnlegg') {
#Uten oppvåkningsslag - gjøres nå i preprosess
	#RegData <- RegData[which(RegData$VaaknetMedSymptom==2), ]
#	RegData$Variabel <- RegData$TidSymptInnlegg
#	}
if (valgtVar == 'NIHSSpreTrombolyse') {
	RegData <- RegData[which(RegData$Trombolyse %in% c(1,3)), ]
	}
if (valgtVar == 'NIHSSetterTrombolyse') {
	RegData <- RegData[which(RegData$Trombolyse %in% c(1,3)), ]
	}
if (valgtVar == 'NIHSSpreTrombektomi') {
	RegData <- RegData[which(RegData$Trombektomi %in% c(1,3)), ]
	}
if (valgtVar == 'NIHSSetterTrombektomi') {
	RegData <- RegData[which(RegData$Trombektomi %in% c(1,3)), ]
	}
#if (valgtVar == 'TidInnleggTrombolyse') {   DEFINERT I PREPROSESS
#	RegData <- RegData[which(RegData$Trombolyse %in% c(1,3)), ]
#	RegData$TidInnleggTrombolyse <- as.numeric(difftime(RegData$TrombolyseStarttid, 
#			RegData$Innleggelsestidspunkt, units='mins'))
#diagnose <- 2 Skal ikke lenger se på bare de med infarkt.
#	}
if (valgtVar == 'NIHSSinnkomst') {
	RegData <- RegData[which(RegData$NIHSSikkeUtfort == 0), ]
	}

if (valgtVar %in% c('Alder', 'AntDagerInnl', 'NIHSSinnkomst','NIHSSpreTrombolyse',
                    'NIHSSetterTrombolyse','NIHSSpreTrombektomi', 'NIHSSetterTrombektomi', 
                    'TidInnleggTrombolyse','TidSymptInnlegg','TidSymptTrombolyse')) {
	RegData$Variabel <- RegData[ ,valgtVar] }

#Tar ut de med manglende registrering av valgt variabel og gjør utvalg
SlagUtvalg <- SlagUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
		erMann=erMann, diagnose=diagnose, innl4t=innl4t, NIHSSinn=NIHSSinn)
RegData <- SlagUtvalg$RegData
utvalgTxt <- SlagUtvalg$utvalgTxt

N <- dim(RegData)[1]
if(N > 0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}

Ngrtxt <- paste(', N=', as.character(Ngr), sep='') #paste('N=', as.character(Ngr), sep='')
indGrUt <- as.numeric(which(Ngr < Ngrense))
if (length(indGrUt)==0) { indGrUt <- 0}
Ngrtxt[indGrUt] <- paste(' (<', Ngrense,')',sep='')	#paste('N<', Ngrense,sep='')

vt <- switch(valgtVar, Alder='alder', 
			AntDagerInnl = 'liggetid',
			TidSymptInnlegg =  'tid fra symptom til innleggelse',
			TidSymptTrombolyse = 'tid fra symptom til trombolyse',
			TidInnleggTrombolyse = 'tid fra innleggelse til trombolyse',
			NIHSSinnkomst = 'NIHSS ved innkomst',
			NIHSSpreTrombolyse = 'NIHSS før trombolyse',
			NIHSSetterTrombolyse = 'NIHSS etter trombolyse',
			NIHSSpreTrombektomi = 'NIHSS før trombektomi', 
			NIHSSetterTrombektomi = 'NIHSS etter trombektomi', 
			NIHSSendrTrombolyse = 'nedgang i NIHSS etter trombolyse', 
			NIHSSendrTrombektomi = 'nedgang i NIHSS etter trombektomi')

xaksetxt <- switch(valgtVar, Alder='alder (år)', 
			AntDagerInnl = 'dager',
			TidSymptInnlegg =  'timer',
			TidSymptTrombolyse = 'timer',
			TidInnleggTrombolyse = 'minutter',
			NIHSSinnkomst = 'NIHSSgrad',
			NIHSSpreTrombolyse = 'NIHSSgrad',
			NIHSSetterTrombolyse = 'NIHSSgrad',
			NIHSSpreTrombektomi = 'NIHSSgrad', 
			NIHSSetterTrombektomi = 'NIHSSgrad', 
			NIHSSendrTrombolyse = 'nedgang i NIHSSgrad', 
			NIHSSendrTrombektomi = 'nedgang i NIHSSgrad')


if (valgtMaal=='Med') {
t1 <- 'Median ' } else {t1 <- 'Gjennomsnittlig '}

tittel <- paste(t1, vt, sep='')
			
			
#-----------Figur---------------------------------------

if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	if (dim(RegData)[1]>0) {
	tekst <- paste('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene', sep='')
	} else {tekst <- 'Ingen registrerte data for dette utvalget'}
	title(main=tittel, cex=0.95)	#line=-8, 
	text(0.5, 0.6, tekst, cex=1.2)
	#text(0.5, 0.3, , cex=1.2)	
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
if ( outfile != '') {dev.off()}
} else {

#--------------------------------------------------------
dummy0 <- -0.001
#Kommer ut ferdig sortert!
if (valgtMaal=='Med') {
	MedIQR <- plot(RegData[ ,grVar], RegData$Variabel, notch=TRUE, plot=FALSE)
	MedIQR$stats[ ,indGrUt] <- dummy0
	MedIQR$conf[ ,indGrUt] <- dummy0
	sortInd <- order( MedIQR$stats[3,], decreasing=TRUE) 
	Midt <- as.numeric(MedIQR$stats[3, sortInd])
	KIned <- MedIQR$conf[1, sortInd]
	KIopp <- MedIQR$conf[2, sortInd]
	MedIQRHele <-  boxplot.stats(RegData$Variabel, do.conf = TRUE)
	MidtHele <- as.numeric(MedIQRHele$stats[3])	#median(RegData$Variabel)
	KIHele <- MedIQRHele$conf
#The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).) 
#They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared, 
#and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give 
#roughly a 95% confidence interval for the difference in two medians. 	
	
} else {	#Gjennomsnitt blir standard.
	Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
	SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
	Gjsn[indGrUt] <- dummy0
	SE[indGrUt] <- 0
	sortInd <- order(Gjsn, decreasing=TRUE) 
	Midt <- as.numeric(Gjsn[sortInd])
	KIned <- Gjsn[sortInd] - 2*SE[sortInd]
	KIopp <- Gjsn[sortInd] + 2*SE[sortInd]
	MidtHele <- round(mean(RegData$Variabel),1)
	KIHele <- MidtHele + sd(RegData$Variabel)/sqrt(N)*c(-2,2)
} 


GrNavnSort <- paste(names(Ngr)[sortInd], Ngrtxt[sortInd], sep='')
AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))

#--------------------------FIGUR---------------------------------------------------
soyletxt <- c(sprintf('%.1f',Midt[1:AntGr]), rep('',length(Ngr)-AntGr))	#	#round(Midt[1:AntGr],1)
xmax <-  min(1.1*max(c(Midt, KIned, KIopp)), 1.4*max(Midt))
cexGrNavn <- 0.8
cexSoyletxt <- 0.75

FigTypUt <- figtype(outfile, height=3*800, fargepalett=SlagUtvalg$fargepalett)	#res=96, 
farger <- FigTypUt$farger
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexGrNavn)*0.7)
		#NB: strwidth oppfører seg ulikt avh. av device...
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

	pos <- barplot(Midt, horiz=T, border=NA, col=farger[3],
		xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='', las=1, cex.names=cexGrNavn) 	
	indGrUtPlot <- AntGr+(1:length(indGrUt)) 
	posKI <- pos[1:AntGr]
	ybunn <- 0
	ytopp <- max(posKI)*1.03	 #min(posKI)
	polygon( c(rep(KIHele[1],2), rep(KIHele[2],2)), c(ybunn, ytopp, ytopp, ybunn), 
		col=farger[4], border=farger[4])
	lines(x=rep(MidtHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2) 
	legend(x=xmax/2, y=ytopp*1.01, xpd=TRUE, xjust=0,  yjust=0, pch=c(NA, 15), pt.cex=2, cex=0.9, #y=ytopp+0.5, 
		lwd=c(2,NA), col=c(farger[2], farger[4]), 
		legend = c(paste(smltxt, ': ', MidtHele, sep=''), paste('95% konf.int., N=', N,sep='' )), 
		bty='o', bg='white', box.col='white')
#legend('topright', xjust=1, , cex=1, lwd=2, col=farger[2], 
#	legend=paste(smltxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N,sep='' ), 
#	bty='o', bg='white', box.col='white')

		
	barplot(Midt, horiz=T, border=NA, col=farger[3], xlim=c(0, xmax), add=TRUE,
			font.main=1, xlab = xaksetxt, las=1) 	#xlim=c(0,ymax), #, cex.names=0.5
	title(tittel, font.main=1)	 
	title('med 95% konfidensintervall', line=0.5, font.main=1, cex.main=0.95)
	mtext(at=pos+0.1, GrNavnSort, side=2, las=1, cex=cexGrNavn, adj=1, line=0.25)	#Sykehusnavn 
	text(x=max(strwidth(soyletxt, units='user', cex=cexSoyletxt)), y=pos+0.1, 
				soyletxt, las=1, cex=cexSoyletxt, adj=1, col=farger[4])	#Tekst på søylene (verdi)
#OK	text(x=xmax/20, y=pos+0.1, soyletxt, las=1, cex=0.75, adj=1, col=farger[1])	#Tekst på søylene (verdi)
	
	
#Tekst som angir hvilket utvalg som er gjort
avst <- 0.8
utvpos <- 3	#Startlinje for teksten
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
	
	options(warn = -1)	#Unngå melding om KI med lengde 0. Fungerer av en eller annen grunn ikke i pdf.
	arrows(x0=Midt[-indGrUtPlot]*0.999, y0=posKI, x1=KIopp[-indGrUtPlot], y1=posKI, 
		length=0.5/max(pos), code=2, angle=90, lwd=1.5, col=farger[1])
	arrows(x0=Midt[-indGrUtPlot]*1.001, y0=posKI, x1=KIned[-indGrUtPlot], y1=posKI, 
		length=0.5/max(pos), code=2, angle=90, lwd=1.5, col=farger[1])
par('fig'=c(0, 1, 0, 1)) 
if ( outfile != '') {dev.off()}
#----------------------------------------------------------------------------------
}
}
