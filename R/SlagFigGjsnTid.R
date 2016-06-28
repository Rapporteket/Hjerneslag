#' Utvikling over tid for gjennomsnitt/median av valgt variabel
#'
#' Figuren viser gjennomsnitt/median per år med konfidensintervall for valgt variabel.
#' I bakgrunn vises konfidensintervall for resten av landet.
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams SlagFigAndeler 
#' @param valgtMaal
#'        'Gjsn': gir middelverdi (standard)
#'        'Med': gir median
#' @param valgtVar Hvilken variabel som skal visualiseres
#'          Alder: Alder
#'          AntDagerInnl: liggetid
#'          NIHSSinnkomst: NIHSS ved innkomst
#'          NIHSSpreTrombolyse: NIHSS før trombolyse
#'          NIHSSetterTrombolyse: NIHSS etter trombolyse
#'          TidInnleggTrombolyse: Tid fra innleggelse til trombolyse
#'          TidSymptInnlegg: Tid fra symptom til innleggelse
#'          TidSymptTrombolyse: Tid fra symptom til trombolyse
#'          
#' @return Linjediagram som viser utvikling over tid for valgt variabel
#'
#' @export
SlagFigGjsnTid <- function(RegData, valgtVar, datoFra='2013-01-01', datoTil='3000-12-31', 
                    minald=0, maxald=130, erMann='',   diagnose='', innl4t='', NIHSSinn='', reshID, outfile='', 
                    enhetsUtvalg=1, valgtMaal='', preprosess=1, hentData=0){

  
  if (hentData == 1) {		
    RegData <- SlagRegDataSQL(datoFra, datoTil)
  }
  
# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
  if (preprosess==1){
    RegData <- SlagPreprosess(RegData=RegData, reshID=reshID)
  }
  
RegData$Aar <- 1900 + strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
RegData$Variabel <- 0	

#retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
grtxt <- ''		#Spesifiseres for hver enkelt variabel
grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
subtxt <- ''	#Benevning
flerevar <- 0
antDes <- 1


#Når skal sammenlikne region - eller ikke sammenlikne, 
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(2,6,7)) {	
		RegData <- switch(as.character(enhetsUtvalg),
						'2' = RegData[which(RegData$ReshId == reshID),],	#kun egen enhet
						'6' = RegData[which(RegData$RHF == as.character(RegData$RHF[indEgen1])),],	#sml region
						'7' = RegData[which(RegData$RHF == as.character(RegData$RHF[indEgen1])),])	#kun egen region
	}

#if (valgtVar == 'TidSymptInnlegg') {
  #Uten oppvåkningsslag
#  RegData <- RegData[which(RegData$VaaknetMedSymptom==2), ]
 # RegData$Symptomdebut <- as.POSIXlt(RegData$Symptomdebut, format="%Y-%m-%d %H:%M:%S" )
  #RegData$TidSymptInnlegg <- as.numeric(difftime(RegData$Innleggelsestidspunkt, RegData$Symptomdebut,  
	#	units='hours'))
  #RegData$Variabel <- RegData$TidSymptInnlegg
#}
#if (valgtVar == 'TidSymptTrombolyse') {
  #Uten oppvåkningsslag
#  RegData <- RegData[which(RegData$Trombolyse %in% c(1,3) & RegData$VaaknetMedSymptom==2), ]
 # RegData$Symptomdebut <- as.POSIXlt(RegData$Symptomdebut, format="%Y-%m-%d %H:%M:%S" )
#  RegData$TrombolyseStarttid <- as.POSIXlt(RegData$TrombolyseStarttid, format="%Y-%m-%d %H:%M:%S" )
 # RegData$Variabel <- as.numeric(difftime(RegData$TrombolyseStarttid, RegData$Symptomdebut,  
  #                                        units='hours'))
#}
#if (valgtVar == 'TidInnleggTrombolyse') {
 # RegData <- RegData[which(RegData$Trombolyse %in% c(1,3)), ]
  #RegData$TrombolyseStarttid <- as.POSIXlt(RegData$TrombolyseStarttid, format="%Y-%m-%d %H:%M:%S" )
#  RegData$Variabel <- as.numeric(difftime(RegData$TrombolyseStarttid, 
 #                                                     RegData$Innleggelsestidspunkt, units='mins'))
#}
if (valgtVar == 'NIHSSinnkomst') {
  RegData <- RegData[which(RegData$NIHSSikkeUtfort == 0), ]
}
if (valgtVar == 'NIHSSpreTrombolyse') {
  RegData <- RegData[which(RegData$Trombolyse %in% c(1,3)), ]
}
if (valgtVar == 'NIHSSetterTrombolyse') {
  RegData <- RegData[which(RegData$Trombolyse %in% c(1,3)), ]
}

if (valgtVar %in% c('Alder', 'AntDagerInnl', 'TidSymptInnlegg','TidSymptTrombolyse', 'TidInnleggTrombolyse',
                    'NIHSSinnkomst', 'NIHSSpreTrombolyse', 'NIHSSetterTrombolyse',
                    'TidSymptInnlegg','TidSymptTrombolyse', 'TidInnleggTrombolyse')) {
  RegData$Variabel <- RegData[ ,valgtVar] }


#Tar ut de med manglende registrering av valgt variabel og gjør utvalg
SlagUtvalg <- SlagUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
                            erMann=erMann, diagnose=diagnose, innl4t=innl4t, NIHSSinn=NIHSSinn)
RegData <- SlagUtvalg$RegData
utvalgTxt <- SlagUtvalg$utvalgTxt

vt <- switch(valgtVar, Alder='alder', 
             AntDagerInnl = 'liggetid',
             NIHSSinnkomst = 'NIHSS ved innkomst',
             NIHSSpreTrombolyse = 'NIHSS før trombolyse',
             NIHSSetterTrombolyse = 'NIHSS etter trombolyse',
             TidInnleggTrombolyse = 'tid fra innleggelse til trombolyse',
             TidSymptInnlegg =  'tid fra symptom til innleggelse',
             TidSymptTrombolyse = 'tid fra symptom til trombolyse')

ytxt1 <- switch(valgtVar, Alder='alder (år)', 
                   AntDagerInnl = 'antall dager',
                   NIHSSinnkomst = 'NIHSSgrad',
                   NIHSSpreTrombolyse = 'NIHSSgrad',
                   NIHSSetterTrombolyse = 'NIHSSgrad',
                   TidInnleggTrombolyse = 'antall minutter',
                   TidSymptInnlegg =  'antall timer',
                   TidSymptTrombolyse = 'antall timer')
            
if (valgtMaal=='Med') {
  t1 <- 'Median ' } else {t1 <- 'Gjennomsnittlig '}

tittel <- paste(t1, vt, sep='') 
	
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(1,2,6)) {	#Involverer egen enhet #Tatt ut 3 (skal ikke ha sykehustype)
		shtxt <- as.character(RegData$Avdeling[indEgen1]) } else {
		shtxt <- switch(as.character(enhetsUtvalg), 	
			'0' = 'Hele landet',
			'7' = as.character(RegData$RHF[indEgen1]),
			'8' = as.character(RegData$RHF[indEgen1]))
			}
			
if (enhetsUtvalg %in% c(0,2,7)) {		#Ikke sammenlikning #Tatt ut 4 (skal ikke ha sykehustype)
			medSml <- 0
			indHoved <- 1:dim(RegData)	#Tidligere redusert datasettet for 2 og 7. 
			indRest <- NULL
		} else {						#Skal gjøre sammenlikning
			medSml <- 1
			if (enhetsUtvalg %in% c(1,6)) {	#Involverer egen enhet #tatt ut 3 - skal ikke ha sykehusgruppe
				indHoved <-which(as.numeric(RegData$ReshId)==reshID) } else {
				indHoved <- which(RegData$RHF == RegData$RHF[indEgen1])}	#region
			smltxt <- switch(as.character(enhetsUtvalg),
				'1' = 'landet forøvrig',
				'6' = paste(RegData$RHF[indEgen1], ' forøvrig', sep=''),	#RegData inneh. kun egen region
				'8' = 'andre regioner')
			indRest <- switch(as.character(enhetsUtvalg),
				'1' = which(as.numeric(RegData$ReshId) != reshID),
				'6' = which(as.numeric(RegData$ReshId)!=reshID),	#RegData inneh. kun egen region
				'8' = which(RegData$RHF != RegData$RHF[indEgen1]))
			}								
			
Tittel <-  c(tittel, shtxt)	#Hva er dette? 



if (length(indHoved)<10 | ((medSml == 1) & (length(indRest) < 10))) {
    #-----------Figur---------------------------------------
figtype(outfile)
	plot.new()
	title(main=Tittel)
	text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
	text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
#	text(0.5, 0.5, tekst,cex=1.5)	#, family="sans")
	if ( outfile != '') {dev.off()}
} else {


Aartxt <- min(RegData$Aar):max(RegData$Aar)
RegData$Aar <- factor(RegData$Aar, levels=Aartxt)
AntAar <- length(Aartxt)


#Resultat for hovedgruppe
N <- tapply(RegData[indHoved ,'Variabel'], RegData[indHoved, 'Aar'], length)
if (valgtMaal=='Med') {
	MedIQR <- plot(RegData$Aar[indHoved],RegData$Variabel[indHoved],  notch=TRUE, plot=FALSE)
	Midt <- as.numeric(MedIQR$stats[3, ])	#as.numeric(MedIQR$stats[3, sortInd])
	Konf <- MedIQR$conf
	#Hvis vil bruke vanlige konf.int:
	#j <- ceiling(N/2 - 1.96*sqrt(N/4))
	#k <- ceiling(N/2 + 1.96*sqrt(N/4))
	#KIHele <- sort(RegData$Variabel)[c(j,k)]
#The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).) 
#They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared, 
#and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give 
#roughly a 95% confidence interval for the difference in two medians. 	
} else {	#Gjennomsnitt blir standard.
	Midt <- tapply(RegData[indHoved ,'Variabel'], RegData[indHoved, 'Aar'], mean)
	SD <- tapply(RegData[indHoved ,'Variabel'], RegData[indHoved, 'Aar'], sd)
	Konf <- rbind(Midt - 2*SD/sqrt(N), Midt + 2*SD/sqrt(N))
}
	#### Noe som ma med i ValgtVardef? Konf <- replace(Konf, which(Konf < KIekstrem[1]), KIekstrem[1])
	#### Konf <- replace(Konf, which(Konf > KIekstrem[2]), KIekstrem[2])

#Resten (gruppa det sammenliknes mot)
MidtRest <- NULL
KonfRest <- NULL
if (medSml ==  1) {
NRest <- tapply(RegData[indRest ,'Variabel'], RegData[indRest, 'Aar'], length)
	if (valgtMaal=='Med') {
		MedIQRrest <- plot(RegData$Aar[indRest],RegData$Variabel[indRest],  notch=TRUE, plot=FALSE)
		MidtRest <- as.numeric(MedIQRrest$stats[3, ])
		KonfRest <- MedIQRrest$conf
	} else {
	MidtRest <- tapply(RegData[indRest,'Variabel'], RegData[indRest, 'Aar'], mean)	#indRest
	SDRest <- tapply(RegData[indRest,'Variabel'], RegData[indRest, 'Aar'], sd)
	NRest <- tapply(RegData[indRest,'Variabel'], RegData[indRest, 'Aar'], length)
	KonfRest <- rbind(MidtRest - 2*SDRest/sqrt(NRest), MidtRest + 2*SDRest/sqrt(NRest))
	}
	### KonfRest <- replace(KonfRest, which(KonfRest < KIekstrem[1]), KIekstrem[1])
	### KonfRest <- replace(KonfRest, which(KonfRest > KIekstrem[2]), KIekstrem[2])
}
#-----------Figur---------------------------------------
xmin <- Aartxt[1]-0.5
xmax <- max(Aartxt)+0.5
cexgr <- 0.9	#Kan endres for enkeltvariable
ymin <- 0.9*min(KonfRest, Konf, na.rm=TRUE)	#ymin1 - 2*h
ymax <- 1.1*max(KonfRest, Konf, na.rm=TRUE)	#ymax1 + 2*h
if (valgtMaal=='Med') {maaltxt <- 'Median ' } else {maaltxt <- 'Gjennomsnittlig '}
ytxt <- paste(maaltxt, ytxt1, sep='')

#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=SlagUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
par('fig'=c(0, 1, 0, 1-0.02*(max((NutvTxt-1),0))))	
	
farger <- FigTypUt$farger
fargeHovedRes <- farger[1]
fargeRestRes <- farger[4]

plot(Aartxt, Midt, xlim= c(xmin, xmax), ylim=c(ymin, ymax), type='n', frame.plot=FALSE, #ylim=c(ymin-0.05*ymax, ymax),
		#cex=0.8, cex.lab=0.9, cex.axis=0.9,	
		ylab=c(ytxt,'med 95% konfidensintervall'), 
		xlab='Innleggelsesår', xaxt='n', 
		sub='(Tall i boksene angir antall innleggelser)', cex.sub=cexgr)	#, axes=F)
axis(side=1, at = Aartxt)	
#Sammenlikning:
if (medSml==1) {
#	polygon( c(Aartxt, Aartxt[AntAar:1]), c(KonfRest[1,], KonfRest[2,AntAar:1]), 
#			col=fargeRestRes, border=NA)
	polygon( c(Aartxt[1]-0.01,Aartxt, Aartxt[AntAar]+0.012, 
				Aartxt[AntAar]+0.012, Aartxt[AntAar:1], Aartxt[1]-0.01), 
		c(KonfRest[1,c(1,1:AntAar, AntAar)], KonfRest[2,c(AntAar,AntAar:1,1)]), 
			col=fargeRestRes, border=NA)
	legend('top', bty='n', fill=fargeRestRes, border=fargeRestRes, cex=cexgr,
		paste('95% konfidensintervall for ', smltxt, ', N=', sum(NRest, na.rm=T), sep=''))
}
h <- strheight(1, cex=cexgr)*0.7	#,  units='figure',
b <- 1.1*strwidth(max(N, na.rm=T), cex=cexgr)/2	#length(Aartxt)/30
rect(Aartxt-b, Midt-h, Aartxt+b, Midt+h, border = fargeHovedRes, lwd=1)	#border=farger[4], col=farger[4]
text(Aartxt, Midt, N, col=fargeHovedRes, cex=cexgr) 	

#Konfidensintervall:
ind <- which(Konf[1, ] > Midt-h) #Konfidensintervall som er tilnærmet 0
options('warn'=-1)
arrows(x0=Aartxt, y0=Midt-h, x1=Aartxt, length=0.08, code=2, angle=90, 
		y1=replace(Konf[1, ], ind, Midt[ind]-h), col=fargeHovedRes, lwd=1.5)
arrows(x0=Aartxt, y0=Midt+h, x1=Aartxt, y1=replace(Konf[2, ], ind, Midt[ind]+h), 
		length=0.08, code=2, angle=90, col=fargeHovedRes, lwd=1.5)
	
title(main=Tittel, font.main=1, line=1)
#Tekst som angir hvilket utvalg som er gjort
if (length(utvalgTxt)>0) {
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))}

if ( outfile != '') {dev.off()}

ResData <- round(rbind(Midt, Konf, MidtRest, KonfRest), 1)
rownames(ResData) <- c('Midt', 'KIned', 'KIopp', 'MidtRest', 'KIRestned', 'KIRestopp')[1:(3*(medSml+1))]
UtData <- list(paste(toString(Tittel),'.', sep=''), ResData )
names(UtData) <- c('Tittel', 'Data')
return(invisible(UtData))

}	#end if statement for 0 observations
}	#end function
