#' Søylediagram som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram som viser andeler (fordeling) av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Aldersfordeling, 10-årige grupper 
#'     \item AntDagerInnl: Liggetid 
#'     \item AvdForstInnlagtHvilken: Hvilken avdeling ble pasienten først innlagt?
#'     \item AvdUtskrFraHvilken: Hvilken avdeling ble pasienten utskrevet fra? 
#'     \item BevissthetsgradInnleggelse: Bevissthetsgrad ved innleggelsen 
#'     \item BildediagnostikkEkstrakranKar: Bildediagnostikk av ekstrakranielle kar 
#'     \item BildediagnostikkHjerne: Bildediagnostikk av hjerneslaget
#'     \item BildediagnostikkHjerte: Bildediagnostikk av hjertet 
#'     \item BildediagnostikkIntraraniell: Bildediagnostikk av intrakranielle kar
#'     \item Boligforhold3mnd: Boligforhold ved oppfølging 
#'     \item BoligforholdPre: Boligforhold ved innleggelse 
#'		\item FokaleUtf: Fokale utfall
#'		\item FokaleUtfAndre: Andre fokale utfall
#'     \item MRS3mnd: Rankinscale ved oppfølging
#'     \item MRSPre: Rankinscale ved innleggelse 
#'		\item NIHSSendrTrombektomi: Endring i NIHSS fra før trombektomi til 24t etter
#'		\item NIHSSendrTrombolyse: Endring i NIHSS fra før trombolyse til 24t etter
#'     \item NIHSSetterTrombektomi: NIHSS 24t etter trombektomi
#'     \item NIHSSetterTrombolyse: NIHSS 24t etter trombolyse
#'     \item NIHSSinnkomst: NIHSS ved innkomst
#'     \item NIHSSpreTrombektomi: NIHSS før trombektomi
#'     \item NIHSSpreTrombolyse: NIHSS før trombolyse
#'     \item RegistreringHjerterytme: Registrering av hjerterytme 
#'     \item Royker3mnd: Røykestatus ved oppfølging 
#'     \item RoykerPre: Røykestatus ved innleggelse 
#'     \item Sivilstatus3mnd: Sivilstatus ved oppfølging
#'     \item SivilstatusPre: Sivilstatus ved innleggelse 
#'     \item Slagdiagnose: Slagdiagnose 
#'     \item TidInnleggTrombolyse: Antall timer fra innleggelse til trombolyse
#'     \item TidSymptInnlegg: Tid fra symptomdebut til innleggelse', '(kun de som ikke våknet med symptom)
#'     \item TidSymptTrombolyse: Tid fra symptomdebut til trombolyse,', '(kun de som ikke våknet med symptom)
#'     \item Tilfredshet: Er du like fornøyd med tilværelsen som før hjerneslaget?
#'     \item Transportmetode: Transport til sykehus etter varsling av AMK 
#'     \item UtskrTil. Hva pasientene ble utskrevet til
#'    }
#'    				
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param valgtVar Hvilken variabel som skal visualiseres. Se \strong{Details} for oversikt.
#' @param datoFra Tidligste dato i utvalget (vises alltid i figuren).
#' @param datoTil Seneste dato i utvalget (vises alltid i figuren).
#' @param erMann Kjønn, standard: alt annet enn 0/1 gir begge kjønn
#'          0: Kvinner
#'          1: Menn
#' @param minald Alder, fra og med (Standardverdi: 0)
#' @param maxald Alder, til og med (Standardverdi: 130)
#' @param outfile Navn på fil figuren skrives til. Standard: '' (Figur skrives
#'    til systemets standard utdataenhet (som regel skjerm))
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param enhetsUtvalg Gjør gruppeutvalg med eller uten sammenlikning for
#'                 0: Hele landet
#'                 1: Egen enhet mot resten av landet (Standard)
#'                 2: Egen enhet
#'				   6: Egen enhet mot egen region 
#'				   7: Egen region 
#'				   8: Egen region mot resten
#' @param preprosess Preprosesser data
#'                 FALSE: Nei
#'                 TRUE: Ja (Standard)
#' @param hentData Gjør spørring mot database
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#' @param diagnose 
#'				1: Infarkt(I61), 
#'				2: Blødning(I63), 
#'				3: definert(I64), 
#'				standard: alle (ikke spesifisert)
#' @param innl4t innlagt innen 4 timer. 
#'				0: nei
#'				1: ja, standard:'' (alt annet)	
#' @param NIHSSinn NIHSS ved innkomst. Registreringer hvor NIHSS ikke er utførtblir tatt bort. 
#'				Kategorier: 0-5,6-10,11-15,...
#'				
#' @return Søylediagram (fordeling) av valgt variabel. De enkelte verdiene kan også sendes med.
#'
#' @export
#'
SlagFigAndeler  <- function(RegData, valgtVar, datoFra='2012-04-01', datoTil='2050-12-31', 
		minald=0, maxald=130, erMann='', diagnose='', innl4t='', NIHSSinn='', outfile='', 
		preprosess=1, hentData=0, reshID, enhetsUtvalg=1)	
{


if (hentData == 1) {		
  RegData <- SlagRegDataSQL(datoFra, datoTil)
}

# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
if (preprosess==1){
       RegData <- SlagPreprosess(RegData=RegData, reshID=reshID)
     }

RegData$Variabel <- 0

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne, 
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(2,6,7)) {	
		RegData <- switch(as.character(enhetsUtvalg),
						'2' = RegData[which(RegData$ReshId == reshID),],	#kun egen enhet
						'6' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),],	#sml region
						'7' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),])	#kun egen region
	}


#if (valgtVar == 'TidSymptInnlegg') {
	#RegData$TidSymptInnlegg <- as.numeric(difftime(RegData$Innleggelsestidspunkt, RegData$Symptomdebut,  
	#		units='hours'))
	#RegData <- RegData[which(RegData$VaaknetMedSymptom==2), ]
#	}
#if (valgtVar == 'TidSymptTrombolyse') {
#	RegData <- RegData[intersect(which(RegData$Trombolyse %in% c(1,3)), 
#		which(RegData$VaaknetMedSymptom==2)), ]
	#RegData$TidSymptTrombolyse <- as.numeric(difftime(RegData$TrombolyseStarttid, RegData$Symptomdebut,  
	#		units='hours'))
#	}
#if (valgtVar == 'TidInnleggTrombolyse') {
#	RegData <- RegData[which(RegData$Trombolyse %in% c(1,3)), ]
	#RegData$TidInnleggTrombolyse <- as.numeric(difftime(RegData$TrombolyseStarttid, 
	#		RegData$Innleggelsestidspunkt, units='hours'))	#units='mins'
#	}
if (valgtVar %in% c('Alder', 'AntDagerInnl', 'TidSymptInnlegg','TidSymptTrombolyse','TidInnleggTrombolyse',
		'NIHSSinnkomst','NIHSSpreTrombolyse','NIHSSetterTrombolyse',
		'NIHSSpreTrombektomi', 'NIHSSetterTrombektomi',
		'BevissthetsgradInnleggelse', 'AvdForstInnlagtHvilken','AvdUtskrFraHvilken', 'UtskrTil',
		'Slagdiagnose', 'BildediagnostikkHjerne','BildediagnostikkHjerte', 'BildediagnostikkIntraraniell',
		'BildediagnostikkEkstrakranKar', 'RegistreringHjerterytme', 'MRSPre', 'MRS3mnd',
		'Transportmetode', 'BoligforholdPre', 'Boligforhold3mnd', 
		'SivilstatusPre', 'Sivilstatus3mnd','RoykerPre', 'Royker3mnd', 'Tilfredshet' )) {
	RegData$Variabel <- RegData[ ,valgtVar] 
	}
	
	
	

#----------- Figurparametre ------------------------------
cexgr <- 1	#Kan endres for enkeltvariable
retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
grtxt <- ''		#Spesifiseres for hver enkelt variabel
grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
# grtxt3 <- ''
subtxt <- ''	#Benevning
flerevar <- 0




#--------------- Variabeldefinisjon ------------------------------
	if (valgtVar=='Alder') {
		tittel <- 'Aldersfordeling'
		gr <- c(0, seq(35, 95, 10), 120)	#c(0,16,31,46,61,76,200)	
		RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
		grtxt <- c('<35','[35,44]','[45,54]','[55,64]','[65,74]','[75,84]','[85,94]','95+')
		#grtxt <- c('<35', levels(RegData$VariabelGr)[2:(length(gr)-2)], '90+')
		subtxt <- 'Aldersgrupper'
	}
	if (valgtVar=='AntDagerInnl') {
		RegData$Variabel <- as.numeric(RegData$Variabel)
		tittel <- 'Liggetid'
		gr <- c(0:15,1000)		#c(0:8,11,15,1000)	#c(18, seq(30, 90, 10), 120)
		RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
		grtxt <- c(gr[1:(length(gr)-2)],'15+')	#c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '20+')	#
		subtxt <- 'Antall dager'
	}

	if (valgtVar == 'TidSymptInnlegg') {
		RegData$Variabel <- as.numeric(RegData$Variabel)
		#De som ikke våknet med symptom
		tittel <- c('Tid fra symptomdebut til innleggelse', '(kun de som ikke våknet med symptom)')
		gr <- c(0,1.5,3,4.5,6,12,24,168,10000)
		RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
		grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-3)], '1-7døgn', '>7døgn')	
		subtxt <- 'Antall timer'
	}
	if (valgtVar == 'TidSymptTrombolyse') {
		RegData$Variabel <- as.numeric(RegData$Variabel)
		#De som ikke våknet med symptom
		tittel <- c('Tid fra symptomdebut til trombolyse,', '(kun de som ikke våknet med symptom)')
		gr <- c(0,1,2,3,4,10000)
		RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
		grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '4+')	
		subtxt <- 'Antall timer'
	}
	if (valgtVar == 'TidInnleggTrombolyse') {
		RegData$Variabel <- as.numeric(RegData$Variabel)
		tittel <- 'Antall timer fra innleggelse til trombolyse'
		#gr <- c(0,0.5,1,1.5,2,2.5,10000)	#*60
		gr <- c(0,30,60,90,120,150,10000)	#*60
		RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
		grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '2.5+')	
		grtxt <- c('[0-0.5)','[0.5-1)', '[1-1.5)', '[1.5-2)', '[2-2.5)' , '2.5+')	
		subtxt <- 'Antall timer'
	}

	if (valgtVar %in% c('NIHSSinnkomst', 'NIHSSpreTrombolyse','NIHSSetterTrombolyse',
				'NIHSSpreTrombektomi', 'NIHSSetterTrombektomi')) {
	#Utvalg på de det faktisk er gjort NIHSS-score på.
	#Utvalg på de som har fått trombolyse/trombektomi:
	#Her vet vi ikke om det er utført NIHSS el om 0 er default
	RegData <- switch(valgtVar,
				NIHSSinnkomst = RegData[which(RegData$NIHSSikkeUtfort==0), ],
				NIHSSpreTrombolyse = RegData[which(RegData$Trombolyse %in% c(1,3)), ],
				NIHSSetterTrombolyse = RegData[which(RegData$Trombolyse %in% c(1,3)), ],
				NIHSSpreTrombektomi = RegData[which(RegData$Trombektomi %in% c(1,3)), ],
				NIHSSetterTrombektomi = RegData[which(RegData$Trombektomi %in% c(1,3)), ],
				)
		tittel <- switch(valgtVar, 
					NIHSSinnkomst = 'NIHSS ved innkomst', 
					NIHSSpreTrombolyse = c('NIHSS før trombolyse', 
						'(fordeling for pasienter som har fått trombolyse)'),
					NIHSSetterTrombolyse = c('NIHSS 24t etter trombolyse', 
						'(fordeling for pasienter som har fått trombolyse)'),
					NIHSSpreTrombektomi = c('NIHSS før trombektomi', 
						'(fordeling for pasienter som har fått trombektomi)'),
					NIHSSetterTrombektomi = c('NIHSS 24t etter trombektomi', 
						'(fordeling for pasienter som har fått trombektomi)'))
		gr <- c(0,6,11,16,21,100)		
		RegData$Variabel <- as.numeric(RegData$Variabel)
		RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
		grtxt <- c('[0-5]','[6-10]','[11-15]','[16-20]','21+')	
		cexgr <- 0.8
		subtxt <- 'Totalscore'
	}
	
if (valgtVar == 'NIHSSendrTrombolyse') {
	RegData <- RegData[which(RegData$Trombolyse %in% c(1,3)), ]
	RegData$NIHSSendrTrombolyse <- RegData$NIHSSetterTrombolyse - RegData$NIHSSpreTrombolyse
	}
if (valgtVar == 'NIHSSendrTrombektomi') {
	RegData <- RegData[which(RegData$Trombektomi %in% c(1,3)), ]
	RegData$NIHSSendrTrombektomi <- RegData$NIHSSetterTrombektomi - RegData$NIHSSpreTrombektomi
	}
	
if (valgtVar %in% c('NIHSSendrTrombolyse','NIHSSendrTrombektomi')) {
	RegData <- switch(valgtVar,
			NIHSSendrTrombolyse = RegData[which(RegData$Trombolyse %in% c(1,3)), ],
			NIHSSendrTrombektomi = RegData[which(RegData$Trombektomi %in% c(1,3)), ])
	RegData$Variabel <- switch(valgtVar,
			NIHSSendrTrombolyse = as.numeric(RegData$NIHSSetterTrombolyse) - as.numeric(RegData$NIHSSpreTrombolyse),
			NIHSSendrTrombektomi = as.numeric(RegData$NIHSSetterTrombektomi) - as.numeric(RegData$NIHSSpreTrombektomi))
	tittel <- switch(valgtVar, 
				NIHSSendrTrombolyse = c('Endring i NIHSS fra før trombolyse til 24t etter', 
					'(fordeling for pasienter som har fått trombolyse)'),
				NIHSSendrTrombektomi = c('Endring i NIHSS fra før trombektomi til 24t etter', 
					'(fordeling for pasienter som har fått trombektomi)'))
	gr <- c(-100,-16,-11,-6,-1,0,5,10,15,100)	
	RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, right=TRUE)	#include.lowest=FALSE, 
	grtxt <- c('-16]', '[-15,-11]', '[-10,-6]', '[-5,-1]', 0, '[1,5]', '[6,10]', 
	'[11,15]', '[16+' )
	subtxt <- 'Endring i totalscore fra før til etter'
}
	if (valgtVar %in% c('MRSPre', 'MRS3mnd')) {
			if (valgtVar == 'MRS3mnd') {
				RegData <- RegData[setdiff(1:dim(RegData)[1], 
					which(RegData$OppfolgUtf ==2 & RegData$MRS3mnd == 0 )), ]
				}
		tittel <- switch(valgtVar, 
					MRSPre = 'Rankinscale før innleggelse', 
					MRS3mnd = 'Rankinscale ved oppfølging')
		#grtxt <- c('Ingen symptomer', 'Ikke sign. hemmet', 'Litt hemmet', 'Moderat hemmet', 
		#	'Moderat/alvorlig hemmet', 'Alvorlig hemmet')	#, 'Død')
		grtxt <- 0:5
		subtxt <- ''
		RegData <- RegData[RegData$Variabel %in% c(0:5), ]
		RegData$VariabelGr <- factor(as.numeric(RegData$Variabel), levels=c(0:5), labels = grtxt) 
#		retn <- 'H'	#Hvis ønsker prosentandeler
	}
	if (valgtVar %in% c('BoligforholdPre', 'Boligforhold3mnd')) {
		tittel <- switch(valgtVar, 
					BoligforholdPre = 'Boligforhold ved innleggelse', 
					Boligforhold3mnd = 'Boligforhold ved oppfølging')
		grtxt <- c('Egen bolig u/hjelp', 'Egen bolig, m/hjelp', 'Omsorgsbolig', 'Sykehjem', 'Ukjent')
		subtxt <- ''
		RegData <- RegData[RegData$Variabel %in% c(1:4,9), ]
		RegData$VariabelGr <- factor(as.numeric(RegData$Variabel), levels=c(1:4,9), labels = grtxt) 
		retn <- 'H'
	}
	if (valgtVar %in% c('RoykerPre', 'Royker3mnd')) {
		tittel <- switch(valgtVar, 
					RoykerPre = 'Røykestatus ved innleggelse', 
					Royker3mnd = 'Røykestatus ved oppfølging')
		grtxt <- c('Aldri', 'Røyker', 'Eks-røyker', 'Ukjent')
		subtxt <- ''
		RegData$VariabelGr <- factor(as.numeric(RegData$Variabel), levels=c(0:2,9), labels = grtxt) 
		retn <- 'H'
	}
	if (valgtVar %in% c('SivilstatusPre', 'Sivilstatus3mnd')) {
		tittel <- switch(valgtVar, 
					SivilstatusPre = 'Sivilstatus ved innleggelse', 
					Sivilstatus3mnd = 'Sivilstatus ved oppfølging')
		grtxt <- c('Gift/samboende', 'Enke/-mann', 'Enslig', 'Ukjent')
		subtxt <- ''
		RegData <- RegData[which(RegData$Variabel %in% c(1:3,9)), ]
		RegData$VariabelGr <- factor(as.numeric(RegData$Variabel), levels=c(1:3,9), labels = grtxt) 
		retn <- 'H'
	}
		if (valgtVar == 'Slagdiagnose') {
		tittel <- 'Slagdiagnose'
		grtxt <- c('Hjerneblødning', 'Hjerneinfarkt', 'Uspesifisert')
		RegData <- RegData[which(RegData$Variabel %in% c(1,2,9)), ]
		RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1,2,9), labels = grtxt)
	}
	if (valgtVar == 'BildediagnostikkHjerne') {
		tittel <- 'Bildediagnostikk av hjerneslaget'
		grtxt <- c('Ingen', 'CT', 'MRI', 'CT+MRI', 'Annen', 'Ukjent')
		RegData <- RegData[which(RegData$Variabel %in% c(1:5,9)), ]
		RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:5,9), labels = grtxt)
		retn <- 'H'
	}

	if (valgtVar == 'BildediagnostikkHjerte') {
		tittel <- 'Bildediagnostikk av hjertet'
		grtxt <- c('Ingen', 'Transthorakal ultralyd', 'Transøsofageal ultralyd', 'MRI', 
				'Kombinasjon av flere', 'Annen', 'Ukjent')
		RegData <- RegData[which(RegData$Variabel %in% c(1:6,9)), ]
		RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:6,9), labels = grtxt)
		retn <- 'H'
	}
	if (valgtVar == 'BildediagnostikkIntraraniell') {
		tittel <- 'Bildediagnostikk av intrakranielle kar'
		grtxt <- c('Ingen', 'Ultralyd', 'CT-angio', 'MR-angio', 'Kombinasjon av flere', 'Ukjent')
		RegData <- RegData[which(RegData$Variabel %in% c(1:5,9)), ]
		RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:5,9), labels = grtxt)
		retn <- 'H'
	}
	if (valgtVar == 'BildediagnostikkEkstrakranKar') {
		tittel <- 'Bildediagnostikk av ekstrakranielle kar'
		grtxt <- c('Ingen', 'Ultralyd', 'CT-angio', 'MR-angio', 'Kombinasjon av flere', 'Ukjent')
		RegData <- RegData[which(RegData$Variabel %in% c(1:5,9)), ]
		RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:5,9), labels = grtxt)
		retn <- 'H'
	}
	if (valgtVar == 'RegistreringHjerterytme') {
		tittel <- 'Registrering av hjerterytme'
		grtxt <- c('Ingen', 'EKG', 'Telemetri/kont.EKG-monit.', 'Holtermonitorering', 'Kombinasjon av flere', 'Ukjent')
		RegData <- RegData[which(RegData$Variabel %in% c(1:5,9)), ]
		RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:5,9), labels = grtxt)
		retn <- 'H'
	}
	if (valgtVar == 'BevissthetsgradInnleggelse') {
		tittel <- 'Bevissthetsgrad ved innleggelsen'
		grtxt <- c('Våken', 'Reagerer v/lett stim.', 'Reagerer v/kraftig stim.', 'Reagerer ikke', 'Ukjent')
		subtxt <- ''
		RegData$VariabelGr <- factor(as.numeric(RegData$Variabel), levels=c(0:3,9), labels = grtxt) 
		retn <- 'H'
	}
	if (valgtVar == 'AvdForstInnlagtHvilken') {
		tittel <- 'Hvilken avdeling ble pasienten først innlagt?'
		grtxt <- c('Slagenhet', 'Medisinsk', 'Nevrologisk', 'Nevrokirurgisk', 'Intensiv', 'Observasjon', 'Annen')
		subtxt <- ''
		RegData$VariabelGr <- factor(as.numeric(RegData$Variabel), levels=c(-1,1:6), labels = grtxt) 
		#Antall 0'er stemmer overens med antall som har svart slagenehet i spm. Avdeling først innlagt?
		retn <- 'H'
	}
	if (valgtVar == 'AvdUtskrFraHvilken') {
		tittel <- 'Hvilken avdeling ble pasienten utskrevet fra?'
		grtxt <- c('Slagenhet', 'Medisinsk', 'Nevrologisk', 'Nevrokirurgisk', 'Intensiv', 'Observasjon', 'Annen')
		RegData$VariabelGr <- factor(as.numeric(RegData$Variabel), levels=c(-1, 1:6), labels = grtxt) 
		#Antall 0'er stemmer overens med antall som har svart slagenehet i spm. Avdeling utskrevet fra? (AvdUtskrFra)
		retn <- 'H'
	}
	if (valgtVar == 'UtskrTil') {
		tittel <- 'Hva pasientene ble utskrevet til'
		grtxt <- c('Hjem u/hjelp', 'Hjem m/hjelp', 'Omsorgsbolig', 'Sykehjem', 'Avd for behandling', 
			'Avd, vente sykehj/rehab', 'Rehabilitering', 'Opptreningssenter', 'Ukjent', 'Død under opphold', 
			'Annen', 'Annet sykehus')
		RegData$VariabelGr <- factor(as.numeric(RegData$Variabel), levels=c(1:12), labels = grtxt) 
		#Antall 0'er stemmer overens med antall som har svart slagenehet i spm. Avdeling utskrevet fra? (AvdUtskrFra)
		retn <- 'H'
	}
	if (valgtVar == 'Tilfredshet') {
		RegData <- RegData[which(RegData$Tilfredshet %in% c(1,2,9)), ]
		tittel <- 'Er du like fornøyd med tilværelsen som før hjerneslaget?'
		grtxt <- c('Ja', 'Nei', 'Vet ikke')
		RegData$VariabelGr <- factor(as.numeric(RegData$Variabel), levels=c(1,2,9), labels = grtxt) 
	}
	if (valgtVar == 'Transportmetode') {
		RegData <- RegData[which(RegData$Transportmetode %in% 1:4), ]
		tittel <- 'Transport til sykehus etter varsling av AMK'
		grtxt <- c('Ambulanse', 'Luftambulanse', 'Kombinasjon', 'Annet')
		RegData$VariabelGr <- factor(as.numeric(RegData$Variabel), levels=c(1:4), labels = grtxt) 
		#retn <- 'H'
	}

 
		
#FIGURER SATT SAMMEN AV FLERE VARIABLE FRA SAMME TOTALUTVALG	
	if (valgtVar == 'FokaleUtf') {
		flerevar <- 1
		retn <- 'H'
		tittel <- 'Fokale utfall'
		subtxt <- 'Andel med svar "ja" av alle totalt'
	#Verdier 1,2,9. 0 kan forekomme.
		variableOrig <- c('Facialisparese', 'Beinparese', 'Armparese', 'SpraakTaleproblem', 
				'AndreFokaleSympt')
		variable <- c('Facialisparese01', 'Beinparese01', 'Armparese01', 'Spraakproblem01', 
				'AndreFokaleSympt01')
		RegData[ ,variable] <- 0
		for (teller in 1:length(variable)) {	#Fysj! STYGG KVIKKFIX...
			RegData[which(RegData[ ,variableOrig[teller]]==1), variable[teller]] <- 1
			}
		grtxt <- c('Facialisparese', 'Beinparese', 'Armparese', 'Språkproblem', 'Andre')
		}
	
	if (valgtVar == 'FokaleUtfAndre'){
		RegData <- RegData[which(RegData$AndreFokaleSympt==1), ]
		flerevar <- 1
		retn <- 'H'
		tittel <- 'Andre fokale utfall'
		subtxt <- 'Andel med svar "ja" av "andre fokale utfall"'
		variable <- c('Dysartri', 'Ataksi', 'Sensibilitetsutfall', 'Neglekt', 'Dobbeltsyn', 'Synsfeltutfall', 'Vertigo')
		grtxt <- c('Dysartri', 'Ataksi', 'Sens.utfall', 'Neglekt', 'Dobbeltsyn', 'Synsfeltutfall', 'Vertigo')
	}


	
if (valgtVar %in% c('Boligforhold3mnd','MRS3mnd', 'Royker3mnd', 'Sivilstatus3mnd')) {
	datoTil <- min(datoTil, as.character(Sys.Date()-90))}
	
#Tar ut de med manglende registrering av valgt variabel og gjør utvalg
SlagUtvalg <- SlagUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
		erMann=erMann, diagnose=diagnose, innl4t=innl4t, NIHSSinn=NIHSSinn)
RegData <- SlagUtvalg$RegData
utvalgTxt <- SlagUtvalg$utvalgTxt


	
	
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(1,2,6)) {	#Involverer egen enhet
		shtxt <- as.character(RegData$Avdeling[indEgen1]) } else {
		shtxt <- switch(as.character(enhetsUtvalg), 	
			'0' = 'Hele landet',
			'7' = as.character(RegData$Region[indEgen1]),
			'8' = as.character(RegData$Region[indEgen1]))
			}
			
if (enhetsUtvalg %in% c(0,2,7)) {		#Ikke sammenlikning
			medSml <- 0
			indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,7. (+ 6)
			indRest <- NULL
		} else {						#Skal gjøre sammenlikning
			medSml <- 1
			if (enhetsUtvalg %in% c(1,6)) {	#Involverer egen enhet
				indHoved <-which(as.numeric(RegData$ReshId)==reshID) } else {
				indHoved <- switch(as.character(enhetsUtvalg),
						'8' = which(RegData$Region == RegData$Region[indEgen1]))}	#region
			smltxt <- switch(as.character(enhetsUtvalg),
				'1' = 'landet forøvrig',
				'6' = paste(RegData$Region[indEgen1], ' forøvrig', sep=''),	#RegData inneh. kun egen region
				'8' = 'andre regioner')
			indRest <- switch(as.character(enhetsUtvalg),
				'1' = which(as.numeric(RegData$ReshId) != reshID),
				'6' = which(as.numeric(RegData$ReshId)!=reshID),	#RegData inneh. kun egen region
				'8' = which(RegData$Region != RegData$Region[indEgen1]))
			}								
			
#--------------- Gjøre beregninger ------------------------------
#Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
Andeler <- list(Hoved = 0, Rest =0)
NRest <- 0
AntRest <- 0
AntHoved <- switch(as.character(flerevar), 
				'0' = table(RegData$VariabelGr[indHoved]),
				'1' = colSums(sapply(RegData[indHoved ,variable], as.numeric), na.rm=T))
NHoved <- switch(as.character(flerevar), 
				'0' = sum(AntHoved),	#length(indHoved)- Kan inneholde NA
				'1' = length(indHoved))
Andeler$Hoved <- 100*AntHoved/NHoved

if (medSml==1) {
	AntRest <- switch(as.character(flerevar), 
					'0' = table(RegData$VariabelGr[indRest]),
					'1' = colSums(sapply(RegData[indRest ,variable], as.numeric), na.rm=T))
	NRest <- switch(as.character(flerevar), 
					'0' = sum(AntRest),	#length(indRest)- Kan inneholde NA
					'1' = length(indRest))
	Andeler$Rest <- 100*AntRest/NRest
}





#-----------Figur---------------------------------------
#Hvis for få observasjoner..
if (NHoved < 10 | (medSml ==1 & NRest<10)) {
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
	text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
	if ( outfile != '') {dev.off()}
} else {

#-----------Figur---------------------------------------
#Innparametre: subtxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr


#Plottspesifikke parametre:
FigTypUt <- figtype(outfile) 
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f',Andeler$Hoved)), '%)', sep='')
vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
#vmarg <- max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7)
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

if (grtxt2 == '') {grtxt2 <- paste(sprintf('%.1f',Andeler$Hoved), '%', sep='')}
# if (grtxt3 == '') {grtxt3 <- paste(sprintf('%.0f',Andeler$Hoved*NHoved/100), '', sep='')}

farger <- FigTypUt$farger
fargeSh <- farger[1]
fargeRest <- farger[3]
antGr <- length(grtxt)
lwdRest <- 3	#tykkelse på linja som repr. landet
cexleg <- 1	#Størrelse på legendtekst

#Horisontale søyler
if (retn == 'H') {
	xmax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
	pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel, 
		col=fargeSh, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#  
	mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)

	if (medSml == 1) {
		points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
		legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')), 
			border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, 
			lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
		} else {	
		legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''), 
			border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
		}
}

if (retn == 'V' ) {
#Vertikale søyler eller linje
	ymax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
	pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",	
		sub=subtxt,	col=fargeSh, border='white', ylim=c(0, ymax))	
	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
	mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
# 	mtext(at=pos, grtxt3, side=1, las=1, cex=cexgr, adj=0.5, line=2.5)
if (medSml == 1) {
	points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
	legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')), 
		border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
		lwd=lwdRest, ncol=2, cex=cexleg)
	} else {	
	legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''), 
		border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
	}
} 


title(tittel, line=1, font.main=1)

#Tekst som angir hvilket utvalg som er gjort
avst <- 0.8
utvpos <- 3	#Startlinje for teksten
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1)) 
if ( outfile != '') {dev.off()}

}
}
