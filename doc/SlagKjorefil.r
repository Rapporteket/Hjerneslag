#---------------Hente Data--------------------------------------

rm(list=ls())
#load(file='C:/Registre/Hjerneslag/data/HjerneSlag2okt2013.Rdata')
#SlagData <- SlagDataALLE[sample(1:dim(SlagDataALLE)[1], 5000), ]
#save(SlagData, file='C:/Registre/Hjerneslag/data/HjerneSlagTest.Rdata')
#SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlagPROD2016-09-19.csv', sep=';', header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 


HovedSkjema <- read.table('A:/Hjerneslag/Akuttskjema2017-11-16.csv', sep=';',  #2017-01-24
                          header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 
OppfSkjema <- read.table('A:/Hjerneslag/AkuttskjemaOppfolging2017-11-16.csv', sep=';', 
                         header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 


#Bør lage automatisk sjekk for hvilke variabelnavn som finnes i begge datasett
#varBegge <- c('PatientAge', 'PatientGender', 'DeathDate')
#OppfSkjema <- OppfSkjema[ ,-which(names(OppfSkjema) %in% varBegge)]
#OppfSkjema$HovedskjemaGUID <- toupper(OppfSkjema$HovedskjemaGUID)
#names(OppfSkjema)[which(names(OppfSkjema)== 'SkjemaGUID')] <- 'OSkjemaGUID'

OppfSkjema$HovedskjemaGUID <- toupper(OppfSkjema$HovedskjemaGUID)


varBegge <- intersect(names(OppfSkjema),names(HovedSkjema))
OppfSkjema <- OppfSkjema[ ,c("HovedskjemaGUID", "SkjemaGUID", names(OppfSkjema)[!(names(OppfSkjema) %in% varBegge)])]
SlagData <- merge(HovedSkjema, OppfSkjema, by.x='SkjemaGUID',by.y="HovedskjemaGUID", all.x = TRUE, all.y = FALSE)
save(RegData, file='A:/Hjerneslag/RegData2017-11-16.Rdata')
load('A:/Hjerneslag/RegData2017-11-16.Rdata')     
SlagData <- RegData


#.---------------------- Sette parametre-----------------------------------

# Inndata til funksjon:
reshID <- 106340 #De tre med flest reg: 601159 (Tromsø)  700264 (Kristiansand)  106340 (St. Olavs)	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2013-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2017-12-31'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
diagnose <- ''	#diagnose: 1,2,3  Infarkt(I61), Blødning(I63), Udefinert(I64), standard: '' (alt annet)
innl4t <- '' 	#Innlagt innen 4t: 'Ja', 'Nei', standard:'' (alt annet)
NIHSSinn <- ''	#NIHSS grupper: 1-6, tilsv. verdi: 0-5,6-10,11-15,..., standard: '' (alt annet)
valgtMaal=''	#'Med' - median, alt annet gir gjennomsnitt
enhetsUtvalg <- 1 	#0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten


#--------------------------------------SAMLERAPPORT-----------------------------------
#Last data
library(tools)
library(Hjerneslag)
library(knitr)
setwd('C:/ResultattjenesteGIT/Hjerneslag/inst')
reshID <- 106340 #StOlav: 106340, Harstad sykehus: 700741, Narvik sykehus: 700742, Tromsø sykehus: 601159
load('A:/Hjerneslag/RegData2017-11-16.Rdata')     
SlagData <- RegData
knit('SlagSamleDok.Rnw')
knit('SlagSamleDokLand.Rnw')

tools::texi2pdf('SlagSamleDokTest.tex')
tools::texi2pdf('SlagSamleDokLand.tex')

#--------------------------------------AntStabel-----------------------------------

# Inndata til funksjon:

#...NB: SkjemaID
reshID <- 106340 #StOlav: 106340, Harstad sykehus: 700741, Narvik sykehus: 700742, Tromsø sykehus: 601159
datoTil <- '2016-03-03'
enhetsUtvalg <-0 #0-hele landet, 2-egen enhet, 7–egen region, 
outfile <- paste0('Registreringer',enhetsUtvalg, 'NY.pdf')	#Navn angis av Jasper

SlagFigAntReg(RegData=RegData,
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
		NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)



#------------------------------ Andeler flere var --------------------------
#------------------------------ (erstatter Fordelinger) --------------------------

valgtVar <- 'FokaleUtfAndre'	#Må velge... Alder, Transportmetode,
		#AntDagerInnl, TidSymptInnlegg, TidSymptTrombolyse, TidInnleggTrombolyse
		#NIHSSinnkomst, NIHSSendrTrombolyse, NIHSSendrTrombektomi
		#NIHSSpreTrombolyse','NIHSSetterTrombolyse','NIHSSpreTrombektomi', 'NIHSSetterTrombektomi'
		#Slagdiagnose, BildediagnostikkHjerne, BildediagnostikkHjerte,
		#BildediagnostikkIntraraniell, BildediagnostikkEkstrakranKar, RegistreringHjerterytme
		#BoligforholdPre, RoykerPre, Royker3mnd, 'MRSPre', 'MRS3mnd', Tilfredshet,
		#SivilstatusPre, BevissthetsgradInnleggelse, 
		#AvdForstInnlagtHvilken, AvdUtskrFraHvilken, UtskrTil
		#FokaleUtf, FokaleUtfAndre,
		#Tatt ut til egen: 'KvalInd', 
outfile <- paste0(valgtVar, '_ford.pdf')	#Navn angis av Jasper
setwd("C:/ResultattjenesteGIT/Hjerneslag")

SlagFigAndeler(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
		NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

for (valgtVar in c('Alder', 'AntDagerInnl', 'AvdForstInnlagtHvilken', 'AvdUtskrFraHvilken', 
		'BevissthetsgradInnleggelse', 'BildediagnostikkHjerne', 'BildediagnostikkHjerte',
		'BildediagnostikkIntraraniell', 'BildediagnostikkEkstrakranKar', 'BoligforholdPre', 
		'FokaleUtf', 'FokaleUtfAndre', 'MRSPre', 'MRS3mnd','NIHSSinnkomst', 'NIHSSendrTrombolyse', 
		'NIHSSendrTrombektomi',
		'NIHSSpreTrombolyse','NIHSSetterTrombolyse','NIHSSpreTrombektomi', 'NIHSSetterTrombektomi',
		'RegistreringHjerterytme', 'RoykerPre', 'Royker3mnd', 'SivilstatusPre', 'Slagdiagnose',  
		 'TidSymptInnlegg', 'TidSymptTrombolyse', 'TidInnleggTrombolyse',
		 'Transportmetode', 'UtskrTil'
		)) {
	outfile <- paste(valgtVar, '.png', sep='')
	FigAndeler(RegData=SlagData, valgtVar=valgtVar, datoFra=datoFra, 
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, 
		innl4t=innl4t, NIHSSinn=NIHSSinn, reshID=reshID, outfile=outfile)
}

#---------------------------------------- KVALITETSINDIKATORER
outfile <- 'KvalInd.png' #KvalInd.pdf'	#Navn angis av Jasper

SlagFigAndelerKvalInd(RegData=RegData, datoFra=datoFra, datoTil=datoTil, erMann=erMann, NIHSSinn=NIHSSinn, 
			reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

#------------------------------ Sammenligning av resultat før og etter [Pre-Post] --------------------------
# Inndata til funksjon:
valgtVar <- 'Boligforh'	#Må velge... 'Boligforh', 'Bosituasjon', 'Toalett', 'Forflytning', 
			#'Paakledning',  'Bilkjoring','MRS', 'Yrkesaktiv', 'NIHSSTrombolyse', 'NIHSSTrombektomi'
			#	På vent: 'Roykestatus',
outfile <- ''	#paste(valgtVar, '.png', sep='')	#Navn angis av Jasper

FigPrePost(RegData=SlagData, valgtVar=valgtVar, datoFra=datoFra, 
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
		NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, libkat=libkat, outfile=outfile)


for (valgtVar in c('Boligforh', 'Bosituasjon', 'Toalett', 'Forflytning', 'Paakledning', 'Roykestatus', 
			'Bilkjoring', 'Yrkesaktiv', 'MRS', 'NIHSSTrombolyse', 'NIHSSTrombektomi')) {
	outfile <- paste(valgtVar, 'PP.png', sep='')
	FigPrePost(RegData=SlagData, valgtVar=valgtVar, datoFra=datoFra, 
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
		NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=1, outfile=outfile)
}

#------------------------------ AndelTid --------------------------

valgtVar <- 'InnlInnen4eSymptom'	#Må velge... 
    # BehSlagenhet, InnlSlagenh, InnlInnen4eSymptom, LipidI63, OppfolgUtf (ta ut siste 3 mnd)
    # SvelgtestUtfort, TidInnTrombolyse40min, TrombolyseI63, UtAntitrombotiskI63, UtAntikoagI63atrie
 	# UtBT(?)
	
outfile <- ''	#paste(valgtVar, '.pdf', sep='')	#Navn angis av Jasper

SlagFigAndelTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
		NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)


#------------------------------ GjsnTid --------------------------

valgtVar <- 'TidSymptInnlegg'	#Alder, AntDagerInnl, TidSymptInnlegg, TidSymptTrombolyse,
               #TidInnleggTrombolyse, NIHSSinnkomst, NIHSSpreTrombolyse, NIHSSetterTrombolyse
outfile <- paste(valgtVar, '.pdf', sep='')	#Navn angis av Jasper
enhetsUtvalg <- 0
SlagFigGjsnTid(RegData=SlagData, datoFra=datoFra, datoTil=datoTil, valgtVar=valgtVar,
		valgtMaal=valgtMaal, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
		NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)


#---------------------------Andeler for ulike grupper (sykehus)-----------------------------
valgtVar <- 'TrombolyseI63'
outfile <- paste0(valgtVar, '_sh.png')
SlagFigAndelerGrVar(RegData=SlagData, valgtVar=valgtVar, datoFra=datoFra, 
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, 
		innl4t=innl4t, NIHSSinn=NIHSSinn, enhetsUtvalg=enhetsUtvalg,outfile=outfile)

for (valgtVar in c('InnlSlagenh', 'BehSlagenhet', 'InnlInnen4eSymptom', 'SvelgtestUtfort', 
		'TrombolyseI63', 'UtAntitrombotiskI63', 'UtAntikoagI63atrie', 'LipidI63u80', 'UtBT')) {
	outfile <- paste(valgtVar, '_GrVar.png', sep='')
FigAndelerGrVar(RegData=SlagData, valgtVar=valgtVar, datoFra=datoFra, 
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, 
		innl4t=innl4t, NIHSSinn=NIHSSinn, outfile=outfile)
}

#------------------------------ MeanMed --------------------------
valgtVar <- 'TidSymptInnlegg'	#Må velge... Alder, 
		#AntDagerInnl, TidSymptInnlegg, TidSymptTrombolyse, TidInnleggTrombolyse
		#NIHSSinnkomst, 
		#NIHSSpreTrombolyse','NIHSSetterTrombolyse','NIHSSpreTrombektomi', 'NIHSSetterTrombektomi'
outfile <- ''	#paste(valgtVar, '.png', sep='')	#Navn angis av Jasper

SlagFigGjsnGrVar(RegData=SlagData, valgtVar=valgtVar, valgtMaal=valgtMaal, datoFra=datoFra, datoTil=datoTil, 
		minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, NIHSSinn=NIHSSinn, 
		enhetsUtvalg=enhetsUtvalg, reshID=reshID,outfile=outfile)
		

for (valgtVar in c('Alder','AntDagerInnl', 'TidSymptInnlegg', 'TidSymptTrombolyse', 'TidInnleggTrombolyse',
		'NIHSSinnkomst','NIHSSpreTrombolyse','NIHSSetterTrombolyse',
		'NIHSSpreTrombektomi', 'NIHSSetterTrombektomi')) {
	outfile <- paste(valgtVar, 'MM.png', sep='')
	SlagFigGjsnGrVar(RegData=SlagData, valgtVar=valgtVar, valgtMaal=valgtMaal, datoFra=datoFra, 
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, 
		innl4t=innl4t, NIHSSinn=NIHSSinn, outfile=outfile)
}


#----------------------------------------- STANDARDISERE FIGURER---------------------------------------
rm(list=ls())
#load(file='C:/Registre/Hjerneslag/data/HjerneSlag2okt2013.Rdata')
SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlag2014-10-21ansi.csv', sep=';', header=T) #, 
RegData <- SlagData

valgtVar <- 'AntDagerInnl'	#Må velge... Alder, Transportmetode,
		#AntDagerInnl, TidSymptInnlegg, TidSymptTrombolyse, TidInnleggTrombolyse
		#BoligforholdPre, RoykerPre, Royker3mnd, 'MRSPre', 'MRS3mnd', Tilfredshet,
		#SivilstatusPre, BevissthetsgradInnleggelse, 
outfile <- paste(valgtVar, '.png', sep='')	#Navn angis av Jasper


#------------------------------ Oppløsning ---------------------------------------------------
#1 - Standard: res=72, bredde=555, høyde=555 el 800, pointsize=12
#2 - res=2*72, bredde=2*555 (pdf:7), høyde=2*555 el 800 (pdf: høyde/bredde, , pointsize=12 
#3 - res=3*72, bredde=3*555 (pdf:7), høyde=3*555 el 800 (pdf: høyde/bredde, , pointsize=12 

for (type in c('.pdf','.png')) {

FigAndelerKvalInd(RegData=SlagData, datoFra=datoFra, datoTil=datoTil, erMann=erMann, NIHSSinn=NIHSSinn, 
		reshID=reshID, enhetsUtvalg=enhetsUtvalg,libkat=libkat, 
		outfile=paste0('KvalInd', test, type))
}
FigAndeler(RegData=SlagData, datoFra=datoFra, valgtVar='BoligforholdPre',
		datoTil=datoTil, minald=minald, maxald=, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
		NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg,libkat=libkat, 
		outfile=paste0('BoligforholdPre', test, type))


FigAndeler(RegData=SlagData, datoFra=datoFra, valgtVar='AntDagerInnl',
		datoTil=datoTil, minald=minald, maxald=, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
		NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg,libkat=libkat, 
		outfile=paste0('AntDagerInnl', test, type))

SlagFigGjsnGrVar(RegData=SlagData, valgtVar='AntDagerInnl', valgtMaal='', datoFra=datoFra, 
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, 
		innl4t=innl4t, NIHSSinn=NIHSSinn, libkat=libkat, 
		outfile=paste0('AntDagerInnlMM', test, type))

FigAndelerGrVar(RegData=SlagData, valgtVar='InnlSlagenh', datoFra=datoFra, 
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, 
		innl4t=innl4t, NIHSSinn=NIHSSinn, libkat=libkat, 
		outfile=paste0('InnlSlagenh', test, type))

}

