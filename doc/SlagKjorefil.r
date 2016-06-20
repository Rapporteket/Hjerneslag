
save(SlagDataTest, file='SlagDataTest.Rdata')
load('C:/Registre/Hjerneslag/data/SlagDataTest.Rdata')


rm(list=ls())
load(file='C:/Registre/Hjerneslag/data/HjerneSlag2okt2013.Rdata')
SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlagPROD2013-12-11.csv', 
		sep=';', header=T) #, encoding='UTF-8')
save(SlagData, file='C:/Registre/Hjerneslag/data/HjerneSlag2013-12-11.Rdata')
write.table(SlagData, file='SlagDataTest.csv', row.names=FALSE, sep=';')

table(table(SlagData$PatientInRegistryKey))
#Flere pasienter som har mer enn tre registreringer

table(table(SlagData$PatientInRegistryKey[which(SlagData$SkjemaID==1)]))


#--------------------------------------SAMLERAPPORT-----------------------------------

rm(list=ls())
#Sys.setenv("MYSQL_HOME"="C:/Program Files/MySQL/mysql-cluster")
#knit(input, output = NULL, tangle = FALSE, text = NULL, envir = parent.frame())

SlagDataALLE <- read.table('C:/Registre/Hjerneslag/data/HjerneSlagPROD2016-02-15.csv', sep=';', header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 
#names(SlagData[which(names(SlagData) == 'PreMedHoytBT')]) <- 'PreMedikBehHoytBT'
SlagData <- SlagDataALLE[sample(1:dim(SlagDataALLE)[1], 5000), ]
#SlagData <- read.table('C:/Registre/Hjerneslag/data/SlagEksempel.csv', sep=';', header=T) #, 
#Brukes kun for å få med alle potensielle sykehus slik at de som evt. har 0 registreringer også blir med.
SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlagPROD2016-02-15.csv', sep=';', header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 
reshID <- 106340 #StOlav: 106340, Harstad sykehus: 700741, Narvik sykehus: 700742, Tromsø sykehus: 601159


setwd('C:/ResultattjenesteGIT/Hjerneslag/inst')
library(knitr)
knit('SlagSamleDokLand.Rnw')
knit('SlagSamleDok.Rnw')
#knit('SlagSamleDok_AlleTabOgKomm.Rnw')

#--------------------------------------------------------


#--------------------------------------AntStabel-----------------------------------

rm(list=ls())
#load(file='C:/Registre/Hjerneslag/data/HjerneSlag2013-12-11.Rdata')
SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlagPROD2016-02-15.csv', sep=';', header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 
RegData <- SlagData
# Inndata til funksjon:

#...NB: SkjemaID
reshID <- 106340 #De tre med flest reg: 601159 (Tromsø)  700264 (Kristiansand)  106340 (St. Olavs)	#Må sendes med til funksjon
datoTil <- '2016-03-03'
enhetsUtvalg <-7 #0-hele landet, 2-egen enhet, 7–egen region, 
outfile <- ''	#paste('Registreringer',enhetsUtvalg, '.pdf', sep='')	#Navn angis av Jasper

FigAntReg(RegData=SlagData,
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
		NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)





#------------------------------ Andeler flere var --------------------------
#------------------------------ (erstatter Fordelinger) --------------------------
rm(list=ls())
#load(file='C:/Registre/Hjerneslag/data/HjerneSlag2okt2013.Rdata')
#SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlag2014-10-21ansi.csv', sep=';', header=T) #, 
SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlagPROD2016-06-20.csv', sep=';', header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 

# Inndata til funksjon:
reshID <- 106340 #De tre med flest reg: 601159 (Tromsø)  700264 (Kristiansand)  106340 (St. Olavs)	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2013-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-08-01'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
diagnose <- ''	#diagnose: 1,2,3  Infarkt(I61), Blødning(I63), Udefinert(I64), standard: '' (alt annet)
innl4t <- '' 	#Innlagt innen 4t: 'Ja', 'Nei', standard:'' (alt annet)
enhetsUtvalg <-1 #0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
				#6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
NIHSSinn <- ''	#NIHSS grupper: 1-6, tilsv. verdi: 0-5,6-10,11-15,..., standard: '' (alt annet)
				#Velges denne, blir registreringer hvor NIHSS ikke er utført, tatt bort.
valgtVar <- 'Alder'	#Må velge... Alder, Transportmetode,
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
outfile <- paste(valgtVar, '_ford.pdf', sep='')	#Navn angis av Jasper
setwd("C:/ResultattjenesteGIT/Hjerneslag")

FigAndeler(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
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
outfile <- 'KvalInd.png'	#'KvalInd.pdf'	#Navn angis av Jasper

SlagFigAndelerKvalIndTest(RegData=SlagData, datoFra=datoFra, datoTil=datoTil, erMann=erMann, NIHSSinn=NIHSSinn, 
			reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

#------------------------------ Sammenligning av resultat før og etter [Pre-Post] --------------------------
rm(list=ls())
SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlagPROD2015-05-20.csv', sep=';', header=T) #, 
RegData <- SlagData
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
rm(list=ls())

# Inndata til funksjon:
reshID <- 106340 #De tre med flest reg: 601159 (Tromsø)  700264 (Kristiansand)  106340 (St. Olavs)	#Må sendes med til funksjon
valgtVar <- 'TidInnTrombolyse40min'	#Må velge... 
    # BehSlagenhet, InnlSlagenh, InnlInnen4eSymptom, LipidI63, OppfolgUtf (ta ut siste 3 mnd)
    # SvelgtestUtfort, TidInnTrombolyse40min, TrombolyseI63, UtAntitrombotiskI63, UtAntikoagI63atrie
 	# UtBT(?)
	
outfile <- ''	#paste(valgtVar, '.pdf', sep='')	#Navn angis av Jasper

FigAndelTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
		NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)


#------------------------------ GjsnTid --------------------------
rm(list=ls())
#SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlag2014-10-21ansi.csv', sep=';', header=T) #, 
SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlagPROD2015-05-20.csv', sep=';', header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 
RegData <- SlagData
# Inndata til funksjon:
valgtMaal=''	#'Med' - median, alt annet gir gjennomsnitt
enhetsUtvalg <- 0 	#0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten

valgtVar <- 'AntDagerInnl'	#Alder, AntDagerInnl, TidSymptInnlegg, TidSymptTrombolyse,
               #TidInnleggTrombolyse, NIHSSinnkomst, NIHSSpreTrombolyse, NIHSSetterTrombolyse
outfile <- ''	#paste(valgtVar, '.pdf', sep='')	#Navn angis av Jasper

FigGjsnTid(RegData=SlagData, datoFra=datoFra, datoTil=datoTil, valgtVar=valgtVar,
		valgtMaal=valgtMaal, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
		NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)


#---------------------------Andeler for ulike grupper (sykehus)-----------------------------
rm(list=ls())
#load(file='C:/Registre/Hjerneslag/data/HjerneSlag2okt2013.Rdata')
#SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlag2014-10-21ansi.csv', sep=';', header=T) #, 
SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlagPROD2015-05-20.csv', sep=';', header=T) #, 
RegData <- SlagData

# Inndata til funksjon:
enhetsUtvalg <- 0	#0:Alle, 7:Egen region
valgtVar <- 'InnlSlagenh'	#Må velge... BehSlagenhet, InnlSlagenh, InnlInnen4eSymptom, 
			#LipidI63u80, SvelgtestUtfort, TidInnTrombolyse30min, TrombolyseI63, UtAntitrombotiskI63, 
			#UtAntikoagI63atrie, UtBT
outfile <- paste(valgtVar, '.pdf', sep='')	#Navn angis av Jasper

FigAndelerGrVar(RegData=SlagData, valgtVar=valgtVar, datoFra=datoFra, 
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, 
		innl4t=innl4t, NIHSSinn=NIHSSinn, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

for (valgtVar in c('InnlSlagenh', 'BehSlagenhet', 'InnlInnen4eSymptom', 'SvelgtestUtfort', 
		'TrombolyseI63', 'UtAntitrombotiskI63', 'UtAntikoagI63atrie', 'LipidI63u80', 'UtBT')) {
	outfile <- paste(valgtVar, '_GrVar.png', sep='')
FigAndelerGrVar(RegData=SlagData, valgtVar=valgtVar, datoFra=datoFra, 
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, 
		innl4t=innl4t, NIHSSinn=NIHSSinn, outfile=outfile)
}

#------------------------------ MeanMed --------------------------
rm(list=ls())
#load(file='C:/Registre/Hjerneslag/data/HjerneSlag2okt2013.Rdata')
SlagData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlag2014-10-21.csv', sep=';', header=T) #, 
RegData <- SlagData
# Inndata til funksjon:
enhetsUtvalg <- 0	#0:Alle, 7:Egen region
valgtMaal <- 'Med'	# valgtMaal - 'Med' = median. Alt annet gir gjennomsnitt
valgtVar <- 'TidSymptInnlegg'	#Må velge... Alder, 
		#AntDagerInnl, TidSymptInnlegg, TidSymptTrombolyse, TidInnleggTrombolyse
		#NIHSSinnkomst, 
		#NIHSSpreTrombolyse','NIHSSetterTrombolyse','NIHSSpreTrombektomi', 'NIHSSetterTrombektomi'
outfile <- ''	#paste(valgtVar, '.png', sep='')	#Navn angis av Jasper

FigMeanMed(RegData=SlagData, valgtVar=valgtVar, valgtMaal=valgtMaal, datoFra=datoFra, datoTil=datoTil, 
		minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, NIHSSinn=NIHSSinn, 
		enhetsUtvalg=enhetsUtvalg, reshID=reshID,outfile=outfile)
		

for (valgtVar in c('Alder','AntDagerInnl', 'TidSymptInnlegg', 'TidSymptTrombolyse', 'TidInnleggTrombolyse',
		'NIHSSinnkomst','NIHSSpreTrombolyse','NIHSSetterTrombolyse',
		'NIHSSpreTrombektomi', 'NIHSSetterTrombektomi')) {
	outfile <- paste(valgtVar, 'MM.png', sep='')
	FigMeanMed(RegData=SlagData, valgtVar=valgtVar, valgtMaal=valgtMaal, datoFra=datoFra, 
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


#-------------------------- Plassering av utvalgstekst -----------------------------------
#Plassering av utvalgstekst, størrelse av teksten og avstand mellom linjene.
outfile <- '' #'test6.pdf'
source("C:/Registre/Hjerneslag/trunk/RAndeler/SlagFigAndeler.R", encoding="UTF-8")
for (type in c('.pdf','.png')) {
FigAndeler(RegData=SlagData, datoFra=datoFra, valgtVar='BoligforholdPre',
		datoTil=datoTil, minald=minald, maxald=, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
		NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, libkat=libkat, 
		outfile=paste0('utvBolig', type))
		}


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

FigMeanMed(RegData=SlagData, valgtVar='AntDagerInnl', valgtMaal='', datoFra=datoFra, 
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, 
		innl4t=innl4t, NIHSSinn=NIHSSinn, libkat=libkat, 
		outfile=paste0('AntDagerInnlMM', test, type))

FigAndelerGrVar(RegData=SlagData, valgtVar='InnlSlagenh', datoFra=datoFra, 
		datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, 
		innl4t=innl4t, NIHSSinn=NIHSSinn, libkat=libkat, 
		outfile=paste0('InnlSlagenh', test, type))

}

