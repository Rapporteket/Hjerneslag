

rm(list=ls())
RegData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlag2015.csv', sep=';', header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 
#RegData <- read.table('C:/Registre/Hjerneslag/data/HjerneSlagPROD2016-09-19.csv', sep=';', header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 


# Inndata til funksjon:
reshID <- 106340 #De tre med flest reg: 601159 (Tromsø)  700264 (Kristiansand)  106340 (St. Olavs)	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2015-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2015-12-31'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
diagnose <- ''	#diagnose: 1,2,3  Infarkt(I61), Blødning(I63), Udefinert(I64), standard: '' (alt annet)
innl4t <- '' 	#Innlagt innen 4t: 'Ja', 'Nei', standard:'' (alt annet)
enhetsUtvalg <- 0 #0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
NIHSSinn <- ''	#NIHSS grupper: 1-6, tilsv. verdi: 0-5,6-10,11-15,..., standard: '' (alt annet)



#------------Variabel: TidInnleggTrombolyse-------------------------
#OK? Gått gjennom alle.
outfile <- 'KvalInd.png'	#'KvalInd.pdf'	#Navn angis av Jasper
SlagFigAndelerKvalInd(RegData=RegData, datoFra=datoFra, datoTil=datoTil, erMann=erMann, NIHSSinn=NIHSSinn, 
                      reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

valgtVar <- 'TidInnTrombolyse40min'
outfile <- paste(valgtVar, 'ATid.pdf', sep='')	#Navn angis av Jasper
SlagFigAndelTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
                NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

valgtVar <- 'TidInnTrombolyse40min'
outfile <- paste(valgtVar, 'AGrVar.pdf', sep='')	#Navn angis av Jasper
SlagFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, 
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, 
                innl4t=innl4t, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

valgtVar <- 'TidInnleggTrombolyse'
outfile <- paste(valgtVar, 'Ford.pdf', sep='')	#Navn angis av Jasper
SlagFigAndeler(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
               datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
               NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

valgtVar <- 'TidInnleggTrombolyse'
outfile <- paste(valgtVar, 'GjsnTid.pdf', sep='')	#Navn angis av Jasper
SlagFigGjsnTid(RegData=RegData, datoFra=datoFra, datoTil=datoTil, valgtVar=valgtVar,
           minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
           NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)



#------------Variabel: TidSymptInnlegg-------------------------

valgtVar <- 'InnlInnen4eSymptom'
outfile <- paste(valgtVar, 'ATid.pdf', sep='')	#Navn angis av Jasper
SlagFigAndelTid(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
                NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

valgtVar <- 'InnlInnen4eSymptom'
outfile <- paste(valgtVar, 'AGrVar.pdf', sep='')	#Navn angis av Jasper
SlagFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, 
                    datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, 
                    innl4t=innl4t, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

valgtVar <- 'TidSymptInnlegg'
outfile <- paste(valgtVar, 'Ford.pdf', sep='')	#Navn angis av Jasper
SlagFigAndeler(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
               datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, 
               NIHSSinn=NIHSSinn, reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile)

#------------Variabel: TidSymptTrombolyse-------------------------
#Bare resultater med gjennomsnitt. Ok.
