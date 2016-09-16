#' Tidstrend av andel pasienter
#'
#' Denne funksjonen lager et søylediagram som viser andeler (fordeling) av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#'
#' @inheritParams SlagFigAndeler 
#' @param valgtVar Hvilken variabel som skal visualiseres
#'         	BehSlagenhet
#' 			InnlSlagenh
#' 			InnlInnen4eSymptom
#' 			LipidI63
#' 			OppfolgUtf
#' 			SvelgtestUtfort
#'			TidInnTrombolyse40min
#' 			TrombolyseI63
#' 			UtAntitrombotiskI63
#' 			UtAntikoagI63atrie
#' 			UtBT
#'
#' @return Figur som viser tidstrend, dvs. andel av valgt variabel for hvert år. 
#'
#' @export
SlagFigAndelTid <- function(RegData, valgtVar, datoFra='2013-01-01', datoTil='3000-12-31', 
                        minald=0, maxald=130, erMann='',   diagnose='', innl4t='', NIHSSinn='', reshID, outfile='', 
                        enhetsUtvalg=1, preprosess=1, hentData=0) {
  
  
  if (hentData == 1) {		
    RegData <- SlagRegDataSQL(datoFra, datoTil)
  }
  
  # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
  if (preprosess==1){
    RegData <- SlagPreprosess(RegData=RegData, reshID=reshID)
  }
  
  
  RegData$Aar <- 1900 + strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
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
  
  indEgen1 <- match(reshID, RegData$ReshId)
  if (enhetsUtvalg %in% c(1,2,6)) {	#Involverer egen enhet
    shtxt <- as.character(RegData$Avdeling[indEgen1]) } else {
      shtxt <- switch(as.character(enhetsUtvalg), 	
                      '0' = 'Hele landet',
                      '7' = as.character(RegData$Region[indEgen1]),
                      '8' = as.character(RegData$Region[indEgen1]))
    }
  
  if (valgtVar == 'BehSlagenhet') {
    RegData$Variabel[union(which(RegData$AvdForstInnlagt==1), which(RegData$AvdUtskrFra==1))] <- 1 
    VarTxt <- 'slag behandlet i slagenhet'
  }
  if (valgtVar == 'InnlSlagenh') {
    indDirInnlSlag <- intersect(union(which(RegData$AvdForstInnlagt==1), 
                            which(RegData$AvdForstInnlagtHvilken %in% 3:4)),
                            which(RegData$AvdUtskrFra ==1))
    RegData$Variabel[indDirInnlSlag] <- 1 
    VarTxt <- 'innleggelser direkte i slagehet'
  }
  if (valgtVar == 'InnlInnen4eSymptom') {
    #  Tar ut oppvåkningsslag - Dette gjøres nå i preprosssesseringa
    #RegData <- RegData[which(RegData$VaaknetMedSymptom==2), ]
    # Innleggelsestidspunkt comes as class 'character', must be 'date-time'
    #RegData$TidSymptInnlegg <- as.numeric(
    #  difftime(as.POSIXlt(RegData$Innleggelsestidspunkt,format = "%Y-%m-%d %H:%M:%S"),
     #          as.POSIXlt(RegData$Symptomdebut,format = "%Y-%m-%d %H:%M:%S"),units='hours'))
    RegData$Variabel[RegData$TidSymptInnlegg <= 4] <- 1 
    VarTxt <- 'innlagt innen 4t etter symptomdebut'
  }
  if (valgtVar == 'LipidI63') {
    #	'Hjerneinfarkt (I63), levende utskrevet 
    RegData <- RegData[which(RegData$UtskrTil != 10), ] # RegData$Slagdiagnose==2 
    diagnose <- 2	#'I63'
    RegData$Variabel[RegData$UtStatinerLipid==1] <- 1
    VarTxt <- 'utskrevet med lipidsenkning'
  }
  if (valgtVar == 'OppfolgUtf') {
    #Regner ut antall dager fra innleggelse til død
   # RegData$TidDeath <- as.numeric(x = #FLYTTER DENNE TIL PREPROSESS. Får bruk for den til 30d-dødelighet.
    #  difftime(as.POSIXlt(RegData$DeathDate, format = "%Y-%m-%d %H:%M:%S"),
     #          as.POSIXlt(RegData$Innleggelsestidspunkt, format = "%Y-%m-%d %H:%M:%S"),
      #         units='days'))
    #RegData$Variabel[RegData$TidDeath %in% 0:98] <- 1 Her vil bare de med heltallsverdier 0:98 komme med.
    #Utvalget må evt. gjøres slik: RegData$Variabel[RegData$TidDeath < 98] <- 1
    datoTil <- min(datoTil, as.character(Sys.Date()-90))
    #RegData <- RegData[which(RegData$OppfolgUtf %in% 1:2), ] #TV 28.juni: Her må det inn at missing-verdier også skal være med
    #                                                         OK Da gjør vi ingen utvalg her.
    #RegData$Variabel[RegData$OppfolgUtf==1 | RegData$TidDeath==1] <- 1	#HER VELGER DU DE SOM HAR DØDD ETTER ETT DØGN
    RegData$Variabel[RegData$OppfolgUtf==1 | RegData$TidDeath <= 98] <- 1	
    VarTxt <- 'som har fått oppfølging'
  }
  if (valgtVar == 'SvelgtestUtfort') {
    #Av alle, dvs. andel er  de som helt sikkert fått utf. svelgtest
    RegData$Variabel[which(RegData$SvelgtestUtfort %in% c(1,3))] <- 1
    VarTxt <- 'med svelgfunksjon vurdert'
  }
  if (valgtVar == 'TidInnTrombolyse40min') {	
    diagnose <- 2	#'I63'
    #RegData <- RegData[which(RegData$Trombolyse %in% c(1,3)), ]  GJØRES NÅ I PREPROSESSERINGA
    #RegData$TrombolyseStarttid <- as.POSIXlt(RegData$TrombolyseStarttid, format="%Y-%m-%d %H:%M:%S" )
    #RegData$TidInnleggTromb <- as.numeric(difftime(RegData$TrombolyseStarttid,
    #                                               RegData$Innleggelsestidspunkt, units='mins'))
    RegData$Variabel[RegData$TidInnleggTrombolyse <= 40] <- 1 
    RegData$Variabel[is.na(RegData$TidInnleggTrombolyse)] <- NA
    VarTxt <- 'som har fått trombolyse innen 40 min.'
  }
  if (valgtVar == 'TrombolyseI63') {
    diagnose <- 2	# Slagdiagnose 'I63'
    RegData$Variabel[which(RegData$Trombolyse %in% c(1,3))] <- 1
    VarTxt <- 'som har fått trombolyse'
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
    VarTxt <- 'utskrevet med antitrombotisk beh.'
  }
  if (valgtVar == 'UtAntikoagI63atrie') {
    #Slagdiagnose I63, atrieflimmer og levende utskrevet
    RegData <- RegData[which(RegData$UtskrTil != 10 & RegData$Atrieflimmer==1), ]	
    diagnose <- 2	#'I63' 	#RegData$Slagdiagnose==2 & 
    Antikoag <- c('UtWarfarin','UtAndreEnnWarfarin')
    RegData$Variabel[unique(which(RegData[Antikoag]==1, arr.ind=T)[,1])] <- 1
    VarTxt <- 'utskrevet med antikoagulasjon'
  }
  if (valgtVar == 'UtBT') {
    #Bare levende pasienter og de vi vet om har fått bt-medikament eller ikke.
    RegData <- RegData[(which(RegData$UtskrTil != 10) & (RegData$PostMedHoytBT %in% 1:2)), ]
    RegData$Variabel[which(RegData$PostMedHoytBT == 1)] <- 1 #-1:None, 1:Ja, 9:Ukjent, 2:Nei
    VarTxt <- 'utskrevet med blodtrykksmedikament'
  }
  
  Tittel <- switch(valgtVar, InnlSlagenh = 'Innlagt direkte i slagehet' ,
                   BehSlagenhet = 'Behandlet i slagenhet',
                   InnlInnen4eSymptom= 'Innlagt innen 4t etter symptomdebut',
                   LipidI63 = 'Utskrevet med lipidsenkning',
                   SvelgtestUtfort = 'Svelgfunksjon vurdert',
                   TidInnTrombolyse40min = 'Trombolyse innen 40 minutter', 
                   TrombolyseI63 = 'Hjerneinfarktpasienter som har fått trombolyse',	
                   UtAntitrombotiskI63 = c('Utskrevet med antitrombotisk behandling', 
                                           '(Innleggelser etter 31.12.2013.)'),
                   UtAntikoagI63atrie = c('Utskrevet med antikoagulasjon',
                                          '(hjerneinfarktpasienter med atrieflimmer)'),
                   UtBT = 'Blodtrykksmedikament ved utskriving')
  
  #-------------------------Forberedelse...-----------------------------------------
  #Tar ut de med manglende registrering av valgt variabel og gjør utvalg
  SlagUtvalg <- SlagUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, 
                           maxald=maxald, erMann=erMann, diagnose=diagnose, innl4t=innl4t, NIHSSinn=NIHSSinn)
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
  
  
  #Hvis for få observasjoner..
  #if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & medSml == 1)) {
  if (length(indHoved) < 10 | (medSml ==1 & length(indRest)<10)) {
    #-----------Figur---------------------------------------
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {
    
    
    NHovedRes <- length(indHoved)
    NSmlRes <- length(indRest)
    
    
    #-------------------------Beregning av andel-----------------------------------------
    Aartxt <- min(RegData$Aar):max(RegData$Aar)
    RegData$Aar <- factor(RegData$Aar, levels=Aartxt)
    
    NAarRest <- tapply(RegData$Variabel[indRest], RegData$Aar[indRest], length)	
    NAarHendRest <- tapply(RegData$Variabel[indRest], RegData$Aar[indRest],sum, na.rm=T)
    AndelRest <- NAarHendRest/NAarRest*100
    NAarHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'Aar'], length)
    NAarHendHoved <- tapply(RegData[indHoved, 'Variabel'], RegData[indHoved ,'Aar'],sum, na.rm=T)
    AndelHoved <- NAarHendHoved/NAarHoved*100
    Andeler <- rbind(AndelRest, AndelHoved)
    
    
    #-----------Figur---------------------------------------
    
    #Plottspesifikke parametre:
    FigTypUt <- figtype(outfile, fargepalett=SlagUtvalg$fargepalett)
    farger <- FigTypUt$farger
    fargeHoved <- farger[3]
    fargeRest <- farger[1]
    NutvTxt <- length(utvalgTxt)
    hmarg <- 0.04+0.01*NutvTxt
    par('fig' = c(0,1,0,1-hmarg)) 
    cexleg <- 1	#Størrelse på legendtekst
    
    
    ymax <- min(119, 1.25*max(Andeler,na.rm=T))
    plot(Aartxt, AndelHoved,  font.main=1,  type='o', pch="'", col='white', #type='o', 
         xlim= c(Aartxt[1], max(Aartxt)), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(Aartxt), max(Aartxt),length(Aartxt)-1)
         cex=2, xlab='Innleggelsesår', ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i') 	#'Operasjonsår', 
    
    #plot(Aartxt, Midt, xlim= c(xmin, xmax), ylim=c(ymin, ymax), type='n', frame.plot=FALSE, #ylim=c(ymin-0.05*ymax, ymax),
    #		#cex=0.8, cex.lab=0.9, cex.axis=0.9,	
    #		ylab=c(ytxt,'med 95% konfidensintervall'), 
    #		xlab='Operasjonsår', xaxt='n', 
    #		sub='(Tall i boksene angir antall operasjoner)', cex.sub=cexgr)	#, axes=F)
    axis(side=1, at = Aartxt)	
    
    title(Tittel, line=1, font.main=1)
    
    #Legge på linjer i plottet. Denne kan nok gjøres mer elegant...
    if ((ymax > 10) & (ymax < 40)) {lines(range(Aartxt),rep(10,2), col=farger[4])}
    if (ymax > 20) {lines(range(Aartxt),rep(20,2), col=farger[4])}
    if ((ymax > 30) & (ymax < 40)) {lines(range(Aartxt),rep(30,2), col=farger[4])}
    if (ymax > 40) {lines(range(Aartxt),rep(40,2), col=farger[4])}
    if (ymax > 60) {lines(range(Aartxt),rep(60,2), col=farger[4])}
    if (ymax > 80) {lines(range(Aartxt),rep(80,2), col=farger[4])}
    if (ymax > 100) {lines(range(Aartxt),rep(100,2), col=farger[4])}
    #		axis(2, at=c(0,20,40,60,80,100), pos=0),
    
    
    lines(Aartxt, AndelHoved, col=fargeHoved, lwd=3)
    points(Aartxt, AndelHoved, pch="'", cex=2, col=fargeHoved)
    text(Aartxt, AndelHoved, pos=1, NAarHendHoved, cex=0.9, col=fargeHoved)
    
    lines(Aartxt, AndelRest, col=fargeRest, lwd=3)
    points(Aartxt, AndelRest, pch="'", cex=2, col=fargeRest)	#}
    
    Ttxt <- paste('(Tall ved punktene angir antall ', VarTxt, ')', sep='') 
    if (medSml == 1) { 
      text(Aartxt, AndelRest, pos=3, NAarHendRest, cex=0.9, col=fargeRest)
      legend('topleft', border=NA, c(paste(shtxt, ' (N=', NHovedRes, ')', sep=''),
                                     paste(smltxt, ' (N=', NSmlRes, ')', sep=''), Ttxt), bty='n', ncol=1, cex=cexleg, 
             col=c(fargeHoved, fargeRest, NA), lwd=3)		
    } else {
      legend('top', c(paste(shtxt, ' (N=', NHovedRes, ')', sep=''), Ttxt), 
             col=c(fargeHoved, NA), lwd=3, bty='n')
    }
    
    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))
    
    par('fig'=c(0, 1, 0, 1)) 
    if ( outfile != '') {dev.off()}
    #------------------------------------------------------------------------------
    
  }	#end else statement
}	#end function



