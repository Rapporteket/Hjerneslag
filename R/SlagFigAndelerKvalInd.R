#' Søylediagram som viser andeler av kvalitetsindikatorer
#'
#' Funksjonen lager et søylediagram som viser kvalitetsindikatorer fra hjerneslagregisteret
#'
#' Detaljer: Her bør man liste opp hvilke variable funksjonen benytter...
#' Mye hardkoding/spesialtilpasning. Ikke så egnet til gjenbruk.
#'
#' @inheritParams SlagFigAndeler
#'
#' @return Figur som viser andeler av kvalitetsindikatorer framstilt i et søylediagram.
#' 
#' @export
#'
SlagFigAndelerKvalInd  <- function(RegData, datoFra='2012-04-01', datoTil='2050-12-31', erMann='', NIHSSinn='', 
                                   outfile='', enhetsUtvalg=1, preprosess=TRUE, hentData=0, reshID)	{

    
  if (hentData == 1) {		
    RegData <- SlagRegDataSQL(datoFra, datoTil)
  }
  
  
  # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
  if (preprosess==1){
    RegData <- SlagPreprosess(RegData=RegData, reshID=reshID)
  }
  
  '%i%' <- intersect
  #------------Gjøre utvalg-------------------------
  
  datoFra <- max(as.POSIXlt(datoFra, format="%Y-%m-%d"), as.POSIXlt('2014-01-01', format="%Y-%m-%d"))
  tittel <- 'Utvalgte kvalitetsindikatorer'
  
  #Tar ut de med manglende registrering av valgt variabel og gjør utvalg
  SlagUtvalg <- SlagUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=0, maxald=130, 
                           erMann=erMann, diagnose='', innl4t='', NIHSSinn=NIHSSinn)
  RegData <- SlagUtvalg$RegData
  utvalgTxt <- SlagUtvalg$utvalgTxt
  
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
    
  
      
      #---------------------------------------------------------
      
      #FIGUR SATT SAMMEN AV FLERE VARIABLE og ULIKE UTVALG
      
      #Antitrombotisk = Platehemmende eller antikoagulerende
      Platehemmere <- c('UtASA','UtKlopidogrel','UtDipyridamol')
      Antikoag <- c('UtWarfarin','UtAndreEnnWarfarin')
      indAntitrombotisk <- which(RegData[ ,c('UtPlatehem','UtAntikoag')]==1, arr.ind=T)[,1]
      RegData$UtAntitrombotisk <- 0
      RegData$UtAntitrombotisk[indAntitrombotisk] <- 1		#4884
      
      NavnBTsenkUt <- c('UtDiuretica','UtACEhemmer', 'UtA2Antagonist', 'UtBetablokker', 'UtKalsiumantagonist')
      indBTsenkUt <- which(RegData[ ,NavnBTsenkUt]==1, arr.ind=T)[,1]
      RegData$UtBTsenk <- 0
      RegData$UtBTsenk[indBTsenkUt] <- 1
      
      
      utvalg <- c('Hoved', 'Rest')	#Hoved vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
      Andeler <- list(Hoved = 0, Rest =0)
      
      RegDataLand <- RegData
      
      for (teller in 1:(medSml+1)) {
        #Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.
        RegData <- RegDataLand[switch(utvalg[teller], Hoved = indHoved, Rest=indRest), ]
        
        #Tar ut eget datasett for I63 (hjerneinfarkt)
        RegDataI63 <- RegData[which(RegData$Slagdiagnose==2), ]		#Slagdiagnose I63
        RegDataI63leve <- RegDataI63[which(RegDataI63$UtskrTil != 10), ]		#Levende utskrevet
        NI63leve <- dim(RegDataI63leve)[1]
        
        indAtrI63leve <- which(RegDataI63leve$Atrieflimmer==1)
        indAtrI63<- which(RegDataI63$Atrieflimmer==1)
        
        Dagensdato <- as.POSIXlt(Sys.Date(), format="%Y-%m-%d")
        ind90d <- which(RegData$InnDato < as.Date(Dagensdato-90*24*60*60)) #Innleggelsesdatoer 90 dager før dagens dato.
        indDirInnlSlag <- union(which(RegData$AvdForstInnlagt==1), 
                                intersect(which(RegData$AvdForstInnlagtHvilken %in% 3:4),
                                which(RegData$AvdUtskrFra ==1)))
        Ntrombolyse <- length(which(RegData$Trombolyse %in% c(1,3)) %i% which(is.na(RegDataI63$TidInnleggTrombolyse)==FALSE))
#        Ntrombolyse <- length(which(RegDataI63$Trombolyse %in% c(1,3)) %i% which(is.na(RegDataI63$TidInnleggTrombolyse)==FALSE))
        Ni63 <- dim(RegDataI63)[1]
        N <- dim(RegData)[1]
        #Svelgfunksjon: Alle, dvs. andel av alle også ukjente. Dvs. de som helt sikkert er testet.
        #Hjerneinfarkt, utskr. antitromb.beh.: Levende utskrevet, Antitrombotisk beh: Platehemmere eller Antikoag.
        # Hjerneinf., atriefl. utskr. antikoag.: Levende utskrevet, antikoag (Warfarin eller andre)
        #Medisin mot høyt BT ved utskr.: Levende pasienter i nevner
        #Oppfølging etter 3 mnd: Tar ut de som er døde og innleggelser for mindre enn 90 dager siden
        
        KvalInd <- rbind(
          'Innlagt direkte i slagenhet' = length(indDirInnlSlag)/N,
          'Behandlet i slagenhet' = length(union(which(RegData$AvdForstInnlagt==1), 
                                                 which(RegData$AvdUtskrFra==1)))/N,
          'Vurdert svelgfunksjon' = sum(RegData$SvelgtestUtfort==1)/N,		#Av alle, dvs. andel er  de som helt sikkert fått utf. svelgtest
          'Hjerneinfarkt, <=80 år, trombolysebehandlet' = 
            length(intersect(which(RegDataI63$Trombolyse %in% c(1,3)),which(RegDataI63$Alder <=80)))/
            sum(RegDataI63$Alder <=80),	
          'Trombolyse innen 40 min.' = sum((RegDataI$TidInnleggTrombolyse <= 40), na.rm = TRUE)/Ntrombolyse,	#med i def: & (RegData$Trombolyse %in% c(1,3))
 #         'Trombolyse innen 40 min.' = sum((RegDataI63$TidInnleggTrombolyse <= 40) & (RegDataI63$Trombolyse %in% c(1,3)), na.rm = TRUE)
  #        /Ntrombolyse,	#
          'Hjerneinfarkt, utskrevet med \nantitrombotisk behandling' = sum(RegDataI63leve$UtAntitrombotisk)/NI63leve,
          'Hjerneinfarkt, atrieflimmer, \nutskrevet med antikoagulasjon' = 
            length(unique(which(RegDataI63leve[indAtrI63leve ,Antikoag]==1, arr.ind=T)[,1]))/
            length(indAtrI63leve),
          'Hjerneinfarkt, <80 utskr. kolesterolsenk.' = 
            length(which(RegDataI63leve$Alder<=80 & RegDataI63leve$UtStatinerLipid==1))/
            sum(RegDataI63leve$Alder<=80),
          'Blodtrykksmedikament \nved utskriving' = length(which(RegData$UtBTsenk==1 & RegData$UtskrTil != 10))
              /sum(RegData$UtskrTil != 10),
          'Fått oppfølging eller død \netter 3 mnd.' = 
					length(intersect(which(RegData$OppfolgUtf==1 | RegData$Dod98 == 1), ind90d))
					/length(ind90d)
          )
        
        AndelerFlereVar  <- round(100*KvalInd,2)
        
        #Hvis vi ikke har sammenlikning, vil resultatet lagres i Andeler$Hoved 
        if (teller == 1) {Andeler$Hoved <- AndelerFlereVar
        Nsh <- dim(RegData)[1]
 		Ngr <- c(N, N, N, sum(RegDataI63$Alder <=80), Ntrombolyse, NI63leve, length(indAtrI63leve),
                 sum(RegDataI63leve$Alder<=80), sum(RegData$UtskrTil != 10), length(ind90d))
        }
        if (teller == 2) {Andeler$Rest <- AndelerFlereVar
        NRest <- dim(RegData)[1]
        }
        
      }	#for-løkke
      
      
      #Ta ut indikatorer med få observasjoner
      Ngrense <- 10
      indGrUt <- as.numeric(which(Ngr < Ngrense))
      if (length(indGrUt)==0) { indGrUt <- 0}
      Andeler$Hoved[indGrUt] <- 0
      
      
  #-----------Figur---------------------------------------
   if (as.POSIXlt(datoTil) < as.POSIXlt('2014-01-01')) {
    FigTypUt <- figtype(outfile)
    plot.new()
    title(main=tittel)	
    text(0.5, 0.6, 'Figuren omfatter ikke registreringer før 1.jan. 2014.', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {	
    
    #Hvis for få observasjoner..
    if (length(indHoved) < 10 | (medSml ==1 & length(indRest) <10)) {
      FigTypUt <- figtype(outfile)
      farger <- FigTypUt$farger
      plot.new()
      title(main=tittel)	
      legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
      text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
      text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
      if ( outfile != '') {dev.off()}
    } else {
  
# Lager resultatfigur  
      #Plottspesifikke parametre:
      FigTypUt <- figtype(outfile, height=3*650, fargepalett=SlagUtvalg$fargepalett)	# res=84,  res påvirker png, men ikke pdf
      vmarg <- 0.23
      NutvTxt <- length(utvalgTxt)
      par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
      
      farger <- FigTypUt$farger
      fargeSh <- farger[1]
      fargeRest <- farger[3]
      grtxt <- rownames(KvalInd)
      antGr <- length(grtxt)
      lwdRest <- 2	#størrelse på punktet som repr. resten av landet
      
      #Horisontale søyler
      xmax <- switch(as.character(medSml), '0'= max(Andeler$Hoved+10,100), '1'= 100)
      ymax <- antGr*1.35
      pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, 
                     xlab=c("Andel pasienter (%)"), cex.axis=0.9, cex.lab=0.9, #main=tittel,
                     col=fargeSh, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05, 1.3)*antGr)	#  
      cexmtxt <- 0.9
      cexNpst <- 0.75
	  mtext(at=pos[-c(3,7)]+0.1, rev(grtxt)[-c(3,7)], side=2, las=1, cex=cexmtxt, adj=1, line=0.25)
      tl1 <- 0.2
      tl2 <- 0.1
      mtext(at=pos[7]+tl1, expression('Hjerneinfarkt,' ~ symbol("\243") ~ '80 år'), side=2, las=1, cex=cexmtxt, adj=1, line=0.25)
      mtext(at=pos[7]-tl2, 'trombolysebehandlet', side=2, las=1, cex=cexmtxt, adj=1, line=0.25)
      #mtext(at=pos[5]+tl1, 'Hjerneinfarkt, utskrevet med', side=2, las=1, cex=cexmtxt, adj=1, line=0.25)
      #mtext(at=pos[5]-tl2, 'antitrombotisk behandling', side=2, las=1, cex=cexmtxt, adj=1, line=0.25)
      #mtext(at=pos[4]+tl1, 'Hjerneinfarkt, atrieflimmer', side=2, las=1, cex=cexmtxt, adj=1, line=0.25)
      #mtext(at=pos[4]-tl2, 'utskrevet med antikoagulasjon', side=2, las=1, cex=cexmtxt, adj=1, line=0.25)
      mtext(at=pos[3]+tl1, expression('Hjerneinfarkt,' ~ ''<=80 ~ 'år utskrevet med'), side=2, las=1, cex=cexmtxt, adj=1, line=0.25)
      mtext(at=pos[3]-tl2, 'kolesterolsenkende behandling', side=2, las=1, cex=cexmtxt, adj=1, line=0.25)
      #mtext(at=pos[2]+tl1, 'Blodtrykksmedikament', side=2, las=1, cex=cexmtxt, adj=1, line=0.25)
      #mtext(at=pos[2]-tl2, 'ved utskrivning', side=2, las=1, cex=cexmtxt, adj=1, line=0.25)
      
      
      if (medSml == 1) {
        points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=lwdRest, pch=18) #c("p","b","o"), 
        legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')), 
               border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=lwdRest, 
               lwd=lwdRest, lty=NA, ncol=1, cex=cexmtxt)	#	
      } else {	
        legend('top', paste(shtxt, ' (N=', Nsh,')', sep=''), 
               border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexmtxt)
        andeltxt <- paste(sprintf('%.1f',as.numeric(Andeler$Hoved)), '%',sep='')
        andeltxt[indGrUt] <-''
        text(x=rev(as.numeric(Andeler$Hoved))+1, y=pos+0.1, rev(andeltxt), 
             las=1, cex=cexNpst, adj=0, col=farger[1])	#Andeler
      }
      Ngrtxt <- paste('N=', Ngr, sep='')	#
      Ngrtxt[indGrUt] <- paste('N<', Ngrense, sep='')	#paste(' (<', Ngrense,')',sep='')	#
      text(1,pos, rev(Ngrtxt), adj=0, cex=cexNpst, col=farger[4])	#Antall skrives på hver søyle
      
      
      title(tittel, font.main=1, cex.main=1.2, line=0.5) 
      #Tekst som angir hvilket utvalg som er gjort
      mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
      
      
      par('fig'=c(0, 1, 0, 1)) 
      if ( outfile != '') {dev.off()}
      
    }
  } #end figif
}
