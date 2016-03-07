#' Søylediagram som viser antall registreringer per måned
#'
#' Søylediagram som viser antall registreringer per måned, siste 12 hele måneder fra valgt sluttdato, 
#'  av hovedskjema, samt oppfølgingsskjema+døde
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams FigAndeler 
#'          
#' @param enhetsUtvalg Gjør gruppeutvalg for
#'                 0: Hele landet
#'                 2: Egen enhet
#'				         7: Egen region 
#'
#' @return Søylediagram som viser antall registreringer per måned for valgte 12 måneder
#'
#' @export
FigAntReg  <- function(RegData, datoTil='2050-12-31', 
		minald=0, maxald=130, erMann='', diagnose='', innl4t='', NIHSSinn='', outfile='', 
		reshID, enhetsUtvalg=2, preprosess=1, hentData=0)
{


if (hentData == 1) {
  library(RMySQL)
  source(paste0(libkat, 'SlagLoadRegData.R'))
  source(paste0(libkat, 'SlagLoadRegDataMinimal.R'))
  cat('\nLocal loading of RegData...\n')
  RegData <- SlagLoadRegDataMinimal()
}
  
# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
if (preprosess){
    RegData <- SlagPreprosess(RegData=RegData, reshID=reshID)
  }
  

reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
shtxt <- switch(as.character(enhetsUtvalg), 	
								'0' = 'Hele landet',
								'2' = as.character(RegData$Avdeling[indEgen1]),
								'7' = as.character(RegData$Region[indEgen1]))
if (enhetsUtvalg %in% c(2,7)) {	
		RegData <- switch(as.character(enhetsUtvalg),
						'2' = RegData[which(RegData$ReshId == reshID),],	#kun egen enhet
						'7' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),])	#kun egen region
	}


DagensDato <- as.POSIXlt(datoTil, tz='UTC', format='%Y-%m-%d')	#min(, as.POSIXlt(Sys.Date()))
datoFra <- as.POSIXlt(paste(DagensDato$year-1+1900,'-',(DagensDato$mon+1),'-',1, sep=''))
datoTil <- as.POSIXlt(paste(DagensDato$year+1900,'-',(DagensDato$mon+1),'-',1, sep='')) - 60*60*24

#Gjør utvalg (siste 12 hele mnd)
SlagUtvalg <- SlagLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
		erMann=erMann, diagnose=diagnose, innl4t=innl4t, NIHSSinn=NIHSSinn)
RegData <- SlagUtvalg$RegData
utvalgTxt <- SlagUtvalg$utvalgTxt

tittel <- 'Registreringer, 12 månedersperiode,'
	

#Standardparametre
cexgr <- 0.9
retn <- 'V'
txtretn <- 1
grtxt <- ''
subtxt <- 'Siste 12 hele måneder fra valgt sluttdato'
Nsvar <- dim(RegData)[1]	

mndtxt <- c('jan','feb','mar','apr','mai','jun','jul','aug','sep','okt','nov','des')	
startmnd <- DagensDato$mon+1
mndRekkef <- c(startmnd:12, min(1,(startmnd-1)):(startmnd-1))	#Må ta høyde for startmnd=1
grtxt <- mndtxt[mndRekkef]	
RegData$InnDato <- as.POSIXlt(RegData$InnDato)
RegData$Mnd <- factor(RegData$InnDato$mon, levels= 0:11, labels = mndtxt)
AntHoved <- table(RegData$Mnd)[mndRekkef]	
indOppf <- which(RegData$OppfolgUtf==1) 						#Levende med oppfølging
AntOppf <- table(RegData$Mnd[indOppf])[mndRekkef]	
#indDod <- with(RegData, union(which(OppfolgUtf==2 & AarsakManglendeOppf==3), which(UtskrTil==10)))
indDod <- with(RegData, which(UtskrTil==10))	#Død under sykehusoppholdet

AntDod <- table(RegData$Mnd[indDod])[mndRekkef]

AntHODmnd <- cbind(AntOppf, AntDod, AntHoved-(AntOppf+AntDod))

#RegData$MndAar <- paste(RegData$Mnd,RegData$InnDato$year-100,sep='')
#table(RegData$MndAar)

#axis(1, RegData$InnDato, format(RegData$InnDato, "%m %y"), cex.axis = .7)
#library(date)
#hist(dat.geo$Date_of_Onset[(dat.geo$suburb=="x")], "weeks", 
# format = "%d %b %y", freq=T, col=rgb(0,0,0,1), axes=F, main="")
#axis.Date(1, at=seq(as.Date("2011-10-10"), as.Date("2012-03-19"), by="2 weeks"),
# format="%d %b %y")



#-----------Figur---------------------------------------

#Plottspesifikke parametre:
antGr <- 12
Ngr <- matrix(c(AntHoved, AntOppf, AntDod), antGr, 3)
NutvTxt <- length(utvalgTxt)
ymax <- max(AntHoved,na.rm=T)*1.25

FigTypUt <- figtype(outfile, fargepalett=SlagUtvalg$fargepalett)	#res=96, width=555,height=555, 
farger <- FigTypUt$farger
par('fig'=c(0, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

pos <- barplot(t(AntHODmnd), beside=FALSE, las=txtretn, ylab="Antall registreringer",	
	names.arg=rep(NA,12), #cex.names=cexgr, 
	col=farger[1:3], border=NA, ylim=c(0, ymax))	#
mtext(grtxt, at=pos, side=1, las=1, cex=1, adj=0.5, line=0.5)
mtext(subtxt, side=1, las=1, cex=1, adj=0.5, line=2.5)
lines(x=c(pos[13-startmnd]+0.6, pos[13-startmnd]+0.6), y=c(0, max(AntHoved)), lty=2, col=farger[3])

legend('topright', c('Bare hovedskjema', 'Døde', 'Oppfølging, 3 mnd'), bty='n',
	fill=farger[3:1], border=NA, ncol=1, cex=0.9)
title(tittel, font.main=1, cex=1.2)	#line=0.5, 
title(paste(shtxt, ' (N=', Nsvar,')', sep=''), font.main=1, line=0.5)

#Tekst som angir hvilket utvalg som er gjort
avst <- 0.8
utvpos <- 3	#Startlinje for teksten
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1)) 
if ( outfile != '') {dev.off()}

#}
}
