HovedSkjema <- read.table('C:/Registre/HjerneslagD/Akuttskjema2017-01-24.csv', sep=';', header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 
OppfSkjema <- read.table('C:/Registre/HjerneslagD/AkuttskjemaOppfolging2017-01-24.csv', sep=';', 
                         header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 

OppfSkjema$HovedskjemaGUID <- toupper(OppfSkjema$HovedskjemaGUID)
#names(OppfSkjema)[which(names(OppfSkjema)== 'SkjemaGUID')] <- 'OSkjemaGUID'
SlagData <- merge(HovedSkjema, OppfSkjema, by.x='SkjemaGUID',by.y="HovedskjemaGUID", all.x = TRUE, all.y = FALSE)


#-------Sjekk for dobbeltregistreringer
#Bruk RegData$ForloepID
Navn2 <- names(table(HovedSkjema$HovedskjemaGUID)[table(HovedSkjema$HovedskjemaGUID)>1])
TestTab <- HovedSkjema[(HovedSkjema$HSkjemaGUID %in% Navn2),
                    c("SkjemaGUID", 'UnitId')]
write.table(TestTab, file='Test2Hovedskjema.csv', sep = ';', row.names = F)


Navn2 <- names(table(OppfSkjema$HovedskjemaGUID)[table(OppfSkjema$HovedskjemaGUID)>1])
TestTab <- OppfSkjema[(OppfSkjema$HovedskjemaGUID %in% Navn2),
                       c("HovedskjemaGUID","SkjemaGUID", 'UnitId')]
write.table(TestTab, file='Test2OppfSkjema.csv', sep = ';', row.names = F)


#------Teste variable
RegData <- SlagPreprosess(RegData=RegData, reshID=reshID)

indTidSymptInn30d <- (which(RegData$TidSymptInnlegg>30*24) %u% which(RegData$TidSymptInnlegg<0)) %i%
                    which(is.na(RegData$TidSymptInnlegg)==F)

indTidInnTromb4t <- (which(RegData$TidInnleggTrombolyse>60*4) %u% which(RegData$TidInnleggTrombolyse<0)) %i% 
                           which(is.na(RegData$TidInnleggTrombolyse)==F)

TidInnSympt30dTromb4t <- RegData[unique(indTidSymptInn30d %u% indTidInnTromb4t), 
          c("SkjemaGUID", "ReshId", 'Symptomdebut', "VaaknetMedSymptom",'Innleggelsestidspunkt',
            'TrombolyseStarttid', 'TidSymptInnlegg', 'TidInnleggTrombolyse')]
	write.table(TidInnSympt30dTromb4t, file='TidInnSympt30dTromb4t.csv', sep = ';', row.names = F)

	
  
