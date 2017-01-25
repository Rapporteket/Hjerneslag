HovedSkjema <- read.table('C:/Registre/HjerneslagD/Akuttskjema2017-01-24.csv', sep=';', header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 
OppfSkjema <- read.table('C:/Registre/HjerneslagD/AkuttskjemaOppfolging2017-01-24.csv', sep=';', 
                         header=T, encoding="UTF-8") #, fileEncoding='UTF-8', 

OppfSkjema$HovedskjemaGUID <- toupper(OppfSkjema$HovedskjemaGUID)
#names(OppfSkjema)[which(names(OppfSkjema)== 'SkjemaGUID')] <- 'OSkjemaGUID'
SlagData <- merge(HovedSkjema, OppfSkjema, by.x='SkjemaGUID',by.y="HovedskjemaGUID", all.x = TRUE, all.y = FALSE)


#Sjekk for dobbeltregistreringer
Navn2 <- names(table(HovedSkjema$HovedskjemaGUID)[table(HovedSkjema$HovedskjemaGUID)>1])
TestTab <- HovedSkjema[(HovedSkjema$HSkjemaGUID %in% Navn2),
                    c("SkjemaGUID", 'UnitId')]
write.table(TestTab, file='Test2Hovedskjema.csv', sep = ';', row.names = F)


Navn2 <- names(table(OppfSkjema$HovedskjemaGUID)[table(OppfSkjema$HovedskjemaGUID)>1])
TestTab <- OppfSkjema[(OppfSkjema$HovedskjemaGUID %in% Navn2),
                       c("HovedskjemaGUID","SkjemaGUID", 'UnitId')]
write.table(TestTab, file='Test2OppfSkjema.csv', sep = ';', row.names = F)





