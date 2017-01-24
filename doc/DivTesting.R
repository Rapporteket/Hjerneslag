Navn2 <- names(table(SlagData$HovedskjemaGUID)[table(SlagData$HovedskjemaGUID)>1])

TestTab <- SlagData[(SlagData$HovedskjemaGUID %in% Navn2),
                    c("HovedskjemaGUID", "SkjemaGUID.x", "SkjemaGUID.y", "HSkjemaGUID", "PasientGUID.y", "Innleggelsestidspunkt")]
write.table(TestTab, file='Test2HovedskjemaGUID.csv', sep = ';', row.names = F)


#Sjekk for dobbeltregistreringer
#Sjekk i oppfølgingsskjema
SkjemaGUID	HovedskjemaGUID
A31A3E8B-9F9A-4912-A2FA-CB9DC7E10FB1	009e5475-6ea0-4ddb-941f-869a35ba3ef0
D784A3D5-5E94-492E-9CAB-227B55613311	009e5475-6ea0-4ddb-941f-869a35ba3ef0

Navn2Oppf <- names(table(OppfSkjema$HovedskjemaGUID)[table(OppfSkjema$HovedskjemaGUID)>1])

