#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @inheritParams FigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Standard: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

SlagUtvalg <- function(RegData, datoFra, datoTil, minald, maxald, erMann, diagnose, innl4t, NIHSSinn, fargepalett='BlaaRapp')
{
    # Definer intersect-operator
    "%i%" <- intersect

#Hvis "Variabel" ikke definert ---FJERN DENNE??
if (length(which(names(RegData) == 'Variabel')) == 0 ) {RegData$Variabel <- 0}

indVarMed <- which(RegData$Variabel != 'NA') %i% which(RegData$Variabel != 'NaN') %i% which(RegData$Variabel != '')
indAldUt <- which(RegData$Alder < minald | RegData$Alder > maxald)
indDatoUt <- which(RegData$InnDato < as.Date(datoFra) | RegData$InnDato > as.Date(datoTil))
indKjUt <- if (erMann %in% 0:1) {which(RegData$erMann != erMann)} else {indKjUt <- NULL}
indDiagUt <- if (diagnose %in% c(1,2,9)){which(RegData$Slagdiagnose != diagnose)} else {indDiagUt <- NULL}
indInnl4tUt <- if (innl4t %in% c('Ja','Nei')){switch(innl4t,
										Ja = which(RegData$TidSymptInnlegg >= 4),
										Nei = which(RegData$TidSymptInnlegg < 4))
									} else {indInnl4tUt <- NULL}
grNIHSS <- c(0,6,11,16,21,100)		
RegData$NIHSSinnGr <- cut(RegData$NIHSSinnkomst, breaks=grNIHSS, include.lowest=TRUE, right=FALSE)
NIHSSgrtxt <- c('[0-5]','[6-10]','[11-15]','[16-20]','21+')	
#Må her ta ut de som ikke har fått utført NIHSS ved innkomst.
indNIHSSinnUt <- if (NIHSSinn %in% 1:6) {union(which(RegData$NIHSSikkeUtfort==1), 
		which(RegData$NIHSSinnGr != levels(RegData$NIHSSinnGr)[NIHSSinn]))
		} else {indNIHSSinn <- NULL}
		
indMed <- intersect(setdiff(1:dim(RegData)[1], c(indAldUt, indDatoUt, indKjUt,indDiagUt,indInnl4tUt,indNIHSSinnUt)), 
		indVarMed)
RegData <- RegData[indMed,]




utvalgTxt <- c(paste('Innleggelsesdato: ', 
		min(RegData$InnDato, na.rm=T), ' til ', max(RegData$InnDato, na.rm=T), sep='' ),
	if ((minald>0) | (maxald<120)) {
		paste('Pasienter fra ', min(RegData$Alder, na.rm=T), ' til ', max(RegData$Alder, na.rm=T), ' år', sep='')},
	if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
	if (diagnose %in% c(1,2,9)) {paste('Diagnose: ', c('Blødning (I61)', 'Infarkt (I63)', rep('',6),
		'Udefinert (I64)')[diagnose], sep='')},
	if (innl4t %in% c('Ja','Nei')) {paste('Innlagt innen 4t: ', innl4t, sep='')},
	if (NIHSSinn %in% 1:6) {paste('NIHSS-score: ', NIHSSgrtxt[NIHSSinn], sep='')}
	)

	

UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett)
#names(UtData) <- c('res', 'width', 'height')
return(invisible(UtData)) 
}