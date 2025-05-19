#---- DIRECTORIO DE TRABAJO ----
setwd("~/Downloads/Datos_INEGI")
#---- LIBRERIAS ----
library(readr)
#---- LECTURA Y VISUALIZACION DE DATOS ----
misdatos <- read_csv("RESAGEBURB_09CSV20_b.csv")
View(RESAGEBURB_09CSV20_b)
#---- LIMPIEZA DE DATOS ----
columnas <- colnames(misdatos); print(columnas)
misdatos <- misdatos[,3:229]; View(misdatos)
##---- MUNICIPIOS ----
municipios <- unique(misdatos$MUN); print(municipios)
temp <- factor(municipios); niveles <- levels(temp)
misdatos$MUN <- factor(misdatos$MUN, levels = municipios, labels = municipios); summary(temp)
##---- NOMBRES DE MUNICIPIOS ----
TemasSelectos <- misdatos$NOM_MUN; n<- length(TemasSelectos)
MateriasCorrected <- c()
for(i in 1:n){
  MateriasCorrected <- toupper(TemasSelectos[i])
  MateriasCorrected <- gsub("Á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("É","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ñ","n",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(":","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("-"," ",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("\\.","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("\\(","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("\\)","",as.character(MateriasCorrected,fixed=TRUE));
  TemasSelectos[i] <- MateriasCorrected
}
misdatos$NOM_MUN <- TemasSelectos;
ListaTemasSelectos <- sort(unique(misdatos$NOM_MUN));
head(ListaTemasSelectos,15); tail(ListaTemasSelectos,15)
temp <- factor(ListaTemasSelectos);  niveles <- levels(temp)
misdatos$NOM_MUN <- factor(misdatos$NOM_MUN, levels = niveles, 
                                   labels = ListaTemasSelectos)
summary(misdatos$NOM_MUN); View(as.data.frame(table(misdatos$NOM_MUN)))
##---- NOMBRES DE LOCALIDAD ----
TemasSelectos <- misdatos$NOM_LOC; n<- length(TemasSelectos)
MateriasCorrected <- c()
for(i in 1:n){
  MateriasCorrected <- toupper(TemasSelectos[i])
  MateriasCorrected <- gsub("Á","A",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("É","E",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Í","I",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ó","O",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ú","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ü","U",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ö","OE",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("Ñ","n",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("'","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(":","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("-"," ",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub(",","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("/","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("\\.","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("\\(","",as.character(MateriasCorrected,fixed=TRUE));
  MateriasCorrected <- gsub("\\)","",as.character(MateriasCorrected,fixed=TRUE));
  TemasSelectos[i] <- MateriasCorrected
}
misdatos$NOM_LOC <- TemasSelectos;
ListaTemasSelectos <- sort(unique(misdatos$NOM_LOC));
head(ListaTemasSelectos,15); tail(ListaTemasSelectos,15)
temp <- factor(ListaTemasSelectos);  niveles <- levels(temp)
misdatos$NOM_LOC <- factor(misdatos$NOM_LOC, levels = niveles, 
                           labels = ListaTemasSelectos)
summary(misdatos$NOM_LOC); View(as.data.frame(table(misdatos$NOM_LOC)))
##---- SE CREA COLUMNA MUNICIPIO-LOC ----
NMUN_LOC <- paste0(misdatos$NOM_LOC,"_",misdatos$NOM_MUN); head(NMUN_LOC); tail(NMUN_LOC)
ListaTemasSelectos <- sort(unique(NMUN_LOC));
head(ListaTemasSelectos,15); tail(ListaTemasSelectos,15)
temp <- factor(ListaTemasSelectos);  niveles <- levels(temp)
misdatos$NMUN_LOC <- factor(NMUN_LOC, levels = niveles, 
                               labels = ListaTemasSelectos)
summary(misdatos$NMUN_LOC); View(as.data.frame(table(misdatos$NMUN_LOC)))
View(misdatos)
##---- LOCALIDAD ----
localidad <- unique(misdatos$LOC); print(localidad)
temp <- factor(localidad); niveles <- levels(temp)
misdatos$LOC <- factor(misdatos$LOC, levels = localidad, labels = localidad); summary(misdatos$LOC)
##---- RESPALDO 1 ----
write.csv(misdatos,'Repositorio/misdatos.csv')
saveRDS(misdatos, file = "Repositorio/mibddwking.rds")
save.image("Wkspaces/Wkspace1.RData")
##---- AGEB ----
AGEB <- unique(misdatos$AGEB); print(AGEB)
temp <- factor(AGEB); niveles <- levels(temp)
misdatos$AGEB <- factor(misdatos$AGEB, levels = AGEB, labels = AGEB); summary(misdatos$AGEB)
