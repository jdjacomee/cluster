#############################################################################

#cargamos todos los datos del archivo en una matriz llamada A
A <-
  read.csv(
    "C:/Users/David/Desktop/Otros/examenad20171h.csv",
    header = TRUE ,
    sep = ";" ,
    dec = ","
  )

#eliminanos el dato NA y el outlier
(B <- A[-35,]) #NA tambien puede ser B<-na.omit(A)
(B <- B[-30,]) #outlier
row.names(B) <- seq(length = nrow(B))

#sacamos los datos que nos interesan
C <- B[, 4:8]


#CLUSTER
clus1 <-
  hclust(dist(as.matrix(C), method = "euclidean"), method = "ward.D2") #aqui segun el grafico me quedo con dos clusters
clus1

#criterio cubico
library(NbClust)
cluster <-
  NbClust(
    C,
    distance = "euclidean",
    min.nc = 4,
    max.nc = 4,
    method = "ward.D2",
    index = "ccc"
  )

cl <- cluster$Best.partition

c1 <- c()
c2 <- c()
c3 <- c()
c4 <- c()

#Separa individuos en conglomerados
for (i in 1:NROW(cl)) {
  if (cl[i] == 1) {
    c1 <- c(c1, i)
  }
  if (cl[i] == 2) {
    c2 <- c(c2, i)
  }
  if (cl[i] == 3) {
    c3 <- c(c3, i)
  }
  if (cl[i] == 4) {
    c4 <- c(c4, i)
    
  }
}

#Media Costo por onza
#Conglomerado 1
for (i in 1:length(c1)) {
  co1 <- C$X..oz[c1]
  mediaCostoOnzaCong1 <- mean(co1)
}
#Conglomerado 2
for (i in 1:length(c2)) {
  co2 <- C$X..oz[c2]
  mediaCostoOnzaCong2 <- mean(co2)
}
#Conglomerado 3
for (i in 1:length(c3)) {
  co3 <- C$X..oz[c3]
  mediaCostoOnzaCong3 <- mean(co3)
}
#Conglomerado 4
for (i in 1:length(c4)) {
  co4 <- C$X..oz[c4]
  mediaCostoOnzaCong4 <- mean(co4)
}

#Media Costo por libra de proteina
#Conglomerado 1
for (i in 1:length(c1)) {
  cp1 <- C$X..lb.Protein[c1]
  mediaCostoProteCong1 <- mean(cp1)
}
#Conglomerado 2
for (i in 1:length(c2)) {
  cp2 <- C$X..lb.Protein[c2]
  mediaCostoProteCong2 <- mean(cp2)
}
#Conglomerado 3
for (i in 1:length(c3)) {
  cp3 <- C$X..lb.Protein[c3]
  mediaCostoProteCong3 <- mean(cp3)
}
#Conglomerado 4
for (i in 1:length(c4)) {
  cp4 <- C$X..lb.Protein[c4]
  mediaCostoProteCong4 <- mean(cp4)
}

#Media Calorias
#Conglomerado 1
for (i in 1:length(c1)) {
  cal1 <- C$Calories[c1]
  mediaCaloriasCong1 <- mean(cal1)
}
#Conglomerado 2
for (i in 1:length(c2)) {
  cal2 <- C$Calories[c2]
  mediaCaloriasCong2 <- mean(cal2)
}
#Conglomerado 3
for (i in 1:length(c3)) {
  cal3 <- C$Calories[c3]
  mediaCaloriasCong3 <- mean(cal3)
}
#Conglomerado 4
for (i in 1:length(c4)) {
  cal4 <- C$Calories[c4]
  mediaCaloriasCong4 <- mean(cal4)
}

#Media Sodio
#Conglomerado 1
for (i in 1:length(c1)) {
  sod1 <- C$Sodium[c1]
  mediaSodioCong1 <- mean(sod1)
}
#Conglomerado 2
for (i in 1:length(c2)) {
  sod2 <- C$Sodium[c2]
  mediaSodioCong2 <- mean(sod2)
}
#Conglomerado 3
for (i in 1:length(c3)) {
  sod3 <- C$Sodium[c3]
  mediaSodioCong3 <- mean(sod3)
}
#Conglomerado 4
for (i in 1:length(c4)) {
  sod4 <- C$Sodium[c4]
  mediaSodioCong4 <- mean(sod4)
}

#Media Protein/Grasa
#Conglomerado 1
for (i in 1:length(c1)) {
  pf1 <- C$Protein.Fat[c1]
  mediaProtGrasCong1 <- mean(pf1)
}
#Conglomerado 2
for (i in 1:length(c2)) {
  pf2 <- C$Protein.Fat[c2]
  mediaProtGrasCong2 <- mean(pf2)
}
#Conglomerado 3
for (i in 1:length(c3)) {
  pf3 <- C$Protein.Fat[c3]
  mediaProtGrasCong3 <- mean(pf3)
}
#Conglomerado 4
for (i in 1:length(c4)) {
  pf4 <- C$Protein.Fat[c4]
  mediaProtGrasCong4 <- mean(pf4)
}


mediaCostoOnza <-
  c(
    as.double(mediaCostoOnzaCong1),
    as.double(mediaCostoOnzaCong2),
    as.double(mediaCostoOnzaCong3),
    as.double(mediaCostoOnzaCong4)
  )

mediaCostoProte <-
  c(
    as.double(mediaCostoProteCong1),
    as.double(mediaCostoProteCong2),
    as.double(mediaCostoProteCong3),
    as.double(mediaCostoProteCong4)
  )

mediaCalorias <-
  c(
    as.double(mediaCaloriasCong1),
    as.double(mediaCaloriasCong2),
    as.double(mediaCaloriasCong3),
    as.double(mediaCaloriasCong4)
  )

mediaSodio <-
  c(
    as.double(mediaSodioCong1),
    as.double(mediaSodioCong2),
    as.double(mediaSodioCong3),
    as.double(mediaSodioCong4)
  )

mediaProtGras <-
  c(
    as.double(mediaProtGrasCong1),
    as.double(mediaProtGrasCong2),
    as.double(mediaProtGrasCong3),
    as.double(mediaProtGrasCong4)
  )

#Tabla de medias
medias <-
  data.frame(mediaCostoOnza,
             mediaCostoProte,
             mediaCalorias,
             mediaSodio,
             mediaProtGras)

medias

#Porcentaje tipos...
#Cluster 1
numBeef <- 0
numMeat <- 0
numPoultry <- 0
for (i in 1:length(c1)) {
  if (B$Type[c1[i]] == 'Beef') {
    numBeef <- numBeef + 1
  }
  if (B$Type[c1[i]] == 'Meat') {
    numMeat <- numMeat + 1
  }
  if (B$Type[c1[i]] == 'Poultry') {
    numPoultry <- numPoultry + 1
  }
}
porcentajeBeefC1 <- (numBeef * 100) / length(c1)
porcentajeMeatC1 <- (numMeat * 100) / length(c1)
porcentajePoultryC1 <- (numPoultry * 100) / length(c1)

#Cluster 2
numBeef <- 0
numMeat <- 0
numPoultry <- 0
for (i in 1:length(c2)) {
  if (B$Type[c2[i]] == 'Beef') {
    numBeef <- numBeef + 1
  }
  if (B$Type[c2[i]] == 'Meat') {
    numMeat <- numMeat + 1
  }
  if (B$Type[c2[i]] == 'Poultry') {
    numPoultry <- numPoultry + 1
  }
}
porcentajeBeefC2 <- (numBeef * 100) / length(c2)
porcentajeMeatC2 <- (numMeat * 100) / length(c2)
porcentajePoultryC2 <- (numPoultry * 100) / length(c2)

#Cluster 3
numBeef <- 0
numMeat <- 0
numPoultry <- 0
for (i in 1:length(c3)) {
  if (B$Type[c3[i]] == 'Beef') {
    numBeef <- numBeef + 1
  }
  if (B$Type[c3[i]] == 'Meat') {
    numMeat <- numMeat + 1
  }
  if (B$Type[c3[i]] == 'Poultry') {
    numPoultry <- numPoultry + 1
  }
}
porcentajeBeefC3 <- (numBeef * 100) / length(c3)
porcentajeMeatC3 <- (numMeat * 100) / length(c3)
porcentajePoultryC3 <- (numPoultry * 100) / length(c3)

#Cluster 4
numBeef <- 0
numMeat <- 0
numPoultry <- 0
for (i in 1:length(c4)) {
  if (B$Type[c4[i]] == 'Beef') {
    numBeef <- numBeef + 1
  }
  if (B$Type[c4[i]] == 'Meat') {
    numMeat <- numMeat + 1
  }
  if (B$Type[c4[i]] == 'Poultry') {
    numPoultry <- numPoultry + 1
  }
}
porcentajeBeefC4 <- (numBeef * 100) / length(c4)
porcentajeMeatC4 <- (numMeat * 100) / length(c4)
porcentajePoultryC4 <- (numPoultry * 100) / length(c4)

porcentajeBeef <-
  c(porcentajeBeefC1,
    porcentajeBeefC2,
    porcentajeBeefC3,
    porcentajeBeefC4)
porcentajeMeat <-
  c(porcentajeMeatC1,
    porcentajeMeatC2,
    porcentajeMeatC3,
    porcentajeMeatC4)
porcentajePoultry <-
  c(
    porcentajePoultryC1,
    porcentajePoultryC2,
    porcentajePoultryC3,
    porcentajePoultryC4
  )

#Porcentaje sabores...
#Cluster 1
numBland <- 0
numMedium <- 0
numScrumptious <- 0
for (i in 1:length(c1)) {
  if (B$Taste[c1[i]] == 'Bland') {
    numBland <- numBland + 1
  }
  if (B$Taste[c1[i]] == 'Medium') {
    numMedium <- numMedium + 1
  }
  if (B$Taste[c1[i]] == 'Scrumptious') {
    numScrumptious <- numScrumptious + 1
  }
}
porcentajeBlandC1 <- (numBland * 100) / length(c1)
porcentajeMediumC1 <- (numMedium * 100) / length(c1)
porcentajeScrumptiousC1 <- (numScrumptious * 100) / length(c1)

#Cluster 2
numBland <- 0
numMedium <- 0
numScrumptious <- 0
for (i in 1:length(c2)) {
  if (B$Taste[c2[i]] == 'Bland') {
    numBland <- numBland + 1
  }
  if (B$Taste[c2[i]] == 'Medium') {
    numMedium <- numMedium + 1
  }
  if (B$Taste[c2[i]] == 'Scrumptious') {
    numScrumptious <- numScrumptious + 1
  }
}
porcentajeBlandC2 <- (numBland * 100) / length(c2)
porcentajeMediumC2 <- (numMedium * 100) / length(c2)
porcentajeScrumptiousC2 <- (numScrumptious * 100) / length(c2)

#Cluster 3
numBland <- 0
numMedium <- 0
numScrumptious <- 0
for (i in 1:length(c3)) {
  if (B$Taste[c3[i]] == 'Bland') {
    numBland <- numBland + 1
  }
  if (B$Taste[c3[i]] == 'Medium') {
    numMedium <- numMedium + 1
  }
  if (B$Taste[c3[i]] == 'Scrumptious') {
    numScrumptious <- numScrumptious + 1
  }
}
porcentajeBlandC3 <- (numBland * 100) / length(c3)
porcentajeMediumC3 <- (numMedium * 100) / length(c3)
porcentajeScrumptiousC3 <- (numScrumptious * 100) / length(c3)

#Cluster 4
numBland <- 0
numMedium <- 0
numScrumptious <- 0
for (i in 1:length(c4)) {
  if (B$Taste[c4[i]] == 'Bland') {
    numBland <- numBland + 1
  }
  if (B$Taste[c4[i]] == 'Medium') {
    numMedium <- numMedium + 1
  }
  if (B$Taste[c4[i]] == 'Scrumptious') {
    numScrumptious <- numScrumptious + 1
  }
}
porcentajeBlandC4 <- (numBland * 100) / length(c4)
porcentajeMediumC4 <- (numMedium * 100) / length(c4)
porcentajeScrumptiousC4 <- (numScrumptious * 100) / length(c4)


porcentajeBland <-
  c(porcentajeBlandC1,
    porcentajeBlandC2,
    porcentajeBlandC3,
    porcentajeBlandC4)
porcentajeMedium <-
  c(porcentajeMediumC1,
    porcentajeMediumC2,
    porcentajeMediumC3,
    porcentajeMediumC4)
porcentajeScrumptious <-
  c(
    porcentajeScrumptiousC1,
    porcentajeScrumptiousC2,
    porcentajeScrumptiousC3,
    porcentajeScrumptiousC4
  )

#Tabla de porcentajes
#Tipo
porcentajeType <-
  data.frame(porcentajeBeef, porcentajeMeat, porcentajePoultry)
porcentajeType

#Sabores
porcentajeTaste <-
  data.frame(porcentajeBland, porcentajeMedium, porcentajeScrumptious)
porcentajeTaste
