# lista de vars para cluster analysis
spssdatac <- spssdata[,c("horas_sono2", "q8.15a", "q8.16", "q8.17", "q4.18.1", "q4.18.2", "q4.18.3",  
                         "q4.18.4", "q4.18.5", "q4.18.6", "q4.18.7", "q4.18.8", "q4.07", "q6.18",
                         "Caminhadas", "Ind3IDADE", "q1.05b", "q5.05.1", "q5.05.2", "q5.05.3", "q5.05.4",
                         "q5.05.5", "q5.07.1b", "q5.07.2b", "q5.07.3b", "q5.07.4b", "q5.07.5b",
                         "q6.07b", "q5.08b", "AjudFilhos", "IntActCults", "q4.06", "Ferias", "FinsSemana",
                         "IndDespFerLazer", "q8.13", "q8.14", "q6.19", "RelsReform", "radio_tv")]

# spssdataf <- NULL
# spssdatac[, c("q8.15a", "q8.16")] <- as.factor(spssdatac[, c("q8.15a", "q8.16")])
# Uma alternativa
spssdatac$q8.15a <- as.factor(spssdatac$q8.15a)
v <- c(2,3)
# for (i in v){
#   ve <- as.factor(spssdatad[,i])
#   spssdataf <- cbind(spssdataf, ve)
# }

spssdatac[,v] <- lapply(spssdatac[,v], as.factor)
str(spssdatac)


