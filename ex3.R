library(foreign)
# joa <- read.spss("r1_final_3.sav", use.value.labels = TRUE, to.data.frame = TRUE)
# # joa2 <- read.spss("r1_final_3.sav", use.value.labels = FALSE, to.data.frame = TRUE)
# # lista de vars para cluster analysis
# joa3 <- joa[,c("horas_sono2", "q8.15a", "q8.16", "q8.17", "q4.18.1", "q4.18.2", "q4.18.3",  
                         "q4.18.4", "q4.18.5", "q4.18.6", "q4.18.7", "q4.18.8", "q4.07", "q6.18",
                         "Caminhadas", "Ind3IDADE", "q1.05b", "q5.05.1", "q5.05.2", "q5.05.3", "q5.05.4",
                         "q5.05.5", "q5.07.1b", "q5.07.2b", "q5.07.3b", "q5.07.4b", "q5.07.5b",
                         "q6.07b", "q5.08b", "AjudFilhos", "IntActCults", "q4.06", "Ferias", "FinsSemana",
                         "IndDespFerLazer", "q8.13", "q8.14", "q6.19", "RelsReform", "radio_tv")]
# Importar sem labels
joa_nl <- read.spss("r1_final_3.sav", use.value.labels = FALSE, to.data.frame = TRUE)
joa_nl2 <- joa_nl[,c("horas_sono2", "q8.15a", "q8.16", "q8.17", "q4.18.1")]


table(joa_nl2$q8.15a)
table(joa_nl2$q8.16)
table(joa_nl2$q8.17)
table(joa_nl2$q4.18.1)

library(plyr)
joa_nl2$q8.15a <- mapvalues(joa_nl2$q8.15a, from = c(1, 2), to = c(0, 1))
joa_nl2$q8.16 <- mapvalues(joa_nl2$q8.16, from = c(1, 2), to = c(0, 1))
joa_nl2$q8.17 <- mapvalues(joa_nl2$q8.17, from = c(1, 2), to = c(0, 1))
joa_nl2$q4.18.1 <- mapvalues(joa_nl2$q4.18.1, from = c(1, 2), to = c(0, 1))

table(joa_nl2$q8.15a)
table(joa_nl2$q8.16)
table(joa_nl2$q8.17)
table(joa_nl2$q4.18.1)

library(cluster)
dm1 <-daisy(joa_nl2, type = list(asymm=c(2:5)))
summary(dm1)

# Hierarchical cluster analysis using complete linkage method
h <- hclust(dm1, method="complete")

# Plot the clustering tree
plot(h, main="Dendrograma de joa_nl2 (5 vars)")
# Cluster analysis
pamx <- pam(joa_nl2,2)
pamx
summary(pamx)
plot(pamx)

