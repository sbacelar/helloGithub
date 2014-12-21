library(foreign)

# Importar sem labels
joa_nl <- read.spss("r1_final_3.sav", use.value.labels = FALSE, to.data.frame = TRUE)
joa_nl2 <- joa_nl[,c("horas_sono2", "q8.15a", "q8.16", "q8.17", "q4.18.1")]

library(plyr)
joa_nl2$q8.15a <- mapvalues(joa_nl2$q8.15a, from = c(1, 2), to = c("0", "1"))
joa_nl2$q8.16 <- mapvalues(joa_nl2$q8.16, from = c(1, 2), to = c("0", "1"))
joa_nl2$q8.17 <- mapvalues(joa_nl2$q8.17, from = c(1, 2), to = c("0", "1"))
joa_nl2$q4.18.1 <- mapvalues(joa_nl2$q4.18.1, from = c(1, 2), to = c("0", "1"))

# Multiple Correspondence Analysis

joa_nl2$q8.15af <- factor(joa_nl2$q8.15a)
joa_nl2$q8.16f <-factor(joa_nl2$q8.16)
joa_nl2$q8.17f <- factor(joa_nl2$q8.17)
joa_nl2$q4.18.1f <- factor(joa_nl2$q4.18.1)

joa_nl3 <- joa_nl2[,c("horas_sono2", "q8.15af", "q8.16f", "q8.17f", "q4.18.1f")]

library(FactoMineR)
joa.mca <- MCA(joa_nl3, quanti.sup=1, quali.sup=NULL)
plot.MCA(joa.mca, invisible=c("ind"), cex=0.7)
plot.MCA(joa.mca, invisible=c("var"), cex=0.7)
plot.MCA(joa.mca, cex=0.7)

dimdesc(joa.mca)

