library(foreign)

# Importar sem labels
joa_nl <- read.spss("r1_final_3.sav", use.value.labels = FALSE, to.data.frame = TRUE)
joa_nl2 <- joa_nl[,c("horas_sono2", "q8.15a", "q8.16", "q8.17", "q4.18.1")]

# Latent Variable Analysis
# Path Analysis
library(lavaan)
attach(joa_nl2)
joa.model <-'
horas_sono2 ~ y*q8.15a + w*q8.16
q8.17 ~ z*q4.18.1 + x*q8.16
'
joa.fit <- sem(joa.model, data = joa_nl2)
summary(joa.fit)
