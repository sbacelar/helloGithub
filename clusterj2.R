library(foreign)
# Importar sem labels
joa_nl <- read.spss("reforma3.sav", use.value.labels = FALSE, to.data.frame = TRUE)

# lista de vars para cluster analysis
joa_nl2 <- joa_nl[,c("num", "horas_sono2d", "projFuturod", "IGDEd", "Caminhadasd", "fechCasad",
"q1.05bz", "AmbFamiliad", "RelsNetosd", "q6.07bz", "q5.08bz", "AjudFilhosd",
"IntActCultsd", "q4.06d", "q4.33b", "q4.13", "IndDespFerLazerd",
"q8.13z", "q8.14z", "q6.19z", "RelsReformd", "ConsumoCultTVd", "PossInid", "HISTPROFbz",
"q2.11bz", "IndANOMIAd", "IndSATISFACAOd", "q6.17z", "DifAdaptMudd", "q8.06bz",
"Voluntariadod", "IndDespSauded", "LeJornal", "LeLivros", "q2.20", "q4.19",
"Contact_Vizinhos", "Contact_Telef_Amigos",
"Ind_SitTrabPassd", "Ind_AtiForaTrabPassd", "Ind_Sauded", "Ind_AmbSociald",
"Escolaridaded", "q2.13", "Rendimentod")]

attach(joa_nl2)
# q8.15a <- as.factor(q8.15a)
# Uma alternativa
# spssdatac$q8.15a <- as.factor(spssdatac$q8.15a)



library(cluster)
dm1 <-daisy(joa_nl2[,-1], type = list(asymm=c(1:44)))
summary(dm1)

# Hierarchical cluster analysis using complete linkage method
h <- hclust(dm1, method="complete")

# Plot the clustering tree
plot(h, main="Dendrograma de joa_nl2 (5 vars)")

# Cluster analysis
pamx <- pam(dm1,6)
pamx
summary(pamx)
plot(pamx)

joa_nl3 <- cbind(joa_nl2, pamx$clustering)
tab2 <- NULL
tab4 <- NULL
for (i in 2:44){
  print (i)
  tab <- table(joa_nl3[,i], by=pamx$clustering)
  print (tab)
  tab2 <- rbind(tab, tab2)
  tab3 <- (prop.table(tab, 2)) # column percentages
  print(tab3, main = i)
  tab4 <- rbind(tab3, tab4)
  barplot(tab, main = i)
}
s <- row.names(tab4)
tab5 <- data.frame(tab4)
tab5 <- cbind(s, tab5)
tab6 <- tab5[which(tab5$s==1),]
tab6 <- cbind((2:44), tab6)
colnames(tab6) <-c("var","s", "c1","c2", "c3", "c4", "c5", "c6")

for (i in 3:8){
barplot(tab6[,i], names.arg = tab6$var, main = (i-2))

# get the range for the x and y axis
xrange <- range(tab6$var)
yrange <- range(0:1)

# set up the plot
plot(xrange, yrange, type="n", xlab="var",
     ylab="% (1)" )
linetype <- c(1:6)
for (i in 3:8){
lines(tab6[,i], type="b", lwd=1.5,
      lty=linetype[i])
}
# "num", "horas_sono2d", "projFuturod", "IGDEd", "Caminhadasd", "fechCasad",
# "q1.05bz", "AmbFamiliad", "RelsNetosd", "q6.07bz", "q5.08bz", "AjudFilhosd
# "IntActCultsd", "q4.06d", "q4.33b", "q4.13", "IndDespFerLazerd", "", "
# "q8.13z", "q8.14z", "q6.19z", "RelsReformd", "
# "ConsumoCultTVd", "PossInid", "HISTPROFbz", "q2.11bz", "
# "IndANOMIAd", "IndSATISFACAOd", "q6.17z", "DifAdaptMudd", "q8.06bz", "
# "Voluntariadod", "IndDespSauded", "LeJornal", "LeLivros", "q2.20", "q4.19
# "Contact_Vizinhos", "Contact_Telef_Amigos
# "Ind_SitTrabPassd", "Ind_AtiForaTrabPassd", "Ind_Sauded", "Ind_AmbSociald",
# "Escolaridaded", "q2.13", "Rendimentod"