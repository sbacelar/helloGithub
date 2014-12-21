# R-friendly way to convert R data.frame column to a vector?
a1 = c(1, 2, 3, 4, 5)
a2 = c(6, 7, 8, 9, 10)
a3 = c(11, 12, 13, 14, 15)
aframe = data.frame(a1, a2, a3)

avector <- as.vector(aframe['a2'])
class(avector) 

avector <- aframe[['a2']]
class(avector)

avector <- aframe[,2]
class(avector)
