a1 = c(1, 2, 1, 1, 2)
a2 = c(1,2,3,1,2)
a3 = c(0,1,NA,0,1)
aframe = data.frame(a1, a2, a3)

aframe[, 2:3] <- lapply(aframe[, 2:3], as.factor)
str(aframe)

# Copy-paste way
a1 <- as.factor(a1)
a2 <- as.factor(a2)
a3 <- as.factor(a3)
