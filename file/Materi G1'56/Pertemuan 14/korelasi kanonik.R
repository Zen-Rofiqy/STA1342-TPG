data <- read.delim("clipboard",header = T)
head(data)

X <- data[,5:7]
Y <- data[,2:4]

library(GGally)
ggpairs(X)
ggpairs(Y)

library(CCA)
correl <- matcor(X, Y)
correl
img.matcor(correl, type = 2)

library(candisc)
cca<- candisc::cancor(X,Y)
summary(cca)

res.cc <- cc(X,Y)
res.cc
