#Membuat qq Plot manual di R
data2 <- read.delim("clipboard", header=T)
data2
cor.test(data2$Chi.squarediurutkan,data2$di.2)

# calculate the sample mean vector
cm <- colMeans(data2)
cm

# calculate the variance-covariance Matrix
S <- cov(data2)
S

# calaculate the statistical square distances
d <- apply(data2, MARGIN = 1, function(data2) +
             t(data2-cm) %% solve(S) %% (data2-cm))
d

# construct a chi-square plot of the ordered distances
par(mfrow = c(1,1))
plot(qchisq((1:nrow(data2)-1/2)/nrow(data2),df=2), sort(d),
     xlab = expression(paste(chi[2]^2, "Quantile")),
     ylab = "Ordered Distances",
     main = "Chi-square Plot"); abline(a=0, b=1)

#Multivariate Norm Test
x<-c(8.8, 8.5, 7.7, 4.9, 9.6, 10, 11.5, 11.6, 11.2, 10.7, 10, 6.8, 2589, 1186, 291,1276, 6633, 12125,
     36717, 43319, 10530, 3931, 1536, 61400)
data<-matrix(x, nrow=12, ncol=2)
data

#Uji normalitas ganda menggunakan Mardia's Skewness and kurtosis
library(MVN)

mardia<-mvn(data, mvnTest = c("mardia"), covariance = TRUE, multivariatePlot = "qq")
mardia
henze<-mvn(data, mvnTest = c("hz"), covariance = TRUE, multivariatePlot = "none")
henze
royston<-mvn(data, mvnTest = c("royston"), covariance = TRUE, multivariatePlot = "persp")
royston
royston<-mvn(data, mvnTest = c("royston"), covariance = TRUE, multivariatePlot = "contour")
royston


x1 <- c(108.28,152.36,95.04,65.45,62.97,263.99,265.19,285.06,
        92.01,165.68,17.05,16.59,10.91,14.14,9.52,25.33,18.54,15.73,
        8.1,11.13)
data1 <-matrix(x1, nrow=10, ncol=2)
data1

mardia1<-mvn(data1, mvnTest = c("mardia"), covariance = TRUE, multivariatePlot = "qq")
mardia1
henze1<-mvn(data1, mvnTest = c("hz"), covariance = TRUE, multivariatePlot = "none")
henze1
royston1<-mvn(data1, mvnTest = c("royston"), covariance = TRUE, multivariatePlot = "persp")
royston1
royston1<-mvn(data1, mvnTest = c("royston"), covariance = TRUE, multivariatePlot = "contour")

