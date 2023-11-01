#Prep packages
install.packages("psych")
install.packages("ggcorrplot")
install.packages("openxlsx")
library(psych)
library(ggcorrplot)
library(openxlsx)

#Data
data_jobApp <- read.xlsx("E:/Asisten APG/P9 FAKTOR/9.jobApplicants.xlsx")
head(data_jobApp)

#Tahap 1 Ekplorasi data dengan melihat korelasi antar peubah
cor_jobApp <- cor(data_jobApp)
ggcorrplot(cor_jobApp,type="lower",lab = TRUE)

#Tahap 2 Menentukan banyaknya factor
#Menggunakan proportion of the sample variance explained
fa_jobApp <- fa(data_jobApp,nfactors = 12,fm="minres",rotate="none")
#menampilkan proportion of variance explained
fa_jobApp$Vaccounted
#Menggunakan Scree plot
SS_loadings <- fa_jobApp$Vaccounted[1,]
number_of_factor <- seq_along(SS_loadings)
plot(number_of_factor,SS_loadings,type = "b", main = "Scree Plot", pch = 16)

# Tahap 3 Estimasi faktor loading
#Menggunakan 4 faktor
fa_jobApp4 <- fa(data_jobApp,nfactors = 4,fm="minres",rotate="none")
print(fa_jobApp4$loadings,cut = 0)

#Menggunakan 2 faktor
fa_jobApp2 <- fa(data_jobApp,nfactors = 2,fm="minres",rotate="none")
print(fa_jobApp2$loadings,cut = 0)

#Tahap 4 Rotasi Faktor
fa_jobApp4_rotate <- fa(data_jobApp,nfactors = 4,fm="minres",rotate="varimax")
print(fa_jobApp4_rotate$loadings,cut = 0)

fa_jobApp4_rotate$communalities

fa_jobApp2_rotate <- fa(data_jobApp,nfactors = 2,fm="minres",rotate="varimax")
print(fa_jobApp2_rotate$loadings,cut = 0)

fa_jobApp2_rotate$communalities

#Tahap 5 Interpretasi Faktor
#Menggunakan 4 faktor Untuk mempermudah interpretasi, faktor loading yang ditampilkan selain [???0.6, 0.6].
print(fa_jobApp4_rotate$loadings,cut = 0.6)

#Menggunakan 4 faktor
print(fa_jobApp2_rotate$loadings,cut = 0.6)

