install.packages("factoextra")
install.packages("ggcorrplot")
install.packages("openxlsx")
library(factoextra)
library(ggcorrplot)
library(openxlsx)

#Menyiapkan data
data_women_records <- read.xlsx("E:/Asisten APG/APG uas/women_track_records.xlsx")
head(data_women_records)
rownames(data_women_records) <- data_women_records$country
data_women_records <- data_women_records[,-8]

#Eksplorasi dengan menggunakan matrix korelasi
cor_women <- cor(data_women_records)
cor_women
ggcorrplot(cor_women,type="lower",lab = TRUE)

#Menerapkan AKU
pca_women_records <- prcomp(data_women_records,scale.=TRUE,center=TRUE)
summary(pca_women_records)
fviz_screeplot(pca_women_records,geom="line")
pca_women_records$rotation #Vektor ciri
pca_women_records$x #Score value
fviz_pca_ind(pca_women_records,col.ind = "darkred")