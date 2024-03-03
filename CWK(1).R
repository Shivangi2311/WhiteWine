# coursework part 1
library(NbClust)
library(readxl)
wine <- read_excel("C:/Users/laptop/Desktop/ML-CWK/Whitewine_v2.xlsx")


wine <- head(wine[,-13],200)
View(wine)
summary(wine)

str(wine)



#removing the outliers
boxplot(wine, ylab = 'Attributes' , main = 'White Wine') # viewing the the graph


outlier_values <- boxplot.stats(wine$`fixed acidity`)$out
outlier_values   # following are the exact outliers needed to be removed
#8.6  9.8

#removing the outliers in the column of fixed acidity
fixed_acidity <- wine$`fixed acidity`[!wine$`fixed acidity`
                                      %in%  boxplot.stats(wine$`fixed acidity`)$out]
#counting the number of total outliers that are removed
length(wine$`fixed acidity`) - length(fixed_acidity)
boxplot (wine$`fixed acidity`, ylab = 'Attributes', main= 'Fixed Acidity')
boxplot(fixed_acidity, ylab = 'Attributes', main = 'Fixed Acidity')



outlier_values <- boxplot.stats(wine$`volatile acidity`)$out
outlier_values
#removing the outliers in the column of volatile acidity
volatile_acidity <- wine$`volatile acidity`[!wine$`volatile acidity`
                                            %in% outlier_values]
#counting the number of total outliers that are removed
length(wine$`volatile acidity`) - length(volatile_acidity)
boxplot (wine$`volatile acidity`,ylab = 'Attributes', main = 'Volatile Acidity')
boxplot(volatile_acidity,ylab = 'Attributes', main = 'Volatile Acidity')




outlier_values <- boxplot.stats(wine$`citric acid`)$out
outlier_values
#removing the outliers in the column of citric acid
citric_acid <- wine$`citric acid`[!wine$`citric acid`
                                  %in% outlier_values]
#counting the number of total outliers that are removed
length(wine$`citric acid`) - length(citric_acid)
boxplot (wine$`citric acid`,ylab = 'Attributes', main = 'Citric Acid')
boxplot(citric_acid,ylab = 'Attributes', main = 'Citric Acid')




outlier_values <- boxplot.stats(wine$`residual sugar`)$out
outlier_values
#removing the outliers in the column of residual sugar
residual_sugar <- wine$`residual sugar`[!wine$`residual sugar`
                                        %in% outlier_values]
#counting the number of total outliers that are removed
length(wine$`residual sugar`) - length(residual_sugar)
boxplot (wine$`residual sugar`,ylab = 'Attributes', main = 'Residual Sugar')
boxplot(residual_sugar,ylab = 'Attributes', main = 'Residual Sugar')





outlier_values <- boxplot.stats(wine$chlorides)$out
outlier_values
#removing the outliers in the column of chlorides
chlorides_new <- wine$chlorides[!wine$chlorides
                                %in% outlier_values]
#counting the number of total outliers that are removed
length(wine$chlorides) - length(chlorides_new)
boxplot (wine$`chlorides`,ylab = 'Attributes', main = 'Chlorides')
boxplot(chlorides_new ,ylab = 'Attributes', main = 'Chlorides')




outlier_values <- boxplot.stats(wine$`free sulfur dioxide`)$out
outlier_values
#removing the outliers in the column of free sulfur dioxide
free_sulfur_dioxide <- wine$`free sulfur dioxide`[!wine$`free sulfur dioxide`
                                                  %in% outlier_values]
#counting the number of total outliers that are removed
length(wine$`free sulfur dioxide`) - length(free_sulfur_dioxide)
boxplot (wine$`free sulfur dioxide`, ylab = 'Atrributes', main = 'Free Sulfur Dioxide')
boxplot(free_sulfur_dioxide, ylab = 'Atrributes', main = 'Free Sulfur Dioxide')





outlier_values <- boxplot.stats(wine$`total sulfur dioxide`)$out
outlier_values
#removing the outliers in the column of total sulphur dioxide
total_sulfur_dioxide <- wine$`total sulfur dioxide`[!wine$`total sulfur dioxide`
                                                    %in% outlier_values]
#counting the number of total outliers that are removed
length(wine$`total sulfur dioxide`) - length(total_sulfur_dioxide)
boxplot (wine$`total sulfur dioxide`, ylab = 'Attributes', main = 'Total Sulfur Dioxide')
boxplot(total_sulfur_dioxide, ylab = 'Attributes', main = 'Total Sulfur Dioxide')




outlier_values <- boxplot.stats(wine$density)$out
outlier_values
#removing the outliers in the column of density
density_new <- wine$density[!wine$density
                            %in% outlier_values]
#counting the number of total outliers that are removed
length(wine$density) - length(density_new)
boxplot (wine$`density`, ylab = 'Attributes', main = 'Density')
boxplot(density_new, ylab = 'Attributes', main = 'Density')




outlier_values <- boxplot.stats(wine$pH)$out
outlier_values
#removing the outliers in the column of pH
pH_new <- wine$pH[!wine$pH
                  %in% outlier_values]
#counting the number of total outliers that are removed
length(wine$pH) - length(pH_new)
boxplot (wine$`pH`, ylab = 'Attributes', main = 'pH')
boxplot(pH_new, ylab = 'Attributes', main = 'pH')





outlier_values <- boxplot.stats(wine$sulphates)$out
outlier_values
#removing the outliers in the column of sulphates
sulphates_new <- wine$sulphates[!wine$sulphates
                                %in% outlier_values]
#counting the number of total outliers that are removed
length(wine$sulphates) - length(sulphates_new)
boxplot (wine$`sulphates`, ylab = 'Attribute', main = 'Sulphates')
boxplot(sulphates_new, ylab = 'Attribute', main = 'Sulphates')





outlier_values <- boxplot.stats(wine$alcohol)$out
outlier_values
#removing the outliers in the column of alcohol
alcohol_new <- wine$alcohol[!wine$alcohol
                            %in% outlier_values]
#counting the number of total outliers that are removed
length(wine$alcohol) - length(alcohol_new)
boxplot (wine$`alcohol`, ylab = 'Attribute', main = 'Alcohol')
boxplot(alcohol_new, ylab = 'Attribute', main = 'Alcohol')





wine_new <- cbind(fixed_acidity,volatile_acidity,citric_acid,residual_sugar,chlorides_new,free_sulfur_dioxide,total_sulfur_dioxide,density_new,pH_new,sulphates_new,alcohol_new)
num <- wine-wine_new
num #prints number of rows deleted

boxplot(wine_new, ylab = 'Attribute', main = 'New WhiteWine')
View(wine_new)




#Min-Max Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
nWine <- as.data.frame(lapply(wine, normalize))
head(nWine)





nc <- NbClust(wine_new,
              min.nc=2, max.nc=11,
              method="kmeans")

table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),    # provide bar charts####
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Criteria")


wss <- 0
for (i in 1:11){
  wss[i] <-
    sum(kmeans(wine_new, centers=i)$withinss)
}
plot(1:11,
     wss,
     type="b",     ### "b" for both####
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")



bss <- 0
for (i in 1:11){
  bss[i] <-
    sum(kmeans(wine_new, centers=i)$betweenss)
}
plot(1:11,
     bss,
     type="b",     ### "b" for both####
     xlab="Number of Clusters",
     ylab="Between groups sum of squares")


library(factoextra)

#Elbow Method
fviz_nbclust(wine_new, kmeans, method = 'wss')

#Silhouette Method
fviz_nbclust(wine_new, kmeans, method = 'silhouette')

#Gap Method
fviz_nbclust(wine_new, kmeans, method = 'gap_stat')


#Auto Fitting

fit.km <- kmeans(wine_new, 2)

fit.km


wss = fit.km$tot.withinss
bss = fit.km$betweenss
wss
bss

fit.km$centers
fit.km$size


library(fpc)
plotcluster(wine_new, fit.km$cluster)



#Manual K-means
#k = 2
x = nWine
y = wine$quality
kc <- kmeans(x,2)
kc
kc$centers
kc$withinss #wss
kc$betweenss #bss
kc$totss #tss
table(y,kc$cluster)
confuseTable.km <- table(nWine$quality, fit.km$cluster)
confuseTable.km


#k=3
kc <- kmeans(x,3)
kc
kc$centers
kc$withinss #wss
kc$betweenss #bss
kc$totss #tss
confuseTable.km <- table(nWine$quality, fit.km$cluster)
confuseTable.km


#k=4
kc <- kmeans(x,4)
kc
kc$centers
kc$withinss #wss
kc$betweenss #bss
kc$totss #tss
confuseTable.km <- table(nWine$quality, fit.km$cluster)
confuseTable.km






#Fit the model 
set.seed(25)
fit.km <- kmeans(wine_new, 4)
fit.km
fit.km$centers
#or
fit.km$size
library (fpc)
plotcluster(wine_new, fit.km$cluster)






pca_result <- prcomp(wine_new, scale = FALSE)
pca_result
summary(pca_result)
names(pca_result)

pca_result$center
pca_result$scale
pca_result$rotation

pca_result$rotation <- -pca_result$rotation
pca_result$rotation
pca_result$x <- - pca_result$x
head(pca_result$x)

pca_result$sdev

(VE <- pca_result$sdev^2)
PVE <- VE / sum(VE)
round(PVE, 3)


varPercent <- PVE*100
barplot(varPercent, xlab='PC', ylab='Percent Variance', names.arg=1:length(varPercent), las=1, ylim=c(0, max(varPercent)), col='gray')
abline(h=1/ncol(USArrests)*100, col='red')

pca_wines = as.data.frame(pca_result$x[,1:4])
pca_wines

library(factoextra)

fviz_nbclust(pca_wines, kmeans, method = 'wss')
fviz_nbclust(pca_wines, kmeans, method = 'silhouette')
fviz_nbclust(pca_wines, kmeans, method = 'gap_stat')

k = 4
kmeans_wines = kmeans(pca_wines, centers = 4, nstart = 15)
fviz_cluster(kmeans_wines, data = pca_wines)

fit.km <- kmeans(pca_wines, 3)
fit.km

wss = fit.km$tot.withinss
bss = fit.km$centre
wss
bss

