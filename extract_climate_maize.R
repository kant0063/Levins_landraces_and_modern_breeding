#Extract climate data for maize

library(dismo)
library(raster)
library(maptools)
library(rasterVis)
data(wrld_simpl)

soja<-read.csv("G:/My Drive/Levins and Starvation/Concave_convex_fitness_sets/Data/all_data_together/environmental_data_sites.csv",header=T)

head(soja)
soja <- soja[,2:3]


plot(wrld_simpl,  axes=TRUE, col="light yellow") #xlim=c(90,110), ylim=c(-20,60),
# restore the box around the
box()
#plot points
points(soja$Lon, soja$Lat, col='orange', pch=20, cex=0.75)

#current climate

files <- list.files(path=("F:/wc2.0_30s_bio - Copy"), pattern='tif', full.names=TRUE)
predictors <- stack(files)

presvals <- extract(predictors, soja)


####PCA for environment

hfa<-read.csv("C:/Users/Michael/Downloads/data_for_home_field_pca_V2.csv",row.names = 1)

head(hfa)

hfa2<-hfa[,-c(1:3)]

stan_hfa<-scale(hfa2)

head(stan_hfa)

out<-prcomp(stan_hfa)

summary(out)

plot(out)
biplot(out)

#extract first two principle components and place in another object for ploting
comp <- data.frame(out$x[,1:2])

comp<-cbind(comp, hfa$Region)
names(comp)[3]<-"Region"

library(ggplot2)
# Basic scatter plot
ggplot(comp, aes(x=PC1, y=PC2, color=hfa$Region)) + geom_point() +
  geom_text(label=rownames(comp))


#getting the convex hull of each unique point set

df <- data.frame(result$ind$coord[,1:5])

#get cluster assignment
df$cluster<-as.factor(res.hpc2$data.clust$clust)

str(df)

find_hull <- function(comp) comp[chull(comp$PC1, comp$PC2), ]
library(plyr)
hulls2 <- ddply(comp, "Region", find_hull)

#Clustering based on Three Groups makes a lot of sense
plot_of_polygons <- ggplot(data = comp, aes(x = PC1, y = PC2, 
                                          colour=Region, fill = Region)) +
  geom_point() + 
  geom_polygon(data = hulls2, alpha = 0.5) +
  geom_text(label=rownames(comp))+
  theme_bw()+
  labs(x = "PC1 56.5% variance explained ", y = "PC2 14.97% variance explained")

plot_of_polygons 



