library(vegan)

data(iris)
ii <- iris[,c(1:4)]
i.sp<-iris[,c(5)]
library(vegan)
i.dist <- vegdist(ii, method="euclidean")
i.mds <- metaMDS(i.dist, try=50)
plot(i.mds, display="sites",type="n",
     xlab="Axis 1", ylab="Axis 2",
     main="Iris species")
# add color
iris$Species
iris$col<-c(rep("red",50),rep("green",50),rep("blue",50))
points(i.mds, display="sites",
       pch=21, cex=1,
       bg=iris$col
)
text(x=-2.5, y=3,paste("Stress=",round(i.mds$stress,4)))
legend("topright",
       legend=unique(iris$Species),
       pch=21,
       pt.bg=unique(iris$col)
)


# PerMANOVA

i.obs <- ii
i.spp <- data.frame(species=iris[,5])
head(i.obs)
head(i.spp)
d.manova <- adonis2(i.obs ~ species, method = "euclidean", data= i.spp)
d.manova
summary(d.manova)

# Lefcheck code

#install.packages("vegan")
library(vegan)
set.seed(2)
community_matrix=matrix(
  sample(1:100,300,replace=T),nrow=10,
  dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))

example_NMDS=metaMDS(community_matrix, # Our community-by-species matrix
                     k=2) # The number of reduced dimensions

stressplot(example_NMDS)

plot(example_NMDS)

ordiplot(example_NMDS,type="n")
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",cex=1.25,air=0.01)