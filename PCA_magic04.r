library(scatterplot3d)
library(treemap)

magic.data <- read.csv('magic04.data')

magic.pca <- princomp(magic.data[-11], cor = TRUE)

summary(magic.pca)
screeplot(magic.pca, main="Proportion of Variance")

vars <- magic.pca$sdev^2 
vars <- vars/sum(vars) 
treemap.data <- data.frame(vars, "Component")
colnames(treemap.data) <- c("sdev")
treemap(treemap.data, "sdev", "sdev", title="Proportion of Variance")

colors <- magic.data[11] == 'h'
colors[colors] <- 'blue'
colors[colors == FALSE] <- 'red'

magic.reduced <- cbind(magic.pca$scores[,1:3], colors)

scatterplot3d(magic.reduced[,1], magic.reduced[,2], 
              magic.reduced[,3], magic.reduced[,4], 
              sub = "MAGIC Gamma Telescope Data Set", 
              xlab="Component #1",
              ylab = "Component #2", 
              zlab = "Component #3", 
              main="MAGIC Gamma Telescope Data Set reduced",
              angle = 210, pch=4)

legend("topleft", inset=0, bty="n", title="legend",
       c("gamma (signal)", "hadron (background)"), fill=c("red", "blue"))