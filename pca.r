# -----------------------------------------------------------
    # Author = Effie
    # Date = August 2016
    # Principle Component Analysis
# -----------------------------------------------------------

library(igraph)
library(ggbiplot)
library(ggfortify)

# for bipartite ecosystem of investors and companies
# -----------------------------------------------------------
adj <- get.adjacency(g)
adj <- as.data.frame(adj)

# exclud columns with 0 variance
adj <- adj[,apply(adj,2,var,na.rm=T) !=0]

# PCA
pca <- prcomp(adj, center = T, scale. = T)
print(pca)

# screeplot
png(file="pca_scree.png",width=400, height=600)
screeplot(pca, type="l", ylim=c(0,50),
          main="Variance Explained by Top 10 Principle Component")
dev.off()

summary(pca)

# biplots
png(file="pca_biplot.png",width=600,height=600)
#biplot(pca, scale=0)
ggbiplot(pca, groups = V(g)$type)
dev.off()

png(file="pca_biplot_wolabels.png",width=600,height=600)
autoplot(pca)
dev.off()


# for unipartite investors
# -----------------------------------------------------------
adj_i <- get.adjacency(investor_g)
adj_i <- adj_i[,apply(adj_i,2,var,na.rm=T) !=0]
pca_i <- prcomp(adj_i,scale. = T)

png(file="pca_i_scree.png", width=500, height=750)
screeplot(pca_i, type="l", 
          ylim=c(0,30), 
          main="Variance Explained by Top 10 Principle Component")
dev.off()

png(file="pca_i_biplot.png",width=600, height=600)
ggbiplot(pca_i)
dev.off()


plot(pca_i$x[, 1], pca_i$x[, 2], main = "PCA", xlab = "PC1", ylab = "PC2")


# for unipartite companies
# -----------------------------------------------------------
adj_c <- adj_c[,apply(adj_c,2,var,na.rm=T) !=0]
pca_c <- prcomp(adj_c,center = T, scale. = T)

png(file="pca_c_scree.png", width=600, height=900)
screeplot(pca_c, type="l")
dev.off()

png(file="pca_c_biplot.png",width=600, height=600)
ggbiplot(pca_c)
dev.off()

x <- pca_i$sdev
plot(x)
