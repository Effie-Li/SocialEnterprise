# -----------------------------------------------------------
    # Author = Effie
    # Date = July 2016
    # network visualization
# -----------------------------------------------------------

library(igraph)
library(plyr)
library(ggplot2)


# -----------------------------------------------------------
    # plot bipartite network
# -----------------------------------------------------------
g <- graph.data.frame(edgelist, directed = T, vertices = nodelist)

# set colors and shapes
V(g)[V(g)$type=="company"]$color <- "lightsalmon"
V(g)[V(g)$type=="investor"]$color <- "skyblue3"
V(g)[V(g)$isGroundTruth==1]$color <- "red"
V(g)[V(g)$GT_social==1]$color <- "red"
V(g)[V(g)$GT_non_social==1]$color <- "black"
V(g)[V(g)$government==1]$color <- "royalblue"
V(g)[V(g)$type=="company"]$shape <- "circle"
V(g)[V(g)$type=="investor"]$shape <- "square"

png(file="ecosystem.png", width=20000,height=5000)
plot.igraph(g,
            layout = layout.bipartite(g, types=V(g)$type=="company"),
            vertex.label=NA,
            #vertex.label=ifelse(degree(g)>=40, V(g)$name,NA),
            #vertex.label.cex=10,
            #vertex.label.dist = 0.3,
            #vertex.label.color = "black",
            vertex.color = V(g)$color,
            vertex.shape = "circle",
            #vertex.frame.color = "white",
            asp = 0.25,
            vertex.size=1.5,
            edge.width = 2,
            edge.color= "black",
            edge.arrow.size=0.2,
            edge.color="grey12",
            edge.curved=F)
dev.off()

# degree distribution
da <- degree(g,mode="total")
png(file="degrees_all.png", width=800, height=400)
qplot(da, 
      geom = "histogram",
      main = "Degree Distribution of Investment Network",
      xlab = "degree",
      binwidth = 1)+
    scale_x_continuous(breaks = round(seq(min(da), max(da), by = 5),1))
dev.off()


# -----------------------------------------------------------
    # project bipartite to unipartites
# -----------------------------------------------------------
bipartite.projection.size(g, types=V(g)$type=="company")
unipartites <- bipartite.projection(g, types = V(g)$type=="company",multiplicity = T)
investor_g <- unipartites[[1]]
company_g <- unipartites[[2]]


# plot investors network
# -----------------------------------------------------------
bigI <- NA
x <- as.character(V(investor_g)$name[degree(investor_g)>=40])
for(i in 1:length(x)) {
    index <- x[i]
    index <- paste(index,getInvestorName(index),sep=" - ")
    if(is.na(bigI))
        bigI <- index
    else
        bigI <- c(bigI,index)
}

png(filename="testI_new.png",width=15000,height=10000)
plot.igraph(investor_g,
            vertex.label=ifelse(degree(investor_g)>=40, V(investor_g)$name,NA),
            vertex.label.cex = 20,
            vertex.label.dist = 0.3,
            vertex.label.color = "black",
            vertex.size=2.5,
            vertex.shape="circle",
            edge.color = "black",
            edge.width=2,
            asp=0.92,
            layout=layout.fruchterman.reingold)
dev.off()

legend(locator(1),legend=bigI, bty = 'n', y.intersp = 0.25)

# degree distribution of investors network
di <- degree(investor_g)
png(file="degrees_i.png", width=800, height=400)
qplot(di, 
      geom = "histogram",
      main = "Degree Distribution of Investors Network",
      xlab = "degree",
      binwidth = 1)+
    scale_x_continuous(breaks = round(seq(min(di), max(di), by = 10),1))
dev.off()


# plot companies network
# -----------------------------------------------------------

# assign colors to different themes for companies
for (i in 1:400) {
    theme <- V(g)$theme[i]
    subtheme <- V(g)$sub_theme[i]
    if (theme=="Greentech")
        V(g)$color[i] <- "chartreuse"
    else if (theme=="Finance")
        V(g)$color[i] <- "lightpink1"
    else if (theme=="Sustainable Environment")
        V(g)$color[i] <- "seagreen4"
    else if (theme=="Sustainable Products")
        V(g)$color[i] <- "lightpink4"
    else if (subtheme=="Health")
        V(g)$color[i] <- "lightskyblue"
    else if (subtheme=="Education")
        V(g)$color[i] <- "royalblue"
    else
        V(g)$color[i] <- "gray66"
}

png(filename="testC.png", width=15000,height=10000)
plot.igraph(company_g,
            vertex.label=NA,
            vertex.color=V(g)$color,
            vertex.frame.color="black",
            vertex.size=2.5,
            edge.color = "black",
            edge.width=2,
            asp=0.95,
            layout=layout.fruchterman.reingold)
dev.off()

legend("bottomright",
       legend=c("Greentech",
                "Sustainable Environment",
                "Health",
                "Education",
                "Finance",
                "Sustainable Products"),
       fill=c("chartreuse","seagreen4","lightskyblue","royalblue","lightpink1","lightpink4"),
       #y.intersp = 0.5,
       #cex=0.4,
       #pt.cex=1,
       bty='n')

# degree distribution
dc <- degree(company_g)
png(file="degrees_c.png", width=800, height=400)
qplot(dc, 
      geom = "histogram",
      main = "Degree Distribution of Companies Network",
      xlab = "degree",
      binwidth = 1)+
    scale_x_continuous(breaks = round(seq(min(dc), max(dc), by = 5),1))
dev.off()