# -----------------------------------------------------------
    # Author = Effie
    # Date = July 2016
    # stochastic block models
# -----------------------------------------------------------

library(Matrix)
library(lattice)
library(blockmodels)

# companies
# -----------------------------------------------------------
    # results:
    # bernoulli max: 15 groups, ICL = -6976.192
    # gaussian max: 33 groups, ICL = 217438.3
    # poisson max: 12 groups, ICL = -13746.51
# -----------------------------------------------------------
adj_c <- as.matrix(as_adj(company_g))
model_c <- BM_bernoulli("SBM",adj_c)
model_c$estimate()
x <- which.max(model_c$ICL)

bm_c <- model_c$memberships[[x]]$Z

pal <- colorRampPalette(c("white","brown1"))
png(file="bm_c.png", width=1500,height=500)
levelplot(bm_c,
          main="Optimal Stochastic Block Model for Companies: block size=15",
          col.regions=pal)
dev.off()

temp3 <- BM_bernoulli("SBM_sym",adj_c)
temp3$estimate()
which.max(temp3$ICL) #15 ICL=-4021.163
a <- temp3$plotting_data

temp <- BM_gaussian("SBM",adj_c)
temp$estimate()
x <- which.max(temp$ICL)


#investors
# -----------------------------------------------------------
    # results:
    # bernoulli max: 18 groups, ICL = -17108.64
    # gaussian max: 50 groups(=explore_max), ICL = 540982.8
    # poisson max: 15 groups, ICL = -18803.79
# -----------------------------------------------------------
adj_i <- as.matrix(as_adj(investor_g))
model_i <- BM_bernoulli("SBM",adj_i)
model_i$estimate()
y <- which.max(model_i$ICL)

bm_i <- model_i$memberships[[y]]$Z

png(file="bm_i.png", width=1500,height=500)
levelplot(bm_i,
          main="Optimal Stochastic Block Model for Investors: block size=18",
          col.regions=pal)
dev.off()

temp2 <- BM_poisson("SBM",adj_i)
temp2$estimate()
which.max(temp2$ICL)

temp4 <- BM_gaussian("SBM",adj_i,explore_max=50)
temp4$estimate()
which.max(temp4$ICL)