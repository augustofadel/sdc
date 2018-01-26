library(sdcMicro)

# load test data
data("testdata", package = "sdcMicro")

# define an object of class 'sdcMicroObj'
sdc <- createSdcObj(
   testdata,
   keyVars = c("urbrur", "water", "sex", "age"),
   numVars = c("expend", "income", "savings"), 
   pramVars = "walls",
   w = "sampling_weight", 
   hhId = "ori_hid"
)

slotNames(sdc)

# slots:
# origData contains the original data
# keyVars the index of categorical key variables
# pramVars contains an index indicating variables that are pramed
# slot numVars specifies indices of continuous key variables
# weightVar contains the vector defining sampling weights
# hhId index determining the cluster variable
# strataVar contains the stratification variable 
# sensibleVar contains the sensible variables
# manipulated variables are saved in the slots beginning with manip
# originalRisk contains risk measures for the original unmodified data
# risk contains risk measures for the manipulated data
# utility collects all information on data utility
# pram and localSuppression contains further information on pramed variables and local suppressions
# additionalResults contains additional results (e.g., self-defined utility measures)
# prev contains previous results

sdc

# folowing lines are equivalent
microaggregation(sdc)
microaggregation(testdata[, c("expend", "income", "savings")])
microaggregation(
   testdata[, c("expend", "income", "savings")], 
   variables = NULL, 
   aggr = 3, 
   strata_variables = NULL,
   method = "mdav", 
   weights = NULL, 
   nc = 8, 
   clustermethod = "clara",
   measure = "mean", 
   trim = 0, 
   varsort = 1, 
   transf = "log"
)



# available methods:
# pca
# rmd
# onedims
# single
# simple
# clustpca
# pppca
# clustpppca
# mdav
# clustmcdpca
# influence
# mcdpca





# microagg_brkga()

dat <- testdata %>% select(expend, income, savings)
sol <- microagg_brkga(dat, 3, 50, 50, metricas = "IL2")
clus <- sol[[1]][,1]
dat_util <- fit(dat %>% as.data.table(), clus, "IL2")

dat <- Tarragona
sol <- microagg_brkga(dat, 3, 50, 50, metricas = "IL2")
clus <- sol[[1]][,1]
dat_util <- fit(dat %>% as.data.table(), clus, "IL2")
