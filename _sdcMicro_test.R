library(sdcMicro)

# load test data
data("testdata", package = "sdcMicro")

# define an object of class 'sdcMicroObj'
sdc <- createSdcObj(
   testdata,                                       # numeric matrix or data frame containing the data 
   keyVars = c("urbrur", "water", "sex", "age"),   # categorical key variables
   numVars = c("expend", "income", "savings"),     # continuous key variables
   pramVars = "walls",                             # categorical variables considered to be pramed
   w = "sampling_weight",                          # vector of sampling weights
   hhId = "ori_hid"                                # cluster ID
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
   testdata[, c("expend", "income", "savings")],  # object of class sdcMicroObj-class or a data.frame
   variables = NULL,                              # variables to microaggregate (for data.frames, all columns are chosen per default)
   aggr = 3,                                      # aggregation level
   strata_variables = NULL,                       # by-variables for applying microaggregation only within strata defined by the variables
   method = "mdav",                               # pca, rmd, onedims, single, simple, clustpca, pppca, clustpppca, mdav, clustmcdpca, influence, mcdpca
   weights = NULL,                                # sampling weights
   nc = 8,                                        # number of cluster, if the chosen method performs cluster analysis
   clustermethod = "clara",                       # clustermethod
   measure = "mean",                              # aggregation statistic: mean, median, trim, onestep
   trim = 0,                                      # trimming percentage, if measure=trim
   varsort = 1,                                   # variable for sorting, if method=single
   transf = "log"                                 # transformation for data x
)





# disclosure risk:
sdc <- microaggregation(sdc)
# sample and population frequencies
get.sdcMicroObj(sdc, type = "risk")$individual %>% head()
freqCalc(
   x = testdata,
   keyVars = c("urbrur", "water", "sex", "age"),
   w = "sampling_weight"
)
# k-anonymity
print(sdc)
# l-diversity
#ldiversity(sdc)
res1 <- ldiversity(testdata, keyVars = c("urbrur", "water", "sex", "age"), ldiv_index = "income")
print(res1)
# DIS SUDA score
# SUDA2 (ignora peso amostral)
disScore()
suda2(sdc)
get.sdcMicroObj(sdc, type = "risk")$suda
#risk$suda2$score
#risk$suda2$disScore
#risk$suda2$contributionPercent
# insdividual and cluster risk approach (considera pesoa mostral)
get.sdcMicroObj(sdc, type = "risk")$individual
# global risk
print(sdc, "risk")
# global risk w/ log-linear models
sdc <- LLmodGlobalRisk(sdc, form = ~urbrur + water + sex + age)
# 'LLmodGlobalRiskX' is deprecated. Use 'modRisk' instead.
get.sdcMicroObj(sdc, "risk")$model$gr1






# microagg_brkga()

dat <- testdata %>% select(expend, income, savings)
sol <- microagg_brkga(dat, 3, 50, 50, metricas = "IL2")
clus <- sol[[1]][,1]
dat_util <- fit(dat %>% as.data.table(), clus, "IL2")

dat <- Tarragona
sol <- microagg_brkga(dat, 3, 50, 50, metricas = "IL2")
clus <- sol[[1]][,1]
dat_util <- fit(dat %>% as.data.table(), clus, "IL2")






# calculo IL1

# nao consegui reproduzir o calculo feito pelo pacote, armazenado no slot 'utility$il1'
# a expressao que mais se aproxima é 
# abs(dat - dat.agreg) %>% 
#    `/`(2 * Sj) %>% 
#    apply(2, sum) %>% 
#    sum() %>% 
#    `/`(prod(dim(dat)))

sdc <- createSdcObj(
   testdata,                                       # numeric matrix or data frame containing the data 
   keyVars = c("urbrur", "water", "sex", "age"),   # categorical key variables
   numVars = c("expend", "income", "savings"),     # continuous key variables
   pramVars = "walls",                             # categorical variables considered to be pramed
   w = "sampling_weight",                          # vector of sampling weights
   hhId = "ori_hid"                                # cluster ID
)
sdc <- microaggregation(sdc, aggr = 3, method = 'mdav')

dat <- testdata[, c("expend", "income", "savings")]
dat.agreg <- sdc@manipNumVars

IL2(as.data.table(dat), as.data.table(dat.agreg))
sdc@utility$il1
dUtility(dat, xm = dat.agreg, method = "IL1") # armazenado no slot utility


# dUtilityWORK(..., method = "IL1")
x <- dat
xm <- dat.agreg

a <- x
for (i in 1:dim(x)[2]) {
   a[[i]] <- abs(x[[i]] - xm[[i]])/(sd(x[[i]], na.rm = TRUE) * sqrt(2))
}
infLoss1 <- 1/(dim(x)[2] * dim(x)[1]) * sum(a, na.rm = TRUE)



Sj <- apply(dat, 2, sd)
res <- abs(dat - dat.agreg) 
res <- apply(res, 1, function(x) {x / (sqrt(2) * Sj)})
sum(res) / prod(dim(dat))




