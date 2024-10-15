source("~/surfdrive/LogitMDA/clmda/Rcode/readISSPdata.R") 
rm(EE)
# land = 11
library(parallel)
library(lmap)

# check files
# library(fmdu)
# library(Rfast)
# setwd("~/surfdrive/LogitMDA/lmap-package/new/R/")
# myfiles = list.files()
# sapply(myfiles, source)


# myfun1 = function(X, E, S){
#   # unsupervised
#   set.seed(X)
#   out = clmdu(Y = E, S = S, trace = FALSE, start = "random")
#   return(out)
# }

myfun2 = function(X, E, XX, S){
  # supervised
  # X is number of repetitions
  # E = responses, XX = prdictors, S = dimensionality
  if(X %% 5 == 0) system(paste("echo 'now processing:", X, "'"))
  set.seed(X)
  out = clmdu(Y = E, X = XX, S = S, trace = FALSE, start = "random")
  return(out)
}


# select only one country (11 = SL)
# id = which(X[ , land] == 1)
id = which(rowSums(X[, 1:12]) == 0) # selects Thailand
# id = sample(nrow(X), 2000)
X = X[id, ]; Y = Y[id, ]

# select gender, educations, age, EE, EC
X = X[, c(13, 14, 17, 18, 19)]
# X = X[, c(13, 14, 17, 18)]
# X = X[, c(14, 17, 18, 19)]

colnames(Y) = c("OUT", "MEAT", "RECYCLE", "AVOID")


# reduced rank model
out1i = clpca(Y = Y, X = X, S = 1, trace = TRUE)
out2i = clpca(Y = Y, X = X, S = 2, trace = TRUE)
out3i = clpca(Y = Y, X = X, S = 3, trace = TRUE)

cbind(c(out1i$deviance, out2i$deviance, out3i$deviance),
c(out1i$AIC, out2i$AIC, out3i$AIC),
c(out1i$BIC, out2i$BIC, out3i$BIC))

# distance model
# debugSource("~/surfdrive/LogitMDA/lmap-package/new/R/clmdu.R")

out1d.a = clmdu(Y = Y, X = X, S = 1, start = "svd", trace = TRUE)
out2d.a = clmdu(Y = Y, X = X, S = 2, start = "svd", trace = TRUE)
out3d.a = clmdu(Y = Y, X = X, S = 3, start = "svd", trace = TRUE)

cbind(c(out1d.a$deviance, out2d.a$deviance, out3d.a$deviance),
c(out1d.a$AIC, out2d.a$AIC, out3d.a$AIC),
c(out1d.a$BIC, out2d.a$BIC, out3d.a$BIC))


out2d.b = clmdu(Y = Y, X = X, S = 2, start = list(B = out2i$B, V = out2i$V), trace = TRUE)
out2d = ifelse(out2d.a$deviance < out2d.b$deviance, out2d.a, out2d.b)
out2i$deviance

# 100 random starts - does not really improve the results
mcres2 = mclapply(X = c(1:100), myfun2, E = Y, XX = X, S = 2, mc.cores = 7) 
idmin = which.min(sapply(mcres2, function(l)l$deviance))
if(mcres2[[idmin]]$deviance < out2d$deviance){out2d = mcres2[[idmin]]}

summary(out2d)

source("~/surfdrive/LogitMDA/lmap-package/new/R/plot.clmdu.R")
source("~/surfdrive/LogitMDA/lmap-package/new/R/plot.clpca.R")
source("~/surfdrive/LogitMDA/lmap-package/new/R/makedfsforX.R")

plt1 = plot(out2d.a, circles = NULL)
plt2 = plot(out2i)

ggsave("~/surfdrive/LogitMDA/clmda/Rcode/issp_peb_clrmdu.pdf", plot = plt1, width = 11.7, height = 8.3, units = "in", limitsize = FALSE)
ggsave("~/surfdrive/LogitMDA/clmda/Rcode/issp_peb_clrrr.pdf", plot = plt2, width = 11.7, height = 8.3, units = "in", limitsize = FALSE)

plt1.1 = plot(out2d.a, circles = 1)
plt1.2 = plot(out2d.a, circles = 2)
plt1.3 = plot(out2d.a, circles = 3)
plt1.4 = plot(out2d.a, circles = 4)

# ggsave("~/surfdrive/LogitMDA/clmda/Rcode/issp_peb_clrmdu1.pdf", plot = plt1.1, width = 11.7, height = 8.3, units = "in", limitsize = FALSE)
# ggsave("~/surfdrive/LogitMDA/clmda/Rcode/issp_peb_clrmdu2.pdf", plot = plt1.2, width = 11.7, height = 8.3, units = "in", limitsize = FALSE)
# ggsave("~/surfdrive/LogitMDA/clmda/Rcode/issp_peb_clrmdu3.pdf", plot = plt1.3, width = 11.7, height = 8.3, units = "in", limitsize = FALSE)
# ggsave("~/surfdrive/LogitMDA/clmda/Rcode/issp_peb_clrmdu4.pdf", plot = plt1.4, width = 11.7, height = 8.3, units = "in", limitsize = FALSE)

plt1.all = ggarrange(plt1.1, plt1.2, plt1.3, plt1.4, nrow = 2, ncol = 2)
ggsave("~/surfdrive/LogitMDA/clmda/Rcode/issp_peb_clrmdu_all.pdf", plot = plt1.all, width = 11.7, height = 11.7, units = "in", limitsize = FALSE)
