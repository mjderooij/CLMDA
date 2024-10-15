# first run Psyscale_score_computing
rm(list=ls())
source("~/surfdrive/LogitMDA/clmda/Rcode/Rosa_data/Psyscale_score_computing.R")

# T1 Application 188:197
# T1 Judgement 198:207
# T1 Knowledge 208:217

# T2 Application 220:229
# T2 Judgement 230:239
# T2 Knowledge 240:249

# T3 Application 252:261
# T3 Judgement 262:271
# T3 Knowledge 272:281

# complete cases for T1 assessment
# idxcomplete = complete.cases(Data[ , c(1, 2, 288, 295:297, 299:302, 304:307, 308, 294, 309:311, 188:217)])
# Y = Data[idxcomplete , 188:217]
# X = Data[idxcomplete, c(1, 2, 288, 295:297, 299:302, 304:307, 308, 294, 309:311)]
# ynames = c(paste0("A", 1:10), paste0("J", 1:10), paste0("K", 1:10))

# complete cases for T1/T2 Judgement assessment
# idxcomplete = complete.cases(Data[ , c(1, 2, 288, 295:297, 299:302, 304:307, 308, 294, 309:311, 198:207, 230:239)])
# Y = Data[idxcomplete , c(198:207, 230:239)]
# X = Data[idxcomplete, c(1, 2, 288, 295:297, 299:302, 304:307, 308, 294, 309:311)]
# ynames = c(paste0("J1", c("a","b","c","d","e","f","g","h","i","j")), 
#            paste0("J2", c("a","b","c","d","e","f","g","h","i","j")))

# complete cases for T1/T2 Application assessment
idxcomplete = complete.cases(Data[ , c(1, 2, 288, 295:297, 299:302, 304:307, 308, 294, 309:311, 188:197, 220:229)])
Y = Data[idxcomplete , c(188:197, 220:229)]
X = Data[idxcomplete, c(1, 2, 288, 295:297, 299:302, 304:307, 308, 294, 309:311)]
ynames = c(paste0("A1", c("a","b","c","d","e","f","g","h","i","j")), 
           paste0("A2", c("a","b","c","d","e","f","g","h","i","j")))

Y = as.matrix(Y)
X = as.matrix(X)

# rm(Data, i, idxcomplete, reverse, reverse2)

xnames = c(
  "A", "G", "PMP", "SASe", "SASh", "SASi", 
  "SATSa" , "SATSc", "SATSv", "SATSd",
  "MSLe", "MSLt", "MSLc", "MSLsr",
  "APS", "AMS", 
  "ENGa", "ENGb", "ENGc"
)

# remember gender = 1 corresponds to girls. G = Girls/Gender

colnames(Y) = ynames
colnames(X) = xnames

set.seed(1); idy = sort(sample(20, 10))
Y = Y[ , idy] # selection of 10 items
ynames = ynames[idy]
original_ynames = colnames(Data)[c(188:197, 220:229)[idy]]

# Analysis
out1 = clpca(Y = Y, X = X, S = 1)
out2 = clpca(Y = Y, X = X, S = 2)
out3 = clpca(Y = Y, X = X, S = 3)

fit = cbind(
  c(1,2,3),
  c(out1$npar, out2$npar, out3$npar),
  c(out1$deviance, out2$deviance, out3$deviance),
  c(out1$AIC, out2$AIC, out3$AIC),
  c(out1$BIC, out2$BIC, out3$BIC)
)
colnames(fit) = c("dimensionality", "npar", "deviance", "aic", "bic"); fit

# summary(out2)
# plot(out2)

# variable selection

out1a = clpca(Y = Y, X = X[ , -1], S = 2) # Age
out1b = clpca(Y = Y, X = X[ , -2], S = 2) # Gender
out1c = clpca(Y = Y, X = X[ , -3], S = 2) # Math
out1d = clpca(Y = Y, X = X[ , -c(4,5,6)], S = 2) # Statistical Anxiety
out1e = clpca(Y = Y, X = X[ , -c(7,8,9,10)], S = 2) # SATS
out1f = clpca(Y = Y, X = X[ , -c(11, 12, 13, 14)], S = 2) # MSLQ
out1g = clpca(Y = Y, X = X[ , -15], S = 2) # APS
out1h = clpca(Y = Y, X = X[ , -16], S = 2) # AMS
out1i = clpca(Y = Y, X = X[ , -c(17, 18, 19)], S = 2) # ENG

# comparison of fit of the 9 models
fit2 = cbind(
  c(out1a$npar, 
    out1b$npar, 
    out1c$npar,
    out1d$npar, 
    out1e$npar, 
    out1f$npar,
    out1g$npar, 
    out1h$npar, 
    out1i$npar),
  c(out1a$deviance, 
    out1b$deviance, 
    out1c$deviance,
    out1d$deviance, 
    out1e$deviance, 
    out1f$deviance,
    out1g$deviance, 
    out1h$deviance, 
    out1i$deviance),
  c(out1a$AIC, 
    out1b$AIC, 
    out1c$AIC,
    out1d$AIC, 
    out1e$AIC, 
    out1f$AIC,
    out1g$AIC, 
    out1h$AIC, 
    out1i$AIC),
  c(out1a$BIC, 
    out1b$BIC, 
    out1c$BIC,
    out1d$BIC, 
    out1e$BIC, 
    out1f$BIC,
    out1g$BIC, 
    out1h$BIC, 
    out1i$BIC))

colnames(fit2) = c("npar", "deviance", "aic", "bic")
rownames(fit2) = c("Age","Gender","Math","Statistical Anxiety","SATS","MSLQ","APS","AMS","ENG")
fit2

# leave out all predictors that lower the AIC/BIC
c(out1a$AIC, 
  out1b$AIC, 
  out1c$AIC,
  out1d$AIC, 
  out1e$AIC, 
  out1f$AIC,
  out1g$AIC, 
  out1h$AIC, 
  out1i$AIC) > out2$AIC

idx = c(2, 3, 7, 8, 9, 10, 15, 17, 18, 19)

outfinal = clpca(Y = Y, X = X[ , idx], S = 2)

summary(outfinal)

source("~/surfdrive/LogitMDA/lmap-package/new/R/makedfsforX.R")
source("~/surfdrive/LogitMDA/lmap-package/new/R/plot.clpca.R")

plt = plot(outfinal)

ggsave("~/surfdrive/LogitMDA/clmda/Rcode/Rosa_data/student.pdf", plot = plt, 
       width = 11, height = 11, units = "in", limitsize = FALSE)

M = matrix(unlist(outfinal$m), ncol = 2, byrow = TRUE); 
rownames(M) = ynames; colnames(M) = names(outfinal$m[[1]])
M

rownames(outfinal$B) = xnames[idx]
rownames(outfinal$V) = ynames

outfinal$B
outfinal$V

BV = outfinal$B %*% t(outfinal$V)
xtable::xtable(rbind(t(M), BV))
