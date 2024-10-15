load("~/surfdrive/LogitMDA/clmda/Rcode/clmdu_issp_out.RData")
rm(list=ls()[! ls() %in% c("res1", "res2", "res3", "X")])

Y = res2$Y
colnames(Y) = ynames = c("too difficult", "do right", 
                         "more important", "others", 
                         "exaggerated", "hard to know", "effect")

# XX = X[, c(1:12, 14, 17)]
# colnames(XX)[13:14] = c("Edu", "Age")
# B0 = res2$B
# V0 = res2$V
# B0 = rbind(B0, matrix(0, 2, 2))

XX = X[, c(1:14, 17)]
colnames(XX)[13:15] = c("Fem", "Edu", "Age")

# 1 dimensional 

B0 = res1$B
V0 = res1$V
B0 = rbind(B0, matrix(0, 3, 1))
out1 = clmdu(Y = Y, X = XX, S = 1, start = list(B = B0, V = V0), trace = TRUE)

# 2 dimensional
B0 = res2$B
V0 = res2$V
B0 = rbind(B0, matrix(0, 3, 2))
out2 = clmdu(Y = Y, X = XX, start = list(B = B0, V = V0), trace = TRUE)

# 3 dimensional
B0 = res3$B
V0 = res3$V
B0 = rbind(B0, matrix(0, 3, 3))
out3 = clmdu(Y = Y, X = XX, S = 3, start = list(B = B0, V = V0), trace = TRUE)

# analysis of fit
fit = matrix(c(out1$deviance, 
  out2$deviance,
  out3$deviance,
  out1$npar, out2$npar, out3$npar,
  out1$AIC,
  out2$AIC,
  out3$AIC,
  out1$BIC,
  out2$BIC,
  out3$BIC), 3, 4)
colnames(fit) = c("deviance", "npar", "aic", "bic")
rownames(fit) = c("1dim", "2dim", "3dim")
round(fit, digits = 2)

plt0 = plot(out2, circles = NULL)
plt1 = plot(out2, circles = 1)
plt2 = plot(out2, circles = 2)
plt3 = plot(out2, circles = 3)
plt4= plot(out2, circles = 4)
plt5 = plot(out2, circles = 5)
plt6 = plot(out2, circles = 6)
plt7 = plot(out2, circles = 7)

ggsave("~/surfdrive/LogitMDA/clmda/Rcode/issp_ee_clrmdu_0.pdf", plot = plt0, width = 11.7, height = 11.7, units = "in", limitsize = FALSE)
ggsave("~/surfdrive/LogitMDA/clmda/Rcode/issp_ee_clrmdu_4.pdf", plot = plt4, width = 11.7, height = 11.7, units = "in", limitsize = FALSE)
ggsave("~/surfdrive/LogitMDA/clmda/Rcode/issp_ee_clrmdu_5.pdf", plot = plt5, width = 11.7, height = 11.7, units = "in", limitsize = FALSE)
ggsave("~/surfdrive/LogitMDA/clmda/Rcode/issp_ee_clrmdu_7.pdf", plot = plt7, width = 11.7, height = 11.7, units = "in", limitsize = FALSE)

