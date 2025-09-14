# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# General Linear Hypotheses Use glht (multcomp) With (In) R Software
install.packages("multcomp")
library("multcomp")
glht = read.csv("https://raw.githubusercontent.com/timbulwidodostp/glht/main/glht/glht.csv",sep = ";")
# Estimation General Linear Hypotheses Use glht (multcomp) With (In) R Software
glht$glht_3 <- ordered(glht$glht_3, levels = c("L", "M", "H"))
glht_3 <- glht$glht_3
glht_1 <- aov(glht_1 ~ glht_3, data = glht)
glht_1_result <- glht(glht_1, linfct = mcp(glht_3 = "Tukey"))
glht_1_result
summary(glht_1_result)
glht_2 <- lm(glht_5 ~ glht_6 + glht_7 + glht_8 + glht_9 + glht_10, data = glht)
K <- diag(length(coef(glht_2)))[-1,]
rownames(K) <- names(coef(glht_2))[-1]
K
glht_2_result <- glht(glht_2, linfct = K)
glht_2_result
summary(glht_2_result)
# General Linear Hypotheses Use glht (multcomp) With (In) R Software
# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# Finished