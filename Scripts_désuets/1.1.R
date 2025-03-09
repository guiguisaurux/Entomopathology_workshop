expc <- read.table("Exp5.1.txt", header = T, row.names = 1)
expc

cc1 <- chisq.test(expc)
cc1$expected
cc1

expb <- read.table("Exp5.2.txt", header = T, row.names = 1)



cc2 <- chisq.test(expb)

cc2$expected

cc2
