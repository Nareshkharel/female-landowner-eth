
setwd("/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/codes")
install.packages("dplyr")  
library("dplyr")  
install.packages("haven")
library("haven")
install.packages("RM.weights")
library("RM.weights")

data <- read_dta("/Users/nareshkharel/Desktop/Thesis/ETH_2021_ESPS-W5_v01_M_Stata_1/Household/sect8_hh_w5.dta")

fies_r21 <- subset(data, select = c(s8q01:s8q08a))


fies_r21[fies_r21>2]=NA

fies_21 <- fies_r21 %>% select(-s8q06a, -s8q07a, -s8q08a)
fies_21[fies_21 == 2] <- 0
RS = rowSums(fies_21)
colnames(fies_21) = c("worried", "healthy", "fewfoods","skipped", "ateless","runout", "hungry", "wholeday" )
table(RS)

fies_21 <- na.omit(fies_21)
res_21 = RM.w(as.matrix(fies_21))

res_21$infit
res_21$res.corr  ## correlation related to Wambogo 2018
res_21$outfit
