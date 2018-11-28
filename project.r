rm(list=ls(all=TRUE))
#install.packages("haven")
library(haven)
library(ggplot2)
yourData = read_dta("guns.dta")
yourData
summary(yourData)
write.csv(yourData, file = "yourStataFile.csv")
?ggplot
