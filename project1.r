 rm(list=ls(all=TRUE))
#install.packages("haven")
library(haven)
library(ggplot2)
guns = read_dta("guns.dta")
guns
summary(guns)
#write.csv(guns, file = "yourStataFile.csv")
?ggplot
head(guns)
str(guns)
for i in 