rm(list=ls(all=TRUE))
#install.packages("haven")
library(haven)
library(ggplot2)
guns = read_dta("guns.dta")
guns
summary(guns)
#write.csv(yourData, file = "yourStataFile.csv")
?ggplot
install.packages("NbClust")
install.packages("randomForest")
install.packages("tidyverse")
install.packages("maps")
install.packages("ggplot2")
install.packages("rworldmap")
library(rworldmap)
library(ggplot2)
library(maps)
library(tidyverse)
library(NbClust)
library(haven)
library(randomForest)



########################################################################################

set.seed(40)
#setwd("/Users/gabriel/desktop/econometrics")
#guns <- read_dta("~/Desktop/Econometrics/guns.dta")
###Important Findings 
table(guns$year, guns$shall) 
#Should breakup between 77-81, 82-85, 86-89, 90-94, 95-99

sum(is.na(guns)) #No empty files
names(guns)
summary(guns)
### Graphs, Plotting. Can't run Histograms
plot(guns$stateid, guns$density)
head(guns$stateid)
unique(guns$stateid)
ggplot2::ggplot(data = guns) + geom_point(mapping = aes(x=year, y = mur, size = density)) ##Murder happening in more dense pop.
ggplot2::ggplot(data = guns) + geom_point(mapping = aes(x=year, y = rob, size = density)) ##Robberies happening in more dense pop.
ggplot2::ggplot(data = guns) + geom_point(mapping = aes(x=year, y = vio, size = density)) ##Violence happening in more dense pop.

ggplot2::ggplot(data = guns) + geom_point(mapping = aes(x=year, y = mur, size = incarc_rate)) 
ggplot2::ggplot(data = guns) + geom_smooth(mapping = aes(x=year, y = mur))  
ggplot2::ggplot(data = guns) + geom_bar( mapping = aes(x=year, y= shall.1))

shall.1 <- guns$shall == "1"

#Remove StateID, Shall, and Year. Only have total cumulative data for all states/years
giy <- guns
giy$stateid <- NULL
giy$year <- NULL
giy$shall <- NULL
year77 <- filter(guns, year == "77") #Break apart Year and analyze data
sum(year77$shall) #Total No. of Shall States
table(guns$year, guns$shall) #Super Important
table(year77$stateid, year77$shall) ## Which state has shall in 1977



head(giy)
colnames(giy)
giy.df <- sapply(giy, scale) #Scale for Distance
guns.df <- sapply(guns, scale)
nc.a <- NbClust(giy.df, distance="euclidean", min.nc=2, max.nc=15, method="average")
nc.a #Best No. of Cluster is 2 

nc.k <- NbClust(giy.df, min.nc=2, max.nc=15, method="kmeans")
nc.k #Best No. of Cluster is 3


fit.giy <- kmeans(giy.df, 2, nstart = 25)
fit.giy

####### Attempt to analyze data solely by year, no StateID/Shall
guns.year <- guns
guns.year <- data.frame(guns.year)
guns.year$stateid <- NULL
guns.year$shall <- NULL

g.avg <- average()


### Hierarchical Mess. Replace Method with SIngle/complete/average
d <- dist(guns.df)
fit.average <- hclust(d, method = "single")
plot(fit.average, hang = -1, cex=0.8, main="average linkage clustering")


#Running Decision Tree

#Before and After Shall law comparison 

shall.0 <- filter(gun., shall == "0") #States w/o Shall
shall.0.mur.avg <- mean(shall.0$mur)
shall.0.mur.vio <- mean(shall.0$vio)
shall.0.mur.rob <- mean(shall.0$rob)

shall.1 <- filter(gun., shall == "1") #States w/o Shall
shall.1.mur.avg <- mean(shall.0$mur)
shall.1.mur.vio <- mean(shall.0$vio)
shall.1.mur.rob <- mean(shall.0$rob)

###### Mapping Attempt ~ Failed 

all_states <- map_data("state")
all_states
head(all_states)
guns$state <- guns$stateid
Total <- merge(all_states, guns, all=TRUE)
head(Total)

ggplot2::geom_polygon()

p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$incarc_rate),colour="white"
) + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")

P1 <- p + theme_bw()  + labs(fill = "Black to White Incarceration Rates \n Weighted by Relative Population" 
                             ,title = "State Incarceration Rates by Race, 2010", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
p


library(rworldmap)
library(ggplot2)
map_data

#Add the data you want to map countries by to map.world
#In this example, I add lengths of country names plus some offset
map.world$name_len <- nchar(map.world$region) + sample(nrow(map.world))

gg <- ggplot()
gg <- gg + theme(legend.position="none")
gg <- gg + geom_map(data=map_data() + aes(map_id=region, x=long, y=lat, fill=name_len))

gg <- gg + scale_fill_gradient(low = "green", high = "brown3", guide = "colourbar")
gg <- gg + coord_equal()
gg
