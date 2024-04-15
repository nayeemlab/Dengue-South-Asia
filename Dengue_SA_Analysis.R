library(ggplot2)
library(ggthemes)
library(extrafont)
library(dplyr)
library("FactoMineR")
library("factoextra")
library(extrafont)
library(ggplot2)
library(pastecs)
library(corrplot)
library(ppcor)
library(factoextra)
library(psych)
library(GPArotation)
library(Hmisc)
library(dplyr)
library(ape)
library(psych)
library(psychometric)

options(scipen = 999)
setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue South-Asia')

charts.data <- read.csv("Dengue_SA_data.csv")
charts.data

describe.by(charts.data$CasesPerTh, charts.data$Countries)


p3 <- ggplot() + geom_line(aes(y = log10(CasesPerTh+1), x = Year, colour = Countries), size=1,
                           data = charts.data, stat="identity")+  xlab("Years") + ylab("Dengue cases per thousand (log10)") + 
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size=15),
       legend.position = c(0.10, 0.80),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust=0.5),
        text=element_text(size=15)) +
  scale_x_continuous(breaks=seq(2000,2023,1))

p3

p4 <- ggplot() + geom_line(aes(y =CFR...., x = Year, colour = Countries), size=1,
                           data = charts.data, stat="identity")+  xlab("Years") + ylab("Case Fatality Ratio (%)") + 
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.position = c(0.90, 0.80),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        text=element_text(size=15)) +
  scale_x_continuous(breaks=seq(2000,2023,1))

p4

library(gridExtra)
tiff("CasesCFR_SA4.tiff", units="in", width=8, height=12, res=300)
gridExtra::grid.arrange(p3,p4, nrow=2, ncol=1)
dev.off()



library(dplyr)
library(tidyverse)
sertoData <- read.csv("dengue serotypes.csv")

sertoData$Serotypes <- factor(sertoData$Serotypes,levels=c("DENV-1","DENV-2", "DENV-3","DENV-4"),
                        labels = c("DENV-1","DENV-2", "DENV-3","DENV-4"))

tab <- table(sertoData$Serotypes)
prop.table(tab)*100

# pie Age
df <- data.frame(Serotypes=c("DENV-1","DENV-2","DENV-3","DENV-4"),
                 Count=c(32.37, 28.63, 28.22, 10.79))
head(df)

SA <- ggplot(df, aes(x = "", y = Count, fill = Serotypes)) +
  geom_col(color = "darkgrey") +
  geom_text(aes(label = Count),cex=7,
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  theme_void()+  theme_bw()+
  xlab("") + ylab("") + ggtitle("Dengue Serotypes in South-Asia (2000-2023)")+
  theme(plot.title = element_text(size = 20,hjust=0.5),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20))
SA


# library(gridExtra)
# tiff("Serotypes_SA.tiff", units="in", width=8, height=6, res=300)
# gridExtra::grid.arrange(SA)
# dev.off()
# 
# 
# 
# #Afghanistan
# factor(sertoData$Country)
# sertoDataAfg <- sertoData[which(sertoData$Country=='Afghanistan'), ]
# 
# sertoDataAfg$Serotypes <- factor(sertoDataAfg$Serotypes,levels=c("DENV-1","DENV-2", "DENV-3","DENV-4"),
#                               labels = c("DENV-1","DENV-2", "DENV-3","DENV-4"))
# 
# tab <- table(sertoDataAfg$Serotypes)
# prop.table(tab)*100
# 
# # pie Age
# df <- data.frame(Serotypes=c("DENV-1","DENV-2","DENV-3","DENV-4"),
#                  Count=c(30, 20, 30, 20))
# head(df)
# 
# Afg <- ggplot(df, aes(x = "", y = Count, fill = Serotypes)) +
#   geom_col(color = "black") +
#   geom_text(aes(label = Count),cex=2,
#             position = position_stack(vjust = 0.5)) +
#   coord_polar(theta = "y")+
#   theme_void()+  theme_bw()+
#   xlab("") + ylab("") + ggtitle("Dengue Serotypes in Afghanistan (2000-2023)")+
#   theme(plot.title = element_text(size = 8,hjust=0.5),
#         legend.title = element_text(size=8),
#         legend.text = element_text(size=8))
# Afg
# 
# #Bangladesh
# factor(sertoData$Country)
# sertoDataBng <- sertoData[which(sertoData$Country=='Bangladesh'), ]
# 
# sertoDataBng$Serotypes <- factor(sertoDataBng$Serotypes,levels=c("DENV-1","DENV-2", "DENV-3","DENV-4"),
#                                  labels = c("DENV-1","DENV-2", "DENV-3","DENV-4"))
# 
# tab <- table(sertoDataBng$Serotypes)
# prop.table(tab)*100
# 
# # pie Age
# df <- data.frame(Serotypes=c("DENV-1","DENV-2","DENV-3","DENV-4"),
#                  Count=c(36.36, 36.36, 18.18, 9.10))
# head(df)
# 
# Bng <- ggplot(df, aes(x = "", y = Count, fill = Serotypes)) +
#   geom_col(color = "black") +
#   geom_text(aes(label = Count),cex=3,
#             position = position_stack(vjust = 0.5)) +
#   coord_polar(theta = "y")+
#   theme_void()+  theme_bw()+
#   xlab("") + ylab("") + ggtitle("Dengue Serotypes in Bangladesh (2000-2023)")+
#   theme(plot.title = element_text(size = 10,hjust=0.5),
#         legend.title = element_text(size=10),
#         legend.text = element_text(size=10))
# Bng
# 
# #Bhutan
# factor(sertoData$Country)
# sertoDataBtn <- sertoData[which(sertoData$Country=='Bhutan'), ]
# 
# sertoDataBtn$Serotypes <- factor(sertoDataBtn$Serotypes,levels=c("DENV-1","DENV-2", "DENV-3","DENV-4"),
#                                  labels = c("DENV-1","DENV-2", "DENV-3","DENV-4"))
# 
# tab <- table(sertoDataBtn$Serotypes)
# prop.table(tab)*100
# 
# 
# 
# 
# # pie Age
# df <- data.frame(Serotypes=c("DENV-1","DENV-2","DENV-3","DENV-4"),
#                  Count=c(36.84, 36.84, 26.32, 0.00))
# head(df)
# 
# Btn <- ggplot(df, aes(x = "", y = Count, fill = Serotypes)) +
#   geom_col(color = "black") +
#   geom_text(aes(label = Count),cex=3,
#             position = position_stack(vjust = 0.5)) +
#   coord_polar(theta = "y")+
#   theme_void()+  theme_bw()+
#   xlab("") + ylab("") + ggtitle("Dengue Serotypes in Bhutan (2000-2023)")+
#   theme(plot.title = element_text(size = 10,hjust=0.5),
#         legend.title = element_text(size=10),
#         legend.text = element_text(size=10))
# Btn
# 
# #India
# factor(sertoData$Country)
# sertoDataInd <- sertoData[which(sertoData$Country=='India'), ]
# 
# sertoDataInd$Serotypes <- factor(sertoDataInd$Serotypes,levels=c("DENV-1","DENV-2", "DENV-3","DENV-4"),
#                                  labels = c("DENV-1","DENV-2", "DENV-3","DENV-4"))
# 
# tab <- table(sertoDataInd$Serotypes)
# prop.table(tab)*100
# 
# 
# 
# 
# # pie Age
# df <- data.frame(Serotypes=c("DENV-1","DENV-2","DENV-3","DENV-4"),
#                  Count=c(28.92, 28.92, 28.92, 13.25))
# head(df)
# 
# Ind <- ggplot(df, aes(x = "", y = Count, fill = Serotypes)) +
#   geom_col(color = "black") +
#   geom_text(aes(label = Count),cex=3,
#             position = position_stack(vjust = 0.5)) +
#   coord_polar(theta = "y")+
#   theme_void()+  theme_bw()+
#   xlab("") + ylab("") + ggtitle("Dengue Serotypes in India (2000-2023)")+
#   theme(plot.title = element_text(size = 10,hjust=0.5),
#         legend.title = element_text(size=10),
#         legend.text = element_text(size=10))
# Ind
# 
# #Maldives
# factor(sertoData$Country)
# sertoDataMal <- sertoData[which(sertoData$Country=='Maldives'), ]
# 
# sertoDataMal$Serotypes <- factor(sertoDataMal$Serotypes,levels=c("DENV-1","DENV-2", "DENV-3","DENV-4"),
#                                  labels = c("DENV-1","DENV-2", "DENV-3","DENV-4"))
# 
# tab <- table(sertoDataMal$Serotypes)
# prop.table(tab)*100
# 
# 
# # pie Age
# df <- data.frame(Serotypes=c("DENV-1","DENV-2","DENV-3","DENV-4"),
#                  Count=c(12.5, 25.0, 50.0, 12.5))
# head(df)
# 
# 
# Mal <- ggplot(df, aes(x = "", y = Count, fill = Serotypes)) +
#   geom_col(color = "black") +
#   geom_text(aes(label = Count),cex=3,
#             position = position_stack(vjust = 0.5)) +
#   coord_polar(theta = "y")+
#   theme_void()+  theme_bw()+
#   xlab("") + ylab("") + ggtitle("Dengue Serotypes in Maldives (2000-2023)")+
#   theme(plot.title = element_text(size = 10,hjust=0.5),
#         legend.title = element_text(size=10),
#         legend.text = element_text(size=10))
# Mal
# 
# #Npl
# factor(sertoData$Country)
# sertoDataNpl <- sertoData[which(sertoData$Country=='Nepal'), ]
# 
# sertoDataNpl$Serotypes <- factor(sertoDataNpl$Serotypes,levels=c("DENV-1","DENV-2", "DENV-3","DENV-4"),
#                                  labels = c("DENV-1","DENV-2", "DENV-3","DENV-4"))
# 
# tab <- table(sertoDataNpl$Serotypes)
# prop.table(tab)*100
# 
# # pie Age
# df <- data.frame(Serotypes=c("DENV-1","DENV-2","DENV-3","DENV-4"),
#                  Count=c(57.14, 28.57, 14.29, 0.00))
# head(df)
# 
# Npl <- ggplot(df, aes(x = "", y = Count, fill = Serotypes)) +
#   geom_col(color = "black") +
#   geom_text(aes(label = Count),cex=3,
#             position = position_stack(vjust = 0.5)) +
#   coord_polar(theta = "y")+
#   theme_void()+  theme_bw()+
#   xlab("") + ylab("") + ggtitle("Dengue Serotypes in Nepal (2000-2023)")+
#   theme(plot.title = element_text(size = 10,hjust=0.5),
#         legend.title = element_text(size=10),
#         legend.text = element_text(size=10))
# Npl
# 
# 
# #Pakistan
# factor(sertoData$Country)
# sertoDataPak <- sertoData[which(sertoData$Country=='Pakistan'), ]
# 
# sertoDataPak$Serotypes <- factor(sertoDataPak$Serotypes,levels=c("DENV-1","DENV-2", "DENV-3","DENV-4"),
#                                  labels = c("DENV-1","DENV-2", "DENV-3","DENV-4"))
# 
# tab <- table(sertoDataPak$Serotypes)
# prop.table(tab)*100
# 
# 
# 
# 
# # pie Age
# df <- data.frame(Serotypes=c("DENV-1","DENV-2","DENV-3","DENV-4"),
#                  Count=c(31.58, 39.47, 26.32, 2.63))
# head(df)
# 
# Pak <- ggplot(df, aes(x = "", y = Count, fill = Serotypes)) +
#   geom_col(color = "black") +
#   geom_text(aes(label = Count),cex=3,
#             position = position_stack(vjust = 0.5)) +
#   coord_polar(theta = "y")+
#   theme_void()+  theme_bw()+
#   xlab("") + ylab("") + ggtitle("Dengue Serotypes in Pakistan (2000-2023)")+
#   theme(plot.title = element_text(size = 10,hjust=0.5),
#         legend.title = element_text(size=10),
#         legend.text = element_text(size=10))
# Pak
# 
# 
# 
# 
# #Srilanka
# factor(sertoData$Country)
# sertoDataSri <- sertoData[which(sertoData$Country=='Srilanka'), ]
# 
# sertoDataSri$Serotypes <- factor(sertoDataSri$Serotypes,levels=c("DENV-1","DENV-2", "DENV-3","DENV-4"),
#                                  labels = c("DENV-1","DENV-2", "DENV-3","DENV-4"))
# 
# tab <- table(sertoDataSri$Serotypes)
# prop.table(tab)*100
# 
# 
# 
# # pie Age
# df <- data.frame(Serotypes=c("DENV-1","DENV-2","DENV-3","DENV-4"),
#                  Count=c(31.91, 14.89, 34.04, 19.15))
# head(df)
# 
# Sri <- ggplot(df, aes(x = "", y = Count, fill = Serotypes)) +
#   geom_col(color = "black") +
#   geom_text(aes(label = Count),cex=3,
#             position = position_stack(vjust = 0.5)) +
#   coord_polar(theta = "y")+
#   theme_void()+  theme_bw()+
#   xlab("") + ylab("") + ggtitle("Dengue Serotypes in Sri Lanka (2000-2023)")+
#   theme(plot.title = element_text(size = 10,hjust=0.5),
#         legend.title = element_text(size=10),
#         legend.text = element_text(size=10))
# Sri
# 
# library(gridExtra)
# tiff("Serotypes_SAC.tiff", units="in", width=5, height=5, res=300)
# gridExtra::grid.arrange(Afg, Bng, Btn, Ind, Mal, Npl, Pak, Sri, nrow=4, ncol=2)
# dev.off()


df = data.frame(type = c(" South Asia"," South Asia"," South Asia"," South Asia",
                         "Afghanistan","Afghanistan","Afghanistan","Afghanistan",
                         "Bangladesh","Bangladesh","Bangladesh","Bangladesh",
                         "Bhutan","Bhutan","Bhutan","Bhutan",
                         "India","India","India","India",
                         "Maldives","Maldives","Maldives","Maldives",
                         "Nepal","Nepal","Nepal","Nepal", 
                         "Pakistan","Pakistan","Pakistan","Pakistan",
                         "Srilanka","Srilanka","Srilanka","Srilanka"), 
                Serotypes = c("DENV-1", "DENV-2","DENV-3", "DENV-4",
                  "DENV-1", "DENV-2","DENV-3", "DENV-4", "DENV-1", "DENV-2","DENV-3", "DENV-4",
                            "DENV-1", "DENV-2","DENV-3", "DENV-4", "DENV-1", "DENV-2","DENV-3", "DENV-4",
                            "DENV-1", "DENV-2","DENV-3", "DENV-4", "DENV-1", "DENV-2","DENV-3", "DENV-4",
                            "DENV-1", "DENV-2","DENV-3", "DENV-4", "DENV-1", "DENV-2","DENV-3", "DENV-4"), 
                value = c(32.37, 28.63, 28.22, 10.79,
                  30, 20, 30, 20, 
                          36.36, 36.36, 18.18, 9.10,
                          36.84, 36.84, 26.32, 0.00,
                          28.92, 28.92, 28.92, 13.25,
                          12.5, 25.0, 50.0, 12.5,
                          57.14, 28.57, 14.29, 0.00,
                          31.58, 39.47, 26.32, 2.63,
                          31.91, 14.89, 34.04, 19.15))


library(ggplot2)
SAC <- ggplot(df, aes(x = factor(1), y = value, fill = Serotypes)) + 
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=3,
            position = position_stack(vjust = 0.5)) +
  scale_x_discrete(NULL, expand = c(0,0)) +
  scale_y_continuous(NULL, expand = c(0,0)) + 
  coord_polar(theta = "y") +
  facet_wrap(~type) +
  theme_void()+ theme_bw()+
  xlab(" ") + ylab("") + ggtitle("Dengue virus serotypes in South Asia (2000-2023)")+
  theme(        legend.position= "bottom",
        plot.title = element_text(size = 15,hjust=0.5),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )
SAC

library(gridExtra)
tiff("Serotypes_SAC.tiff", units="in", width=8, height=8, res=300)
gridExtra::grid.arrange(SAC)
dev.off()





# Libraries
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

# Build dummy data
data <- data.frame(
  day = as.Date("2019-01-01") + 0:99,
  temperature = runif(100) + seq(1,100)^2.5 / 10000,
  price = runif(100) + seq(100,1)^1.5 / 10
)

data <- aggregate(cbind(Cases, Deaths) ~ Year, data = charts.data, FUN = sum, na.rm = TRUE)

# Value used to transform the data
coeff <- 0.01

# A few constants
temperatureColor <- "seagreen"
priceColor <- "black"

doubleY <- ggplot(head(data, 80), aes(x=Year)) +
  
  geom_bar( aes(y=Cases), stat="identity", size=.1, fill=temperatureColor, color="black", alpha=.4) + 
  geom_line( aes(y=Deaths / coeff), size=1.5, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total number of cases",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Total number of deaths")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=15, hjust = 0.5,face="bold"),
    axis.title.y.right = element_text(color = priceColor, size=15, hjust = 0.5,face="bold")
  ) +
  
  ggtitle("Total number of cases and deaths \n due to dengue virus in South Asia (2000-2023)")+  theme(legend.title = element_text(size=15),
                                                legend.text = element_text(size=15),
                                                legend.position = c(0.85, 0.85),
                                                plot.title = element_text(hjust = 0.5),
                                                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
                                                text=element_text(size=15),
                                                axis.text.y = element_text(hjust = 0.5),
                                                axis.title.x = element_text(hjust = 0.5,size=15)) +
  scale_x_continuous(breaks=seq(2000,2023,1))


library(gridExtra)
tiff("BarCasesDeaths.tiff", units="in", width=10, height=8, res=300)
gridExtra::grid.arrange(doubleY)
dev.off()

