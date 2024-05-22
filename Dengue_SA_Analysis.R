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
library(psych)
library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(psych)
require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions
library(mgcv)
library(GGally)
library(mgcv)
library(visreg)


options(scipen = 999)
setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue South-Asia')

charts.data <- read.csv("Dengue_SA_data.csv")
charts.data

describe.by(charts.data$CasesPerTh, charts.data$Countries)
describe.by(charts.data$CasesPerTh)

IQR(charts.data[which(charts.data$Countries=="Afghanistan"),]$CasesPerTh)
summary(charts.data[which(charts.data$Countries=="Afghanistan"),]$CasesPerTh)

IQR(charts.data[which(charts.data$Countries=="Bangladesh"),]$CasesPerTh)
summary(charts.data[which(charts.data$Countries=="Bangladesh"),]$CasesPerTh)

IQR(charts.data[which(charts.data$Countries=="Bhutan"),]$CasesPerTh)
summary(charts.data[which(charts.data$Countries=="Bhutan"),]$CasesPerTh)

IQR(charts.data[which(charts.data$Countries=="India"),]$CasesPerTh)
summary(charts.data[which(charts.data$Countries=="India"),]$CasesPerTh)

IQR(charts.data[which(charts.data$Countries=="Maldives"),]$CasesPerTh)
summary(charts.data[which(charts.data$Countries=="Maldives"),]$CasesPerTh)

IQR(charts.data[which(charts.data$Countries=="Nepal"),]$CasesPerTh)
summary(charts.data[which(charts.data$Countries=="Nepal"),]$CasesPerTh)

IQR(charts.data[which(charts.data$Countries=="Pakistan"),]$CasesPerTh)
summary(charts.data[which(charts.data$Countries=="Pakistan"),]$CasesPerTh)

IQR(charts.data[which(charts.data$Countries=="Sri Lanka "),]$CasesPerTh)
summary(charts.data[which(charts.data$Countries=="Sri Lanka "),]$CasesPerTh)

IQR(charts.data$CasesPerTh)
summary(charts.data$CasesPerTh)







describe.by(charts.data$Deaths, charts.data$Countries)
describe.by(charts.data$Deaths)

IQR(charts.data[which(charts.data$Countries=="Afghanistan"),]$Deaths)
summary(charts.data[which(charts.data$Countries=="Afghanistan"),]$Deaths)

IQR(charts.data[which(charts.data$Countries=="Bangladesh"),]$Deaths)
summary(charts.data[which(charts.data$Countries=="Bangladesh"),]$Deaths)

IQR(charts.data[which(charts.data$Countries=="Bhutan"),]$Deaths)
summary(charts.data[which(charts.data$Countries=="Bhutan"),]$Deaths)

IQR(charts.data[which(charts.data$Countries=="India"),]$Deaths)
summary(charts.data[which(charts.data$Countries=="India"),]$Deaths)

IQR(charts.data[which(charts.data$Countries=="Maldives"),]$Deaths)
summary(charts.data[which(charts.data$Countries=="Maldives"),]$Deaths)

IQR(charts.data[which(charts.data$Countries=="Nepal"),]$Deaths)
summary(charts.data[which(charts.data$Countries=="Nepal"),]$Deaths)

IQR(charts.data[which(charts.data$Countries=="Pakistan"),]$Deaths)
summary(charts.data[which(charts.data$Countries=="Pakistan"),]$Deaths)

IQR(charts.data[which(charts.data$Countries=="Sri Lanka "),]$Deaths)
summary(charts.data[which(charts.data$Countries=="Sri Lanka "),]$Deaths)

IQR(charts.data$Deaths)
summary(charts.data$Deaths)



describe.by(charts.data$CFR...., charts.data$Countries)
describe.by(charts.data$CFR....)

IQR(charts.data[which(charts.data$Countries=="Afghanistan"),]$CFR....)
summary(charts.data[which(charts.data$Countries=="Afghanistan"),]$CFR....)

IQR(charts.data[which(charts.data$Countries=="Bangladesh"),]$CFR....)
summary(charts.data[which(charts.data$Countries=="Bangladesh"),]$CFR....)

IQR(charts.data[which(charts.data$Countries=="Bhutan"),]$CFR....)
summary(charts.data[which(charts.data$Countries=="Bhutan"),]$CFR....)

IQR(charts.data[which(charts.data$Countries=="India"),]$CFR....)
summary(charts.data[which(charts.data$Countries=="India"),]$CFR....)

IQR(charts.data[which(charts.data$Countries=="Maldives"),]$CFR....)
summary(charts.data[which(charts.data$Countries=="Maldives"),]$CFR....)

IQR(charts.data[which(charts.data$Countries=="Nepal"),]$CFR....)
summary(charts.data[which(charts.data$Countries=="Nepal"),]$CFR....)

IQR(charts.data[which(charts.data$Countries=="Pakistan"),]$CFR....)
summary(charts.data[which(charts.data$Countries=="Pakistan"),]$CFR....)

IQR(charts.data[which(charts.data$Countries=="Sri Lanka "),]$CFR....)
summary(charts.data[which(charts.data$Countries=="Sri Lanka "),]$CFR....)

IQR(charts.data$CFR....)
summary(charts.data$CFR....)


p3 <- ggplot() + geom_line(aes(y = log10(CasesPerTh+1), x = Year, colour = Countries), size=1,
                           data = charts.data, stat="identity")+  xlab("Years") + ylab("Dengue cases/100000 (log10)") + 
  theme(legend.position='none',
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust=0.5),
        text=element_text(size=15)) +
  scale_x_continuous(breaks=seq(2000,2023,1))

p3

p4 <- ggplot() + geom_line(aes(y =CFR...., x = Year, colour = Countries), size=1,
                           data = charts.data, stat="identity")+  xlab("Years") + ylab("Case Fatality Ratio (%)") + 
  theme(legend.position= "bottom",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
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


library(ggplot2)
library(tidyverse)
setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue South-Asia')
dat <- read.csv("Country_Year.csv")


dat$Countries <- factor(dat$Countries)
dat$Year <- factor(dat$Year)
#dat$Values <- factor(dat$Values)
x<- ggplot(dat, aes( Countries, Year)) +
  geom_tile(aes(fill = Values), colour = "black") +
  geom_text(aes(label = Sero))+
  guides(fill=guide_legend(title="Total \nSerotypes \nCount"))+
  scale_fill_gradient(low = "white",high = "steelblue")+ 
#  scale_fill_brewer(palette = "Dark2") +
#   scale_shape_manual(values=c("0", "1", "2", "3", "4"))+ 
#  scale_color_manual(values=c('white','red', 'darkgreen', 'grey', 'purple'))+
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15))
x

tiff("x.tiff", units="in", width=8, height=8, res=300)
gridExtra::grid.arrange(x, nrow=1)
dev.off()


############
library(tidyverse)


df1 <- data.frame(Countries=c("BD Cases","BD Cases","BD Cases","BD Cases","BD Cases","BD Cases",
                              "BD Cases","BD Cases","BD Cases","BD Cases","BD Cases","BD Cases",
                              "BD Cases","BD Cases","BD Cases","BD Cases","BD Cases","BD Cases",
                              "BD Cases","BD Cases","BD Cases","BD Cases","BD Cases","BD Cases",
                              
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases",
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases",
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases",
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases",
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases"), 
                  
                  Years=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
                          
                          2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023), 
                  
                  Values=c(5551, 2430, 6232, 486, 3934, 1048, 2200, 466, 1153, 474, 409, 1359, 671,
                           1749, 375, 3162, 6060, 2769, 10148, 101354, 1405, 28429, 62382, 321179,
                           
                           0, 0, 0, 0, 1, 0, 32, 27, 10, 30, 917, 79, 183, 686, 356, 135, 
                           1527, 2111, 811, 17992, 530, 540, 54784, 51243
                  ))

df1

df2 <- data.frame(Countries=c("BD Deaths","BD Deaths","BD Deaths","BD Deaths","BD Deaths","BD Deaths",
                              "BD Deaths","BD Deaths","BD Deaths","BD Deaths","BD Deaths","BD Deaths",
                              "BD Deaths","BD Deaths","BD Deaths","BD Deaths","BD Deaths","BD Deaths",
                              "BD Deaths","BD Deaths","BD Deaths","BD Cases","BD Cases","BD Cases",
                              
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases",
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases",
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases",
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases",
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases"), 
                  
                  Years=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
                          
                          2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023), 
                  
                  Values=c(5551, 2430, 6232, 486, 3934, 1048, 2200, 466, 1153, 474, 409, 1359, 671,
                           1749, 375, 3162, 6060, 2769, 10148, 101354, 1405, 28429, 62382, 321179,
                           
                           0, 0, 0, 0, 1, 0, 32, 27, 10, 30, 917, 79, 183, 686, 356, 135, 
                           1527, 2111, 811, 17992, 530, 540, 54784, 51243
                  ))


ggplot() + 
  geom_col(data = df1, aes(x = Years, y = Values, fill = Countries), position = position_dodge()) +
  scale_fill_manual("Countries", values = c("BD Cases" = "#56B4E9", "Nepal Cases" = "#E69F00"))+
  geom_point(data = df2, aes(x = Years, y = Values*10,  group = Countries, col = Countries)) + 
  geom_line(data = df2, aes(x = Years, y = Values*10, group = Countries, col = Countries)) +
  scale_color_manual("Countries", values = c("BD Deaths" = "darkgrey", "Nepal Deaths" = "black"))+
  scale_y_continuous(name = "First Axis",
                     sec.axis = sec_axis(trans = ~.*1/10, name="Second Axis"))+
  theme_bw()



+
  ggtitle("Total number of cases and deaths \n due to dengue virus in South Asia (2000-2023)")+  theme(legend.title = element_text(size=15),
                                                                                                       legend.text = element_text(size=15),
                                                                                                       legend.position = c(0.9, 0.9),
                                                                                                       plot.title = element_text(hjust = 0.5),
                                                                                                       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
                                                                                                       text=element_text(size=15),
                                                                                                       axis.text.y = element_text(hjust = 0.5),
                                                                                                       axis.title.x = element_text(hjust = 0.5,size=15))













library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)


library(ggplot2)
library(forecast)
setwd('E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue South-Asia')
Dengue <- read.csv("TS_Dengue.csv")

#ARIMA Case

DengueTS <- ts(Dengue$Case, start=c(2000))

auto.arima(DengueTS)

Fit<-Arima(DengueTS,order=c(1,2,2))
summary(Fit)

fcast <- forecast(Fit, h=5)
library(ggfortify)
x <- autoplot(fcast, size = 2) +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("ARIMA Model (Cases)")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") + theme_bw()+
  theme( legend.text = element_text(color = "Black", size = 20),
         text = element_text(size = 20))+ scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                        labels = trans_format("log10", math_format(10^.x)))
x


#ARIMA Death

DengueTS <- ts(Dengue$Death, start=c(2000))

auto.arima(DengueTS)

Fit<-Arima(DengueTS,order=c(1,1,2))
summary(Fit)

fcast <- forecast(Fit, h=5)
library(ggfortify)
y <- autoplot(fcast, size = 2) +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("ARIMA Model (Deaths)")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") + theme_bw()+
  theme( legend.text = element_text(color = "Black", size = 20),
         text = element_text(size = 20))+ scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                        labels = trans_format("log10", math_format(10^.x)))
y


#ARIMA Death

DengueTS <- ts(Dengue$CFR, start=c(2000))

auto.arima(DengueTS)

Fit<-Arima(DengueTS,order=c(1,2,2))
summary(Fit)

fcast <- forecast(Fit, h=5)
library(ggfortify)
z <- autoplot(fcast, size = 2) +
  xlab("Years") + ylab("Number of dengue cases") + ggtitle("ARIMA Model (CFR (%))")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") + theme_bw()+
  theme( legend.text = element_text(color = "Black", size = 20),
         text = element_text(size = 20))
z


tiff("arima.tiff", units="in", width=18, height=6, res=300)
gridExtra::grid.arrange(x,y,z, ncol=3, nrow=1)
dev.off()




