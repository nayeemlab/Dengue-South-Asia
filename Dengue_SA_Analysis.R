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


df1 <- data.frame(Countries=c("Bangladesh Cases","Bangladesh Cases","Bangladesh Cases","Bangladesh Cases",
                              "Bangladesh Cases","Bangladesh Cases","Bangladesh Cases","Bangladesh Cases",
                              "Bangladesh Cases","Bangladesh Cases","Bangladesh Cases","Bangladesh Cases", 
                              "Bangladesh Cases","Bangladesh Cases","Bangladesh Cases","Bangladesh Cases",
                              "Bangladesh Cases","Bangladesh Cases", "Bangladesh Cases","Bangladesh Cases",
                              "Bangladesh Cases","Bangladesh Cases","Bangladesh Cases","Bangladesh Cases","Bangladesh Cases",
                              
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases",
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases",
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases",
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases",
                              "Nepal Cases","Nepal Cases","Nepal Cases","Nepal Cases"), 
                  
                  Years=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,2024,
                          
                          2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024), 
                  
                  Values=c(52.21276396,
                           19.54083069,
                           50.11459129,
                           3.908166137,
                           31.63523783,
                           8.427485827,
                           17.69128704,
                           3.747336255,
                           9.271842708,
                           3.811668208,
                           3.288971091,
                           9.434637046,
                           4.658308652,
                           12.14214878,
                           2.603376668,
                           21.95167207,
                           42.07056696,
                           19.22333332,
                           70.45084382,
                           703.6337036,
                           9.753984584,
                           197.3637208,
                           377.7096316,
                           1944.669965,
                           89.64,
                           
                           
                           0,
                           0,
                           0,
                           0,
                           0.043193889,
                           0,
                           1.382204455,
                           1.166235009,
                           0.431938892,
                           1.295816676,
                           39.6087964,
                           2.981750479,
                           6.907092882,
                           25.89216239,
                           13.436749,
                           5.095396389,
                           57.63459471,
                           79.67690205,
                           30.61012201,
                           679.0842357,
                           20.00414878,
                           18.51561164,
                           1878.443089,
                           1757.02868,
                           234.22
                           
                  ))

df1

df2 <- data.frame(Countries=c("Bangladesh Deaths","Bangladesh Deaths","Bangladesh Deaths","Bangladesh Deaths",
                              "Bangladesh Deaths","Bangladesh Deaths",
                              "Bangladesh Deaths","Bangladesh Deaths","Bangladesh Deaths","Bangladesh Deaths","Bangladesh Deaths",
                              "Bangladesh Deaths",
                              "Bangladesh Deaths","Bangladesh Deaths","Bangladesh Deaths","Bangladesh Deaths",
                              "Bangladesh Deaths","Bangladesh Deaths",
                              "Bangladesh Deaths","Bangladesh Deaths","Bangladesh Deaths",
                              "Bangladesh Deaths","Bangladesh Deaths","Bangladesh Deaths","Bangladesh Deaths",
                              
                              "Nepal Deaths","Nepal Deaths","Nepal Deaths","Nepal Deaths","Nepal Deaths",
                              "Nepal Deaths","Nepal Deaths","Nepal Deaths","Nepal Deaths","Nepal Deaths",
                              "Nepal Deaths","Nepal Deaths","Nepal Deaths","Nepal Deaths","Nepal Deaths",
                              "Nepal Deaths","Nepal Deaths","Nepal Deaths","Nepal Deaths","Nepal Deaths","Nepal Deaths",
                              "Nepal Deaths","Nepal Deaths","Nepal Deaths","Nepal Deaths"), 
                  
                  Years=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024,
                          
                          2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024), 
                  
                  Values=c(0.874758971,
                           0.353825741,
                           0.466406658,
                           0.080414941,
                           0.104539423,
                           0.032165976,
                           0.088456435,
                           0,
                           0,
                           0,
                           0,
                           0.041654027,
                           0.006942338,
                           0.013884676,
                           0,
                           0.041654027,
                           0.097192729,
                           0.055538702,
                           0.180500782,
                           1.138543396,
                           0.020827013,
                           0.728945467,
                           1.701394737,
                           10.32340935,
                           0.56,
                           
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0,
                           0.215969446,
                           0,
                           0,
                           0,
                           0,
                           0.037743677,
                           0.037743677,
                           0.113231031,
                           0.113231031,
                           0.226462062,
                           0,
                           0,
                           3.017358935,
                           0.685763394,
                           0.14
                  ))
df2

BDNPL <- ggplot() + 
  geom_col(data = df1, aes(x = Years, y = Values, fill = Countries), position = position_dodge()) +
  geom_point(aes(x = 2024, y = -30),
             position = position_dodge(0.9), 
             show.legend = FALSE,
             shape="\u2605", size=5, fill = "black") +
  scale_shape_manual(values = c(NA, 8))+
  scale_fill_manual("", values = c("Bangladesh Cases" = "#56B4E9", "Nepal Cases" = "#E69F00"))+
  geom_point(data = df2, aes(x = Years, y = Values*160,  group = Countries, col = Countries)) + 
  geom_line(data = df2, aes(x = Years, y = Values*160, group = Countries, col = Countries), size=1.2) +
  scale_color_manual("", values = c("Bangladesh Deaths" = "darkslateblue", "Nepal Deaths" = "gold4"))+
  scale_y_continuous(name = "Dengue cases per million",
                     sec.axis = sec_axis(trans = ~.*1/160, name="Dengue deaths per million"))+
  theme_bw() +
            ggtitle("Dengue cases and deaths per million population")+  
            theme(legend.title = element_text(size=15),
            legend.text = element_text(size=15),
            legend.position = c(0.2, 0.80),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
            text=element_text(size=15),
            axis.text.y = element_text(hjust = 0.5),
            axis.title.x = element_text(hjust = 0.5,size=15))+
  labs(caption = "★ = up to September' 2024")
BDNPL

tiff("BDNPL.tiff", units="in", width=8, height=8, res=300)
gridExtra::grid.arrange(BDNPL, nrow=1)
dev.off()

##################################################################################################

df1 <- data.frame(Countries=c("Temperature Bangladesh","Temperature Bangladesh","Temperature Bangladesh","Temperature Bangladesh",
                              "Temperature Bangladesh","Temperature Bangladesh",
                              "Temperature Bangladesh","Temperature Bangladesh","Temperature Bangladesh","Temperature Bangladesh","Temperature Bangladesh",
                              "Temperature Bangladesh",
                              "Temperature Bangladesh","Temperature Bangladesh","Temperature Bangladesh","Temperature Bangladesh",
                              "Temperature Bangladesh","Temperature Bangladesh",
                              "Temperature Bangladesh","Temperature Bangladesh","Temperature Bangladesh",
                              "Temperature Bangladesh","Temperature Bangladesh","Temperature Bangladesh",
                              
                              "Temperature Nepal","Temperature Nepal","Temperature Nepal","Temperature Nepal","Temperature Nepal",
                              "Temperature Nepal","Temperature Nepal","Temperature Nepal","Temperature Nepal","Temperature Nepal",
                              "Temperature Nepal","Temperature Nepal","Temperature Nepal","Temperature Nepal","Temperature Nepal",
                              "Temperature Nepal","Temperature Nepal","Temperature Nepal","Temperature Nepal","Temperature Nepal",
                              "Temperature Nepal","Temperature Nepal","Temperature Nepal","Temperature Nepal"), 
                  
                  Years=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
                          
                          2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023), 
                  
                  Values=c(24.34303254,
                           28.76560798,
                           24.02183244,
                           25.9875,
                           26.22083333,
                           26.6,
                           26.85416667,
                           26.1625,
                           26.3875,
                           27.06666667,
                           27.11666667,
                           26.25416667,
                           26.54166667,
                           26.57083333,
                           26.6875,
                           26.58333333,
                           27.35416667,
                           26.8875,
                           26.675,
                           26.90833333,
                           26.8375,
                           27.40416667,
                           27.39166667,
                           25.81583333,
                           
                           19.17663934,
                           19.6030411,
                           19.70057534,
                           19.45391781,
                           18.6707377,
                           19.02917808,
                           18.98857534,
                           18.62764384,
                           18.60989071,
                           19.47550685,
                           19.62463014,
                           18.47720548,
                           18.89453552,
                           18.59569863,
                           19.01679452,
                           19.16052055,
                           19.62538251,
                           19.63928767,
                           18.96046575,
                           19.10846575,
                           18.37161202,
                           19.0500274,
                           18.79024658,
                           19.30854795))
df1

df2 <- data.frame(Countries=c("Rainfall Bangladesh","Rainfall Bangladesh","Rainfall Bangladesh","Rainfall Bangladesh",
                              "Rainfall Bangladesh","Rainfall Bangladesh",
                              "Rainfall Bangladesh","Rainfall Bangladesh","Rainfall Bangladesh","Rainfall Bangladesh","Rainfall Bangladesh",
                              "Rainfall Bangladesh",
                              "Rainfall Bangladesh","Rainfall Bangladesh","Rainfall Bangladesh","Rainfall Bangladesh",
                              "Rainfall Bangladesh","Rainfall Bangladesh",
                              "Rainfall Bangladesh","Rainfall Bangladesh","Rainfall Bangladesh",
                              "Rainfall Bangladesh","Rainfall Bangladesh","Rainfall Bangladesh",
                              
                              "Rainfall Nepal","Rainfall Nepal","Rainfall Nepal","Rainfall Nepal","Rainfall Nepal",
                              "Rainfall Nepal","Rainfall Nepal","Rainfall Nepal","Rainfall Nepal","Rainfall Nepal",
                              "Rainfall Nepal","Rainfall Nepal","Rainfall Nepal","Rainfall Nepal","Rainfall Nepal",
                              "Rainfall Nepal","Rainfall Nepal","Rainfall Nepal","Rainfall Nepal","Rainfall Nepal",
                              "Rainfall Nepal","Rainfall Nepal","Rainfall Nepal","Rainfall Nepal"), 
                  
                  Years=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
                          
                          2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
                          2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023), 
                  
                  Values=c(2104,
                           1679,
                           1795,
                           1674.3,
                           2343.4,
                           2682.3,
                           1968.8,
                           3039.7,
                           2129.9,
                           1930.5,
                           1518.3,
                           1780.1,
                           1325.2,
                           1592.5,
                           1393.6,
                           2162.1,
                           1360,
                           2892.6,
                           1736.9,
                           1842.1,
                           1917.5,
                           1870.4,
                           1301,
                           2547.91,
                           
                           
                           709.24,
                           620.58,
                           525.02,
                           534.11,
                           1288.89,
                           1107.73,
                           1147.5,
                           1241.2,
                           1372.11,
                           900.96,
                           1044.96,
                           1146.67,
                           1029.34,
                           1424.53,
                           1331.65,
                           1205.33,
                           1155.23,
                           1077.25,
                           940.48,
                           1182.41,
                           1400.22,
                           1561.95,
                           1242.18,
                           1208.61
                  ))

BDNPL_TR <- ggplot() + 
  geom_col(data = df1, aes(x = Years, y = Values, fill = Countries), position = position_dodge()) +
  scale_fill_manual("", values = c("Temperature Bangladesh" = "brown1", "Temperature Nepal" = "darkgoldenrod1"))+
  geom_point(data = df2, aes(x = Years, y = Values/120,  group = Countries, col = Countries)) + 
  geom_line(data = df2, aes(x = Years, y = Values/120, group = Countries, col = Countries), size=1.2) +
  scale_color_manual("", values = c("Rainfall Bangladesh" = "blue4", "Rainfall Nepal" = "cornflowerblue"))+
  scale_y_continuous(name = "Annual mean temperature (°C)",
                     sec.axis = sec_axis(trans = ~.*1*120, name="Annual total Rainfall (mm)"))+
  theme_bw() +
  ggtitle("Annual mean temperature and total rainfall")+  
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15))
BDNPL_TR
tiff("BDNPL_TR.tiff", units="in", width=16, height=8, res=300)
gridExtra::grid.arrange(BDNPL_TR, nrow=1)
dev.off()











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



################################################################################
## Dengue - Data analysis ##
################################################################################
rm(list=ls()) #Remove all previous R objects#
## Packages ##
library(maptools)
library(RColorBrewer)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(ggrepel)
library(ggplot2)
library(tidyverse)
setwd("E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue Spatio\\resierraleonespacetimeclusteringanalysis")

sldata <- read.csv("DataNew_17_8_23.csv", header = T)
NROW(sldata)

sldata_inside <- sldata[sldata$Latitude >= 23.7 & sldata$Latitude <= 23.9,]
sldata_inside$Latitude
max(sldata_inside$Latitude, na.rm=T)
sldata_inside <- sldata_inside[sldata_inside$Longitude >= 90.35 & sldata_inside$Longitude <= 90.45,]
sldata_inside$Longitude
max(sldata_inside$Longitude, na.rm=T)
NROW(sldata_inside)

x <- NROW(sldata_inside)/NROW(sldata)
x

#sldata_inside <- sldata[sldata$Location == "Inside",]
#sldata_inside <- sldata[sldata$Location == "Outside",]
shp <- readOGR(dsn = "E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue Spatio\\Dhaka", "cc486qp3429")

head(shp@data)
xLon = sldata$HLon
xLat = sldata$HLat

SL.map <- fortify(shp, region = "fid")

map1 <- ggplot() + 
  geom_polygon(data = SL.map, aes(x = long, y = lat, group = group), colour = "cadetblue", fill = "azure2") +
  labs(title = "Location of Dengue patients (Red) and Hospitals (Green)") +
  xlab(label="Longitute") + ylab(label="Latitute")
map1
map2 <- map1 +  geom_point(data=sldata_inside, aes(x=Longitude, y=Latitude), colour = "darkgreen", size = 1)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 15))


map2


#bangladesh

sldata <- read.csv("DataNew_17_8_23.csv", header = T)

shp <- readOGR(dsn = "E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue Spatio\\sle_admbnda_adm4_1m_gov_ocha", "BGD_adm2")

head(shp@data)
xLon = sldata$Longitude
xLat = sldata$Latitude

SL.map <- fortify(shp, region = "NAME_2")

map1 <- ggplot() + 
  geom_polygon(data = SL.map, aes(x = long, y = lat, group = group), colour = "cadetblue", fill = "azure2") +
  labs(title = "Location of Dengue patients") +
  xlab(label="Longitute") + ylab(label="Latitute")
map1

map2 <- map1 +  geom_point(data=sldata, aes(x=Longitude, y=Latitude), colour = "darkgreen", size = 2)+ 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 15))


map2

