#Loading the necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(summarytools)

#Cleaning the environment
rm(list=ls())

#Reading & wrangling the data 
gdp_regions <- read.csv(file="Real_GDP_per_c.csv",header=TRUE)
gdp_growth_regions <- read.csv(file="Real_GDP_per_c_growth.csv",header=TRUE)
gdp_cities <- read.csv(file="cities_real_gdp_pc.csv",header=TRUE)
gdp_growth_cities <- read.csv(file="cities_real_gdp_growth.csv",header=TRUE)
  
colnames(gdp_regions)<- c("NUTS.code","Region.Name",1998:2018)
colnames(gdp_growth_regions)<- c("NUTS.code","Region.Name",1998:2018)
colnames(gdp_cities)<- c("NUTS.code","Region.Name",1998:2018)
colnames(gdp_growth_cities)<- c("NUTS.code","Region.Name",1998:2018)

panel_gdp_regions <- pivot_longer(gdp_regions,cols=c(3:23),names_to="Year",values_to="GDP")
panel_gdp_growth_regions <- pivot_longer(gdp_growth_regions,cols=c(3:23),names_to="Year",values_to="GDP_g")
panel_gdp_cities <- pivot_longer(gdp_cities,cols=c(3:23),names_to="Year",values_to="GDP")
panel_gdp_growth_cities <- pivot_longer(gdp_growth_cities,cols=c(3:23),names_to="Year",values_to="GDP_g")

panel_gdp_regions$GDP_g <- panel_gdp_growth_regions$GDP_g
panel_gdp_cities$GDP_g <- panel_gdp_growth_cities$GDP_g


### Common trends visual analysis - finding the control units ###
panel_gdp_regions$Year_date <-as.Date(ISOdate(panel_gdp_regions$Year, 1, 1))
panel_gdp_cities$Year_date <- as.Date(ISOdate(panel_gdp_cities$Year, 1,1))


all_reg_gg <- ggplot(data=panel_gdp_regions,aes(x=Year_date,y=GDP_g,group=Region.Name,col=Region.Name))+
  geom_line()+
  scale_x_date(limit=c(as.Date("1999-01-01"),as.Date("2005-01-1")))+
  ylab("Real GDP growth (%)")+
  xlab("Year")+
  ylim(-5,7.5)+
  theme(legend.title = element_blank())+
  geom_vline(xintercept=as.Date("2004-01-01"), linetype=4)
all_reg_gg
ggplotly(all_reg_gg) #Based on visual analysis, we only choose Northern Ireland as the control unit for the regional-level data

ireland <- dplyr::filter(panel_gdp_regions,Region.Name=="Northern Ireland" | Region.Name=="London")

ireland_gg <- ggplot(data=ireland,aes(x=Year_date,y=GDP_g,group=Region.Name,col=Region.Name))+
  geom_line()+
  scale_x_date(limit=c(as.Date("1999-01-01"),as.Date("2005-01-1")))+
  ylab("Real GDP growth (%)")+
  ylim(-5,7.5)+
  xlab("Year")+
  theme(legend.title = element_blank())+
  geom_vline(xintercept=as.Date("2004-01-01"), linetype=4)
ireland_gg

all_city_gg <- ggplot(data=panel_gdp_cities,aes(x=Year_date,y=GDP_g,group=Region.Name,col=Region.Name))+
  geom_line()+
  scale_x_date(limit=c(as.Date("1999-01-01"),as.Date("2005-01-1")))+
  ylab("Real GDP growth (%)")+
  ylim(-10,10)+
  xlab("Year")+
  theme(legend.title = element_blank())+
  geom_vline(xintercept=as.Date("2004-01-01"), linetype=4)
all_city_gg
ggplotly(all_city_gg) #Based on visual analysis, we only choose Leeds as the control unit for the city-level data

leeds <- dplyr::filter(panel_gdp_cities,Region.Name=="Leeds" | Region.Name=="London")

leeds_gg <- ggplot(data=leeds,aes(x=Year_date,y=GDP_g,group=Region.Name,col=Region.Name))+
  geom_line()+
  scale_x_date(limit=c(as.Date("1999-01-01"),as.Date("2005-01-1")))+
  ylab("Real GDP growth (%)")+
  ylim(-5,7.5)+
  xlab("Year")+
  theme(legend.title = element_blank())+
  geom_vline(xintercept=as.Date("2004-01-1"), linetype=4)
leeds_gg

### Signaling effect with diff-in-diff ###
#Pre-treatment period 1999 to 2004, post-treatment 2005
#Regional level control - Northern Ireland
#City level control - Leeds

ireland$Treated <-0
ireland$Treated[which(ireland$Region.Name =="London")] <- 1
ireland$Post<-0
ireland$Post[which(ireland$Year>2004)] <- 1
ireland <- ireland %>% filter(Year!=1998)

leeds$Treated <-0
leeds$Treated[which(leeds$Region.Name =="London")] <- 1
leeds$Post<-0
leeds$Post[which(leeds$Year>2004)] <- 1
leeds <- leeds %>% filter(Year!=1998)

signal_ireland <- dplyr::filter(ireland,Year<2006)
signal_ireland_model <- lm(GDP_g~ Treated * Post+log(GDP), data=signal_ireland)
summary(signal_ireland_model)

signal_leeds <- dplyr::filter(leeds,Year<2006)
signal_leeds_model <- lm(GDP_g~ Treated * Post+log(GDP), data=signal_leeds)
summary(signal_leeds_model)

### Aggregate effect with diff-in-diff ###
#Pre-treatment period 1999 to 2004, post-treatment 2005-2012
#Regional level control - Northern Ireland
#City level control - Leeds

#To reduce the number of commands, we replace 2005 outcome with the mean of post-treatment values
aggregate_ireland <- ireland
aggregate_ireland$GDP_g[7]<- mean(aggregate_ireland$GDP_g[7:14])
aggregate_ireland$GDP[7]<- mean(aggregate_ireland$GDP[7:14])
aggregate_ireland$GDP_g[27]<- mean(aggregate_ireland$GDP_g[27:34])
aggregate_ireland$GDP[27]<- mean(aggregate_ireland$GDP[27:34])

aggregate_ireland <- dplyr::filter(aggregate_ireland,Year<2006)
aggregate_ireland_model <- lm(GDP_g~ Treated * Post+log(GDP), data=aggregate_ireland)
summary(aggregate_ireland_model)

aggregate_leeds<- leeds
aggregate_leeds$GDP_g[7]<- mean(aggregate_leeds$GDP_g[7:14])
aggregate_leeds$GDP[7]<- mean(aggregate_leeds$GDP[7:14])
aggregate_leeds$GDP_g[27]<- mean(aggregate_leeds$GDP_g[27:34])
aggregate_leeds$GDP[27]<- mean(aggregate_leeds$GDP[27:34])

aggregate_leeds <- dplyr::filter(aggregate_leeds,Year<2006)
aggregate_leeds_model <- lm(GDP_g~ Treated * Post+log(GDP), data=aggregate_leeds)
summary(aggregate_leeds_model)

### Post-olympic effect with diff-in-diff ###
#Pre-treatment period 1999 to 2004, post-treatment 2013
#Regional level control - Northern Ireland
#City level control - Leeds

post_ireland <- dplyr::filter(ireland,Year<2005 | Year==2013)
post_ireland_model <- lm(GDP_g~ Treated * Post+log(GDP), data=post_ireland)
summary(post_ireland_model)


post_leeds <- dplyr::filter(leeds,Year<2005 | Year==2013)
post_leeds_model <- lm(GDP_g~ Treated * Post+log(GDP), data=post_leeds)
summary(post_leeds_model)

###Generating tables

#Table 1 - data summary table
summary.regions <- dplyr::select(panel_gdp_regions,c(4,5)) %>% summarytools::descr() %>% as.data.frame()
summary.regions <- summary.regions[c(1:7,14),] %>% t() %>% as.data.frame()
rownames(summary.regions) <- c("GDP per capita", "GDP growth (%)")
colnames(summary.regions)[8] <- "N of observations"
colnames(summary.cities)[1] <- "Variable"

summary.cities <- dplyr::select(panel_gdp_cities,c(4,5)) %>% summarytools::descr() %>% as.data.frame()
summary.cities <- summary.cities[c(1:7,14),] %>% t() %>% as.data.frame()
rownames(summary.cities) <- c("GDP per capita", "GDP growth (%)")
colnames(summary.cities)[8] <- "N of observations"
colnames(summary.cities)[1] <- "Variable"

tab_dfs(list(summary.regions,summary.cities),
        titles=c("Summary statistics for regions","Summary statistics for cities"),
        show.rownames=TRUE)

#Table 2 - Signaling effect estimates
tab_model(signal_ireland_model,signal_leeds_model,
          #pred.labels = c("Intercept", "Post-Treatment", "Treated","I(Post-Treatment * Treated)"),
          dv.labels=c("Model 1 with Ireland","Model 2 with Leeds"),
          show.se = TRUE,
          string.pred = "Coeffcient",
          string.est = "Estimate",
          string.se = "SE",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value",
          p.style="numeric_stars",
          emph.p=FALSE,
          title="Table 2. Diff-in-diff estimates for the signaling effect (real GDP growth per capita)"
          )
#Table 3 - Aggregate Effect estimates
tab_model(aggregate_ireland_model,aggregate_leeds_model,
          #pred.labels = c("Intercept", "Post-Treatment", "Treated","I(Post-Treatment * Treated)"),
          dv.labels=c("Model 1 with Ireland","Model 2 with Leeds"),
          show.se = TRUE,
          string.pred = "Coeffcient",
          string.est = "Estimate",
          string.se = "SE",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value",
          p.style="numeric_stars",
          emph.p=FALSE,
          title="Table 3.Diff-in-diff estimates for the aggregate effect"
)

#Table 4 - Post-Olympic Effect estimates
tab_model(post_ireland_model,post_leeds_model,
          #pred.labels = c("Intercept", "Post-Treatment", "Treated","I(Post-Treatment * Treated)"),
          dv.labels=c("Model 1 with Ireland","Model 2 with Leeds"),
          show.se = TRUE,
          string.pred = "Coeffcient",
          string.est = "Estimate",
          string.se = "SE",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value",
          p.style="numeric_stars",
          emph.p=FALSE,
          title="Table 4.Diff-in-diff estimates for the post-Olympics effect"
)
