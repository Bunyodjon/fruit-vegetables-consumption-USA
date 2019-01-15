# Authod Name: Bunyod Tusmatov 
# Date:10/27/2018
# Solo Project 
# ========================================================================================== #
# load libraries 
library(Hmisc)
library(foreign)
library(tidyverse)
library(dplyr)
library(maps)
library(ggmap)
library(maptools)
library(ggplot2)
library(readxl)
library(gridExtra)
library(grid)
# =========================================================================================== # 
# Upload data 
mydata <- sasxport.get("C:\\Users\\Admin\\Documents\\All Miami Courses\\Fall 2017 Courses\\DataViz\\Project Solo\\LLCP2017XPT\\LLCP2017.XPT")

# Subset the dataset with the following variables: 
mydata <- select(mydata, c("x.state", "ladult", "genhlth", "menthlth", "poorhlth", 
                           "sex", "frutda2.", "grenda1.", "x.frutsu1", "x.vegesu1", "x.rfbmi5")) 
write.csv(mydata, "mydata.csv")

######################### you can start from here.

# read the data set from here. 
mydata <- read.csv("mydata.csv")
# Summarise and create variables avgFruit and avgVegetable (they are divided by 100 because it was two desimal place)
mydata1 <- mydata %>% 
  group_by(x.state) %>% 
  summarise(avgFruit_d=mean(x.frutsu1, na.rm = TRUE), 
            avgVegetable_d=mean(x.vegesu1, na.rm = TRUE)) %>% 
  mutate(avgFruit = avgFruit_d/100,
         avgVegetable = avgVegetable_d/100)

mydata2 <- mydata %>% 
  group_by(x.state, x.rfbmi5) %>%
  summarise(count=n())%>%
  mutate(prc_obese=count/(sum(count)*100)) %>% 
  filter(x.rfbmi5==2) %>% 
  select(-x.rfbmi5)


# Sometimes there is an issue with above code because RStudio says that 
# "x and labels must be same type". If so, please run the code below and it fixes the issue   
labelDataset <- function(data) {
  correctLabel <- function(x) {
    
    if(!is.null(attributes(x)$labels)) {
      class(attributes(x)$labels) <- typeof(x)
    }
    return(x)
  }
  for(i in colnames(data)) {
    data[, i] <- correctLabel(data[, i])
  }
  return(data)
}

labelDataset(mydata)
# head(mydata)

mydata[] <- lapply(mydata, unclass)
# head(mydata)


# Left join all two data sets using FIPS code
mydata3 <- left_join(mydata1, mydata2, by=c("x.state"="x.state")) %>%
  select(x.state, avgFruit, avgVegetable, prc_obese)

# Extract data on the state level from the maps package data
states.outlines <- map_data("state")
# head(states.outlines)

# Load states.stats data set from the class for recieving state names
states.stats <- read.csv("http://kmaurer.github.io/documents/data/StateStatsBRFSS.csv") %>%
  select(X_STATE, StateName, avgHealth, avgPhy, propHealthcare)

# merge three data sets 
mydata.all <- states.outlines %>% 
  left_join(states.stats, by=c("region"="StateName")) %>%
  left_join(mydata3, by=c("X_STATE"="x.state"))

# Adding layer of labels 
labelData <- mydata.all %>%
  group_by(region) %>%  
  summarise(lat = (min(lat)+median(lat, na.rm = T)+max(lat))/3,
            long = (min(long)+median(long, na.rm = T)+max(long))/3,
            state = state2abbr(region[1]),
            prc_obese = mean(prc_obese, na.rm = TRUE),
            avgHealth = mean(avgHealth, na.rm =TRUE),
            avgPhy = mean(avgPhy, na.rm = TRUE)) %>% 
  mutate(label = paste(state,"\n",round(prc_obese*100, 1),"BMI"),
         avgHealth = round(avgHealth, 2),
         avgPhy = round(avgPhy))

# Only working with East Cost states
east_cost <- c("FL", "GA", "SC", "NC", "VA", "MD", "DE", "NJ", "NY", "CT",
               "RI", "MA", "NH", "ME", "PA", "VT")

# subset label data based on east cost list 
labelData2 <- labelData %>% filter(state %in% east_cost)

# Get full names of states for later use 
ec_state_names <- labelData2$region

mydata.all2 <- mydata.all %>% 
  filter(region %in% ec_state_names)

# Only plotting East Cost states  Average Fruit 
plot3 <- ggplot() + 
  geom_polygon(data=mydata.all2, aes(x=long, y=lat,
                                     fill=avgFruit, group=group)) + 
  geom_path(data=mydata.all2, aes(x=long, y=lat, group=group)) +
  scale_fill_gradient("Average Fruit \n Consumption \n per Day", low="#FF9999", high="#FF0000", 
                      breaks=c(1, 1.25,  1.5, 1.75, 2, 2.25)) +
  coord_map() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        legend.position = c(.9, .2),
        plot.title = element_text(size=18, hjust=.5)) +
  geom_text(aes(x=long,y=lat,label=state), 
            size=2.5, color="black", data=labelData2)
plot3

# Only plotting East Cost states  Average Vegetables 
plot4 <- ggplot() + 
  geom_polygon(data=mydata.all2, aes(x=long, y=lat,
                                     fill=avgVegetable, group=group)) + 
  geom_path(data=mydata.all2, aes(x=long, y=lat, group=group))+
  scale_fill_gradient("Average Vegetable \n Consumption \n per Day", low="#ADEBAD", high="#33CC33", 
                      breaks=c(1.5, 2, 2.5, 3, 3.5)) +
  coord_map() + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        legend.position = c(.9, .2),
        plot.title = element_text(size=18, hjust=.5)) +
  # labs(title="Map of Average Vegetable \nConsumption by State",
  #     caption="Data source: Center for Disease Control - BRFSS, 2017") 
  geom_text(aes(x=long,y=lat,label=state), color="black", 
            size=2, data=labelData2)
plot4 

# Two Maps 
# Plot inside grid using grid.arrange  
grid.arrange(plot3, plot4, nrow = 1,
             top=textGrob("Map of Average Fruit and Vegetable Consumption Per Day", gp=gpar(fontsize=20, fontface="bold")),
             bottom=textGrob("Source: Center for Disease Control - BRFSS, 2017", hjust =1.5))

# =====================================================================================# 
# Plotting food data 
# County Orchard Acreage from 2012
local_food1 <- read_excel(path="DataDownload.xlsx", sheet="LOCAL") %>%
  select(FIPS, State, County, FRESHVEG_FARMS12, ORCHARD_ACRES12, GHVEG_FARMS12, FMRKTPTH16) %>% 
  filter(State %in% east_cost)

# summarise local food data by state 
local_food <- local_food1 %>% 
  group_by(State) %>%
  summarise(total.orchard = sum(ORCHARD_ACRES12, na.rm = TRUE),
            total.fresh.vegetable.farms = sum(FRESHVEG_FARMS12, na.rm = TRUE),
            total.green.house.farms = sum(GHVEG_FARMS12, na.rm = TRUE),
            farmer.market.per1000 = mean(FMRKTPTH16, na.rm = TRUE)) %>%
  mutate(farmer.market.per100000 = farmer.market.per1000*100) 


# Prepare state population data 
state_population <- read_excel(path="DataDownload.xlsx", sheet="Supplemental Data - County") %>%
  select(FIPS, State, County, pop2016)
state_population$State <- tolower(state_population$State)

# Subset state population based on EC state list 
state_population <- state_population %>% filter(State %in% ec_state_names)
# remove commas and change character to number 
state_population$pop2016 <- as.numeric(gsub(",","", state_population$pop2016))

# summarise state population using county population 
state_population <- state_population %>% 
  group_by(State) %>%
  summarise(pop2016=sum(pop2016, na.rm = TRUE))

state_population$State <- state2abbr(state_population$State)

local_food <- left_join(local_food, state_population, by=c("State"="State"))

# Selecting and filtering east cost names 
east_cost_state <- states.stats %>% 
  select(X_STATE, StateName) %>% 
  filter(StateName %in% ec_state_names)
east_cost_state$State <- state2abbr(east_cost_state$StateName)

# =====================================================================================#
# Availibility 
# Making the first data set for the first plot 
local_food2 <- local_food %>% 
  left_join(east_cost_state, by=c("State"="State")) %>% 
  left_join(mydata3, by=c("X_STATE"="x.state")) %>% 
  mutate(fruit.vegetable.per.day=(avgFruit+avgVegetable)) %>% 
  select(StateName, State, X_STATE, pop2016, avgFruit, avgVegetable, total.orchard,
         total.fresh.vegetable.farms, total.green.house.farms, farmer.market.per100000, fruit.vegetable.per.day)

# Plotting some relationships 
# Matrix Plot for observing some strong relationship 
ggscatmat(data=local_food2, columns =c(3:10))


# Average Fruit and Vegetable consumption, and farmer market per 100,0000
g1 <- ggplot() + 
  geom_point(data=local_food2, aes(x=farmer.market.per100000, y=fruit.vegetable.per.day)) +
  annotate(geom = "rect", xmin = 9.35, xmax = 10.4, ymin = 5.1, 
           ymax =5.3, fill = "#FF0066", alpha =1) + 
  geom_smooth(data=local_food2, aes(x=farmer.market.per100000, y=fruit.vegetable.per.day), 
              method=lm, formula = 'y~x', fill="#FF80FF", alpha=0.6) +
  geom_text(data=local_food2, aes(x=farmer.market.per100000,   y=fruit.vegetable.per.day, label=State), vjust=-0.25) +
  scale_y_continuous(breaks=c(3.5, 4, 4.5, 5, 5.5)) +
  theme_classic() + 
  labs(y="Fruit & Vegetable Per Day (unit)",
       x="Numer of Farmer Markets Per 100,000 People", 
       title="AVAILABILITY")+ 
  theme(plot.title = element_text(face="bold", size = 14, hjust = 0.5),
        plot.margin = unit(c(0, 1, 0, 1), "lines"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), 
                                    size = 12, face = "bold"))
g1 

# Fitting linear model and checking its significance 
lm1 <- lm(fruit.vegetable.per.day~farmer.market.per100000, data=local_food2)
summary(lm1)

# =========================================================================================# 
# ACCESSIBILITY accessibility 
# I will be working with the following variables: 
# PCT_LACCESS_POP15 = Population, low access to store (%), 2015  
# PCT_LACCESS_LOWI15 = Low income & low access to store (%), 2015 
# PCT_LACCESS_HHNV15 = Households, no car & low access to store (%), 2015 
# PCT_LACCESS_CHILD15 = Children, low access to store (%), 2015 
# PCT_LACCESS_SENIORS15 = Seniors, low access to store (%), 2015 

local_food5 <- read_excel(path="DataDownload.xlsx", sheet="ACCESS") %>%
  select(FIPS, State, County, PCT_LACCESS_POP15, PCT_LACCESS_LOWI15, PCT_LACCESS_HHNV15, 
         PCT_LACCESS_CHILD15, PCT_LACCESS_SENIORS15) %>% 
  filter(State %in% east_cost) %>% 
  group_by(State) %>%
  summarise(PCT_LACCESS_POP15= mean(PCT_LACCESS_POP15, na.rm = TRUE),
            PCT_LACCESS_LOWI15=mean(PCT_LACCESS_LOWI15, na.rm = T),
            PCT_LACCESS_HHNV15=mean(PCT_LACCESS_HHNV15, na.rm =T), 
            PCT_LACCESS_CHILD15=mean(PCT_LACCESS_CHILD15, na.rm = T), 
            PCT_LACCESS_SENIORS15=mean(PCT_LACCESS_SENIORS15, na.rm = T))

# Merge with other data sets so we can compare 
local_food5 <- local_food5 %>% 
  left_join(east_cost_state, by=c("State"="State")) %>% 
  left_join(mydata3, by=c("X_STATE"="x.state")) %>% 
  select(-prc_obese) %>% 
  mutate(fruit.vegetable.per.day=(avgFruit+avgVegetable))

# Overview of which variables have a relationship with  fruit.vegetable.per.day
pairs(local_food5[,c(11, 2:6)], pch = 19)

# PCT_LACCESS_HHNV15 = Households, no car & low access to store (%), 2015 
g2 <- ggplot() + 
  geom_point(data=local_food5, aes(x=PCT_LACCESS_HHNV15, y=fruit.vegetable.per.day)) +
  annotate(geom = "rect", xmin =2.65, xmax =2.9, ymin = 5.1, 
           ymax =5.3, fill = "#FF0066", alpha=1) +
  geom_smooth(data=local_food5, aes(x=PCT_LACCESS_HHNV15, y=fruit.vegetable.per.day), 
              method=lm, fill="#FF80FF", alpha=0.6) +
  geom_text(data=local_food5, aes(x=PCT_LACCESS_HHNV15, y=fruit.vegetable.per.day, label=State), vjust=-0.25) +
  scale_x_continuous(breaks = c(2, 2.5, 3, 3.5, 4, 4.5),
                     labels = c("2%", "2.5%", "3%", "3.5%", "4%", "4.5%")) +
  theme_classic() + 
  labs(y="Number of Fruit + Vegetable \nConsumption Per Day", 
       x="Households, no car & low access to store",
       title="ACCESSIBILITY") + 
  theme(plot.title = element_text(face="bold", size = 14, hjust=0.5), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_line(color = "gray90"),
        plot.margin = unit(c(0,1,0,1), "lines"), 
        plot.background = element_blank())
g2

# fitting a linear model to check for significance of the coefficients 
lm2 <- lm(fruit.vegetable.per.day~PCT_LACCESS_HHNV15, data=local_food5)
summary(lm2)

logm2 <- lm(log(fruit.vegetable.per.day)~PCT_LACCESS_HHNV15, data=local_food5)
summary(logm2)

# =========================================================================================# 
# SOCIOECONOMIC 

# MEDHHINC15  - Median household income, 2015
# PERPOV10 - Poverty rate, 2015 

local_food6 <- read_excel(path="DataDownload.xlsx", sheet="SOCIOECONOMIC") %>%
  select(FIPS, State, County, MEDHHINC15, PERPOV10) %>% 
  filter(State %in% east_cost) %>% 
  group_by(State) %>%
  summarise(MEDHHINC15= mean(MEDHHINC15, na.rm = TRUE),
            PERPOV10=mean(PERPOV10, na.rm = TRUE))

# merge with other data sets so we can compare 
local_food6 <- local_food6 %>% 
  left_join(east_cost_state, by=c("State"="State")) %>% 
  left_join(mydata3, by=c("X_STATE"="x.state")) %>% 
  select(-prc_obese) %>% 
  mutate(fruit.vegetable.per.day=(avgFruit+avgVegetable))

# Overview of which variables have a relationship with  fruit.vegetable.per.day
pairs(local_food6[,c(8, 2:3)], pch = 19)

# MEDHHINC15  - Median household income, 2015
g3 <- ggplot() + 
  geom_jitter(data=local_food6, aes(x=MEDHHINC15, y=fruit.vegetable.per.day)) +
  annotate(geom = "rect", xmin = 61000, xmax = 63000, ymin = 5.1, 
           ymax =5.3, fill = "#FF0066", alpha = 1) +
  geom_smooth(data=local_food6, aes(x=MEDHHINC15, y=fruit.vegetable.per.day), 
              method =lm, formula = 'y~x', fill="#FF80FF", alpha=0.6) +
  geom_text(data=local_food6, aes(x=MEDHHINC15, y=fruit.vegetable.per.day, label=State), vjust=-0.25) +
  scale_x_continuous(breaks = c(40000, 50000, 60000, 70000, 80000),
                     labels = c("$40,000", "$50,000", "$60,000", "$70,000", "$80,000")) +
  theme_classic() + 
  labs(y="Number of Fruit + Vegetable \nConsumption Per Day", 
       x="Median Household Income", 
       title="ABILITY TO BUY") + 
  theme(plot.title = element_text(size=14, face="bold", hjust=0.5), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_line(color = "gray90"),
        plot.margin = unit(c(0,1, 0,1), "lines"), 
        plot.background = element_blank())

g3


# fitting a linear model to check for significance of the coefficients 
lm3 <- lm(fruit.vegetable.per.day~MEDHHINC15, data=local_food6)
summary(lm3)

# ======================================================================================# 
# Plot all three graphs with the same y-axis 

# plotting three plots with the same y-axis
plot5 <- ggplotGrob(g1)
plot6 <- ggplotGrob(g2)
plot7 <- ggplotGrob(g3)

width <- unit.pmax(plot5$widths[2,3],plot6$widths[2,3], plot7$widths[2,3])

plot5$widths[2:3]=as.list(width)
plot6$widths[2:3]=as.list(width)
plot7$widths[2:3]=as.list(width)

grid.arrange(plot5, plot6, plot7, ncol=3, 
             bottom=textGrob("Source: USDA Food Environment Atlas (2016)", hjust=-0.9))

# Reference: 

# Learned how to annotate inside the graph. Adopted the code from here: https://socviz.co/workgeoms.html
# I used this stackoverflow answer as a quideline for putting threee plots with the same 
# y axis. https://stackoverflow.com/questions/14743060/r-ggplot-graphs-sharing-the-same-y-axis-but-with-different-x-axis-scales

