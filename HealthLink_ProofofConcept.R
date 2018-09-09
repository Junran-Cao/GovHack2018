#GovHack 2018 (9 September 2018)

#Team name: HealthLink
#Team members: Alex, Anna, Caleb, Faraz, Junran

#EXPLANATION: 
#This source code is a proof-of-concept in support of our submission.
#In this example, we select from five previously disparate datasets and
#i) merge them together such that the list of local socieconomic variables is combined by records from surrouding hospital records
#ii) estimate the correlations between hospital admission numbers & demand on different healthcare services with certain demographic data

#In the limited time of this hackathon, our aim is not to achieve a comprehensive integration of all current
#publicly available datasets, nor are the estimation results from the econometric models meant to 
#shed operational insights in the absence of a fuller selection of data inputs. 

#Instead, this proof-of-concept serves to illustrate the feasibility of our proposal. 
#In particular, we demonstrate the significant improvement in values added resultant from 
#combining ABS' SA3 regional data to the health sector's Local Hospital Network geospatial categorization
#as well as the type of statistical analyses (e.g. panel data regression, multinominal logit model) since made possible.

#################### 
# limited scope: four regions (ABS' SA3 classification) from NSW & selected number of socioeconomic and health data

#import datasets (uploaded onto our Project page)
getwd()
setwd("...")

input1_1 <- read.csv("input1_1revised.csv")
input1_2 <- read.csv("input1_2revised.csv")
input2 <- read.csv("input2.csv")
input3 <- read.csv("input3.csv", fileEncoding="latin1")
input4 <- read.csv("input4.csv", fileEncoding="latin1")

input3 <- input3[, -5]
input4 <- input4[, -6]

#Merge datasets sequentially
#The datasets have been transformed into the long format so as to be ready for subsequently analyses

# merge step 1
library(tidyverse)
data1.0 <- input1_1 %>% 
  left_join(input1_2, by = c("RegionID", "Year"))

# merge step 2
data1.1 <- data1.0 %>%
  left_join(input2, by = "RegionID")

# merge step 3
data1.2 <- data1.1 %>%
  left_join(input3, by = c("RegionID", "Year"))

# merge step 4
data1.3 <- data1.2 %>%
  left_join(input4, by = c("RegionID", "Year"))

data1.4 <- data1.3[, -which(names(data1.3) %in% c("Hospital.name",
                                                    "State.x",
                                                    "State.y",
                                                    "Hospital.y",
                                                    "Category.y",
                                                    "Local.Hospital.Network..LHN..y"))]

#Prepare a test dataset for illustrastion (The merged dataset has about 1.3 million rows)

# data1.5 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.6 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.7 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.8 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.9 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.10 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.11 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.12 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.13 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.14 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.15 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.16 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.17 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.18 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# data1.19 <- subset(data1.4[sample(nrow(data1.4)), ], !duplicated(RegionID) | !duplicated(Year))
# 
# data.test <- rbind(data1.5, data1.6, data1.7, data1.8, data1.9, data1.10, data1.11, data1.12, data1.13, data1.14, data1.15, data1.16, data1.17, data1.18, data1.19)
# rm(data1.5, data1.6, data1.7, data1.8, data1.9, data1.10, data1.11, data1.12, data1.13, data1.14, data1.15, data1.16, data1.17, data1.18, data1.19)
# 
# write.csv(data.test, "/Users/junran_cao/Desktop/random_testing/data.test.csv")

# data.test1 <- read.csv("data.test1.csv")

# names(data.test1)

# This dataset can now be analysed as a panel data that tracks the same region across time
library(plm)
data.test2 <- pdata.frame(data.test1, index = c("RegionID", "Year"))

#Econometric model 1: a linear regression model
#estimate the correlation between number of hospital admissions with selected wealth & income data
model.1 <- lm(Number.of.patient.admissions ~ ., data = data.test1[, c(79,2:3,40:48)])
summary(model.1)

#Econometric model 2: multinomial logit model
#estimate the expected demand on hospital services from selected causal variables
library(nnet)
model.2 <- multinom(Category.x ~ ., data = data.test1[, c(78,2:3,40:48)])
summary(model.2)
