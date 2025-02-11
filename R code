# load libraries
library(dplyr)
library(ggplot2)
library(ROCR)
library(MASS)

# getting a glimpse at the data
summary(churndata)
head(churndata)
str(churndata)


#########################
## 3. DATA PREPARATION ##
#########################

# check for missing values
sapply(churndata, function(x) sum(is.na(x)))

# data preparation
churndata$Gender <- as.factor(churndata$Gender)
churndata$Age <- as.numeric(churndata$Age)
churndata$Income <- as.numeric(churndata$Income)
churndata$Relation_length <- as.numeric(churndata$Relation_length)
churndata$Contract_length <- as.numeric(churndata$Contract_length)
churndata$Start_channel <- as.factor(churndata$Start_channel)
churndata$Email_list <- as.factor(churndata$Email_list)
churndata$Home_age <- as.numeric(churndata$Home_age)
churndata$Home_label <- as.factor(churndata$Home_label)
churndata$Electricity_usage <- as.numeric(churndata$Electricity_usage)
churndata$Gas_usage <- as.numeric(churndata$Gas_usage)
churndata$Churn <- as.factor(churndata$Churn)


#create new variable for flexible contract 
churndata$flexcontract <- ifelse(churndata$Contract_length==0,1,0)
churndata$contractlevels <- churndata$Contract_length

churndata$contractlevels[churndata$Contract_length >= 1 &churndata$Contract_length <= 12 ] <- 1
churndata$contractlevels[churndata$Contract_length > 12 &churndata$Contract_length <= 24 ] <- 2
churndata$contractlevels[churndata$Contract_length > 24 &churndata$Contract_length <= 36 ] <- 3


#Create new variable for province
unique(churndata$Province)

churndata$Province2 <- churndata$Province
churndata$Province2[churndata$Province2 == "Noord-Brabant"] <- 1
churndata$Province2[churndata$Province == "Noord-Holland"] <- 2
churndata$Province2[churndata$Province == "Gelderland"] <- 3
churndata$Province2[churndata$Province == "Zuid-Holland"] <- 4
churndata$Province2[churndata$Province == "Friesland"] <- 5
churndata$Province2[churndata$Province == "Limburg"] <- 6
churndata$Province2[churndata$Province == "Overijssel"] <- 7
churndata$Province2[churndata$Province == "Utrecht"] <- 8
churndata$Province2[churndata$Province == "Groningen"] <- 9
churndata$Province2[churndata$Province == "Drenthe"] <- 10
churndata$Province2[churndata$Province == "Flevoland"] <- 11
churndata$Province2[churndata$Province == "Zeeland"] <- 12
churndata$Province2 <- as.factor(churndata$Province2)
churndata$Province <- as.factor(churndata$Province)

# OUTLIERS ---------------------------------------------------------------------

FindOutliers <- function(data){
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq
  extrem.threshold.upper = (iqr * 1.5) + upperq
  extrem.threshold.lower = lowerq - (iqr * 1.5)    
  result <- which(data > extrem.threshold.upper | data < extrem.threshold.lower)
}

iqr_age <- FindOutliers(churndata$Age)
iqr_age

iqr_income <- FindOutliers(churndata$Income)
iqr_income

iqr_electricityusage <- FindOutliers(churndata$Electricity_usage)
iqr_electricityusage

iqr_gasusage <- FindOutliers(churndata$Gas_usage)
iqr_gasusage

iqr_homeage <- FindOutliers(churndata$Home_age)
iqr_homeage
summary(churndata$Home_age)
hist(churndata$Home_age)

#eliminate income outliers 
churndata$Income2 <- ifelse(churndata$Income>11036,11036,churndata$Income)
hist(churndata$Income2)
hist(log(churndata$Income2))
library("car")
qqnorm(log(churndata$Income2))
qqline(log(churndata$Income2))
logincome <- log(churndata$Income2)

#eliminate electricity outliers
churndata$Electricity_usage2 <- ifelse(churndata$Electricity_usage>3760,3760,churndata$Electricity_usage)
hist(churndata$Electricity_usage2)

#eliminate gas outliers
churndata$Gas_usage2 <- ifelse(churndata$Gas_usage>2618.5,2618.5,churndata$Gas_usage)
hist(churndata$Gas_usage2)


#############################
## 4. EXPLORATIVE ANALYSIS ##
#############################

# get descriptives
table(churndata$Gender)
round(prop.table(table(churndata$Gender)), digits = 2)

table(churndata$Start_channel)
round(prop.table(table(churndata$Start_channel)), digits = 2)

table(churndata$Home_label)
round(prop.table(table(churndata$Home_label)), digits = 2)
plot(churndata$Home_label)

table(churndata$Province)
round(prop.table(table(churndata$Province)), digits = 2)

table(churndata$Email_list)
round(prop.table(table(churndata$Email_list)), digits = 2)

table(churndata$flexcontract)
round(prop.table(table(churndata$flexcontract)), digits = 2)

table(churndata$Churn)
round(prop.table(table(churndata$Churn)), digits = 2)
## data set is balanced regarding churn

## inspect age
hist(churndata$Age)
age <- churndata[,c(1,3)]
age <-age %>% mutate(MySpecificBins = cut(Age, breaks = c(-Inf,18,30,40,50,60,70,Inf)))
head(age,10)
age %>% group_by(MySpecificBins) %>% count()
remove(age)

summary(churndata$Age)
summary(churndata$Relation_length)
summary(churndata$Contract_length)
summary(churndata$Home_age)
summary(churndata$Electricity_usage2)
summary(churndata$Gas_usage2)
apply(churndata, 2, sd)



# CORRELATIONS -----------------------------------------------------------------

library(corrplot)
corrplot(cor(churndata[,c(3,5,9,17:19)]), title = "Correlation plot", order="hclust", addCoef.col = TRUE, tl.col = "black")
round(churndata[,c(3:5,9,18,19)], digits = 2)

#Age
#Gender
summary(aov(churndata$Age ~ churndata$Gender))

#Contract levels
summary(aov(Age ~ contractlevels, data = churndata))

#Contract types
summary(aov(Age ~ flexcontract, data = churndata))

#Start channel
aov1 <- aov(Age ~ Start_channel, data = churndata) #high
aov1$coefficients #7.691621, phone

#Email list  
aov2 <- aov(Age ~ Email_list, data = churndata) #high
aov2$coefficients #-4.187751, "1"

#Home lable
summary(aov(Age ~ Home_label, data = churndata))

#Province
summary(aov(Age ~ Province2, data = churndata))

#Income
#Gender
aov3 <- aov(Income2 ~ Gender, data = churndata)
summary(aov3) #high
aov3$coefficients #-842.3244, "1"

#Contract levels
summary(aov(Income2 ~ contractlevels, data = churndata))

#Contract types
summary(aov(Income2 ~ flexcontract, data = churndata))

#Home label
summary(aov(Income2 ~ Home_label, data = churndata))

#Start channel
aov4 <- aov(Income2 ~ Start_channel, data = churndata) #high
summary(aov4)
aov4$coefficients #114.7596, "phone"

#Email list  
aov5 <- aov(Income2 ~ Email_list, data = churndata)
summary(aov5) #high
aov5$coefficients #-98.16321, "1"

#Province
summary(aov(Income2 ~ Province2, data = churndata))

#Relation length
#Gender
summary(aov(Relation_length ~ Gender, data = churndata))

#Contract levels
summary(aov(Relation_length ~ contractlevels, data = churndata))

#Contract types
summary(aov(Relation_length ~ flexcontract, data = churndata))

#Start channel
aov6 <- aov(Relation_length ~ Start_channel, data = churndata)
summary(aov6) #high
aov6$coefficients #11.09397, "Phone"

#Email list  
aov7 <- aov(Relation_length ~ Email_list, data = churndata)
summary(aov7) #high
aov7$coefficients #-6.499348, "1"

#Home label
summary(aov(Relation_length ~ Home_label, data = churndata))

#Province
summary(aov(Relation_length ~ Province2, data = churndata))

#Electricity usage
#Gender
summary(aov(Electricity_usage2 ~ Gender, data = churndata))

#Contract levels
summary(aov(Electricity_usage2 ~ contractlevels, data = churndata))

#Contract types
summary(aov(Electricity_usage2 ~ flexcontract, data = churndata))

#Start channel
summary(aov(Electricity_usage2 ~ Start_channel, data = churndata))

#Email list  
aov8 <- aov(Electricity_usage2 ~ Email_list, data = churndata)
summary(aov8) #high
aov8$coefficients #-20.98176, "1"

#Home label
aov9 <- aov(Electricity_usage2 ~ Home_label, data = churndata) #high
summary(aov9) #high
aov9$coefficients #Home_labelB Home_labelC Home_labelD Home_labelE Home_labelF Home_labelG
#247.5993	387.7180	514.3980	617.4130	676.7499	733.8244

#Province
summary(aov(Electricity_usage2 ~ Province2, data = churndata))

#Gas usage
#Gender
summary(aov(Gas_usage2 ~ Gender, data = churndata))

#Contract levels
summary(aov(Gas_usage2 ~ contractlevels, data = churndata))

#Contract types
summary(aov(Gas_usage2 ~ flexcontract, data = churndata))

#Start channel
summary(aov(Gas_usage2 ~ Start_channel, data = churndata))

#Email list  
summary(aov(Gas_usage2 ~ Email_list, data = churndata))

#Home label
aov10 <- aov(Gas_usage2 ~ Home_label, data = churndata) #high
summary(aov10)
aov10$coefficients #Home_labelB Home_labelC Home_labelD Home_labelE Home_labelF Home_labelG
#390.0831	625.9635	804.5827	933.1130   1047.8016   1126.2171

#Province
summary(aov(Gas_usage2 ~ Province2, data = churndata))

#Gender
#Start channel
chisq.test(churndata$Gender , churndata$Start_channel)

#Email list
chisq.test(churndata$Gender, churndata$Email_list)

#Home label
chisq.test(churndata$Gender, churndata$Home_label)

#Flex contact
chisq.test(churndata$Gender, churndata$flexcontract)

#Contract level
chisq.test(churndata$Gender, churndata$contractlevels)

#Start channel
#Email list
chisq.test(churndata$Start_channel, churndata$Email_list) #high

#Home label
chisq.test(churndata$Start_channel, churndata$Home_label)

#Flex contract
chisq.test(churndata$Start_channel, churndata$flexcontract)

#Contract levels
chisq.test(churndata$Start_channel, churndata$contractlevels)

#Home label
#Flex contract
chisq.test(churndata$Home_label, churndata$flexcontract)

#Contract levels
chisq.test(churndata$Home_label, churndata$contractlevels)




# STATISTICAL ANALYSIS ---------------------------------------------------------

#AGE
#testing for equal variance
leveneTest(churndata$Age,churndata$Churn)
#reject null hypothesis of equal variance (p= 0.014)
t.test(churndata$Age~churndata$Churn,var.equal = FALSE)
#significant different in means --> churners (0) are slightly older than non churners1
#calculate impact with regression
Ma<- (lm(churndata$Age ~ churndata$Churn, data= churndata))
summary(Ma)
# churners are 2.22 years younger on average 

#GENDER
#Correlation between Gender and Churn (1-0)
Gendertest<-chisq.test(churndata$Churn, churndata$Gender)
Gendertest
#Significant difference between segments 
Gendertest$observed
#5191: males non churners/ 5007: females non churners 
#4761: male churners / 5041: female churners 
#females churn more than men
round(Gendertest$residuals, 3)
corrplot(Gendertest$residuals, is.cor = FALSE, title="Correlation of Churn with Gender")

# load packages
library(ggstatsplot)
library(ggplot2)
library(dplyr)

# plot
ggbarstats(
  data = churndata,
  x = Churn,
  y = Gender
) +
  labs(caption = NULL) 


#RELATION LENGTH
library(car)
#testing for equal variance
leveneTest(churndata$Relation_length,churndata$Churn)
#reject null hypothesis of equal variance (p= 0.014)
t.test(churndata$Relation_length~churndata$Churn,var.equal = FALSE)
#significant different in means --> non churners have longer relations (66,5) than churners (52,1)
#calculate impact with regression
Mb<- (lm(churndata$Relation_length ~ churndata$Churn, data= churndata))
summary(Mb)
#churners have  on average 14 months less relation length

#START CHANNEL
#Correlation btw Start channel and Churn
Channeltest<-chisq.test(churndata$Churn, churndata$Start_channel)
Channeltest
#Significant difference between segments 
Channeltest$observed
#More people start online than by phone
round(Channeltest$residuals, 3)
corrplot(Channeltest$residuals, is.cor = FALSE, title="Correlation of churn with channel")

#EMAIL LIST
#Correlation btw EMAIL LIST and Churn
Emailtest<-chisq.test(churndata$Churn, churndata$Email_list)
Emailtest
#Significant difference between segments 
Emailtest$observed
#more people in email list than not
round(Emailtest$residuals, 3)
corrplot(Emailtest$residuals, is.cor = FALSE, title="Correlation of churn with emaillist")

#HOME AGE
library(car)
#testing for equal variance
leveneTest(churndata$Home_age,churndata$Churn)
# cannot reject null hypothesis of equal variance (p= 0.014)
t.test(churndata$Home_age~churndata$Churn,var.equal = TRUE)
#significant different in means --> non churners have less years house age than churners
#calculate impact with regression
Md<- (lm(churndata$Home_age ~ churndata$Churn, data= churndata))
summary(Md)
#churners have houses that are on average 4 years older

#HOME LABEL
#Correlation btw HOMELABEL and Churn
Homelabeltest<-chisq.test(churndata$Churn, churndata$Home_label)
Homelabeltest
#Significant difference between segments 
Homelabeltest$observed
round(Homelabeltest$residuals, 3)
corrplot(Homelabeltest$residuals, is.cor = FALSE, title="Correlation of churn with homelabel")

## Contract Type: Flexible vs Fixed
## Flexible contract has significantly more churn behaviour than fixed contract
table(churndata$Churn, churndata$flexcontract, dnn = c("Churn", "Contract type"))
Contract_type_test <- chisq.test(churndata$Churn,  churndata$flexcontract)

# load packages
library(ggstatsplot)
library(ggplot2)
library(dplyr)

# plot
ggbarstats(data = churndata,  x = Churn,  y = flexcontract) +  labs(caption = NULL) 

## CONTRACT LENGTH: 0 vs 1 vs 2 vs 3 years 
## Contract length perform differently between years (p-value < 2.2e-16)
table(churndata$Churn, churndata$contractlevels, dnn = c("Churn", "Contract Levels"))
Contract_level_test <- chisq.test(churndata$Churn,  churndata$contractlevels)
Contract_level_test

# plot
ggbarstats(data = churndata,x = Churn,y = contractlevels) +  labs(caption = NULL) 


## Province 
table(churndata$Churn, churndata$Province, dnn = c("Churn", "Province"))
Province_test <- chisq.test(churndata$Churn,  churndata$Province)

# plot
ggbarstats( data = churndata,  x = Churn,  y = Province) +  labs(caption = NULL) 

## Start Channel (Online vs Phone)
## Result: Online channel has significant churning behaviour than online start channel 
table(churndata$Churn, churndata$Start_channel, dnn = c("Churn", "Start Channel"))
Start_channel_test <- chisq.test(churndata$Churn,  churndata$Start_channel)

# plot
ggbarstats(
  data = churndata,  x = Churn,  y = Start_channel) +  labs(caption = NULL)

## INCOME 2
#testing for equal variance
leveneTest(churndata$Income2,churndata$Churn)
#reject null hypothesis of equal variance (p < 2.2e-16)
t.test(churndata$Income2 ~ churndata$Churn,var.equal = FALSE)
#significant difference in means --> Churners(1) earn lower income than non churners(0)
#calculate impact with regression
Mc<- (lm(churndata$Income2 ~ churndata$Churn, data= churndata))
summary(Mc)
# churners on average earn 704.90 euro less than the people who didn't churn on average 

# ELECTRICITY USAGE 2
#testing for equal variance
leveneTest(churndata$Electricity_usage2,churndata$Churn)
# Cannot reject null hypothesis of equal variance (p=0.2567)
t.test(churndata$Electricity_usage2 ~ churndata$Churn,var.equal = FALSE)
#significant difference in means --> Churners1 have higher yearly electricity usage(kWh) than non churners0
#calculate impact with regression
M_ElecUsage2<- (lm(churndata$Electricity_usage2 ~ churndata$Churn, data= churndata))
summary(M_ElecUsage2)
# churners on average have 315.63(kWh) yearly electricity usage than the people who didn't churn on average 

# GAS USAGE 2
#testing for equal variance
leveneTest(churndata$Gas_usage2,churndata$Churn)
# Reject null hypothesis of equal variance (p=8.506e-08 ***)
t.test(churndata$Gas_usage2 ~ churndata$Churn,var.equal = FALSE)
#significant different in means --> Churners1 have higher yearly gas usage(in cubic meters) than non churners0
#calculate impact with regression
M_GasUsage2<- (lm(churndata$Gas_usage2 ~ churndata$Churn, data= churndata))
summary(M_GasUsage2)
# churners on average have 259.33(in cubic) yearly gas usage than the people who didn't churn on average 






# GRAPHICAL INSPECTION ---------------------------------------------------------

library(tidyverse) 
library(caret)
library(plyr)
library(dplyr) 
library(reshape2)
library(grid)
library(gridExtra) 
library(kableExtra) 
library(formattable) 
library(scales)
library(ltm)
library(ROSE)
library(foreign) 
library(ggplot2)
library(tidyr)
library(survival) 
library(lattice) 
library(Formula) 
library(Hmisc)
library(HH)
library(base)
library(ggthemes) 
library(MASS)
library(party) 
library(performanceEstimation)
install.packages("caret")


# PLOTS CONDITIONAL ON CHURN

churndata$Churn2 <- ifelse(churndata$Churn == 1, "Current Customers", "Lost Customers")
churndata$Gender2 <- ifelse(churndata$Gender == 1, "Female", "Male")


g1 <- ggplot(churndata, aes(x = Gender2, group = Churn2)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count",
           alpha = 0.6, color = "grey20", show.legend = F) +
  geom_text(aes(label = percent(..prop.., accuracy = 0.1), y = ..prop..), 
            size = 4, stat = "count", position = position_stack(vjust = 0.5)) +
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =15)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, .9)) +
  theme(axis.text.y = element_text(size = 12)) +
  scale_fill_manual(values = c("#69b3a2", "#56797F", "#2F4448")) + theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n Gender and Churn \n", x = "\n Gender", y = "") + 
  theme(plot.title = element_text(size=15)) + theme(axis.title.x = element_text(size =15))
g1

g2 <- ggplot(churndata, aes(x = Start_channel, group = Churn2)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count",
           alpha = 0.6, color = "grey20", show.legend = F) +
  geom_text(aes(label = percent(..prop.., accuracy = 0.1), y = ..prop..), 
            size = 4, stat = "count", position = position_stack(vjust = 0.5)) +
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =15)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, .9)) +
  theme(axis.text.y = element_text(size = 12)) +
  scale_fill_manual(values = c("#69b3a2", "#56797F", "#2F4448")) + theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n Start Channel and Churn \n", x = "\n Start Channel", y = "") + 
  theme(plot.title = element_text(size=15)) + theme(axis.title.x = element_text(size =15))
g2

churndata$Email_list2 <- ifelse(churndata$Email_list == 1, "Known", "Unknown")

g3 <- ggplot(churndata, aes(x = Email_list2, group = Churn2)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count",
           alpha = 0.6, color = "grey20", show.legend = F) +
  geom_text(aes(label = percent(..prop.., accuracy = 0.1), y = ..prop..), 
            size = 4, stat = "count", position = position_stack(vjust = 0.5)) +
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =15)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, .9)) +
  theme(axis.text.y = element_text(size = 12)) +
  scale_fill_manual(values = c("#69b3a2", "#56797F", "#2F4448")) + theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n E-Mail List and Churn \n", x = "\n E-Mail List", y = "") + 
  theme(plot.title = element_text(size=15)) + theme(axis.title.x = element_text(size =15))
g3

g4 <- ggplot(churndata, aes(x = Home_label, group = Churn2)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count",
           alpha = 0.6, color = "grey20", show.legend = F) +
  geom_text(aes(label = percent(..prop.., accuracy = 0.1), y = ..prop..), 
            size = 4, stat = "count", position = position_stack(vjust = 0.5)) +
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =15)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, .25)) +
  theme(axis.text.y = element_text(size = 12)) +
  scale_fill_manual(values = c("#CEF7E2", "#AEE8CF", "#90D1BC", "#69B3A2", "#4C998F", "#34807E","#216367" )) + 
  theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n Home Label and Churn \n", x = "\n Home Label", y = "") + 
  theme(plot.title = element_text(size=15)) + theme(axis.title.x = element_text(size =15))
g4

churndata$flexcontract2 <- ifelse(churndata$flexcontract == 1, "Flexible Contract", "Fixed Contract")

g6 <- ggplot(churndata, aes(x = flexcontract2, group = Churn2)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count",
           alpha = 0.6, color = "grey20", show.legend = F) +
  geom_text(aes(label = percent(..prop.., accuracy = 0.1), y = ..prop..), 
            size = 4, stat = "count", position = position_stack(vjust = 0.5)) +
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =15)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, .9)) +
  theme(axis.text.y = element_text(size = 12)) +
  scale_fill_manual(values = c("#69b3a2", "#56797F", "#2F4448")) + theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n Contract Type and Churn \n", x = "\n Contract Type", y = "") + 
  theme(plot.title = element_text(size=15)) + theme(axis.title.x = element_text(size =15))
g6


g7 <- ggplot(churndata, aes(y = Age, group = Churn2)) +
  geom_boxplot(alpha = 0.6, color = "grey20", show.legend = F, fill = "steelblue") + 
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =12)) +
  theme(axis.text.y = element_text(size = 8)) + theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n Age and Churn \n", y = "\n Age", y = "") + 
  theme(plot.title = element_text(size=12)) + theme(axis.title.x = element_text(size =12)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
g7

g8 <- ggplot(churndata, aes(y = Income2, group = Churn2)) +
  geom_boxplot(alpha = 0.6, color = "grey20", show.legend = F, fill = "steelblue") + 
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =12)) +
  theme(axis.text.y = element_text(size = 8)) + theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n Income and Churn \n", y = "\n Income", y = "") + 
  theme(plot.title = element_text(size=12)) + theme(axis.title.x = element_text(size =12)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
g8

g9 <- ggplot(churndata, aes(y = Relation_length, group = Churn2)) +
  geom_boxplot(alpha = 0.6, color = "grey20", show.legend = F, fill = "steelblue") + 
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =12)) +
  theme(axis.text.y = element_text(size = 8)) + theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n Relation Length and Churn \n", y = "\n Relation Length", y = "") + 
  theme(plot.title = element_text(size=12)) + theme(axis.title.x = element_text(size =12)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
g9

g10 <- ggplot(churndata, aes(y = Contract_length, group = Churn2)) +
  geom_boxplot(alpha = 0.6, color = "grey20", show.legend = F, fill = "steelblue") + 
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =12)) +
  theme(axis.text.y = element_text(size = 8)) + theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n Contract Length and Churn \n", y = "\n Contract Length", y = "") + 
  theme(plot.title = element_text(size=12)) + theme(axis.title.x = element_text(size =12)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
g10

g11 <- ggplot(churndata, aes(y = Home_age, group = Churn2)) +
  geom_boxplot(alpha = 0.6, color = "grey20", show.legend = F, fill = "steelblue") + 
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =12)) +
  theme(axis.text.y = element_text(size = 8)) + theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n Home Age and Churn \n", y = "\n Home Age", y = "") + 
  theme(plot.title = element_text(size=12)) + theme(axis.title.x = element_text(size =12)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
g11

g12 <- ggplot(churndata, aes(y = Electricity_usage2, group = Churn2)) +
  geom_boxplot(alpha = 0.6, color = "grey20", show.legend = F, fill = "steelblue") + 
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =12)) +
  theme(axis.text.y = element_text(size = 8)) + theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n Electricity Usage and Churn \n", y = "\n Electricity Usage", y = "") + 
  theme(plot.title = element_text(size=12)) + theme(axis.title.x = element_text(size =12)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
g12

g13 <- ggplot(churndata, aes(y = Gas_usage2, group = Churn2)) +
  geom_boxplot(alpha = 0.6, color = "grey20", show.legend = F, fill = "steelblue") + 
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =12)) +
  theme(axis.text.y = element_text(size = 8)) + theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n Gas Usage and Churn \n", y = "\n Gas Usage", y = "") + 
  theme(plot.title = element_text(size=12)) + theme(axis.title.x = element_text(size =12)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
g13

g14 <- ggplot(churndata, aes(y = contractlevels, group = Churn2)) +
  geom_boxplot(alpha = 0.6, color = "grey20", show.legend = F, fill = "steelblue") + 
  facet_grid(~Churn2) + theme(strip.text.x = element_text(size =12)) +
  theme(axis.text.y = element_text(size = 8)) + theme(axis.text.x = element_text(size = 12)) +
  labs(title = "\n Contract Levels and Churn \n", y = "\n Contract Levels", y = "") + 
  theme(plot.title = element_text(size=12)) + theme(axis.title.x = element_text(size =12)) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
g14


grid.arrange(g1, g2, g3, nrow = 1)
grid.arrange(g4, g6, nrow = 1)
grid.arrange(g7, g8, g9, g10, nrow = 1)
grid.arrange(g11, g12, g13, g14, nrow = 1)


# HISTORGRAMS

hist(churndata$Income)
hist(churndata$Relation_length)
hist(churndata$Contract_length)
hist(churndata$Home_age)
hist(churndata$Electricity_usage)
hist(churndata$Gas_usage)

h1 <- ggplot(churndata, aes(x = Gender2)) + 
  geom_bar(color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Gender \n", x = "\n Gender", y = "Count")

h2 <- ggplot(churndata, aes(x = Age)) + 
  geom_histogram(bins = 30, color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Age \n", x = "\n Age", y = "Count")

h3 <- ggplot(churndata, aes(x = Income2)) + 
  geom_histogram(bins = 30, color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Income \n", x = "\n Income", y = "Count")

h4 <- ggplot(churndata, aes(x = Relation_length)) + 
  geom_histogram(bins = 30, color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Relation Length \n", x = "\n Relation Length", y = "Count")

h5 <- ggplot(churndata, aes(x = Contract_length)) + 
  geom_histogram(bins = 30, color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Contract Length \n", x = "\n Contract Length", y = "Count")

h6 <- ggplot(churndata, aes(x = Start_channel)) + 
  geom_bar(color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Start Channel \n", x = "\n Start Channel", y = "Count")

h7 <- ggplot(churndata, aes(x = Email_list2)) + 
  geom_bar(color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of E-Mail List \n", x = "\n E-Mail List", y = "Count")

h8 <- ggplot(churndata, aes(x = Home_age)) + 
  geom_histogram(bins = 30, color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Home Age \n", x = "\n Home Age", y = "Count")

h9 <- ggplot(churndata, aes(x = Home_label)) + 
  geom_bar(color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Home Label \n", x = "\n Home Label", y = "Count")

h10 <- ggplot(churndata, aes(x = Electricity_usage2)) + 
  geom_histogram(bins = 30, color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Electricity Usage \n", x = "\n Electricity Usage", y = "Count")

h11 <- ggplot(churndata, aes(x = Gas_usage2)) + 
  geom_histogram(bins = 30, color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Gas Usage \n", x = "\n Gas Usage", y = "Count")

h12 <- ggplot(churndata, aes(x = Province)) + 
  geom_bar(color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Province \n", x = "\n Province", y = "Count")

h13 <- ggplot(churndata, aes(x = Churn2)) + 
  geom_bar(color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Churn \n", x = "\n Churn", y = "Count")

h14 <- ggplot(churndata, aes(x = flexcontract2)) + 
  geom_bar(color = "black", fill = "#AAC8DE") +
  labs(title = "\n Distribution of Contract Type \n", x = "\n Contract Type", y = "Count")

grid.arrange(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, nrow = 4, ncol = 4)

       
