#Final Project - Initial Analysis (Assignment 2)


#installing the packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("psych")
install.packages("skimr")
install.packages("stargazer")
install.packages("corplot")
install.packages("ggplot2")
install.packages("car")
install.packages("gtsummary")
install.packages("leaps")
install.packages("nhstplot")
install.packages("reshape")
install.packages("knitr")
install.packages("ggpubr")


#importing the packages
library(dplyr)
library(tidyverse)
library(psych)
library(skimr)
library(stargazer)
library(corrplot)
library(ggplot2)
library(car)
library(gtsummary)
library(leaps)
library(nhstplot)
library(reshape)
library(writexl)
library(knitr)
library(psych)
library(car)
library(ggpubr)
library(leaps)
library(MASS)


#-----------------------------------------------------------------------------------------------------------------#

#1.SUBSET ANALYSIS:


#EDA on the subset data


#subset by Regions
north <- subset(codebook, region == "North", select=c(BB_PT:sum))
descripnorth<--psych ::describe(north)
write_xlsx(descripnorth,"descripnorth.xlsx")


south <- subset(codebook, region == "South", select=c(BB_PT:sum))
descripsouth<--psych ::describe(south)
write_xlsx(descripsouth,"descripsouth.xlsx")


west <- subset(codebook, region == "West", select=c(BB_PT:sum))
descripwest<--psych ::describe(west)
write_xlsx(descripwest,"descripwest.xlsx")


east <- subset(codebook, region == "East", select=c(BB_PT:sum))
descripeast<--psych ::describe(east)
write_xlsx(descripeast,"descripeast.xlsx")


central <- subset(codebook, region == "Central", select=c(BB_PT:sum))
descripcentral<--psych ::describe(central)
write_xlsx(descripcentral,"descripcentral.xlsx")



#------------------------------------------------------------------------------------------------------------------#

#2.CREATING NEW VARIABLES 


#by regions (processed)
regionsum=aggregate(x = codebook$sum,                        # Specify data column
                    by = list(codebook$region),              # Specify group indicator
                    FUN = sum) 

newregionsum<-regionsum[order(-regionsum$x),]



#visualization

#plot1
Plot1= ggplot(newregionsum, aes(x=Group.1, y=x))+
  ggtitle("Net Processed Crops by Region (1961- 2013)") +
  ylab("Net Processed Crops") + xlab("Region") +
  geom_bar(stat='identity', fill= "lightblue") + 
  geom_text(aes(label=round(x,digits=0) , vjust = 0)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()
Plot1



#by year (processed)
#sum by year
yearsum=aggregate(x = codebook$sum,                      # Specify data column
                  by = list(codebook$year),              # Specify group indicator
                  FUN = sum) 


#visualization

#plot2
Plot2= ggplot(yearsum, aes(x=Group.1, y=x))+
  ggtitle("Net Processed Crops by Year") +
  ylab("Net Processed Crops") + xlab("Year") +
  geom_line(stat='identity', color="blue") + 
  scale_x_discrete(limits=c(1961,1965,1969,1973,1977,1981,1985,1989,1993,1997,2001,2005,2009,2013)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()
Plot2


#----------------------------------------------------------------------------------------------------------------#


#3.MERGING ADDITIONAL DATASET

crop_production_data <- read.csv("CropProduction_formatted.csv")
crop_production_data

crop_processed_data <- read.csv("CropProcessing_editedversion.csv")
crop_processed_data


crop_data <- merge(crop_production_data, crop_processed_data, by=c("Country","Year"))
crop_data


#----------------------------------------------------------------------------------------------------------------#


#3. EDA OF THE CROP PROCESSING DATASET


#describing the data for Crop Processing dataset
Class <- class(crop_processed_data)
Class
ColNames <- colnames(crop_processed_data)
ColNames
No_of_Rows <- nrow(crop_processed_data)
No_of_Rows
Dimension_of_Data <- dim(crop_processed_data)
Dimension_of_Data



#exploratory data analysis


#data cleaning
#omiting the NA values
final_crop_processed_data <- na.omit(crop_processed_data)
head(final_crop_processed_data,5)



#descriptive analysis

#using the describe function to describe the dataset
#descriptive statistics of the dataset
describe(final_crop_processed_data, na.rm = TRUE, interp=FALSE, skew = TRUE, ranges = TRUE, trim=.1,
         type=3, check=TRUE, fast=NULL, quant=NULL, IQR=FALSE, omit=FALSE)

#describing the dataset
describeData(final_crop_processed_data,head=4,tail=4)


#data analysis (EDA)
BalesProd <- c(summary(final_crop_processed_data$BB_PT), sd(final_crop_processed_data$BB_PT))
CottonLintProd <- c(summary(final_crop_processed_data$CL_PT), sd(final_crop_processed_data$CL_PT))
CottonSeedProd <- c(summary(final_crop_processed_data$CS_PT), sd(final_crop_processed_data$CS_PT))
OilNutProd <- c(summary(final_crop_processed_data$OGN_PT), sd(final_crop_processed_data$OGN_PT))
OilSunProd <-c(summary(final_crop_processed_data$OSUN_PT), sd(final_crop_processed_data$OSUN_PT))
SugarProd <- c(summary(final_crop_processed_data$SRC_PT), sd(final_crop_processed_data$SRC_PT))
WineProd <- c(summary(final_crop_processed_data$W_PT), sd(final_crop_processed_data$W_PT))
OilCottonsProd <- c(summary(final_crop_processed_data$OCS_PT), sd(final_crop_processed_data$OCS_PT))
OilPalmProd <- c(summary(final_crop_processed_data$OP_PT), sd(final_crop_processed_data$OP_PT))
OilSoybeanProd <- c(summary(final_crop_processed_data$OSB_PT), sd(final_crop_processed_data$OSB_PT))
OilSeasmeProd <- c(summary(final_crop_processed_data$OS_PT.1), sd(final_crop_processed_data$OS_PT.1))

CropDataStat <- round(cbind(BalesProd,CottonLintProd,
                            CottonSeedProd, OilNutProd,OilSunProd, SugarProd, WineProd, OilCottonsProd,
                            OilPalmProd, OilSoybeanProd, OilSeasmeProd),2)
knitr::kable(CropDataStat, caption = "Descriptive Statistics of Crop Processing Data")



#summary
summary(final_crop_processed_data)


#summary statistics
skim(final_crop_processed_data)


#analyzing statistical results
stargazer(final_crop_processed_data,
          title = "Descriptive Analysis of the Crop Processed Data", 
          header = FALSE,
          single.row = TRUE)


#importing stargazer to table
stargazer(final_crop_processed_data,  type="html", out="CropProcessedData_SummaryOutput.html") 




#CREATING NEW VARIABLES FOR VISUALIZATION

#Crop = Cotton
cotton_Data <- cbind(final_crop_processed_data$CL_PT, final_crop_processed_data$CS_PT)


#Crop = Oil
oil_Data <- cbind(final_crop_processed_data$OGN_PT, final_crop_processed_data$OSUN_PT, 
                  final_crop_processed_data$OCS_PT ,final_crop_processed_data$OP_PT, 
                  final_crop_processed_data$OSB_PT, final_crop_processed_data$OS_PT)



#data visualization

#Data Analysis of Crop Processing Data through visualization

#graph 1: Bales of Barley Production within the region
barley<-ggplot(final_crop_processed_data,aes(y=BB_PT,x=Region)) +
  geom_bar(width=0.2,stat="identity", fill="lightblue3")+
  labs(x="Region")+
  labs(y= "Bales of Barley Production")+
  ggtitle("Bales of Barley Production within the Region")

#graph 2: Cotton Production within the region
cotton<-ggplot(final_crop_processed_data,aes(y=CL_PT,x=Region)) +
  geom_bar(width=0.2,stat="identity", fill="steelblue3")+
  labs(x="Region")+
  labs(y= "Cotton Lint Production")+
  ggtitle("Cotton Lint Production within the Region")

#graph 3: Wine Production Tonnes within the region
wine<-ggplot(final_crop_processed_data,aes(y=W_PT,x=Region)) +
  geom_bar(width=0.2,stat="identity", fill="cadetblue4")+
  labs(x="Region")+
  labs(y= "Wine Production Tonnes")+
  ggtitle("Wine Production Tonnes within the Region")

#graph 4: Sugar Production Tonnes within the region
sugar<-ggplot(final_crop_processed_data,aes(y=SRC_PT,x=Region)) +
  geom_bar(width=0.2,stat="identity", fill="deepskyblue2")+
  labs(x="Region")+
  labs(y= "Sugar Production Tonnes")+
  ggtitle("Sugar Production Tonnes within the Region")

ggarrange(barley, cotton, wine, sugar, ncol=1, nrow=4)


#graph 5: Net Production of crops within the region
ggplot(final_crop_processed_data,aes(y=Sum,x=Region)) +
  geom_boxplot(color = "darkblue")+
  labs(x="Region")+
  labs(y= "Net Production")+
  ggtitle("Net Production of Crops within the Region")



#---------------------------------------------------------------------------------------------------------------#


#4.ANALYTICAL METHODS


#a. ANALYTICAL METHOD - CORRELATION

#correlation matrix
correlation_data <- final_crop_processed_data %>% dplyr::select (-Country, -Region, -Sum, -M_PT, -OR_PT, 
                                                                 -OS_PT, -SRC_PT, -OP_PT, -PK_PT, -OCC_PT, 
                                                                 -oilmaize_production_tonnes)
colnames(correlation_data)

correlation_data <- as.data.frame(sapply(correlation_data, as.numeric))
correlation_matrix <- cor(correlation_data)
correlation_matrix


#Plot of the correlation matrix
corrplot(correlation_matrix, order="hclust", title="Plot of the Correlation Matrix for Crop Processing Data",
         number.cex=(0.50), addCoef.col = "black", mar=c(0,0,1,0))



#b. ANALYTICAL METHOD - REGRESSION ANALYSIS


#correlation value for BARLEY production in the year
correlation_value_barleyproduction <- cor(final_crop_processed_data$Year, final_crop_processed_data$BB_PT)
correlation_value_barleyproduction

#linear regression model for BARLEY production in the year
plot(final_crop_processed_data$Year, final_crop_processed_data$BB_PT, col = "cadetblue")
regression_model_1 <- lm(BB_PT ~ Year, data = final_crop_processed_data)
regression_model_1
abline(regression_model_1, col = "red")
summary(regression_model_1)

#regression table for BARLEY production in the year
tbl_regression(regression_model_1)



#correlation value for COTTON production in the year
correlation_value_cottonproduction <- cor(final_crop_processed_data$Year, final_crop_processed_data$CL_PT)
correlation_value_cottonproduction

#linear regression model for COTTON production in the year
plot(final_crop_processed_data$Year, final_crop_processed_data$CL_PT, col = "lightblue")
regression_model_2 <- lm(CL_PT ~ Year, data = final_crop_processed_data)
regression_model_2
abline(regression_model_2, col = "red")
summary(regression_model_2)

#regression table for COTTON production in the year
tbl_regression(regression_model_2)



#REGRESSION ANALYSIS ON SUBSET DATA

#linear regression model for OIL GROUND NUT production tonnes in the region
regression_model_3 <- lm(OGN_PT ~ Region, data = final_crop_processed_data)
regression_model_3
summary(regression_model_3)

#regression table for OIL GROUND NUT production tonnes in the region
tbl_regression(regression_model_3)



#linear regression model for OIL PALM production tonnes in the region
regression_model_4 <- lm(OP_PT ~ Region, data = final_crop_processed_data)
regression_model_4
summary(regression_model_4)

#regression table for OIL PALM production tonnes in the region
tbl_regression(regression_model_4)




#c. ANALYTICAL METHOD - CHI-SQUARE TESTING


#chi square test for net production of crop in the region
crop.data <- data.frame(final_crop_processed_data$Region, final_crop_processed_data$Sum)
crop.data = table(final_crop_processed_data$Region, final_crop_processed_data$Sum)

print(chisq.test(crop.data))


