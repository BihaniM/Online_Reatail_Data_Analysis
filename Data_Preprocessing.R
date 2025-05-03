install.packages("readxl")
install.packages("dplyr")
install.packages("arules")

library(readxl)
library(dplyr)
library(arules)

#Loading Data
data <- read_excel("D:\Education\MSc - Data Sceince\Semester II\Data Mining\CW\CW_Data_Mining\Online_Retail.xlsx")
data <- read_excel("D:\\Education\\MSc - Data Sceince\\Semester II\\Data Mining\\CW\\CW_Data_Mining\\Online_Retail.xlsx")

head(data)
View(data)

# Find duplicate 
sum(duplicated(data))

#Summary of Data
summary(data)

# Check missing Customer ID
sum(is.na(data$CustomerID))
data <- data %>% filter(!is.na(CustomerID))

# Check for Quantity <= 0
sum(data$Quantity <= 0)
data <- data %>% filter(Quantity > 0)

# Check for Unit Price <= 0
sum(data$UnitPrice <= 0)
data <- data %>% filter(UnitPrice > 0)

# Check missing Descriptions
sum(is.na(data$Description))
sum(data$Description == "")

# Check missing StockCodes
sum(is.na(data$StockCode))
sum(data$StockCode == "")

# Check if InvoiceNo starts with "C"
sum(grepl("^C", data$InvoiceNo))

# Description contains "Manual" or "Postage", do not related for the analysis, checking if its still available.
sum(grepl("Manual|POSTAGE", data$Description, ignore.case = TRUE))
data <- data %>% filter(!grepl("Manual|POSTAGE", Description, ignore.case = TRUE))

summary(data)
glimpse(data)

#Saving the cleaned data set
cleaned_data <- data
write.csv(cleaned_data, 
          "D:/Education/MSc - Data Sceince/Semester II/Data Mining/CW/CW_Data_Mining/cleaned_Online_Retail.csv", 
          row.names = FALSE)
