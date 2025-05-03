#Libraries 
library(dplyr)
install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)
install.packages("arules")
library(arules)

install.packages("arulesViz")
library(arulesViz)

#Load cleaned dataset
cleaned_data <- read.csv("D:/Education/MSc - Data Sceince/Semester II/Data Mining/CW/CW_Data_Mining/cleaned_Online_Retail.csv")

#Distribution Analysis

#Boxplot for Quantity
boxplot(cleaned_data$Quantity,
        main = "Boxplot of Quantity",
        col = "lightgreen",
        horizontal = TRUE)

#Unit Price Ditribution
hist(data$UnitPrice, main = "Distribution of Unit Price", xlab = "Unit Price", col = "lightgreen", breaks = 50)

data %>%
  count(Country, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(Country, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 10 Countries by Transactions", x = "Country", y = "Number of Transactions")

# For MBA we will only need Invoice Date, Customer ID and Stock Code.  
data_clean <- data %>% select(InvoiceNo, Description, Country)

#Chacek for missing values
colSums(is.na(data_clean))

#Craeting Buckets:
# Now let's find the top 3 countries with most transactions
top_countries <- data_clean %>%
  group_by(Country) %>%
  summarise(NumTransactions = n_distinct(InvoiceNo)) %>%
  arrange(desc(NumTransactions)) %>%
  top_n(3, NumTransactions)
print(top_countries)

# Filter data for each of the top 3 countries
UnitedKingdom_data <- data_clean %>% filter(Country == top_countries$Country[1])
Germany_data <- data_clean %>% filter(Country == top_countries$Country[2])
France_data <- data_clean %>% filter(Country == top_countries$Country[3])

# Creating transaction lists for each country
transactions_list1 <- split(UnitedKingdom_data$Description, country1_data$InvoiceNo)
transactions_list2 <- split(Germany_data$Description, country2_data$InvoiceNo)
transactions_list3 <- split(France_data$Description, country3_data$InvoiceNo)

#Convert transactions to Transaction object
transactions1 <- as(transactions_list1, "transactions")
transactions2 <- as(transactions_list2, "transactions")
transactions3 <- as(transactions_list3, "transactions")

inspect(head(transactions1, 5))
inspect(head(transactions2, 5))
inspect(head(transactions3, 5))

#Finding association rules, Apply Apriori on transactions1
rules1 <- apriori(transactions1,parameter = list(supp = 0.01, conf = 0.5, minlen = 2))

# Apply Apriori on transactions2
rules2 <- apriori(transactions2, parameter = list(supp = 0.01, conf = 0.5, minlen = 2))

# Apply Apriori on transactions3                
rules3 <- apriori(transactions3, parameter = list(supp = 0.01, conf = 0.5, minlen = 2))

inspect(head(rules1, 10))
inspect(head(rules2, 10))
inspect(head(rules3, 10))

# Inspect top rules
inspect(sort(rules1, by = "lift")[1:10])
inspect(sort(rules2, by = "lift")[1:10])
inspect(sort(rules3, by = "lift")[1:10])

plot(rules1, method = "grouped")
plot(rules2, method = "grouped")
plot(rules3, method = "grouped")

plot(rules1)
plot(rules2)
plot(rules3)
