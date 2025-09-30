#Load libararies that I need to performe my analysis

library(tidyverse)
library(dplyr)
library(ggplot2)
library(formattable)
library(lubridate)

#read the data from my device and view the first rows of data

chocolate_sales <- 
  read.csv("C:/Users/SHEKO/Downloads/Sales Data for Analysis/Chocolate Sales.csv"
           , stringsAsFactors = FALSE)
sales_df <- as.data.frame(chocolate_sales)
sales_df
summary(sales_df)

#cleaning data

#check for missing value

sum(is.na(chocolate_sales))

#do some operation on the data

preprocess_sales_df <- function(chocolate_sales)
{
  shape <- dim(chocolate_sales)

#convert column data types

chocolate_sales$Date <- as.Date(chocolate_sales$Date, format = "%d-%b-%y")
chocolate_sales$Amount <- 
  formattable::currency(chocolate_sales$Amount, symbol = "$")

return(chocolate_sales)
}

sales_data <- preprocess_sales_df(sales_df)
sales_data
sales_data <- sales_data %>% 
  mutate(day = day(Date), month = month(Date), year = year(Date))
sales_data

#add the column of sales (Revenue) and calculate the total sales

sales_data <- sales_data %>% mutate(Revenue = Amount * Boxes.Shipped)
sales_data
summary(sales_data)
total_sales <- sum(sales_data$Revenue)
total_sales

#calculate daily and monthly sales

daily_salse <- sales_data %>% group_by(day) %>% 
  summarise(total_daily_sales = sum(Revenue, na.rm = TRUE))
daily_salse
monthly_salse <- sales_data %>% group_by(month) %>% 
  summarise(total_monthly_sales = sum(Revenue, na.rm = TRUE))
monthly_salse

#chart of daily and monthly sales

ggplot(daily_salse, 
       aes(x = day, y = total_daily_sales)) + geom_line(color = "black") +
  labs(title = "Daily Sales", x = "Day", y = "Total Daily Sales")
ggplot(monthly_salse, 
       aes(x = month, y = total_monthly_sales)) + geom_line(color = "red") + 
  labs(title = "Monthly Sales", x = "Month", y = "Total Monthly Sales")

#analysis according to product

product_sales <- sales_data %>% group_by(Product) %>% 
  summarise(total_quantity = sum(Boxes.Shipped), 
            total_product_sales = sum(Revenue, na.rm = TRUE)) %>% 
  arrange(desc(total_product_sales))
product_sales

#chart of best-selling products

ggplot(product_sales, aes(x = reorder(Product, -total_product_sales), 
                          y = total_product_sales)) + 
  geom_bar(stat = "identity", fill = "blue") + coord_flip() + 
  labs(title = "Sales Distribution by product", x = "Product", 
       y = "Total Product Sales")

#analysis according to best-selling country

country_sales <- sales_data %>% group_by(Country) %>% 
  summarise(total_country_sales = sum(Revenue, na.rm = TRUE)) %>% 
  mutate(Percentage = percent(total_country_sales / sum(total_country_sales)),
         Lables = scales::percent(Percentage), ypos = cumsum(Percentage) - 
           0.5 * Percentage)
country_sales

#chart of best-selling country

ggplot(country_sales, aes(x = "", y = Percentage, fill = Country)) + 
  geom_bar(stat = "identity", width = 1, color = "blue") + 
  coord_polar(theta = "y") + 
  geom_text(aes(y = ypos, label = Percentage)) + theme_void() + 
  labs(title = "Sales Distribution by Country", fill = "Country")
