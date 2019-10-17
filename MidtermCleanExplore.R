library(tidyverse)
library(forcats)


######Register Data########

#Read in register.csv.
register_data <- read_csv("register.csv", na = "--")

#The first thing I want to do is change/alter the column names.
colnames(register_data) <- c("purchase", "item", "charge", "price", "customer_type", "customer_id", "receipt", "contact_preferences",
                             "newsletter", "sales", "preferred_customer_discount")

#The data exhibits an observation scattered across two rows. Simply uisng spread() didn't work for some indexing issue.
register_data <- register_data %>% 
  group_by_at(vars(- price)) %>%           #Group by everything other than the value column.
  mutate(row_id = 1:n()) %>% 
  ungroup() %>%                            #Build group index.
  spread(key = charge, value = price) %>%  #spread
  select(- row_id)                         #Drop the index

#The customer_type variable  has 0's for university affiliated customers and 1's for unaffiliated customers. We can change this to 
#False and True, respectively.
register_data$customer_type <- as.logical(as.integer(register_data$customer_type))

#We can convert the variable contact_preferences to a factor variable.
register_data$contact_preferences <- as_factor(register_data$contact_preferences)

#Coerce the variable item from a double to a character since it's not a mathematical number, its for i.d. only.
register_data$item <- as.character(register_data$item)
#register_data$item <- as.factor(register_data$item)
#Convert the dataframe to a tibble.
register_data <- as_tibble(register_data)


######Sales Data###########

#Read in sales.csv.
sales_data <- read.csv("sales.csv", stringsAsFactors = FALSE)

#One variable category.of.inventory.goods is spread across multiple columns.
sales_data <- sales_data %>% 
  gather(X1.2018:X10.2019, key = "year", value = "monthly_amount")

#Separate the month from the year under the year variable.
sales_data <- sales_data %>% 
  separate(year, into = c("month", "year"))

#Rename the values of the variable month so that they're easier to intuit.
sales_data$month[sales_data$month == "X1"] <- "January"
sales_data$month[sales_data$month == "X2"] <- "February"
sales_data$month[sales_data$month == "X3"] <- "March"
sales_data$month[sales_data$month == "X4"] <- "April"
sales_data$month[sales_data$month == "X5"] <- "May"
sales_data$month[sales_data$month == "X6"] <- "June"
sales_data$month[sales_data$month == "X7"] <- "July"
sales_data$month[sales_data$month == "X8"] <- "August"
sales_data$month[sales_data$month == "X9"] <- "September"
sales_data$month[sales_data$month == "X10"] <- "October"
sales_data$month[sales_data$month == "X11"] <- "November"
sales_data$month[sales_data$month == "X12"] <- "December"

#Get the total purchase amount from each unique purchase (including tax).
total_individual_purchases <- rep.int(0, times = 20)
for  (i in 1:length(unique(register_data$purchase))){
  total_individual_purchases[i] <- sum(register_data$cost[register_data$purchase == i] + register_data$tax[register_data$purchase == i])
}

#Separate the inventory categories in sales_data so we may see how much each item type made per month.
monthly_sales <- sales_data %>% 
  group_by_at(vars(- monthly_amount)) %>%                                                #Group by everything other than the value column.
  mutate(row_id = 1:n()) %>% 
  ungroup() %>%                                                                          #Build group index.
  spread(key = category.of.inventory.goods, value = monthly_amount, convert = TRUE) %>%  #spread
  select(- row_id)                                                                       #Drop the index

#Make a tibble with sales data.
register_sales <- tibble(
  item = register_data$item,
  cost = register_data$cost,
  tax = register_data$tax
)

#Make a tibble of total individual purchases
total_individual_purchases <- tibble(
  purchase_num = 1:20,
  total_purchase = total_individual_purchases
)

########Visualizations#########

#How many times were each of the items purchased?
ggplot(register_data, aes(item)) +
  geom_bar()
register_data %>% 
  count(item)

#Display a boxplot showing the 5 number summary of each good sold.
ggplot(data = sales_data) +
  geom_boxplot(
    mapping = aes(
      x = reorder(category.of.inventory.goods, monthly_amount, FUN = median),
      y = monthly_amount
    )
  )

ggplot(
  data = sales_data,
  mapping = aes(x = monthly_amount)
) +
  geom_freqpoly(mapping = aes(color = category.of.inventory.goods), binwidth = 10)

ggplot(
  data = sales_data,
  mapping = aes(x = monthly_amount)
) +
  geom_freqpoly(mapping = aes(color = month), binwidth = 10)

#Who are the most frequent customer type?
ggplot(register_data, aes(customer_type)) +
  geom_bar()
register_data %>% 
  count(customer_type)



#What is the preferred contact method?
ggplot(register_data, aes(contact_preferences)) +
  geom_bar()
register_data %>% 
  count(fct_explicit_na(contact_preferences))

ggplot(data = register_data) +
  geom_point(mapping = aes(x = item, y = cost)) +
  facet_wrap(~customer_type, nrow = 2)


