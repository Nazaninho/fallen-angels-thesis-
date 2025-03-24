
  #' title: MSC 
  #' author: Nazanin Hosseini
  #' subtitle: thesis
  #' date: '30.06.2024'
  #' output: pdf_document
  
  
  
##############################
## Preparatory steps

rm(list = ls())

# Packages 
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("statsmodels")
#install.packages("dplyr")
#install.packages("car")
#install.packages("lmtest")
#install.packages("sandwich")
library(rmarkdown)  # to generate pdf file
library(tinytex)    # to generate pdf file
library(readxl)     # to read Excel files
#library(sandwich)
#library(zoo)
#library(tidyr)
#library(moments)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(lubridate)

# install.packages("moments")

# Set working directory
setwd("/Users/iraho/Downloads") 


##############################
## Import data
df <- read.csv("edclb8nt8epqmlgd.csv", sep = ",",header = FALSE)

# Set the column names to the values of the second row
colnames(df) <- as.character(df[1,])

#df is a full set

# Remove the second row from the dataframe
df <- df[-1,]
print(df)
print(colnames(df))
colnames(df) <- trimws(colnames(df))

# change as.date
df$DATE                <- as.Date(df$DATE, format = "%m/%d/%Y")
df$OFFERING_DATE       <- as.Date(df$OFFERING_DATE, format = "%m/%d/%Y")
df$MATURITY            <- as.Date(df$MATURITY, format = "%m/%d/%Y")
df$DATED_DATE          <- as.Date(df$DATED_DATE, format = "%m/%d/%Y")
df$FIRST_INTEREST_DATE <- as.Date(df$FIRST_INTEREST_DATE, format = "%m/%d/%Y")
df$LAST_INTEREST_DATE  <- as.Date(df$LAST_INTEREST_DATE, format = "%m/%d/%Y")
df$T_DATE              <- as.Date(df$T_DATE, format = "%m/%d/%Y")
df$nextcoup            <- as.Date(df$nextcoup, format = "%m/%d/%Y")

# change as.number
df$ISSUE_ID           <- as.numeric(df$ISSUE_ID)
df$OFFERING_AMT       <- as.numeric(df$OFFERING_AMT)
df$OFFERING_PRICE     <- as.numeric(df$OFFERING_PRICE)
df$PRINCIPAL_AMT      <- as.numeric(df$PRINCIPAL_AMT)
df$COUPON             <- as.numeric(df$COUPON)
df$NCOUPS             <- as.numeric(df$NCOUPS)
df$AMOUNT_OUTSTANDING <- as.numeric(df$AMOUNT_OUTSTANDING)
df$N_SP               <- as.numeric(df$N_SP)
df$N_MR               <- as.numeric(df$N_MR)
df$N_FR               <- as.numeric(df$N_FR)
df$RATING_NUM         <- as.numeric(df$RATING_NUM)
df$T_Volume <- as.numeric(gsub("[\\$,]", "", df$T_Volume))
df$T_DVolume          <- as.numeric(gsub("[\\$,]", "", df$T_DVolume))
df$T_Spread           <- gsub("%", "", df$T_Spread)
df$T_Spread           <- as.numeric(df$T_Spread) / 100
df$T_Yld_Pt           <- as.numeric(df$T_Yld_Pt)
df$YIELD              <- gsub("%", "", df$YIELD)
df$YIELD              <- as.numeric(df$YIELD) 

df$PRICE_EOM          <- as.numeric(df$PRICE_EOM)
df$PRICE_LDM          <- as.numeric(df$PRICE_LDM)
df$PRICE_L5M          <- as.numeric(df$PRICE_L5M)

df$GAP                <- as.numeric(df$GAP)
df$COUPMONTH          <- as.numeric(df$COUPMONTH)
df$COUPAMT            <- as.numeric(df$COUPAMT)
df$COUPACC            <- as.numeric(df$COUPACC)

df$MULTICOUPS         <- as.numeric(df$MULTICOUPS)

df$RET_EOM            <- gsub("%", "", df$RET_EOM)
df$RET_EOM            <- as.numeric(df$RET_EOM)
df$RET_LDM <- gsub("%", "", df$RET_LDM)
df$RET_LDM <- as.numeric(df$RET_LDM)

df$RET_L5M <- gsub("%", "", df$RET_L5M)
df$RET_L5M <- as.numeric(df$RET_L5M)



df$TMT                 <- as.numeric(df$TMT)

df$REMCOUPS           <- as.numeric(df$REMCOUPS)
df$DURATION           <- as.numeric(df$DURATION)


df$CUSIP              <- as.factor(df$CUSIP)
df$bond_sym_id        <- as.factor(df$bond_sym_id)
df$bsym               <- as.factor(df$bsym)
df$ISIN               <- as.factor(df$ISIN)
df$company_symbol     <- as.factor(df$company_symbol)
df$BOND_TYPE          <- as.factor(df$BOND_TYPE)
df$SECURITY_LEVEL     <- as.factor(df$SECURITY_LEVEL)


df$TREASURY_MATURITY  <- as.factor(df$TREASURY_MATURITY)
df$R_SP               <- as.factor(df$R_SP)
df$R_MR               <- as.factor(df$R_MR)
df$R_FR               <- as.factor(df$R_FR)
df$RATING_CAT         <- as.factor(df$RATING_CAT)
df$RATING_CLASS       <- as.factor(df$RATING_CLASS)


##### 1.a ######
Alllist <- df[,]
str(Alllist)
print (Alllist)


unique_All <- Alllist %>%
  summarise(unique_All = sum(n_distinct(CUSIP)))

# Print the result
print(unique_All)

#################### FA list ############################################


# Assuming your data frame is named Alllist

# Sort the data by CUSIP and T_DATE
Alllist <- Alllist %>% 
  arrange(CUSIP, T_DATE)

# Find the first transition for each CUSIP
transition_data <- Alllist %>%
  group_by(CUSIP) %>%
  filter((RATING_CAT == "BB" |RATING_CAT == "B" | RATING_CAT == "CCC" | RATING_CAT == "CC" | RATING_CAT == "C" | RATING_CAT == "D") & 
           (lag(RATING_CAT) == "AAA" | lag(RATING_CAT) == "AA" |lag(RATING_CAT) == "A" | lag(RATING_CAT) == "BBB")) %>%
  slice(1)

# Print the result
print(transition_data)





# Convert T_DATE to character to filter properly
library(dplyr)

####table before###

# Convert T_DATE to character to filter properly
transition_data$T_DATE <- as.character(transition_data$T_DATE)

# Join Alllist with transition_data on CUSIP
Before_D <- Alllist %>%
  inner_join(transition_data, by = "CUSIP") %>%
  filter(T_DATE.x < T_DATE.y)  # Filter transactions occurring before downgrade date
 
Before_D <- Before_D[, 1:58]

# Print the result
print(Before_D)



##### table after###

# Convert T_DATE to character to filter properly
transition_data$T_DATE <- as.character(transition_data$T_DATE)

# Join Alllist with transition_data on CUSIP
After_D <- Alllist %>%
  inner_join(transition_data, by = "CUSIP") %>%
  filter(T_DATE.x >= T_DATE.y)  # Filter transactions occurring after downgrade date

# Select only the first 58 columns
After_D <- After_D[, 1:58]

# Print the result
print(After_D)

unique_cusips <- After_D %>% 
  summarise(unique_cusips = n_distinct(CUSIP))



# Print the result
print(unique_cusips)

####create table that always been IG############


always_IG <- Alllist %>%
  group_by(CUSIP) %>%
  filter(
    # Check if all ratings are in "AAA", "AA", "A", "BBB"
    all(RATING_CAT %in% c("AAA", "AA", "A", "BBB")) |
      # Check if ratings are in "AAA", "AA", "A", "BBB" and at least one observation of them is "NR", NA, or empty
      (any(RATING_CAT %in% c("AAA", "AA", "A", "BBB")) &&
         any(is.na(RATING_CAT) | RATING_CAT == "NR" | RATING_CAT == ""))
  )

print(always_IG)

unique_IG_count <- always_IG %>%
  summarise(unique_IG_count = n_distinct(CUSIP))

# Print the result
print(unique_IG_count)


print(always_IG)

unique_IG_count <- always_IG %>%
  summarise(unique_IG_count = n_distinct(CUSIP))

# Print the result
print(unique_IG_count)


# Filter CUSIPs in always_IG table with ratings in the specified categories
violating_cusips <- always_IG %>%
  filter(RATING_CAT %in% c("BB", "B", "CCC", "CC", "C", "D")) %>%
  distinct(CUSIP)

# Print the violating CUSIPs
print(violating_cusips)



####create table that always been HY############


always_HY <- Alllist %>%
  group_by(CUSIP) %>%
  filter(
    # Check if all ratings are in "BB", "B", "CCC", "CC", "C", "D"
    all(RATING_CAT %in% c("BB", "B", "CCC", "CC", "C", "D")) |
      # Check if ratings are in "BB", "B", "CCC", "CC", "C", "D" and at least one observation of them is "NR", NA, or empty
      (any(RATING_CAT %in% c("BB", "B", "CCC", "CC", "C", "D")) &&
         any(is.na(RATING_CAT) | RATING_CAT == "NR" | RATING_CAT == ""))
  )


print(always_HY)

unique_HY_count <- always_HY %>%
  summarise(unique_HY_count = sum(n_distinct(CUSIP)))

# Print the result
print(unique_HY_count)

violating_cusips <- always_HY %>%
  filter(RATING_CAT %in% c("BBB", "AA", "AAA","A")) %>%
  distinct(CUSIP)

# Print the violating CUSIPs
print(violating_cusips)

####create table that always dont have rating############

always_noRat <- Alllist %>%
  group_by(CUSIP) %>%
  filter(all(is.na(RATING_CAT) | RATING_CAT %in% c("NR")| RATING_CAT == ""))

print(always_noRat)

unique_no_rat <- always_noRat %>%
  summarise(unique_no_rat = sum(n_distinct(CUSIP)))

# Print the result
print(unique_no_rat)


violating_cusips <- always_noRat %>%
  filter(RATING_CAT %in% c("BBB", "AA", "AAA", "CCC", "BB")) %>%
  distinct(CUSIP)

# Print the violating CUSIPs
print(violating_cusips)





############# Table 1 stat ############################################

##### Panel A Rating downgrade #####



library(dplyr)

# Define a function to assign numbers to RATING_CAT
assign_rating_number <- function(rating_cat) {
  rating_map <- c("AAA" = 10, "AA" = 9, "A"= 8, "BBB" = 7, "BB" = 6, "B" = 5, "CCC" = 4, "CC" = 3, "C" = 2, "D" = 1)
  return(rating_map[rating_cat])
}

# Define a function to calculate the difference in ratings
calculate_rating_difference <- function(rating1, rating2) {
  return(abs(rating1 - rating2))
}

# Find the last historical observation for each CUSIP before downgrade
Before_D_last <- Before_D %>%
  group_by(CUSIP) %>%
  slice(n()) %>%
  mutate(rating_number = assign_rating_number(RATING_CAT.x))

After_D_latest <- After_D %>%
  group_by(CUSIP) %>%
  slice(n()) %>%
  mutate(rating_number = assign_rating_number(RATING_CAT.x))

# Join the two tables to calculate the rating difference
rating_difference <- Before_D_last %>%
  inner_join(After_D_latest, by = "CUSIP") %>%
  mutate(rating_diff = calculate_rating_difference(rating_number.x, rating_number.y))

# Count the unique CUSIPs for each rating difference
rating_diff_count <- rating_difference %>%
  group_by(rating_diff) %>%
  summarise(unique_cusips = n_distinct(CUSIP))

# Calculate the average TMT and RET_EOM for each group of rating differences
rating_diff_summary <- rating_difference %>%
  group_by(rating_diff) %>%
  summarise(avg_TMT = mean(TMT.x.x, na.rm = TRUE),
            avg_RET_EOM = mean(RET_EOM.x.x, na.rm = TRUE))


print(rating_diff_summary)


##### Panel B rating after downgrade #####

# Extract unique CUSIPs from the dataset
unique_cusips <- After_D %>%
  distinct(CUSIP) %>%
  pull()

# Initialize a data frame to store the counts
rating_counts <- data.frame(RATING_CAT = c("BB", "B", "CCC", "CC", "C","D"),
                            unique_CUSIPs = 0)

# Loop through each unique CUSIP
for (cusip in unique_cusips) {
  # Find the earliest historical rating category for the CUSIP
  earliest_rating <- After_D %>%
    filter(CUSIP == cusip) %>%
    slice_min(T_DATE.x) %>%
    pull(RATING_CAT.x)
  
  # Increment the count for the corresponding rating category
  rating_counts[rating_counts$RATING_CAT == earliest_rating, "unique_CUSIPs"] <- 
    rating_counts[rating_counts$RATING_CAT == earliest_rating, "unique_CUSIPs"] + 1
}

# Print the result
print(rating_counts)

# Filter to select only the earliest observation for each CUSIP
earliest_observation <- After_D %>%
  group_by(CUSIP) %>%
  slice_min(T_DATE.x)

# Calculate the averages for each rating category
averages <- earliest_observation %>%
  group_by(RATING_CAT.x) %>%
  summarise(avg_TMT = mean(TMT.x, na.rm = TRUE),
            avg_RET_EOM = mean(RET_EOM.x, na.rm = TRUE))

# Print the result
print(averages)

#check: ok

# Group the data by rating category and calculate the averages
averages <- transition_data %>%
  group_by(RATING_CAT) %>%
  summarise(avg_TMT = mean(TMT, na.rm = TRUE),
            avg_RET_EOM = mean(RET_EOM, na.rm = TRUE))

# Print the result
print(averages)


##### Panel C Seniority level #####


# Step 1: Count the number of unique CUSIPs for each SECURITY_LEVEL.x
cusip_count <- transition_data %>%
  group_by(SECURITY_LEVEL) %>%
  summarise(unique_cusips = n_distinct(CUSIP))

# Step 2: Calculate the average TMT and RET_OEM within each SECURITY_LEVEL.x group
avg_security <- transition_data %>%
  group_by(SECURITY_LEVEL) %>%
  summarise(
    avg_TMT = mean(TMT, na.rm = TRUE),
    avg_RET_EOM = mean(RET_EOM, na.rm = TRUE)
  )

# Print the results
print(cusip_count)
print(avg_security)


##### Panel D bond type #####

cusip_count_1 <- transition_data %>%
  group_by(BOND_TYPE) %>%
  summarise(unique_cusips = n_distinct(CUSIP))

# Step 2: Calculate the average TMT and RET_OEM within each bond type
avg_type <- transition_data %>%
  group_by(BOND_TYPE) %>%
  summarise(
    avg_TMT = mean(TMT, na.rm = TRUE),
    avg_RET_EOM = mean(RET_EOM, na.rm = TRUE)
  )

# Print the results
print(cusip_count_1)
print(avg_type)

##### Panel F offering amount ######


# Define the breakpoints for the groups
breakpoints <- c(0, 10000, 50000, 100000, 200000, 300000, 400000, 500000, 1000000, Inf)

# Create a new grouping variable based on OFFERING_AMT
transition_data <- transition_data %>%
  mutate(amt_group = cut(OFFERING_AMT, breaks = breakpoints, labels = c("<10k", "10k-50k", "50k-100k", "100k-200k", "200k-300k", "300k-400k", "400k-500k", "500k-1M", ">1M")))

# Count the number of unique CUSIPs within each group
unique_cusips_by_amt <- transition_data %>%
  group_by(amt_group) %>%
  summarise(unique_cusips = n_distinct(CUSIP))

# Print the result
print(unique_cusips_by_amt)


# Calculate average TMT and RET_EOM within each category
avg_tmt_ret_by_amt <- transition_data %>%
  group_by(amt_group) %>%
  summarise(avg_TMT = mean(TMT, na.rm = TRUE),
            avg_RET_EOM = mean(RET_EOM, na.rm = TRUE))

# Print the result
print(avg_tmt_ret_by_amt)


######################## Table 2 #############
##############################################

####### Benchmark ############



# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)

# Read the CSV file
data <- read_csv("C:/Users/iraho/Downloads/MT data/BAMLH0A0HYM2EY.csv" )

# Assuming the structure of the data has columns 'Date' and 'Index'
# Convert 'Date' to Date format
data$Date <- as.Date(data$DATE, format = "%Y-%m-%d")
# Ensure the 'BAMLH0A0HYM2EY' column is numeric
data$Index <- as.numeric(data$BAMLH0A0HYM2EY, na.rm = TRUE) # Convert with NA handling

# Calculate daily returns if not already in the data
data <- data %>%
  mutate(daily_return = (Index / lag(Index) - 1))

# Calculate annual returns from daily returns
annual_returns <- data %>%
  group_by(Year = year(Date)) %>%
  summarize(
    # Compute the product of 1 + daily returns for the year, then subtract 1 to get annual growth
    Annual_Return = (prod(1 + daily_return, na.rm = TRUE) - 1) * 100
  ) %>%
  ungroup()

# Calculate equivalent monthly returns for each year
annual_returns <- annual_returns %>%
  mutate(Equivalent_Monthly_Return = (1 + Annual_Return/100)^(1/12) - 1) %>%
  mutate(Equivalent_Monthly_Return = Equivalent_Monthly_Return * 100)

# Print the annual returns with equivalent monthly returns
print(annual_returns)




# FA

# Find the earliest observation for each CUSIP
earliest_observation <- After_D %>%
  group_by(CUSIP) %>%
  slice_min(T_DATE.x) %>%
  ungroup()

# Extract year from DATE_T.x column
earliest_observation <- earliest_observation %>%
  mutate(year = lubridate::year(T_DATE.x))

# Group by year and count unique CUSIPs
unique_cusips_by_year <- earliest_observation %>%
  group_by(year) %>%
  summarise(unique_cusips = n_distinct(CUSIP))

# Print the result
print(unique_cusips_by_year)


# Find the earliest historical T_DATE for each CUSIP
earliest_observation <- After_D %>%
  group_by(CUSIP) %>%
  summarise(earliest_year = min(lubridate::year(T_DATE.x)))

# Join the earliest year information back to the After_D table
After_D <- left_join(After_D, earliest_observation, by = "CUSIP")

# Calculate the average values by the assigned earliest year for each CUSIP
average_by_year <- After_D %>%
  group_by(earliest_year) %>%
  summarise(avg_TMT = mean(TMT.x, na.rm = TRUE),
            median_TMT = median(TMT.x, na.rm = TRUE),
            avg_RET_EOM = mean(RET_EOM.x, na.rm = TRUE),
            median_RET_EOM = median(RET_EOM.x, na.rm = TRUE),
            avg_T_DVolume = mean(T_DVolume.x, na.rm = TRUE) / 100000,
            median_T_DVolume = median(T_DVolume.x, na.rm = TRUE) / 100000,
            avg_YIELD = mean(YIELD.x, na.rm = TRUE),
            median_YIELD = median(YIELD.x, na.rm = TRUE))

# Print the result
print(average_by_year)




##################### Table 3 ######################
####################################################

##### Always IG #####


library(dplyr)

always_IG_metrics <- always_IG %>%
  summarise(
    unique_cusips_count = n_distinct(CUSIP),
    avg_OFFERING_AMT = mean(OFFERING_AMT, na.rm = TRUE),
    count_T_DATE = n(),
    avg_T_DATE_per_CUSIP = sum(count_T_DATE) / n_distinct(CUSIP),
    avg_T_DVolume = mean(T_DVolume, na.rm = TRUE),
    avg_RET_EOM = mean(RET_EOM, na.rm = TRUE),
    avg_PRICE_EOM = mean(PRICE_EOM, na.rm = TRUE),
    avg_TMT = mean(TMT, na.rm = TRUE),
    avg_DURATION = mean(DURATION, na.rm = TRUE),
    median_T_DVolume = median(T_DVolume, na.rm = TRUE),
    median_RET_EOM = median(RET_EOM, na.rm = TRUE),
    median_PRICE_EOM = median(PRICE_EOM, na.rm = TRUE),
    median_TMT = median(TMT, na.rm = TRUE),
    median_DURATION = median(DURATION, na.rm = TRUE)
  ) %>%
  summarise(
    unique_cusips_count = sum(unique_cusips_count),
    avg_OFFERING_AMT = mean(avg_OFFERING_AMT),
    count_T_DATE = sum(count_T_DATE),
    avg_T_DATE_per_CUSIP = mean(avg_T_DATE_per_CUSIP),
    avg_T_DVolume = mean(avg_T_DVolume),
    avg_RET_EOM = mean(avg_RET_EOM, na.rm = TRUE),
    avg_PRICE_EOM = mean(avg_PRICE_EOM),
    avg_TMT = mean(avg_TMT, na.rm = TRUE),  # Fix NaN values
    avg_DURATION = mean(avg_DURATION, na.rm = TRUE),  # Fix NaN values
    median_T_DVolume = median(median_T_DVolume),
    median_RET_EOM = median(median_RET_EOM, na.rm = TRUE),
    median_PRICE_EOM = median(median_PRICE_EOM),
    median_TMT = median(median_TMT, na.rm = TRUE),  # Fix NaN values
    median_DURATION = median(median_DURATION, na.rm = TRUE)
  )

print(always_IG_metrics)


  
##### before downgrade ##### 
  
  
before_D_metrics <- Before_D %>%
  summarise(
    unique_cusips_count = n_distinct(CUSIP),
    avg_OFFERING_AMT = mean(OFFERING_AMT.x, na.rm = TRUE),
    count_T_DATE = n(),
    avg_T_DATE_per_CUSIP = sum(count_T_DATE) / n_distinct(CUSIP),
    avg_T_DVolume = mean(T_DVolume.x, na.rm = TRUE),
    avg_RET_EOM = mean(RET_EOM.x, na.rm = TRUE),
    avg_PRICE_EOM = mean(PRICE_EOM.x, na.rm = TRUE),
    avg_TMT = mean(TMT.x, na.rm = TRUE),
    avg_DURATION = mean(DURATION.x, na.rm = TRUE),
    median_T_DVolume = median(T_DVolume.x, na.rm = TRUE),
    median_RET_EOM = median(RET_EOM.x, na.rm = TRUE),
    median_PRICE_EOM = median(PRICE_EOM.x, na.rm = TRUE),
    median_TMT = median(TMT.x, na.rm = TRUE),
    median_DURATION = median(DURATION.x, na.rm = TRUE)
  ) %>%
  summarise(
    unique_cusips_count = sum(unique_cusips_count),
    avg_OFFERING_AMT = mean(avg_OFFERING_AMT),
    count_T_DATE = sum(count_T_DATE),
    avg_T_DATE_per_CUSIP = mean(avg_T_DATE_per_CUSIP),
    avg_T_DVolume = mean(avg_T_DVolume),
    avg_RET_EOM = mean(avg_RET_EOM, na.rm = TRUE),
    avg_PRICE_EOM = mean(avg_PRICE_EOM),
    avg_TMT = mean(avg_TMT,na.rm = TRUE),
    avg_DURATION = mean(avg_DURATION,na.rm = TRUE),
    median_T_DVolume = median(median_T_DVolume),
    median_RET_EOM = median(median_RET_EOM, na.rm = TRUE),
    median_PRICE_EOM = median(median_PRICE_EOM),
    median_TMT = median(median_TMT,na.rm = TRUE),
    median_DURATION = median(median_DURATION,na.rm = TRUE)
  )

print(before_D_metrics)



##### After downgrade #####



after_D_metrics <- After_D %>%
  summarise(
    unique_cusips_count = n_distinct(CUSIP),
    avg_OFFERING_AMT = mean(OFFERING_AMT.x, na.rm = TRUE),
    count_T_DATE = n(),
    avg_T_DATE_per_CUSIP = sum(count_T_DATE) / n_distinct(CUSIP),
    avg_T_DVolume = mean(T_DVolume.x, na.rm = TRUE),
    avg_RET_EOM = mean(RET_EOM.x, na.rm = TRUE),
    avg_PRICE_EOM = mean(PRICE_EOM.x, na.rm = TRUE),
    avg_TMT = mean(TMT.x, na.rm = TRUE),
    avg_DURATION = mean(DURATION.x, na.rm = TRUE),
    median_T_DVolume = median(T_DVolume.x, na.rm = TRUE),
    median_RET_EOM = median(RET_EOM.x, na.rm = TRUE),
    median_PRICE_EOM = median(PRICE_EOM.x, na.rm = TRUE),
    median_TMT = median(TMT.x, na.rm = TRUE),
    median_DURATION = median(DURATION.x, na.rm = TRUE)
  ) %>%
  summarise(
    unique_cusips_count = sum(unique_cusips_count),
    avg_OFFERING_AMT = mean(avg_OFFERING_AMT),
    count_T_DATE = sum(count_T_DATE),
    avg_T_DATE_per_CUSIP = mean(avg_T_DATE_per_CUSIP),
    avg_T_DVolume = mean(avg_T_DVolume),
    avg_RET_EOM = mean(avg_RET_EOM,na.rm = TRUE),
    avg_PRICE_EOM = mean(avg_PRICE_EOM),
    avg_TMT = mean(avg_TMT,na.rm = TRUE),
    avg_DURATION = mean(avg_DURATION,na.rm = TRUE),
    median_T_DVolume = median(median_T_DVolume),
    median_RET_EOM = median(median_RET_EOM,na.rm = TRUE),
    median_PRICE_EOM = median(median_PRICE_EOM),
    median_TMT = median(median_TMT,na.rm = TRUE),
    median_DURATION = median(median_DURATION,na.rm = TRUE)
  )

print(after_D_metrics)



##### Always HY #####




always_HY_metrics <- always_HY %>%
  summarise(
    unique_cusips_count = n_distinct(CUSIP),
    avg_OFFERING_AMT = mean(OFFERING_AMT, na.rm = TRUE),
    count_T_DATE = n(),
    avg_T_DATE_per_CUSIP = sum(count_T_DATE) / n_distinct(CUSIP),
    avg_T_DVolume = mean(T_DVolume, na.rm = TRUE),
    avg_RET_EOM = mean(RET_EOM, na.rm = TRUE),
    avg_PRICE_EOM = mean(PRICE_EOM, na.rm = TRUE),
    avg_TMT = mean(TMT, na.rm = TRUE),
    avg_DURATION = mean(DURATION, na.rm = TRUE),
    median_T_DVolume = median(T_DVolume, na.rm = TRUE),
    median_RET_EOM = median(RET_EOM, na.rm = TRUE),
    median_PRICE_EOM = median(PRICE_EOM, na.rm = TRUE),
    median_TMT = median(TMT, na.rm = TRUE),
    median_DURATION = median(DURATION, na.rm = TRUE)
  ) %>%
  summarise(
    unique_cusips_count = sum(unique_cusips_count),
    avg_OFFERING_AMT = mean(avg_OFFERING_AMT),
    count_T_DATE = sum(count_T_DATE),
    avg_T_DATE_per_CUSIP = mean(avg_T_DATE_per_CUSIP),
    avg_T_DVolume = mean(avg_T_DVolume),
    avg_RET_EOM = mean(avg_RET_EOM,na.rm = TRUE),
    avg_PRICE_EOM = mean(avg_PRICE_EOM),
    avg_TMT = mean(avg_TMT, na.rm = TRUE),
    avg_DURATION = mean(avg_DURATION, na.rm = TRUE),
    median_T_DVolume = median(median_T_DVolume),
    median_RET_EOM = median(median_RET_EOM,na.rm = TRUE),
    median_PRICE_EOM = median(median_PRICE_EOM),
    median_TMT = median(median_TMT, na.rm = TRUE),
    median_DURATION = median(median_DURATION, na.rm = TRUE)
  )

print(always_HY_metrics)


##### always no rating #####

always_no_rating <- always_noRat %>%
  summarise(
    unique_cusips_count = n_distinct(CUSIP),
    avg_OFFERING_AMT = mean(OFFERING_AMT, na.rm = TRUE),
    count_T_DATE = n(),
    avg_T_DATE_per_CUSIP = sum(count_T_DATE) / n_distinct(CUSIP),
    avg_T_DVolume = mean(T_DVolume, na.rm = TRUE),
    avg_RET_EOM = mean(RET_EOM, na.rm = TRUE),
    avg_PRICE_EOM = mean(PRICE_EOM, na.rm = TRUE),
    avg_TMT = mean(TMT, na.rm = TRUE),
    avg_DURATION = mean(DURATION, na.rm = TRUE),
    median_T_DVolume = median(T_DVolume, na.rm = TRUE),
    median_RET_EOM = median(RET_EOM, na.rm = TRUE),
    median_PRICE_EOM = median(PRICE_EOM, na.rm = TRUE),
    median_TMT = median(TMT, na.rm = TRUE),
    median_DURATION = median(DURATION, na.rm = TRUE)
  ) %>%
  summarise(
    unique_cusips_count = sum(unique_cusips_count),
    avg_OFFERING_AMT = mean(avg_OFFERING_AMT,na.rm = TRUE),
    count_T_DATE = sum(count_T_DATE),
    avg_T_DATE_per_CUSIP = mean(avg_T_DATE_per_CUSIP),
    avg_T_DVolume = mean(avg_T_DVolume),
    avg_RET_EOM = mean(avg_RET_EOM,na.rm = TRUE),
    avg_PRICE_EOM = mean(avg_PRICE_EOM),
    avg_TMT = mean(avg_TMT,na.rm = TRUE),
    avg_DURATION = mean(avg_DURATION,na.rm = TRUE),
    median_T_DVolume = median(median_T_DVolume),
    median_RET_EOM = median(median_RET_EOM,na.rm = TRUE),
    median_PRICE_EOM = median(median_PRICE_EOM),
    median_TMT = median(median_TMT,na.rm = TRUE),
    median_DURATION = median(median_DURATION,na.rm = TRUE)
  )

print(always_no_rating)


##### all table #####

always_all <- Alllist %>%
  summarise(
    unique_cusips_count = n_distinct(CUSIP),
    avg_OFFERING_AMT = mean(OFFERING_AMT, na.rm = TRUE),
    count_T_DATE = n(),
    avg_T_DATE_per_CUSIP = sum(count_T_DATE) / n_distinct(CUSIP),
    avg_T_DVolume = mean(T_DVolume, na.rm = TRUE),
    avg_RET_EOM = mean(RET_EOM, na.rm = TRUE),
    avg_PRICE_EOM = mean(PRICE_EOM, na.rm = TRUE),
    avg_TMT = mean(TMT, na.rm = TRUE),
    avg_DURATION = mean(DURATION, na.rm = TRUE),
    median_T_DVolume = median(T_DVolume, na.rm = TRUE),
    median_RET_EOM = median(RET_EOM, na.rm = TRUE),
    median_PRICE_EOM = median(PRICE_EOM, na.rm = TRUE),
    median_TMT = median(TMT, na.rm = TRUE),
    median_DURATION = median(DURATION, na.rm = TRUE)
  ) %>%
  summarise(
    unique_cusips_count = sum(unique_cusips_count),
    avg_OFFERING_AMT = mean(avg_OFFERING_AMT, na.rm = TRUE),
    count_T_DATE = sum(count_T_DATE),
    avg_T_DATE_per_CUSIP = mean(avg_T_DATE_per_CUSIP),
    avg_T_DVolume = mean(avg_T_DVolume),
    avg_RET_EOM = mean(avg_RET_EOM),
    avg_PRICE_EOM = mean(avg_PRICE_EOM),
    avg_TMT = mean(avg_TMT, na.rm = TRUE),
    avg_DURATION = mean(avg_DURATION, na.rm = TRUE),
    median_T_DVolume = median(median_T_DVolume),
    median_RET_EOM = median(avg_RET_EOM),
    median_PRICE_EOM = median(median_PRICE_EOM),
    median_TMT = median(median_TMT, na.rm = TRUE),
    median_DURATION = median(median_DURATION, na.rm = TRUE)
  )

print(always_all)


########### Empirical Study #######
###################################
###################################

########## Return pattern #########


# Remove earliest year

# Get the index of the "earliest_year" column
column_index <- which(names(After_D) == "earliest_year")

# Remove the column using negative indexing
After_D <- After_D[, -column_index]

##

# Add a column to Before_D to indicate the source
Before_D$observation_source <- 0

# Add a column to After_D to indicate the source
After_D$observation_source <- 1

# Stack Before_D and After_D vertically
merged_table <- rbind(Before_D, After_D)

# Sort the merged table by CUSIP
merged_table <- merged_table[order(merged_table$CUSIP), ]

print(merged_table)


# Rename the observation source column to downgrade_event
merged_table <- merged_table %>%
  rename(downgrade_event = observation_source)

# Find the corresponding transition dates for each CUSIP in merged_table
merged_table$transition_date <- transition_data$T_DATE[match(merged_table$CUSIP, transition_data$CUSIP)]

# Print the updated table
print(merged_table)

# days before after downgrade

# Calculate the difference in days between T_DATE.x and Transition_date
merged_table <- merged_table %>%
  mutate(date_difference = as.numeric(round(difftime(T_DATE.x, transition_date, units = "days"))))


library(tidyr)

# trades around downgrades

unique_date_difference <- merged_table %>%
  group_by(date_difference) %>%
  summarise(count_observation = n())

print(unique_date_difference)

library(ggplot2)

# Plot histogram of frequency of trading
ggplot(unique_date_difference, aes(x = date_difference, y = count_observation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Observations by Date Difference",
       x = "Date Difference",
       y = "Frequency") +
  theme_minimal()



library(ggplot2)
library(dplyr)

# Assuming 'unique_date_difference' is your data frame

# Filter the data to limit the x-axis range
filtered_data <- unique_date_difference %>%
  filter(date_difference >= -365*3 & date_difference <= 365*3 & date_difference != 0)

# Calculate the mean and standard deviation of the date differences
mean_diff <- mean(filtered_data$date_difference)
sd_diff <- sd(filtered_data$date_difference)

# Create a sequence of x values for the normal distribution
x_values <- seq(-365*3, 365*3, length.out = 1000)

# Calculate the normal distribution values
normal_dist <- dnorm(x_values, mean = mean_diff, sd = sd_diff) * max(filtered_data$count_observation) / max(dnorm(x_values, mean = mean_diff, sd = sd_diff))

# Create a data frame for the normal distribution
normal_dist_df <- data.frame(x_values = x_values, normal_dist = normal_dist)


# Plot the histogram with the normal distribution overlay
plot <- ggplot(filtered_data, aes(x = date_difference, y = count_observation)) +
  geom_bar(stat = "identity", fill = "skyblue", aes(color = "Frequency")) +
  geom_line(data = normal_dist_df, aes(x = x_values, y = normal_dist, color = "Normal Distribution"), size = 1) +
  labs(title = "Frequency of Observations by Date Difference",
       x = "Date Difference",
       y = "Frequency") +
  scale_x_continuous(breaks = seq(-365*3, 365*3, by = 100)) +
  scale_color_manual(values = c("Frequency" = "skyblue", "Normal Distribution" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(title = "Legend"))

# Save the plot as a PNG file
ggsave("frequency_distribution.png", plot = plot, width = 12, height = 6, dpi = 300)

# Display the plot
print(plot)



################# T-bills excess return#############

# Set working directory
setwd("/Users/iraho/Downloads/MT data") 


TB30 <- read.csv("1m T bill monthly eom.csv", sep = ",", header = TRUE)


# Step 2: Check the data types of the columns
str(TB30)

# Step 3: Convert the DATE column to Date format (assuming the initial format is "%Y-%m-%d")
TB30$DATE <- as.Date(TB30$DATE, format = "%Y-%m-%d")

# Verify the conversion
str(TB30)
head(TB30)


# Identify non-numeric values in the DGS1MO column
non_numeric_values <- TB30$DGS1MO[!grepl("^[0-9.]+$", TB30$DGS1MO)]
print(non_numeric_values)

# Optionally, replace non-numeric values with NA
TB30$DGS1MO[!grepl("^[0-9.]+$", TB30$DGS1MO)] <- NA

# Convert DGS1MO column to numeric
TB30$DGS1MO <- as.numeric(TB30$DGS1MO)

# Check for any NAs introduced by the conversion
if (any(is.na(TB30$DGS1MO))) {
  warning("There are NAs in the DGS1MO column after conversion. Please check the data format.")
}

# Check the final structure of the data
str(TB30)
head(TB30)


# Calculate excess return

# Convert annual interest rate to monthly rate
TB30$DGS1MO_monthly <- TB30$DGS1MO / 12

# Ensure DATE in TB30 is in Date format (if not already)
TB30$DATE <- as.Date(TB30$DATE, format = "%Y-%m-%d")

# Calculate excess_return_eom without merging tables
merged_table$excess_return_eom <- with(merged_table, RET_EOM.x - TB30$DGS1MO_monthly[match(T_DATE.x, TB30$DATE)])

# Check for any resulting NAs to verify matching went correctly
sum(is.na(merged_table$excess_return_eom))

sum(is.na(merged_table$RET_EOM.x))




############ graph ##############
# Calculate median return_eom.x for each date difference
median_excess_return <- merged_table %>%
  group_by(date_difference) %>%
  summarise(median_return_eom = median(excess_return_eom, na.rm = TRUE))

# Calculate average return_eom.x for each date difference
average_excess_return <- merged_table %>%
  group_by(date_difference) %>%
  summarise(average_return_eom = mean(excess_return_eom, na.rm = TRUE))

library(ggplot2)

# Combine the median and average excess return data into one data frame
combined_returns <- merge(median_excess_return, average_excess_return, by = "date_difference", suffixes = c("_median", "_average"))

# Plot the data

ggplot(combined_returns, aes(x = date_difference)) +
  geom_line(aes(y = median_return_eom, colour = "Median"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = average_return_eom, colour = "Average"), size = 1, alpha = 0.8) +
  geom_point(aes(y = median_return_eom, colour = "Median"), size = 2, shape = 21, fill = "white") +
  scale_color_manual(values = c("Median" = "red", "Average" = "blue")) +
  labs(title = "Median and Average Returns over Date Differences",
       x = "Date Difference (days)",
       y = "Returns",
       color = "Return Type") +
  theme_minimal() +
  coord_cartesian(xlim = c(-365*3, 365*3)) 




 ##################   Panel regression ###############
library(plm)
library(lubridate)


# Create a month variable from the date difference, where 0 is treated as part of month 1
merged_table <- merged_table %>%
  mutate(month = ifelse(date_difference == 0, 1, ceiling(abs(date_difference) / 30) * sign(date_difference)))

# Generate dummies for each time window from -24 to +36, skipping 0
time_windows <- c(seq(-24, -1), seq(1, 36))  # Adjusted range to skip 0

for (window in time_windows) {
  merged_table <- merged_table %>%
    mutate(!!paste0("Dummy_", window) := ifelse(month == window, 1, 0))
}





# Assuming 'date_difference' is the number of days around the downgrade
merged_table <- merged_table %>%
  mutate(
    quarter = ifelse(date_difference == 0, 1, ceiling(abs(date_difference) / 90) * sign(date_difference))  # Convert days to quarters
  )

# Define the range for the quarters; for example, 5 quarters before to 5 quarters after
quarter_windows <- c(seq(-8, -1), seq(1, 12))   # Adjust this range as needed

for (q in quarter_windows) {
  merged_table <- merged_table %>%
    mutate(!!paste0("Dummy_Q", q) := ifelse(quarter == q, 1, 0))
}

colnames(merged_table)



# First, identify the downgrade year for each CUSIP when date_difference is 0
downgrade_years <- merged_table %>%
  filter(date_difference == 0) %>%
  distinct(CUSIP, T_DATE.x) %>%
  mutate(downgrade_year = format(as.Date(T_DATE.x), "%Y")) %>%
  mutate(downgrade_year = ifelse(downgrade_year == "2002", NA, downgrade_year)) %>%
  drop_na() %>%
  select(CUSIP, downgrade_year)

# Create a wide format of dummy variables for each year (excluding the reference year 2002)
downgrade_year_dummies <- downgrade_years %>%
  mutate(dummy = 1) %>%
  pivot_wider(names_from = downgrade_year, values_from = dummy, values_fill = list(dummy = 0)) %>%
  rename_with(~ gsub("([0-9]{4})", "Dummy_Year_\\1", .x))

# Left join these dummies back to the original dataset on CUSIP
merged_table <- merged_table %>%
  left_join(downgrade_year_dummies, by = "CUSIP")

# Check the structure and sample data
print(str(merged_table))
print(head(merged_table))




# Adjust the return calculation to account for compounding, properly handling NAs
merged_table <- merged_table %>%
  group_by(CUSIP) %>%
  mutate(
    # Adjust returns to be compounded, and handle NAs by assuming no change (0% return)
    adj_return = if_else(is.na(excess_return_eom), 1, 1 + excess_return_eom / 100)
  ) %>%
  arrange(T_DATE.x) %>%
  # Calculate cumulative returns, compounding across all dates within each CUSIP
  mutate(
    # Start the cumulative product calculation, subtract 1 at the end for actual cumulative return
    cum_return_to_date = cumprod(adj_return) - 1
  ) %>%
  ungroup()

# Adjust the calculation to display as percentage by multiplying by 100, if necessary
merged_table <- merged_table %>%
  mutate(
    cum_return_to_date = cum_return_to_date * 100
  )


# Check: Filter the merged_table for the specific CUSIP and select relevant columns
specific_cusip_data <- merged_table %>%
  filter(CUSIP == "893817AA4") %>%
  dplyr::select(CUSIP, T_DATE.x, excess_return_eom, cum_return_to_date,date_difference)

# Print the data
print(specific_cusip_data)

# Assuming 'Dummy_1' is the dummy variable for a specific time window where it equals 1
# Filter the merged_table for the specific CUSIP and where Dummy_1 equals 1
specific_cusip_data <- merged_table %>%
  filter(CUSIP == "893817AA4", Dummy_2 == 1) %>%
  dplyr::select(CUSIP, T_DATE.x, excess_return_eom, cum_return_to_date, date_difference, Dummy_2)

# Print the data
print(specific_cusip_data)


# Summary table
summary_by_window <- merged_table %>%
  filter(month >= -24 & month <= 36) %>%
  group_by(month) %>%
  summarise(
    Avg_Cum_Return = mean(cum_return_to_date, na.rm = TRUE),
    Median_Cum_Return = median(cum_return_to_date, na.rm = TRUE),
    Count = n()
  )

# Print the summary table
print(n=61,summary_by_window)


# Rating mapping vector
rating_map <- c("AAA" = 10, "AA" = 9, "A" = 8, "BBB" = 7,
                "BB" = 6, "B" = 5, "CCC" = 4, "CC" = 3, "C" = 2, "D" = 1)

# Ensure RATING_CAT.x values are trimmed and correctly formatted
Before_D <- Before_D %>%
  mutate(RATING_CAT.x = trimws(as.character(RATING_CAT.x)))

# Apply the mapping to the RATING_CAT.x
Before_D <- Before_D %>%
  mutate(numeric_rating = rating_map[RATING_CAT.x])

# Find the last observation for each CUSIP before downgrade in Before_D
last_rating_before_downgrade <- Before_D %>%
  group_by(CUSIP) %>%
  slice_max(order_by = T_DATE.x) %>%
  ungroup() %>%
  dplyr::select(CUSIP, last_numeric_rating = numeric_rating)

# Check if the process was successful by viewing a sample
print(sample_n(last_rating_before_downgrade, 10))

# Join this last numeric rating back to the main table, 'merged_table'
merged_table <- merged_table %>%
  left_join(last_rating_before_downgrade, by = "CUSIP")

# Optionally, check the results in 'merged_table' for a specific CUSIP
specific_cusip_data <- merged_table %>%
  filter(CUSIP == "893817AA4") %>%
  dplyr::select(CUSIP, T_DATE.x, last_numeric_rating, RATING_CAT.x)

# Print the results
print(specific_cusip_data)


After_D <- After_D %>%
  mutate(RATING_CAT.x = trimws(as.character(RATING_CAT.x)))

After_D <- After_D %>%
  mutate(numeric_rating = rating_map[RATING_CAT.x])


# Find the first rating after downgrade for each CUSIP
first_rating_after_downgrade <- After_D %>%
  group_by(CUSIP) %>%
  slice_min(order_by = T_DATE.x, with_ties = FALSE) %>%
  summarise(first_numeric_rating = first(numeric_rating), .groups = 'drop')

# Join these ratings back to the merged_table
merged_table <- merged_table %>%
  left_join(last_rating_before_downgrade, by = "CUSIP") %>%
  left_join(first_rating_after_downgrade, by = "CUSIP")

# Calculate the notches of downgrade
merged_table <- merged_table %>%
  mutate(notches_of_downgrade = last_numeric_rating.x - first_numeric_rating)

# Output the merged_table to check results
print(select(merged_table, CUSIP, T_DATE.x, last_numeric_rating.x, first_numeric_rating, notches_of_downgrade))




# Assuming merged_table already includes the 'notches_of_downgrade' and 'CUSIP' columns correctly

# Count unique CUSIPs for each notch of downgrade
notches_count <- merged_table %>%
  group_by(notches_of_downgrade) %>%
  summarise(count = n_distinct(CUSIP)) %>%
  arrange(notches_of_downgrade)  # Arranging to show in order of notches

# Print the result
print(notches_count)


# Filter for the specific CUSIP and select relevant columns
specific_cusip_details <- merged_table %>%
  filter(CUSIP == "893817AA4") %>%
  select(CUSIP, T_DATE.x, notches_of_downgrade, last_numeric_rating.x,RATING_CAT.x)

# Print the details
print(specific_cusip_details)
      

print(colnames(merged_table))



library(dplyr)

# Ensure 'last_numeric_rating.x' is converted to a factor with only the levels that exist
merged_table$last_numeric_rating.x <- factor(merged_table$last_numeric_rating.x, levels = c(7, 8))

# Generate a single dummy variable manually:
# This will create a dummy where 1 represents a rating of 7 and 0 represents a rating of 8
merged_table$Rating_Dummy_7 <- as.numeric(merged_table$last_numeric_rating.x == 7)

# Check the structure and summary to verify the creation
str(merged_table)
summary(merged_table$Rating_Dummy_7)

# Optionally, view the head of the table to see the new dummy
colnames(merged_table)


# Filter for the specific CUSIP and select relevant columns
specific_cusip_details <- merged_table %>%
  filter(CUSIP == "893817AA4") %>%
  dplyr::select(CUSIP, T_DATE.x, notches_of_downgrade, last_numeric_rating.x,RATING_CAT.x)

# Print the details
print(specific_cusip_details)



# nothces dummies

library(dplyr)

# Assuming 'merged_table' contains your data and 'notches_of_downgrade' is the variable of interest
merged_table <- merged_table %>%
  mutate(downgrade_group = if_else(notches_of_downgrade > 1, 1, 0))

# View the changes to ensure correctness
colnames(merged_table)


# Assuming 'merged_table' contains your data
unique_cusips_with_group_one <- merged_table %>%
  filter(downgrade_group == 1) %>%
  distinct(CUSIP) %>%
  nrow()  # This will count the number of unique rows, or unique CUSIPs

# Print the number of unique CUSIPs with downgrade group equal to 1
print(unique_cusips_with_group_one)



# Optionally, check the structure and a few rows to confirm the dummies are correctly created
str(merged_table)
colnames(merged_table)

names(merged_table) <- gsub("Dummy_-", "Dummy_neg_", names(merged_table))
names(merged_table) <- gsub("Dummy_Q-", "Dummy_Q_neg_", names(merged_table))




write.csv(merged_table, "merged_table.csv", row.names = FALSE)


# Uplod duration matched from Python


Duration_matched <- read.csv("C:/Users/iraho/Downloads/MT data/df_export.csv", header = TRUE)

# Check the first few entries in the date column to see their format
head(Duration_matched)

# Check the structure of the dataframe
str(Duration_matched)

# Assuming your DataFrame is called Duration_matched
Duration_matched <- Duration_matched %>%
  mutate(
    CUSIP = as.factor(toupper(cusip)),   # Convert cusip to uppercase and then to a factor
    t_date = ymd(t_date),                # Convert t_date to Date object assuming 'yyyy-mm-dd' format
    T_DATE.x = ymd(T_DATE.x)             # Convert T_DATE.x to Date object
  )

# Check the structure again to confirm changes
str(Duration_matched)



# Check the first few entries in the date column to see their format
head(merged_table)

# Check the structure of the dataframe
str(merged_table)


selected_data <- Duration_matched %>%
  select(cusip, T_DATE.x, ret_interp_dur)
selected_data <- rename(selected_data, CUSIP = cusip)


merged_table <- merged_table %>%
  left_join(selected_data, by = c("CUSIP", "T_DATE.x"))

# Check the structure of the dataframe
str(merged_table)



selected_data <- merged_table %>%
  select(CUSIP, T_DATE.x, ret_interp_dur, RET_EOM.x)

# Assuming 'RET_EOM' and 'ret_interp_dur' are already in the appropriate format
merged_table <- merged_table %>%
  mutate(dur_m_ret = (RET_EOM.x - ret_interp_dur*100))


merged_table %>%
  select(CUSIP, RET_EOM.x, ret_interp_dur, dur_m_ret) %>%
  head()

# Check the structure of the dataframe
str(merged_table)

summary(merged_table)
var(merged_table)




##### Regression results #####



# Convert data to a pdata.frame for plm usage
pdata <- pdata.frame(merged_table, index = c("CUSIP", "T_DATE.x"))
colnames(merged_table)

formula1 <- cum_return_to_date ~ downgrade_group + as.numeric(TMT.x) +last_numeric_rating.x +
  Rating_Dummy_7 + 
  `Dummy_neg_24` + `Dummy_neg_23` + `Dummy_neg_22` + `Dummy_neg_21` + `Dummy_neg_20` + `Dummy_neg_19` +
  `Dummy_neg_18` + `Dummy_neg_17` + `Dummy_neg_16` + `Dummy_neg_15` + `Dummy_neg_14` + `Dummy_neg_13` +
  `Dummy_neg_12` + `Dummy_neg_11` + `Dummy_neg_10` + `Dummy_neg_9` + `Dummy_neg_8` + `Dummy_neg_7` +
  `Dummy_neg_6` + `Dummy_neg_5` + `Dummy_neg_4` + `Dummy_neg_3` + `Dummy_neg_2` + `Dummy_neg_1` +
   Dummy_1 + Dummy_2 + Dummy_3 + Dummy_4 + Dummy_5 +
  Dummy_6 + Dummy_7 + Dummy_8 + Dummy_9 + Dummy_10 + Dummy_11  +
  Dummy_12 + Dummy_13 + Dummy_14 + Dummy_15 + Dummy_16 + Dummy_17 +
  Dummy_18 + Dummy_19 + Dummy_20 + Dummy_21 + Dummy_22 + Dummy_23 +
  Dummy_24 + Dummy_25 + Dummy_26 + Dummy_27 + Dummy_28 + Dummy_29 +
  Dummy_30 + Dummy_31 + Dummy_32 + Dummy_33 + Dummy_34 + Dummy_25 + Dummy_36 + factor(COUPON.x)
  
  # Fit the fixed effects model
  fixed_model1 <- plm(formula1, data = pdata, model = "within")
  
  summary(fixed_model1)
   
  formula2 <- cum_return_to_date ~ downgrade_group + as.numeric(TMT.x) +last_numeric_rating.x +
   Rating_Dummy_7 + 
    `Dummy_neg_24` + `Dummy_neg_23` + `Dummy_neg_22` + `Dummy_neg_21` + `Dummy_neg_20` + `Dummy_neg_19` +
    `Dummy_neg_18` + `Dummy_neg_17` + `Dummy_neg_16` + `Dummy_neg_15` + `Dummy_neg_14` + `Dummy_neg_13` +
    `Dummy_neg_12` + `Dummy_neg_11` + `Dummy_neg_10` + `Dummy_neg_9` + `Dummy_neg_8` + `Dummy_neg_7` +
    `Dummy_neg_6` + `Dummy_neg_5` + `Dummy_neg_4` + `Dummy_neg_3` + `Dummy_neg_2` + `Dummy_neg_1` +
     Dummy_1 + Dummy_2 + Dummy_3 + Dummy_4 + Dummy_5 +
    Dummy_6 + Dummy_7 + Dummy_8 + Dummy_9 + Dummy_10 + Dummy_11  +
    Dummy_12 + Dummy_13 + Dummy_14 + Dummy_15 + Dummy_16 + Dummy_17 +
    Dummy_18 + Dummy_19 + Dummy_20 + Dummy_21 + Dummy_22 + Dummy_23 +
    Dummy_24 + Dummy_25 + Dummy_26 + Dummy_27 + Dummy_28 + Dummy_29 +
    Dummy_30 + Dummy_31 + Dummy_32 + Dummy_33 + Dummy_34 + Dummy_25 + Dummy_36 + factor(COUPON.x)
  
  # Fit the fixed effects model
  fixed_model2 <- plm(formula2, data = pdata, model = "within")
  
  summary(fixed_model2)
  
  
  
  formula4 <- cum_return_to_date ~ downgrade_group + as.numeric(TMT.x) +last_numeric_rating.x +
    Rating_Dummy_7 +  
    Dummy_1 + Dummy_2 + Dummy_3 + Dummy_4 + Dummy_5 +
    Dummy_6 + Dummy_7 + Dummy_8 + Dummy_9 + Dummy_10 + Dummy_11  +
    Dummy_12 + Dummy_13 + Dummy_14 + Dummy_15 + Dummy_16 + Dummy_17 +
    Dummy_18 + Dummy_19 + Dummy_20 + Dummy_21 + Dummy_22 + Dummy_23 +
    Dummy_24 + Dummy_25 + Dummy_26 + Dummy_27 + Dummy_28 + Dummy_29 +
    Dummy_30 + Dummy_31 + Dummy_32 + Dummy_33 + Dummy_34 + Dummy_25 + Dummy_36 + factor(COUPON.x)
  
  # Fit the fixed effects model
  fixed_model4 <- plm(formula4, data = pdata, model = "within")
  
  summary(fixed_model4)
  
  
  
  # reset dumies
  
  
  library(dplyr)
  
  # Assuming merged_table has Dummy_-36 to Dummy_36 for each period relative to the downgrade
  merged_table <- merged_table %>%
    arrange(CUSIP, T_DATE.x) %>%
    mutate(
      # Create a single reset trigger that changes at the start of each new period
      Reset_Trigger = rowSums(dplyr::select(., starts_with("Dummy_"))),
      # Normalize Reset_Trigger to be binary (1 at the start of new periods, 0 otherwise)
      Reset_Trigger = if_else(Reset_Trigger > 0, 1, 0)
    ) %>%
    group_by(CUSIP) %>%
    mutate(
      # Create a period ID that increments each time the Reset_Trigger changes
      Period_ID = cumsum(Reset_Trigger == 1)
    ) %>%
    # Calculate the cumulative return, resetting at the start of each new period
    group_by(CUSIP, Period_ID) %>%
    mutate(
      cum_return_reset = cumprod(adj_return) - 1
    ) %>%
    ungroup()
  
  # Convert cumulative returns to percentage
  merged_table <- merged_table %>%
    mutate(
      cum_return_reset = cum_return_reset * 100
    )
  
  
  
  # Filter the dataset for a specific CUSIP when Dummy_2 equals 1
  specific_cusip_data <- merged_table %>%
    filter(CUSIP == "893817AA4", `Dummy_neg_10` == 1) %>%
    dplyr::select(CUSIP, T_DATE.x, excess_return_eom, cum_return_reset, date_difference, `Dummy_neg_10`)
  
  # Print the data
  print(specific_cusip_data)
  
  
  
  
  pdata <- pdata.frame(merged_table, index = c("CUSIP", "T_DATE.x"))
  
  formula3 <- cum_return_reset ~ downgrade_group + as.numeric(TMT.x) +last_numeric_rating.x +
    Rating_Dummy_7 + 
    `Dummy_neg_24` + `Dummy_neg_23` + `Dummy_neg_22` + `Dummy_neg_21` + `Dummy_neg_20` + `Dummy_neg_19` +
    `Dummy_neg_18` + `Dummy_neg_17` + `Dummy_neg_16` + `Dummy_neg_15` + `Dummy_neg_14` + `Dummy_neg_13` +
    `Dummy_neg_12` + `Dummy_neg_11` + `Dummy_neg_10` + `Dummy_neg_9` + `Dummy_neg_8` + `Dummy_neg_7` +
    `Dummy_neg_6` + `Dummy_neg_5` + `Dummy_neg_4` + `Dummy_neg_3` + `Dummy_neg_2` + `Dummy_neg_1` +
    Dummy_1 + Dummy_2 + Dummy_3 + Dummy_4 + Dummy_5 +
    Dummy_6 + Dummy_7 + Dummy_8 + Dummy_9 + Dummy_10 + Dummy_11  +
    Dummy_12 + Dummy_13 + Dummy_14 + Dummy_15 + Dummy_16 + Dummy_17 +
    Dummy_18 + Dummy_19 + Dummy_20 + Dummy_21 + Dummy_22 + Dummy_23 +
    Dummy_24 + Dummy_25 + Dummy_26 + Dummy_27 + Dummy_28 + Dummy_29 +
    Dummy_30 + Dummy_31 + Dummy_32 + Dummy_33 + Dummy_34 + Dummy_25 + Dummy_36 + factor(COUPON.x)
  
  # Fit the fixed effects model
  fixed_model3 <- plm(formula3, data = pdata, model = "within")
  
  summary(fixed_model3)
  

  
  install.packages("data.table")
  library(data.table)
  
  ##########################################
  # just returns on returns
  pdata <- as.data.table(pdata)
   
  # Install the lfe package
  install.packages("lfe")
  
  # Load the lfe package
  library(lfe)
  
  
  
 
  
  # Scaling down the variables
  merged_table$AMOUNT_OUTSTANDING_Millions <- merged_table$AMOUNT_OUTSTANDING.x / 100000
  merged_table$OFFERING_AMT_Millions <- merged_table$OFFERING_AMT.x / 100000
  
  
  model_one_way_excess <- felm(excess_return_eom ~ downgrade_group + as.numeric(TMT.x) + as.numeric (DURATION.x) +
                          as.numeric(COUPON.x) + as.numeric(AMOUNT_OUTSTANDING_Millions) + as.numeric(OFFERING_AMT_Millions)+
                          Rating_Dummy_7 + `Dummy_Q_neg_8`+ `Dummy_Q_neg_7`+ `Dummy_Q_neg_6`+ `Dummy_Q_neg_5`+ 
                          `Dummy_neg_12` + `Dummy_neg_11` + `Dummy_neg_10` + `Dummy_neg_9` + `Dummy_neg_8` + `Dummy_neg_7` +
                          `Dummy_neg_6` + `Dummy_neg_5` + `Dummy_neg_4` + `Dummy_neg_3` + `Dummy_neg_2` + `Dummy_neg_1`  + 
                          Dummy_1 + Dummy_2 + Dummy_3 + Dummy_4 + Dummy_5 +
                          Dummy_6 + Dummy_7 + Dummy_8 + Dummy_9 + Dummy_10 + Dummy_11  +
                          Dummy_12 + `Dummy_Q5` + `Dummy_Q6` +`Dummy_Q7` +`Dummy_Q8` +
                          `Dummy_Q9` + `Dummy_Q10` +  `Dummy_Q11` + `Dummy_Q12` | T_DATE.x,
                          data = merged_table)
  
  # Print the summary of the model
  summary(model_one_way_excess)
  
  model_one_way_excess_y <- felm(excess_return_eom ~ downgrade_group + as.numeric(TMT.x) + as.numeric (DURATION.x) +
                                 as.numeric(COUPON.x) + as.numeric(AMOUNT_OUTSTANDING_Millions) + as.numeric(OFFERING_AMT_Millions)+
                                 Rating_Dummy_7 + `Dummy_Q_neg_8`+ `Dummy_Q_neg_7`+ `Dummy_Q_neg_6`+ `Dummy_Q_neg_5`+ 
                                 `Dummy_neg_12` + `Dummy_neg_11` + `Dummy_neg_10` + `Dummy_neg_9` + `Dummy_neg_8` + `Dummy_neg_7` +
                                 `Dummy_neg_6` + `Dummy_neg_5` + `Dummy_neg_4` + `Dummy_neg_3` + `Dummy_neg_2` + `Dummy_neg_1`  +  
                                 Dummy_1 + Dummy_2 + Dummy_3 + Dummy_4 + Dummy_5 +
                                 Dummy_6 + Dummy_7 + Dummy_8 + Dummy_9 + Dummy_10 + Dummy_11  +
                                 Dummy_12 + `Dummy_Q5` + `Dummy_Q6` +`Dummy_Q7` +`Dummy_Q8` +
                                 `Dummy_Q9` + `Dummy_Q10` +  `Dummy_Q11` + `Dummy_Q12` +
  `Dummy_Year_2003` +`Dummy_Year_2004` + `Dummy_Year_2005` + `Dummy_Year_2006` +
  `Dummy_Year_2007` +`Dummy_Year_2008` + `Dummy_Year_2009` + `Dummy_Year_2010` +
  `Dummy_Year_2011` +`Dummy_Year_2012` + `Dummy_Year_2013` + `Dummy_Year_2014` +
  `Dummy_Year_2015` +`Dummy_Year_2016` + `Dummy_Year_2017` + `Dummy_Year_2018` +
  `Dummy_Year_2019` +`Dummy_Year_2020` + `Dummy_Year_2021` + `Dummy_Year_2022` | T_DATE.x,data = merged_table)
  
  # Print the summary of the model
  summary(model_one_way_excess_y)

  
  
  # Formula with fixed effects for DATE and CUSIP
   formula_twoway <- excess_return_eom ~ downgrade_group + as.numeric(TMT.x) + as.numeric (DURATION.x) +
     as.numeric(COUPON.x) + as.numeric(AMOUNT_OUTSTANDING_Millions) + as.numeric(OFFERING_AMT_Millions)+
     Rating_Dummy_7 + `Dummy_Q_neg_8`+ `Dummy_Q_neg_7`+ `Dummy_Q_neg_6`+ `Dummy_Q_neg_5`+ 
     `Dummy_neg_12` + `Dummy_neg_11` + `Dummy_neg_10` + `Dummy_neg_9` + `Dummy_neg_8` + `Dummy_neg_7` +
     `Dummy_neg_6` + `Dummy_neg_5` + `Dummy_neg_4` + `Dummy_neg_3` + `Dummy_neg_2` + `Dummy_neg_1` + 
     Dummy_1 + Dummy_2 + Dummy_3 + Dummy_4 + Dummy_5 +
     Dummy_6 + Dummy_7 + Dummy_8 + Dummy_9 + Dummy_10 + Dummy_11  +
     Dummy_12 + `Dummy_Q5` + `Dummy_Q6` +`Dummy_Q7` +`Dummy_Q8` +
     `Dummy_Q9` + `Dummy_Q10` +  `Dummy_Q11` + `Dummy_Q12`  | T_DATE.x + CUSIP

  # Run the model
  model_lfe <- felm(formula_twoway, data = merged_table)
  print(summary(model_lfe))
  
  
  model_one_way_d <- felm(dur_m_ret ~ downgrade_group + as.numeric(TMT.x) + as.numeric (DURATION.x) +
                                 as.numeric(COUPON.x) + as.numeric(AMOUNT_OUTSTANDING_Millions) + as.numeric(OFFERING_AMT_Millions)+
                                 Rating_Dummy_7 + `Dummy_Q_neg_8`+ `Dummy_Q_neg_7`+ `Dummy_Q_neg_6`+ `Dummy_Q_neg_5`+ 
                                 `Dummy_neg_12` + `Dummy_neg_11` + `Dummy_neg_10` + `Dummy_neg_9` + `Dummy_neg_8` + `Dummy_neg_7` +
                                 `Dummy_neg_6` + `Dummy_neg_5` + `Dummy_neg_4` + `Dummy_neg_3` + `Dummy_neg_2` + `Dummy_neg_1`  + 
                                 Dummy_1 + Dummy_2 + Dummy_3 + Dummy_4 + Dummy_5 +
                                 Dummy_6 + Dummy_7 + Dummy_8 + Dummy_9 + Dummy_10 + Dummy_11  +
                                 Dummy_12 + `Dummy_Q5` + `Dummy_Q6` +`Dummy_Q7` +`Dummy_Q8` +
                                 `Dummy_Q9` + `Dummy_Q10` +  `Dummy_Q11` + `Dummy_Q12` | T_DATE.x,
                               data = merged_table)
  
  
  # Print the summary of the model
  summary(model_one_way_d)
  
  
  
  # Formula with fixed effects for DATE and CUSIP
  formula_twoway_d <- dur_m_ret ~ downgrade_group + as.numeric(TMT.x) + as.numeric (DURATION.x) +
    as.numeric(COUPON.x) + as.numeric(AMOUNT_OUTSTANDING_Millions) + as.numeric(OFFERING_AMT_Millions)+
    Rating_Dummy_7 + `Dummy_Q_neg_8`+ `Dummy_Q_neg_7`+ `Dummy_Q_neg_6`+ `Dummy_Q_neg_5`+ 
    `Dummy_neg_12` + `Dummy_neg_11` + `Dummy_neg_10` + `Dummy_neg_9` + `Dummy_neg_8` + `Dummy_neg_7` +
    `Dummy_neg_6` + `Dummy_neg_5` + `Dummy_neg_4` + `Dummy_neg_3` + `Dummy_neg_2` + `Dummy_neg_1` + 
    Dummy_1 + Dummy_2 + Dummy_3 + Dummy_4 + Dummy_5 +
    Dummy_6 + Dummy_7 + Dummy_8 + Dummy_9 + Dummy_10 + Dummy_11  +
    Dummy_12 + `Dummy_Q5` + `Dummy_Q6` +`Dummy_Q7` +`Dummy_Q8` +
    `Dummy_Q9` + `Dummy_Q10` +  `Dummy_Q11` + `Dummy_Q12`  | T_DATE.x + CUSIP
  
  # Run the model
  model_lfe_d <- felm(formula_twoway_d, data = merged_table)
  print(summary(model_lfe_d))
  
  

  
  # Checking for multicollinearity
  library(car)
  vif_result <- vif(lm(excess_return_eom ~ downgrade_group + as.numeric(TMT.x) + as.numeric (DURATION.x) +
                         as.numeric(COUPON.x) + as.numeric(AMOUNT_OUTSTANDING.x) + as.numeric(OFFERING_AMT.x)+
                         Rating_Dummy_7 + `Dummy_Q_neg_8`+ `Dummy_Q_neg_7`+ `Dummy_Q_neg_6`+ `Dummy_Q_neg_5`+ 
                         `Dummy_neg_12` + `Dummy_neg_11` + `Dummy_neg_10` + `Dummy_neg_9` + `Dummy_neg_8` + `Dummy_neg_7` +
                         `Dummy_neg_6` + `Dummy_neg_5` + `Dummy_neg_4` + `Dummy_neg_3` + `Dummy_neg_2` + `Dummy_neg_1` +
                         Dummy_1 + Dummy_2 + Dummy_3 + Dummy_4 + Dummy_5 +
                         Dummy_6 + Dummy_7 + Dummy_8 + Dummy_9 + Dummy_10 + Dummy_11  +
                         Dummy_12 + `Dummy_Q5` + `Dummy_Q6` +`Dummy_Q7` +`Dummy_Q8` +
                         `Dummy_Q9` + `Dummy_Q10` +  `Dummy_Q11` + `Dummy_Q12` +
                         `Dummy_Year_2003` +`Dummy_Year_2004` + `Dummy_Year_2005` + `Dummy_Year_2006` +
                         `Dummy_Year_2007` +`Dummy_Year_2008` + `Dummy_Year_2009` + `Dummy_Year_2010` +
                         `Dummy_Year_2011` +`Dummy_Year_2012` + `Dummy_Year_2013` + `Dummy_Year_2014` +
                         `Dummy_Year_2015` +`Dummy_Year_2016` + `Dummy_Year_2017` + `Dummy_Year_2018` +
                         `Dummy_Year_2019` +`Dummy_Year_2020` + `Dummy_Year_2021` + `Dummy_Year_2022`
                       , data = merged_table))
  print(vif_result) 
  
  

  # Fit a basic linear model to check for aliased coefficients
  basic_model <- lm(excess_return_eom ~ downgrade_group + as.numeric(TMT.x) + as.numeric (DURATION.x) +
                      as.numeric(COUPON.x) + as.numeric(AMOUNT_OUTSTANDING.x) + as.numeric(OFFERING_AMT.x)+
                      Rating_Dummy_7 + `Dummy_Q_neg_8`+ `Dummy_Q_neg_7`+ `Dummy_Q_neg_6`+ `Dummy_Q_neg_5`+ 
                      `Dummy_neg_12` + `Dummy_neg_11` + `Dummy_neg_10` + `Dummy_neg_9` + `Dummy_neg_8` + `Dummy_neg_7` +
                      `Dummy_neg_6` + `Dummy_neg_5` + `Dummy_neg_4` + `Dummy_neg_3` + `Dummy_neg_2` + `Dummy_neg_1` +
                      Dummy_1 + Dummy_2 + Dummy_3 + Dummy_4 + Dummy_5 +
                      Dummy_6 + Dummy_7 + Dummy_8 + Dummy_9 + Dummy_10 + Dummy_11  +
                      Dummy_12 + `Dummy_Q5` + `Dummy_Q6` +`Dummy_Q7` +`Dummy_Q8` +
                      `Dummy_Q9` + `Dummy_Q10` +  `Dummy_Q11` + `Dummy_Q12` +
                      `Dummy_Year_2003` +`Dummy_Year_2004` + `Dummy_Year_2005` + `Dummy_Year_2006` +
                      `Dummy_Year_2007` +`Dummy_Year_2008` + `Dummy_Year_2009` + `Dummy_Year_2010` +
                      `Dummy_Year_2011` +`Dummy_Year_2012` + `Dummy_Year_2013` + `Dummy_Year_2014` +
                      `Dummy_Year_2015` +`Dummy_Year_2016` + `Dummy_Year_2017` + `Dummy_Year_2018` +
                      `Dummy_Year_2019` +`Dummy_Year_2020` + `Dummy_Year_2021` + `Dummy_Year_2022`
                    , data = merged_table)
  
  # Check for aliased (linearly dependent) terms in the model
  print(alias(basic_model))
  
  
  
  library(stargazer)

  
  # Example for multiple models
  stargazer(model_one_way_excess, model_lfe,model_one_way_excess_y, type = "html", out = "panel_results.html")
  stargazer(model_one_way_d, model_lfe_d, type = "html", out = "panel_results_d.html")
  
  
  
  # upload factors
  
  library(readr)
  library(zoo)
  
  # Load the data
  factors <- read.csv("C:/Users/iraho/Downloads/MT data/BondFactorZoo_vOct_2023.csv", header = TRUE)
 
  factors$date <- as.Date(factors$date, format = "%Y-%m-%d")
  # Check the first few entries in the date column to see their format
  head(factors)
  
  factors <- factors %>%
    mutate(across(-date, ~ . * 100))
  
  # Check the structure of the dataframe
  str(factors)
  
  
  
  # Load the data using read.table
  factor_pastor <- read.table("C:/Users/iraho/Downloads/MT data/Pastor.csv", 
                              header = TRUE, 
                              sep = "", 
                              fill = TRUE)
  
  # Display the structure to understand how the columns are formatted
  str(factor_pastor)
  
  # Display the first few rows
  head(factor_pastor)

  
  # Properly naming the columns based on your data inspection
  names(factor_pastor) <- c("date", "Agg_Liq", "Innov_Liq_eq8", "Traded_Liq", "LIQ_V")
  
  factor_pastor$date <- as.Date(as.yearmon(as.character(factor_pastor$date), "%Y%m"), frac = 1) 
  
  # Format the date to "yyyy-mm-dd"
  factor_pastor$date <- format(factor_pastor$date, "%Y-%m-%d")
  factor_pastor$date <- as.Date(factor_pastor$date, format = "%Y-%m-%d")
  
  # Check the result
  head(factor_pastor$date)
  
  # Converting to numeric as it seems these columns might have been read as characters
  factor_pastor$Agg_Liq <- as.numeric(factor_pastor$Agg_Liq)*100
  factor_pastor$Innov_Liq_eq8 <- as.numeric(factor_pastor$Innov_Liq_eq8)*100
  factor_pastor$Traded_Liq <- as.numeric(factor_pastor$Traded_Liq)*100
  factor_pastor$LIQ_V <- as.numeric(factor_pastor$LIQ_V)*100
  # Check the updated structure
  str(factor_pastor)
  
  # Check the first few entries to confirm proper loading and formatting
  head(factor_pastor)
  
  # Remove columns that are entirely NA
  factor_pastor <- factor_pastor[, colSums(is.na(factor_pastor)) != nrow(factor_pastor)]
  
  # Check the first few entries to confirm proper loading and formatting
  head(factor_pastor)
  

  # Attempt the merge again
  factors <- merge(factors, factor_pastor, by = "date", all = TRUE)
  
  # Check the result
  head(factors)
  

  
  bbw_ice_data <- read.csv("C:/Users/iraho/Downloads/MT data/bbw_ice_oct_2023_lastest.csv", header = TRUE, stringsAsFactors = FALSE)
  
  head(bbw_ice_data)
  str(bbw_ice_data)
  
  # Convert the date format from 'dd/mm/yyyy' to 'yyyy-mm-dd'
  bbw_ice_data$date <- as.Date(bbw_ice_data$date, format = "%d/%m/%Y")
  
  # Check the first few dates to ensure the conversion was successful
  head(bbw_ice_data$date)
  
  # Assuming both dataframes have the 'Date' column properly formatted
  factors <- merge(factors, bbw_ice_data, by = "date", all = TRUE)
  
  # Check the structure of the merged dataframe to ensure it looks correct
  str(factors)
  
  # Optionally, view the first few rows to see the merged data
  head(factors)
  
  
  # Load the data from the CSV file
  int_factors <- read.csv("C:/Users/iraho/Downloads/MT data/He_Kelly_Manela_Factors_monthly.csv", header = TRUE)
  
  # Check the structure of the data to understand the column names and formats
  str(int_factors)
  
  # Display the first few rows to see the date format
  head(int_factors)
  
  # Assuming the date column is named incorrectly or in yyyymm format
  # Convert 'yyyymm' to date assuming it's the end of the month
  int_factors$date <- as.Date(as.yearmon(as.character(int_factors$yyyymm), "%Y%m"), frac = 1) 
  
  # Check the conversion
  head(int_factors$date)
  
  int_factors$intermediary_capital_risk_factor <- int_factors$intermediary_capital_risk_factor *100
  
  # Merge the He Kelly Manela factors data with the existing factors table
  factors <- merge(factors, int_factors, by = "date", all = TRUE)
  
  
  
  
  # Check the structure and the first few rows of the merged dataframe
  str(factors)
  head(factors)
  
  
  #### fix inn liq
  
  
  
  # Load the data, skipping the first three rows and setting headers on the fourth row
  factors_FF <- read.csv("C:/Users/iraho/Downloads/MT data/F-F_Research_Data_5_Factors_2x3.csv", skip = 3, header = TRUE)
  
  # Manually set the name of the first column as 'Date' if it's unnamed
  names(factors_FF)[1] <- "date"
  
  # Display the structure to verify column names and data format
  str(factors_FF)
  
  # Display the first few rows to check the initial date format
  head(factors_FF)
  
  # Convert 'yyyymm' to date assuming it's the end of the month
  # The 'as.yearmon' function from the 'zoo' package interprets yyyymm formats correctly

  factors_FF$date <- as.Date(as.yearmon(as.character(factors_FF$date), "%Y%m"), frac = 1) 
  
  # Check the conversion to ensure the dates are now correct
  head(factors_FF$date)
  
  # Loop over the columns from 'Mkt.RF' to 'RF' and convert them
  cols_to_convert <- names(factors_FF)[2:7]  # Adjust if more columns need conversion
  factors_FF[cols_to_convert] <- lapply(factors_FF[cols_to_convert], function(x) as.numeric(as.character(trimws(x))))
  
  
  factors_FF <- factors_FF %>%
    filter(!is.na(date))
  
  # Check the structure to ensure conversions were successful
  str(factors_FF)
  
  
  
  # Load the data
  momentum_factors <- read.csv("C:/Users/iraho/Downloads/MT data/F-F_Momentum_Factor.CSV", header = FALSE, skip = 13)
  
  # Set the first row of data as column names
  colnames(momentum_factors) <- as.character(momentum_factors[1,])
  momentum_factors <- momentum_factors[-1,]  # Remove the row used for names
  
  # Check the structure
  str(momentum_factors)
  
  names(momentum_factors)[1] <- "date"
  
  
  # Convert 'date' from 'yyyymm' to the last day of each month
  momentum_factors$date <- as.Date(as.yearmon(momentum_factors$date, "%Y%m"), frac = 1) 
  
  # Convert other columns to numeric
  numeric_cols <- names(momentum_factors)[-1]  # Assuming all other columns need to be numeric
  momentum_factors[numeric_cols] <- lapply(momentum_factors[numeric_cols], function(x) as.numeric(as.character(trimws(x))))
  
  # Verify the changes
  str(momentum_factors)
  
  # Merge the datasets on the 'Date' column
  factors_FF <- merge(factors_FF, momentum_factors, by = "date", all = TRUE)
  
  factors_FF <- factors_FF %>%
    filter(!is.na(date))
  
  # Check the structure of the combined data frame to ensure the merge was successful
  str(factors_FF)
  
  # Preview the first few rows of the merged data frame
  head(factors_FF)
  
  
  
  # Load the data with the first row as column names
  lt_reversal_factors <- read.csv("C:/Users/iraho/Downloads/MT data/F-F_LT_Reversal_Factor.CSV", skip = 13, header = TRUE)
  
  # Set the first row of data as column names and remove the first row from data
  colnames(lt_reversal_factors) <- lt_reversal_factors[1, ]
  lt_reversal_factors <- lt_reversal_factors[-1, ]
  
  # Rename the columns appropriately
  names(lt_reversal_factors)[1] <- "date"
  names(lt_reversal_factors)[2] <- "REV_LT"
  
  # Convert 'Date' from 'yyyymm' format to the last day of each month
  lt_reversal_factors$date <- as.Date(as.yearmon(as.character(lt_reversal_factors$date), "%Y%m"), frac = 1)
  
  # Convert 'REV_LT' to numeric
  lt_reversal_factors$REV_LT <- as.numeric(lt_reversal_factors$REV_LT)
  
  # Check the structure to ensure the data types are correct
  str(lt_reversal_factors)
  
  # Preview the first few rows to confirm the changes
  head(lt_reversal_factors)
  
  # Merge the datasets on the 'Date' column
  factors_FF <- merge(factors_FF, lt_reversal_factors, by = "date", all = TRUE)
  
  factors_FF <- factors_FF %>%
    filter(!is.na(date))
  
  # Check the structure of the combined data frame to ensure the merge was successful
  str(factors_FF)
  
  # Preview the first few rows of the merged data frame
  head(factors_FF)
  
  
  
  
  # Load the data, skipping the header rows and setting the first row of data as column names
  st_reversal_factors <- read.csv("C:/Users/iraho/Downloads/MT data/F-F_ST_Reversal_Factor.CSV", skip = 13, header = TRUE)
  
  # Assign the first row as column names and then remove it from the data
  colnames(st_reversal_factors) <- st_reversal_factors[1, ]
  st_reversal_factors <- st_reversal_factors[-1, ]
  
  # Rename the columns appropriately
  names(st_reversal_factors)[1] <- "date"
  names(st_reversal_factors)[2] <- "REV_ST"
  
  # Convert the 'Date' from 'yyyymm' format to the last day of each month
  st_reversal_factors$date <- as.Date(as.yearmon(as.character(st_reversal_factors$date), "%Y%m"), frac = 1) 
  

  # Check the structure to ensure the data types are correct
  str(st_reversal_factors)
  
  # Preview the first few entries to confirm proper loading and formatting
  head(st_reversal_factors)
  
  # Merge the datasets on the 'Date' column
  factors_FF <- merge(factors_FF, st_reversal_factors, by = "date", all = TRUE)
  
  # Check the structure of the combined data frame to ensure the merge was successful
  str(factors_FF)
  
  # Preview the first few rows of the merged data frame
  head(factors_FF) 
  

  
  #macro factors
  
  
  
  
  # Assuming Macro_factors is loaded and DATE is a character
  Macro_factors <- read.csv("C:/Users/iraho/Downloads/MT data/EXPINF1YR.csv", stringsAsFactors = FALSE)
  Macro_factors$DATE <- as.Date(Macro_factors$DATE, format = "%Y-%m-%d")
  
  # Ensure the data is sorted by date
  Macro_factors <- Macro_factors[order(Macro_factors$DATE), ]
  
  # Manually shift the EXPINF1YR values to create a 'previous month' column
  Macro_factors$prev_EXPINF1YR <- c(NA, Macro_factors$EXPINF1YR[-length(Macro_factors$EXPINF1YR)])
  
  # Calculate the month-over-month difference
  Macro_factors$infl_diff <- (Macro_factors$EXPINF1YR - Macro_factors$prev_EXPINF1YR)
  
  # Adjust the 'DATE' to be the end of the previous month
  Macro_factors$date <- Macro_factors$DATE - 1  # Subtracting one day to set it to the end of the previous month
  
  
  # Print the first few rows to verify the calculations
  head(Macro_factors)
  
  
  # Load and adjust Y10_2 dataset
  Y10_2 <- read.csv("C:/Users/iraho/Downloads/MT data/T10Y2YM.csv", stringsAsFactors = FALSE) %>%
    mutate(
      date = as.Date(DATE, format = "%Y-%m-%d"),
      # Move the date to the end of the previous month
      date = date - days(1),
      T10Y2YM = T10Y2YM /100
    )
  
  # Check the changes
  head(Macro_factors)
  head(Y10_2)
  
  # Merge Macro_factors with Y10_2, selecting only specific columns from Macro_factors
  Macro_factors <- Macro_factors %>%
    select(date, infl_diff) %>%  # Selecting only date and infl_diff from Macro_factors
    left_join(Y10_2, by = "date")  # Merging with Y10_2 on date
  
  # Check the merged data frame
  head(Macro_factors)
  
  

  library(dplyr)
  library(lubridate)
  
  
  # Load and prepare the VIX data
  VIX <- read.csv("C:/Users/iraho/Downloads/MT data/VIX_History.csv", stringsAsFactors = FALSE)
  VIX$DATE <- as.Date(VIX$DATE, format = "%m/%d/%Y")
  
  # Summarize to get the last CLOSE of each month
  VIX_monthly <- VIX %>%
    mutate(
      Year = year(DATE),
      Month = month(DATE),
      EOM = ceiling_date(DATE, unit = "month") - days(1)  # Find the end of month
    ) %>%
    group_by(Year, Month) %>%
    summarise(
      Last_Trading_Day = max(DATE),
      Last_Close = last(CLOSE),
      .groups = 'drop'
    )
  
  # Prepare the data for joining by creating a 'Previous Month' and 'Previous Year' column
  VIX_monthly <- VIX_monthly %>%
    mutate(
      Prev_Year = if_else(Month == 1, Year - 1, Year),
      Prev_Month = if_else(Month == 1, 12, Month - 1)
    )
  
  # Join to get the previous month's close
  VIX_monthly <- VIX_monthly %>%
    left_join(VIX_monthly, by = c("Prev_Year" = "Year", "Prev_Month" = "Month")) %>%
    mutate(
      VIX_diff = (Last_Close.x - Last_Close.y)
    ) %>%
    select(Year, Month, Current_Last_Trading_Day = Last_Trading_Day.x, Current_Close = Last_Close.x, Prev_Close = Last_Close.y, VIX_diff)
  
  # Print the resulting table
  print(head(VIX_monthly))
  
  
  VIX_monthly <- VIX_monthly %>%
    mutate(
      
      # Create 'Date' as the actual end of the month date
      date = ceiling_date(Current_Last_Trading_Day, "month") - days(1)
    )
  
  # Print the resulting table
  print(head(VIX_monthly))
  

  # Merge Macro_factors with VIX on the 'date' column
  Macro_factors <- left_join(Macro_factors, VIX_monthly, by = "date")
  
  # Check the structure of the combined data
  str(Macro_factors)
  
  # View the first few rows of the combined data to confirm the merge
  head(Macro_factors)
  
  
  ##### Correlation matrix #####
  
  # Load the necessary library for date operations
  library(zoo)
  
  # Define the date range
  start_date <- as.Date("2002-06-30")
  end_date <- as.Date("2022-09-30")
  
  # Filter data
  filtered_factors <- factors %>% filter(date >= start_date & date <= end_date)
  filtered_factors_FF <- factors_FF %>% filter(date >= start_date & date <= end_date)
  filtered_Macro_factors <- Macro_factors %>% filter(date >= start_date & date <= end_date)
  
  ###################################### Correctly specify columns for numeric conversion (check names directly from your dataset)
  numeric_columns_factors <- c("DEF", "LTREVB", "MKTB.x", "PEADB", "MOMB", "TERM", "Traded_Liq", "intermediary_capital_risk_factor")
  numeric_columns_FF <- c("Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom   ", "REV_LT", "REV_ST")
  numeric_columns_Macro_factors <- c("infl_diff","T10Y2YM","VIX_diff")
  
  # Convert columns to numeric
  filtered_factors[numeric_columns_factors] <- lapply(filtered_factors[numeric_columns_factors], as.numeric)
  filtered_factors_FF[numeric_columns_FF] <- lapply(filtered_factors_FF[numeric_columns_FF], as.numeric)
  filtered_Macro_factors[numeric_columns_Macro_factors] <- lapply(filtered_Macro_factors[numeric_columns_Macro_factors], as.numeric)
  
  # Merge the data
  aligned_data <- merge(filtered_factors, filtered_factors_FF, by = "date", all = TRUE)
  aligned_data <- merge(aligned_data, filtered_Macro_factors, by = "date", all = TRUE)
  
  str(aligned_data)
  
  
  # Calculate the correlation matrix
  correlation_matrix <- cor(aligned_data %>% select(all_of(c(numeric_columns_factors, numeric_columns_FF,numeric_columns_Macro_factors))), use = "complete.obs")
  
  # Output the correlation matrix
  print(correlation_matrix)
  
  stargazer(correlation_matrix,
            type = "html", out = "correlation matrix.html")

  
  
colnames(aligned_data)
str(aligned_data)
 

 # Define the columns to exclude (e.g., 'date' or any non-numeric columns)
  columns_to_exclude <- c("date","DATE","Current_Last_Trading_Day")
  
  # Filter out the columns to exclude
  numeric_data <- aligned_data[, !(names(aligned_data) %in% columns_to_exclude)]
  
  # Calculate the required statistics for the numeric columns only
  summary_table <- sapply(numeric_data, function(x) {
    x <- na.omit(x)  # Remove NA values for accurate calculations
    c(
      Mean = round(mean(x), 4),
      Median = round(median(x), 4),
      `5th Percentile` = round(quantile(x, 0.05), 4),
      `95th Percentile` = round(quantile(x, 0.95), 4),
      `Standard Deviation` = round(sd(x), 4)
    )
  })
  
  # Convert to a more readable format if desired
  summary_table_factors <- t(summary_table)  # Transpose for a better table format
  
  # Print the summary table
  print(summary_table_factors)
  
  
  stargazer(summary_table_factors,
            type = "html", out = "factors_summary.html")
  
  

  
  colnames(merged_table)
  
  ###### Regression before downgrade #####
  
  # Filter the data for observations between -510 and 0 days and adjust excess returns for a short position
  short_data_for_regression <- merged_table %>%
    filter(date_difference >= -12*30 & date_difference <= 30) %>%
    mutate(adjusted_excess_return_eom = -excess_return_eom,
           dur_m_ret_adj = -dur_m_ret) %>%
    select(DATE.x, CUSIP, TMT.x, OFFERING_AMT.x, COUPON.x, DURATION.x, T_DATE.x, T_DVolume.x,RET_EOM.x,
           adjusted_excess_return_eom, Rating_Dummy_7, downgrade_group, date_difference, dur_m_ret_adj )  
  
  # Check the structure to ensure only the necessary data is included
  str(short_data_for_regression)
  
  # Display the first few rows to verify adjustments and filtering
  head(short_data_for_regression)
  
  # Calculate minimum and maximum T_DATE from the filtered data
  min_date <- min(short_data_for_regression$T_DATE.x)
  max_date <- max(short_data_for_regression$T_DATE.x)
  
  # Print the results
  print(paste("Minimum T_DATE:", min_date))
  print(paste("Maximum T_DATE:", max_date))
  
  
  library(dplyr)
  
  # Group by date.x and summarize the data
  date_summary <- short_data_for_regression %>%
    group_by(DATE.x) %>%
    summarise(
      total_observations = n(),                             # Total observations per date
      total_cusips = sum(!is.na(CUSIP)),                    # Count of CUSIPs (non-NA)
      unique_cusips = n_distinct(CUSIP)                     # Count of unique CUSIPs
    )
  
  # Print the summary table
  print(date_summary)
  
  
  
  library(dplyr)
  
  # Calculate equally weighted portfolio returns
  equally_weighted_returns <- short_data_for_regression %>%
    group_by(DATE.x) %>%
    summarise(equally_weighted_return = mean(adjusted_excess_return_eom, na.rm = TRUE))
  
  print(equally_weighted_returns)
  
  
  # Calculate offering amount weighted portfolio returns
  offering_amount_weighted_returns <- short_data_for_regression %>%
    group_by(DATE.x) %>%
    summarise(
      total_offering_amt = sum(OFFERING_AMT.x, na.rm = TRUE),
      weighted_return = sum(adjusted_excess_return_eom * OFFERING_AMT.x, na.rm = TRUE) / total_offering_amt
    )
  
  print(offering_amount_weighted_returns)
  
  # Merge the results
  short_portfolio_returns <- merge(equally_weighted_returns, offering_amount_weighted_returns, by = "DATE.x", all = TRUE)
  
  print(short_portfolio_returns)
  
  
  
  
  average_return_e <- mean(short_portfolio_returns$equally_weighted_return, na.rm = TRUE)
  average_return_o <- mean(short_portfolio_returns$weighted_return, na.rm = TRUE)
  volatility_return_e <- sd(short_portfolio_returns$equally_weighted_return, na.rm = TRUE)
  volatility_return_o <- sd(short_portfolio_returns$weighted_return, na.rm = TRUE)
  
  print(paste("Average Equally Weighted Return:", average_return_e))
  print(paste("Average Weighted Return:", average_return_o))
  print(paste("Volatility of Equally Weighted Return:", volatility_return_e))
  print(paste("Volatility of Weighted Return:", volatility_return_o))
  
  
  library(ggplot2)

  # Plot the graph
  ggplot(short_portfolio_returns, aes(x = DATE.x)) +
    geom_line(aes(y = equally_weighted_return, color = "Equally Weighted"), size = 1.2) +
    geom_point(aes(y = weighted_return, color = "Offering Amount Weighted"), size = 3) +
    labs(
      title = "Portfolio Returns Over Time",
      x = "Date",
      y = "Return (%)",
      color = "Portfolio Type"
    ) +
    scale_color_manual(values = c("Equally Weighted" = "blue", "Offering Amount Weighted" = "red")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  library(ggplot2)
  
  # Plot the graph with transparent red line
  fancy_plot_transparent <- ggplot(short_portfolio_returns, aes(x = DATE.x)) +
    geom_line(aes(y = equally_weighted_return, color = "Equally Weighted"), size = 1.2) +
    geom_line(aes(y = weighted_return, color = "Offering Amount Weighted"), size = 1.2, alpha = 0.5) +
    labs(
      title = "Before Downgrade Portfolios Return Over Time",
      x = "Date",
      y = "Return (%)",
      color = "Portfolio Type"
    ) +
    scale_color_manual(values = c("Equally Weighted" = "blue", "Offering Amount Weighted" = "red")) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      panel.grid.major = element_line(size = 0.5, linetype = 'dashed', color = 'grey'),
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  # Display the plot
  print(fancy_plot_transparent)
  
  # Save the plot as a PNG file
  ggsave("short_portfolio_returns_transparent.png", plot = fancy_plot_transparent, width = 12, height = 6, dpi = 300)
  
  
 
  
  
  library(lubridate)
  library(dplyr)
  
  # Assuming 'short_data_for_regression', 'short_portfolio_returns', and 'aligned_data' are your dataframes
  
  # Ensure all dataframes have the 'Year_Month' column prepared
  short_data_for_regression <- short_data_for_regression %>%
    mutate(Year_Month = format(DATE.x, "%Y-%m"))
  
  short_portfolio_returns <- short_portfolio_returns %>%
    mutate(Year_Month = format(DATE.x, "%Y-%m"))
  
  aligned_data <- aligned_data %>%
    mutate(Year_Month = format(date, "%Y-%m"))
  
  # Merge the first two datasets
  merged_data_first_step <- left_join(short_data_for_regression, short_portfolio_returns, by = "Year_Month")
  
  # Merge the resulting dataframe with the third dataset
  short_merged_data <- left_join(merged_data_first_step, aligned_data, by = "Year_Month")
  
  # Check the structure to ensure the merge was successful
  str(short_merged_data)
  
  
  
  

  
  # Now you can proceed with further analysis or model fitting
  
  # Detach plm when you're done
  detach("package:plm", unload=TRUE)
  
  
  
  install.packages("nlme")
  library(nlme)
  
  # Assuming 'merged_data_short' is your final dataset prepared for regression
  # and it includes the response variable 'adjusted_excess_return_eom' and predictors like 'DEF', 'LTREVB', etc.
  
  library(dplyr)
  
  # Exclude rows where 'adjusted_excess_return_eom' is NA
  short_merged_data <- short_merged_data %>%
    filter(!is.na(adjusted_excess_return_eom) & !is.na(dur_m_ret_adj))
  
  # Replace NA in factor columns with 0
  factor_columns <- c("DEF",  "LTREVB", "MKTB.x", "PEADB" ,"MOMB", "TERM", "Traded_Liq", "intermediary_capital_risk_factor",
                      "Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom   ", "REV_LT", "REV_ST", "infl_diff","T10Y2YM","VIX_diff", "COUPON.x",  "Rating_Dummy_7", "downgrade_group")
  short_merged_data[factor_columns] <- lapply(short_merged_data[factor_columns], function(x) {
    ifelse(is.na(x), 0, x)
  })
  
  # Now check the structure and summary to confirm the changes
  str(short_merged_data)
  summary(short_merged_data)
  
  
  # Rename the 'Mom   ' column to 'Mom' by removing extra spaces
  names(short_merged_data)[names(short_merged_data) == "Mom   "] <- "Mom"
  
  
  print(class(short_merged_data))
  
  short_merged_data <- as.data.frame(short_merged_data)
  
  colnames(short_merged_data)
  
  # Replace NA values with 0
  short_merged_data[is.na(short_merged_data)] <- 0
  
 
  # GLS model considering potential autocorrelation within CUSIP groups
  # Here, `corAR1()` specifies an autoregressive process of order 1 within each CUSIP group
  gls_short_model <- gls(adjusted_excess_return_eom ~ DEF + LTREVB + MKTB.x + PEADB + MOMB + TERM + Traded_Liq + 
                     intermediary_capital_risk_factor + Mkt.RF + SMB + HML + RMW + CMA + Mom + 
                     REV_LT + REV_ST + infl_diff + T10Y2YM + VIX_diff ,
                   data = short_merged_data,
                   correlation = corAR1(form = ~ DATE.x.x | CUSIP)) # Ensure DATE.x.x is the correct time variable
  

  # Summary of the GLS model
  summary(gls_short_model)
  
  
  
  # Number of observations
  n <- nrow(short_merged_data)
  
  # Number of predictors (replace this with the actual number of predictors in your model)
  k <- 19  
  
  # Calculate the mean of the dependent variable
  y_bar <- mean(short_merged_data$adjusted_excess_return_eom)
  
  # Calculate SST
  SST <- sum((short_merged_data$adjusted_excess_return_eom - y_bar)^2)
  
  # Calculate SSR using the provided RSE and DF
  RSE <- 6.9709 
  DF_residual <- 21909
  SSR <- RSE^2 * DF_residual
  
  # Calculate R-squared
  R2 <- 1 - (SSR / SST)
  
  # Calculate adjusted R-squared
  R2_adjusted <- 1 - ((SSR / (n - k - 1)) / (SST / (n - 1)))
  
  # Print R-squared and adjusted R-squared
  print(paste("R-squared:", R2))
  print(paste("Adjusted R-squared:", R2_adjusted))
  
  
  # GLS model considering potential autocorrelation within CUSIP groups
  # Here, `corAR1()` specifies an autoregressive process of order 1 within each CUSIP group
  gls_short_model_d <- gls(dur_m_ret_adj ~ DEF + LTREVB + MKTB.x + PEADB + MOMB + TERM + Traded_Liq + 
                           intermediary_capital_risk_factor + Mkt.RF + SMB + HML + RMW + CMA + Mom + 
                           REV_LT + REV_ST + infl_diff + T10Y2YM + VIX_diff,
                         data = short_merged_data,
                         correlation = corAR1(form = ~ DATE.x.x | CUSIP)) # Ensure DATE.x.x is the correct time variable
  
  
  # Summary of the GLS model
  summary(gls_short_model_d)
  
  
  
  # Number of observations
  n <- nrow(short_merged_data)
  
  # Number of predictors (replace this with the actual number of predictors in your model)
  k <- 19  
  
  # Calculate the mean of the dependent variable
  y_bar <- mean(short_merged_data$dur_m_ret_adj)
  
  # Calculate SST
  SST <- sum((short_merged_data$dur_m_ret_adj - y_bar)^2)
  
  # Calculate SSR using the provided RSE and DF
  RSE <- 6.970537   
  DF_residual <- 21909 
  SSR <- RSE^2 * DF_residual
  
  # Calculate R-squared
  R2 <- 1 - (SSR / SST)
  
  # Calculate adjusted R-squared
  R2_adjusted <- 1 - ((SSR / (n - k - 1)) / (SST / (n - 1)))
  
  # Print R-squared and adjusted R-squared
  print(paste("R-squared:", R2))
  print(paste("Adjusted R-squared:", R2_adjusted))
  
  
  
  library(nlme)
  
  # Fit a simple GLS model to portfolio
  gls_model_EW <- gls(equally_weighted_return ~   DEF + LTREVB + MKTB.x + PEADB + MOMB + TERM + Traded_Liq + 
                               intermediary_capital_risk_factor + Mkt.RF + SMB + HML + RMW + CMA + Mom + 
                               REV_LT + REV_ST + infl_diff + T10Y2YM + VIX_diff,
                             data = short_merged_data,
                             correlation = corAR1(form = ~ DATE.x.x | CUSIP))
  
  # Summarize the GLS model
  summary(gls_model_EW)
  
  
  # Number of observations
  n <- nrow(short_merged_data)
  
  # Number of predictors (replace this with the actual number of predictors in your model)
  k <- 19  
  
  # Calculate the mean of the dependent variable
  y_bar <- mean(short_merged_data$equally_weighted_return)
  
  # Calculate SST
  SST <- sum((short_merged_data$equally_weighted_return - y_bar)^2)
  
  # Calculate SSR using the provided RSE and DF
  RSE <- 1.864326   
  DF_residual <- 21909 
  SSR <- RSE^2 * DF_residual
  
  # Calculate R-squared
  R2 <- 1 - (SSR / SST)
  
  # Calculate adjusted R-squared
  R2_adjusted <- 1 - ((SSR / (n - k - 1)) / (SST / (n - 1)))
  
  # Print R-squared and adjusted R-squared
  print(paste("R-squared:", R2))
  print(paste("Adjusted R-squared:", R2_adjusted))
  
  
  
  
  
  ##WW
  
  # GLS model considering potential autocorrelation within CUSIP groups
  # Here, `corAR1()` specifies an autoregressive process of order 1 within each CUSIP group
  gls_model_WW <- gls(weighted_return ~  DEF + LTREVB + MKTB.x + PEADB + MOMB + TERM + Traded_Liq + 
                        intermediary_capital_risk_factor + Mkt.RF + SMB + HML + RMW + CMA + Mom + 
                        REV_LT + REV_ST + infl_diff + T10Y2YM + VIX_diff,
                      data = short_merged_data,
                      correlation = corAR1(form = ~ DATE.x.x | CUSIP)) # Ensure DATE.x.x is the correct time variable
  
  
  # Summary of the GLS model
  summary(gls_model_WW)
  
  # Number of observations
  n <- nrow(short_merged_data)
  
  # Number of predictors (replace this with the actual number of predictors in your model)
  k <- 19  
  
  # Calculate the mean of the dependent variable
  y_bar <- mean(short_merged_data$weighted_return)
  
  # Calculate SST
  SST <- sum((short_merged_data$weighted_return - y_bar)^2)
  
  # Calculate SSR using the provided RSE and DF
  RSE <- 2.100068   
  DF_residual <- 21909 
  SSR <- RSE^2 * DF_residual
  
  # Calculate R-squared
  R2 <- 1 - (SSR / SST)
  
  # Calculate adjusted R-squared
  R2_adjusted <- 1 - ((SSR / (n - k - 1)) / (SST / (n - 1)))
  
  # Print R-squared and adjusted R-squared
  print(paste("R-squared:", R2))
  print(paste("Adjusted R-squared:", R2_adjusted))
  
  

 
 
  
  # Fit an OLS model
  ols_model_short <- lm(adjusted_excess_return_eom ~ DEF + LTREVB + MKTB.x + PEADB + MOMB + TERM + Traded_Liq + 
                          intermediary_capital_risk_factor + Mkt.RF + SMB + HML + RMW + CMA + Mom + 
                          REV_LT + REV_ST + infl_diff + T10Y2YM + VIX_diff ,
                  data = short_merged_data)
  
  # Summarize the OLS model
  summary(ols_model_short)
  
  
  library(forecast)
  
  # Assuming 'residuals' is your vector of residuals from the GLS model
  residuals <- residuals(gls_short_model)
  
  # Plot ACF
  acf(residuals, main="Autocorrelation Function")
  
  # Plot PACF
  pacf(residuals, main="Partial Autocorrelation Function")
  
  # Optionally, using auto.arima on the residuals to perform a Box-Jenkins test
  auto_model <- auto.arima(residuals)
  summary(auto_model)
  
  library(lmtest)
  
  # Run the Breusch-Pagan test
  bp_test <- bptest(ols_model_short)
  
  # Print the results
  print(bp_test)
  
  library(sandwich)
  
  # Coefficient test with robust standard errors
  robust_summary <- coeftest(ols_model_short, vcov = vcovHC(ols_model_short, type = "HC1"))
  
  # Print the summary with robust standard errors
  print(robust_summary)
  
  
  
  
  ##### Regression after downgrade #####
  
  
  
  # Filter the data for observations between 31 days and the end of the observation period and adjust excess returns for a long position
  long_data_for_regression <- merged_table %>%
    filter(date_difference >= 30 & date_difference <= 12*30) %>%
    mutate(adjusted_excess_return_eom = excess_return_eom,
           dur_m_ret_adj=dur_m_ret) %>%
    select(DATE.x, CUSIP, TMT.x, OFFERING_AMT.x, COUPON.x, DURATION.x, T_DATE.x, T_DVolume.x, RET_EOM.x,
           adjusted_excess_return_eom,dur_m_ret_adj, Rating_Dummy_7, downgrade_group, date_difference)  
  
  # Check the structure to ensure only the necessary data is included
  str(long_data_for_regression)
  
  # Display the first few rows to verify adjustments and filtering
  head(long_data_for_regression)
  
  # Calculate minimum and maximum T_DATE from the filtered data
  min_date_long <- min(long_data_for_regression$T_DATE.x)
  max_date_long <- max(long_data_for_regression$T_DATE.x)
  
  # Print the results
  print(paste("Minimum T_DATE:", min_date_long))
  print(paste("Maximum T_DATE:", max_date_long))
  
  # Group by date.x and summarize the data
  date_summary_long <- long_data_for_regression %>%
    group_by(DATE.x) %>%
    summarise(
      total_observations = n(),                             # Total observations per date
      total_cusips = sum(!is.na(CUSIP)),                    # Count of CUSIPs (non-NA)
      unique_cusips = n_distinct(CUSIP)                     # Count of unique CUSIPs
    )
  
  # Print the summary table
  print(date_summary_long)
  
  # Calculate equally weighted portfolio returns
  equally_weighted_returns_long <- long_data_for_regression %>%
    group_by(DATE.x) %>%
    summarise(equally_weighted_return = mean(adjusted_excess_return_eom, na.rm = TRUE))
  
  print(equally_weighted_returns_long)
  
  # Calculate offering amount weighted portfolio returns
  offering_amount_weighted_returns_long <- long_data_for_regression %>%
    group_by(DATE.x) %>%
    summarise(
      total_offering_amt = sum(OFFERING_AMT.x, na.rm = TRUE),
      weighted_return = sum(adjusted_excess_return_eom * OFFERING_AMT.x, na.rm = TRUE) / total_offering_amt
    )
  
  print(offering_amount_weighted_returns_long)
  
  # Merge the results
  long_portfolio_returns <- merge(equally_weighted_returns_long, offering_amount_weighted_returns_long, by = "DATE.x", all = TRUE)
  
  print(long_portfolio_returns)
  
  average_return_e <- mean(long_portfolio_returns$equally_weighted_return, na.rm = TRUE)
  average_return_o <- mean(long_portfolio_returns$weighted_return, na.rm = TRUE)
  volatility_return_e <- sd(long_portfolio_returns$equally_weighted_return, na.rm = TRUE)
  volatility_return_o <- sd(long_portfolio_returns$weighted_return, na.rm = TRUE)

  print(paste("Average Equally Weighted Return:", average_return_e))
  print(paste("Average Weighted Return:", average_return_o))
  print(paste("Volatility of Equally Weighted Return:", volatility_return_e))
  print(paste("Volatility of Weighted Return:", volatility_return_o))
  
  
  
  
  # Reset the graphics device
  dev.off()
  
 
  library(ggplot2)
  
  library(ggplot2)
  
  # Plot the graph with transparent red line
  plot_long_returns_transparent <- ggplot(long_portfolio_returns, aes(x = DATE.x)) +
    geom_line(aes(y = equally_weighted_return, color = "Equally Weighted"), size = 1.2) +
    geom_line(aes(y = weighted_return, color = "Offering Amount Weighted"), size = 1.2, alpha = 0.5) +
    labs(
      title = "After Downgrade Portfolios Return Over Time",
      x = "Date", 
      y = "Return (%)",
      color = "Portfolio Type"
    ) +
    scale_color_manual(values = c("Equally Weighted" = "blue", "Offering Amount Weighted" = "red")) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      panel.grid.major = element_line(size = 0.5, linetype = 'dashed', color = 'grey'),
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  # Display the plot
  print(plot_long_returns_transparent)
  
  # Save the plot as a PNG file
  ggsave("long_portfolio_returns_transparent.png", plot = plot_long_returns_transparent, width = 12, height = 6, dpi = 300)
  
  
  
  
  # Ensure all dataframes have the 'Year_Month' column prepared
  long_data_for_regression <- long_data_for_regression %>%
    mutate(Year_Month = format(DATE.x, "%Y-%m"))
  
  long_portfolio_returns <- long_portfolio_returns %>%
    mutate(Year_Month = format(DATE.x, "%Y-%m"))
  
  aligned_data <- aligned_data %>%
    mutate(Year_Month = format(date, "%Y-%m"))
  
  # Merge the first two datasets
  merged_data_first_step_long <- left_join(long_data_for_regression, long_portfolio_returns, by = "Year_Month")
  
  # Merge the resulting dataframe with the third dataset
  long_merged_data <- left_join(merged_data_first_step_long, aligned_data, by = "Year_Month")
  
  # Check the structure to ensure the merge was successful
  str(long_merged_data)
  
  # Exclude rows where 'adjusted_excess_return_eom' is NA
  long_merged_data <- long_merged_data %>%
    filter(!is.na(adjusted_excess_return_eom) & !is.na(dur_m_ret_adj))
  
  
  
  # Check the exact column names
  colnames(long_merged_data)
  
 
  
  # Replace NA in factor columns with 0
  factor_columns <- c("DEF", "LTREVB", "MKTB.x", "PEADB" ,"MOMB", "TERM", "Traded_Liq", "intermediary_capital_risk_factor",
                      "Mkt.RF", "SMB", "HML", "RMW", "CMA", "Mom   ", "REV_LT", "REV_ST", "infl_diff","T10Y2YM","VIX_diff", "COUPON.x",  "Rating_Dummy_7", "downgrade_group")
  long_merged_data[factor_columns] <- lapply(long_merged_data[factor_columns], function(x) {
    ifelse(is.na(x), 0, x)
  })
  

  
  # Now check the structure and summary to confirm the changes
  str(long_merged_data)
  summary(long_merged_data)
  
  # Rename the 'Mom  ' column to 'Mom' by removing extra spaces
  names(long_merged_data)[names(long_merged_data) == "Mom   "] <- "Mom"

  # GLS model considering potential autocorrelation within CUSIP groups
  gls_model_long <- gls(adjusted_excess_return_eom ~ DEF + LTREVB + MKTB.x + PEADB + MOMB + TERM + Traded_Liq + 
                          intermediary_capital_risk_factor + Mkt.RF + SMB + HML + RMW + CMA + Mom + 
                          REV_LT + REV_ST + infl_diff + T10Y2YM + VIX_diff,
                        data = long_merged_data,
                        correlation = corAR1(form = ~ DATE.x.x | CUSIP)) # Ensure DATE.x.x is the correct time variable
  
  # Summary of the GLS model
  summary(gls_model_long)
  
  
  # Number of observations
  n <- nrow(long_merged_data)
  
  # Number of predictors (replace this with the actual number of predictors in your model)
  k <- 19  
  
  # Calculate the mean of the dependent variable
  y_bar <- mean(long_merged_data$adjusted_excess_return_eom)
  
  # Calculate SST
  SST <- sum((long_merged_data$adjusted_excess_return_eom - y_bar)^2)
  
  # Calculate SSR using the provided RSE and DF
  RSE <- 6.837192 
  DF_residual <- 18433
  SSR <- RSE^2 * DF_residual
  
  # Calculate R-squared
  R2 <- 1 - (SSR / SST)
  
  # Calculate adjusted R-squared
  R2_adjusted <- 1 - ((SSR / (n - k - 1)) / (SST / (n - 1)))
  
  # Print R-squared and adjusted R-squared
  print(paste("R-squared:", R2))
  print(paste("Adjusted R-squared:", R2_adjusted))
  
  
  
  # GLS model considering potential autocorrelation within CUSIP groups
  gls_model_long_d <- gls(dur_m_ret_adj~ DEF + LTREVB + MKTB.x + PEADB + MOMB + TERM + Traded_Liq + 
                          intermediary_capital_risk_factor + Mkt.RF + SMB + HML + RMW + CMA + Mom + 
                          REV_LT + REV_ST + infl_diff + T10Y2YM + VIX_diff,
                        data = long_merged_data,
                        correlation = corAR1(form = ~ DATE.x.x | CUSIP)) # Ensure DATE.x.x is the correct time variable
  
  # Summary of the GLS model
  summary(gls_model_long_d)
  
  
  # Number of observations
  n <- nrow(long_merged_data)
  
  # Number of predictors (replace this with the actual number of predictors in your model)
  k <- 19  
  
  # Calculate the mean of the dependent variable
  y_bar <- mean(long_merged_data$dur_m_ret_adj)
  
  # Calculate SST
  SST <- sum((long_merged_data$dur_m_ret_adj - y_bar)^2)
  
  # Calculate SSR using the provided RSE and DF
  RSE <- 6.832222  
  DF_residual <- 18433
  SSR <- RSE^2 * DF_residual
  
  # Calculate R-squared
  R2 <- 1 - (SSR / SST)
  
  # Calculate adjusted R-squared
  R2_adjusted <- 1 - ((SSR / (n - k - 1)) / (SST / (n - 1)))
  
  # Print R-squared and adjusted R-squared
  print(paste("R-squared:", R2))
  print(paste("Adjusted R-squared:", R2_adjusted))
  
  

  
  # Fit a simple GLS model without AR or MA components
  simple_gls_model_long <- gls(adjusted_excess_return_eom ~ DEF + LTREVB + MKTB.x + PEADB + MOMB + TERM + Traded_Liq + 
                                 intermediary_capital_risk_factor + Mkt.RF + SMB + HML + RMW + CMA + Mom + 
                                 REV_LT + REV_ST + infl_diff + T10Y2YM + VIX_diff,
                               data = long_merged_data)
  
  # Summarize the GLS model
  summary(simple_gls_model_long)
  
  # Fit an OLS model
  ols_model_long <- lm(adjusted_excess_return_eom ~ DEF + LTREVB + MKTB.x + PEADB + MOMB + TERM + Traded_Liq + 
                         intermediary_capital_risk_factor + Mkt.RF + SMB + HML + RMW + CMA + Mom + 
                         REV_LT + REV_ST + infl_diff + T10Y2YM + VIX_diff,
                       data = long_merged_data)
  
  # Summarize the OLS model
  summary(ols_model_long)
  
  # Fit an OLS model
  ols_model_long_d <- lm(dur_m_ret_adj ~ DEF + LTREVB + MKTB.x + PEADB + MOMB + TERM + Traded_Liq + 
                         intermediary_capital_risk_factor + Mkt.RF + SMB + HML + RMW + CMA + Mom + 
                         REV_LT + REV_ST + infl_diff + T10Y2YM + VIX_diff + COUPON.x + Rating_Dummy_7 + downgrade_group,
                       data = long_merged_data)
  
  # Summarize the OLS model
  summary(ols_model_long_d)
  

  
  # Run the Breusch-Pagan test
  bp_test_long <- bptest(ols_model_long)
  
  # Print the results
  print(bp_test_long)
  
  # Coefficient test with robust standard errors
  robust_summary_long <- coeftest(ols_model_long, vcov = vcovHC(ols_model_long, type = "HC1"))
  
  # Print the summary with robust standard errors
  print(robust_summary_long)
  
  
  
  ##WW
  
  # GLS model considering potential autocorrelation within CUSIP groups
  # Here, `corAR1()` specifies an autoregressive process of order 1 within each CUSIP group
  gls_model_long_WW <- gls(weighted_return ~  DEF + LTREVB + MKTB.x + PEADB + MOMB + TERM + Traded_Liq + 
                        intermediary_capital_risk_factor + Mkt.RF + SMB + HML + RMW + CMA + Mom + 
                        REV_LT + REV_ST + infl_diff + T10Y2YM + VIX_diff ,
                      data = long_merged_data,
                      correlation = corAR1(form = ~ DATE.x.x | CUSIP)) # Ensure DATE.x.x is the correct time variable
  
  
  # Summary of the GLS model
  summary(gls_model_long_WW)
  
  
  # Number of observations
  n <- nrow(long_merged_data)
  
  # Number of predictors (replace this with the actual number of predictors in your model)
  k <- 19  
  
  # Calculate the mean of the dependent variable
  y_bar <- mean(long_merged_data$weighted_return)
  
  # Calculate SST
  SST <- sum((long_merged_data$weighted_return - y_bar)^2)
  
  # Calculate SSR using the provided RSE and DF
  RSE <- 1.951073   
  DF_residual <- 18433
  SSR <- RSE^2 * DF_residual
  
  # Calculate R-squared
  R2 <- 1 - (SSR / SST)
  
  # Calculate adjusted R-squared
  R2_adjusted <- 1 - ((SSR / (n - k - 1)) / (SST / (n - 1)))
  
  # Print R-squared and adjusted R-squared
  print(paste("R-squared:", R2))
  print(paste("Adjusted R-squared:", R2_adjusted))
  


  
  ##EW
  
  
  # GLS model considering potential autocorrelation within CUSIP groups
  # Here, `corAR1()` specifies an autoregressive process of order 1 within each CUSIP group
  gls_model_long_EW <- gls(equally_weighted_return ~  DEF + LTREVB + MKTB.x + PEADB + MOMB + TERM + Traded_Liq + 
                        intermediary_capital_risk_factor + Mkt.RF + SMB + HML + RMW + CMA + Mom + 
                        REV_LT + REV_ST + infl_diff + T10Y2YM + VIX_diff ,
                      data = long_merged_data,
                      correlation = corAR1(form = ~ DATE.x.x | CUSIP)) # Ensure DATE.x.x is the correct time variable
  
  
  # Summary of the GLS model
  summary(gls_model_long_EW)
  
  
  
  n <- nrow(long_merged_data)
  
  # Number of predictors (replace this with the actual number of predictors in your model)
  k <- 19  
  
  # Calculate the mean of the dependent variable
  y_bar <- mean(long_merged_data$equally_weighted_return)
  
  # Calculate SST
  SST <- sum((long_merged_data$equally_weighted_return - y_bar)^2)
  
  # Calculate SSR using the provided RSE and DF
  RSE <- 1.720394   
  DF_residual <- 18433
  SSR <- RSE^2 * DF_residual
  
  # Calculate R-squared
  R2 <- 1 - (SSR / SST)
  
  # Calculate adjusted R-squared
  R2_adjusted <- 1 - ((SSR / (n - k - 1)) / (SST / (n - 1)))
  
  # Print R-squared and adjusted R-squared
  print(paste("R-squared:", R2))
  print(paste("Adjusted R-squared:", R2_adjusted))
  
  
  
  
  install.packages("stargazer")
  library(stargazer)
  
  
  # Example for multiple models
  stargazer(gls_short_model, gls_model_long,
            type = "html", out = "GLS_results.html")
  
  stargazer(gls_short_model_d, gls_model_long_d,
            type = "html", out = "GLS_results_d.html")
  
  stargazer(gls_model_WW, gls_model_EW, gls_model_long_WW, gls_model_long_EW, 
            type = "html", out = "GLS_portfolio_results.html")
  
  
  
  ##### Statictics #####
  
  # Load necessary library
  library(dplyr)
  
  colnames(short_portfolio_returns)
  
  # Create summary table for short_portfolio_returns
  short_summary <- short_merged_data %>%
    summarise(
      adjusted_return_eom_mean = mean(adjusted_excess_return_eom, na.rm = TRUE),
      adjusted_return_eom_median = median(adjusted_excess_return_eom, na.rm = TRUE),
      adjusted_return_eom_sd = sd(adjusted_excess_return_eom, na.rm = TRUE),
      dur_m_ret_adj_mean = mean(dur_m_ret_adj, na.rm = TRUE),
      dur_m_ret_adj_median = median(dur_m_ret_adj , na.rm = TRUE),
      dur_m_ret_adj_sd = sd(dur_m_ret_adj , na.rm = TRUE),
      weighted_return_mean = mean(weighted_return, na.rm = TRUE),
      weighted_return_median = median(weighted_return, na.rm = TRUE),
      weighted_return_sd = sd(weighted_return, na.rm = TRUE),
      equally_weighted_return_mean = mean(equally_weighted_return, na.rm = TRUE),
      equally_weighted_return_median = median(equally_weighted_return, na.rm = TRUE),
      equally_weighted_return_sd = sd(equally_weighted_return, na.rm = TRUE)
    )
  
  # Create summary table for long_portfolio_returns
  long_summary <- long_merged_data %>%
    summarise(
      adjusted_return_eom_mean = mean(adjusted_excess_return_eom, na.rm = TRUE),
      adjusted_return_eom_median = median(adjusted_excess_return_eom, na.rm = TRUE),
      adjusted_return_eom_sd = sd(adjusted_excess_return_eom, na.rm = TRUE),
      dur_m_ret_adj_mean = mean(dur_m_ret_adj, na.rm = TRUE),
      dur_m_ret_adj_median = median(dur_m_ret_adj , na.rm = TRUE),
      dur_m_ret_adj_sd = sd(dur_m_ret_adj , na.rm = TRUE),
      weighted_return_mean = mean(weighted_return, na.rm = TRUE),
      weighted_return_median = median(weighted_return, na.rm = TRUE),
      weighted_return_sd = sd(weighted_return, na.rm = TRUE),
      equally_weighted_return_mean = mean(equally_weighted_return, na.rm = TRUE),
      equally_weighted_return_median = median(equally_weighted_return, na.rm = TRUE),
      equally_weighted_return_sd = sd(equally_weighted_return, na.rm = TRUE)
    )
  
  # Combine both summaries into one table
  summary_table <- rbind(
    cbind(Type = "Short", short_summary),
    cbind(Type = "Long", long_summary)
  )
  
  # Display the combined summary table
  print(summary_table)
  
  
  
  # Calculate the Sharpe Ratio for short portfolio
  short_SR_adjusted <- (short_summary$adjusted_return_eom_mean ) / short_summary$adjusted_return_eom_sd
  short_SR_adjusted_d <- (short_summary$dur_m_ret_adj_mean ) / short_summary$dur_m_ret_adj_sd
  short_SR_weighted <- (short_summary$weighted_return_mean ) / short_summary$weighted_return_sd
  short_SR_equally <- (short_summary$equally_weighted_return_mean ) / short_summary$equally_weighted_return_sd
  
  # Calculate the Sharpe Ratio for long portfolio
  long_SR_adjusted <- (long_summary$adjusted_return_eom_mean ) / long_summary$adjusted_return_eom_sd
  long_SR_adjusted_d <- (long_summary$dur_m_ret_adj_mean ) / long_summary$dur_m_ret_adj_sd
  long_SR_weighted <- (long_summary$weighted_return_mean ) / long_summary$weighted_return_sd
  long_SR_equally <- (long_summary$equally_weighted_return_mean ) / long_summary$equally_weighted_return_sd
  
  # Combine the Sharpe Ratios into a summary table
  sharpe_ratios <- data.frame(
    Portfolio_Type = c("Short", "Long"),
    SR_Adjusted = c(short_SR_adjusted, long_SR_adjusted),
    SR_Adjusted_d = c(short_SR_adjusted_d, long_SR_adjusted_d),
    SR_Weighted = c(short_SR_weighted, long_SR_weighted),
    SR_Equally = c(short_SR_equally, long_SR_equally)
  )
  
  # Print the summary table with Sharpe Ratios
  print(sharpe_ratios)
  
  
  
  
  # Adjust the return calculation to account for compounding, properly handling NAs for both return columns
  merged_table <- merged_table %>%
    group_by(CUSIP) %>%
    mutate(
      # Adjust returns to be compounded, and handle NAs by assuming no change (0% return)
      adj_return_eom = if_else(is.na(excess_return_eom), 1, 1 + excess_return_eom / 100),
      adj_return_dur = if_else(is.na(dur_m_ret), 1, 1 + dur_m_ret / 100)
    ) %>%
    arrange(T_DATE.x) %>%
    # Calculate cumulative returns, compounding across all dates within each CUSIP for both return types
    mutate(
      # Start the cumulative product calculation, subtract 1 at the end for actual cumulative return
      cum_return_to_date_eom = cumprod(adj_return_eom) - 1,
      cum_return_to_date_dur = cumprod(adj_return_dur) - 1
    ) %>%
    ungroup()
  
  # Adjust the calculation to display as percentage by multiplying by 100, if necessary
  merged_table <- merged_table %>%
    mutate(
      cum_return_to_date_eom = cum_return_to_date_eom * 100,
      cum_return_to_date_dur = cum_return_to_date_dur * 100
    )
  
  # Print specific CUSIP data to verify the calculations
  specific_cusip_data <- merged_table %>%
    filter(CUSIP == "893817AA4") %>%
    dplyr::select(CUSIP, T_DATE.x, excess_return_eom, cum_return_to_date_eom, dur_m_ret, cum_return_to_date_dur, date_difference)
  
  # Print the data
  print(specific_cusip_data)
  
  
  volatility_summary <- merged_table %>%
    filter(month >= -24 & month <= 36) %>%
    group_by(month) %>%
    summarise(
      Volatility_EOM = sd(cum_return_to_date_eom, na.rm = TRUE),
      Volatility_Dur = sd(cum_return_to_date_dur, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Optionally, display the first few rows of the volatility summary to check the results
  print(volatility_summary)
  
  library(dplyr)
  
  # Assuming merged_table is already loaded with the necessary columns and cum_return_to_date_eom, cum_return_to_date_dur
  
  # Define two specific time windows for volatility calculation
  time_windows <- list(
    Before_Downgrade = -24:-1,
    After_Downgrade = 1:36
  )
  
  # Initialize a list to store the results
  volatility_summary <- list()
  
  # Calculate volatility for each time window
  for (window_name in names(time_windows)) {
    window_range <- time_windows[[window_name]]
    
    # Calculate volatility for the current window
    summary_data <- merged_table %>%
      filter(month %in% window_range) %>%
      summarise(
        Volatility_EOM = sd(cum_return_to_date_eom, na.rm = TRUE),
        Volatility_Dur = sd(cum_return_to_date_dur, na.rm = TRUE)
      ) %>%
      mutate(Window = window_name) %>%
      ungroup()
    
    # Store the result in the list
    volatility_summary[[window_name]] <- summary_data
  }
  
  # Combine all summaries into a single data frame
  volatility_summary <- bind_rows(volatility_summary)
  
  # Print the summary
  print(volatility_summary)
  
  
  ##### Cummulative return Graph #####
  
 
  
  # Assuming merged_table is already loaded and contains necessary columns
  library(dplyr)
  
  # Adjust the return calculation to account for compounding, properly handling NAs for both return columns
  merged_table <- merged_table %>%
    group_by(CUSIP) %>%
    mutate(
      # Adjust returns to be compounded, and handle NAs by assuming no change (0% return)
      adj_return_eom = if_else(is.na(excess_return_eom), 0, excess_return_eom / 100),
      adj_return_dur = if_else(is.na(dur_m_ret), 0, dur_m_ret / 100)
    ) %>%
    arrange(T_DATE.x) %>%
    # Calculate cumulative returns, compounding across all dates within each CUSIP for both return types
    mutate(
      # Start the cumulative product calculation, subtract 1 at the end for actual cumulative return
      cum_return_to_date_eom = cumprod(1 + adj_return_eom),
      cum_return_to_date_dur = cumprod(1 + adj_return_dur)
    ) %>%
    ungroup()
  
  # Rebase cumulative returns to start at 1 for month -24
  merged_table <- merged_table %>%
    group_by(CUSIP) %>%
    mutate(
      # Check if there are observations for month -24
      rebasing_factor_eom = ifelse(any(month == -24), cum_return_to_date_eom[month == -24], 1),
      rebasing_factor_dur = ifelse(any(month == -24), cum_return_to_date_dur[month == -24], 1),
      # Rebase cumulative returns
      cum_return_to_date_eom = cum_return_to_date_eom / rebasing_factor_eom,
      cum_return_to_date_dur = cum_return_to_date_dur / rebasing_factor_dur
    ) %>%
    ungroup()
  
  # Adjust the calculation to display as percentage by multiplying by 100, if necessary
  merged_table <- merged_table %>%
    mutate(
      cum_return_to_date_eom = cum_return_to_date_eom * 100,
      cum_return_to_date_dur = cum_return_to_date_dur * 100
    )
  
  # Print specific CUSIP data to verify the calculations
  specific_cusip_data <- merged_table %>%
    filter(CUSIP == "893817AA4") %>%
    dplyr::select(CUSIP, T_DATE.x, excess_return_eom, cum_return_to_date_eom, dur_m_ret, cum_return_to_date_dur, date_difference)
  
  # Print the data
  print(specific_cusip_data)
  
  
  # Summary table for average and median cumulative returns for both excess and duration-matched returns, grouped by month window
  summary_by_window <- merged_table %>%
    filter(month >= -24 & month <= 36) %>%
    group_by(month) %>%
    summarise(
      Avg_Cum_Return_EOM = mean(cum_return_to_date_eom/100, na.rm = TRUE),
      Median_Cum_Return_EOM = median(cum_return_to_date_eom/100, na.rm = TRUE),
      Avg_Cum_Return_Dur = mean(cum_return_to_date_dur/100, na.rm = TRUE),
      Median_Cum_Return_Dur = median(cum_return_to_date_dur/100, na.rm = TRUE),
      Count = n()
    )
  
  # Format numeric columns to three decimal places
  summary_by_window$Avg_Cum_Return_EOM <- format(summary_by_window$Avg_Cum_Return_EOM, nsmall = 3)
  summary_by_window$Avg_Cum_Return_Dur <- format(summary_by_window$Avg_Cum_Return_Dur, nsmall = 3)
  
  
  print(n=61, summary_by_window)
  
  # Plotting the data
  library(ggplot2)
  
  # Assuming 'summary_by_window' contains the summarized data for plotting
  summary_by_window <- data.frame(
    month = c(-24:-1, 1:36),
    Avg_Cum_Return_EOM = c(1, 1.0231200, 1.0255733, 1.0289300, 1.0297820, 1.0340906, 1.0398514, 1.0419979, 1.0381510, 1.0421078, 1.0514228, 1.0464148, 1.0423546, 1.0437027, 1.0398669, 1.0480382 , 1.0421968, 1.0394803, 1.0323819, 1.0316513, 1.0285676 , 1.0175974, 1.0083032, 1.0299293, 0.9870578, 1.0114493, 1.0285452, 1.0411357, 1.0474118, 1.0573086, 1.0527193 , 1.0715921, 1.0855130, 1.0805155, 1.0947836, 1.0895930, 1.1069027, 1.1344918, 1.1250142, 1.1422783, 1.1520308, 1.1460447 , 1.1804824, 1.1839466, 1.1778610, 1.1904824, 1.1974482, 1.1962841, 1.2046284, 1.2231714, 1.2102853, 1.2184500, 1.2352005, 1.2325643, 1.2272143, 1.2601443, 1.2608474, 1.2676212, 1.2891972, 1.2888852 ),
    Avg_Cum_Return_Dur = c(1, 1.0149728, 1.0183441, 1.0185081, 1.0185438, 1.0217504, 1.0238223, 1.0233497, 1.0162887, 1.0173449, 1.0233171, 1.0190058, 1.0171248, 1.0148467, 1.0108364 , 1.0125847, 1.0052841, 1.0019738, 0.9954516, 0.9928446 , 0.9890397, 0.9830263, 0.9627142, 0.9848396, 0.9433837, 0.9622788, 0.9758585, 0.9851853, 0.9867583, 0.9949010 , 0.9914242, 1.0066818, 1.0155666, 1.0071034, 1.0211805, 1.0176935, 1.0311668, 1.0531485, 1.0418766, 1.0531147, 1.0616591 , 1.0521902, 1.0812991, 1.0870462, 1.0764749, 1.0891247, 1.0949450, 1.0940413, 1.1017877, 1.1148694, 1.1017146, 1.1038339, 1.1137454, 1.1149999, 1.1101346, 1.1263712, 1.1254544, 1.1298613, 1.1443095, 1.1468792 )
  )
  
  # Convert 'summary_by_window' to a long format for plotting
  long_data <- summary_by_window %>%
    pivot_longer(cols = c("Avg_Cum_Return_EOM", "Avg_Cum_Return_Dur"), names_to = "Type", values_to = "Value")
  
  # Create the line plot
  ggplot(long_data, aes(x = month, y = Value, color = Type, group = Type)) +
    geom_line() +
    labs(title = "Average Cumulative Returns Around Downgrade",
         x = "Month Relative to Downgrade",
         y = "Average Cumulative Return",
         color = "Type of Return") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(-24, 36, 12))
  
  # Save the plot as a PNG file
  ggsave("Cumulative_Returns_Around_Downgrade.png", plot = last_plot(), width = 10, height = 6, dpi = 300)
  