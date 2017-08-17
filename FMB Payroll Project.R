# FMB Payroll Data


# Load revelant packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(babynames)
library(gender)
library(genderdata)

# Load data
df1 <- read.csv("bank_payroll.csv")
unique(df1$employee_id)

# remove duplicates
df2 <- unique.data.frame(df1)


# data manipulation
df3 <- df2 %>% 
  mutate(First_name = str_split_fixed(name, " ", 3)[,1],
         Middle_name =str_split_fixed(name, " ",3)[,2],
         Last_name = str_split_fixed(name, " ", 3)[,3]) %>% 
  select(employee_id, First_name, Middle_name, Last_name, basic_salary, 
         employee_pension_contribution, employer_pension_contribution)

# data visualization
ggplot(df3, aes(x=basic_salary))+ geom_histogram(bins = 100)

# Load names dataframe
Babynames <-babynames
Babynames_clean <- Babynames %>% 
  rename(First_name = name) %>% 
  select(sex, First_name) %>% 
  unique()

# Uning the Babynames package
df4 <- left_join(df3, Babynames_clean, by ="First_name")
rm(Babynames1)

# Using the Gender package
Gender_names<- as.data.frame(gender(df3$First_name))

