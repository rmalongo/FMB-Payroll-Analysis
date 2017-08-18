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

# remove duplicates
df2 <- unique.data.frame(df1)

# data manipulation and adding a gender column (with First name)
df3 <- df2 %>% 
  mutate(First_name = str_split_fixed(name, " ", 3)[,1],
         Middle_name =str_split_fixed(name, " ",3)[,2],
         Last_name = str_split_fixed(name, " ", 3)[,3]) %>% 
  select(employee_id, First_name, Middle_name, Last_name, basic_salary, 
         employee_pension_contribution, employer_pension_contribution) 

# Predict gender using  first name
df4 <- df3 %>% 
  rowwise() %>% 
  do(results = gender(.$First_name, method = "ssa")) %>% 
  do(bind_rows(.$results))

# predict gender using middle name
df5 <- df3 %>% 
  rowwise() %>% 
  do(results = gender(.$Middle_name, method = "ssa")) %>% 
  do(bind_rows(.$results))
 
# Joining first name with name in Results from gender function
df6 <- inner_join(df3,df4, by = c("First_name"="name"))

# Joining middle name with name in Results from Gender function
df7 <- inner_join(df3,df5, by = c("First_name" = "name"))

# Find unique employees for both first and last name
df8 <- unique(df6)
df9 <- unique(df7)

# Combine data frame
df10 <- df8 %>% 
  rbind(df9) %>% 
  select(basic_salary, gender, employee_id)

# data visualization:
ggplot(df3, aes(x=basic_salary))+ geom_histogram(bins = 100)

# Histogram
ggplot(df10, aes(x=gender, y=basic_salary))+geom_boxplot(fill="bisque") 
ggplot(df10, aes(x=basic_salary))+ geom_histogram(bins = 100)
