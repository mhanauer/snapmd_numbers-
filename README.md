---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Load data SnapMD client count
####### Data cleaning notes #####################
Removing those clients with test in first last name or know Centerstone staff 
Removing those before Monday, March 23rd
Removing consultations times less than 1 minute consitent with Nathan
Descriptive statistcs include multiple sessions of clients not based on unique clients
Assuming demographics in SnapMD are correct

** Maybe look at
Watch for appointments that take place back to back

```{r}
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")
client_count_data = read.csv("snap_data_4_2_20_5pm.csv", header = TRUE)

library(lubridate)
```
Clean data
Delete all data before Monday, March 23rd
Get rid of all test clients
Last name: Nehrt.TestPatient, test, Leverett.Test, Test Patient, Dobbs_Test, Nehrt, Graham-test, Test.Account, Testerson Jr.
First and Last: shawn brooks, tyka williams

```{r}
client_count_dat = client_count_data
head(client_count_dat)

#clean up count time
describe.factor(client_count_dat$P..Consultation.Time)
client_count_dat$P..Consultation.Time = gsub("\\D", "", client_count_dat$P..Consultation.Time)
client_count_dat$P..Consultation.Time = as.numeric(client_count_dat$P..Consultation.Time)
describe.factor(client_count_dat$P..Consultation.Time)


## Get rid of anyone with date before Monday, March 23rd
client_count_dat$D..Consultation.Dates = mdy(client_count_dat$D..Consultation.Dates)
describe.factor(client_count_dat$D..Consultation.Dates)
client_count_dat = subset(client_count_dat, D..Consultation.Dates >= "2020-03-23")
dim(client_count_dat)
describe.factor(client_count_dat$D..Consultation.Dates)
## Lower case and try to get rid of tests
client_count_dat$P..Last.Name = tolower(client_count_dat$P..Last.Name)
client_count_dat$P..First.Name = tolower(client_count_dat$P..First.Name)

describe.factor(client_count_dat$P..Last.Name)

### Get rid of last names
client_count_dat = subset(client_count_dat, P..Last.Name != "dobbs_test" & P..Last.Name != "nehrt.testpatient" & P..Last.Name != "test" & P..Last.Name != "leverett.test" & P..Last.Name != "test patient" & P..Last.Name !=  "nehrt" & P..Last.Name != "graham-test" & P..Last.Name != "test.account" & P..Last.Name != "testerson jr.", P..Last.Name != "testerson")
describe.factor(client_count_dat$P..Last.Name)
### Get rid of first and last names need to create an indicator variable
client_count_dat$delete = ifelse(client_count_dat$P..Last.Name == "brooks" & client_count_dat$P..First.Name == "shawn", 1, 0)

subset(client_count_dat, delete == 1)

client_count_dat$delete = ifelse(client_count_dat$P..Last.Name == "williams" & client_count_dat$P..First.Name == "tyka", 1, client_count_dat$delete)
## Drop CRI staff
client_count_dat = subset(client_count_dat, delete == 0)
## Check that test clients are excluded
describe.factor(client_count_dat$P..Last.Name)
# Get count
n_clients =  dim(client_count_dat)[1]
# Drop delete variable
client_count_dat$delete = NULL


### clean up age
describe.factor(client_count_dat$P..Age)
client_count_dat$P..Age = gsub("\\D", "", client_count_dat$P..Age)
client_count_dat$P..Age = as.numeric(client_count_dat$P..Age)
describe.factor(client_count_dat$P..Age)
hist(client_count_dat$P..Age)
### Clean up wait time
client_count_dat$P..Wait.Time = gsub("\\D", "", client_count_dat$P..Wait.Time)
client_count_dat$P..Wait.Time = as.numeric(client_count_dat$P..Wait.Time)
describe.factor(client_count_dat$P..Wait.Time)
hist(client_count_dat$P..Wait.Time)
#Clean up others
head(client_count_dat)
describe.factor(client_count_dat$P..Gender)
head(client_count_dat)
describe.factor(client_count_dat$P..Ethnicity)
### Get rid of clients with less than 1 minute of consultation time see if you can match close to Nathan
client_count_dat = subset(client_count_dat, P..Consultation.Time >= 1 )
describe.factor(client_count_dat_1_greater$D..Consultation.Dates, decr.order=FALSE)
### In the ballpark
```
Get numeric descriptives
P..Wait.Time
P..Consultation.Time
P..Age
```{r}
head(client_count_dat)
numeric_des = client_count_dat[c("P..Wait.Time", "P..Consultation.Time", "P..Age")]
head(numeric_des)
numeric_names = c("wait time", "consultation time", "age")
numeric_des_mean = round(apply(numeric_des,2, median, na.rm = TRUE)),2)
numeric_des_mean

numeric_des_sd = round(apply(numeric_des, 2, sd, na.rm = TRUE),2)
numeric_des_sd

numeric_des_dat = paste0(numeric_des_mean, "(",numeric_des_sd, ")" )

numeric_des_dat = data.frame(t(numeric_des_dat))
numeric_des_dat = data.frame(mean_sd = t(numeric_des_dat))
vars = c("wait time", "Consultation time", "age")
numeric_des_dat = data.frame(vars, numeric_des_dat)

desc_range = apply(numeric_des, 2, range, na.rm = TRUE)
desc_range = t(desc_range)
desc_range = round(desc_range,2)
desc_range = paste0(desc_range[,1], sep = ",", desc_range[,2])
numeric_des_dat$range = desc_range
numeric_des_dat

write.csv(numeric_des_dat)
describe.factor(client_count_dat$D..Consultation.Dates, decr.order=FALSE)
### Descriptives other than number of clients per day
head(client_count_dat)
#D..Consultation.Dates, P..Gender, P..Ethnicity
```
Just demonstrate trend get count per day by client and see if age is predictive
Female == 1; Male == 0
```{r}
dat_count_per_day = client_count_dat
dat_count_per_day$female = ifelse(dat_count_per_day$P..Gender == "Female", 1, 0)
dat_count_per_day$male = ifelse(dat_count_per_day$P..Gender == "Male", 1, 0)

zero_suicide_dat_agg = zero_suicide_dat_agg %>%
  group_by(D..Consultation.Dates) %>%
  summarise_all(funs(sum))

```



Count results here
```{r}
n_clients
numeric_des_dat

```
Need to get kept rate if possible
