---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Load data SnapMD client count

Need to filter out tests 
those before Monday, March 23rd
Need those who have a dimissal reason
Watch for appointments that take place back to back

```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/TelehealthCIN/data_codebooks")
client_count_data = read.csv("snap_data_3_31_20_9am.csv", header = TRUE)

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
client_count_dat = subset(client_count_dat, P..Last.Name != "dobbs_test" & P..Last.Name != "nehrt.testpatient" & P..Last.Name != "test" & P..Last.Name != "leverett.test" & P..Last.Name != "test patient" & P..Last.Name !=  "nehrt" & P..Last.Name != "graham-test" & P..Last.Name != "test.account" & P..Last.Name != "testerson jr.")
describe.factor(client_count_dat$P..Last.Name)
### Get rid of first and last names need to create an indicator variable
client_count_dat$delete = ifelse(client_count_dat$P..Last.Name == "brooks" & client_count_dat$P..First.Name == "shawn", 1, 0)

subset(client_count_dat, delete == 1)

client_count_dat$delete = ifelse(client_count_dat$P..Last.Name == "williams" & client_count_dat$P..First.Name == "tyka", 1, client_count_dat$delete)
## Drop CRI staff
client_count_dat = subset(client_count_dat, delete == 0)
# Get count
n_clients =  dim(client_count_dat)[1]
# Drop delete variable
client_count_dat$delete = NULL
client_count_dat

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

```
Count results here
```{r}
n_clients
client_count_dat
```
Need to get kept rate if possible
