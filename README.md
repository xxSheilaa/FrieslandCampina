# Data summary 

#rm(list = ls())

library(anytime)

setwd("C:/Users/kinse/Desktop/Block 3 MS/data/SPM_Eurosparen_Part1")
#setwd("C:/Users/sheil/Documents/data blok 3/SPM_Eurosparen_Part1")
## Load and clean the account creation table ##

acct.create <- read.csv2("AccountCreation.csv", header = TRUE)

acct.create$modified <-
  anytime(as.factor(acct.create$modified)) # change to date time from factor

acct.create$created <-
  anytime(as.factor(acct.create$created)) # change to date time from factor

acct.create.sample <-
  acct.create[acct.create$modified > "2018-11-01", ] # limit the number of obs to only recently modified data

acct.create.sample$id <-
  acct.create.sample$ï..id # cleaning up some variable names

acct.create.sample$ï..id <- NULL # remove the bad variable name

## Load and clean the account demogeo table ##

acct.demogeo <-
  read.csv2("AccountDemoGeo.csv") # read demographic data

acct.demogeo$dateOfBirth <-
  anytime(as.factor(acct.demogeo$dateOfBirth)) # change to date time from factor

acct.create.sample <-
  merge(acct.create.sample,
        acct.demogeo,
        by.x = "id",
        by.y = "accountID") # combine demographics with account create data

post.gender <-
  table(PostalCode = acct.demogeo$postalCode, Sex = acct.demogeo$gender) # information of gender counts by postal code
head(post.gender)
## Load and clean the account balance table ##

acct.cashback <- read.csv2("Cashbacks.csv", header = TRUE)

acct.cashback$amount <- NULL # remove the bad variable name
acct.cashback$ï..ROW_ID <- NULL # remove the bad variable name
acct.cashback$RUN_ID <- NULL # remove the bad variable name
acct.cashback$IsActual <- NULL # remove the bad variable name

acct.cashback$created <-
  anytime(as.factor(acct.cashback$created)) # change to date time from factor
acct.cashback$modified <-
  anytime(as.factor(acct.cashback$modified)) # change to date time from factor

acctid.origin <-
  table(acct.cashback$accountId, acct.cashback$origin) # summarizes how each account id submitted the cashback via app or website
sku.origin <-
  table(acct.cashback$sku, acct.cashback$origin) # summarizes product registered under cashback as app or website count

## Load and clean Code usage for 2018YTD ##

code.2018 <- read.csv2("CodeUsage_2018YTD.csv") 
code.2018$id <- code.2018$ï..id
code.2018$accountid <- code.2018$person_id
code.2018$status_code <- NULL # status_code is only 0, no value there, so it was removed
code.2018$person_id <- NULL
code.2018$ï..id <- NULL
code.2018$ip_addr <- NULL
code.2018$seq_nr <- NULL
code.2018$is_reserved <- NULL
code.2018$batch_id <- NULL
code.2018$units_id <- NULL
code.2018$portal_id <- NULL


