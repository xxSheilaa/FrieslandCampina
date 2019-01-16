# Data summary 

#rm(list = ls())

library(anytime)
library(recommenderlab)

setwd("C:/Users/kinse/Desktop/Block 3 MS/data/SPM_Eurosparen_Part1")
#setwd("C:/Users/sheil/Documents/data blok 3/SPM_Eurosparen_Part1")


#******************************************************************#
#************************ Account Creation ************************#
#******************************************************************#

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

#*****************************************************************#
#************************ Account Balance ************************#
#*****************************************************************#

acct.bal <- read.csv2("AccountBalance.csv")

acct.bal$id <- acct.bal$ï..id # change name
acct.bal$ï..id <- NULL # remove old names

acct.bal$modified <- anytime(acct.bal$modified) # change from factor to datetime variable
acct.bal$created <- anytime(acct.bal$created) # change from factor to datetime variable

acct.bal.sample <- acct.bal[acct.bal$accountId %in% acct.create.sample$id,] # trim to the sampled data

acct.not <- acct.create.sample[!(acct.create.sample$id %in% acct.bal.sample$accountId),] # check the accounts that did not show up in the balance sample

# confirming that the ids which are not in account balance are not in code usage table. 
# This means that if an account does not input a code then that account will not show up in the account balance table.

code.2018[ code.2018$accountId  %in% acct.not$id,] # should be empty

#*****************************************************************#
#************************ Account DemoGeo ************************#
#*****************************************************************#

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


#***********************************************************#
#************************ Cashbacks ************************#
#***********************************************************#

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

#************************************************************#
#************************ CODE USAGE ************************#
#************************************************************#

## Load and clean Code usage for 2018YTD ##

code.2018 <- read.csv2("CodeUsage_2018YTD.csv") # load in CodeUsage data

code.2018$id <- code.2018$ï..id # remove the bad variable name by copying data into corrected variable name
code.2018$accountid <- code.2018$person_id # rename the person_id to account_id
code.2018$status_code <- NULL # status_code is only 0, no value there, so it was removed
code.2018$person_id <- NULL # remove the person_id variable since it now is account_id

code.2018$ï..id <- NULL # remove bad variable
code.2018$ip_addr <- NULL # remove bad variable
code.2018$seq_nr <- NULL # remove bad variable
code.2018$is_reserved <- NULL # remove bad variable
code.2018$batch_id <- NULL # remove bad variable
code.2018$units_id <- NULL # remove bad variable
code.2018$portal_id <- NULL # remove bad variable
code.2018$RUN_ID <- NULL # remove bad variable
code.2018$IsActual <- NULL # remove bad variable

code.2018$crton <- anytime(code.2018$crton) # convert to date time


#************************************************************#
#************************ Part 2 ****************************#
#************************************************************#


#################
##shoppingtable##
#################

###Shopping orders

setwd("C:/Users/kinse/Desktop/Block 3 MS/data/SPM_Eurosparen_Part2")

shop.orders<- read.csv2("ShopOrders.csv", header = TRUE)

shop.orders$modified <-
  anytime(as.factor(shop.orders$modified)) # change to date time from factor

shop.orders$created <-
  anytime(as.factor(shop.orders$created)) # change to date time from factor

shop.orders.sample <-
  shop.orders[shop.orders$modified > "2018-11-01", ] # limit the number of obs to only recently modified data

shop.orders.sample$confirmEmail <- NULL #only null values

shop.orders.sample$id <-
  shop.orders.sample$X...id # cleaning up some variable names

shop.orders.sample$X...id <- NULL # remove the bad variable name
# shop.orders.sample$IsActual <- NULL # number of one 1762 number of two 62909

rm(shop.orders)

##Shop order products
shop.order.product<- read.csv2("ShopOrderProducts.csv", header = TRUE)

shop.order.product$modified <-
  anytime(as.factor(shop.order.product$modified)) # change to date time from factor

shop.order.product$created <-
  anytime(as.factor(shop.order.product$created)) # change to date time from factor

shop.order.product.sample <-
  shop.order.product[shop.order.product$modified > "2018-11-01", ] # limit the number of obs to only recently modified data

shop.order.product.sample$id <-
  shop.order.product.sample$X...id # cleaning up some variable names

shop.order.product.sample$X...id <- NULL # remove the bad variable name

shop.order.product.sample$batchId <- NULL  
shop.order.product.sample$variantId <- NULL
shop.order.product.sample$merchantId <- NULL

rm(shop.order.product)

##Shop info products
shop.product.info <- read.csv2("ShopProductInfo.csv", header = TRUE)

shop.product.info$modified <-
  anytime(as.factor(shop.product.info$modified)) # change to date time from factor

shop.product.info$created <-
  anytime(as.factor(shop.product.info$created)) # change to date time from factor

shop.product.info.sample <-
  shop.product.info[shop.product.info$modified > "2018-11-01", ] # limit the number of obs to only recently modified data

shop.product.info.sample$id <-
  shop.product.info.sample$X...id # cleaning up some variable names

shop.product.info.sample$X...id <- NULL # remove the bad variable name


shop.product.info.sample$shopIdList <- NULL  
shop.product.info.sample$labelIdList <- NULL
shop.product.info.sample$voucherTemplateUrlId <- NULL
shop.product.info.sample$tempSoldOutDate <- NULL
shop.product.info.sample$type <- NULL

rm(shop.product.info)

##Shop brand info
shop.brand.info <- read.csv2("ShopBrandInfo.csv", header = TRUE)

shop.brand.info$modified <-
  anytime(as.factor(shop.brand.info$modified)) # change to date time from factor

shop.brand.info$created <-
  anytime(as.factor(shop.brand.info$created)) # change to date time from factor


###we would have only 10 observations lol
# shop.brand.info.sample <-
#   shop.brand.info[shop.brand.info$modified > "2018-11-01", ] # limit the number of obs to only recently modified data

shop.brand.info$id <-
  shop.brand.info$X...id # cleaning up some variable names

shop.brand.info$X...id <- NULL # remove the bad variable name
shop.brand.info$description <- NULL  

##ShopCategoryInfo
shop.category.info <- read.csv2("ShopCategoryInfo.csv", header = TRUE)

shop.category.info$modified <-
  anytime(as.factor(shop.info.product$modified)) # change to date time from factor

shop.category.info$created <-
  anytime(as.factor(shop.info.product$created)) # change to date time from factor

shop.category.info.sample <-
  shop.category.info[shop.category.info$modified > "2018-11-01", ] # limit the number of obs to only recently modified data

shop.category.info.sample$id <-
  shop.category.info.sample$X...id # cleaning up some variable names

shop.category.info.sample$X...id <- NULL # remove the bad variable name

shop.category.info.sample$description <- NULL  
shop.category.info.sample$shopIdList <- NULL
shop.category.info.sample$rank <- NULL #NAs
shop.category.info.sample$enabled <- NULL #NAs
shop.category.info.sample$RUN_ID <- NULL #NAs
rm(shop.category.info)


########
##PSAM##
########

###PSAM
#psam first
psam.first<- read.csv2("PSAM_2018H2TD.csv", header = TRUE)
psam.first$modified <- NULL #only null values
psam.first$id <-
  psam.first$X...id # cleaning up some variable names
psam.first$X...id <- NULL # remove the bad variable name

#psam second 
psam.second<- read.csv2("PSAM_2018H2TD.csv", header = TRUE)

psam.second$modified <-
  anytime(as.factor(psam.second$modified)) # change to date time from factor

psam.second$created <-
  anytime(as.factor(psam.second$created)) # change to date time from factor

psam.second.sample <-
  acct.balance[psam.second$modified > "2018-11-01", ] # limit the number of obs to only recently modified data

psam.second.sample$id <-
  psam.second.sample$X....id # cleaning up some variable names

psam.second.sample$X...id <- NULL # remove the bad variable name


##########
###UPPC###
##########

#uppc sku
UPPC.sku<- read.csv2("UPPC_SKU.csv", header = TRUE)
UPPC.sku$id <-
  UPPC.sku$X....id # cleaning up some variable names

UPPC.sku$X...id <- NULL # remove the bad variable name

#uppc brands (only 19)
UPPC.brands<- read.csv2("UPPC_Brands.csv", header = TRUE)
UPPC.brands$id <-
  UPPC.brands$X...id # cleaning up some variable names

UPPC.brands$X...id <- NULL # remove the bad variable name
UPPC.brands$IsActual <- NULL 

#uppc prodgroups
UPPC.prodgroups <- read.csv2("UPPC_ProdGroups.csv", header = TRUE)
UPPC.prodgroups$id <-
  UPPC.prodgroups$X...id # cleaning up some variable names

UPPC.prodgroups$X...id <- NULL # remove the bad variable name

#uppc categories
UPPC.categories <- read.csv2("UPPC_categories.csv")

UPPC.categories$id <-
  UPPC.categories$X...id # cleaning up some variable names

UPPC.categories$X...id <- NULL # remove the bad variable name



