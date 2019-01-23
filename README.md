# Data summary 

rm(list = ls())

library(anytime) 

library(recommenderlab)

library(data.table)

setwd("C:/Users/kinse/Desktop/Block 3 MS/data/SPM_Eurosparen_Part1")

#setwd("C:/Users/sheil/Documents/data blok 3/SPM_Eurosparen_Part1")

# Part 1

## Account Creation

### Load and clean the account creation table ###

acct.create <- fread("AccountCreation.csv", header = TRUE, sep = ';')

acct.create$modified <-
  anytime(as.character(acct.create$modified)) # change to date time from factor

acct.create.sample <-
  acct.create[acct.create$modified > "2018-11-01", ] # limit the number of obs to only recently modified data

acct.create.sample$created <-
  anytime(as.character(acct.create.sample$created)) # change to date time from factor

rm(acct.create)

# Account Balance 

acct.bal <- fread("AccountBalance.csv", header = TRUE, sep = ';')


acct.bal$SavingsAccount <- acct.bal$id # change name
acct.bal$id <- NULL # remove old names

acct.bal$modified <- anytime(as.character(acct.bal$modified)) # change from factor to datetime variable

acct.bal.sample <- acct.bal[which(acct.bal$accountId %in% acct.create.sample$id),] # create sample

acct.bal.sample$created <- anytime(as.character(acct.bal.sample$created)) # change from factor to datetime variable

rm(acct.bal)

# Account DemoGeo

## Load and clean the account demogeo table ##

acct.demogeo <-
  fread("AccountDemoGeo.csv", header = T, sep = ';') # read demographic data

acct.demogeo.sample <- acct.demogeo[which(acct.demogeo$accountID %in% acct.create.sample$id),] # create sample

acct.demogeo.sample$dateOfBirth <-
  anytime(as.character(acct.demogeo.sample$dateOfBirth)) # change to date time from factor

acct.create.sample <-
  merge(acct.create.sample,
        acct.demogeo.sample,
        by.x = "id",
        by.y = "accountID") # combine demographics with account create data

post.gender <-
  table(PostalCode = acct.demogeo$postalCode, Sex = acct.demogeo$gender) # information of gender counts by postal code
head(post.gender)

rm(acct.demogeo)

# Cashbacks 

## Load and clean the account balance table ##

acct.cashback <- fread("Cashbacks.csv", header = TRUE, sep = ';')

acct.cashback$amount <- NULL # remove the bad variable name
acct.cashback$ROW_ID <- NULL # remove the bad variable name
acct.cashback$RUN_ID <- NULL # remove the bad variable name
acct.cashback$IsActual <- NULL # remove the bad variable name

acct.cashback.sample <- acct.cashback[which(acct.cashback$accountId %in% acct.create.sample$id),]

acct.cashback.sample$created <-
  anytime(as.character(acct.cashback.sample$created)) # change to date time from factor
acct.cashback.sample$modified <-
  anytime(as.character(acct.cashback.sample$modified)) # change to date time from factor

rm(acct.cashback)

acctid.origin <-
  table(acct.cashback.sample$accountId, acct.cashback.sample$origin) # summarizes how each account id submitted the cashback via app or website
sku.origin <-
  table(acct.cashback.sample$sku, acct.cashback.sample$origin) # summarizes product registered under cashback as app or website count

# CODE USAGE

## Load and clean Code usage for 2018YTD ##

code.2018 <-
  fread("CodeUsage_2018YTD.csv", header = T, sep = ';') # load in CodeUsage data

code.2018$accountid <-
  code.2018$person_id # rename the person_id to account_id

code.2018$status_code <-
  NULL # status_code is only 0, no value there, so it was removed

code.2018$person_id <-
  NULL # remove the person_id variable since it now is account_id

code.2018$id <- NULL # remove bad variable
code.2018$ip_addr <- NULL # remove bad variable
code.2018$seq_nr <- NULL # remove bad variable
code.2018$is_reserved <- NULL # remove bad variable
code.2018$batch_id <- NULL # remove bad variable
code.2018$units_id <- NULL # remove bad variable
code.2018$portal_id <- NULL # remove bad variable
code.2018$RUN_ID <- NULL # remove bad variable
code.2018$IsActual <- NULL # remove bad variable

code.2018.sample <-
  code.2018[which(code.2018$accountid %in% acct.create.sample$id),] # create the sample set

code.2018.sample$crton <-
  anytime(code.2018.sample$crton) # convert to date time

rm(code.2018)

# Part 2 

setwd("C:/Users/kinse/Desktop/Block 3 MS/data/SPM_Eurosparen_Part2")

### Shop orders

shop.orders <- fread("ShopOrders.csv", header = TRUE, sep = ';')

shop.orders.sample <-
  shop.orders[which(shop.orders$accountId %in% acct.create.sample$id),]

shop.orders.sample$orderId <- shop.orders.sample$id # change name

shop.orders.sample$modified <-
  anytime(as.character(shop.orders.sample$modified)) # change to date time from factor

shop.orders.sample$created <-
  anytime(as.character(shop.orders.sample$created)) # change to date time from factor

shop.orders.sample$id <- NULL # remove changed name
shop.orders.sample$confirmEmail <- NULL #only null values
shop.orders.sample$IsActual <- NULL # number of one 1762 number of two 62909
shop.orders.sample$RUN_ID <- NULL # remove RUN_ID

rm(shop.orders)

## Shop order products

shop.order.product <- fread("ShopOrderProducts.csv", header = TRUE, sep = ';')

shop.order.product.sample <-
  shop.order.product[which(shop.order.product$orderId %in% shop.orders.sample$id),] # create the sample set

rm(shop.order.product) # remove the big table from environment

shop.order.product.sample$modified <-
  anytime(as.character(shop.order.product.sample$modified)) # change to date time from factor

shop.order.product.sample$created <-
  anytime(as.character(shop.order.product.sample$created)) # change to date time from factor

shop.order.product.sample$id <- NULL # remove the bad variable name
shop.order.product.sample$batchId <- NULL  
shop.order.product.sample$variantId <- NULL
shop.order.product.sample$merchantId <- NULL
shop.order.product.sample$RUN_ID <- NULL
shop.order.product.sample$IsActual <- NULL


## Shop info products
shop.product.info <- fread("ShopProductInfo.csv", header = TRUE, sep = ';')

shop.product.info$productId <- shop.product.info$id # change name to agreed upon naming convention

shop.product.info$modified <-
  anytime(as.character(shop.product.info$modified)) # change to date time from factor

shop.product.info$created <- NULL

shop.product.info$created <- 
  shop.product.info$artcode

shop.product.info$id <- NULL
shop.product.info$artcode <- NULL
shop.product.info$shopIdList <- NULL  
shop.product.info$labelIdList <- NULL
shop.product.info$voucherTemplateUrlId <- NULL
shop.product.info$tempSoldOutDate <- NULL
shop.product.info$type <- NULL
shop.product.info$RUN_ID <- NULL
shop.product.info$IsActual <- NULL

## Shop brand info

shop.brand.info <- fread("ShopBrandInfo.csv", header = TRUE, sep = ';')

shop.brand.info$brandId <- shop.brand.info$id # change name to agreed upon naming convention

shop.brand.info$modified <-
  anytime(as.character(shop.brand.info$modified)) # change to date time from factor

shop.brand.info$online <- 
  anytime(as.character(shop.brand.info$online)) # change to date time 

shop.brand.info$offline <- 
  anytime(as.character(shop.brand.info$offline)) # change to date time 

shop.brand.info$id <- NULL 
shop.brand.info$created <- NULL #variable is empty
shop.brand.info$description <- NULL # variable was only NULL
shop.brand.info$RUN_ID <- NULL # removing useless variables
shop.brand.info$IsActual <- NULL # removing useless variable


## ShopCategoryInfo

shop.category.info <- fread("ShopCategoryInfo.csv", header = TRUE, sep = ';')

shop.category.info$categoryId <- shop.category.info$id # change name to agreed upon naming convention

shop.category.info$modified <-
  anytime(as.character(shop.category.info$modified)) # change to date time from factor

shop.category.info$online <-
  anytime(as.character(shop.category.info$online)) # change to date time from factor

shop.category.info$offline <-
  anytime(as.character(shop.category.info$offline)) # change to date time from factor

shop.category.info$id <- NULL
shop.category.info$description <- NULL  
shop.category.info$shopIdList <- NULL
shop.category.info$rank <- NULL #NAs
shop.category.info$enabled <- NULL #NAs
shop.category.info$RUN_ID <- NULL #NAs
shop.category.info$IsActual <- NULL
shop.category.info$created <- NULL
shop.category.info$image <- NULL
shop.category.info$parentId <- NULL
shop.category.info$headerImage <- NULL


## PSAM ##

### PSAM First
psam.first <- fread("PSAM_2018H1.csv", header = TRUE, sep = ';')

psam.first.sample <- psam.first[which(psam.first$savingsAccountId %in% acct.bal.sample$SavingsAccount),]

psam.first.sample$modified <- anytime(as.character(psam.first.sample$modified))
psam.first.sample$created <- anytime(as.character(psam.first.sample$created))

rm(psam.first)

### psam second 
psam.second<- fread("PSAM_2018H2TD.csv", header = TRUE, sep = ';')

psam.second.sample <- psam.second[which(psam.second$savingsAccountId %in% acct.bal.sample$SavingsAccount),]

psam.second.sample$modified <-
  anytime(as.character(psam.second.sample$modified)) # change to date time from factor

psam.second.sample$created <-
  anytime(as.character(psam.second.sample$created)) # change to date time from factor

rm(psam.second)

psam.2018.sample <- rbind(psam.first.sample,psam.second.sample) # combine 2018 into one table

rm(psam.first.sample,psam.second.sample)

### UPPC ###

#### uppc sku

UPPC.sku<- fread("UPPC_SKU.csv", header = TRUE, sep = ';')

UPPC.sku$sku_id <-
  UPPC.sku$id # cleaning up some variable names

UPPC.sku$id <- NULL # remove the bad variable name
UPPC.sku$RUN_ID <- NULL
UPPC.sku$IsActual <- NULL
UPPC.sku$remark <- NULL # empty variable

#### uppc brands (only 19)
UPPC.brands<- fread("UPPC_Brands.csv", header = TRUE, sep = ';')

UPPC.brands$brand_id <-
  UPPC.brands$id # changing name to the agreed upon naming convention

UPPC.brands$id <- NULL
UPPC.brands$IsActual <- NULL
UPPC.brands$RUN_ID <- NULL

#### uppc prodgroups
UPPC.prodgroups <- fread("UPPC_ProdGroups.csv", header = TRUE, sep = ';')

UPPC.prodgroups$prodgroup_id <-
  UPPC.prodgroups$id # changing name to the agreed upon naming convention

UPPC.prodgroups$id <- NULL # remove the bad variable name
UPPC.prodgroups$IsActual <- NULL
UPPC.prodgroups$RUN_ID <- NULL


#### uppc categories
UPPC.categories <- fread("UPPC_categories.csv", header = TRUE, sep = ';')

UPPC.categories$category_id <- UPPC.categories$id# cleaning up some variable names

UPPC.categories$id <- NULL # remove the bad variable name
UPPC.categories$RUN_ID <- NULL # remove the bad variable name
UPPC.categories$IsActual <- NULL # remove the bad variable name

### *** Recommender Lab begin *** ###
