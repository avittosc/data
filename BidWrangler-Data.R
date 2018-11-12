# BidWrangler bid data analysis

library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("lubridate")
library(lubridate)
install.packages("tidyr")
library(tidyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("data.table")
library(data.table)
install.packages("data.table")
library(data.table)



# accounts 
# CLEAN 
accounts = read_csv("Updated_Data_CSVs/accounts.csv", quote = "\"")
accounts = accounts[-grep('bidwrangler',accounts$email),]
accounts = accounts[-grep('cottonwood',accounts$email),]
  IDaccounts = as.factor(na.omit(accounts$id))
    is.factor(IDaccounts)
    levels(IDaccounts)
  SICaccounts = na.omit(accounts$sign_in_count)
    is.numeric(SICaccounts)          
  LSIaccounts = na.omit(accounts$last_sign_in_at)
    LSIaccounts = as.POSIXlt(LSIaccounts, tz = "", format = "%Y-%m-%d %H:%M:%OS")
  LSIPaccounts = as.factor(na.omit(accounts$last_sign_in_ip))
    LSIPaccounts                        # lots of 127.0.0.1, good or bad ???       
    View(accounts)
# Detecting Outliers for accounts 
summary(SICaccounts)
boxplot.stats(SICaccounts)$out          #lots of outliers, this is fine
summary(LSIaccounts)                    



# auctions 
# CLEAN 
auctions = read_delim("Updated_Data_CSVs/auctions.csv", ",", quote = "\"", escape_double = TRUE)
auctions = auctions[-grep('2',auctions$company_id),]
View(auctions)
  IDauctions = as.factor(na.omit(auctions$id))
    is.factor(IDauctions)
    IDauctions
  CIDauctions = as.factor(na.omit(auctions$company_id))
    is.factor(CIDauctions)
    IDauctions
  ZIPauctions = na.omit(auctions$l.zip)
    ZIPauctions = na_if(ZIPauctions, '-')
    ZIPauctions = na.omit(ZIPauctions)
    is(ZIPauctions)
    ZIPauctions
  TZauctions = as.factor(na.omit(auctions$timezone))
    levels(TZauctions)
  SAauctions = na.omit(auctions$starts_at)
  SAauctions = as.POSIXlt(SAauctions, tz="", format = "%Y-%m-%d %H:%M:%OS")
  SAauctions
  
  auctions$scheduled_end_time = as.POSIXlt(na.omit(auctions$scheduled_end_time), tz="", format = "%Y-%m-%d %H:%M:%OS")
  auctions$scheduled_end_time = na_if(auctions$scheduled_end_time, '1969-12-31 19:00:00 EST')
  auctions$scheduled_end_time = na_if(auctions$scheduled_end_time, '1970-01-01 00:00:00 UTC')
  auctions$scheduled_end_time = na.omit(auctions$scheduled_end_time)
  is.na(auctions$scheduled_end_time)
  SEDauctions = auctions$scheduled_end_time[!is.na(auctions$scheduled_end_time)]
  SEDauctions
  
  ICauctions = na.omit(auctions$items_count)
    is.numeric(ICauctions)
    summary(ICauctions)
    boxplot.stats(ICauctions)$out
  OOauctions = na.omit(auctions$online_only)
    is.numeric(OOauctions)
    summary(OOauctions)               
    # these are all 0 or 1
  AtLauctions = na.omit(auctions$advance_to_live)
    is.numeric(AtLauctions)
    summary(AtLauctions)
    # these are all 0 or 1
  Brauctions = na.omit(auctions$broadcast)
    is.numeric(Brauctions)
    summary(Brauctions)
    # these are all 0 or 1
  CSLauctions = na.omit(auctions$closing_speed_lots)
    is.numeric(CSLauctions)
    summary(CSLauctions)
    boxplot.stats(CSLauctions)$out
  CSMauctions = na.omit(auctions$closing_speed_minutes)
    is.numeric(CSMauctions)
    summary(CSMauctions)
    boxplot.stats(CSMauctions)$out 

# users
# CLEAN
users = read_csv("Updated_Data_CSVs/users.csv")
  IDusers = as.factor(na.omit(users$id))
    is.factor(IDusers)
    levels(IDusers)
  AIDusers = as.factor(na.omit(users$account_id))
    is.factor(AIDusers)
    levels(AIDusers)
  DRSusers = na.omit(users$default_registration_status)
    DRSusers = as.factor(DRSusers)
    is.factor(DRSusers)
    levels(DRSusers)
  PCIDusers = as.factor(na.omit(users$payment_customer_id))
    as.factor(PCIDusers)
    levels(PCIDusers)
  ZIPusers = na.omit(users$l.zip)
    is(ZIPusers)

    
# items
# CLEAN
install.packages("data.table", type = "source", repos = getOption("http://Rdatatable.github.io/data.table"))
items = fread("~/Desktop/Updated_Data_CSVs/items.csv", header=TRUE, sep=",")
items = items[-grep('2',items$company_id),]
  IDitems = na.omit(items$id)
    is.numeric(IDitems)
  CIDitems = as.factor(na.omit(items$company_id))
    is.factor(CIDitems)
    levels(CIDitems)
  AIDitems = as.factor(na.omit(items$auction_id))
    is.factor(AIDitems)
    levels(AIDitems)
  SEQitems = na.omit(items$sequence)
    is.numeric(SEQitems)
    SEQitems
    summary(SEQitems)
  LIDitems = na.omit(items$lot_identifier)
    is.numeric(LIDitems)
  STitems = as.factor(na.omit(items$status))
    is.factor(STitems)
    levels(STitems)
  CBIDitems = na.omit(items$closing_bid_id)
  SAitems = na.omit(items$start_amount)
    is.numeric(SAitems)
  MAitems = na.omit(items$minimum_amount)
  AEIitems = na.omit(items$autoextend_increment)
    is.numeric(AEIitems)
  AETitems = as.POSIXlt(na.omit(items$actual_end_time), tz="", format = "%Y-%m-%d %H:%M:%OS")
    AETitems
    is(AETitems)
  ABCitems = na.omit(items$accepted_bid_count)
    is.numeric(ABCitems)
  MCitems = na.omit(items$manual_close)
    is.numeric(MCitems)
    summary(MCitems)
  AEitems = na.omit(items$autoextensions)
    is.numeric(AEitems)
    summary(AEitems)
    

# Detecting Outliers
boxplot.stats(AEitems)$out      # some outliers seem fake
summary(AEitems)            # this is werid.....
summary(SAitems)            # has non-numerical values; why
summary(MAitems)
boxplot.stats(MAitems)$out  # a LOT of outliers
summary(AEIitems)
boxplot.stats(AEIitems)$out # also a LOT of outliers
summary(ABCitems)
boxplot.stats(ABCitems)$out # a LOT of outliers
summary(MCitems)
boxplot.stats(MCitems)$out
summary(AEitems)


# companies 
# CLEAN
companies = read_csv("Updated_Data_CSVs/companies.csv")
companies = companies[-grep('2', companies$id),]
  IDcompanies = na.omit(companies$id)
    IDcompanies = as.factor(IDcompanies)
    is.factor(IDcompanies)
    levels(IDcompanies)
  ZIPcompanies = na.omit(companies$l.zip)
    is(ZIPcompanies)


# bids 
# CLEAN
bids = read_csv("Updated_Data_CSVs/bids.csv")
# this is what I used to remove all rows that != "auto", "live", "manual" for bid_type 
bids = bids[-grep('remote',bids$bid_type),]
bids = bids[-grep('ask', bids$bid_type),]
bids = bids[-grep('maxbid',bids$bid_type),] 
bids = bids[-grep('buyitnow',bids$bid_type),]
bids = bids[-grep('offer',bids$bid_type),]
bids = bids[-grep('2',bids$company_id),]
# note
  # live = bid placed on behalf of a person who is physically present (but it is not identifiable to a specific person)
  # auto = came from a maxbid
  # manual = came from a remote bidder place a specific bid (i.e. clicking the “BID $1,000” button in the app/website)
  IDbids = na.omit(bids$id)
    IDbids = as.factor(IDbids)
    levels(IDbids)
  CIDbids = na.omit(bids$company_id)
    CIDbids = as.factor(CIDbids)
    levels(CIDbids)
  AIDbids = na.omit(bids$auction_id)
    AIDbids = as.factor(AIDbids)
    levels(AIDbids)
  IIDbids = na.omit(bids$item_id)
    IIDbids = as.factor(IIDbids)
    levels(IIDbids)
  UIDbids = na.omit(bids$user_id)
    UIDbids = as.factor(UIDbids)
    levels(UIDbids)
  Abids = na.omit(bids$amount)
    is.numeric(Abids)
    summary(Abids)
    boxplot.stats(Abids)$out
  STbids = factor(na.omit(bids$status))
    is.factor(STbids)
  BTbids = factor(na.omit(bids$bid_type))
    is.factor(BTbids)
    levels(BTbids)
    PAbids = na.omit(bids$placed_at)
    PAbids = as.POSIXlt(PAbids, tz="", format = "%Y-%m-%d %H:%M:%OS")
  
  
  

