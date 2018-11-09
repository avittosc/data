# BidWrangler bid data analysis
setwd("~/Dropbox/bid-data-20181015")
library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("lubridate")
library(lubridate)
install.packages("tidyr")
library(tidyr)
install.packages("tidyverse")
library(tidyverse)

# accounts 
accounts = read_csv("Desktop/accounts.csv", quote = "\"")
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


auctions = read_delim("Desktop/auctions.csv", ",", quote = "\"", escape_double = TRUE)
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
  TZauctions = as.factor(na.omit(auctions$timezone))
  TZauctions = na_if(TZauctions, 'KS [ONLINE]"')
  TZauctions = na_if(TZauctions, "Texas's Oldest Dance Hall")
  levels(TZauctions)
  SAauctions = na.omit(auctions$starts_at)
  SAauctions = as.POSIXlt(SAauctions, tz="", format = "%Y-%m-%d %H:%M:%OS")
  SAauctions
  
  SEDauctions = na.omit(as.POSIXlt(auctions$scheduled_end_time, tz="", format = "%Y-%m-%d %H:%M:%OS"))
  SEDauctions = na_if(SEDauctions, "1969-12-31 19:00:00 EST")
  SEDauctions = na_if(SEDauctions, "1970-01-01 00:00:00 UTC")
  SEDauctions = na.omit(SEDauctions)
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


users = read_csv("Desktop/users.csv")
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

items = scan(file = "Desktop/items.csv", what = double(), sep = "",
         quote = if(identical("", "\n")) "" else "'\"", dec = ".")

items = read_delim("Desktop/items.csv", delim = ',', quote = "", escape_double = TRUE)
items = read.delim("Desktop/items.csv", quote = "\"")
# items = read_csv("Desktop/items.csv")
items = items[-grep('2',items$company_id),]
View(items)
  IDitems = na.omit(items$id)
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
  STitems = factor(STitems)
  CBIDitems = na.omit(items$closing_bid_id)
  SAitems = na.omit(items$start_amount)
    is.numeric(SAitems)
  MAitems = na.omit(items$minimum_amount)
  AEIitems = na.omit(items$autoextend_increment)
  AETitems = na.omit(items$actual_end_time)
  AETitems
  ABCitems = na.omit(items$accepted_bid_count)
  MCitems = na.omit(items$manual_close)
  AEitems = na.omit(items$autoextensions)

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


companies = read_csv("Desktop/companies.csv")
companies = companies[-grep('2', companies$id),]
  IDcompanies = na.omit(companies$id)
    is.numeric(IDcompanies)
  ZIPcompanies = na.omit(companies$l.zip)
    is.numeric(ZIPcompanies)


bids = read_csv("Desktop/bids.csv")
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
    is.numeric(IDbids)
  CIDbids = na.omit(bids$company_id)
    is.numeric(CIDbids)
  AIDbids = na.omit(bids$auction_id)
    is.numeric(AIDbids)
  IIDbids = na.omit(bids$item_id)
    is.numeric(IIDbids)
  UIDbids = na.omit(bids$user_id)
    is.numeric(UIDbids)
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
  
  PAbids

  
# this isn't used but good to have   
# bids %>%
   # select(bid_type)    %>%
   # filter(bid_type == "auto", "live", "manual")
 



