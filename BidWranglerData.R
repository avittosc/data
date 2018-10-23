setwd("~/Dropbox/bid-data-20181015")
# ^^^^ don't think i need that 


library(readr)
accounts = read_csv("~/Downloads/accounts.csv")
IDaccounts = na.omit(accounts$`ƒ"id"`)
SICaccounts = na.omit(accounts$sign_in_count)
LSIaccounts = na.omit(accounts$last_sign_in_at)
LSIPaccounts = na.omit(accounts$last_sign_in_ip)

# Detecting Outliers
summary(SICaccounts)
boxplot.stats(SICaccounts)$out          #lots of outliers: look into that 


library(readr)
auctions = read_csv("~/Downloads/bid-data-20181015/auctions.csv")
IDauctions = na.omit(auctions$id)
ZIPauctions = na.omit(auctions$l.zip)
TZauctions = na.omit(auctions$timezone)
TZauctions = factor(TZauctions)
SAauctions = na.omit(auctions$starts_at)
SEDauctions = na.omit(auctions$scheduled_end_time)
ICauctions = na.omit(auctions$items_count)
OOauctions = na.omit(auctions$online_only)
AtLauctions = na.omit(auctions$advance_to_live)
Brauctions = na.omit(auctions$broadcast)
CSLauctions = na.omit(auctions$closing_speed_lots)
CSMauctions = na.omit(auctions$closing_speed_minutes)

# Detecting Outliers
summary(ICauctions)               # ICauctions has non-numerical values; these should be numerical 
summary(CSMauctions)
boxplot.stats(CSMauctions)$out    # see if 1000 should be removed 
summary(OOauctions)               # OOauctions has non-numerical values; these should be numerical 
summary(AtLauctions)
boxplot.stats(AtLauctions)$out    # isn't this supposed to be 0 or 1? 
summary(Brauctions)
boxplot.stats(Brauctions)$out     # why is 1 an outlier?   what does 200 and 252 mean?
summary(CSLauctions)
boxplot.stats(CSLauctions)$out


library(readr)
users = read_csv("~/Downloads/bid-data-20181015/users.csv")
IDusers = na.omit(users$id)
AIDusers = na.omit(users$account_id)
DRSusers = na.omit(users$default_registration_status)
DRSusers = factor(DRSusers)
PCIDusers = na.omit(users$payment_customer_id)
ZIPusers = na.omit(users$l.zip)


library(readr)
items = read_csv("~/Downloads/bid-data-20181015/items.csv")
IDitems = na.omit(items$id)
AIDitems = na.omit(items$auction_id)
SEQitems = na.omit(items$sequence)
LIDitems = na.omit(items$lot_identifier)
STitems = na.omit(items$status)
STitems = factor(STitems)
CBIDitems = na.omit(items$closing_bid_id)
SAitems = na.omit(items$start_amount)
MAitems = na.omit(items$minimum_amount)
AEIitems = na.omit(items$autoextend_increment)
AETitems = na.omit(items$actual_end_time)
ABCitems = na.omit(items$accepted_bid_count)
MCitems = na.omit(items$manual_close)
AEitems = na.omit(items$autoextensions)

# Detecting Outliers
boxplot.stats(AEitems)      # some outliers seem fake
mean(AEitems)               # the outliers seem to really affect the mean; look into getting rid of them (especially 999999)
AEitems = AEitems[AEitems!=999999]
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



library(readr)
companies = read_csv("~/Downloads/bid-data-20181015/companies.csv")
IDcompanies = na.omit(companies$id)
ZIPcompanies = na.omit(companies$l.zip)
