# BidWrangler Bid Data Analysis



# Packages Used
  install.packages("dplyr")
  install.packages("lubridate")  
  install.packages("tidyr")
  install.packages("tidyverse")
  install.packages("data.table")
  install.packages("naniar")
  library(readr)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(tidyverse)
  library(data.table)
  library(naniar)


# Data Cleaning 
  # accounts 
    # CLEAN 
      accounts = read_csv("Updated_Data_CSVs/accounts.csv", quote = "\"")
      accounts = accounts[-grep('bidwrangler',accounts$email),]
      accounts = accounts[-grep('cottonwood',accounts$email),]
        IDaccounts = as.numeric(na.omit(accounts$id))
          is.numeric(IDaccounts)
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
      IDauctions = as.numeric(na.omit(auctions$id))
        is.numeric(IDauctions)
        IDauctions
      CIDauctions = as.numeric(na.omit(auctions$company_id))
        is.numeric(CIDauctions)
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
        IDusers = as.numeric(na.omit(users$id))
          is.numeric(IDusers)
        AIDusers = as.numeric(na.omit(users$account_id))
          is.numeric(AIDusers)
        DRSusers = na.omit(users$default_registration_status)
          DRSusers = as.factor(DRSusers)
          is.factor(DRSusers)
          levels(DRSusers)
        PCIDusers = na.omit(as.numeric(users$payment_customer_id))
          is.numeric(PCIDusers)
        ZIPusers = na.omit(users$l.zip)
          is(ZIPusers)

    
  # items
    # CLEAN
      install.packages("data.table", type = "source", repos = getOption("http://Rdatatable.github.io/data.table"))
        items = fread("~/Desktop/github/Updated_Data_CSVs/items.csv", header=TRUE, sep=",")
        items = items[-grep('2',items$company_id),]
          IDitems = na.omit(items$id)
            is.numeric(IDitems)
          CIDitems = as.numeric(na.omit(items$company_id))
            is.numeric(CIDitems)
          AIDitems = as.numeric(na.omit(items$auction_id))
            is.numeric(AIDitems)
          SEQitems = na.omit(items$sequence)
            is.numeric(SEQitems)
            SEQitems
            summary(SEQitems)
          LIDitems = na.omit(as.numeric(items$lot_identifier))
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
          AETitems = as.POSIXlt(na.omit(items$actual_end_time), tz = "", format = "%Y-%m-%d %H:%M:%OS")
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
          IDcompanies = as.numeric(IDcompanies)
          is.numeric(IDcompanies)
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
        IDbids = na.omit(bids$id)
          IDbids = as.numeric(IDbids)
          is.numeric(IDbids)
        CIDbids = na.omit(bids$company_id)
          CIDbids = as.numeric(CIDbids)
          is.numeric(CIDbids)
        AIDbids = na.omit(bids$auction_id)
          AIDbids = as.numeric(AIDbids)
          is.numeric(AIDbids)
        IIDbids = na.omit(bids$item_id)
          IIDbids = as.numeric(IIDbids)
          is.numeric(IIDbids)
        UIDbids = na.omit(bids$user_id)
          UIDbids = as.numeric(UIDbids)
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
          # note
            # live = bid placed on behalf of a person who is physically present (but it is not identifiable to a specific person)
            # auto = came from a maxbid
            # manual = came from a remote bidder place a specific bid (i.e. clicking the “BID $1,000” button in the app/website)
        PAbids = na.omit(bids$placed_at)
          PAbids = as.POSIXlt(PAbids, tz="", format = "%Y-%m-%d %H:%M:%OS")
        

    
    
    
    
# Data Analysis
  install.packages("car")
  library(car)
  
  # Separating Categorical Objects into Boolean Objects 
    TZauctions
    levels(TZauctions)
      GMTm10.auctions = (TZauctions == "GMT-10")
        GMTm10.auctions = ifelse(GMTm10.auctions == TRUE, 1, 0)
        GMTm10.auctions
      GMTm5.auctions = (TZauctions == "GMT-5") 
        GMTm5.auctions = ifelse(GMTm5.auctions == TRUE, 1, 0)
        GMTm5.auctions
      GMTm6.auctions = (TZauctions == "GMT-6")
        GMTm6.auctions = ifelse(GMTm6.auctions == TRUE, 1, 0)
        GMTm6.auctions
      GMTm7.auctions = (TZauctions == "GMT-7")
        GMTm7.auctions = ifelse(GMTm7.auctions == TRUE, 1, 0)
        GMTm7.auctions
      GMTm8.auctions = (TZauctions == "GMT-8") 
        GMTm8.auctions = ifelse(GMTm8.auctions == TRUE, 1, 0)
        GMTm8.auctions
      GMTp11.auctions = (TZauctions == "GMT+11") 
        GMTp11.auctions = ifelse(GMTp11.auctions == TRUE, 1, 0)
        GMTp11.auctions
      GMTp2.auctions = (TZauctions == "GMT+2") 
        GMTp2.auctions = ifelse(GMTp2.auctions == TRUE, 1, 0)
        GMTp2.auctions
      GMTp6.auctions = (TZauctions == "GMT+6") 
        GMTp6.auctions = ifelse(GMTp6.auctions == TRUE, 1, 0)
        GMTp6.auctions
      GMTp8.auctions = (TZauctions == "GMT+8") 
        GMTp8.auctions = ifelse(GMTp8.auctions == TRUE, 1, 0)
        GMTp8.auctions
      GMTp930.auctions = (TZauctions == "GMT+9:30")
        GMTp930.auctions = ifelse(GMTp930.auctions == TRUE, 1, 0) 
        GMTp930.auctions
                    
    DRSusers
     levels(DRSusers)
      DRSapproved.users = (DRSusers == "approved")
        DRSapproved.users = ifelse(DRSapproved.users == TRUE, 1, 0)
        DRSapproved.users
      DRSblocked.users = (DRSusers == "blocked")
        DRSblocked.users = ifelse(DRSblocked.users == TRUE, 1, 0)
        DRSblocked.users
      DRSpending.users = (DRSusers == "pending")
        DRSpending.users = ifelse(DRSpending.users == TRUE, 1, 0)
        DRSpending.users
    
    STitems
     levels(STitems)
      STacceptingbids.items = (STitems == "accepting_bids")
        STacceptingbids.items = ifelse(STacceptingbids.items == TRUE, 1, 0)  
        STacceptingbids.items
      STnosale.items = (STitems == "no_sale")
        STnosale.items = ifelse(STnosale.items == TRUE, 1, 0)
        STnosale.items
      STpaused.items = (STitems == "paused")
        STpaused.items = ifelse(STpaused.items == TRUE, 1, 0)    
        STpaused.items 
      STpending.items = (STitems == "pending")
        STpending.items = ifelse(STpending.items == TRUE, 1, 0)
        STpending.items
      STpendingclose.items = (STitems == "pending_close")
        STpendingclose.items = ifelse(STpendingclose.items == TRUE, 1, 0)
        STpendingclose.items
      STsold.items = (STitems == "sold")
        STsold.items = ifelse(STsold.items == TRUE, 1, 0)
        STsold.items
      
    STbids
      levels(STbids)
        STaccepted.bids = (STbids == "accepted")
          STaccepted.bids = ifelse(STaccepted.bids == TRUE, 1, 0)
          STaccepted.bids
        STacceptingbids.bids = (STbids == "accepting_bids")
          STacceptingbids.bids = ifelse(STacceptingbids.bids == TRUE, 1, 0)
          STacceptingbids.bids
        STcancelled.bids = (STbids == "cancelled")
          STcancelled.bids = ifelse(STcancelled.bids == TRUE, 1, 0)
          STcancelled.bids
        STdeleted.bids = (STbids == "deleted")
          STdeleted.bids = ifelse(STdeleted.bids == TRUE, 1, 0)
          STdeleted.bids
        STnosale.bids = (STbids == "no_sale")
          STnosale.bids = ifelse(STnosale.bids == TRUE, 1, 0)
          STnosale.bids
        SToutbid.bids = (STbids == "outbid")
          SToutbid.bids = ifelse(SToutbid.bids == TRUE, 1, 0)
          SToutbid.bids
        STpending.bids = (STbids == "pending")
          STpending.bids = ifelse(STpending.bids == TRUE, 1, 0)
          STpending.bids
        STrejected.bids = (STbids == "rejected")
          STrejected.bids = ifelse(STrejected.bids == TRUE, 1, 0)
          STrejected.bids
        STsold.bids = (STbids == "sold")
          STsold.bids = ifelse(STsold.bids == TRUE, 1, 0)
          STsold.bids
          
    BTbids
      levels(BTbids)
        BTauto.bids = (BTbids == "auto")
          BTauto.bids = ifelse(BTauto.bids == TRUE, 1, 0)
          BTauto.bids
        BTlive.bids = (BTbids == "live")
          BTlive.bids = ifelse(BTlive.bids == TRUE, 1, 0)
          BTlive.bids
        BTmanual.bids = (BTbids == "manual")
          BTmanual.bids = ifelse(BTmanual.bids == TRUE, 1, 0)
          BTmanual.bids
          
          

    
    Auction_Duration_Seconds = auctions$scheduled_end_time-auctions$starts_at
    Auction_Duration_Seconds
    
    auctions = cbind(auctions, Auction_Duration_Seconds)   
    Auction_Duration_Seconds[Auction_Duration_Seconds<0] <- NA
    View(auctions)
    auctiondurationsecs = as.numeric(na.omit(Auction_Duration_Seconds))        
    auctiondurationsecs      
    summary(auctiondurationsecs)
 
    
    
    
    
    
    
    
  # Saving cleaned dataframes to Desktop
    write.csv(auctions, file = "auctionsCLEAN.csv")
    write.csv(accounts, file = "accountsCLEAN.csv")
    write.csv(bids, file = "bidsCLEAN.csv")
    write.csv(companies, file = "companiesCLEAN.csv")
    write.csv(items, file = "itemsCLEAN.csv")
    write.csv(users, file = "usersCLEAN.csv")
    
    
    
    
    
    