setwd("~/Dropbox/bid-data-20181015")
accounts = read.csv("accounts.csv", stringsAsFactors = FALSE, header=T)
ID = accounts$id
SIC = accounts$sign_in_count
LSI = accounts$last_sign_in_at
LSIP = accounts$last_sign_in_ip
plot(SIC, LSIP)
