# Crunch FX Reconciliation

The purpose of this tool is to convert statements issued by my bank - Starling - into a statement that can be used in 
reconciliation activity in my online accounting provider, Crunch Accounting.

usage crunch ecb-rates-csv eur-starling-statement-csv gbp-starling-statement-csv

where
 - ecb-rates-csv is a csv containing ecb reference rates for eur-gbp (FIXME - add the link)
 - eur-starling-statement-csv is a csv-formatted starling statement (downloaded from Starling EUR account)
 - gbp-starling-statement-csv is a csv-formatted startling statement (downloaded from Starling GBP account)

Apparently, Crunch will be starting an api service at some unspecified point in the future.
Apparently, Crunch will also support reconcliliation for currencies other than GBP

## How it works

This is a script for a very special case.  I have two starling accounts that I transfer money between,
and Crunch prefers to account for EUR  by transforming the transactions into GBP using a nominal 
ECB rate.  The wrinkle comes when there is a transfer from my EUR account to my GBP account, as then the recrded
GBP quantity is likely to be different to the EUR quantity.  My GBP-EUR account transfers don't have this problem.

