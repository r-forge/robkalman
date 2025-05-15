
### counting::::

#%----------------------------------------------------------------------------
# DarlehensCommitments.neu
#%----------------------------------------------------------------------------

nrow(DarlehensCommitments.neu)
## completecases
with(DarlehensCommitments.neu, idX <<- !is.na(Effective.date)&!is.na(Sector)&!is.na(COUNTRY)&!is.na(Currency)&!is.na(Laufzeit)&!is.na(Default.Spread))
sum(idX)

wL  <- unlist(sapply(DarlehensCommitments.neu[,"COUNTRY"],function(x)which(x==LaenderTabelle[,"BlbDarlCom"])))
wL.i <- unlist(sapply(DarlehensCommitments.neu[idX,"COUNTRY"],function(x)which(x==LaenderTabelle[,"BlbDarlCom"])))

## Zahl der Fälle
sum(LaenderTabelle[wL,"EmMarket"]==3, na.rm=T)
sum(LaenderTabelle[wL.i,"EmMarket"]==3, na.rm=T)


## Zahl der Laender

sum(LaenderTabelle[unique(wL),"EmMarket"]==3, na.rm=T)
sum(LaenderTabelle[unique(wL.i),"EmMarket"]==3, na.rm=T)

sum(LaenderTabelle[unique(wL),"EmMarket"]<=3, na.rm=T)
sum(LaenderTabelle[unique(wL.i),"EmMarket"]<=3, na.rm=T)

#%----------------------------------------------------------------------------
# DarlehensCommitments.neu
#%----------------------------------------------------------------------------

nrow(DarlehensCommitments.neu)
## completecases
with(DarlehensCommitments.neu, idX <<- !is.na(Effective.date)&!is.na(Sector)&!is.na(COUNTRY)&!is.na(Currency)&!is.na(Laufzeit)&!is.na(Default.Spread))
sum(idX)

wL  <- unlist(sapply(DarlehensCommitments.neu[,"COUNTRY"],function(x)which(x==LaenderTabelle[,"BlbDarlCom"])))
wL.i <- unlist(sapply(DarlehensCommitments.neu[idX,"COUNTRY"],function(x)which(x==LaenderTabelle[,"BlbDarlCom"])))

## Zahl der Fälle
sum(LaenderTabelle[wL,"EmMarket"]==3, na.rm=T)
sum(LaenderTabelle[wL.i,"EmMarket"]==3, na.rm=T)


## Zahl der Laender

sum(LaenderTabelle[unique(wL),"EmMarket"]==3, na.rm=T)
sum(LaenderTabelle[unique(wL.i),"EmMarket"]==3, na.rm=T)

sum(LaenderTabelle[unique(wL),"EmMarket"]<=3, na.rm=T)
sum(LaenderTabelle[unique(wL.i),"EmMarket"]<=3, na.rm=T)


#%----------------------------------------------------------------------------
# Issues
#%----------------------------------------------------------------------------

##
nrow(Issues)

## completecases
with(Issues, idX1 <<- (!is.na(Announcement.date)&!is.na(Sector)&!is.na(Country)&!is.na(Curncy)&!is.na(Anlagefrist)&!is.na(Issuespread.o.Swp.)))
sum(idX1)

wL  <- unlist(sapply(Issues[,"Country"],function(x)which(x==LaenderTabelle[,"BlbDarlCom"])))
wL.i <- unlist(sapply(Issues[idX1,"Country"],function(x)which(x==LaenderTabelle[,"BlbDarlCom"])))

## Zahl der Fälle
sum(LaenderTabelle[wL,"EmMarket"]==3, na.rm=T)
sum(LaenderTabelle[wL.i,"EmMarket"]==3, na.rm=T)


## Zahl der Laender

sum(LaenderTabelle[unique(wL),"EmMarket"]==3, na.rm=T)
sum(LaenderTabelle[unique(wL.i),"EmMarket"]==3, na.rm=T)

sum(LaenderTabelle[unique(wL),"EmMarket"]<=3, na.rm=T)
sum(LaenderTabelle[unique(wL.i),"EmMarket"]<=3, na.rm=T)
