# Podaci
library(ggplot2)
library(randomForest)
library(rfUtilities)

sve= read.csv("ESS10.csv")
sve.sc= read.csv("ESS10SC.csv") # SC: self-completion; zbog COVID-19; ne ulazi
bdp= read.csv("BDP.csv")
bdp= bdp[-(32:40), -(2:4)]
bdp= bdp[-c(21:23,24:29), ]
names(bdp)= c("country", "income")

sve= sve[, c("cntry", "stflife","anweight", "stflife", "health",
             "marsts", "domicil", "iplylfr", "rlgdgr",
             "pray", "rlgatnd", "wkhtot", "eisced", 
             "hinctnta", "hhmmb")]

cntry= as.factor(sve$cntry)
health= sve$health
health[health> 5]= NA
health= as.factor(health) # da ili ne? ja bi rekel da; konzultacije
stflife= as.integer(sve$stflife)
stflife[stflife>10]= NA
stflife= cut(stflife, 
             breaks= c(0, 3, 5, 7, 9, 10),
             right= F)
# a možda može i 4 kategorije; našel sam opravdanje u knjizi
stflife= as.factor(stflife)
marsts= sve$marsts
marsts[marsts>5]= NA
marsts= as.factor(marsts)
domicil= sve$domicil
domicil[domicil>5]= NA
domicil= as.factor(domicil)
iplylfr= sve$iplylfr
iplylfr[iplylfr>6]= NA
iplylfr= as.factor(iplylfr)
rlgdgr= sve$rlgdgr
rlgdgr[rlgdgr> 10]= NA
rlgdgr= as.integer(rlgdgr)
pray= sve$pray
pray[pray> 7]= NA
pray= as.factor(pray)
rlgatnd= sve$rlgatnd
rlgatnd[rlgatnd> 7]= NA
wkhtot= sve$wkhtot
wkhtot[wkhtot>= 666]= NA
ggplot(data= as.data.frame(wkhtot), aes(x= wkhtot)) + geom_histogram(binwidth= 1)
# da maknem sve koji su stavili da rade više od 70 sati tjedno?
eisced= sve$eisced
eisced[eisced> 7]= NA
eisced= as.factor(eisced)
hinctnta= sve$hinctnta
hinctnta[hinctnta> 10]= NA
hhmmb= sve$hhmmb
hhmmb[hhmmb>= 77]= NA

ggplot(data= as.data.frame(hhmmb), aes(x= hhmmb)) + geom_histogram(binwidth= 1)


tab=table(sve$cntry)
income= bdp$income
each= tab
bdp.prihodi= as.numeric(unlist(mapply(rep, income, each)))
sve$bdp_prihodi= bdp.prihodi
bdp_decil= 
sve= data.frame(cntry, stflife, bdp.prihodi, health, hinctnta, iplylfr, hhmmb,
                domicil, wkhtot, rlgdgr, rlgatnd, pray, marsts)

# napraviti primanja prema decilu kojemu svatko pripada u BDPu

# TO JE DATASET


# izračunati primanja po decilima; KONZULTACIJE!!!
library(ggplot2)
ggplot(data= sve, mapping= aes(x= hinctnta)) + geom_histogram(binwidth = 1) # jasno se vidi distrbucija


# REGIJE
be= subset(sve, cntry== "BE")
hr= subset(sve, cntry== "HR")
bg= subset(sve, cntry=="BG")
ch= subset(sve, cntry=="CH")
cz= subset(sve, cntry== "CZ")
ee= subset(sve, cntry=="EE")
fi= subset(sve, cntry=="FI")
fr= subset(sve, cntry== "FR")
gb= subset(sve, cntry== "GB")
gr= subset(sve, cntry== "GR")
hu= subset(sve, cntry=="HU")
ie= subset(sve, cntry=="IE")
is= subset(sve, cntry== "IS")
it= subset(sve, cntry== "IT")
lt= subset(sve, cntry== "LT")
me= subset(sve, cntry== "ME")
mk= subset(sve, cntry== "MK")
nl= subset(sve, cntry=="NL")
no= subset(sve, cntry== "NO")
pt= subset(sve, cntry=="PT")
sl= subset(sve, cntry=="SI")
sk= subset(sve, cntry=="SK")

zapadna= rbind(be, ch, fr, gb, ie, nl)
sjeverna= rbind(ee, fi, lt, is)
juzna= rbind(it, pt, gr)
istocna= rbind(hr, bg, cz, hu, me, mk, sl, sk)

set.seed(57123)
index.zapadna= sample(3, nrow(zapadna), replace= T, prob= c(0.7, 0.2, 0.1))
trainset.zapadna= zapadna[index.zapadna==1, ]
testset.zapadna = zapadna[index.zapadna== 2, ]
valset.zapadna= zapadna[index.zapadna== 3, ]

index.istocna= sample(3, nrow(istocna), replace= T, prob= c(0.7, 0.2, 0.1))
trainset.istocna= istocna[index.istocna==1, ]
testset.istocna = istocna[index.istocna== 2, ]
valset.istocna= istocna[index.istocna== 3, ]
  
index.sjeverna= sample(3, nrow(sjeverna), replace= T, prob= c(0.7, 0.2, 0.1))
trainset.sjeverna= sjeverna[index.sjeverna==1, ]
testset.sjeverna = sjeverna[index.sjeverna== 2, ]
valset.sjeverna= sjeverna[index.sjeverna== 3, ]
  
index.juzna= sample(3, nrow(juzna), replace= T, prob= c(0.7, 0.2, 0.1))
trainset.juzna= juzna[index.juzna==1, ]
testset.juzna= juzna[index.juzna== 2, ]
valset.juzna= juzna[index.juzna== 3, ]

# napraviti primanja prema decilu kojemu pripada u BDPu; ali to kod "sve"

