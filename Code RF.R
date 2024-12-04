# Podaci
library(rpart)
library(rpart.plot)
library(ggplot2)
library(randomForest)
install.packages("rfUtilities"); library(rfUtilities) # instalirati ručno

sve.raw<- read.csv("ESS10.csv")
bdp<- read.csv("BDP.csv")
bdp<- bdp[-(32:40), -(2:4)]
bdp<- bdp[-c(21:23,24:29), ]
names(bdp)<- c("country", "income")

bdp$country<- sort(bdp$country)
sve<- sve.raw[, c("cntry", "stflife","anweight", "health",
             "marsts", "domicil", "iplylfr", "rlgdgr",
             "pray", "rlgatnd", "wkhtot", "eisced", 
             "hinctnta","hincfel", "hhmmb")]

cntry<- as.factor(sve$cntry)

health<- sve$health
health[health== 1]<- 10
health[health== 5]<- 1 
health[health== 10]<- 5
health[health== 2]<- 9
health[health== 4]<- 2
health[health== 9]<- 4
health<- as.integer(health)
health[health> 5]<- floor(mean(health, na.rm= T))

stflife<- sve$stflife
stflife[stflife>10]<- floor(mean(stflife, na.rm= T))
stflife<- cut(stflife, 
             breaks= c(-1, 2, 4, 6, 8, 10),
             labels= c(1, 2, 3, 4, 5),
             right= T)# to je to; automatski postaje factor
# a možda može i 4 kategorije; našel sam opravdanje u knjizi
levels(stflife)<- c("Totally unsatisfied",
                    "Unsatisfied",
                    "Not satisfied nor unsatisfied",
                    "Satisfied",
                    "Totally satisfied")

marsts<- sve$marsts
table(marsts)
marsts[marsts>6]<- 6 # mod je inače 66, ali to znači "Not Applicable", tak da sam uzel da je mod 6
marsts<- as.factor(marsts)
levels(marsts)<- c("Legally married",
                   "In a legally registered union",
                   "Legally separated",
                   "Legally divorced/Civil union dissolved",
                   "Widowed/Civil partner died",
                   "None of these")

domicil<- sve$domicil
table(domicil)
domicil[domicil>5]<- which.max(table(domicil)) # 56 NA vrijednosti
domicil<- as.factor(domicil)
levels(domicil)<- c("A big city",
                    "Suburbs or outskirts of big city",
                    "Town or small city",
                    "Country village",
                    "Farm or home in countryside")

iplylfr= sve$iplylfr # isto kao i za health; obrnuti redoslijed i ostaviti kao integer; jednostavno napravim jednu dodatnu varijablu za svaku zamjenu!; smijem li to?
iplylfr[iplylfr== 1]<- 10
iplylfr[iplylfr== 6]<- 1
iplylfr[iplylfr== 10]<- 1
iplylfr[iplylfr== 2]<- 9
iplylfr[iplylfr== 5]<- 2
iplylfr[iplylfr== 9]<- 5
iplylfr[iplylfr== 3]<- 8
iplylfr[iplylfr== 4]<- 3
iplylfr[iplylfr== 8]<- 3
iplylfr[iplylfr> 6]<- mean(iplylfr[iplylfr< 6]) # mod

rlgdgr<- sve$rlgdgr
rlgdgr[rlgdgr>10]<- as.integer(mean(rlgdgr[rlgdgr<= 10], na.rm= T))

rlgatnd<- sve$rlgatnd
table(rlgatnd)
rlgatnd[rlgatnd> 7]<- which.max(table(rlgatnd))
rlgatnd<- as.factor(rlgatnd)
levels(rlgatnd)<- c("Every day",
                    "More than once a week",
                    "Once a week",
                    "At least once a month",
                    "Only on special holy days",
                    "Less often",
                    "Never")

pray<- sve$pray
pray[pray> 7]<- which.max(table(pray))
pray<- as.factor(pray)
levels(pray)<- c("Every day",
                 "More than once a week",
                 "Once a week",
                 "At least once a month",
                 "Only on special holy days",
                 "Less often",
                 "Never")


wkhtot<- sve$wkhtot
wkhtot[wkhtot>= 140]<- mean(wkhtot[wkhtot<= 140], na.rm= T) # mean od svih koji imaju manje od 120 radnih sati tjedno

eisced<- sve$eisced
eisced[eisced> 55]<- which.max(table(eisced))
eisced[eisced== 55]<- 8
eisced<- as.factor(eisced)
levels(eisced)<- c("Not possible to harmonise into ES-ISCED",
                   "Less than lower secondary",
                   "Lower secondary",
                   "Lower tier upper secondary",
                   "Upper tier upper secondary",
                   "Advanced vocational, sub-degree",
                   "Lower tertiary education, BA level",
                   "Higher tertiary education, >= MA level",
                   "Other")

hinctnta<- sve$hinctnta
hinctnta[hinctnta> 10]<- floor(mean(hinctnta[hinctnta<= 10], na.rm= T)) # ovo bi možda trebalo za svaku državu zajedno

hhmmb<- sve$hhmmb # veće od 13 moram pretvoriti u NA
hhmmb[hhmmb> 13]<- floor(mean(hhmmb[hhmmb<=13], na.rm= T))

each<- table(cntry)
income<- c(45609.0034936111, 10148.3423954435, 85897.7843338323, 22992.8793833348,
           23595.2436836441, 49169.7193388499, 39179.7442596057, 40217.0090116986,
           17617.2915057014, 14269.9088549332, 16125.6094085407, 85973.0884875501,
           58848.41812446, 31922.9191626183, 20381.8557827478, 7677.37132106817,
           5965.45023195365, 52162.5701150406, 68340.0181033702, 22242.406417972,
           25558.4290544506, 19552.0911095911)
bdp.prihodi<- as.numeric(unlist(mapply(rep, income, each)))


# JUHU HEHE
sve<- data.frame(cntry, stflife, bdp.prihodi, health, hinctnta, iplylfr, hhmmb,
                domicil, wkhtot, rlgdgr, rlgatnd, pray, marsts)

# još napraviti primanja prema decilu kojemu svatko pripada u BDPu i to je to
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

zapadna= na.roughfix(rbind(be, ch, fr, gb, ie, nl))
sjeverna= na.roughfix(rbind(ee, fi, lt, is))
juzna= na.roughfix(rbind(it, pt, gr))
istocna= na.roughfix(rbind(hr, bg, cz, hu, me, mk, sl, sk))

set.seed(57123)
index.zapadna= sample(1:3, size= nrow(zapadna), replace= T, prob= c(0.7, 0.2, 0.1))
trainset.zapadna= zapadna[index.zapadna==1, ]
testset.zapadna = zapadna[index.zapadna== 2, ]
valset.zapadna= zapadna[index.zapadna== 3, ]

index.istocna= sample(1:3, nrow(istocna), replace= T, prob= c(0.7, 0.2, 0.1))
trainset.istocna= istocna[index.istocna==1, ]
testset.istocna = istocna[index.istocna== 2, ]
valset.istocna= istocna[index.istocna== 3, ]
  
index.sjeverna= sample(1:3, nrow(sjeverna), replace= T, prob= c(0.7, 0.2, 0.1))
trainset.sjeverna= sjeverna[index.sjeverna==1, ]
testset.sjeverna = sjeverna[index.sjeverna== 2, ]
valset.sjeverna= sjeverna[index.sjeverna== 3, ]

index.juzna= sample(1:3, nrow(juzna), replace= T, prob= c(0.7, 0.2, 0.1))
trainset.juzna= juzna[index.juzna==1, ]
testset.juzna= juzna[index.juzna== 2, ]
valset.juzna= juzna[index.juzna== 3, ]

# Primjer za rpart; treniranje i testiranje zapadne
primjer_diplomski<- rpart(stflife~.,
                          data= trainset.zapadna[, -1],
                          cp= 0,
                          minsplit= 2)
plot(primjer_diplomski) # jao brate; ovo je ogromno
primjer_cp<- primjer_diplomski$cptable

primjer_cp[which.min(primjer_cp[, 4]), ]
primjer_optimal<- prune(primjer_diplomski, cp= primjer_cp[which.min(primjer_cp[, 4])], 1)
plot(primjer_optimal)
text(primjer_optimal, xpd= T, cex= 0.5)
rpart.plot(primjer_optimal,
           type= 4,
           extra= 101,
           box.palette = "auto",
           clip.facs= T) # da, mislim da imam problea s "underrepresented klasama"

uvjet_1SE<- primjer_cp[which.min(primjer_cp[, 4]), 4:5]
optimal_1SE= primjer_diplomski$cptable[min(which(primjer_diplomski$cptable[, 4] <= sum(uvjet_1SE))), 1]

primjer_1SE<- prune(primjer_diplomski, cp= optimal_1SE)
rpart.plot(primjer_1SE, 
           type= 4, 
           extra= 101, 
           box.palette= "auto")
rpart.rules(primjer_1SE, 
            cover= T,
            extra= 4)

var

### Zapadna
sve= randomForest::na.roughfix(sve)
set.seed(123)
rftz= randomForest::randomForest(formula= stflife~., 
                                 data= na.roughfix(trainset.zapadna[, -1]),
                                 mtry= sqrt(ncol(sve)),
                                 ntree= 500,
                                 proximity= T,
                                 do.trace= 25) # svakih 25 stabala da pokaže OOB i ostale pokazatelje)

trainset.zapadna= trainset.zapadna[, -c(1, 3, 5)]

brojmtry= as.numeric(vector(length= 8))
najmtry= for (i in 1:8){
  rfztrain= randomForest(stflife~., 
                         data= na.roughfix(trainset.zapadna),
                         mtry= i,
                         ntree= 500)
  brojmtry[i]= rfztrain$err.rate[nrow(rfztrain$err.rate), 1]
}
