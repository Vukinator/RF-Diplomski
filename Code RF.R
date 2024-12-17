ess.raw<- read.csv("ESS10.csv")
bdp<- read.csv("BDP.csv")
bdp<- bdp[-(32:40), -(2:4)]
bdp<- bdp[-c(21:23,24:29), ]
names(bdp)<- c("country", "income")

bdp$country<- sort(bdp$country)
ess<- ess.raw[, c("cntry", "stflife", "health", "hhmmb",
                  "rshpsts","domicil", "sclmeet","iplylfr", 
                  "inprdsc", "imptrad", "imprich","rlgdgr",
                  "rlgdnm","pray", "rlgatnd", "wkhtot",
                  "eisced","hinctnta","gndr","agea")]

# da stavim i spol (gndr) i dob (agea)? i important to be rich (imprich) i important to follow traditions and customs(imptrad) za zajednicu kod iplylfr?
# da stavim i koliko se često nalaze s prijateljima (sclmeet) i s koliko može ljudi razgovarati o intimnim stvarima (inprdsc)
# koji je odnos s onim s kojim živi (rshpsts)- to ide za obitelj
cntry<- as.factor(ess$cntry)

gndr<- as.factor(ess$gndr)
age<- ess$agea

health<- ess$health
health[health== 1]<- 10 # zamjena; obrnuti redoslijed
health[health== 5]<- 1 
health[health== 10]<- 5
health[health== 2]<- 9
health[health== 4]<- 2
health[health== 9]<- 4
health<- as.integer(health)
health[health> 5]<- floor(mean(health, na.rm= T))

stflife<- ess$stflife
stflife[stflife>10]<- floor(mean(stflife, na.rm= T))
stflife<- cut(stflife, 
              breaks= c(-1, 2, 4, 6, 8, 10),
              labels= c(1, 2, 3, 4, 5),
              right= T) # 5 KATEGORIJA; automatski postaje factor

levels(stflife)<- c("Totally unsatisfied",
                    "Unsatisfied",
                    "Not satisfied nor unsatisfied",
                    "Satisfied",
                    "Totally satisfied")

#stflife<- ess$stflife
#stflife[stflife>10]<- floor(mean(stflife, na.rm= T))
#stflife<- cut(stflife, 
#              breaks= c(-1, 2, 5, 8, 10),
#             labels= c(1, 2, 3, 4),
#              right= T) # 4 KATEGORIJA; automatski postaje faktor

# a možda može i 4 kategorije; našel sam opravdanje u knjizi
#levels(stflife)<- c("Totally unsatisfied",
#                    "Unsatisfied",
#                    "Satisfied",
#                    "Totally satisfied")

rshpsts<- ess$rshpsts
table(rshpsts)
rshpsts[rshpsts> 66]<-  1 
rshpsts<- as.factor(rshpsts)
levels(rshpsts)<- c("Legally married",
                   "In a legally registered civil union",
                   "Living with my partner - not legally recognised",
                   "Living with my partner - legally recognised",
                   "Legally separated",
                   "Legally divorced",
                   "Not applicable")


domicil<- ess$domicil
table(domicil)
domicil[domicil>5]<- which.max(table(domicil)) # 56 NA vrijednosti
domicil<- as.factor(domicil)
levels(domicil)<- c("A big city",
                    "Suburbs or outskirts of big city",
                    "Town or small city",
                    "Country village",
                    "Farm or home in countryside")

iplylfr= ess$iplylfr
iplylfr[iplylfr== 1]<- 10 # znači, tu mijenjamo redoslijed
iplylfr[iplylfr== 6]<- 1
iplylfr[iplylfr== 10]<- 6
iplylfr[iplylfr== 2]<- 11
iplylfr[iplylfr== 5]<- 2
iplylfr[iplylfr== 11]<- 5
iplylfr[iplylfr== 3]<- 13
iplylfr[iplylfr== 4]<- 3
iplylfr[iplylfr== 13]<- 4
iplylfr[iplylfr> 6]<- floor(mean(iplylfr[iplylfr< 6])) # tak sam se riješil NA

imprich<- ess$imprich
table(imprich)
imprich[imprich== 1]<- 10 # isto kao i kod iplylfr
imprich[imprich== 6]<- 1
imprich[imprich== 10]<- 6
imprich[imprich== 2]<- 11
imprich[imprich== 5]<- 2
imprich[imprich== 11]<- 5
imprich[imprich== 3]<- 13
imprich[imprich== 4]<- 3
imprich[imprich== 13]<- 4
imprich[imprich> 6]<- floor(mean(imprich[imprich< 6]))

imptrad<- ess$imptrad
table(imptrad)
imptrad[imptrad== 1]<- 10
imptrad[imptrad== 6]<- 1
imptrad[imptrad== 10]<- 6
imptrad[imptrad== 2]<- 11
imptrad[imptrad== 5]<- 2
imptrad[imptrad== 11]<- 5
imptrad[imptrad== 3]<- 13
imptrad[imptrad== 4]<- 3
imptrad[imptrad== 13]<- 4
imptrad[imptrad> 6]<- floor(mean(imptrad[imptrad< 6]))

sclmeet<- ess$sclmeet
sort(table(sclmeet), decreasing= T)
sclmeet[sclmeet> 7]<- 6
levels(sclmeet)<- c("Never", "Less than once a month",
                    "Once a month", "Several times a month",
                    "Once a week", "Several times a week",
                    "Every day")

inprdsc<- ess$inprdsc
sort(table(inprdsc), decreasing= T)
inprdsc[inprdsc> 6]<- 2
inprdsc<- as.factor(inprdsc)
levels(inprdsc)<- c("None", "1", "2", "3", "4-6", "7-9", "10+")




rlgdgr<- ess$rlgdgr
rlgdgr[rlgdgr>10]<- as.integer(mean(rlgdgr[rlgdgr<= 10], na.rm= T))

rlgatnd<- ess$rlgatnd
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
rlgdnm<- ess$rlgdnm
sort(table(rlgdnm), decreasing= T)
rlgdnm[rlgdnm> 8]<- 66
rlgdnm<- as.factor(rlgdnm)
levels(rlgdnm)<- c("Roman Catholic", "Protestant",
                   "Eastern Orthodox", "Other Christian Denomination",
                   "Jewish", "Islam", "Eastern religions", 
                   "Other Non-Christian Religions",
                   "Not applicable")


pray<- ess$pray
pray[pray> 7]<- which.max(table(pray))
pray<- as.factor(pray)
levels(pray)<- c("Every day",
                 "More than once a week",
                 "Once a week",
                 "At least once a month",
                 "Only on special holy days",
                 "Less often",
                 "Never")


wkhtot<- ess.raw$wkhtot
wkhtot[wkhtot>= 140]<- mean(wkhtot[wkhtot<= 140], na.rm= T)# mean od svih koji imaju manje od 120 radnih sati tjedno

eisced<- ess$eisced
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

hinctnta<- ess$hinctnta
hinctnta[hinctnta> 10]<- floor(mean(hinctnta[hinctnta<= 10], na.rm= T)) # ovo bi možda trebalo za svaku državu zajedno; i staviti kao faktor!
hinctnta<- as.factor(hinctnta)

hhmmb<- ess$hhmmb # veće od 13 moram pretvoriti u NA
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
ajmo<- data.frame(cntry, bdp.prihodi, stflife, health, hhmmb,
                 rshpsts, domicil, sclmeet,iplylfr, 
                 inprdsc, imptrad, imprich,rlgdgr,
                 rlgdnm,pray, rlgatnd, wkhtot,
                 eisced,hinctnta,gndr,age)

# REGIJE
be<- subset(ajmo, cntry== "BE")
hr<- subset(ajmo, cntry== "HR")
bg<- subset(ajmo, cntry=="BG")
ch<- subset(ajmo, cntry=="CH")
cz<- subset(ajmo, cntry== "CZ")
ee<- subset(ajmo, cntry=="EE")
fr<- subset(ajmo, cntry== "FR")
fi<- subset(ajmo, cntry== "FI") 
gb<- subset(ajmo, cntry== "GB")
gr<- subset(ajmo, cntry== "GR")
hu<- subset(ajmo, cntry=="HU")
ie<- subset(ajmo, cntry=="IE")
is<- subset(ajmo, cntry== "IS")
it<- subset(ajmo, cntry== "IT")
lt<- subset(ajmo, cntry== "LT")
me<- subset(ajmo, cntry== "ME")
mk<- subset(ajmo, cntry== "MK")
nl<- subset(ajmo, cntry=="NL")
no<- subset(ajmo, cntry== "NO")
pt<- subset(ajmo, cntry=="PT")
sl<- subset(ajmo, cntry=="SI")
sk<- subset(ajmo, cntry=="SK")

zapadna= rbind(be, ch, fr, gb, ie, nl)
sjeverna= rbind(ee, fi, lt, is)
juzna= rbind(it, pt, gr)
istocna= rbind(hr, bg, cz, hu, me, mk, sl, sk)

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
