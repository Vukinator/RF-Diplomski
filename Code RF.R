library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ranger)
library(randomForestExplainer)
library(rfUtilities) # instalirano ručno

# Podaci

ess.raw<- read.csv("ESS10.csv")
bdp<- read.csv("BDP.csv")
bdp<- bdp[-(32:40), -(2:4)]
bdp<- bdp[-c(21:23,24:29), ]
names(bdp)<- c("country", "income")

bdp$country<- sort(bdp$country)
ess<- ess.raw[, c("cntry", "stflife", "health", "hhmmb",
                  "rshpsts","domicil", "sclmeet","iplylfr", 
                  "inprdsc", "imptrad", "imprich","rlgdgr",
                  "rlgdnm","pray", "rlgatnd", "wkhtot", "stfmjob",
                  "eisced","hinctnta","gndr","agea")]

cntry<- as.factor(ess$cntry)
levels(cntry)<- c("Belgija", "Bugarska", "Švicarska", "Češka", "Estonija",
                  "Finska", "Francuska", "V.Britanija", "Grčka",
                  "Hrvatska", "Mađarska", "Irska", "Island", "Italija",
                  "Litva", "C.Gora", "S.Makedonija", "Nizozemska",
                  "Norveška", "Portugal", "Slovenija", "Slovačka")
gndr<- as.factor(ess$gndr)
agea<- ess$agea

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
stflife[stflife>10]<- floor(mean(stflife[stflife<= 10], na.rm= T))
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
#levels(domicil)<- c("A big city",
#                   "Suburbs or outskirts of big city",
#                  "Town or small city",
#                 "Country village",
#                "Farm or home in countryside")

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
#levels(sclmeet)<- c("Never", "Less than once a month",
#                    "Once a month", "Several times a month",
#                   "Once a week", "Several times a week",
#                  "Every day")

inprdsc<- ess$inprdsc
sort(table(inprdsc), decreasing= T)
inprdsc[inprdsc> 6]<- 2
inprdsc<- as.factor(inprdsc)
# levels(inprdsc)<- c("None", "1", "2", "3", "4-6", "7-9", "10+")




rlgdgr<- ess$rlgdgr
rlgdgr[rlgdgr>10]<- as.integer(mean(rlgdgr[rlgdgr<= 10], na.rm= T))

rlgatnd<- ess$rlgatnd
table(rlgatnd)
rlgatnd[rlgatnd> 7]<- which.max(table(rlgatnd))
rlgatnd<- as.factor(rlgatnd)
#levels(rlgatnd)<- c("Every day",
#                   "More than once a week",
#                 "At least once a month",
#                   ""
#                 "Only on special holy days",
#                "Less often",
#               "Never")
rlgdnm<- ess$rlgdnm
sort(table(rlgdnm), decreasing= T)
rlgdnm[rlgdnm> 8]<- 66
rlgdnm<- as.factor(rlgdnm)
# levels(rlgdnm)<- c("Roman Catholic", "Protestant",
#                  "Eastern Orthodox", "Other Christian Denomination",
#                "Jewish", "Islam", "Eastern religions", 
#              "Other Non-Christian Religions",
#            "Not applicable")


pray<- ess$pray
pray[pray> 7]<- which.max(table(pray))
pray<- as.factor(pray)
# levels(pray)<- c("Every day",
#               "More than once a week",
#              "Once a week",
#             "At least once a month",
#            "Only on special holy days",
#           "Less often",
#          "Never")


wkhtot<- ess.raw$wkhtot
wkhtot[wkhtot>= 140]<- mean(wkhtot[wkhtot<= 140], na.rm= T)# mean od svih koji imaju manje od 120 radnih sati tjedno

stfmjob<- ess$stfmjob
stfmjob[stfmjob> 10]<- ceiling(mean(stfmjob[stfmjob<=10], na.rm= T))
stfmjob<- as.integer(cut(stfmjob, 
                         breaks= c(-1, 2, 4, 6, 8, 10),
                         labels= c(1, 2, 3, 4, 5),
                         right= T))


eisced<- ess$eisced
eisced[eisced> 55]<- which.max(table(eisced))
eisced[eisced== 55]<- 8
eisced<- as.factor(eisced)
# levels(eisced)<- c("Not possible to harmonise into ES-ISCED",
#                 "Less than lower secondary",
#                "Lower secondary",
#               "Lower tier upper secondary",
#              "Upper tier upper secondary",
#             "Advanced vocational, sub-degree",
#            "Lower tertiary education, BA level",
#           "Higher tertiary education, >= MA level",
#          "Other")

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
                  rlgdnm, pray, rlgatnd, stfmjob, wkhtot,
                  eisced ,hinctnta ,gndr ,agea)

summary(ajmo)

# REGIJE
be<- subset(ajmo, cntry== "Belgija")
hr<- subset(ajmo, cntry== "Hrvatska")
bg<- subset(ajmo, cntry=="Bugarska")
ch<- subset(ajmo, cntry=="Švicarska")
cz<- subset(ajmo, cntry== "Češka")
ee<- subset(ajmo, cntry=="Estonija")
fr<- subset(ajmo, cntry== "Francuska")
fi<- subset(ajmo, cntry== "Finska") 
gb<- subset(ajmo, cntry== "V.Britanija")
gr<- subset(ajmo, cntry== "Grčka")
hu<- subset(ajmo, cntry=="Mađarska")
ie<- subset(ajmo, cntry=="Irska")
is<- subset(ajmo, cntry== "Island")
it<- subset(ajmo, cntry== "Italija")
lt<- subset(ajmo, cntry== "Litva")
me<- subset(ajmo, cntry== "C.Gora")
mk<- subset(ajmo, cntry== "S.Makedonija")
nl<- subset(ajmo, cntry=="Nizozemska")
no<- subset(ajmo, cntry== "Norveška")
pt<- subset(ajmo, cntry=="Portugal")
sl<- subset(ajmo, cntry=="Slovenija")
sk<- subset(ajmo, cntry=="Slovačka")

zapadna<- rbind(be, ch, fr, gb, ie, nl)
sjeverna<- rbind(ee, fi, lt, is, no)
juzna<- rbind(it, pt, gr)
istocna<- rbind(hr, bg, cz, hu, me, mk, sl, sk)


set.seed(380)
index.zapadna<- sample(1:3, size=nrow(zapadna), replace= T, prob= c(0.7, 0.2, 0.1))
train.zapadna<- zapadna[index.zapadna == 1, ]
test.zapadna<- zapadna[index.zapadna == 2, ]
val.zapadna<- zapadna[index.zapadna== 3, ]

#Tezina klasa, zapadna; izračun težina prema obrnutoj frekvenciji (one kojih je najmanje su najviše penalizirane)
distribucija_klasa_zapadna<- table(zapadna$stflife)
zapadna_tezine_obrnuto<- 1/distribucija_klasa_zapadna; # težina svake klase obrnuto
zapadna_tezine_obrnuto<- zapadna_tezine/sum(zapadna_tezine) # normalizacija težina

zapadna_tezine<- distribucija_klasa_zapadna/sum(distribucija_klasa_zapadna)



index.sredist<- sample(1:3, size=nrow(istocna), replace= T, prob= c(0.7, 0.2, 0.1))
train.sredist<- istocna[index.sredist== 1, ]
test.sredist<- istocna[index.sredist== 2, ]
val.sredist<- istocna[index.sredist== 3, ]

# Tezina klasa, juzna:
distribucija_klasa_zapadna<- table(juzna$stflife)
juzna_tezine<- 1/distribucija_klasa; # težina svake klase
juzna_tezine<- juzna_tezine/sum(juzna_tezine) # normalizacija težina

index.juzna<- sample(1:3, size=nrow(juzna), replace= T, prob= c(0.7, 0.2, 0.1))
train.juzna<- juzna[index.juzna== 1, ]
test.juzna<- juzna[index.juzna== 2,]
val.juzna<- juzna[index.juzna== 3, ]

index.sjeverna<- sample(1:3, size=nrow(sjeverna), replace= T, prob= c(0.7, 0.2, 0.1))
train.sjeverna<- sjeverna[index.sjeverna== 1, ]
test.sjeverna<- sjeverna[index.sjeverna== 2, ]
val.sjeverna<- sjeverna[index.sjeverna== 3, ]

## BROJ ISPITANIKA PO DRŽAVAMA
ggplot(data= ajmo, aes(x=cntry, fill= cntry)) + 
  geom_bar() + 
  theme_minimal() + 
  labs(title= "Broj ispitanika po državama",
       x= "Država",
       y= "Broj ispitanika") + 
  theme(plot.title= element_text(hjust= 0.5), 
        legend.position = "none") + 
  scale_y_continuous(limits= c(0, 3000), breaks= seq(0, 3000, by= 100))

# PRIKAZ PROSJEČNOG ŽIVOTNOG ZADOVOLJSTVA ZA SVAKU DRŽAVU U SVAKOJ REGIJI
## Zapadna
meanstlife_zapadna<- zapadna %>% 
  group_by(cntry) %>% 
  summarise(stflife_mean= mean(as.integer(stflife)), na.rm= T)

ggplot(data= meanstlife_zapadna, aes(x= cntry, y= stflife_mean, fill= cntry)) + 
  geom_bar(stat= "identity") + 
  labs(title= "Srednja vrijednost razine životnoga zadovoljstva ispitanika država Zapadne Europe",
       x= "Država",
       y= "Srednja vrijednost") + 
  scale_y_continuous(limits= c(0, 5), breaks= seq(0, 5, by= 0.5)) + 
  theme(plot.title= element_text(hjust= 0.5),
        legend.position = "none") + 
  theme_minimal() + 
  guides(fill= "none")

## Srednjoistočna
meanstflife_sredistocna<- istocna %>% 
  group_by(cntry) %>% 
  summarise (stflife_mean= mean(as.integer(stflife)), na.rm= T)

ggplot(data= meanstflife_sredistocna, aes(x= cntry, y= stflife_mean, fill= cntry)) +
  geom_bar(stat= "identity") + 
  labs(title= "Srednja vrijednost razine životnoga zadovoljstva ispitanika država Srednjoistočne Europe",
       x= "Država",
       y= "Srednja vrijednost") + 
  scale_y_continuous(limits= c(0, 5), breaks= seq(0, 5, by= 0.5)) + 
  theme(plot.title= element_text(hjust= 0.5),
        legend.position = "none") + 
  theme_minimal() + 
  guides(fill= "none")

## Južna
meanstflife_juzna<- juzna %>% 
  group_by(cntry) %>% 
  summarise(stflife_mean= mean(as.integer(stflife)), na.rm= T)

ggplot(data= meanstflife_juzna, aes(x= cntry, y= stflife_mean, fill= cntry)) +
  geom_bar(stat= "identity") + 
  labs(title= "Srednja vrijednost razine životnoga zadovoljstva ispitanika država Južne Europe",
       x= "Država",
       y= "Srednja vrijednost") + 
  scale_y_continuous(limits= c(0, 5), breaks= seq(0, 5, by= 0.5)) + 
  theme(plot.title= element_text(hjust= 0.5),
        legend.position = "none") + 
  theme_minimal() + 
  guides(fill= "none")

## Sjeverna
meanstflife_sjeverna<- sjeverna %>% 
  group_by(cntry) %>% 
  summarise(stflife_mean= mean(as.integer(stflife)), na.rm= T)

ggplot(data= meanstflife_sjeverna, aes(x= cntry, y= stflife_mean, fill= cntry)) +
  geom_bar(stat= "identity") + 
  labs(title= "Srednja vrijednost razine životnoga zadovoljstva ispitanika država Sjeverna",
       x= "Država",
       y= "Srednja vrijednost") + 
  scale_y_continuous(limits= c(0, 5), breaks= seq(0, 5, by= 0.5)) + 
  theme(plot.title= element_text(hjust= 0.5),
        legend.position = "none") + 
  theme_minimal() + 
  guides(fill= "none")

# PROSJEČNA RAZINA ŽIVOTNOGA ZADOVOLJSTVA ZA SVAKU DRŽAVU
mean_stflife<- ajmo %>% 
  group_by(cntry) %>% 
  summarise(mean_stflife<- mean(as.integer(stflife), na.rm= T)) 

ggplot(mean_stflife, aes(x= cntry, y= mean_stflife$`mean_stflife <- mean(as.integer(stflife), na.rm = T)`, fill= cntry)) + 
  geom_bar(stat= "identity") + 
  labs(title= "Prosječna razina životnoga zadovoljstva ispitanika svake države",
       x= "Država", y= "Srednja vrijednost") +
  theme(plot.title = element_text(hjust= 0.5), 
        legend.position= "none") + 
  theme_minimal()

# Dodavanje varijable regija da bi napravil barplot kolko ima ispitanika iz svake regije
regije<- vector(length= nrow(ajmo))
for(i in 1:nrow(ajmo)){
  drzava<- ajmo$cntry[i]
  if(drzava== "Belgija" || drzava== "Francuska" || drzava== "Švicarska" || drzava== "Irska" || drzava== "V.Britanija" || drzava== "Nizozemska"){
    regije[i]<- "Zapadna Europa"
  } else if (drzava== "Grčka" || drzava== "Italija"){
    regije[i]<- "Južna Europa"
  } else if (drzava== "Norveška" || drzava== "Island" || drzava== "Finska" || drzava== "Estonija"){
    regije[i]<- "Sjeverna Europa"
  } else{
    regije[i]<- "Srednjoistočna Europa"
  }
  ajmo$regije<- regije
}

# FREKVENCIJA BROJA ISPITANIKA ZA SVAKU OD GEOGRAFSKIH CJELINA
ggplot(data= ajmo, aes(x= regije, fill= regije)) + 
  geom_bar() + 
  labs(title= "Frekvencija broja ispitanika po geografskim cjelinama",
       y= "Frekvencija ispitanika",
       x= "Geografska cjelina") + 
  scale_y_continuous(limits= c(0, 18000), 
                     breaks= seq(0, 18000, by= 1000)) +
  theme_minimal() +
  theme(plot.title= element_text(hjust= 0.5),
        legend.position = "none",
        axis.text.y = element_text(size= 10))

# SREDNJA VRIJEDNOST RAZINE ŽIVOTNOG ZADOVOLJSTVA ZA SVAKU OD GEOGRAFSKIH CJELINA
stf_regije_mean<- ajmo %>% group_by(regije) %>% summarize(stf_mean= (mean(as.integer(stflife))))

ggplot(data= stf_regije_mean, aes(x=regije, y=stf_mean, fill= regije)) +
  geom_bar(stat= "identity") +
  labs(title= "Prosječno zadovoljstvo životom po geografskim cjelinama",
       x= "Geografske cjeline",
       y= "Razina životnoga zadovoljstva") + 
  scale_y_continuous(limits= c(0, 5), 
                     breaks= seq(0, 5, by= 0.5)) +
  theme_minimal() + 
  theme(plot.title= element_text(hjust= 0.5),
        legend.position= "none",
        axis.title.y= element_text(size= 12))

# RANDOM FOREST ANALIZA
# Zapadna
## table(train.zapadna$stflife) / sum(nrow(train.zapadna)) - ovo iskoristiti za dodavanje težina (pogledaj kaj imaš na CHATGPT-u)
oob_zapadna_train<- vector(length= 20)
for (i in 1:20){
  set.seed(380)
  zapadna_train<- randomForest(
    stflife ~ ., 
    data= train.zapadna[, -1],
    mtry= i,
    ntree= 1000,
    classwt= zapadna_tezine
  )
  oob_zapadna_train[i]<- zapadna_train$err.rate[nrow(zapadna_train$err.rate), 1]
} # znači, sve može ići u for petlju

# OBAVEZNO ISKOPIRATI REZULTAT OOB_ZAPADNA_TRAIN
## oob_zapadna_train[i]<- 






seed380_zapadna<- function(data, target_column, seed, ntree, mtry){
  set.seed(seed)
  criteria<- data[[target_column]]
  predictors<- data[, !(names(data) %in% target_column)]
  zapadna_optimal<- randomForest(x= predictors,
                                 y= criteria,
                                 ntree= ntree,
                                 mtry= mtry
                                              ) 
  }
zapadna_model<- seed380_zapadna(data= train.zapadna,
                                target_column= "stflife",
                                seed= 380,
                                ntree= 500,
                                mtry= which.min(oob_zapadna_train) #7
                                )

emperror_zapadna<- mean(predict(zapadna_model, train.zapadna) != train.zapadna$stflife)
testerror_zapadna<- mean(predict(zapadna_model, test.zapadna) != test.zapadna$stflife)
valerror_zapadna<- mean(predict(zapadna_model, val.zapadna) != val.zapadna$stflife)

# Sredistočna
oob_sredist_train<- vector(length= 20)
for (i in 1:20){
  set.seed(380)
  sredist_train<- randomForest(
    stflife ~ ., 
    data= train.sredist[, -1],
    mtry= i,
    ntree= 500
  )
  oob_sredist_train[i]<- sredist_train$err.rate[nrow(sredist_train$err.rate), 1]
}
# OBAVEZNO ISKOPIRATI REZULTAT OOB_SREDIST_TRAIN
## 

seed380_sredist<- function(data, target_column, seed, ntree, mtry){
  set.seed(seed)
  criteria<- data[[target_column]]
  predictors<- data[, !(names(data) %in% target_column)]
  zapadna_optimal<- randomForest(x= predictors,
                                 y= criteria,
                                 ntree= ntree,
                                 mtry= mtry)
  }

sredist_model<- seed380_sredist(data= train.sred,
                                target_column = "stflife",
                                seed= 380,
                                ntree= 500,
                                mtry= which.min(oob_sredist_train) # 
                                )

emperror_sredistocna<- mean(predict(sredist_model, train.sredist) != train.sredist$stflife)
testerror_sredistocna<- mean(predict(sredist_model, test.sredist) != test.sredist$stflife)

# Južna
oob_juzna_train<- vector(length= 20)
for (i in 1:20){
  set.seed(380)
  juzna_train<- randomForest(stflife~., 
                             data= train.juzna,
                             mtry= i,
                             ntree= 500)
  oob_juzna_train[i]<- juzna_train$err.rate[nrow(juzna_train$err.rate), 1]
}

# OBAVEZNO ISKOPIRATI REZULTAT OOB_JUZNA_TRAIN
##

seed380_juzna<- function(data, target_column, seed, ntree, mtry){
  set.seed(seed)
  criteria<- data[[target_column]]
  predictors<- data[, !(names(data) %in% target_column)]
  zapadna_optimal<- randomForest(x= predictors,
                                 y= criteria,
                                 ntree= ntree,
                                 mtry= mtry)
}

juzna_model<- seed380_juzna(data= train.juzna,
                            target_column = "stflife",
                            seed= 380,
                            ntree= 500,
                            mtry= which.min(oob_juzna_train) # 
)

emperror_juzna<- mean(predict(juzna_model, train.juzna) != train.juzna$stflife)
testerror_juzna<- mean(predict(juzna_model, test.juzna) != test.juzna$stflife)

# Sjeverna
oob_sjeverna_train<- vector(length= 20)
for (i in 1:20){
  set.seed(380)
  sjeverna_train<- randomForest(stflife~., 
                                data= train.sjeverna,
                                mtry= i,
                                ntree= 500)
  oob_sjeverna_train[i]<- sjeverna_train$err.rate(nrow(sjeverna_train$err.rate), 1)
}
# OBAVEZNO ISKOPIRATI OOB_SJEVERNA_TRAIN
##

seed380_sjeverna<- function(data, target_column, seed, ntree, mtry){
  set.seed(seed)
  criteria<- data[[target_column]]
  predictors<- data[, !(names(data) %in% target_column)]
  zapadna_optimal<- randomForest(x= predictors,
                                 y= criteria,
                                 ntree= ntree,
                                 mtry= mtry)
}

sjeverna_model<- seed380_sjeverna(data= train.sjeverna,
                                  target_column= "stflife",
                                  seed= 380,
                                  ntree= 500,
                                  mtry= which.min(oob_sjeverna_train)

emperror_sjeverna<- mean(predict(sjeverna_model, train.sjeverna) != train.sjeverna$stflife)
testerror_sjeverna<- mean(predict(sjeverna_model, test.sjeverna) != test.sjeverna$stflife)  


# Navesti da je svaka geografska cjelina podijeljena na skup za treniranje i skup za testiranje
# Skup za treniranje čini 80% ukupnih podataka svake geografske cjeline, a skup za testiranje 20%.
# To znači da će u radu postojati 4 modela ansambla stabala odluke, svaki za jednu geografsku cjelinu.
# 








