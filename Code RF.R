# Data
data1= read.csv("ESS10.csv") # 22 countries
# do the same with data1 as with data                              
# just rbind and that's it

data= read.csv("ESS10SC.csv") # 9 countries
#rbind for all!!!

gdppc.gini=  read.csv("GDPpc, GINI, WB 2020.csv")
all.econs= read.csv("All econ wb.csv")
all.econs= all.econs[-c(249:253),]
governance.vars= read.csv("Governance wb.csv")
govs= governance.vars[-c(156:160),]
efi= read.csv("EFI Heritage.csv")
efi2020= efi %>% filter(Index.Year==2020) %>% select(Name, Overall.Score)
wb= read.csv("WB1 gdp per capita.csv")
wb2= read.csv("WB2 gdp per capita.csv")
govexp.data= "https://data.imf.org/?sk=a0867067-d23c-4ebc-ad23-d3b015045405"
imf.govexp= readxl::read_excel("IMF govexps.xlsx")

#Sorting, filtering, choosing etc
stf= data$stflife
stf1= data1$stflife
# criteria variable

cntry= data$cntry
cntry1= data1$cntry
age= data$agea
age1= data1$agea
gender= data$gndr
gender1= data1$gndr
edu= data$eisced
edu1= data1$eisced
# ann.inc from world bank data 2020 (GDP per capita)
# political affiliation not found

poltrust= data$trstplt
poltrust1= data1$trstplt
ppltrust= data$ppltrst
ppltrust1= data1$ppltrst
rlgs= data$rlgdgr
rlgs1=data1$rlgdgr
denom= data$rlgdnm
denom1= data1$rlgdnm
rlgatnd= data$rlgatnd
rlgatnd1= data1$rlgatnd
pray= data$pray
pray1= data1$pray
employ= data$emplrel
employ1= data1$emplrel
left.right= data$lrscale
left.right1= data1$lrscale
householdmembers= data$hhmmb
householdmembers1= data1$hhmmb
netusoft= data$netusoft
netuseoft1= data1$netusoft
netustm= data$netustm
netustm1= data1$netustm
vote= data$vote
vote1= data1$vote
safetydark= data$aesfdrk
safetydark1= data1$aesfdrk
closetoparent= data$closepnt
closetoparent1= data1$closepnt
jobsatisfaction= data$stfmjob
jobasatisfaction1= data1$stfmjob
politics.time= data$nwspol
politics.time1= data1$nwspol
govtisfaction= data$stfgov
govtisfaction1= data1$stfgov
urban.rural= data$domicil
urban.rural1= data1$domicil

# for rbind(), the data must be a data frame
age.all= rbind(as.data.frame(c(age, age1)))
closeparent= rbind(as.data.frame(c(closetoparent, closetoparent1)))
country= rbind(as.data.frame(c(cntry, cntry1)))
# denomination? (yes or no)
education= rbind(as.data.frame(c(edu, edu1)))
employment= rbind(as.data.frame(c(employ, employ1)))
genders= rbind(as.data.frame(c(gender, gender1)))
housemembers= rbind(as.data.frame(c(householdmembers, householdmembers1)))
jobtisfy= rbind(as.data.frame(c(jobsatisfaction, jobasatisfaction1)))
pol.scale= rbind(as.data.frame(c(left.right, left.right1)))
net.minutes= rbind(as.data.frame(c(netustm, netustm1)))
affairs.minutes= rbind(as.data.frame(c(politics.time, politics.time1)))
trust.pol= rbind(as.data.frame(c(poltrust, poltrust1)))
trust.ppl= rbind(as.data.frame(c(ppltrust, ppltrust1)))
prayer= rbind(as.data.frame(c(pray, pray1)))
rlgsattendance= rbind(as.data.frame(c(rlgatnd, rlgatnd1)))
religios= rbind(as.data.frame(c(rlgs, rlgs1)))
safetyafdark= rbind(as.data.frame(c(safetydark, safetydark1)))
satisfaction= rbind(as.data.frame(c(stf, stf1)))
voted= rbind(as.data.frame(c(vote, vote1)))
govsatis= rbind(as.data.frame(c(govtisfaction, govtisfaction1)))
domicil= rbind(as.data.frame(c(urban.rural, urban.rural1)))

ess= cbind(age.all, closeparent, country, education, employment, genders, 
           housemembers, jobtisfy, pol.scale, net.minutes, affairs.minutes,
           trust.pol, trust.ppl, prayer, rlgsattendance, religios, safetyafdark,
           satisfaction, voted, govsatis, domicil)

names(ess)= c("age", "parent.close", "country", "edu", "employ", "gender",
              "house.members", "jobtisfy", "pol.spectre", "net.use", "pol.time",
              "pol.trust","people.trust", "prayer", "rel.attend", "religious",
              "safety", "satisfaction", "voted", "govtisfaction", "urbanized")

rm(vote1, vote, urban.rural1,urban.rural, stf1, stf, safetydark, safetydark1,
   rlgs1, rlgs, rlgtand1, rlgatnd, pray1, pray, ppltrust, ppltrust1, poltrust,
   poltrust1, politics.time, politics.time1, netustm, netustm1, netuseoft1, netusoft,
   left.right, left.right1, jobsatisfaction, jobasatisfaction1, householdmembers,
   householdmembers1, govtisfaction, govtisfaction1, gender1, gender, employ, employ1,
   edu1, edu, denom1, denom, cntry1, cntry, closetoparent, closetoparent1, age1, age, 
   voted, trust.ppl, trust.pol, satisfaction, safetyafdark, rlgsattendance, religios,
   prayer, pol.scale, net.minutes, jobtisfy, housemembers,govsatis, genders, employment,
   education, domicil, closeparent, age.all, affairs.minutes, rlgatnd1, country)

ess= ess[order(ess$country), ] # optional alphabetical ordering
unique(ess$country) # all the states

at= ess[ess$country== "AT", 1:length(ess)]
cy= ess[ess$country== "CY", 1:length(ess)]
de= ess[ess$country== "DE", 1:length(ess)]
es= ess[ess$country== "ES", 1:length(ess)]
il= ess[ess$country== "IL", 1:length(ess)]
lv= ess[ess$country== "LV", 1:length(ess)]
pl= ess[ess$country== "PL", 1:length(ess)]
rs= ess[ess$country== "RS", 1:length(ess)]
se= ess[ess$country== "SE", 1:length(ess)]
be= ess[ess$country== "BE", 1:length(ess)]
bg= ess[ess$country== "BG", 1:length(ess)]
ch= ess[ess$country== "CH", 1:length(ess)]
cz= ess[ess$country== "CZ", 1:length(ess)]
ee= ess[ess$country== "EE", 1:length(ess)]
fi= ess[ess$country== "FI", 1:length(ess)]
fr= ess[ess$country== "FR", 1:length(ess)]
gb= ess[ess$country== "GB", 1:length(ess)]
gr= ess[ess$country== "GR", 1:length(ess)]
hr= ess[ess$country== "HR", 1:length(ess)]
hu= ess[ess$country== "HU", 1:length(ess)]
ie= ess[ess$country== "IE", 1:length(ess)]
is= ess[ess$country== "IS", 1:length(ess)]
it= ess[ess$country== "IT", 1:length(ess)]
lt= ess[ess$country== "LT", 1:length(ess)]
me= ess[ess$country== "ME", 1:length(ess)]
mk= ess[ess$country== "MK", 1:length(ess)]
nl= ess[ess$country== "NL", 1:length(ess)]
no= ess[ess$country== "NO", 1:length(ess)]
pt= ess[ess$country== "PT", 1:length(ess)]
si= ess[ess$country== "SI", 1:length(ess)]
sk= ess[ess$country== "SK", 1:length(ess)]

# Economic freedom index
efi= read.csv("EFI Heritage.csv")
efi2020= efi %>% filter(Index.Year==2020) %>% select(Name, Overall.Score)

at$efi= 73.3
cy$efi= 70.1
de$efi= 73.5
es$efi= 66.9
il$efi= 74
lv$efi= 71.9
pl$efi= 69.1
rs$efi= 66
se$efi= 74.9
be$efi= 68.9
bg$efi= 70.2
ch$efi= 82
cz$efi= 74.8
ee$efi= 77.7
fi$efi= 75.7
fr$efi= 66
gb$efi= 79.3
gr$efi= 59.5
hr$efi= 62.2
hu$efi= 66.4
ie$efi= 80.9
is$efi= 74
it$efi= 63.8
lt$efi= 76.7
me$efi= 61.5
mk$efi= 69.5
nl$efi= 77
no$efi= 73.4
pt$efi= 67
si$efi= 67.8
sk$efi= 66.8

# Corruption perception index
at$cpi= 76
cy$cpi= 57
de$cpi= 80
es$cpi= 62
il$cpi= 60
lv$cpi= 57
pl$cpi= 56
rs$cpi= 38
se$cpi= 85
be$cpi= 76
bg$cpi= 44
ch$cpi= 85
cz$cpi= 54
ee$cpi= 75
fi$cpi= 85
fr$cpi= 69
gb$cpi= 77
gr$cpi= 50
hr$cpi= 47
hu$cpi= 44
ie$cpi= 72
is$cpi= 75
it$cpi= 53
lt$cpi= 60
me$cpi= 45
mk$cpi= 35
nl$cpi= 77
no$cpi= 84
pt$cpi= 61
si$cpi= 60
sk$cpi= 49

# GDP per capita
at$gdp.pc= 48409.23
cy$gpd.pc= 28035.98
de$gdp.pc= 46722.82
es$gdp.pc= 26959.61
il$gdp.pc= 44846.79
lv$gdp.pc= 18207.14
pl$gdp.pc= 15816.82
rs$gdp.pc= 7733.80
se$gdp.pc= 52837.90
be$gdp.pc= 45517.90
bg$gdp.pc= 10153.48
ch$gdp.pc= 85656.32
cz$gdp.pc= 22992.88
ee$gdp.pc= 23595.24
fi$gdp.pc= 49169.72
fr$gdp.pc= 39055.28
gb$gdp.pc= 40318.42
gr$gdp.pc= 17658.95
hr$gdp.pc= 14236.53
hu$gdp.pc= 16125.61
ie$gdp.pc= 85420.20
is$gdp.pc= 58813.80
it$gdp.pc= 31918.70
lt$gdp.pc= 20363.92
me$gdp.pc= 7677.37
mk$gdp.pc= 5965.45
nl$gdp.pc= 52162.57
no$gdp.pc= 68340.02
pt$gdp.pc= 22242.41
si$gdp.pc= 25545.24
sk$gdp.pc= 19551.62

# Uneployment rate
at$unemp= 5.36
cy$unemp= 7.59
de$unemp= 3.86
es$unemp= 15.53
il$unemp= 4.33
lv$unemp= 8.1
pl$unemp= 3.16
rs$unemp= 9.01
se$unemp= 8.29
be$unemp= 5.55
bg$unemp= 5.12
ch$unemp= 4.82
cz$unemp= 2.55
ee$unemp= 6.96
fi$unemp= 7.76
fr$unemp= 8.01
gb$unemp= 4.472
gr$unemp= 16.31
hr$unemp= 7.51
hu$unemp= 4.25
ie$unemp= 5.62
is$unemp= 5.48
it$unemp= 9.16
lt$unemp= 8.49
me$unemp= 17.88
mk$unemp= 16.55
nl$unemp= 3.82
no$unemp= 4.42
pt$unemp= 6.8
si$unemp= 4.97
sk$unemp= 6.69

# Government expenditure in GDP
at$govexp= 51.27
cy$govexp= 43.61
de$govexp= 32.77
es$govexp= 41.03
il$govexp= 42.34
lv$govexp= 36.4
pl$govexp= 42.14
rs$govexp= 49.02
se$govexp= 35.44
be$govexp= 44.57
bg$govexp= 37.72
ch$govexp= 20.11
cz$govexp= 39.03
ee$govexp= 42.56
fi$govexp= 41.38
fr$govexp= 52.1
gb$govexp= 47.36
gr$govexp= 57.34
hr$govexp= 45.74
hu$govexp= 47.58
ie$govexp= 26.63
is$govexp= 37.68
it$govexp= 50.53
lt$govexp= 41.21
me$govexp= NA   # nije im se dalo; no data since 2012
mk$govexp= 36.76
nl$govexp= 43.38
no$govexp= 47.22
pt$govexp= 46.26
si$govexp= 46.25
sk$govexp= 43.35

# Political stability 
at$polstab= 0.88
cy$polstab= 0.29
de$polstab= 0.64
es$polstab= 0.41
il$polstab= -0.89
lv$polstab= 0.46
pl$polstab= 0.49
rs$polstab= -0.17
se$polstab= 1
be$polstab= 0.52
bg$polstab= 0.4
ch$polstab= 1.18
cz$polstab= 0.91
ee$polstab= 0.71
fi$polstab= 0.98
fr$polstab= 0.28
gb$polstab= 0.48
gr$polstab= 0.11
hr$polstab= 0.61
hu$polstab= 0.84
ie$polstab= 0.96
is$polstab= 1.37
it$polstab= 0.4
lt$polstab= 0.92
me$polstab= -0.06
mk$polstab= 0.11
nl$polstab= 0.83
no$polstab= 1.22
pt$polstab= 1
si$polstab= 0.7
sk$polstab= 0.63

# Multidimensional poverty headcount ratio
at$multipov= 17.5
cy$multipov= 21.3
de$multipov= 24
es$multipov= 26.4
il$multipov= NA
lv$multipov= 26
pl$multipov= 17.3
rs$multipov= 29.8
se$multipov= 17.9
be$multipov= 18.9
bg$multipov= 32.1
ch$multipov= NA
cz$multipov= 11.9
ee$multipov= 23.3
fi$multipov= 16
fr$multipov= 18.2
gb$multipov= NA
gr$multipov= 28.9
hr$multipov= 23.2
hu$multipov= 17.8
ie$multipov= 20.8
is$multipov= NA
it$multipov= 25.3
lt$multipov= 24.8
me$multipov= 30.9
mk$multipov= 39.8
nl$multipov= 16.3
no$multipov= 15.9
pt$multipov= NA
si$multipov= 15
sk$multipov= 14.8

# Estimated control of corruption
at$corcon= 1.48
cy$corcon= 0.35
de$corcon= 1.83
es$corcon= 0.71
il$corcon= 0.54
lv$corcon= 0.70
pl$corcon= 0.62
rs$corcon= -0.45
se$corcon= 2.10
be$corcon= 1.45
bg$corcon= -0.32
ch$corcon= 2.05
cz$corcon= 0.56
ee$corcon= 1.58
fi$corcon= 2.17
fr$corcon= 1.12
gb$corcon= 1.66
gr$corcon= 0.03
hr$corcon= 0.18
hu$corcon= 0.07
ie$corcon= 1.53
is$corcon= 1.68
it$corcon= 0.51
lt$corcon= 0.78
me$corcon= -0.04
mk$corcon= -0.50
nl$corcon= 2
no$corcon= 2.07
pt$corcon= 0.72
si$corcon= 0.78
sk$corcon= 0.42

# Estimated governance effectivenes
at$govef= 1.61
cy$govef= 0.84
de$govef= 1.31
es$govef= 0.85
il$govef= 1.05
lv$govef= 0.84
pl$govef= 0.32
rs$govef= -0.04
se$govef= 1.67
be$govef= 1.08
bg$govef= -0.21
ch$govef= 1.97
cz$govef= 0.91
ee$govef= 1.30
fi$govef= 1.90
fr$govef= 1.20
gb$govef= 1.33
gr$govef= 0.40
hr$govef= 0.42
hu$govef= 0.54
ie$govef= 1.44
is$govef= 1.48
it$govef= 0.36
lt$govef= 1.01
me$govef= -0.11
mk$govef= 0.03
nl$govef= 1.81
no$govef= 1.89
pt$govef= 0.98
si$govef= 1.12
sk$govef= 0.50

# Estimated regulatory quality
at$regqual= 1.4
cy$regqual= 1
de$regqual= 1.58
es$regqual= 0.75
il$regqual= 1.23
lv$regqual= 1.18
pl$regqual= 0.85
rs$regqual= 0.08
se$regqual= 1.68
be$regqual= 1.34
bg$regqual= 0.46
ch$regqual= 1.58
cz$regqual= 1.23
ee$regqual= 1.53
fi$regqual= 1.85
fr$regqual= 1.19
gb$regqual= 1.47
gr$regqual= 0.54
hr$regqual= 0.36
hu$regqual= 0.47
ie$regqual= 1.48
is$regqual= 1.44
it$regqual= 0.49
lt$regqual= 1.08
me$regqual= 0.42
mk$regqual= 0.43
nl$regqual= 1.75
no$regqual= 1.7
pt$regqual= 0.82
si$regqual= 0.91
sk$regqual= 0.77

# Estimated rule of law
at$law= 1.77
cy$law= 0.55
de$law= 1.52
es$law= 0.86
il$law= 0.94
lv$law= 0.92
pl$law= 0.52
rs$law= -0.12
se$law= 1.77
be$law= 1.33
bg$law= -0.13
ch$law= 1.78
cz$law= 1.02
ee$law= 1.34
fi$law= 2.02
fr$law= 1.3
gb$law= 1.46
gr$law= 0.29
hr$law= 0.24
hu$law= 0.5
ie$law= 1.46
is$law= 1.75
it$law= 0.21
lt$law= 0.95
me$law= -0.04
mk$law= -0.11
nl$law= 1.71
no$law= 1.93
pt$law= 1.15
si$law= 1.04
sk$law= 0.65

# GINI Index (qustionable for our research)
at$gini= 29.8
cy$gini= 31.7
de$gini= NA
es$gini= 34.9
il$gini= NA
lv$gini= 35.7
pl$gini= NA
rs$gini= 35
se$gini= 28.9
be$gini= 26
bg$gini= 40.5
ch$gini= NA
cz$gini= 26.2
ee$gini= 30.7
fi$gini= 27.1
fr$gini= 30.7
gb$gini= 32.6
gr$gini= 33.6
hr$gini= 29.5
hu$gini= 29.7
ie$gini= 29.2
is$gini= NA
it$gini= 35.2
lt$gini= 36
me$gini= NA
mk$gini= NA
nl$gini= 26
no$gini= NA
pt$gini= 34.7
si$gini= 24
sk$gini= NA

ess= rbind(at, be, bg, ch, cy, cz, de, ee, es, fi, fr, gb, gr, hr, hu, ie, il,
           is, it, lt, lv, me, mk, nl, no, pl, pt, rs, se, si, sk)
rm(at, be, bg, ch, cy, cz, de, ee, es, fi, fr, gb, gr, hr, hu, ie, il, is, it,
   lt, lv, me, mk, nl, no, pl, pt, rs, se, si, sk)
# column names of the two data frames don’t match
# "Error in match.names(clabs, names(xi)) : names do not match previous names"
identical(names(at), names(cy)) # CYPRUS IS THE INTRUDER!!!! AAAAAAA
which(!identical(names(at), names(cy))) # 1!!!
names(cy)= names(at) # because all the others are the same as AT

as.factor(ess$country) # it's easier for us to make country a factor, and then merge the efi values
ess$country= as.factor(ess$country)
is.factor(ess$country) # makes no difference; still "Error in if (ess$country == "AT") { : the condition has length > 1"

ess$efi= if(ess$country== "AT"){
  73.3} else if(ess$country== "BE"){
      68.9} else if(ess$country== "BG"){
          62.2} else if(ess$country== "CH"){
              82} else if(ess$country== "CY"){
                  70.1} else if(ess$country== "CZ"){
                      74.8} else if(ess$country== "DE"){
                          78.3} else if(ess$country== "EE"){
                              77.7} else if(ess$country== "ES"){
                                  66.9} else if(ess$country== "FI"){
                                      75.7} else if(ess$country== "FR"){
                                          66} else if(ess$country== "GB"){
                                              79.3} else if(ess$country== "GR"){
                                                  59.9} else if(ess$country== "HR"){
                                                      62.2} else if(ess$country== "HU"){
                                                          66.4} else if(ess$country== "IE"){
                                                              80.9} else if(ess$country== "IL"){
                                                                  74} else if(ess$country== "IS"){
                                                                      77.1} else if(ess$country== "IT"){
                                                                          63.8} else if(ess$country== "LT"){
                                                                              77.3} else if(ess$country== "ME"){
                                                                                  61.5} else if (ess$country== "MK"){
                                                                                      66.5} else if(ess$country== "NL"){
                                                                                          77} else if(ess$country== "NO"){
                                                                                              73.4} else if(ess$country== "PT"){
                                                                                                  67} else if(ess$country== "SI"){
                                                                                                      67.8} else if(ess$country== "SK"){
                                                                                                          66.8} else{
                                                                                                            NA}
# if() checks only 1 element, not a vector!!!
### REMEMBER: FOR LOOPING THROGH VECTORS (NOT SINGLE VALUES) YOU USE for() loop!


#LIBRARY
library(tidyverse)
library(randomForest)
library(ranger)
library(tidymodels)

#BOOTSTRAP, SPLIT
set.seed(13472841)
splitting= sample(1:3, size=nrow(ess), prob=c(0.7,0.2,0.1), replace = T)
ess.train= ess[splitting==1, ]
ess.test= ess[splitting==2, ]
ess.valid= ess[splitting==3, ]

#RANDOM FOREST
### what we have to do is to differently code 999's and stuff like that
### (for classification of course)

rf.model1= randomForest(ess.train$satisfaction~., data= ess.train, na.action = na.omit)
# regression type; what we want is classification

rf.model2= randomForest(as.factor(ess.train$satisfaction)~., 
                                  data= ess.train,
                                  na.action= na.omit)
# classification; bagging done!

# do the breaks and merge it into the ess.train
# then do the interactions and merge them into the ess.train (for example, do the interactions with government satisfaction and gdp per capita etc.)
ess.train$satisfaction.cat= cut(ess.train, breaks= 3, # to be continued...)
