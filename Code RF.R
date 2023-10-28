data1= read.csv("ESS10.csv") # 22 countries
# do the same with data1 as with data                              
# just rbind and that's it

data= read.csv("ESS10SC.csv") # 9 countries
#rbind for all!!!

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

rm(vote1, vote, urban.rural1,urban.rural, stf1, stf, safetydark, safetydark1,
   rlgs1, rlgs, rlgtand1, rlgatnd, pray1, pray, ppltrust, ppltrust1, poltrust,
   poltrust1, politics.time, politics.time1, netustm, netustm1, netuseoft1, netusoft,
   left.right, left.right1, jobsatisfaction, jobasatisfaction1, householdmembers,
   householdmembers1, govtisfaction, govtisfaction1, gender1, gender, employ, employ1,
   edu1, edu, denom1, denom, cntry1, cntry, closetoparent, closetoparent1, age1, age, 
   voted, trust.ppl, trust.pol, satisfaction, safetyafdark, rlgsattendance, religios,
   prayer, pol.scale, net.minutes, jobtisfy, housemembers,govsatis, genders, employment,
   education, domicil, closeparent, age.all, affairs.minutes, rlgatnd1, country)

# make a dataframe
ess= cbind(age.all, closeparent, country, education, employment, genders, 
           housemembers, jobtisfy, pol.scale, net.minutes, affairs.minutes,
           trust.pol, trust.ppl, prayer, rlgsattendance, religios, safetyafdark,
           satisfaction, voted, govsatis, domicil)
names(ess)= c("age", "parent.close", "country", "edu", "employ", "gender",
              "house.members", "jobtisfy", "pol.spectre", "net.use", "pol.time",
              "pol.trust","people.trust", "prayer", "rel.attend", "religious",
              "safety", "satisfaction", "voted", "govtisfaction", "urbanized")
ess= ess[order(ess$country), ] # optional alphabetical ordering
unique(ess$country) # all the states
ess$efi= if (country== "AT"){
  73.3 else
    if(country== "BE"){
      68.9 else
        if(country== "BG"){
          62.2 else
            if(country== "CH"){
              82 else
                if(country== "CY"){
                  70.1 else
                    if(country== "CZ"){
                      74.8 else
                        if(country== "DE"){
                          78.3 else
                            if(country== "EE"){
                              77.7 else
                                if(country= "ES"){
                                  66.9 else
                                    if(country=="FI"){
                                      75.7 else
                                        if(country== "FR"){
                                          66 else
                                            if(country== "GB"){
                                              79.3 else 
                                                if(country== "GR"){
                                                  59.9 else 
                                                    if(country== "HR"){
                                                      62.2 else
                                                        if(country== "HU"){
                                                          66.4 else
                                                            if(country== "IE"){
                                                              80.9 else 
                                                                if(country== "IL"){
                                                                  74 else 
                                                                    if(country== "IS"){
                                                                      77.1 else
                                                                        if(country== "IT"){
                                                                          63.8
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                  }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

for(unique(i) in ess$country){
  ess$efi= c()
} ### maybe the alternative

library(tidyverse)
library(randomForest)
library(tidymodels)

#splitting the ess data frame

set.seed(13472841)
splitting= sample(1:3, size=nrow(ess), prob=c(0.7,0.2,0.1), replace = TRUE)
ess.train= ess[splitting==1, ]
ess.test= ess[splitting==2, ]
ess.valid= ess[splitting==3, ]

rf.model1= randomForest(ess.train$satisfaction~., data= ess.train, proximity= T)
# output= Error: cannot allocate vector of size 13.0 Gb (library)

ranger.rf= ranger::ranger(ess.train$satisfaction~., data= ess.train)
### check it out at home!!!


# ECON VARS
gov.in.gdp=
unemployment.rate=
gdp.pc= data.frame(cntrys.vec,
                     gdp= )
efi= read.csv("EFI Heritage.csv")
efi2020= efi %>% filter(Index.Year==2020) %>% select(Name, Overall.Score)

wb= read.csv("WB1 gdp per capita.csv")
wb2= read.csv("WB2 gdp per capita.csv")
#wb & wb2 are the same data frames

cntry.cpy= data.frame(cntrys.vec,
                      cpi= c(76, 76, 44, 47, 57, 54, 75, 85, 69, 56, 80,
                             50, 44, 75, 72, 60, 53, 57, 60, 45, 82, 35, 84,
                             56, 61, 38, 60, 49, 62, 85, 85, 77))
cntrys.vec= c("AT", "BE", "BU", "CRO", "CYP", "CZE", "EST", "FIN", "FRA", "GEO",
              "GER", "GRE", "HUN", "ICE", "IRE", "ISR", "ITA", "LAT", "LIT", "MNG",
              "NED", "NRMAC", "NOR", "POL", "POR", "SRB", "SVK", "SLO", "ESP",
              "SWE", "SCH", "UK")

poverty.rate= read.csv("Poverty rate OECD.csv")
