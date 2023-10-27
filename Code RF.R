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
householdmembers1= data$hhmmb
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
politics.time1= data$nwspol

# for rbin(), the data must be a data frame
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
# make a dataframe

# ECON VARS
gov.in.gdp=
unemployment.rate=

gdp.pc= data.frame(cntrys.vec,
                   gdp= )
efi= read.csv("EFI Heritage.csv")
efi.sorted= efi[,3:6]
cpi= c(76, 76, 44, 47, 57, 54, 75, 85, 69, 56, 80,
       50, 44, 75, 72, 60, 53, 57, 60, 45, 82, 35, 84,
       56, 61, 38, 60, 49, 62, 85, 85, 77)
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
findata= cbind(cntry, stf ,gender, age, edu, poltrust, ppltrust, pray, rlgatnd,
               rlgs, efi) # just to see how does it look like

poverty.rate= read.csv("Poverty rate OECD.csv")








