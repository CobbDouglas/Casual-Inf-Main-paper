## This needs to be space for the actual analysis. 
rm(list = ls())
library(tidyverse)
library(data.table)
library(plm)
library(MatchIt)
library(did)
library(readr)
Masterdata<-read_csv("Masterdataset.csv")
Treatdf <- read_delim("Treatdf.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)
View(Treatdf)

library(readr)
Treatment_times_ <- read_csv("Treatment times .csv")

## We need our treatment variables to be redefined. This list includes all states treated up to 2002. 
# Not sure how useful this is
Treat2 <- c("Arkansas","California","Colorado","Connecticut",
            "Delaware","Hawaii","Illinois","Kansas","Louisiana","Maine","Maryland",
            "Massachusetts","Minnesota","Montana",
            "New Hampshire","New Jersey","New Mexico","North Dakota",
            "Oklahoma","Rhode Island", "South Dakota",
            "Texas","Vermont","Virginia","West Virginia")

## Here we make some ifelse statements estbalishing treatment times
Treatment_times_[is.na(Treatment_times_)] <- 0
Treatment_times_$AccesstoParity <- Treatment_times_$`Pairty Law Passed`+Treatment_times_$`Mandated offering laws`
Treattimetemp <-Treatment_times_ |>
  filter( 1 < AccesstoParity) |>
  print(n=30)
TreatedStates1998<-Treattimetemp$Name
Treattimetemp2 <-Treatment_times_ |>
  filter( 0 == AccesstoParity) |>
  print(n=30)
NotTreatedStates1998 <- Treattimetemp2$Name
## Treated in 1998, Setting up Treatment Variables
## Note, Pre-Pairty and Post Parity are not based on the 1998 cut off. They have to be simply the observations that different states had
## Pre and post recieving Access to pairty states in total
Masterdata$Treat <- as.numeric(if_else(Masterdata$State %in% TreatedStates1998,1,0))
Masterdata$NonTreat <- as.numeric(if_else(Masterdata$State %in% TreatedStates1998,0,1))
Masterdata$Post <-  if_else(as.numeric(as.character(Masterdata$Year.x)) >= 1998,1,0)
Masterdata$TreatPost<- as.numeric(Masterdata$Treat*Masterdata$Post)

## Treat is broken, Needs to be specfic to what time each state actually becomes treated. 

ifelse(Masterdata$State %in% TreatedStates1998 & Masterdata$Year.x > Treattimetemp$AccesstoParity,1,0)
## Use this Treat if you need exact dates.
Masterdata <- Masterdata |>
  left_join(Treatment_times_, join_by("State"=="Name")) 
Masterdata$Treat <- ifelse(Masterdata$State %in% TreatedStates1998 & Masterdata$Year.x >= Masterdata$AccesstoParity,1,0)
Masterdata$NonTreat <- ifelse(Masterdata$State %in% TreatedStates1998 & Masterdata$Year.x >= Masterdata$AccesstoParity,0,1)
Masterdata$Pre_Post_Parity <- ifelse(Masterdata$State %in% TreatedStates1998,
                                     ifelse(Masterdata$Year.x >= Masterdata$AccesstoParity,"Post-Parity","Pre-Pairty"),"No-Parity")
## Fixed. It was R recycling the shorter object length. The ifelse statement was masking it. 


## Forgot to add a logged crude rate variable
Masterdata$lcruderate <- log(Masterdata$`Crude Rate`)
Masterdata$PercW
Masterdata$BankrupcyP100k

## 
## Need to adjust the Years in the data, Keep it 1990-2004
Masterdata1998 <- Masterdata |>
  filter(Year.x <= 2004)
## Here we Create our first sum stats table
library(vtable)
Covariates <- c("Crude Rate",
"lcruderate",
"unemployment rate",
"BankrupcyP100k",
"PercW")
sumtable(Masterdata1998,vars = Covariates,digits = 3 )
sumtable(Masterdata1998,vars = Covariates,group = "Pre_Post_Parity",digits = 3,group.weights = Masterdata1998$population )
## So there are 8 observations we need to fix outta this. If we allow one observation from each state that had a late adaoption
## It should work.  ACTUALLY DONT BOTHER THIS ISNT WORTH IT JUST REPORT YOUR FINDINGS 



FixSumstats <- c("Arkansas","California","Hawaii","Maryland","Minnesota","New Mexico","South Dakota", "West Virginia","Georgia")
library(plotrix)
library(Hmisc)
## I copied this from stackexchange hope it works to get a weighted standard error
wtd.stderror <- function(x, weights){
  var <- Hmisc::wtd.var(x, weights)
  weights <- sum( (weights / sum(weights))^2 )
  
  sqrt(var*weights)
}

Masterdata1998 |>
  mutate(TreatPrePostPeriod1998= if_else(State %in% TreatedStates1998 & Year.x >= 1998,"Post-Period","Pre-Period"))|>
  group_by(TreatPrePostPeriod1998)|>
  summarise(mean = weighted.mean( x= `Crude Rate`, w = population),
            st.err= wtd.stderror( x=`Crude Rate`,weights = population),
            sd   = weighted.sd(x=`Crude Rate`,w=population),
            n = n())

Masterdata1998 |>
mutate(NonTreatPrePostPeriod1998= if_else(State %in% NotTreatedStates1998 & Year.x >= 1998,"NoTreatPost-Period","NoTreatPre-Period")) |>
  group_by(NonTreatPrePostPeriod1998) |>
  summarise(mean = weighted.mean( x= `Crude Rate`, w = population),
            st.err= wtd.stderror( x=`Crude Rate`,weights = population),
            sd   = weighted.sd(x=`Crude Rate`,w=population),
            n = n())

## TABLE 3 DONE 
Masterdata1998 |>
mutate(TreatPrePostPeriod1998= if_else(State %in% TreatedStates1998 & Year.x >= 1998,"Post-Period",
                                       if_else(State %in% TreatedStates1998 & Year.x < 1998,"Pre-Period",
                                               if_else(State %in% NotTreatedStates1998 & Year.x >= 1998,"NoTreatPost-Period","NoTreatPre-Period"))))|>
  group_by(TreatPrePostPeriod1998)|>
  summarise(mean = as.numeric(weighted.mean( x= `Crude Rate`, w = population)),
            st.err= wtd.stderror( x=`Crude Rate`,weights = population),
            sd   = weighted.sd(x=`Crude Rate`,w=population),
            n = n())
## Log Suicide Rate
Masterdata1998 |>
  mutate(TreatPrePostPeriod1998= if_else(State %in% TreatedStates1998 & Year.x >= 1998,"Post-Period",
                                         if_else(State %in% TreatedStates1998 & Year.x <= 1998,"Pre-Period",
                                                 if_else(State %in% NotTreatedStates1998 & Year.x >= 1998,"NoTreatPost-Period","NoTreatPre-Period"))))|>
  group_by(TreatPrePostPeriod1998)|>
  summarise(mean = as.numeric(weighted.mean( x= lcruderate, w = population)),
            st.err= wtd.stderror( x=lcruderate,weights = population),
            sd   = weighted.sd(x=lcruderate,w=population),
            n = n())



Masterdata1998 |>
  group_by(State) |>
  count(Treat)|>
  print(n=80) |>
  filter(Treat==1)

library(skimr)

Masterdata1998 |>
  select(all_of(Covariates)) |>
  map_df(.f = ~ skimr::skim(.x))

Masterdata1998 |>
  select(all_of(Covariates)) |>
  skim()


write.csv(Treatment_times_, file= "Treatment times .csv", row.names = FALSE)
## Couple Notes- 1. MHPA started Jan 1st 1998. 
## 2. DID period is spilt up 1990-1997, post 1998-2004


## Weighted Means?  PRINT THIS OUT
Masterdata1998 |>
  summarise(weighted.mean(x=Masterdata1998$`Crude Rate`, w= Masterdata1998$population)) |>
  print(n=51)

## NEXT STEPS MODELING. CREATE TABLE 3, JUST USE DIFFERENCE MEANS
## TABLE 4 USES FIXED EFFECTS Log Suicide Rate- We need dummy variables for All Law statuses 

Masterdata1998 <-Masterdata1998 |>
  mutate(D_ParityLawPassed = if_else(`Pairty Law Passed` > 1,1,0))|>
  mutate(D_Mandatedofferinglaws = if_else(`Mandated offering laws` > 1,1,0))|>
  mutate(D_MinimumMandatedBenefit = if_else(`Minimum Mandated Benefit` > 1 & `Pairty Law Passed`==0,1,0))|>
  mutate(D_MandatedifOffered = if_else(`Mandated if Offered` > 1,1,0))|>
  mutate(D_NoLaw = if_else(`No Law` > 1,1,0)) |>
  mutate(D_AccessToParity = if_else(AccesstoParity > 1,1,0)) |>
  mutate(D_NonParityLaw = if_else(`Mandated if Offered` > 1 | `Minimum Mandated Benefit` > 1 & `Pairty Law Passed` == 0,1,0)) 
  
library(plm)

PanelMasterdata <- pdata.frame(Masterdata1998,index = c("State","Year.x"))



## Code for eventually doing the regressions 
MasterdataTable4Reg1 <- plm(PanelMasterdata$lcruderate ~
                   PanelMasterdata$unemployment.rate+
                   PanelMasterdata$BankrupcyP100k +
                   PanelMasterdata$PercW + factor(PanelMasterdata$D_Mandatedofferinglaws),
                   data = PanelMasterdata, model = "within", effect = "twoways", weights = PanelMasterdata$populaticoeon )
library(lmtest)
#coeftest(MasterdataTable4Reg1, vcov = vcovHC(MasterdataTable4Reg1, type = "sss",cluster = "state") )
## didn't work

library(estimatr)
lm_robust(lcruderate ~
            `unemployment rate` +
            `BankrupcyP100k`+
            `PercW`+
            `D_AccessToParity` +
            `D_NonParityLaw`
          ,Masterdata1998,weights = `population`,fixed_effects = ~ `State` )
# didn't work
summary(MasterdataTable4Reg1)

lapply(Masterdata1998[,41:46], sum)

Masterdata1998 |>
  filter(D_NoLaw == 1) |>
  count(State)

# We need the dummy varibles to correspond to their actual times they become parity states at time t
Treattimetemp <-Treatment_times_ |>
  filter( 1 < AccesstoParity) |>
  print(n=30)
StatesAccesstoParity<-Treattimetemp$Name
Treattimetemp <-Treatment_times_ |>
  filter( 1 < `No Law`) |>
  print(n=30)
StatesNoLaw <- Treattimetemp$Name
Treattimetemp <-Treatment_times_ |>
  filter( 1 < `Mandated if Offered`) |>
  print(n=30)
StatesMandatedifOffered <- Treattimetemp$Name
Treattimetemp <-Treatment_times_ |>
  filter( 1 < `Mandated offering laws` ) |>
  print(n=30)
StatesMandatedOffering <- Treattimetemp$Name
Treattimetemp <-Treatment_times_ |>
  filter( 1 < `Minimum Mandated Benefit`& `Pairty Law Passed` == 0) |>
  print(n=30)
StatesMinimumMandated <- Treattimetemp$Name
Treattimetemp <-Treatment_times_ |>
  filter( 1 < `Mandated if Offered` | 1 < `Minimum Mandated Benefit`& `Pairty Law Passed` == 0) |>
  print(n=30)
StatesNoAccessToParity <- Treattimetemp$Name
Treattimetemp <-Treatment_times_ |>
  filter( 1 < `Pairty Law Passed` ) |>
  print(n=30)
StatesParityLawPassed <- Treattimetemp$Name

StateGroups <- c(StatesAccesstoParity,
                 StatesNoLaw,
                 StatesMandatedifOffered,
                 StatesMandatedOffering,
                 StatesMinimumMandated,
                 StatesNoAccessToParity,
                 StatesParityLawPassed)

Masterdata1998$D_ParityLawPassed       <- ifelse(Masterdata1998$State %in% StatesParityLawPassed & Masterdata1998$Year.x >= Masterdata1998$`Pairty Law Passed`,1,0)
Masterdata1998$D_AccessToParity        <- ifelse(Masterdata1998$State %in% StatesAccesstoParity & Masterdata1998$Year.x >= Masterdata1998$AccesstoParity,1,0)
Masterdata1998$D_MandatedifOffered     <- ifelse(Masterdata1998$State %in% StatesMandatedifOffered & Masterdata1998$Year.x >= Masterdata1998$`Mandated if Offered`,1,0)
Masterdata1998$D_Mandatedofferinglaws  <- ifelse(Masterdata1998$State %in% StatesMandatedOffering & Masterdata1998$Year.x >= Masterdata1998$`Mandated offering laws`,1,0)
Masterdata1998$D_MinimumMandatedBenefit<- ifelse(Masterdata1998$State %in% StatesMinimumMandated & Masterdata1998$Year.x >= Masterdata1998$`Minimum Mandated Benefit`,1,0)
Masterdata1998$D_NonParityLaw          <- ifelse(Masterdata1998$State %in% StatesNoAccessToParity & Masterdata1998$Year.x >= Masterdata1998$NonPairtyLaw,1,0)
Masterdata1998$D_NoLaw                 <- ifelse( Masterdata1998$D_AccessToParity+ Masterdata1998$D_NonParityLaw ==0,1,0)

Masterdata1998 |>
  mutate(testnolaw = D_ParityLawPassed +D_Mandatedofferinglaws +D_MandatedifOffered+D_MinimumMandatedBenefit+D_NonParityLaw) |>
  select(testnolaw,State,Year.x) |>
  filter(testnolaw == 0) |>
  print(n=100)
## The above mess is an attempt to bring in line all groups w/o having them secretly mix behind my code
## Each group will be turned into dummies using ifelse statement to match when they turn on
## Check if group sum to their totals
lapply(Masterdata1998[,41:47], sum)
### Let's use Matchit
library(MatchIt)
# No matching; constructing a pre-match matchit object
m.out0 <- matchit(D_AccessToParity ~ `unemployment rate` 
                  + `BankrupcyP100k` 
                  + `PercW`, data = Masterdata1998,
                  method = NULL, distance = "glm")
summary(m.out0)
## Nearest Neighbor
m.out1 <- matchit(D_AccessToParity ~ `unemployment rate` 
                  + `BankrupcyP100k` 
                  + `PercW`, data = Masterdata1998,
                  method = "nearest", distance = "glm")
summary(m.out1, un =FALSE)
plot(m.out1)

plot(m.out1, type = "density", interactive = FALSE,
     which.xs = ~`unemployment rate` 
     + `BankrupcyP100k` 
     + `PercW`)

plot(summary(m.out1),var.order = "unmatched",abs = F)

## Probit
m.out2 <- matchit(D_AccessToParity ~ `unemployment rate` 
                  + `BankrupcyP100k` 
                  + `PercW`, data = Masterdata1998,
                  method = "full",link = "probit")
m.out2
summary(m.out2, un=F)
plot(summary(m.out2),var.order = "unmatched",abs = F)


## True PS matching 
m.out3 <- matchit(D_AccessToParity ~ `unemployment rate` 
                  + `BankrupcyP100k` 
                  + `PercW`, data = Masterdata1998,
                  method = "full", distance = "glm")
m.out3
summary(m.out3, un=F)
plot(summary(m.out3),var.order = "unmatched",abs = F)

## Introducing Sampling Weight is chaos. Going to try this to get robust clustered standerrs
m.data <- match.data(m.out2)

library("marginaleffects")

fit <- lm(lcruderate ~ D_AccessToParity * (`unemployment rate` 
                                           + `BankrupcyP100k` 
                                           + `PercW`), data = m.data)

avg_comparisons(fit,
                variables = "D_AccessToParity",
                vcov = ~subclass,
                newdata = subset(m.data, D_AccessToParity == 1),
                wts = "population")
# try fixest
library(fixest)
fixestmodelcol1 = feols(lcruderate ~ `D_AccessToParity`
                    +`D_NonParityLaw`
                    +`unemployment rate` 
                    +`BankrupcyP100k`
                    +`PercW`| State + Year.x, Masterdata1998, weights = Masterdata1998$population )
summary(fixestmodelcol1, cluster = "State")

fixestmodelcol2 = feols(lcruderate ~ `D_AccessToParity`
                        +`unemployment rate` 
                        +`BankrupcyP100k`
                        +`PercW`| State + Year.x, Masterdata1998, weights = Masterdata1998$population )
summary(fixestmodelcol2, cluster = "State")
fixestmodelcol3 = feols(lcruderate ~ `D_ParityLawPassed`+
                        +`D_Mandatedofferinglaws`
                        +`D_MandatedifOffered`
                        +`unemployment rate` 
                        +`BankrupcyP100k`
                        +`PercW`| State + Year.x, Masterdata1998, weights = Masterdata1998$population )
summary(fixestmodelcol3, cluster = "State")
fixestmodelcol4 = feols(lcruderate ~ `D_AccessToParity`
                        +`D_MandatedifOffered`
                        +`D_NoLaw`
                        +`unemployment rate` 
                        +`BankrupcyP100k`
                        +`PercW`| State + Year.x, Masterdata1998, weights = Masterdata1998$population )
summary(fixestmodelcol4, cluster = "State")

Masterdata1998 <-Masterdata1998 |>
  mutate(TimingCntrl = Lag(Masterdata1998$lcruderate,shift=1))|>
  mutate(lDelta = lcruderate- TimingCntrl)

fixestmodel1stDifcol1 = feols(`lDelta` ~ `D_AccessToParity`
                        +`D_NonParityLaw`
                        +`unemployment rate` 
                        +`BankrupcyP100k`
                        +`PercW`| State + Year.x, Masterdata1998, weights = Masterdata1998$population )
summary(fixestmodel1stDifcol1, cluster = "State")

Firstdifcol2data <- Masterdata1998 |>
  filter( AccesstoParity > 1 ) 


fixestmodel1stDifcol2 = feols(`lDelta` ~ `D_AccessToParity`
                              +`unemployment rate` 
                              +`BankrupcyP100k`
                              +`PercW`| State + Year.x, Firstdifcol2data, weights = Firstdifcol2data$population )

summary(fixestmodel1stDifcol2, cluster = "State")





## Fixest has the Sun aberham option, let's use it
Sunabmodel = feols(lcruderate ~ `unemployment rate`
                   +`BankrupcyP100k`
                   +`PercW`
                   + sunab(`AccesstoParity`,Year.x)
                   | State + Year.x, 
                   Masterdata1998, 
                   weights = Masterdata1998$population)

summary(Sunabmodel, cluster = "State")
Sunabmodelref = feols(lcruderate ~ `unemployment rate`
                   +`BankrupcyP100k`
                   +`PercW`
                   + sunab(`AccesstoParity`,Year.x,
                   ref.p = c(.F + 0:2,-1))
                   | State + Year.x,
                   Masterdata1998, 
                   weights = Masterdata1998$population)
iplot(list(Sunabmodel,Sunabmodelref))
iplot(list(Sunabmodel,Sunabmodelref), ref = "all")

summary(Sunabmodel,agg = "ATT")
##
## event study. 
## Here we are going to use DiD
## We need to create two different variables for groups. One 2x2 

Masterdata1998 <-Masterdata1998 |>
  mutate(simpledid=if_else(State %in% TreatedStates1998,1998,0))
                           

Masterdata1998 <-Masterdata1998 |>
  mutate(simple3did=if_else(State %in% TreatedStates1998 &AccesstoParity < 1998,1998,
          if_else(State %in% TreatedStates1998 &AccesstoParity >= 1998 & AccesstoParity < 2000,2000,
                   if_else(State %in% TreatedStates1998& AccesstoParity >= 2000,2002,0))))



library(did)
out <- att_gt(yname = "lcruderate",
              gname = "simple3did",
              tname = "Year.x",
              idname = "FIPS",
              xformla = ~ `unemployment rate`+ `BankrupcyP100k`  +  `PercW`,
              data = Masterdata1998,
              weightsname = "population",
              bstrap = T,
              base_period = "universal",
              control_group = "nevertreated",
              
              )
summary(out)
ggdid(out)
agg.simple <- aggte(out,type = "simple",na.rm = T)
agg.dynamic <- aggte(out,type = "dynamic", na.rm =T)
summary(agg.simple)
ggdid(agg.simple)
summary(agg.dynamic)



out2 <- att_gt(yname = "lcruderate",
              gname = "simpledid",
              tname = "Year.x",
              idname = "FIPS",
              xformla = ~ `unemployment rate`+ `BankrupcyP100k`  +  `PercW`,
              data = Masterdata1998,
              weightsname = "population",
              bstrap = T,
              base_period = "universal",
              control_group = "notyettreated",
              est_method = "reg"
              
)
summary(out2)
ggdid(out2)
agg.simple2 <- aggte(out2,type = "simple",na.rm = T)
agg.dynamic2 <- aggte(out2,type = "dynamic", na.rm =T)
summary(agg.simple2)
ggdid(agg.simple2)
summary(agg.dynamic2)




library(bacondecomp)
df_bacon <- bacon(lcruderate ~ Treat+Post+TreatPost+,
                  data = Masterdata1998,
                  id_var = "FIPS",
                  time_var = "Year.x")

df_bacon <- bacon(Masterdata1998$lcruderate ~ Treat+Post+TreatPost+ Masterdata1998$`unemployment rate`
                  + Masterdata1998$BankrupcyP100k  
                  + Masterdata1998$PercW,
                  data = Masterdata1998,
                  id_var = "FIPS",
                  time_var = "Year.x"
)

