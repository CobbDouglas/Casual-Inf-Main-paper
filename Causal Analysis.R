## This needs to be space for the actual analysis. 
rm(list = ls())
library(tidyverse)
library(data.table)
library(plm)
library(MatchIt)
library(did)
library(readr)
library(fixest)
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
## Clean cruderate name 
Masterdata <-Masterdata %>%
  rename(crude_rate = `Crude Rate` )

## Forgot to add a logged crude rate variable
Masterdata$lcruderate <- log(Masterdata$`crude_rate`)
Masterdata$PercW
Masterdata$BankrupcyP100k

## 
## Need to adjust the Years in the data, Keep it 1990-2004
Masterdata1998 <- Masterdata |>
  filter(Year.x <= 2004)
## Here we Create our first sum stats table
library(vtable)
Covariates <- c("crude_rate",
"lcruderate",
"unemployment rate",
"BankrupcyP100k",
"PercW")
table2labs <- c("Suicide Rate",
                "Log suicide rate",
                "Unemployment rate", 
                "Bankrupcy rate per 100k",
                "Percent of workers in large Firms")
table2all <-sumtable(Masterdata1998,vars = Covariates,digits = 4,
         labels = table2labs,
         group.weights = Masterdata1998$population,
         out = "latex"
         )
table2pairity <- sumtable(Masterdata1998,vars = Covariates,
         group = "Pre_Post_Parity",digits = 4,
         group.weights = Masterdata1998$population,
         labels = table2labs,
         group.long = F,
         out = "latex")
kable(list(table2all,table2pairity))
## So there are 8 observations we need to fix outta this. If we allow one observation from each state that had a late adaoption
## It should work.  ACTUALLY DONT BOTHER THIS ISNT WORTH IT JUST REPORT YOUR FINDINGS 

# Here we define the new group to use. 
Masterdata1998 %>%
mutate(Table2TreatPost= if_else(State %in% TreatedStates1998 & Year.x >= AccesstoParity,"Post-Period",
                                       if_else(State %in% TreatedStates1998 & Year.x < AccesstoParity,"Pre-Period",
                                               if_else(State %in% NotTreatedStates1998 & Year.x >= AccesstoParity,"NoTreatPost-Period","NoTreatPre-Period"))))

Table2Redo_all <-sumtable(Masterdata1998,vars = Covariates,digits = 4,
                     labels = table2labs,
                     group.weights = Masterdata1998$population,
                     out = "latex"
)
Table2Redo_Parity <- sumtable(Masterdata1998,vars = Covariates,
                          group = "Pre_Post_Parity",digits = 4,
                          group.weights = Masterdata1998$population,
                          labels = table2labs,
                          group.long = F,
                          out = "latex")
kable(list(Table2Redo_all,Table2Redo_all))



## Here in 10/14/24 We're going to copy the above code and redo the table. Instead with a
## Pre-post variable that is accurate to what we're doing with the CS and SunAbram Est

FixSumstats <- c("Arkansas","California","Hawaii","Maryland","Minnesota","New Mexico","South Dakota", "West Virginia","Georgia")
library(plotrix)
library(Hmisc)
## I copied this from stackexchange hope it works to get a weighted standard error
wtd.stderror <- function(x, weights){
  var <- Hmisc::wtd.var(x, weights)
  weights <- sum( (weights / sum(weights))^2 )
  
  sqrt(var*weights)
}
## Hey I don't think These are trust worthy they group the rest of the data into the pre preiod bucket here
TestFigure1 <-Masterdata1998 |>
  mutate(Group_Time_Period= if_else(State %in% TreatedStates1998 & Year.x >= 1998,"Post-Period","Pre-Period"))|>
  group_by(Group_Time_Period)|>
  summarise(mean = weighted.mean( x= `crude_rate`, w = population),
            st.err= wtd.stderror( x=`crude_rate`,weights = population),
            sd   = weighted.sd(x=`crude_rate`,w=population),
            n = n())
### !!!!!!!!!!!!
TestFigure2 <-Masterdata1998 |>
mutate(Group_Time_Period= if_else(State %in% NotTreatedStates1998 & Year.x >= 1998,"NoTreatPost-Period","NoTreatPre-Period")) |>
  group_by(Group_Time_Period) |>
  summarise(mean = weighted.mean( x= `crude_rate`, w = population),
            st.err= wtd.stderror( x=`crude_rate`,weights = population),
            sd   = weighted.sd(x=`crude_rate`,w=population),
            n = n())
## Testing the difference between Group_Time_Period is the same is Pre_Post_Period-
## SO WE DIDNT NEED TO DO THIS. Ofc they are different look at definitions. 
## The Treated States definition
##Masterdata1998 %>%
#  mutate(Group_Time_Period= if_else(State %in% NotTreatedStates1998 & Year.x >= 1998,
#                                    "NoTreatPost-Period","NoTreatPre-Period")) %>%
#  mutate(Group_Time_Period2= if_else(State %in% TreatedStates1998 & Year.x >= 1998,
#                                    "TreatPost-Period","TreatPre-Period")) %>%
#  select(State,Year.x,Pre_Post_Parity,Group_Time_Period,AccesstoParity,Group_Time_Period2)%>%
#  group_by(Group_Time_Period) %>%
#  write_excel_csv(file="SumstatsCheckingData.csv")

## !!!!!!!!!!!!!!!!!!!!!!!!!!!
kable(TestFigure1,format = "latex",digits = 4,
      caption = "Weighted Means of Treated States, Pre and Post Period",
      booktabs = T
      )
kable(TestFigure2,format = "latex",digits = 4,
      caption = "Weighted Means of Non-Treated States, Pre and Post Period",
      booktabs = T)
## Testing kable god help me
kable(TestFigure1,digits =4,
      caption = "Weighted Means of Treated States, Pre and Post Period")
## let's create the diff in diff table 3 

## This is me trying to use regression as a way to run a ttest for myself
## 3/26/24- Please use Overleaf and Check if Treat is good. Table 3 looks good but need work
## TABLE 3 DONE 
Table3crude <- Masterdata1998 |>
mutate(TreatPrePostPeriod1998= if_else(State %in% TreatedStates1998 & Year.x >= 1998,"Post-Period",
                                       if_else(State %in% TreatedStates1998 & Year.x < 1998,"Pre-Period",
                                               if_else(State %in% NotTreatedStates1998 & Year.x >= 1998,"NoTreatPost-Period","NoTreatPre-Period"))))|>
  group_by(TreatPrePostPeriod1998)|>
  summarise(mean = as.numeric(weighted.mean( x= `crude_rate`, w = population)),
            st.err= wtd.stderror( x=`crude_rate`,weights = population),
            sd   = weighted.sd(x=`crude_rate`,w=population),
            n = n())
## Log Suicide Rate
Table3log <- Masterdata1998 |>
  mutate(TreatPrePostPeriod1998= if_else(State %in% TreatedStates1998 & Year.x >= 1998,"Post-Period",
                                         if_else(State %in% TreatedStates1998 & Year.x <= 1998,"Pre-Period",
                                                 if_else(State %in% NotTreatedStates1998 & Year.x >= 1998,"NoTreatPost-Period","NoTreatPre-Period"))))|>
  group_by(TreatPrePostPeriod1998)|>
  summarise(mean = as.numeric(weighted.mean( x= lcruderate, w = population)),
            st.err= wtd.stderror( x=lcruderate,weights = population),
            sd   = weighted.sd(x=lcruderate,w=population),
            n = n())





## here we create the figures for the differences between means 
## OKay Here's what I've done so far with Table 3. There's a Overleaf table that I'm working on
## 
## Redfine what it means here to be Treated and post
Masterdata1998 <- Masterdata1998|> mutate(
  Expr=if_else(State %in% TreatedStates1998 & Year.x >= 1998,"Treated-Post-Period",
                           if_else(State %in% TreatedStates1998 & Year.x < 1998,"Treated-Pre-Period",
                           if_else(State %in% NotTreatedStates1998 & Year.x >= 1998,"No-TreatPost-Period","No-TreatPre-Period")))) 
  
Masterdata1998 <- fastDummies::dummy_cols(Masterdata1998,select_columns = "Expr") 

Masterdata1998 <- Masterdata1998 |>
  mutate(Treat=if_else(State %in% TreatedStates1998 & Year.x >= 1998,1,
                       if_else(State %in% TreatedStates1998 & Year.x < 1998,1,
                               if_else(State %in% NotTreatedStates1998 & Year.x >= 1998,0,0)))) |>
  mutate(Post = ifelse(Year.x >= 1998,1,0)) |>
  mutate(TreatPost = Treat * Post)


ttestlm <- lm(crude_rate ~ Masterdata1998$Treat+
              Masterdata1998$Post+
              Masterdata1998$TreatPost,
              weights = Masterdata1998$population,
              data = Masterdata1998)

summary(ttestlm)

logttestlm <- lm(lcruderate ~ Masterdata1998$Treat+
                   Masterdata1998$Post+
                   Masterdata1998$TreatPost,
                 weights = Masterdata1998$population,
                 data = Masterdata1998)
summary(logttestlm)

## In order to get all Difference estimates you have to take away one from each group for a total of three regressions, and that coef is the diference

## Create Tables
kable(Table3crude,format = "latex",digits = 4,
      caption = "Weighted Mean Suicide Rates of Treated and Nontreated States, Pre and Post Period",
      booktabs = T
)

kable(Table3log,format = "latex",digits = 4,
      caption = "Weighted Mean Log Suicide Rates of Treated and Nontreated States, Pre and Post Period",
      booktabs = T
)
## I wanna try and add columns to this tibble

  

## We're going to use gt summary here to create good tables. Just using Kable is ugly and I wanna cry
## I gave up on this, idk why it wont work
#library(gtsummary)

#summaryfunc <- function(data,variable,...) {
  
#  weightedmean <- weighted.mean(data[[paste0(variable)]], Masterdata1998$population, na.rm =T)
#  st.err <-     wtd.stderror(data[[paste0(variable)]],    Masterdata1998$population)
#  weightedsd <-  weighted.sd(data[[paste0(variable)]],    Masterdata1998$population, na.rm =T)
  
#  dplyr::tibble(
 #   weightedmean = weightedmean,
#    st.err= st.err,
#    weightedsd = weightedsd
 # )
#}


#Masterdata1998 |>
#  rename(unemployment_rate =`unemployment rate`) |>
#  select(crude_rate,unemployment_rate,BankrupcyP100k,PercW,Pre_Post_Parity) |>
#  tbl_custom_summary(by= "Pre_Post_Parity",
 #             stat_fns = ~ summaryfunc,
 #             statistic = ~ "{weightedmean} {st.err} {weightedsd}"
#              )


## Idk what this is / I think this was check what states were treated in the data
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
  summarise(weighted.mean(x=Masterdata1998$`crude_rate`, w= Masterdata1998$population)) |>
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
##lapply(Masterdata1998[,41:47], sum)
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
                  method = "nearest", distance = "glm", replace = T)
summary(m.out1, un =FALSE)
plot(m.out1)

plot(m.out1, type = "density", interactive = FALSE,
     which.xs = ~`unemployment rate` 
     + `BankrupcyP100k` 
     + `PercW`)

Matchedplot1 <-plot(summary(m.out1),var.order = "unmatched",abs = F, main= "Nearest Neighbor")

## Nearest Neighbhor with Probit
m.out1.2 <- matchit(D_AccessToParity ~ `unemployment rate` 
                  + `BankrupcyP100k` 
                  + `PercW`, data = Masterdata1998,
                  method = "nearest", link = "probit", replace = T)
summary(m.out1.2, un =FALSE)

Matchedplot2 <-plot(summary(m.out1.2),var.order = "unmatched",abs = F,main= "Nearest Neighbor using Probit")

## Quick check with this.
m.data <- match.data(m.out1.2)

MatchedDid0 = feols(lcruderate ~  `TreatPost`
                    +`unemployment rate` 
                    +`BankrupcyP100k`
                    +`PercW`| State + Year.x, m.data, weights = ~population )

summary(MatchedDid0, cluster = "State")
## Mahalaobis
m.out1.3 <- matchit(D_AccessToParity ~ `unemployment rate` 
                    + `BankrupcyP100k` 
                    + `PercW`, data = Masterdata1998,
                    method = "nearest", distance  = "glm",
                    mahvars = ~ `unemployment rate`+ `BankrupcyP100k`+ `PercW`
                    ,replace = T)
summary(m.out1.3, un =FALSE)

Matchedplot3 <- plot(summary(m.out1.3),var.order = "unmatched",abs = F,main= "Mahalanobis matching with Nearest Neighbor computing Propensity Score")
## Mahal- ATT Estimate
m.data <- match.data(m.out1.3)

library("marginaleffects")
library("modelsummary")

fit <- lm(lcruderate ~ D_AccessToParity * (`unemployment rate` 
                                           + `BankrupcyP100k` 
                                           + `PercW`), data = m.data)

MahalMatchResult <-avg_comparisons(fit,
                variables = "D_AccessToParity",
                vcov = "stata",
                newdata = subset(m.data, D_AccessToParity == 1),
                wts = "population")
#Mahalmodelsave<- modelsummary(MahalMatchResult,output = "testtable.tex")
#Mahalmodelsave %>% kable_styling(latex_options = "striped",full_width = T)
  
## Mala- Let's use it in a twfe
MatchedDid = feols(`crude_rate` ~  `TreatPost`
                        +`unemployment rate` 
                        +`BankrupcyP100k`
                        +`PercW`| State + Year.x, m.data, weights = ~population )

summary(MatchedDid, cluster = "State")
#lcrude
MatchedDid2 = feols(lcruderate ~  `TreatPost`
                   +`unemployment rate` 
                   +`BankrupcyP100k`
                   +`PercW`| State + Year.x, m.data, weights = ~population )

summary(MatchedDid2, cluster = "State")

## Probit
m.out2 <- matchit(D_AccessToParity ~ `unemployment rate` 
                  + `BankrupcyP100k` 
                  + `PercW`, data = Masterdata1998,
                  method = "full",link = "probit", estimand = "ATE")
m.out2
summary(m.out2, un=F)
Matchedplot4 <- plot(summary(m.out2),var.order = "unmatched",abs = F,main= "Full Matching Using Probit")
## We try to get the ATE 
m.data <- match.data(m.out2)

library("marginaleffects")

fit <- lm(lcruderate ~ D_AccessToParity * (`unemployment rate` 
                                           + `BankrupcyP100k` 
                                           + `PercW`), data = m.data)

ProbitMatch <- avg_comparisons(fit,
                variables = "D_AccessToParity",
                vcov = ~subclass,
                newdata = subset(m.data, D_AccessToParity == 1),
                wts = "population")
#modelsummary(list(ProbitMatch,Mahalmodelsave),output = "testtable2.tex")

# Simple Did With the Probit
MatchedDid3 = feols(`crude_rate` ~  `TreatPost`
                   +`unemployment rate` 
                   +`BankrupcyP100k`
                   +`PercW`| State + Year.x, m.data, weights = ~population )

summary(MatchedDid3, cluster = "State")
#lcruderate
MatchedDid4 = feols(`lcruderate` ~  `TreatPost`
                    +`unemployment rate` 
                    +`BankrupcyP100k`
                    +`PercW`| State + Year.x, m.data, weights = ~population )

summary(MatchedDid4, cluster = "State")
## FULL PS w/Matching DID

## True PS matching 
m.out3 <- matchit(D_AccessToParity ~ `unemployment rate` 
                  + `BankrupcyP100k` 
                  + `PercW`, data = Masterdata1998,
                  method = "full", distance = "glm",estimand = "ATE")
m.out3
summary(m.out3, un=F)
Matchedplot5 <- plot(summary(m.out3),var.order = "unmatched",abs = F,main = "Full Matching using GLM")

## Introducing Sampling Weight is chaos. Going to try this to get robust clustered standerrs
## Now the Did
# Simple Did With the Probit
m.data <- match.data(m.out3)
MatchedDid5 = feols(`crude_rate` ~  `TreatPost`
                    +`unemployment rate` 
                    +`BankrupcyP100k`
                    +`PercW`| State + Year.x, m.data, weights = ~population )

summary(MatchedDid5, cluster = "State")
#lcruderate
MatchedDid6 = feols(`lcruderate` ~  `TreatPost`
                    +`unemployment rate` 
                    +`BankrupcyP100k`
                    +`PercW`| State + Year.x, m.data, weights = ~population )

summary(MatchedDid6, cluster = "State")
## We're going to try cardinality
m.out4 <- matchit(D_AccessToParity ~ `unemployment rate` 
                  + `BankrupcyP100k` 
                  + `PercW`, data = Masterdata1998,
                  method = "cardinality", distance = "glm",estimand = "ATE")
m.out4
summary(m.out4, un=F)
Matchedplot6 <- plot(summary(m.out4),var.order = "unmatched",abs = F,main = "Full matching using Cardinality")

## Did
m.data <- match.data(m.out4)
MatchedDid7 = feols(`crude_rate` ~  `TreatPost`
                    +`unemployment rate` 
                    +`BankrupcyP100k`
                    +`PercW`| State + Year.x, m.data, weights = ~population )

summary(MatchedDid7, cluster = "State")
#lcruderate
MatchedDid8 = feols(`lcruderate` ~  `TreatPost`
                    +`unemployment rate` 
                    +`BankrupcyP100k`
                    +`PercW`| State + Year.x, m.data, weights = ~population )

summary(MatchedDid8, cluster = "State")


## Let's try Coarsened exact matching
m.out5 <- matchit(D_AccessToParity ~ `unemployment rate` 
                  + `BankrupcyP100k` 
                  + `PercW`, data = Masterdata1998,
                  method = "cem", distance = "glm",estimand = "ATE")
m.out5
summary(m.out5, un=F)
Matchedplot7 <-plot(summary(m.out5),var.order = "unmatched",abs = F,main="Coarse-Exact Matching")

## Did
m.data <- match.data(m.out5)
MatchedDid9 = feols(`crude_rate` ~  `TreatPost`
                    +`unemployment rate` 
                    +`BankrupcyP100k`
                    +`PercW`| State + Year.x, m.data, weights = ~population )

summary(MatchedDid9, cluster = "State")
#lcruderate
MatchedDid10 = feols(`lcruderate` ~  `TreatPost`
                    +`unemployment rate` 
                    +`BankrupcyP100k`
                    +`PercW`| State + Year.x, m.data, weights = ~population )

summary(MatchedDid10, cluster = "State")
## Let's try subclass
m.out6 <- matchit(D_AccessToParity ~ `unemployment rate` 
                  + `BankrupcyP100k` 
                  + `PercW`, data = Masterdata1998,
                  method = "subclass", distance = "glm",estimand = "ATE")
m.out6
summary(m.out6, un=F)
Matchedplot8 <-plot(summary(m.out6),var.order = "unmatched",abs = F,main= "Subclass Matching")

# Did
m.data <- match.data(m.out6)
MatchedDid11 = feols(`crude_rate` ~  `TreatPost`
                    +`unemployment rate` 
                    +`BankrupcyP100k`
                    +`PercW`| State + Year.x, m.data, weights = ~population )

summary(MatchedDid11, cluster = "State")
#lcruderate
MatchedDid12 = feols(`lcruderate` ~  `TreatPost`
                     +`unemployment rate` 
                     +`BankrupcyP100k`
                     +`PercW`| State + Year.x, m.data, weights = ~population )
summary(MatchedDid12, cluster = "State")
## We pull together fixest results
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
                        +`D_NoLaw`
                        +`PercW`| State + Year.x, Masterdata1998, weights = Masterdata1998$population )
summary(fixestmodelcol3, cluster = "State")
fixestmodelcol4 = feols(lcruderate ~ `D_AccessToParity`
                        +`D_MandatedifOffered`
                        +`D_NoLaw`
                        +`unemployment rate` 
                        +`BankrupcyP100k`
                        +`PercW`| State + Year.x, Masterdata1998, weights = Masterdata1998$population )
summary(fixestmodelcol4, cluster = "State")

Masterdata1998 <- Masterdata1998 |>
  group_by(State) |>
  mutate(TimingCntrl = dplyr::lag(lcruderate,n=1,default = NULL))|>
  mutate(lDelta = lcruderate- TimingCntrl) 

fixestmodel1stDifcol1 = feols(`lDelta` ~ `D_AccessToParity`
                        +`D_NonParityLaw`
                        +`unemployment rate` 
                        +`BankrupcyP100k`
                        +`PercW`| State + Year.x, Masterdata1998, weights = Masterdata1998$population )
summary(fixestmodel1stDifcol1, cluster = "State")

Firstdifcol2data <- Masterdata1998 |>
  filter( AccesstoParity > 1 ) 


fixestmodel1stDifcol2 = feols(`lDelta` ~ TreatPost
                              +`unemployment rate` 
                              +`BankrupcyP100k`
                              +`PercW`| State + Year.x, Firstdifcol2data, weights = Firstdifcol2data$population )

summary(fixestmodel1stDifcol2, cluster = "State")
## REDO  FIRST DIFFERENCE TABLE, DIDNT DO IT RIGHT NEED TO USE PLM
PanelMasterdata <- plm::pdata.frame(Masterdata1998)
class(PanelMasterdata)
PanelMasterdata <- subset(PanelMasterdata, select = -c(TimingCntrl
                                                       ,Delta
                                                       ,lDelta))
PanelMasterdata <- na.omit(PanelMasterdata)
PanelMasterdata <- plm::pdata.frame(PanelMasterdata,index = c("State","Year.x"))
index(PanelMasterdata)
PanelMasterdata$population <- as.numeric(PanelMasterdata$population)
# converting it into a plm frame cause something to happen
firstdifdata <- subset(Masterdata1998,select = -c(TimingCntrl
                                                  ,Delta
                                                  ,lDelta))
firstdifplm <- plm(lcruderate ~ D_AccessToParity
     +D_NonParityLaw
     +unemployment.rate
      +BankrupcyP100k
      +PercW,
     index = c("State","Year.x"),
    data = firstdifdata,
model = "fd")
summary(firstdifplm)
# The weight isn't the same size as the fc coefficents because the regression is omitting the first
# year 1990 from all states in the dataset. the weight needs to do the same
FirstdifWeight<-firstdifdata$population
FirstdifWeight <- as.data.frame(FirstdifWeight)
FirstdifWeight <- FirstdifWeight %>%
  slice(-c(seq(1,765,15)))

firstdifdata <- firstdifdata %>%
  filter( Year.x !="1990")


FirstdifWeight2 <- as.numeric(FirstdifWeight$FirstdifWeight)
firstdifplm <- plm(lcruderate ~ D_AccessToParity
                   +D_NonParityLaw
                   +unemployment.rate
                   +BankrupcyP100k
                   +PercW,
                   index = c("State","Year.x"),
                   data = firstdifdata,
                   model = "fd",
                   weights = `population`)

library(foreign)
Masterdata1998 %>%
  rename(`unemployment_rate`=`unemployment rate`) %>%
write.dta(file = "Masterdata1998STATA.dta")

#summary(firstdifplm)
# CANT FIX IDK WHY RESORTING TO STATA- EDIT STATA IS EATING MY DATA FOUND ANOTHER PACKAGE
library(lfe)
library(wfe)
library(sandwich)
library(writexl)


Firstdiffreg1 <- wfe::wfe(lcruderate ~  D_AccessToParity
        + D_NonParityLaw
       +`unemployment rate`
         +PercW
       + BankrupcyP100k,
        treat = "D_AccessToParity",
        unit.index = "State",
       time.index = "Year.x", 
      data = Masterdata1998,
        C.it = "population",
       method = "unit",
      qoi = "ate",
      auto.se = T,
        estimator = "did")


summary(Firstdiffreg1)

 #Masterdata1998 |>
#   select(where(is.numeric))|>
#   names()


#Masterdata1998$population <- as.numeric(Masterdata1998$population)
#Firstdiffreg1 <- lfe::felm(lcruderate ~ D_AccessToParity
#                       +D_NonParityLaw
#                       +`unemployment rate`
#                       +BankrupcyP100k
#                       +PercW| FIPS + Year.x,
 #                     weights = population,
 #                      data = Masterdata1998)

#class(Masterdata1998$population)
# Trying to export shit
write_xlsx(Masterdata1998, path = "Masterdata1998.xlsx")
write_delim(Masterdata1998,file = "Masterdata1998.csv")
write.csv2(Masterdata1998,file = "Masterdata1998.csv")
write_excel_csv2(Masterdata1998,file = "Masterdata1998.csv2")
write.table(Masterdata1998, "Masterdata1998.txt", sep=";")
Masterdata1998 |>
  select(-1)|>
  write_excel_csv2(file = "Masterdata1998.csv")

## BRUH WE NEED TABLES ON TABLES ON TABLES
setFixest_dict(c(`crude_rate` ="Crude Suicide Rate",
                 `lcruderate`= "Log Suicide Rate",
                 `unemployment rate` = "Unemployment Rate",
                 `BankrupcyP100k`= "Bankruptcy Per 100,000",
                 `PercW` = "Percent Working in Large Firms"))
etable(feols(`crude_rate`~`unemployment rate` 
             +`BankrupcyP100k`
             +`PercW`
             + csw0(`D_AccessToParity`,
                    `D_ParityLawPassed`,
                    `D_Mandatedofferinglaws`,
                    `D_MandatedifOffered`,
                    `D_NonParityLaw`)|State + Year.x, Masterdata1998, 
             weights = Masterdata1998$population,
             cluster = "State"),file ="FETablesResults.tex" )
etable(feols(`lcruderate`~`unemployment rate` 
             +`BankrupcyP100k`
             +`PercW`
             + csw0(`D_AccessToParity`,
                    `D_ParityLawPassed`,
                    `D_Mandatedofferinglaws`,
                    `D_MandatedifOffered`,
                    `D_NonParityLaw`)|State + Year.x, Masterdata1998, 
             weights = Masterdata1998$population,
             cluster = "State"),file ="FETablesResults2.tex" )
## Let's create the table columns- TABLE 4
etable(fixestmodelcol1,fixestmodelcol2,fixestmodelcol3,fixestmodelcol4,
       file = "FETable4Results.tex",
       title = "Wowthis is a title")

## We run Matched regressions now
 paste0("MatchedDid",1:12)
 MatchedDidList <- paste0("MatchedDid",1:12)
 #etable(MatchedDidList)
 #MatchedDidList <- c(MatchedDid,MatchedDid2,MatchedDid3,MatchedDid4,MatchedDid5,MatchedDid6
 #,MatchedDid7,MatchedDid8,MatchedDid9,MatchedDid10,MatchedDid11,MatchedDid12)
 #Matchlist2 <- matrix(MatchedDid,MatchedDid2)
etable(MatchedDid,MatchedDid2,MatchedDid3,MatchedDid4,file = "BigMatchTable1.tex",digits = 3)
etable(MatchedDid5,MatchedDid6,MatchedDid7,MatchedDid8,file = "BigMatchTable2.tex",digits = 3)
etable(MatchedDid9,MatchedDid10,MatchedDid11,MatchedDid12,file = "BigMatchTable3.tex",digits = 3)
 ## TABLE 5 DONE IN STATA
## We pull together the plots for Matching
#library(patchwork)
#par(mfcol=c(2,2))
#plot(summary(m.out1),var.order = "unmatched",abs = F, main= "Nearest Neighbor") 
#plot(summary(m.out1.2),var.order = "unmatched",abs = F,main= "Nearest Neighbor using Probit") 
#plot(summary(m.out1.3),var.order = "unmatched",abs = F,main= "Mahalanobis matching Using Nearest Neighbor",legend("topright")) 
#plot(summary(m.out2),var.order = "unmatched",abs = F,main= "Full Matching Using Probit")


## Let's run a simple Did from Fixest

fixestmodelDid1 = feols(`crude_rate` ~  `TreatPost`
                        +`unemployment rate` 
                        +`BankrupcyP100k`
                        +`PercW`| State + Year.x, Masterdata1998, weights = Masterdata1998$population )

summary(fixestmodelDid1, cluster = "State")

fixestmodelDid2 = feols(lcruderate ~TreatPost
                        +`unemployment rate` 
                        +`BankrupcyP100k`
                        +`PercW`| State + Year.x, Masterdata1998, weights = Masterdata1998$population )
summary(fixestmodelDid2, cluster = "State")

fixestmodelDid3 = feols(`crude_rate` ~  `D_AccessToParity`
                        +`unemployment rate` 
                        +`BankrupcyP100k`
                        +`PercW`| State + Year.x, Masterdata1998, weights = Masterdata1998$population )

summary(fixestmodelDid3, cluster = "State")

fixestmodelDid3.1 = feols(`crude_rate` ~  `D_AccessToParity`
                        | State + Year.x, Masterdata1998, weights = Masterdata1998$population )

summary(fixestmodelDid3.1, cluster = "State")

fixestmodelDid4 = feols(lcruderate ~D_AccessToParity
                        +`unemployment rate` 
                        +`BankrupcyP100k`
                        +`PercW`| State + Year.x, Masterdata1998, weights = Masterdata1998$population )
summary(fixestmodelDid4, cluster = "State")

fixestmodelDid4.1 = feols(lcruderate ~D_AccessToParity
                        | State + Year.x, Masterdata1998, weights = Masterdata1998$population )
summary(fixestmodelDid4.1, cluster = "State")



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
                   ref.p = c(1))
                   | State + Year.x,
                   Masterdata1998, 
                   weights = Masterdata1998$population)
# Testing on Sunab
SunAbTest <- feols(lcruderate ~ `unemployment rate`
      +`BankrupcyP100k`
      +`PercW`
      + sunab(`AccesstoParity`,Year.x,
              ref.p = c(-1))
      | State + Year.x,
      Masterdata1998, 
      weights = Masterdata1998$population)
SunAbTest2 <- feols(lcruderate ~ `unemployment rate`
                   +`BankrupcyP100k`
                   +`PercW`
                   + sunab(`AccesstoParity`,Year.x,
                           ref.p = c(-2))
                   | State + Year.x,
                   Masterdata1998, 
                   weights = Masterdata1998$population)
SunAbTest3 <- feols(lcruderate ~ `unemployment rate`
                    +`BankrupcyP100k`
                    +`PercW`
                    + sunab(`AccesstoParity`,Year.x,
                            ref.p = c(.F,-2))
                    | State + Year.x,
                    Masterdata1998, 
                    weights = Masterdata1998$population)
SunAbTest4 <- feols(lcruderate ~ `unemployment rate`
                   +`BankrupcyP100k`
                   +`PercW`
                   + sunab(`AccesstoParity`,Year.x,
                           ref.p = c(.F,-1))
                   | State + Year.x,
                   Masterdata1998, 
                   weights = Masterdata1998$population)
summary(SunAbTest,agg = "ATT")
summary(SunAbTest,agg = "Cohort")
iplot(SunAbTest)
iplot(list(SunAbTest,SunAbTest2,SunAbTest3), main = "Sun and Abraham adjusted Fixed Effects")
legend("topright",col = 1:3, pch = 20, lwd = 1, lty = 1:2,cex = .75 ,
       legend = c("SA with Base period of -1",
                  "SA with Base period of -2",
                  "SA with First Year Dropped and BP of -2"))
## ALT RESULTS 
SunAbResult <- summary(SunAbTest,agg = "ATT")
SunAbResult2 <- summary(SunAbTest2, agg = "ATT")
SunAbResult3 <- summary(SunAbTest3, agg = "ATT")
SunAbResult4 <- summary(SunAbTest4, agg = "ATT")
etable(list(SunAbResult,SunAbResult2,SunAbResult3),
       file = "SunAbResultsNEW.tex",
       title = "Sun and Abraham Estimates")

#Creating a time to treated variable and creating a count of how 
#many are in that group
Masterdata1998 %>%
  mutate(TimetillTreat = ifelse((Year.x - AccesstoParity)>20,
                                NA,
                                Year.x - AccesstoParity)) %>%
  select(State,Year.x,AccesstoParity,TimetillTreat) %>%
  group_by(TimetillTreat) %>%
  summarise( count= n()) %>%
  print(n=51)

## What is this comparing?
iplot(list(Sunabmodel,Sunabmodelref))
SAplot <-iplot(list(Sunabmodel,Sunabmodelref), ref = "all")
legend("topright",col = 1:2, pch = 20, lwd = 1, lty = 1:2,legend = c("SA model", "SA with relative points" ))
SunAbamResults1 <- summary(Sunabmodel,agg = "ATT")
SunAbamResults2 <- summary(Sunabmodelref, agg = "ATT")

iplot(Sunabmodel,main= "Sun and Abraham Adjusted Fixed Effects")


## I export the Sun and Abraham results into a table
etable(SunAbamResults1,SunAbamResults2,
       file = "SunAbamResults.tex",
       title = "Table 11: Sun and Abraham Estimates")
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

# Check Group Size of EAch Group of Years that Treated 
 Masterdata1998 %>% 
select(AccesstoParity,State,Year.x)  %>% 
 group_by(AccesstoParity) %>% 
 summarise( count=n()) %>%
   print(n=51)
 ##
 #Conditonal Pre-trend test
 conditional_did_pretest(yname = "lcruderate",
        gname = "AccesstoParity",
        tname = "Year.x",
        idname = "FIPS",
        data = Masterdata1998,
        weightsname = "population",
        bstrap = T,
        control_group = "notyettreated",
        est_method = "reg"
        
 )
 
 ##

library(did)
 ## Regression without Covariates
out0 <- att_gt(yname = "lcruderate",
               gname = "AccesstoParity",
               tname = "Year.x",
               idname = "FIPS",
               data = Masterdata1998,
               weightsname = "population",
               bstrap = T,
               base_period = "universal",
               control_group = "notyettreated",
               print_details = T,
               est_method = "ipw"
)
summary(out0)
ggdid(out0)
agg.simple0 <- aggte(out0,type = "simple",na.rm = T)
agg.dynamic0 <- aggte(out0,type = "dynamic", na.rm =T)
agg.gs0 <- aggte(out0,type = "group",na.rm=T)
summary(agg.simple0)
#ggdid(agg.simple)
summary(agg.dynamic0)
summary(agg.gs0)
ggdid(agg.dynamic0)
## COVARIATE REGRESSION with simplfied groups
out <- att_gt(yname = "lcruderate",
              gname = "simple3did",
              tname = "Year.x",
              idname = "FIPS",
              xformla = ~ `unemployment rate`+ `BankrupcyP100k`  +  `PercW`,
              data = Masterdata1998,
              weightsname = "population",
              bstrap = T,
              base_period = "varying",
              control_group = "notyettreated",
              )
summary(out)
ggdid(out)
agg.simple <- aggte(out,type = "simple",na.rm = T)
agg.dynamic <- aggte(out,type = "dynamic", na.rm =T)
agg.gs <- aggte(out,type = "group",na.rm=T)
summary(agg.simple)
#ggdid(agg.simple)
summary(agg.dynamic)
summary(agg.gs)
ggdid(agg.dynamic)
## 
# Let's try something, Spilt the Access to Parity groups into groups. 
#  Ideally we would want to have something that can generate seperate models with each
Masterdata1998 %>%
  mutate(ParityLawGroups = ifelse( 1 == D_ParityLawPassed | 1 == D_MandatedifOffered,1,
                                   ifelse( 1==D_MandatedifOffered | 1== D_MinimumMandatedBenefit,2,0))) %>%
  select(ParityLawGroups,Year.x,State,AccesstoParity) %>%
  group_by(ParityLawGroups) %>%
  summarise(count=n()) 
Masterdata1998 <- Masterdata1998 %>%
  mutate(ParityLawGroups = ifelse( 1 == D_ParityLawPassed | 1 == D_MandatedifOffered,1,
                                   ifelse( 1==D_MandatedifOffered | 1== D_MinimumMandatedBenefit,2,0)))

outGroup <- att_gt(yname = "lcruderate",
                    gname = "ParityLawGroups",
                    tname = "Year.x",
                    idname = "FIPS",
                    xformla = ~ `unemployment rate`+ `BankrupcyP100k`  +  `PercW`,
                    data = Masterdata1998,
                    weightsname = "population",
                    bstrap = T,
                    base_period = "universal",
                    control_group = "notyettreated",
                    est_method = "reg",
                    anticipation = 0
                    
)

## REGULAR OUTCOME REGRESSION
out2 <- att_gt(yname = "lcruderate",
              gname = "AccesstoParity",
              tname = "Year.x",
              idname = "FIPS",
              xformla = ~ `unemployment rate`+ `BankrupcyP100k`  +  `PercW`,
              data = Masterdata1998,
              weightsname = "population",
              bstrap = T,
              base_period = "universal",
              control_group = "notyettreated",
              est_method = "reg",
              anticipation = 0
              
)
summary(out2)
ggdid(out2)
agg.simple2 <- aggte(out2,type = "simple",na.rm = T)
agg.dynamic2 <- aggte(out2,type = "dynamic", na.rm =F)
agg.gs2 <- aggte(out2,type = "group",na.rm=T)
agg.cal <- aggte(out2,type = "calendar",na.rm=T)
summary(agg.simple2)
#ggdid(agg.simple2)
summary(agg.dynamic2)
summary(agg.gs2)
summary(agg.cal)
ggdid(agg.gs2)
ggdid(agg.dynamic2)

## Try again with the CS Did
## Doubly Robust
out3 <- att_gt(yname = "lcruderate",
               gname = "AccesstoParity",
               tname = "Year.x",
               idname = "FIPS",
               xformla = ~ `unemployment rate`+ `BankrupcyP100k`  +  `PercW`,
               data = Masterdata1998,
               bstrap = T,
               base_period = "varying",
               control_group = "notyettreated"
               
               
)
testlm <- lm(lcruderate  ~ `unemployment rate`+ `BankrupcyP100k`  +  `PercW`+ `population`,Masterdata1998)
summary(testlm)
summary(out3)
ggdid(out3)
agg.simple3 <- aggte(out3,type = "simple",na.rm = T)
agg.dynamic3 <- aggte(out3,type = "dynamic", na.rm =T)
agg.gs3 <- aggte(out3,type = "group",na.rm=T)
summary(agg.simple3)
#ggdid(agg.simple2)
summary(agg.dynamic3)
summary(agg.gs3)
ggdid(agg.gs3)
ggdid(agg.dynamic3)

### Here is assigning results into tibbles where we turn those into kable tables. I want to get the IPW results
### And the Outcome Regression results. I will report that the doubly robust starts dropping pairs of DID
## New Table
CSCol1 <- att_gt(yname = "lcruderate",
               gname = "AccesstoParity",
               tname = "Year.x",
               idname = "FIPS",
               data = Masterdata1998,
               weightsname = "population",
               bstrap = T,
               base_period = "universal",
               control_group = "notyettreated",
               est_method = "reg"
               
)
CSCol2<- att_gt(yname = "lcruderate",
          gname = "AccesstoParity",
          tname = "Year.x",
          idname = "FIPS",
          data = Masterdata1998,
          weightsname = "population",
          bstrap = T,
          base_period = "universal",
          control_group = "notyettreated",
          est_method = "ipw"
          
)
CSCol3<- att_gt(yname = "lcruderate",
          gname = "AccesstoParity",
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
CSCol4<- att_gt(yname = "lcruderate",
          gname = "AccesstoParity",
          tname = "Year.x",
          idname = "FIPS",
          xformla = ~ `unemployment rate`+ `BankrupcyP100k`  +  `PercW`,
          data = Masterdata1998,
          weightsname = "population",
          bstrap = T,
          base_period = "universal",
          control_group = "notyettreated",
          est_method = "ipw"
          
)
CSCol5<- att_gt(yname = "lcruderate",
          gname = "AccesstoParity",
          tname = "Year.x",
          idname = "FIPS",
          xformla = ~ `unemployment rate`+ `BankrupcyP100k`  +  `PercW`,
          data = Masterdata1998,
          weightsname = "population",
          bstrap = T,
          base_period = "varying",
          control_group = "notyettreated",
          est_method = "reg"
          
)
CSCol6<- att_gt(yname = "lcruderate",
                gname = "AccesstoParity",
                tname = "Year.x",
                idname = "FIPS",
                xformla = ~ `unemployment rate`+ `BankrupcyP100k`  +  `PercW`,
                data = Masterdata1998,
                weightsname = "population",
                bstrap = T,
                base_period = "varying",
                control_group = "notyettreated",
                est_method = "ipw"
)
CSColNames <- list(paste("CSCol",1:6))
## Running into Werid Tidy Bug here. I will just tidy them manually
#map(CSColNames,tidy)
CSColTidylistSimple <- list(
tidy(aggte(CSCol1,type = "simple")),
#tidy(aggte(CSCol2,type = "simple")),
tidy(aggte(CSCol3,type = "simple")),
#tidy(aggte(CSCol4,type = "simple",na.rm = T)),
tidy(aggte(CSCol5,type = "simple",na.rm = T))
#tidy(aggte(CSCol6,type = "simple",na.rm = T))
)
CSColTidylistSimplerow <- rbind(
  tidy(aggte(CSCol1,type = "simple")),
  #tidy(aggte(CSCol2,type = "simple")),
  tidy(aggte(CSCol3,type = "simple")),
  #tidy(aggte(CSCol4,type = "simple",na.rm = T)),
  tidy(aggte(CSCol5,type = "simple",na.rm = T))
  #tidy(aggte(CSCol6,type = "simple",na.rm = T))
)

kable(CSColTidylistSimplerow[1:3,2:3], booktabs = T) %>%
  kable_styling() %>%
  pack_rows("Simple Group",1,3) 
# Commented out since I edited the csv file to include parathesis
#write.csv(CSColTidylistSimplerow[1:3,2:3],file = "CStidysimpleresults.csv")


kable(CSColTidylistSimple,booktabs = T) 
## Commented out are the reported IPW stuff
CSColTidylistSGroup <- list(
  tidy(aggte(CSCol1,type = "group")),
  #tidy(aggte(CSCol2,type = "group")),
  tidy(aggte(CSCol3,type = "group")),
  #tidy(aggte(CSCol4,type = "group",na.rm = T)),
  tidy(aggte(CSCol5,type = "group"))
  #tidy(aggte(CSCol6,type = "group",na.rm = T))
)
CSColTidylistSGrouprbind <- rbind(
  tidy(aggte(CSCol1,type = "group")),
  #tidy(aggte(CSCol2,type = "group")),
  tidy(aggte(CSCol3,type = "group")),
  #tidy(aggte(CSCol4,type = "group",na.rm = T)),
  tidy(aggte(CSCol5,type = "group"))
  #tidy(aggte(CSCol6,type = "group",na.rm = T))
)
CSColTidylistSGrouprbind %>%
  filter( grepl("Average",CSColTidylistSGrouprbind$term,perl  = T)) %>%
  select(1:5) %>%
  select(4,5) %>%
  kable(booktabs=T) %>%
  kable_styling() %>%
  pack_rows(group_label = "Group Averages",1,3)

CSColTidylistSGrouprbind %>%
  filter( grepl("Average",CSColTidylistSGrouprbind$term,perl  = T)) %>%
  select(1:5) %>%
  select(4,5)
#Commented out since I edited the file to include parathesis. 
#  write.csv(file = "CStidygroupresults.csv")

ggdid(aggte(CSCol1,type = "group"))
ggdid(aggte(CSCol3,type = "group"))
ggdid(aggte(CSCol5,type = "group"))

# Dynamic- First Two are Universal, The Third is Varying
# The Varying should be interperted as the ATT if the Policy Had been implmented
# At that time. 
ggdid(aggte(CSCol1,type = "dynamic"))
#ggdid(aggte(CSCol2,type = "dynamic"))
ggdid(aggte(CSCol3,type = "dynamic"))
#ggdid(aggte(CSCol4,type = "dynamic",na.rm = T))
ggdid(aggte(CSCol5,type = "dynamic"))
#ggdid(aggte(CSCol6,type = "dynamic",na.rm = T))



tidy(CSCol1)
tidy(CSCol2)
tidy(CSCol3)
tidy(CSCol4)
tidy(CSCol5)
tidy(CSCol6)

# Below is did troubleshoot with trimmer
# did reports different coefficents and sigficance if different methods used and different sets
# of observations 

trimmer(g = 2000  ,
        gname = "AccesstoParity",
        tname = "Year.x",
        idname = "FIPS",
        xformla = ~ `unemployment rate`+ `BankrupcyP100k`  +  `PercW`,
        data = Masterdata1998,
        control_group = "notyettreated",
        threshold = 0.0000001)
## Trimmer shows that I have a lot of obs that are close to 0 rather than 1. 
## Below is output out to stata because the stata offered a feature that it runs the regression
## seperately from the doubly robust part. We'll see how it goes. 

### Here is Me retriving estimates for a anticipation CS 
CSanticipation <- att_gt(yname = "lcruderate",
                gname = "AccesstoParity",
                tname = "Year.x",
                idname = "FIPS",
                xformla = ~ `unemployment rate`+ `BankrupcyP100k`  +  `PercW`,
                data = Masterdata1998,
                weightsname = "population",
                bstrap = T,
                base_period = "universal",
                control_group = "notyettreated",
                est_method = "reg",
                anticipation = -1
                
)
summary(CSanticipation)
aggte(CSanticipation, type = "simple")
aggte(CSanticipation, type = "group")
aggte(CSanticipation, type = "dynamic")

###
Masterdata1998 %>%
  select(FIPS,Year.x,lcruderate,D_AccessToParity,
         AccesstoParity,`unemployment rate`,
         BankrupcyP100k,PercW,population) %>%
  write_xlsx(path = "MasterdataSTATA.csv",col_names = T)

#below is bacon decomp


library(bacondecomp)
df_bacon <- bacon(lcruderate ~ TreatPost,
                  data = Masterdata1998,
                  id_var = "FIPS",
                  time_var = "Year.x")

df_bacon <- bacon(`lcruderate` ~ TreatPost
                  + `unemployment rate`
                  +  `BankrupcyP100k`  
                  +  `PercW`,
                  data = Masterdata1998,
                  id_var = "FIPS",
                  time_var = "Year.x",
                  
)
df_bacon <- bacon(lcruderate ~ D_AccessToParity,
                  data = Masterdata1998,
                  id_var = "FIPS",
                  time_var = "Year.x")



df_bacon <- bacon(lcruderate ~ D_AccessToParity,
                  data = Masterdata1998,
                  id_var = "FIPS",
                  time_var = "Year.x")
coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))
df_bacon |>
  filter(weight > .05)|>
  arrange(treated)

#df_bacon <- bacon(`lcruderate` ~ D_AccessToParity
 #                 + `unemployment rate`
 #                 +  `BankrupcyP100k`  
  #                +  `PercW`
  #                + population,
  #                data = Masterdata1998,
  #                id_var = "State",
  #                time_var = "Year.x",)

df_bacon <- bacon(`lcruderate` ~ D_AccessToParity
                + `unemployment rate`
                 +  `BankrupcyP100k`  
                 +  `PercW`,
                 data = Masterdata1998,
                 id_var = "State",
                 time_var = "Year.x",)
coef_bacon <- sum(df_bacon$two_by_twos$estimate * df_bacon$two_by_twos$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))

ggplot(df_bacon$two_by_twos) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  geom_point() +
  geom_hline(yintercept = 0) + 
  theme_minimal() +
  labs(x = "Weight", y = "Estimate", shape = "Type")
##  This is for a without controls
# ggplot(df_bacon) +
#  aes(x = weight, y = estimate, shape = factor(type)) +
#  geom_point() +
#  geom_hline(yintercept = 0) + 
#  theme_minimal() +
 # labs(x = "Weight", y = "Estimate", shape = "Type")
# Install & load ggpmisc
library("ggpmisc")


GoodmannBacon1 <-df_bacon$two_by_twos %>% 
  mutate(subgroup = paste0(treated, "_", untreated),
         subgroup = factor(subgroup),
         subgroup = forcats::fct_reorder(subgroup, estimate)) %>% 
  ggplot(aes(x = estimate, 
             y = subgroup,
             size = weight)) +
  geom_point() +
  geom_vline(xintercept = weighted.mean(df_bacon$two_by_twos$estimate, df_bacon$two_by_twos$weight),
             linetype = "longdash") +
  theme_minimal() +
  labs(size = "Weight",
       y = "Subgroup",
       x = "Estimate",
       title = "Goodman-Bacon diff in diff decomposition",
       subtitle = "Dotted line indicates two-way FE estimate.",
       caption = "Subgroups 99999 correspond to never treated groups") +
  annotate(geom = "table",
           x= 0,
           y= 0,
           label = list(bacontable))

#GoodmannBacon1 <-df_bacon %>% 
#  mutate(subgroup = paste0(treated, "_", untreated),
#         subgroup = factor(subgroup),
 #        subgroup = forcats::fct_reorder(subgroup, estimate)) %>% 
  #ggplot(aes(x = estimate, 
   #          y = subgroup,
    #         size = weight)) +
#  geom_point() +
 # geom_vline(xintercept = weighted.mean(df_bacon$estimate, df_bacon$weight),
  #           linetype = "longdash") +
#  theme_minimal() +
#  labs(size = "Weight",
 #      y = "Subgroup",
 #      x = "Estimate",
  #     title = "Goodman-Bacon diff in diff decomposition",
#     subtitle = "Dotted line indicates two-way FE estimate.",
#       caption = "Subgroups 99999 correspond to never treated groups")
#


ggsave(plot = last_plot(),filename = "Goodman-Bacon-Diff-in-Diff.png")
 #      units = "px",dpi = 300)
#png(GoodmannBacon1,filename = "Goodman-Bacon-Diff-in-Diff.png",
 #   width= 480,height=480, units = "px")

Masterdata1998 %>%
  select(Year.x,State,TreatPost,D_AccessToParity) %>%
  print(n=100)

bacontable <- data.frame(type = c("Both Treated","Treated vs Untreated"),
                         weight = c(0.2916,.7084),
                         avg_est = c(.0050,-.0211))
