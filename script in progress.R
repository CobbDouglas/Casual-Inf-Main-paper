library(tidyverse)

library(readr)
Compressed_Mortality_1999_2016_4_ <- read_delim("Compressed Mortality, 1999-2016 (4).txt", 
                                                  +     "\t", escape_double = FALSE, col_types = cols(Notes = col_skip()), 
                                                  +     trim_ws = TRUE)
 View(Compressed_Mortality_1999_2016_4_)
 
  library(readr)
  Compressed_Mortality_1979_1998_2_ <- read_delim("Compressed Mortality, 1979-1998 (2).txt", 
                                                   +     "\t", escape_double = FALSE, col_types = cols(Notes = col_skip()), 
                                                   +     trim_ws = TRUE)
  

thirdtry <- Compressed_Mortality_1979_1998_2_ %>%
  full_join(Compressed_Mortality_1999_2016_4_,by = c("State"="State",
                                                     "Year"="Year",
                                                     "State Code"="State Code",
                                                     "Year Code"="Year Code",
                                                     "Deaths"="Deaths",
                                                     "Population"="Population",
                                                     "Crude Rate"="Crude Rate"))

thirdtry <- na.omit(thirdtry)
thirdtry <- thirdtry %>%
  arrange(State)

write.csv(thirdtry,file = "Base suicide data.csv")

SuicideData <- read.csv("Base suicide data.csv")

## Import UE data
library(readxl)
Alabama <- read_excel("BLS data/Alabama.xlsx", 
                      skip = 10)
Alaska <- read_excel("BLS data/Alaska.xlsx", 
                     skip = 10)
Arizona <- read_excel("BLS data/Arizona.xlsx", 
                      skip = 10)
Arkansas <- read_excel("BLS data/Arkansas.xlsx", 
                       skip = 10)
California <- read_excel("BLS data/California.xlsx", 
                         skip = 10)
Colorado <- read_excel("BLS data/Colorado.xlsx", 
                       skip = 10)
Connecticut <- read_excel("BLS data/Connecticut.xlsx", 
                          skip = 10)
Delaware <- read_excel("BLS data/Delaware.xlsx", 
                       skip = 10)
District_of_Columbia <- read_excel("BLS data/District of Columbia.xlsx", 
                                   skip = 10)
Florida <- read_excel("BLS data/Florida.xlsx", 
                      skip = 10)
Georgia <- read_excel("BLS data/Georgia.xlsx", 
                      skip = 10)
Hawaii <- read_excel("BLS data/Hawaii.xlsx", 
                     skip = 10)
Idaho <- read_excel("BLS data/Idaho.xlsx", 
                    skip = 10)
Illinois <- read_excel("BLS data/Illinois.xlsx", 
                       skip = 10)
Indiana <- read_excel("BLS data/Indiana.xlsx", 
                      skip = 10)
Iowa <- read_excel("BLS data/Iowa.xlsx", 
                   skip = 10)
Kansas <- read_excel("BLS data/Kansas.xlsx", 
                     skip = 10)
Kentucky <- read_excel("BLS data/Kentucky.xlsx", 
                       skip = 10)
Louisiana <- read_excel("BLS data/Louisiana.xlsx", 
                        skip = 10)
Maine <- read_excel("BLS data/Maine.xlsx", 
                    skip = 10)
Maryland <- read_excel("BLS data/Maryland.xlsx", 
                       skip = 10)
Massachusetts <- read_excel("BLS data/Massachusetts.xlsx", 
                            skip = 10)
Michigan <- read_excel("BLS data/Michigan.xlsx", 
                       skip = 10)
Minnesota <- read_excel("BLS data/Minnesota.xlsx", 
                        skip = 10)
Mississippi <- read_excel("BLS data/Mississippi.xlsx", 
                          skip = 10)
Missouri <- read_excel("BLS data/Missouri.xlsx", 
                       skip = 10)
Montana <- read_excel("BLS data/Montana.xlsx", 
                      skip = 10)
Nebraska <- read_excel("BLS data/Nebraska.xlsx", 
                       skip = 10)
Nevada <- read_excel("BLS data/Nevada.xlsx", 
                     skip = 10)
New_Hampshire <- read_excel("BLS data/New Hampshire.xlsx", 
                            skip = 10)
New_Jersey <- read_excel("BLS data/New Jersey.xlsx", 
                         skip = 10)
New_Mexico <- read_excel("BLS data/New Mexico.xlsx", 
                         skip = 10)
New_York <- read_excel("BLS data/New York.xlsx", 
                       skip = 10)
North_Carolina <- read_excel("BLS data/North Carolina.xlsx", 
                             skip = 10)
North_Dakota <- read_excel("BLS data/North Dakota.xlsx", 
                           skip = 10)
Ohio <- read_excel("BLS data/Ohio.xlsx", 
                   skip = 10)
Oklahoma <- read_excel("BLS data/Oklahoma.xlsx", 
                       skip = 10)
Oregon <- read_excel("BLS data/Oregon.xlsx", 
                     skip = 10)
Pennsylvania <- read_excel("BLS data/Pennsylvania.xlsx", 
                           skip = 10)
Rhode_Island <- read_excel("BLS data/Rhode Island.xlsx", 
                           skip = 10)
South_Carolina <- read_excel("BLS data/South Carolina.xlsx", 
                             skip = 10)
South_Dakota <- read_excel("BLS data/South Dakota.xlsx", 
                           skip = 10)
Tennessee <- read_excel("BLS data/Tennessee.xlsx", 
                        skip = 10)
Texas <- read_excel("BLS data/Texas.xlsx", 
                    skip = 10)
Utah <- read_excel("BLS data/Utah.xlsx", 
                   skip = 10)
Vermont <- read_excel("BLS data/Vermont.xlsx", 
                      skip = 10)
Virginia <- read_excel("BLS data/Virginia.xlsx", 
                       skip = 10)
Washington <- read_excel("BLS data/Washington.xlsx", 
                         skip = 10)
West_Virginia <- read_excel("BLS data/West Virginia.xlsx", 
                            skip = 10)
Wisconsin <- read_excel("BLS data/Wisconsin.xlsx", 
                        skip = 10)
Wyoming <- read_excel("BLS data/Wyoming.xlsx", 
                      skip = 10)


USList <-list(Alaska,
     Arizona,Arkansas,
     California,Colorado,
     Connecticut,Delaware,
     District_of_Columbia,Florida,
     Georgia,Hawaii,
     Idaho,Illinois,
     Indiana,Iowa,
     Kansas,Kentucky,
     Louisiana,Maine,
     Maryland,Massachusetts,
     Michigan,Missouri,
     Montana,Nebraska,
     Nevada,New_Hampshire,
     New_Jersey,New_Mexico,
     New_York,North_Carolina,
     North_Dakota,Ohio,
     Oklahoma,Oregon,
     Pennsylvania,Rhode_Island,
     South_Carolina,South_Dakota,
     Tennessee,Texas,
     Utah,Vermont,
     Virginia,Washington,
     West_Virginia,Wisconsin,
     Wyoming )
Alabama$State <- "Alabama"
  Alaska$State <- "Alaska"
  Arkansas$State <- "Arkansas"
  Arizona$State <- "Arizona"
  California$State <- "California"
  Colorado$State <- "Colorado"
  Connecticut$State <- "Connecticut"
  Delaware$State <- "Delaware"
  District_of_Columbia$State <- "District_of_Columbia"
  Florida$State <- "Florida"
  Georgia$State <- "Georgia"
  Hawaii$State <- "Hawaii"
  Idaho$State <- "Idaho"
  Illinois$State <- "Illinois"
  Indiana$State <- "Indiana"
  Iowa$State <- "Iowa"
  Kansas$State <- "Kansas"
  Kentucky$State <- "Kentucky"
  Louisiana$State <- "Louisiana"
  Maine$State<- "Maine"
  Maryland$State <- "Maryland"
  Massachusetts$State <- "Massachusetts"
  Michigan$State <- "Michigan"
  Minnesota$State <- "Minnesota"
  Mississippi$State <- "Mississippi"
  Missouri$State <- "Missouri"
  Montana$State <- "Montana"
  Nebraska$State <- "Nebraska"
  Nevada$State <- "Nevada"
  New_Hampshire$State <- "New_Hampshire"
  New_Jersey$State <- "New_Jersey"
  New_Mexico$State <- "New_Mexico"
  New_York$State <- "New_York"
  North_Carolina$State <- "North_Carolina"
  North_Dakota$State <- "North_Dakota"
  Ohio$State <- "Ohio"
  Oklahoma$State <- "Oklahoma"
  Oregon$State <- "Oregon"
  Pennsylvania$State <- "Pennsylvania"
  Rhode_Island$State <- "Rhode_Island"
  South_Carolina$State <- "South_Carolina"
  South_Dakota$State <- "South_Dakota"
  Tennessee$State <- "Tennessee"
  Texas$State <- "Texas"
  Utah$State <- "Utah"
  Vermont$State <- "Vermont"
  Virginia$State <- "Virginia"
  Washington$State <- "Washington"
  West_Virginia$State <- "West_Virginia"
  Wisconsin$State <- "Wisconsin"
  Wyoming$State <- "Wyoming"
 ## trying bindrows----THIS IS Backup incase I break stuff
  Alabama$State <- "Alabama"
  Alaska$State <- "Alaska"
  Arkansas$State <- "Arkansas"
  Arizona$State <- "Arizona"
  California$State <- "California"
  Colorado$State <- "Colorado"
  Connecticut$State <- "Connecticut"
  Delaware$State <- "Delaware"
  District_of_Columbia$State <- "District of Columbia"
  Florida$State <- "Florida"
  Georgia$State <- "Georgia"
  Hawaii$State <- "Hawaii"
  Idaho$State <- "Idaho"
  Illinois$State <- "Illinois"
  Indiana$State <- "Indiana"
  Iowa$State <- "Iowa"
  Kansas$State <- "Kansas"
  Kentucky$State <- "Kentucky"
  Louisiana$State <- "Louisiana"
  Maine$State<- "Maine"
  Maryland$State <- "Maryland"
  Massachusetts$State <- "Massachusetts"
  Michigan$State <- "Michigan"
  Minnesota$State <- "Minnesota"
  Mississippi$State <- "Mississippi"
  Missouri$State <- "Missouri"
  Montana$State <- "Montana"
  Nebraska$State <- "Nebraska"
  Nevada$State <- "Nevada"
  New_Hampshire$State <- "New Hampshire"
  New_Jersey$State <- "New Jersey"
  New_Mexico$State <- "New Mexico"
  New_York$State <- "New York"
  North_Carolina$State <- "North Carolina"
  North_Dakota$State <- "North Dakota"
  Ohio$State <- "Ohio"
  Oklahoma$State <- "Oklahoma"
  Oregon$State <- "Oregon"
  Pennsylvania$State <- "Pennsylvania"
  Rhode_Island$State <- "Rhode Island"
  South_Carolina$State <- "South Carolina"
  South_Dakota$State <- "South Dakota"
  Tennessee$State <- "Tennessee"
  Texas$State <- "Texas"
  Utah$State <- "Utah"
  Vermont$State <- "Vermont"
  Virginia$State <- "Virginia"
  Washington$State <- "Washington"
  West_Virginia$State <- "West Virginia"
  Wisconsin$State <- "Wisconsin"
  Wyoming$State <- "Wyoming"
##  data <- bind_rows(Alabama,Alaska) 
  
  ## let's try bindrows
  US <-bind_rows(Alabama,Alaska,
            Arizona,Arkansas,
            California,Colorado,
            Connecticut,Delaware,
            District_of_Columbia,Florida,
            Georgia,Hawaii,
            Idaho,Illinois,
            Indiana,Iowa,
            Kansas,Kentucky,
            Louisiana,Maine,
            Maryland,Massachusetts,
            Michigan,Minnesota,
            Mississippi,Missouri,
            Montana,Nebraska,
            Nevada,New_Hampshire,
            New_Jersey,New_Mexico,
            New_York,North_Carolina,
            North_Dakota,Ohio,
            Oklahoma,Oregon,
            Pennsylvania,Rhode_Island,
            South_Carolina,South_Dakota,
            Tennessee,Texas,
            Utah,Vermont,
            Virginia,Washington,
            West_Virginia,Wisconsin,
            Wyoming )
##testA <-   US %>%
##    filter(US$State=="Alaska")
##testA2 <- aggregate(Alaska,by = list(Alaska$Year), FUN = mean)
##Ag_Alaska <-aggregate(Alaska,by = list(Alaska$Year), FUN = mean)

  
US1 <- US %>%
  group_by(State,Year) %>%
  summarise_all(mean)
  
  library(data.table)
  
length(unique(US1$State))

US1 <- US1 %>%
  group_by(State) %>%
  mutate(n=n())



US1$Id <- paste0(US1$State,"_",US1$Year)
thirdtry$Id <- paste0(thirdtry$State,"_",thirdtry$Year)
US1$population <- thirdtry$Population[match(US1$Id, thirdtry$Id)]
US1$Deaths <- thirdtry$Deaths[match(US1$Id, thirdtry$Id)]
US1$`Crude Rate` <- thirdtry$`Crude Rate`[match(US1$Id, thirdtry$Id)]


path <- ("C:/Users/Daniel/Documents/Casual Inf Main paper/cleaning excel data for bankrupcy (version 1).xlsx")
sheetnames <- excel_sheets(path)
mylist <- lapply(excel_sheets(path), read_excel, path = path)
names(mylist) <- sheetnames
list2env(mylist ,.GlobalEnv)
Bankrupcy1 <- data.table::rbindlist(mylist,use.names = F,fill = T)
Bankrupcy1 <-  Bankrupcy1[,c(1:3)]
Bankrupcy1 <-  Bankrupcy1[!(Bankrupcy1$`CIRCUIT AND DISTRICT` %in% c( "TOTAL","1ST","2ND","3RD","4TH","5TH","6TH","7TH","8TH,","9TH","10TH,","11TH,","10TH","11TH","8TH",	
                                                                      "STH,",NA)),]
Bankrupcy1$`CIRCUIT AND DISTRICT` <- gsub("[,][A-z]{1}$","",Bankrupcy1$`CIRCUIT AND DISTRICT`)
Bankrupcy1$`CIRCUIT AND DISTRICT` <- gsub("[,][A-z]{1},$","",Bankrupcy1$`CIRCUIT AND DISTRICT`)
Bankrupcy1$`CIRCUIT AND DISTRICT` <- substring(trimws(Bankrupcy1$`CIRCUIT AND DISTRICT`),1,2)
Bankrupcy1$`TOTAL FILINGS` <- gsub("S","5",Bankrupcy1$`TOTAL FILINGS`)
Bankrupcy1$`TOTAL FILINGS` <- gsub('["]',"",Bankrupcy1$`TOTAL FILINGS`)

Bankrupcy1$`TOTAL FILINGS` <- as.numeric(gsub("[,]","",Bankrupcy1$`TOTAL FILINGS`))

#list_of_dataframe <- names(.GlobalEnv)[stringr::str_detect(names(.GlobalEnv),"^[0-9]{4}")]

#ls()[ls() %in% names(.GlobalEnv)[stringr::str_detect(names(.GlobalEnv),"^[0-9]{4}")]]
length(unique(toupper(Bankrupcy1$`CIRCUIT AND DISTRICT`)))
Bankrupcy1$`CIRCUIT AND DISTRICT` <-toupper(Bankrupcy1$`CIRCUIT AND DISTRICT`)
Bankrupcy1 <- Bankrupcy1 %>%
  group_by(`CIRCUIT AND DISTRICT`,Year) %>%
  summarise_all(sum)

Statenames <- read.csv(file = "Statenames.csv")
Bankrupcy2003_2016 <- read_excel("Bankrupcy data after 2003/Bankrupcy2003-2016.xlsx")
View(Bankrupcy2003_2016)

## Rename
Bankrupcy2003_2016 <- Bankrupcy2003_2016 %>%
  rename("CIRCUIT AND DISTRICT" = "Circuit and District")%>%
  rename("Year" = "Years")
##Clean
Bankrupcy2003_2016$`CIRCUIT AND DISTRICT` <- gsub("[...]","",Bankrupcy2003_2016$`CIRCUIT AND DISTRICT`)
Bankrupcy2003_2016$`CIRCUIT AND DISTRICT` <- gsub("[,][A-z]{1}$","",Bankrupcy2003_2016$`CIRCUIT AND DISTRICT`)
Bankrupcy2003_2016$`CIRCUIT AND DISTRICT` <- gsub("[,][A-z]{1},$","",Bankrupcy2003_2016$`CIRCUIT AND DISTRICT`)
Bankrupcy2003_2016 <-  Bankrupcy2003_2016[!(Bankrupcy2003_2016$`CIRCUIT AND DISTRICT` %in% c( "TOTAL","1ST",
                                                                                              "2ND","3RD",
                                                                                              "4TH","5TH",
                                                                                              "6TH","7TH",
                                                                                              "8TH,","9TH",
                                                                                              "10TH,","11TH,",
                                                                                              "10TH","11TH",
                                                                                              "8TH","STH,",NA)),]

##Aggreate 
length(unique(toupper(Bankrupcy2003_2016$`CIRCUIT AND DISTRICT`)))
Bankrupcy2003_2016$`CIRCUIT AND DISTRICT` <-toupper(Bankrupcy2003_2016$`CIRCUIT AND DISTRICT`)
Bankrupcy2003_2016 <- Bankrupcy2003_2016 %>%
  group_by(`CIRCUIT AND DISTRICT`,Year) %>%
  summarise_all(sum)


#Combine

MasterBank <- bind_rows(Bankrupcy1,Bankrupcy2003_2016)

## Have to rename things
View(Statenames)
MasterBank$`CIRCUIT AND DISTRICT` <- abbr2state(MasterBank$`CIRCUIT AND DISTRICT`)
MasterBank$Id <- paste0(MasterBank$`CIRCUIT AND DISTRICT`,"_",MasterBank$Year)
US1$`TOTAL FILINGS` <- MasterBank$`TOTAL FILINGS`[match(US1$Id, MasterBank$Id)]

length(unique(toupper(US1$State)))
# US1 is master dataset
# Filter US1 to only include 1990-2016 and drop period because that's dumb
US1 <- US1 %>%
  filter(between(Year,1990,2016)) %>%
  select(-c(3))

library("plm")

## Create Panel data
p.US1 <- pdata.frame(US1,index = c("State","Year"))
#Subsetting
EmploymentV <- p.US1[,c(3:8)]
SuicideV<- p.US1[,c(12:13)]
PopulV<- p.US1[,11]
BankrupcyV <- p.US1[,14]
#Model
model1twoway <- plm(log(p.US1$Crude.Rate)~
                      p.US1$labor.force.participation.rate+
                      p.US1$employment.population.ratio+
                      p.US1$unemployment.rate+
                      p.US1$population+
                      p.US1$TOTAL.FILINGS,data =p.US1,model = "within" )
library(broom)
Tmodel1 <-tidy(model1twoway)
model2twoway <- plm(log(p.US1$Deaths)~
                      p.US1$labor.force.participation.rate+
                      p.US1$employment.population.ratio+
                      p.US1$unemployment.rate+
                      p.US1$population+
                      p.US1$TOTAL.FILINGS,data =p.US1,model = "within" )
Tmodel2 <- tidy(model2twoway)
model3twoway <- plm(log(p.US1$Crude.Rate)~
                      p.US1$labor.force.participation.rate+
                      p.US1$employment.population.ratio+
                      p.US1$unemployment.rate+
                      p.US1$TOTAL.FILINGS,data =p.US1,model = "within" )
Tmodel3 <-tidy(model3twoway)

## Difference in Difference 1997
Treat1 <- c( "Maine","Maryland","Minnesota",
             "New Hampshire","North Dakota", "Rhode Island")
## Diiference in Difference 2000
Treat2 <- c("Arkansas","California","Colorado","Connecticut",
            "Delaware","Hawaii","Maine","Maryland","Minnesota","Montana",
            "New Hampshire","New Jersey","North Dakota",
            "Oklahoma","Rhode Island", "South Dakota",
            "Texas","Vermont","Virginia","West Virginia")
## I think I need to run an match like we did last time to get the year that it was enacted. 
p.US1$Treat <-as.numeric(if_else(p.US1$State %in% Treat1,1,0))
p.US1$NonTreat <-as.numeric(if_else(p.US1$State %in% Treat1,0,1))
#p.US1$Treat <-as.numeric(if_else(p.US1$State %in% Treat1,if_else(as.numeric(as.character(p.US1$Year)) > 1997,1,0),0))
p.US1$Post <- if_else(as.numeric(as.character(p.US1$Year)) > 1997,1,0)
p.US1$TreatPost <- as.numeric(p.US1$Treat*p.US1$Post)
DiD1997<-  plm(log(p.US1$Crude.Rate)~
                                p.US1$labor.force.participation.rate+
                                p.US1$employment.population.ratio+
                                p.US1$unemployment.rate+
                                p.US1$TOTAL.FILINGS+
                                p.US1$Treat+
                                p.US1$Post+p.US1$TreatPost,data =p.US1,model = "within",weights = p.US1$population )
tidy(DiD1997)
DiD1997_2 <- plm(log(p.US1$Crude.Rate)~
               p.US1$labor.force.participation.rate+
               p.US1$employment.population.ratio+
               p.US1$unemployment.rate+
               p.US1$TOTAL.FILINGS+
               p.US1$population+
               p.US1$Treat+
               p.US1$Post+p.US1$TreatPost,data =p.US1,model = "within" )
tidy(DiD1997_2)
## 
detect.lindep(DiD1997)

cor(p.US1$Treat,p.US1$TreatPost)
cor(p.US1$Treat,p.US1$Post)
test_Did <- plm(log(p.US1$Crude.Rate)~
                  p.US1$labor.force.participation.rate+
                  p.US1$employment.population.ratio+
                  p.US1$unemployment.rate+
                  p.US1$TOTAL.FILINGS+
                  p.US1$population+
                  p.US1$Treat+
                  p.US1$TreatPost,data =p.US1,model = "within" )
tidy(test_Did)
## Actual Did 
DiD_test1 <- lm(log(`Crude.Rate`)~
  `labor.force.participation.rate`+
  `employment.population.ratio`+
  `unemployment.rate`+
  `TOTAL.FILINGS`+
  `population`+
  `Post`+
  `Treat`+
  `TreatPost`,data = p.US1)

tidy(DiD_test1) 

## MATCHING 1997
p.US1 %>%
  group_by(Treat) %>%
  summarise(obs = n(),
            mean_S = mean(`Crude.Rate`),
            std_error = sd(`Crude.Rate`) / sqrt(obs))

with(p.US1, t.test(`Crude.Rate` ~ `Treat`))


p.US1_cov <- c("labor.force.participation.rate",
               "employment.population.ratio",
               "unemployment.rate",
               "TOTAL.FILINGS",
               "population") 
p.US1 %>%
  group_by(`Treat`) %>%
  select(one_of(p.US1_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

lapply(p.US1_cov, function(v) {
  t.test(p.US1[, v] ~ p.US1[, 'Treat'])
})


glmus1 <- glm(`Treat` ~ p.US1$labor.force.participation.rate+
                p.US1$employment.population.ratio+
                p.US1$unemployment.rate+
                p.US1$TOTAL.FILINGS+
                p.US1$population,
            family = binomial(), data = p.US1) 
summary(glmus1)

predict_df <- data.frame(pr_score = predict(glmus1, type = "response"),
                     treat = glmus1$model$Treat)
head(predict_df)

labs <- c("Treated States","NonTreated States")
predict_df %>%
  mutate(treatgg = as.character( ifelse(`treat` == 1,"Treated States","NonTreated States"))) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~treatgg) +
  xlab("Probability of being Treated") +
  theme_bw()

p.US1_nomiss <- p.US1 %>%  # MatchIt does not allow missing values
  select(`Crude.Rate`, Treat, one_of(p.US1_cov)) %>%
  na.omit()
library(MatchIt)
mod_match <- matchit(Treat ~ p.US1$labor.force.participation.rate+
                       p.US1$employment.population.ratio+
                       p.US1$unemployment.rate+
                       p.US1$TOTAL.FILINGS+
                       p.US1$population,
                     method = "nearest", data = p.US1)
mod_match2 <- matchit(`Crude.Rate` ~ p.US1$labor.force.participation.rate+
                        p.US1$employment.population.ratio+
                        p.US1$unemployment.rate+
                        p.US1$TOTAL.FILINGS+
                        p.US1$population+
                        p.US1$Post+
                        p.US1$TreatPost,
                      method = "nearest", data = p.US1)
datamatch <- match.data(mod_match)
dim(datamatch)

## Matching Images
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  if (variable == 'Unemployment.rate') dta$variable <- dta$variable 
  dta$Treat <- as.factor(dta$Treat)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = Treat)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

library(gridExtra)
grid.arrange(
  fn_bal(datamatch, "employment.population.ratio"),
  fn_bal(datamatch, "unemployment.rate") + theme(legend.position = "none"),
  fn_bal(datamatch, "TOTAL.FILINGS") + theme(legend.position = "none"),
  nrow = 2, widths = c(1, 0.8)
)

# images not working 

CovarMatch <- datamatch %>%
  group_by(Treat) %>%
  select(one_of(p.US1_cov)) %>%
  summarise_all(funs(mean))

with(datamatch, t.test(`Crude.Rate` ~ `Treat`))

MatchDid <-lm(log(`Crude.Rate`) ~ `labor.force.participation.rate`+
  `employment.population.ratio`+
  `unemployment.rate`+
  `TOTAL.FILINGS`+
  `population`+
    `Treat`+
    ,data = datamatch)

tidy(MatchDid)
## Stargazer
stargazertest <- stargazer::stargazer(DiD_test1,type = "html", out ="testfig.html" )
## Summary table
US1$Treat <-as.numeric(if_else(US1$State %in% Treat1,1,0))
US1$NonTreat <-as.numeric(if_else(US1$State %in% Treat1,0,1))
US1$Post <- if_else(as.numeric(as.character(US1$Year)) > 1997,1,0)
US1$TreatPost <- as.numeric(US1$Treat*US1$Post)
smallUS1 <- US1 %>%
  select("labor force participation rate",
         "employment-population ratio",
         "unemployment rate",
         "TOTAL FILINGS",
         "population",
         "Treat",
         "Post",
         "TreatPost",
         "Crude Rate",
         "State",
         "Year") 
summary(smallUS1)
stargazer::stargazer(as.data.frame(smallUS1), type = "html",out = "smallUS1.html")
stargazer::stargazer(MatchDid,type = "html",out="Matching Results.html")
stargazer::stargazer(as.data.frame(CovarMatch),type = "html",out = "Covariate Matching Results.html")
stargazer::stargazer(DiD_test1,type="html",out = "DiD Results.html")
stargazer::stargazer(DiD_test1,MatchDid,type = "html",out = "Did and Match Results.html")
library(knitr)
kable(CovarMatch, format = "html")
cat(kable(CovarMatch, format = "html"),file = "Covarmatch.html")
sumstatslist<- c(p.US1_cov,"Crude.Rate")

SumStats <- p.US1 %>%
  group_by(Treat) %>%
  select(one_of(sumstatslist)) %>%
  summarise_all(funs(mean))
cat(kable(SumStats, format="html"),file = "Treatment summary Statistics.html")






### 2/6/24 
### Hoping this is something that fixes what we got here. 
### Things to do 
###  * append/merge each year of data into a big excel sheet and append that into our data
###  Taking "Large business datasheet" into years of data with TOTAL and firms that are 500+
### * implement a timing control ydelta basically
### * It's not just the LBD but also states 2006_2021 that need to be appended together. 
### Removed all state related dataframes that were already in the Masterdataset. 
### The rest of this script will be decicated to finsihing the masterdataset
library(data.table)
library(tidyverse)
library(readxl)
library(readr)

path <- "us_state_totals_2007-2021.xlsx"

yeartitle <- as.character(c(2007:2021))

Firmdata <- path |>
  excel_sheets() |>
  purrr::set_names() |>
  purrr::map(read_excel,path=path,skip=4,col_names = T) |>
  map(rename,"Fips" =1,"State"=2,"Enterprise Size"=3,"Firms"=4,"Establishments"=5,"Employment"=6) |>
  map(select,1:6) 

Firmdata <-  mapply(cbind, Firmdata, "Year"=yeartitle, SIMPLIFY=F) 

Firmdata<- Firmdata |>
  list_rbind() 
#Firmdata <-Firmdata |>
 # filter(grepl(Firmdata$`Enterprise Size`,pattern = "1:")|
  #       grepl(Firmdata$`Enterprise Size`,pattern = "9:")) 
FirmdataTotal <- Firmdata |>
  filter(grepl(Firmdata$`Enterprise Size`,pattern = "1:"))
  ## above Produces 1560 ob dataset with each 500+ and Total Entry


Firmdata500 <- Firmdata |>
  filter(grepl(Firmdata$`Enterprise Size`,pattern = "9:")) 
FirmdataWperc <- Firmdata500$Employment /FirmdataTotal$Employment 


## You need just need to get the percentage of W out of this sheet. 
#Filter for 500+ and Total. 
#Fill down for State
# Calc Percentage
# Creat it's down dataset with Year and State asssoc w it

Pastyeartitle <- as.character(c(1988:2006))
path2 <- "us_state_totals_1988-2006.xlsx"
FirmData88_06 <-path2 |>
  excel_sheets() |>
  purrr::set_names() |>
  purrr::map(read_excel,path=path2,skip=7,col_names = T)

# Here we have the list of dataframes, we need to get use a loop to drop columns from each dataset
ColIneed <- c("AREA","DATA TYPE","TOTAL","500+")
FirmData88_06 <- lapply(FirmData88_06, subset,select= ColIneed)
FirmData88_06 <- lapply(FirmData88_06)
## save this for later
  list_rbind(names_to = "Year") 

  FirmData86_06$Year <-substr(FirmData86_06$Year,2,6)
## For 1990 and 1991 You have to filter for "United States".
## This also goes for Firms Or "Grand Total"
  ## NEed to create line to make the united states column a total column for each 
  ## Year
 FirmData86_06 <- FirmData86_06 |>
   filter(!(`Row Labels` %in% c("Firms","United States","Grand Total"))) |>
   rename("State"= `Row Labels`,"Firms"=`Sum of 500+`)
