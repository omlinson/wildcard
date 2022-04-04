#packages 

library(tidyverse)
library(lubridate)


#set folder
#merge data

setwd("~/R/projects/wlidcard/data")
dailyCampaign <- read.csv("dailyCampaignData.csv")
dailyCost <- read.csv("dailyCostData.csv")
daily <- merge(dailyCampaign, dailyCost)


#make dates easier since in 1 quarter

daily$month <- month(dmy(daily$date))
daily$day <- day(dmy(daily$date))

#summary

gameReview <- daily %>% 
  group_by(game) %>%
  summarise(
    game = game,
    totalCost =sum(cost),
    totalRev = sum(cohort_revenue),
    totalInstalls = sum(installs),
    CPI =  (totalInstalls / totalCost),
    ARPU = (totalRev / totalInstalls),
    ROAS = (totalRev / totalCost)) %>%
  unique()

#one game seems to suck a lot more than the others, only one is profitable at at 30. 
# ULC: can we drive down CPI drastically? do we become profitable at 90+? 
# ULC: why so much spend? is it an older game? did we tap out audience / using old profits to pay for growth?
# ULH: seems to have a much lower spend? is it a newer game or is it CPI bidding related? almost profts
# ULH: there must be a profitable campaign or network here to be more aggressive in?
# ULM: Profitable after 30 days. highest CPI - maybe somethingn to cut to increase profits. 
# I haven't really looked at cohorts / retention yet. 30 days seems like a good target for profitable
# would love to know if there are "addicts" 

#check cohorts

gameCohort <- daily %>%
  group_by(game) %>%
  summarise(
    game = game,
    revenue_d0 = sum(revenue_d0),
    revenue_d15 = sum(revenue_d15),
    revenue_d30 = sum(revenue_d30),
    retained_users_d0 = sum(retained_users_d0),
    retained_users_d15 = sum(retained_users_d15),
    retained_users_d30 = sum(retained_users_d30)
  ) %>%
  unique()


gameCohort2 <- gather(gameCohort, "cohort", "revenue", 2:4)
gameCohort3 <- gather(gameCohort, "cohort", "retained", 5:7)

# graph it out

ggplot(gameCohort2, aes(cohort, revenue, group=factor(game))) + geom_line(aes(color=factor(game)))
ggplot(gameCohort3, aes(cohort, retained, group=factor(game))) + geom_line(aes(color=factor(game)))


# go into networks


networkMacro <- daily %>% 
  group_by(network) %>%
  summarise(
    network = network,
    totalCost =sum(cost),
    totalRev = sum(cohort_revenue),
    totalInstalls = sum(installs),
    CPI =  (totalInstalls / totalCost),
    ARPU = (totalRev / totalInstalls),
    ROAS = (totalRev / totalCost)) %>%
  unique()

networkReview <- daily %>% 
  group_by(network, os_name, game) %>%
  summarise(
    game = game,
    network = network,
    os_name = os_name,
    totalCost =sum(cost),
    totalRev = sum(cohort_revenue),
    totalInstalls = sum(installs),
    CPI =  (totalInstalls / totalCost),
    ARPU = (totalRev / totalInstalls),
    ROAS = (totalRev / totalCost)) %>%
  unique()

# BLUE - IOS - ULH is not present?!?
# GREEN - Something is off here, why is IOS so much better (we already know ULC sucks)
# GREEN - Low spending on Green, don't really care that much about it, if I need to cut some fat... 
# BLUE - ULC - bulk of spend is here, has better than ULC global ROAS + bigger CPI... 
# BLUE - ULC - Android - seems to be the winner for this guy, are we testing the other or are they getting weened off?
# BLUE - ULC - IOS - weird how ULC is the only one with a drastic difference between os in BLUE
# RED - Similar performance but better (Red - ULM - Android = amazing) here if we ignore ULC 
# Need to dig into Blue for ULC / take a look at campaigns or countries for Red/Blue for ULM/ULH to see if strong performance
# Green just smells like fraud 


countryReview <- daily %>% 
  group_by(country, game) %>%
  summarise(
    game = game,
    country = country,
    totalCost =sum(cost),
    totalRev = sum(cohort_revenue),
    totalInstalls = sum(installs),
    CPI =  (totalInstalls / totalCost),
    ARPU = (totalRev / totalInstalls),
    ROAS = (totalRev / totalCost)) %>%
  unique()

# English only speaking countries, I believe lower costs could be achieved in other markets but ad money is probably lower too
# Was not expecting NZ to have such a high CPI
# US seems to be standard around all games in terms of ROAS, weird play between CA / UK in terms of which games are performing best

# Going to split campaigns by games - I don't necessarily agree that it should always be done that way
# Will not look at Green as it's the low spender and this is a test - i'm just going to suggest an exit

###  ULC


ULCReview <- daily %>% 
  group_by(game, network, campaign) %>%
  summarise(
    game = game,
    campaign = campaign,
    totalCost =sum(cost),
    totalRev = sum(cohort_revenue),
    totalInstalls = sum(installs),
    CPI =  (totalInstalls / totalCost),
    ARPU = (totalRev / totalInstalls),
    ROAS = (totalRev / totalCost)) %>%
  filter(game == "ULC", network != "Green")  %>%
  unique()

#tried to remove campaigns that did not last the whole quarter, but it removed a few high paying campaigns so 

ULChurnReview <- daily %>% 
  group_by(game, network, campaign) %>%
  summarise(
    game = game,
    campaign = campaign,
    start = min(dmy(daily$date[daily$campaign == campaign])),
    end = max(dmy(daily$date[daily$campaign == campaign])),
    totalCost = sum(cost),
    totalInstalls = sum(installs),
    CPI = sum(cost)/sum(installs),
    ROAS = sum(cohort_revenue)/totalCost,
    churn_d1 = (totalInstalls-sum(retained_users_d1))/totalInstalls,
    churn_d15 = (totalInstalls-sum(retained_users_d15))/totalInstalls,
    churn_d30 = (totalInstalls-sum(retained_users_d30))/totalInstalls
  ) %>%
  ungroup %>%
  filter(game == "ULC", totalCost >= mean(totalCost))  %>%
  unique()


# ULChurnReview$costShare <- ULChurnReview$totalCost / sum(ULChurnReview$totalCost)



summary(ULChurnReview)



# option to look at churn to just get the code for it and not sure why i get a warning for the dates
# will opt to ignore campaigns that did not span the full period - some were paused late in November
# they were placed by a few others -> might be worth eliminating from all data


