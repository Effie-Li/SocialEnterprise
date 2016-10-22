# -----------------------------------------------------------
    # Author = Effie
    # Date = July 2016
    # Getting investment activites via Crunchbase API
# -----------------------------------------------------------

setwd("C:/Users/Effie/Documents/SUMMER@COLUMBIA/Data/")

library(rcrunchbase)
library(openxlsx)

# import crunchbase data
funded_companies <- read.xlsx("crunchbase_export_Jul12.xlsx",sheet=2)
rounds <- read.xlsx("crunchbase_export_Jul12.xlsx",sheet=3)
all_investors <- read.xlsx("crunchbase_export_Jul12.xlsx",sheet=4)

# ground truth social investors
social_investors <- read.csv("social_investors.csv")

# -----------------------------------------------------------
    # preparation work
# -----------------------------------------------------------

# format uuids
funded_companies$uuid <- gsub("-","",funded_companies$uuid)
rounds$funding_round_uuid <- gsub("-","",rounds$funding_round_uuid)
rounds$company_uuid <- gsub("-","",rounds$company_uuid)
all_investors$uuid <- gsub("-","",all_investors$uuid)


# -----------------------------------------------------------
    # getting data via Crunchbase api
    # 502 total investment activities from ground-truth investors
# -----------------------------------------------------------

# get investment activites from crunchbase
output <- NA
for (i in 1:nrow(social_investors)){
    link <- as.character(social_investors[i,2])

    if (link != "") {
        investor <- crunchbase_get_details(link)
        if (i!=127)
            temp <- crunchbase_expand_section(investor, "investments")
        else {
            a <- investor[[1]]
            temp <- a$relationships$investments$items
        }
        if (is.na(output)) output <- temp else output <- rbind(output,temp)
    }
    cat(i)
    cat("\n")
}

# pull social funding rounds from crunchbase data export
temp <- unique(output$relationships.funding_round.uuid)
social_rounds <- NA
for (i in temp) {
    x = which(grepl(i, rounds$funding_round_uuid))
    if (x != 0) {
        social_rounds <- rbind(social_rounds,rounds[x,])
    }
}

# all investors for companies in the social rounds (second round of investors)
temp <- NA
for (i in 1:nrow(social_rounds)) {
    investors <- social_rounds$investor_names[i]
    investors <- processInvestors(investors)
    if (is.na(temp))
        temp <- investors
    else
        temp <- c(temp, investors)
}
coinvestors <- unique(temp)

# -----------------------------------------------------------
    # social fund rounds overview
    # 477 unique funding rounds
    # 400 unique companies
    # 33 countries
    # 209 companies in 33 states in the US
    # 1347 investors, 667 unique investors

    # some of the initial investors who are present on crunchbase did not identify themselves as investors
# -----------------------------------------------------------