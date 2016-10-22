# -----------------------------------------------------------
    # Author = Effie
    # Date = July 2016
    # Some data cleaning/rearranging for the network
# -----------------------------------------------------------

companies <- unique(social_rounds$company_name)
investors <- social_rounds$investor_names
temp <- NA
for(i in investors) {
    if (is.na(temp))
        temp <- processInvestors(i)
    else
        temp <- c(temp, processInvestors(i))
}
investors <- unique(temp)

# create lookup tables for companies and investors
company_index <- data.frame(companies)
for(i in 1:nrow(company_index)) {
    company_index$index[i] <- paste("C",i,sep="")
    name = company_index$companies[i]
    company_index$lookup_name[i] <- remove_special(name)
}

investor_index <- data.frame(investors)
for(i in 1:nrow(investor_index)) {
    investor_index$index[i] <- paste("I",i,sep="")
    name = investor_index$investors[i]
    investor_index$lookup_name[i] <- remove_special(name)
}


# create edgelist with company/inestor's index
edgelist <- matrix("", ncol = 2)
for(i in 1:nrow(social_rounds)) {
    cat(i)
    cat('  ')
    company <- remove_special(social_rounds$company_name[i])
    com <- getCompanyIndex(company)
    
    investors <- social_rounds$investor_names[i]
    investors <- processInvestors(investors)
    
    for(investor in investors) {
        investor <- remove_special(investor)
        inv <- getInvestorIndex(investor)[1]
        edgelist <- rbind(edgelist, c(inv,com))
    }
}
edgelist <- edgelist[-1,]
edges <- as.data.frame(edgelist,
                       stringsAsFactors = default.stringsAsFactors())


# -----------------------------------------------------------
# build metadata for the nodes
# 667 investors and 400 companies
# i.e., type, geography info and categories
# -----------------------------------------------------------

# type of nodes
nodelist <- data.frame(node=character(), 
                       type=character(),
                       isGroundTruth=integer(), 
                       stringsAsFactors = F)
for(i in 1:nrow(company_index)) {
    index <- company_index$index[i]
    nodelist[nrow(nodelist)+1,] <- c(index,"company",0)
}

for(i in 1:nrow(investor_index)) {
    x <- 0
    index <- investor_index$index[i]
    name <- investor_index$lookup_name[i]
    isSocial <- which(grepl(name, social_investors$investor_name))
    if (length(isSocial) > 0) {
        x <- 1
    }
    nodelist[nrow(nodelist)+1,] <- c(index,"investor",x)
}

nodelist$type <- factor(nodelist$type)

# lookup names (without special chars)
for (i in 1:nrow(nodelist)) {
    index <- nodelist$node[i]
    type <- as.character(nodelist$type[i])
    
    if (type == "company") {
        x <- which(grepl(index,company_index$index))
        lookup <- company_index$lookup_name[x]
    }
    else if (type == "investor") {
        x <- which(grepl(index,investor_index$index))
        lookup <- investor_index$lookup_name[x]
    }
    
    nodelist$lookup_name[i] <- lookup
}
for (i in 1:nrow(social_rounds)) {
    company <- social_rounds$company_name[i]
    rs <- remove_special(company)
    social_rounds$lookup_name[i] <- rs
}
for (i in 1:nrow(all_investors)) {
    investor <- all_investors$investor_name[i]
    rs <- remove_special(investor)
    all_investors$lookup_name[i] <- rs
}


# actual names of the nodes in crunchbase database
x <- nodelist
for(i in 1:nrow(x)) {
    node <- x$node[i]
    type <- as.character(x$type[i])
    if (type=="company")
        name <- getCompanyName(node)
    else if (type=="investor")
        name <- getInvestorName(node)
    x$crunchbase_name[i] <- name
}
nodelist <- x


# geo info
temp <- nodelist
for (i in 1:nrow(temp)) {
    cat(i)
    cat("  ")
    lookup <- temp$lookup_name[i]
    type <- as.character(temp$type[i])
    if (type == "company") {
        x <- which(grepl(lookup, social_rounds$lookup_name))
        country_code <- social_rounds$country_code[x]
        state_code <- social_rounds$state_code[x]
        region <- social_rounds$region[x]
        city <- social_rounds$city[x]
        category <- social_rounds$company_category_list[x]
    }
    if (type == "investor") {
        x <- which(grepl(lookup, all_investors$lookup_name))
        country_code <- all_investors$country_code[x]
        state_code <- all_investors$state_code[x]
        region <- all_investors$region[x]
        city <- all_investors$city[x]
        category <- all_investors$investor_type[x]
    }
    temp$country_code[i] <- country_code
    temp$state_code[i] <- state_code
    temp$region[i] <- region
    temp$city[i] <- city
    temp$category[i] <- category
}

nodelist <- temp


# broader categories by hand
write.csv(nodelist, "node_metadata.csv", row.names = F)
nodelist <- read.csv("node_metadata.csv")


# -----------------------------------------------------------
    # added new GT social and GT non-social
# -----------------------------------------------------------
nodelist$GT_social <- 0
nodelist$GT_non_social <- 0

write.csv(nodelist, "node_metadata.csv", row.names = F)
# some editing by hand
nodelist <- read.csv("node_metadata.csv")

nodelist$GT_social[which(grepl(1, nodelist$isGroundTruth))] <- 1

# 86 GT_social
# 59 GT_non_social
# 9 government

nodelist$government[is.na(nodelist$government)] <- 0
nodelist$government <- as.integer(nodelist$government)
