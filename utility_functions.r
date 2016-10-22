# -----------------------------------------------------------
    # Author = Effie
    # Date = July 2016
    # utility functions for parsing investors names
# -----------------------------------------------------------

# remove leading/trailing spaces
trim <- function (x) gsub("^\\s+|\\s+$","",x)


# split: split by ","
split_investors <- function(x) {
    strsplit(x, ",")[[1]]
}


# processInvestors: overall parsing 
# for investor string in Crunchbase database of funding rounds
processInvestors <- function(x) {
    investors <- split_investors(x)
    temp <- NA
    x <- split_investors(x)
    for (j in x) {
        j <- gsub("Lead - ", "", j)
        j <- trim(j)
        if (is.na(temp))
            temp <- j
        else
            temp <- c(temp,j)
    }
    temp
}

# removes special characters in a string
remove_special <- function (x) {
    gsub("[^0-9A-Za-z///' ]", "", x)
}

# lookup node names and indices
getCompanyIndex <- function(name) {
    i <- which(grepl(name, company_index$lookup_name))
    company_index$index[i]
}
getCompanyName <- function(i) {
    x <- which(grepl(i,company_index$index))[[1]]
    as.character(company_index$companies[x])
}
getInvestorIndex <- function(name) {
    i <- which(grepl(name, investor_index$lookup_name))
    investor_index$index[i]
}
getInvestorName <- function(i) {
    x <- which(grepl(i,investor_index$index))[[1]]
    as.character(investor_index$investors[x])
}