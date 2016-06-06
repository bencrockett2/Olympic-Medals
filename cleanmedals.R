library(rvest)
library(stringr)
library(stringi)

# This code scrapes data from Wikipedia on how many Olympic Medals each country won at
# each Olympic Games in a specified time period

# set sequence of years with summer Olympic Games
yearbegin <- 1924
yearend <- 2012
rem <- c(1916, 1940, 1944) # remove years due to war
years <- seq(from = yearbegin, to = yearend, by = 4)
years <- years[! years %in% rem]
# years <- 1936

# set mappings for alternate names of countries
altnames <- matrix(c("Ceylon", "Chinese Taipei", "Unified Team", 
    "Sri Lanka", "Republic of China", "Soviet Union"), nrow = 3, ncol = 3)

# initialize data frame for all data
alldata <- data.frame("Nation" = c(""))

# count how many times need to pull second table
tablecount <- 0

for (i in years) {
    # build url using wikipedia template and inserting year
    url <- paste("http://en.wikipedia.org/wiki/", i, "_Summer_Olympics_medal_table", sep = "")

    # pull table data from webpage
    dat <- url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
        html_table(fill = TRUE)
    dat <- dat[[1]]

    # Remove strange characters from column names
    colnames(dat) <- str_trim(gsub("Â", "", colnames(dat)))
    
    # If data isn't found in table[1], pull from table[2] (sloppy)

    if (colnames(dat)[1] != "Rank") {
        dat <- url %>%
            read_html() %>%
            html_nodes(xpath='//*[@id="mw-content-text"]/table[2]') %>%
            html_table(fill = TRUE)
        dat <- dat[[1]]
        tablecount <- tablecount + 1
    }
    
    temp <- dat

    # Remove strange characters from column names
    colnames(temp) <- str_trim(gsub("Â", "", colnames(temp)))

    # Rename NOC to Nation, if applicable
    colnames(temp)[2] <- "Nation"

    # Find rows that are shifted and align them with the rest
    for (j in 1:dim(temp)[1]) {
        if (is.na(temp[j, "Total"])) {
            temp[j, ] <- c(0, temp[j, 1:5])
        }
    }

    # Erase first column which has country rankings
    temp <- temp[1:dim(temp)[1] - 1, 2:6]

    # Specify odd characters to be deleted
    chars <- c("Â", "â", "€", "¡", "\\*", "\\(", "\\)", "\\[", "\\]", "8")
    for (j in chars){
        temp$Nation <- gsub(j, "", temp$Nation)
    }
    

    # Remove parenthetical abbreviations following country names
    temp$Nation <- stri_sub(temp$Nation, 1, -5)
    
    # Trim white space from columns
    temp$Nation <- str_trim(temp$Nation)
    
    # Insert year into medal columns
    len <- length(colnames(temp))
    colnames(temp)[2:len] <- sapply(colnames(temp)[2:len], function(x) paste(x, i, sep = ""))
    
    # standardize country names (replace alternate names)
    for (j in 1:dim(altnames)[1]) {
        temp$Nation <- gsub(altnames[j,1], altnames[j,2], temp$Nation)
    }
    
    # add data from current table to alldata table, add nations if necessary
    alldata <- merge(alldata, temp, by = "Nation", all.x = TRUE, all.y = TRUE)

}
# remove top, empty row of data frame
alldata <- alldata[-c(1), ]

# replace NAs with 0's
alldata[is.na(alldata)] <- 0

# convert numbers saved as characters to all numerics
alldata[, 2:ncol(alldata)] <- sapply(alldata[, 2:ncol(alldata)], as.numeric)

# Tack on columns with total won medals of each type
alldata$TotalBronze <- apply(alldata[ , bronseq], 1, sum)
alldata$TotalSilver <- apply(alldata[ , silvseq], 1, sum)
alldata$TotalGold <- apply(alldata[ , goldseq], 1, sum)
alldata$TotalWon <- apply(alldata[, totseq], 1, sum)

