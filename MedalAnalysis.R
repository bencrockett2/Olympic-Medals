library(dplyr)

# set sequences to subset medals by color
goldseq <- seq(from = 2, to = 4 * length(years) + 1, by = 4)
silvseq <- seq(from = 3, to = 4 * length(years) + 2, by = 4)
bronseq <- seq(from = 4, to = 4 * length(years) + 3, by = 4)
totseq <- seq(from = 5, to = 4 * length(years) + 4, by = 4)

# Look at plot of number of medals a country has won over the years
country <- "China"
hostyears <- OlympicHosts[OlympicHosts$Country == country, 1]
plot(years, alldata[alldata$Nation == country, totseq], 
     col = ifelse(years %in% hostyears, "red", "black"),
     pch = ifelse(years %in% hostyears, 19, 1), 
     xlab = "Year",
     ylab = "Medals",
     main = c("Total Medals Won by ", country))
legend("bottomright", c("Host", "Guest"), 
       col = c("red", "black"), 
       pch = c(19, 1))

# Population notes:
# East & West Germany - from populstat.info
# Cuba, DPRK, and Jamaica - from populstat.info, only years provided after 
# 2001 are 2005, 2010, 2015, years in between are interpolated linearly
# Mongolia - 1960 - 1969
# Republic of China - any available