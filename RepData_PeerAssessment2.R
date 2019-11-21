library(dplyr)
library(ggplot2)
library(tidyr)
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileURL, "./StormData.csv.bz2")
data <- read.csv("./StormData.csv.bz2")

## Health
data.h <- select(data, c(8, 23, 24))
sum.data.h <- data.h %>% group_by(EVTYPE) %>% summarise(Sum.FAT = sum(FATALITIES, na.rm = TRUE), Sum.INJ = sum(INJURIES, na.rm = TRUE)) %>% mutate(Total.Health = Sum.FAT+Sum.INJ) %>% arrange(desc(Total.Health)) %>% top_n(n = 5, wt = Total.Health)
health <- sum.data.h %>% gather("Sum.FAT", "Sum.INJ", key = "H.Type", value = "Quantity", factor_key = TRUE) %>% select(-Total.Health)
plot1 <- ggplot(health, aes(x = reorder(EVTYPE, -Quantity), Quantity), fill = H.Type)
plot1+geom_bar(aes(fill = H.Type), stat = "identity", position = "stack")+
        labs(title = "Types of events that most harmful \n with respect to population health across the United States", x = "Event type", y = "Number of population")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_fill_discrete(name = "", labels = c("Fatalities", "Injuries"))

## Economy
data.e <- select(data, c(8, 25:28))
symbol <- sort(unique(as.character(data.e$PROPDMGEXP)))
exp <- c <- c(rep(0, 3), 1, rep(10, 9), 1e9, rep(1e2, 2), 1e3, rep(1e6, 2))
conv.tab <- data.frame(symbol, exp)
data.e$PROPDMGCONV <- conv.tab$exp[match(data.e$PROPDMGEXP, conv.tab$symbol)]
data.e$CROPDMGCONV <- conv.tab$exp[match(data.e$CROPDMGEXP, conv.tab$symbol)]
data.e <- data.e %>% mutate(PROPDMG = PROPDMG*PROPDMGCONV) %>% mutate(CROPDMG = CROPDMG*CROPDMGCONV) %>% mutate(TOTALDMG = PROPDMG+CROPDMG)
sum.data.e <- data.e %>% group_by(EVTYPE) %>% summarise(Sum.PROP = sum(PROPDMG, na.rm = TRUE), Sum.CROP = sum(CROPDMG, na.rm = TRUE)) %>% mutate(Total.ECON = Sum.PROP+Sum.CROP) %>% arrange(desc(Total.ECON)) %>% top_n(n = 5, wt = Total.ECON)
econ <- sum.data.e %>% gather("Sum.PROP", "Sum.CROP", key = "E.Type", value = "Amount", factor_key = TRUE) %>% select(-Total.ECON)
plot2 <- ggplot(econ, aes(x = reorder(EVTYPE, -Amount), Amount/1e9), fill = E.Type)
plot2+geom_bar(aes(fill = E.Type), stat = "identity", position = "stack")+
        labs(title = "Types of events that have the greatest \n economic consequences across the United States", x = "Event type", y = "Cost of damages: Billon USD")+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_fill_discrete(name = "", labels = c("Properties", "Crops"))