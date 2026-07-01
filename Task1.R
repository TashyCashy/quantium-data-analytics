install.packages("data.table")
install.packages("ggplot2")
install.packages("ggmosaic")
install.packages("readr")
install.packages("readxl")

library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(readxl)

# fread is a function from the data.table package designed for CSV files
# read_excel from the readxl package is for Excel files
# set your working directory to the folder containing the data files
filePath <- "C:/Users/natma/OneDrive - University of Cape Town/PROJECTS/Statistics/Quantium Analytics/" 
customerData <- fread(paste0(filePath, "QVI_purchase_behaviour.csv"))
transactionData <- read_excel(paste0(filePath, "QVI_transaction_data.xlsx"))

# exploratory data analysis
head(transactionData)

# converting to desired formats
# converting date column to the date format
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

# generate a summary of the PROD_NAME column
summary(transactionData$PROD_NAME)

# converting transactionData to a data.table first
setDT(transactionData)

# text analysis by summarising the individual words in the product name
# checking for incorrect entries like products that are not chips
# splitting by space to separate different words
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), " ")))
setnames(productWords, 'words')

# removing digits, and special characters and then sort the distinct words by
# frequency of occurrence
# sorted from most frequent to least frequent
productWords <- productWords[!grepl("[0-9]", words) & !grepl("[^A-Za-z]", words)]
wordFrequency <- productWords[, .N, by=words][order(-N)]
print(wordFrequency)

# let's remove salsa products
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]

# summarising data to check for any null values and possible outliers
summary(transactionData)

# filtering for transactions where quantity is 200
transactionData[PROD_QTY == 200]

# get the customer number from the outlier and investigate all their transactions
outlierCust <- transactionData[PROD_QTY == 200, LYLTY_CARD_NBR]
transactionData[LYLTY_CARD_NBR %in% outlierCust]

# probably for commercial use
# filter out the customer based on the loyalty number
# all you have to do is negate the filter
transactionData <- transactionData[!LYLTY_CARD_NBR %in% outlierCust]

# reexamine the transaction data
summary(transactionData)

# count the number of transactions by date
transactionsByDate <- transactionData[, .N, by=DATE][order(DATE)]
print(transactionsByDate)

# summary of transaction count by date
summary(transactionsByDate)

# there is a missing date in the year, let us find it
# use seq to generate every single date between the two dates
# left join, keeping all the dates even if there is no matching transaction data
# fill the missing date with 0 transactions instead of NA
allDates <- data.table(DATE=seq(as.Date("2018-07-01"), as.Date("2019-06-30"), by="day"))
transactionsByDate <- merge(allDates, transactionsByDate, by="DATE", all.x=TRUE)
transactionsByDate[is.na(N), N :=0]
print(transactionsByDate)

# what is the missing date
transactionsByDate[N==0]

# setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust=0.5))

# plot transactions over time
ggplot(transactionsByDate, aes(x=DATE, y=N)) + 
  geom_line() +
  labs(x="DAY", y="Number of transactions", title="Transactions over time") +
  scale_x_date(breaks= "1 month") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# let's recreate the chart and zoom over the month of December
december <- transactionsByDate[DATE >= as.Date("2018-12-01") & DATE <= as.Date("2018-12-31")]
ggplot(december, aes(x=DATE, y=N)) + 
  geom_line() +
  labs(x="DAY", y="Number of transactions", title="Transactions over time") +
  scale_x_date(breaks= "1 month") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# data has no outliers
# we move on to creating other features such as brand of chips or pack size from PROD_NAME
# getting pack sizes by taking the digits that are in PROD_NAME
transactionData[, PACK_SIZE :=parse_number(PROD_NAME)]
# do the pack sizes look sensible
transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]

# plot a histogram of PACK_SIZE since we know it is a categorical variable and 
# not continuous even though it is numberic
ggplot(transactionData, aes(x=PACK_SIZE)) +
  geom_histogram(binwidth=10) + 
  labs(x="Pack Size(g)", y="Number of transactions", title="Transactions by pack size") + 
  theme_minimal()

# let's use the first word in PROD_NAME to work out the brand name
# converting to uppercase for consistency
transactionData[, BRAND := toupper(gsub("([A-Za-z]+).*", "\\1", PROD_NAME))]
transactionData[, .N, by=BRAND][order(-N)]

# fix inconsistent brand names
transactionData[BRAND == "RED", BRAND := "RRD"]
transactionData[BRAND == "SNBTS", BRAND := "SUNBITES"]
transactionData[BRAND == "INFZNS", BRAND := "INFUZIONS"]
transactionData[BRAND == "WW", BRAND := "WOOLWORTHS"]
transactionData[BRAND == "SMITH", BRAND := "SMITHS"]
transactionData[BRAND == "NCC", BRAND := "NATURAL"]
transactionData[BRAND == "DORITO", BRAND := "DORITOS"]
transactionData[BRAND == "GRAIN", BRAND := "GRANA"]
# let's recheck
transactionData[, .N, by=BRAND][order(-N)]

# CUSTOMER DATASET
summary(customerData) 

# merge transaction data to customer data
data <- merge(transactionData, customerData, all.x=TRUE)

# as the number of rows in 'data' is the same as that of 'transactionData', we 
# can be sure that no duplicates were created. This is because we created 'data'
# by setting 'all.x=TRUE' i.e. a left join. This means take all the rows in
# 'transactionData' and find rows with matching values in shared columns and 
# then joining the details in these rows to the first mentioned table.

# see if any transactions did not have a matched customer
# check for nulls in the merged data
sum(is.na(data))
data[is.na(LIFESTAGE)|is.na(PREMIUM_CUSTOMER)]
# result is 0. Every transaction was successfully matched to a customer.

# save the dataset as a csv
fwrite(data, paste0(filePath, "QVI_data.csv"))

# total sales by LIFESTAGE and PREMIUM_CUSTOMER
salesBySegment <- data[, .(totalSales=sum(TOT_SALES)),
                       by=.(LIFESTAGE, PREMIUM_CUSTOMER)][order(-totalSales)]
ggplot(salesBySegment, aes(x=LIFESTAGE, y=totalSales, fill=PREMIUM_CUSTOMER)) +
  geom_bar(stat="identity", position="dodge") + 
  labs(x="Lifestage", y="Total sales", 
       title="Total sales by lifestage and premium customer segment",
       fill="Premium Customer") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5))

# let's see if the higher sales are due to there being more customers who buy chips
# number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customersBySegment <- data[, .(numCustomers=uniqueN(LYLTY_CARD_NBR)),
                     by=.(LIFESTAGE, PREMIUM_CUSTOMER)][order(-numCustomers)]
ggplot(customersBySegment, aes(x=LIFESTAGE, y=numCustomers, fill=PREMIUM_CUSTOMER)) +
  geom_bar(stat="identity", position="dodge") + 
  labs(x="Lifestage", y="Number of customers", 
       title="Number of customers by lifestage and premium customer segment",
       fill="Premium Customer") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5))

# when we compare the two graphs, we can tell whether high sales in a segment
# are driven by:
# more customers (number of customers graph looks similar to the sales graph) or
# higher spend per customer (a segment has high sales but relatively few customers)

# higher sales may also be driven by more units of chips being bought per customer
averageUnits <- data[, .(customerAvg=sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)),
                           by=.(LIFESTAGE, PREMIUM_CUSTOMER)][order(-customerAvg)]
ggplot(averageUnits, aes(x=LIFESTAGE, y=customerAvg, fill=PREMIUM_CUSTOMER)) +
  geom_bar(stat="identity", position="dodge") + 
  labs(x="Lifestage", y="Average units per customer", 
       title="Average number of units per customer by lifestage and premium customer segment",
       fill="Premium Customer") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5))
# older and young families in general buy more chips per customer

# average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
averagePrice <- data[, .(priceAvg=sum(TOT_SALES)/sum(PROD_QTY)),
                     by=.(LIFESTAGE, PREMIUM_CUSTOMER)][order(-priceAvg)]
ggplot(averagePrice, aes(x=LIFESTAGE, y=priceAvg, fill=PREMIUM_CUSTOMER)) +
  geom_bar(stat="identity", position="dodge") + 
  labs(x="Lifestage", y="Average price per unit", 
       title="Average price per unit by lifestage and premium customer segment",
       fill="Premium Customer") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5))
# mainstream midage and young singles and couples are more willing to pay more
# per packet of chips compared to their budget and premium counterparts. This
# may be due to premium shoppers being more likely to buy healthy snacks and 
# when they buy chips, this is mainly for entertainment purpossees rather than 
# their own consumption. This is also supported by there being fewer premium
# midage and young singles and couples buying chips compared to their mainstream
# counterparts.

# as the difference in average price per unit is not large, we can check if this
# difference is statistically significant.
# independent t-test between mainstream vs premium and budget midage and young
# singles and couples

# perform necessary filters of the different categories
mainstreamYoungMid <- data[PREMIUM_CUSTOMER == "Mainstream" &
                             LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES"),
                           TOT_SALES/PROD_QTY]
premiumBudgetYoungMid <- data[PREMIUM_CUSTOMER %in% c("Premium", "Budget") & 
                                LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES"),
                              TOT_SALES/PROD_QTY]
testResult <- t.test(mainstreamYoungMid, premiumBudgetYoungMid, alternative="greater")
print(testResult)
# the unit price for mainstream, young and midage singles and couples are 
# significantly higher than that of budget or premium young and midage singles 
# and couples

# are there brands that mainstream young singles/couples prefer more than others
mainstreamBrands <- data[PREMIUM_CUSTOMER == "Mainstream" & 
                           LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES"),
                         .(mainstreamCount=.N), by=BRAND]
otherBrands <- data[!PREMIUM_CUSTOMER == "Mainstream" & 
                           LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES"),
                         .(otherCount=.N), by=BRAND]

# merge the two
brandAffinity <- merge(mainstreamBrands, otherBrands, by="BRAND")
# affinity score
brandAffinity[, affinityScore := (mainstreamCount/sum(mainstreamCount)) / 
                (otherCount/sum(otherCount))]
# sort by affinity score
brandAffinity <- brandAffinity[order(-affinityScore)]
print(brandAffinity)

ggplot(brandAffinity, aes(x=reorder(BRAND, affinityScore), y=affinityScore)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Brand", y="Affinity score", 
       title="Brand affinity for mainstream young and midage singles/couples") +
  geom_hline(yintercept=1, colour="red", linetype="dashed") +
  theme_minimal()
# score > 1: this segment buys this brand more than the rest of the population (preference)
# score = 1: this segment buys this brand at the same rate as everyone else
# score < 1: this segment buys this brand less than the rest of the population

# let's do the same for packet size
mainstreamPackSize <- data[PREMIUM_CUSTOMER == "Mainstream" & 
                           LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES"),
                         .(mainstreamCount=.N), by=PACK_SIZE]
otherPackSize <- data[!PREMIUM_CUSTOMER == "Mainstream" & 
                      LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES"),
                    .(otherCount=.N), by=PACK_SIZE]

# merge the two
packSizeAffinity <- merge(mainstreamPackSize, otherPackSize , by="PACK_SIZE")
# affinity score
packSizeAffinity[, affinityScore := (mainstreamCount/sum(mainstreamCount)) / 
                (otherCount/sum(otherCount))]
# sort by affinity score
packSizeAffinity <- packSizeAffinity[order(-affinityScore)]
print(packSizeAffinity)

ggplot(packSizeAffinity, aes(x=reorder(PACK_SIZE, affinityScore), y=affinityScore)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(x="Pack size", y="Affinity score", 
       title="Pack size affinity for mainstream young and midage singles/couples") +
  geom_hline(yintercept=1, colour="red", linetype="dashed") +
  theme_minimal()
