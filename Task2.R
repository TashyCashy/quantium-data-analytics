install.packages("data.table")
install.packages("ggplot2")
install.packages("tidyr")

library(data.table)
library(ggplot2)
library(tidyr)

filePath <- "C:/Users/natma/OneDrive - University of Cape Town/PROJECTS/Statistics/Quantium Analytics/" 
data <- fread(paste0(filePath, "QVI_data.csv"))

# set themes for plots
theme_set(theme_bw())
theme_update(plot.title=element_text(hjust=0.5))

# select control stores
# create a new column by 'YEARMONTH' by formatting the DATE column into a 6
# digit string converted to integer for ease of use
data[, YEARMONTH := as.integer(format(DATE, "%Y%m"))]

# for each store and month, calculate total sales, number of customers, 
# transactions per customer, chips per customer and the average price per unit
measureOverTime <- data[, .(totSales = sum(TOT_SALES), 
                            nCustomers = uniqueN(LYLTY_CARD_NBR), 
                            nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY/uniqueN(TXN_ID)),
                            priceAvg = sum(TOT_SALES)/sum(PROD_QTY)), by = .(STORE_NBR, YEARMONTH)][order(-totSales)]

# filter to the pre-trial period and stores with full observation periods
storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% storesWithFullObs, ]

# function to calculate correlation for a measure, looping through each control store
calculateCorrelation <- function(inputTable, metricColumn, storeComparison) {
  # empty data table to store results
  calcCorrTable <- data.table(Store1=numeric(), Store2=numeric(), corr_measure=numeric())
  # get list of stores to compare against
  storeNumbers <- unique(inputTable[, STORE_NBR])
  # looping through each control store
  for (i in storeNumbers) {
    # get metric for trial store
    calculatedMeasure <- cor(inputTable[STORE_NBR == storeComparison, get(metricColumn)],
                             inputTable[STORE_NBR == i, get(metricColumn)])
    # append result to table
    calcCorrTable <- rbind(calcCorrTable, data.table(Store1=storeComparison, Store2=i, corr_measure=calculatedMeasure))
  }
  return(calcCorrTable)
}

# function to calculate standardised magnitude distance for a measure, looping
# through each control store
calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
  calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), 
                             YEARMONTH = numeric(), measure = numeric())
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison,
                                   "Store2" = i,
                                   "YEARMONTH" = inputTable[STORE_NBR == storeComparison, YEARMONTH],
                                   "measure" = abs(inputTable[STORE_NBR == storeComparison, eval(metricCol)]
                                                   - inputTable[STORE_NBR == i, eval(metricCol)]))
    calcDistTable <- rbind(calcDistTable, calculatedMeasure)
  }
  
  # standardise the magnitude distance so that the measure ranges from 0 to 1
  minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)),
                              by = c("Store1", "YEARMONTH")]
  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)),
                              by = .(Store1, Store2)]
  return(finalDistTable)
}

# use the created functions to calculate correlations against store 77 using
# total sales and number of customers
for (trial_store in c(77, 86, 88)) {
  corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
  corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)
  
  magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
  magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)
  
  # create a combinded score composed of correlation and magnitude, by first 
  # merging the correlations table with the magnitude table
  corr_weight <- 0.5
  score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1", "Store2"))[,
                scoreNSales := corr_weight*corr_measure + (1-corr_weight)*mag_measure]
  score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c("Store1", "Store2"))[,
                scoreNCust := corr_weight*corr_measure + (1-corr_weight)*mag_measure]
  
  # combine the scores across the drivers by first merging the sales scores and 
  # customer scores into a single table
  scoreControl <- merge(score_nSales, score_nCustomers, by = c("Store1", "Store2"))
  scoreControl[, finalControlScore := scoreNSales*0.5 + scoreNCust*0.5]
  # select the control store with the highest score (excluding the trail store itself)
  bestControlStore <- scoreControl[Store2 != trial_store][order(-finalControlScore)][1, Store2]
  # print the top 5 most similar matching stores
  print(head(scoreControl[Store2 != trial_store][order(-finalControlScore)], 5))
  
  # select the most appropriate control store for trial store 77 by finding the 
  # store with the highest final score
  controlStore <- scoreControl[Store2 != trial_store][order(-finalControlScore)][1, Store2]
  
  # visual checks on trends based on the drivers
  measureOverTimeSales <- measureOverTime
  pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
                                                           "Trial",
                                                           ifelse(STORE_NBR == controlStore,
                                                                  "Control", "Other stores"))
                                    ][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
                                      ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
                                        ][YEARMONTH < 201903, ]
  
  print(ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
    geom_line() +
    labs(x = "Month of operation", y = "Total sales", title = paste("Total sales by month - Trial store", trial_store)))
  
  # visual checks on customer count trends by comparing the trial store to the 
  # control store and other stores
  measureOverTimeCustomers <- measureOverTime
  pastCustomers <- measureOverTimeCustomers[, Store_type := ifelse(STORE_NBR == trial_store,
                                                                   "Trial",
                                                                   ifelse(STORE_NBR == controlStore,
                                                                          "Control", "Other stores"))
                                            ][, nCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")
                                              ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
                                                ][YEARMONTH < 201903, ]
  
  print(ggplot(pastCustomers, aes(TransactionMonth, nCustomers, color = Store_type)) +
    geom_line() + 
    labs(x = "Month of operation", y = "Number of customers", title = paste("Number of customers by month - Trial store", trial_store))) 
    
  # scale pre-trial control sales to match pre-trial trial store sales
  scalingFactorControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(totSales)
                                                ]/preTrialMeasures[STORE_NBR == controlStore & YEARMONTH < 201902, sum(totSales)]
  
  # apply the scaling factor
  measureOverTimeSales <- measureOverTime
  scaledControlSales <- measureOverTimeSales[STORE_NBR == controlStore, ][ ,
                                                                           controlSales := totSales*scalingFactorControlSales]
  
  # calculating percentage difference between scaled control sales and trial sales
  trialSales <- measureOverTime[STORE_NBR == trial_store, .(YEARMONTH, totSales)]
  percentageDiff <- merge(scaledControlSales, trialSales, by = "YEARMONTH")[,
                                                                            percentageDiff := abs(totSales.x - totSales.y)/totSales.x]
  
  # let's see if the difference is significant
  # our null hypothesis is that the trial period is the same as the pre-trial period, 
  # take the standard deviation based on the scaled percentage difference in the pre-trial period
  
  # standard deviation based on pre-trial period
  stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])
  df <- 7
  
  # t-values for the trial months
  # 95th percentile of the t distribution 
  print(percentageDiff[, tValue := (percentageDiff-0)/stdDev
                 ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
                   ][, .(YEARMONTH, TransactionMonth, percentageDiff, tValue, 
                         threshold = qt(0.95, df = df),
                         sigDiff = tValue > qt(0.95, df = df))])
  # the increase in sales in the trial store in March and April is statistically
  # greater than in the control store
  
  # new variables, Store_type, totSales and TransactionMonth in the data table
  measureOverTimeSales <- measureOverTime
  pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                    ifelse(STORE_NBR == controlStore, "Control", "Other stores"))
                                    ][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
    ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
      ][Store_type %in% c("Trial", "Control"), ] 
  
  # control store 95th percentile
  pastSalesControls95 <- pastSales[Store_type == "Control",
                                   ][, totSales := totSales*(1 + stdDev*2)
                                     ][, Store_type := "Control 95th % confidence interval"]
  
  # control store 5th percentile
  pastSalesControls5 <- pastSales[Store_type == "Control",
                                   ][, totSales := totSales*(1 - stdDev*2)
                                     ][, Store_type := "Control 5th % confidence interval"]
  
  trialAssessment <- rbind(pastSales, pastSalesControls95, pastSalesControls5)
  
  # plot in one graph
  # the rectangle in the plot highlights the trial period
  print(ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
    geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901,],
              aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax =
                    Inf, color = NULL), show.legend = FALSE) + 
    geom_line() +
    labs(x = "Month of operation", y = "Total sales", title = paste("Total sales by month - Trial store", trial_store)))
  # trial in store 77 is significantly different to its control store in the trial 
  # period as the trial store performance lies outside the 5% to 95% confidence 
  # interval of the control store in two of the three trial months
  
  # LET'S ASSESS FOR NUMBER OF CUSTOMERS TOO
  
  # scale pre-trial control sales to match pre-trial trial store customer numbers
  scalingFactorControlCustomers <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(nCustomers)
                                               ]/preTrialMeasures[STORE_NBR == controlStore & YEARMONTH < 201902, sum(nCustomers)]
  
  # apply the scaling factor
  measureOverTimeCustomers <- measureOverTime
  scaledControlCustomers <- measureOverTimeCustomers[STORE_NBR == controlStore, ][ ,
                                                                           controlCustomers := nCustomers*scalingFactorControlCustomers]
  
  # calculating percentage difference between scaled control customers and trial customers
  trialCustomers <- measureOverTime[STORE_NBR == trial_store, .(YEARMONTH, nCustomers)]
  percentageDiffCustomers <- merge(scaledControlCustomers, trialCustomers, by = "YEARMONTH")[,
                                                                            percentageDiff := abs(nCustomers.x - nCustomers.y)/nCustomers.x]
  
  # let's see if the difference is significant
  # our null hypothesis is that the trial period is the same as the pre-trial period, 
  # take the standard deviation based on the scaled percentage difference in the pre-trial period
  
  # standard deviation based on pre-trial period
  stdDevCustomers <- sd(percentageDiffCustomers[YEARMONTH < 201902, percentageDiff])
  df <- 7
  
  # t-values for the trial months
  # 95th percentile of the t distribution 
  print(percentageDiffCustomers[, tValue := (percentageDiff-0)/stdDevCustomers
                          ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
                            ][, .(YEARMONTH, TransactionMonth, percentageDiff, tValue, 
        threshold = qt(0.95, df = df),
        sigDiff = tValue > qt(0.95, df = df))])
  
  # new variables, Store_type, nCustomers and TransactionMonth in the data table
  measureOverTimeCustomers <- measureOverTime
  pastCustomers <- measureOverTimeCustomers[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                           ifelse(STORE_NBR == controlStore, "Control", "Other stores"))
                                            ][, nCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")
                                              ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
                                                ][Store_type %in% c("Trial", "Control"), ]
  
  # control store 95th percentile
  pastCustomersControls95 <- pastCustomers[Store_type == "Control",
                                           ][, nCustomers := nCustomers*(1 + stdDevCustomers*2)
                                             ][, Store_type := "Control 95th % confidence interval"]
  
  # control store 5th percentile
  pastCustomersControls5 <- pastCustomers[Store_type == "Control",
                                          ][, nCustomers := nCustomers*(1 - stdDevCustomers*2)
                                            ][, Store_type := "Control 5th % confidence interval"]
  
  trialAssessmentCustomers <- rbind(pastCustomers, pastCustomersControls95, pastCustomersControls5)
  
  # plot in one graph
  # the rectangle in the plot highlights the trial period
  print(ggplot(trialAssessmentCustomers, aes(TransactionMonth, nCustomers, color = Store_type)) +
    geom_rect(data = trialAssessmentCustomers[YEARMONTH < 201905 & YEARMONTH > 201901,],
              aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, ymax =
                    Inf, color = NULL), show.legend = FALSE) + 
    geom_line() +
    labs(x = "Month of operation", y = "Number of customers", title = paste("Number of customers by month - Trial store", trial_store)))
}
