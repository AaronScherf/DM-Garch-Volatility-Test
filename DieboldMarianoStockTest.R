# GARCH Model Test
  # Created by Aaron Scherf
  # Final Version: 01.07.18

FirstRun <- FALSE

#### Installing and Loading Packages ####
if(FirstRun==TRUE) {
install.packages("rugarch")
install.packages("quantmod")
install.packages("forecast")
install.packages("dplyr")
install.packages("InformationValue")
install.packages("lattice")
install.packages("xlsx")
install.packages("ggplot2")
install.packages("progress")
install.packages("ggthemes")
}  

library("quantmod")
library("forecast")
library("dplyr")
library("InformationValue")
library("lattice")
library("xlsx")
library("ggplot2")
library("rugarch")
library("progress")
library("ggthemes")


#### ####

rm(list = ls())

# Set Working Directory

WD <- "C:/Users/Aaron/Desktop/DieboldMarianoTest"
setwd(WD)

#### Creating Directories ####

Charts <- paste(WD,"/Charts", sep = "")
dir.create(file.path(Charts), showWarnings = FALSE)
Charts.Values <- paste(WD,"/Charts/Values", sep = "")
dir.create(file.path(Charts.Values), showWarnings = FALSE)
Charts.ErrorDensity <- paste(WD,"/Charts/ErrorDensity", sep = "")
dir.create(file.path(Charts.ErrorDensity), showWarnings = FALSE)
Charts.Autocorrelation <- paste(WD,"/Charts/Autocorrelation", sep = "")
dir.create(file.path(Charts.Autocorrelation), showWarnings = FALSE)
Charts.LossDiff <- paste(WD,"/Charts/LossDifferential", sep = "")
dir.create(file.path(Charts.LossDiff), showWarnings = FALSE)
Charts.ForecastComp <- paste(WD,"/Charts/ForecastComparison", sep = "")
dir.create(file.path(Charts.ForecastComp), showWarnings = FALSE)
#### ####

#### Downloading Stock Data and Creating Dataframe ####

dataset.volatility <- read.csv('Volas_long_eng.csv', sep = ";", header = TRUE, row.names = NULL, na.strings = ".")
  dataset.volatility$dd <- NULL
  dataset.volatility$dow <- NULL

dataset.volatility$date <- as.Date(dataset.volatility$date)
  
startDate <- min(dataset.volatility$date)  
endDate <- max(dataset.volatility$date)+1

Stocks <- colnames(dataset.volatility)[-1]

dataset.volatility <- as.xts(dataset.volatility, order.by = dataset.volatility$date)

for (Ticker in Stocks) {
  getSymbols(Ticker, from = startDate, to = endDate, "getSymbols.warning4.0" = FALSE)
}

Stocks.Upper <- toupper(Stocks)
Stocks.DF <- as.data.frame(Stocks.Upper)
Num.Stocks <- as.numeric(nrow(Stocks.DF))

#### ####

#### Set Cutoff Date for Train-Test Split ####
  cutoffDate <- endDate-350
  cutoffDate.char <- as.character(cutoffDate)
  startDate.char <- as.character(startDate)
  endDate.char <- as.character(endDate)

# Saving Workspace Image in case of Errors  
save.image(file = "Stocks.RData") 

#### ####

# Decide to Output Plots or Not
RunPlots <- FALSE


#### Loop Forecast and Reporting Process over each Stock ####

    Output <- data.frame(Stock.Name=character(),
                         GARCH.MAE=double(),
                         RW.MAE=double(),
                         Forecast.Winner=character(),
                         Loss.Diff.Mean=double(),
                         Loss.Diff.Var=double(),
                         DM.Stat=double(),
                         DM.Horizon=double(),
                         DM.Power=double(),
                         stringsAsFactors = FALSE)


pb <- progress_bar$new(
format = " Working [:bar] :percent in :elapsed ",
total = Num.Stocks, clear = FALSE, width= 60)


for (i in 1:Num.Stocks) {
StockPick <- get((Stocks.Upper)[i])
StockPick <- as.xts(StockPick)

 # mypath.Values <- as.character(file.path(Charts.Values,paste("Values",Stocks.Upper[i], ".jpeg", sep = "")))
 # jpeg(file = mypath.Values)
 # StockValuesChart <- chartSeries(StockPick, name = paste(Stocks.Upper[i],"Values", sep = " "), plot.new())
 # print(StockValuesChart)
 # dev.off()

StockPick.Char <- as.character(Stocks[i])

StockPick$Volatility <- dataset.volatility[,i+1]

StockPick <- na.omit(StockPick)


  # Splitting into Training and Test Data
  Stock.train <- as.xts(window(StockPick, start=startDate.char, end=cutoffDate.char))
  Stock.test <- as.xts(window(StockPick, start=cutoffDate.char, end=endDate.char))
  
  Test.Rows <- max(1:nrow(Stock.test))
  Test.Rows <- as.numeric(Test.Rows)
  
  # GARCH Model using ugarch package

  ug_spec <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,1)), distribution.model = "std")

  ugfit <- ugarchfit(spec = ug_spec, data = Stock.train$Volatility)
  
  ugfore <- ugarchforecast(ugfit, n.ahead = Test.Rows)
  ugfore.series <- as.ts(ugfore@forecast$seriesFor)

    # Evaluating GARCH Forecast Accuracy against Test Data

  GARCH.Acc <- accuracy(f = ugfore.series, x = Stock.test$Volatility)
  
  # Calculating Errors of GARCH Model
    
  GARCH.series <- as.numeric(ugfore.series)
  
  GARCH.Error <- (GARCH.series - Stock.test$Volatility)
  
  # Calculating Absolute and Mean Absolute Errors of GARCH Model
   
  GARCH.Error <- as.numeric(GARCH.Error)
  GARCH.AbsError <- abs(GARCH.Error)
  GARCH.MAE <- mean(abs(GARCH.Error))
  
  # Exporting Density Plot of GARCH Errors
  if (RunPlots==TRUE) {  
  mypath.GARCH.Density <- as.character(file.path(Charts.ErrorDensity,paste("GARCH_Error_Density",Stocks.Upper[i], ".jpeg", sep = "")))
  jpeg(file=mypath.GARCH.Density)
  GARCH.DensityPlot <- densityplot(GARCH.Error,
            main = paste(toupper(StockPick.Char),"Error Density GARCH", sep = " "), ylab = "Error Density",
            plot.new())
  print(GARCH.DensityPlot)
  dev.off()
  }

  
  # Random Walk Forecast
  
  RandomWalk <- rwf(Stock.train$Volatility, h = Test.Rows)

  # Calculating Error of Random Walk Forecast
  
  RandomWalk.Mean <- as.numeric(RandomWalk$mean)
  RandomWalk.Error <- (RandomWalk.Mean - Stock.test$Volatility)

  # Calculating Absolute and Mean Absolute Errors of RandomWalk Model
  
  RandomWalk.Error <- as.numeric(RandomWalk.Error)
  RandomWalk.AbsError <- abs(RandomWalk.Error)
  RandomWalk.MAE <- mean(abs(RandomWalk.Error))

  # Exporting Density Plot of Random Walk Errors
    if(RunPlots==TRUE) {  
  mypath.RW.Density <- as.character(file.path(Charts.ErrorDensity,paste("RW_Error_Density",Stocks.Upper[i], ".jpeg", sep = "")))
  jpeg(file=mypath.RW.Density)
  RW.ErrorDensity <- densityplot(RandomWalk.Error,
          main = paste(toupper(StockPick.Char),"Error Density", sep = " "), ylab = "Error Density", plot.new())
  print(RW.ErrorDensity)
  dev.off()
    }
  
  # Evaluating RandomWalk Accuracy against Test Data
  
  RandomWalkAcc <-  accuracy(f = RandomWalk, x = Stock.test$Volatility)

  # Combining Forecasts and Calculating Loss Differential
  
  Forecast.Comparison <- as.data.frame(Stock.test$Volatility)
  Forecast.Comparison$ugfore.series <- ugfore.series
  Forecast.Comparison$RandomWalk.Mean <- RandomWalk.Mean
  Forecast.Comparison$GARCH.AbsError <- GARCH.AbsError
  Forecast.Comparison$RandomWalk.AbsError <- RandomWalk.AbsError
  Forecast.Comparison$Forecast.LossDiff <- ((Forecast.Comparison$GARCH.AbsError) - (Forecast.Comparison$RandomWalk.AbsError))
  Forecast.Comparison$Forecast.LossDiff.Sq <- ((Forecast.Comparison$GARCH.AbsError)^2 - (Forecast.Comparison$RandomWalk.AbsError)^2)

  Forecast.Comparison.Graph <- Forecast.Comparison
  Forecast.Comparison.Graph$Observed.Volatility <- as.numeric(Forecast.Comparison.Graph$Volatility)
  Forecast.Comparison.Graph$Volatility <- NULL
  Forecast.Comparison.Graph$GARCH.Forecast <- as.numeric(Forecast.Comparison.Graph$ugfore.series)
  Forecast.Comparison.Graph$ugfore.series <- NULL
  Forecast.Comparison.Graph$RW.Forecast <- Forecast.Comparison.Graph$RandomWalk.Mean
  Forecast.Comparison.Graph$RandomWalk.Mean <- NULL 
  Forecast.Comparison.Graph$Time <- as.Date(rownames(Forecast.Comparison.Graph))

  # Plotting Autocorrelation Function
    if(RunPlots==TRUE) {
  mypath.ACF <- as.character(file.path(Charts.Autocorrelation,paste("Forecast_Autocorrelation_Function",Stocks.Upper[i], ".jpeg", sep = "")))
  jpeg(file=mypath.ACF)
  ACF.Plot <-  acf(Forecast.Comparison$Forecast.LossDiff, lag.max = NULL,
      type = c("correlation"),
      plot = TRUE, na.action = na.fail, demean = TRUE,
      main = paste(toupper(StockPick.Char),"Autocorrelation Function", sep = " "))
  print(ACF.Plot)
  dev.off()
    }
  
  # Calculating Loss Differential Mean and Variance

  specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

  Forecast.LossDiff.Mean <- mean(Forecast.Comparison$Forecast.LossDiff)
  Forecast.LossDiff.Mean.3 <- specify_decimal(Forecast.LossDiff.Mean, 3)
  Forecast.LossDiff.VarP <- var(Forecast.Comparison$Forecast.LossDiff)
  Forecast.LossDiff.VarP.3 <- specify_decimal(Forecast.LossDiff.VarP, 3)

  
  # Plotting Loss Differential
  if(RunPlots==TRUE) {
  mypath.LossDiff <- as.character(file.path(Charts.LossDiff,paste("Forecast_Loss_Differential",Stocks.Upper[i], ".jpeg", sep = "")))
  
  jpeg(file=mypath.LossDiff)
  LossDiffPlot <-  ggplot(data = Forecast.Comparison.Graph, aes(Time)) +
    geom_line(aes(y = Forecast.LossDiff), linetype = 1) +
    geom_line(aes(y = Forecast.LossDiff.Mean), linetype = 2)
  print(LossDiffPlot + ggtitle(paste(toupper(StockPick.Char),"Loss Differential","\n",
                                     paste(toupper(StockPick.Char),"Mean LD:", Forecast.LossDiff.Mean.3, sep = " "), sep = " ")) +
                        theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank()))
  dev.off()
  }
  # Plotting Forecast Comparison
    if(RunPlots==TRUE) {
  mypath.ForecastComp <- as.character(file.path(Charts.ForecastComp,paste("Forecast_Comparison",Stocks.Upper[i], ".jpeg", sep = "")))
  jpeg(filename=mypath.ForecastComp)
  ForecastComparisonPlot <- ggplot(data = Forecast.Comparison.Graph, aes(Time)) + 
    geom_line(aes(y = Observed.Volatility), linetype = 1) +
    geom_line(aes(y = GARCH.Forecast), linetype = 2) +
    geom_line(aes(y = RW.Forecast), linetype = 3) +
    scale_x_date() + xlab("Time") + ylab("Volatility")
  print(ForecastComparisonPlot + ggtitle(as.character(paste(toupper(StockPick.Char),"Forecast Comparison", sep = " "))) + 
                                theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank()))
  dev.off()
    }


# Comparison of uGARCH and RW forecasts with test data through Diebold Mariano

DM.Horizon <- as.numeric(1)
  
while (DM.Horizon==1) {
for (j in 1:15)  {
DM.Test <- dm.test(GARCH.AbsError, RandomWalk.AbsError, alternative = c("two.sided"), h = Test.Rows,
  power = j);
    DM.Stat <- as.numeric(DM.Test$statistic);
    DM.Horizon <- as.numeric(DM.Test$parameter)[1];
    DM.Power <- as.numeric(DM.Test$parameter)[2];
    if (DM.Horizon != 1) break
}
}
  

    
# Reporting Output as Dataframe

    Forecast.Winner.MAE <- ifelse(GARCH.MAE > RandomWalk.MAE, "RandomWalk", ifelse(GARCH.MAE < RandomWalk.MAE, "GARCH", "Equal"))

    Output[i,1] <- c(StockPick.Char)
    Output[i,2] <- c(GARCH.MAE)
    Output[i,3] <- c(RandomWalk.MAE)
    Output[i,4] <- c(Forecast.Winner.MAE)
    Output[i,5] <- c(Forecast.LossDiff.Mean)
    Output[i,6] <- c(Forecast.LossDiff.VarP)
    Output[i,7] <- c(DM.Stat)
    Output[i,8] <- c(DM.Horizon)
    Output[i,9] <- c(DM.Power)

    
    # Update Progress Bar

    pb$tick()
    Sys.sleep(1 / Num.Stocks)
}  
  
#### ####


  #### Write Output to an XLSX File and Separate Stocks with Errors ####

    write.xlsx(Output, "DM.Test.Output.All.xlsx", sheetName = "Stocks", 
      col.names = TRUE, row.names = TRUE, append = TRUE)

  Output$DM.Stat <- as.factor(Output$DM.Stat)   
  Output$DM.Horizon <- as.factor(Output$DM.Horizon)
  Output$Forecast.Winner <- as.factor(Output$Forecast.Winner)
  Output$DM.Power <- as.factor(Output$DM.Power)
  summary(Output)

  #### ####
