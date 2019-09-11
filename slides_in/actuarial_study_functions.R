# The function will take in 2 arguments
# data: claims data for all insurers
# insurerCode: the GR code of insurer that we want to analyze
compileData <- function(data, insurerCode){
  library(lubridate)
  # GRCODE represents the code assigned to individual insurers by NAIC
  data$GRCODE <- as.factor(data$GRCODE)
  
  # Let's analyze insurer with GRCODE = insurerCode
  insurerData <- subset(data, GRCODE == insurerCode)
  
  # Convert columns to date
  insurerData$AccidentDate <- as.Date(insurerData$AccidentDate,
                                      origin = "1899-12-30")
  insurerData$TransactionDate <- as.Date(insurerData$TransactionDate,
                                         origin = "1899-12-30")
  
  # Calculate accident years and development ages
  insurerData$AccidentYear <- year(insurerData$AccidentDate)
  insurerData$DevAge <- 12*(year(insurerData$TransactionDate)-
                              year(insurerData$AccidentDate)+1)
  
  # Output compiled data
  return(insurerData)
}

# create function to create triangles
createTriangles <- function(data, triColName, triType){
  # create triangle matrix
  accYears <- sort(unique(data$AccidentYear))
  devAges <- sort(unique(data$DevAge))
  tri <- matrix(nrow = length(accYears), ncol = length(devAges),
                dimnames = list(accYears, devAges))
  
  # a matrix has different attributes
  # you can add a comment using the comment attribute
  # add comment on type of triangle
  comment(tri) <- triType
  
  # create cumulative paid loss triangle
  for(i in 1:length(accYears)){
    for(j in 1:(length(devAges)-i+1)){
      tri[i,j] <- as.numeric(subset(data,
                                    AccidentYear == accYears[i] & DevAge == devAges[j],
                                    triColName))
      
    }
  }
  
  return(tri)
}

# create function to get earned premium
getEarnPrem <- function(data){
  # get earned premium for accident years
  accYears <- sort(unique(data$AccidentYear))
  earnPrem <- sapply(accYears, 
                     function(x) unique(data$EarnedPremNet[data$AccidentYear == x]))
  # totals
  earnPrem <- c(earnPrem, sum(earnPrem))
  # add row names
  names(earnPrem) <- c(accYears, "Total")
  # output
  earnPrem  
}

# function to get last diagonal of a triangle, in vector form
# this is done by taking the tail non-NA value for each row
lastDiag <- function(tri) {
  apply(tri, MARGIN=1, FUN=function(x) tail(na.omit(x),1))
}


# function to get age-to-age triangle based on an input triangle
ataTri <- function(tri) {
  sapply(2:ncol(tri), function(x) tri[,x]/tri[,x-1])
}

# function to get unweighted A-A all year avg
avgStr <- function(aaTri) {
  apply(aaTri, MARGIN=2, FUN=mean, na.rm=TRUE)
}

# function to get weighted A-A on a loss triangle
avgWtd <- function(tri) {
  aa <- ataTri(tri)
  tri_x_last <- tri[, 1:(ncol(tri)-1)]
  wtd <- aa * tri_x_last
  # mask for values to consider
  wtd_cell <- (!is.na(wtd))*1
  return(apply(wtd, MARGIN=2, FUN=sum, na.rm=TRUE) / 
           apply(wtd_cell * tri_x_last, MARGIN=2, FUN=sum, na.rm=TRUE))
}

# Chainladder development method
# Arguments:
# triangle = triangle matrix object to run the development method on
# paidTri = paid loss triangle matrix object to get unpaid loss
# tail (optional, default value = 1)
#         = tail factor to use in development method
# wtdAvg (optional, default value is not to use wtdAvg)
#         = boolean vector indicating use of weighted averages for each dev age
# Return:
#   list of results, including A-A vector, A-U vector, loss to date, 
devMethod <- function(triangle, paidTri, tail=1, wtdAvg=rep(FALSE, ncol(triangle))){
  # data frame to store dev method results
  results <- data.frame(accYear = c(row.names(triangle), "Total"),
                        latestLoss = NA, latestPaid = NA,
                        ata = NA, atu = NA, ult = NA, unpaid = NA)
  
  # get latest triangle losses and paid losses
  results$latestLoss <- c(lastDiag(triangle), NA)
  results$latestPaid <- c(lastDiag(paidTri), NA)
  
  # create ATA triangle
  ataTriangle <- ataTri(triangle)
  row.names(ataTriangle) <- row.names(triangle)
  colnames(ataTriangle) <- colnames(triangle)[-ncol(triangle)]
  
  # wtd all year avg
  ataWtd <- c(tail, rev(avgWtd(triangle)))
  # straight all year avg
  ataStr <- c(tail, rev(avgStr(ataTriangle)))
  # get selected ATA
  results$ata <- c(ifelse(wtdAvg, ataWtd, ataStr), NA)
  # get selected ATU
  results$atu <- cumprod(results$ata)
  
  # ultimate loss
  results$ult <- results$latestLoss*results$atu
  # unpaid loss
  results$unpaid <- results$ult - results$latestPaid
  
  # totals row - sums everything but last row (only for to-date and ultimate columns)
  results[nrow(results), c(2:3, 6:7)] <- sapply(c(2:3, 6:7),
                                                function(x) sum(results[-nrow(results), x]))
  
  # set column names
  colnames(results) <- c("Accident Year", paste("Latest", comment(triangle)),
                         "Latest Paid", "Age-to-Age", "Age-to-Ult",
                         "Ultimate Loss", "Unpaid Loss")
  
  # output results
  return(results)
}

# IELR method
# Arguments:
# rptdDev = data frame object with reported development method results
# paidDev = data frame object with paid development method results
# earnPrem = earned premium
# rptdWt (optional, default value = 0.5)
#        = weight between 0 and 1 to use for reported ultimates 
#        = 1 implies full weight given to reported ultimates
#        = 0 implies full weight given to paid ultimates
#        = 0.5 implies equal weight given to paid and reported ultimates
# nyears (optional, default value = 5)
#        = number of recent years to include in IELR straight average
# excludeYears (optional, default value = 1)
#        = number of recent years to exclude in IELR straight average
IELR <- function(rptdDev, paidDev, earnPrem, rptdWt = 0.5, nyears=5, excludeYears=1){
  # data frame to store ielr method results
  results <- data.frame(accYear = names(earnPrem),
                        latestPaid = NA, earnPrem = earnPrem,
                        devLoss = NA, ultLR = NA, selLR = NA,
                        ult = NA, unpaid = NA)
  
  # get weighted ultimate losses for IELR
  results$devLoss <- rptdWt*rptdDev[,c("Ultimate Loss")] + 
    (1-rptdWt)*paidDev[,c("Ultimate Loss")]
  
  
  # store latest loss, latest paid loss and earned premium
  results$latestPaid <- paidDev[, c("Latest Paid")]
  
  # calculate ultimate loss ratio
  results$ultLR <- results$devLoss / results$earnPrem
  
  # selected loss ratio as straight average of nyears LR
  ## calculate position of latest AY to include in IELR calculation
  latestAYPos <- nrow(results)-1-excludeYears
  results$selLR <- mean(results$ultLR[seq(latestAYPos-nyears+1, latestAYPos)])
  
  # ultimate and unpaid loss
  results$ult <- results$earnPrem * results$selLR
  results$unpaid <- results$ult - results$latestPaid
  
  # set column names
  colnames(results) <- c("Accident Year", "Latest Paid", "Earned Premium",
                         "Development Method Ultimate", "Ultimate Loss Ratio", "Selected Loss Ratio",
                         "IELR Ultimate Loss", "IELR Unpaid Loss")
  
  # output results
  return(results)
}

# B-F Method
# Arguments:
# devResults = data frame object with development method results
# ielrResults = data frame object with IELR method results
# triType = "Paid" to use paid development patterns
#         = "Reported" to use reported development patterns
BF <- function(devResults, ielrResults, triType){
  # data frame to store ielr method results
  results <- data.frame(accYear = devResults$`Accident Year`,
                        latestLoss = NA, latestPaid = NA,
                        devPct = NA, devPctMod = NA, devEst = NA, ielrEst = NA,
                        ult = NA, unpaid = NA)
  
  # latest loss and latest paid loss
  results$latestLoss <- devResults[,paste("Latest", triType)]
  results$latestPaid <- devResults[,"Latest Paid"]
  
  # reported/paid %
  results$devPct <- 1/devResults[,"Age-to-Ult"]
  
  # B-F definition of credibility = reported/paid % is not valid
  # if reported/paid % > 1 i.e. when there is negative development
  results$devPctMod <- ifelse(results$devPct > 1, 1, results$devPct)
  
  # dev/ielr method estimates
  results$devEst <- devResults[, c("Ultimate Loss")]
  results$ielrEst <- ielrResults[, c("IELR Ultimate Loss")]
  
  # calculate B-F ultimate and unpaid loss
  results$ult <- results$devEst*results$devPctMod + results$ielrEst*(1-results$devPctMod)
  results$ult[nrow(results)] <- sum(results$ult[-nrow(results)])
  results$unpaid <- results$ult - results$latestPaid
  
  # set column names
  colnames(results) <- c("Accident Year", "Latest Loss", "Latest Paid",
                         paste(triType, "%"), paste(triType, "% for B-F"), "Development Method Ultimate",
                         "IELR Method Ultimate", "B-F Ultimate Loss",
                         "B-F Unpaid Loss")
  
  # output results
  return(results)
}