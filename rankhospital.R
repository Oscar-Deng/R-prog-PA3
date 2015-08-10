rankall <- function(outcome, rnk = "best") {
  #Read outcome database
  datas <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  const <- subset(datas[,c(2,7,11,17,23)])
  valid_outcomes <- c("heart attack","heart failure","pneumonia") # outcome must be inside, else stop
  lstate <- const$State #list out state name
  lstate_uni <- unique(lstate)
  state_arr <- sort(lstate_uni)
  arr_len <- length(state_arr)
  hosp <- rep("",arr_len)
  ## Check that state and outcome are valid
    if(!is.element(outcome,valid_outcomes)){stop("invalid outcome")}
  else{
    for(i in 1:arr_len){
      filt_state <- const[const[,2] == state_arr[i], ] 
      if(outcome == "heart attack"|outcome == "1"){hosp[i] <- rangeNum2(filt_state,3,rnk)}  ## For each state, find the hospital of the given rank
      else if (outcome == "heart failure"|outcome == "2"){hosp[i] <- rangeNum2(filt_state,4,rnk)}
      else if (outcome == "pneumonia"|outcome == "3"){hosp[i] <- rangeNum2(filt_state,5,rnk)}
    }
    DF <- data.frame(hospital = hosp,state = state_arr)
    result <- DF
    return(result)  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  }}
# tests
# head(rankall("heart attack", 20), 10)
# tail(rankall("pneumonia", "worst"), 3)
# tail(rankall("heart failure"), 10)
#.............................................................#
rangeNum1 <- function(const, col_no, state, rnk){ # dataset, col number, state name, rank no.
  rangeRank <- function(filt_state,outcome,rnk = "best"){
    result <- filt_state[,1][order(outarr,filt_state[,1])[rnk]]
    return(result)
  }
  #
  filt_state <- const[const[,2]==state, ] # data that meets the state
  # col number(kind of dataset) that we want to pick
  outarr <- filt_state[,col_no]
  len <- dim(filt_state[!is.na(outarr), ])[1]
  if (rnk == "worst"){ rank <- rangeRank(filt_state,outarr,len)} # if use worst as rank, then dim rank order as below
  else if(rnk>len){rank <- NA}
  else{rank <- rangeRank(filt_state,outarr,rnk)}
  result <- rank
  return(result)
  }
rangeNum2 <- function(const, col_no, rnk){ # dataset, col number, state name, rank no.
  #
  rangeRank <- function(const,outarr,rnk){
    result <- const[,1][order(outarr,const[,1])[rnk]]
    return(result)
  }
  #
  # col number(kind of dataset) that we want to pick
  outarr <- as.numeric(const[,col_no])
  len <- dim(const[!is.na(outarr), ])[1]
  if (rnk == "worst"){ rank <- rangeRank(const,outarr,len)} # if use worst as rank, then dim rank order as below
  else if(rnk == "best"){rank <- rangeRank(const,outarr,1)}
  else if(rnk>len){rank <- NA}
  else{rank <- rangeRank(const,outarr,rnk)}
  result <- rank
  return(result)
}


rankhospital <- function(state,outcome,rnk = "best"){
  # Please insert correct state name
  # You can insert number 1 in outcome to present "heart attack"
  #;2 for "heart failure";3 for "pneumonia"
  #Read outcome database
  datas <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #Check state & outcome valid
  const <- subset(datas[,c(2,7,11,17,23)])
  const[,3] <- as.numeric(const[,3])  # heart attack
  const[,4] <- as.numeric(const[,4])  # heart failure
  const[,5] <- as.numeric(const[,5])  # pneumonia
  filt_state <- const[const[,2] == state, ] # data that meets the state
  #colnames of const: 1.hosp names, 2.state, 3.heart attack, 4.heart failure, 5.pneumonia
  lstate <- const$State #list out state name
  lstate_uni <- unique(lstate)
  valid_outcomes <- c("heart attack","heart failure","pneumonia") # outcome must be inside, else stop
  
  #if state & outcome doesn't meet condition, stop
  #using look function
  if (rnk == "best"){ rank <- best(state,outcome)}
  else{
  if(outcome == "heart attack"|outcome == "1"){rank <- rangeNum1(const,3,state,rnk)}
  else if (outcome == "heart failure"|outcome == "2"){rank <- rangeNum1(const,4,state,rnk)}
  else if (outcome == "pneumonia"|outcome == "3"){rank <- rangeNum1(const,5,state,rnk)}
  else{
    if (!is.element(state,lstate_uni)){stop("invalid state")}
    else if(!is.element(outcome,valid_outcomes)){stop("invalid outcome")}
  }
    # result here
    result <- rank

    return(result)
  }}
# How to use this func?
# > source("rankhospital.R")
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA
#.............................................................# 
  best <- function(state,outcome){
    # Please insert correct state name
    # You can insert number 1 in outcome to present "heart attack"
    #;2 for "heart failure";3 for "pneumonia"
    
    
    # First dim a function "look" to look for hospital name
    look <- function(const,col_no,state){ # dataset, col number, state name
      filt_state <- const[const[,2]==state,] # data that meets the state
      out <- filt_state[,col_no] # col number(kind of dataset) that we want to pick
      min <- min(out,na.rm = TRUE) 
      min.index <- which(out == min) 
      hospName <- filt_state[min.index,1] # find the hospital name that meets all condition
      return(hospName)}
    
    #Read outcome database
    datas <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    #Check state & outcome valid
    const <- subset(datas[,c(2,7,11,17,23)])
    const[,3] <- as.numeric(const[,3])  # heart attack
    const[,4] <- as.numeric(const[,4])  # heart failure
    const[,5] <- as.numeric(const[,5])  # pneumonia
    #colnames of const: 1.hosp names, 2.state, 3.heart attack, 4.heart failure, 5.pneumonia
    lstate <- const$State #list out state name
    lstate_uni <- unique(lstate)
    valid_outcomes <- c("heart attack","heart failure","pneumonia") # outcome must be inside, else stop
    
    #if state & outcome doesn't meet condition, stop
    #using look function
    if(outcome == "heart attack"|outcome == "1"){return(look(const,3,state))}
    else if (outcome == "heart failure"|outcome == "2"){return(look(const,4,state))}
    else if (outcome == "pneumonia"|outcome == "3"){return(look(const,5,state))}
    else{
      if (!is.element(state,lstate_uni)){stop("invalid state")}
      else if(!is.element(outcome,valid_outcomes)){stop("invalid outcome")}
    }
  }

# How to test this file:
# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome
#