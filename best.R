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
    if (!is.element(state,lstate_uni)){stop("invalid state")}
    else if(!is.element(outcome,valid_outcomes)){stop("invalid outcome")}
    else{if(outcome == "heart attack"|outcome == "1"){return(look(const,3,state))}
    else if (outcome == "heart failure"|outcome == "2"){return(look(const,4,state))}
    else if (outcome == "pneumonia"|outcome == "3"){return(look(const,5,state))}
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
