### function "rankhospital" in script rankhospital.R for hospital outcome data as Part 3 of ProgrammingAssignment3, C. Bleile
### the function reads in data from an unzipped folder under the working directory, subsets the data
### and uses three arguments: the 2-character abbreviated name of a state (state), an outcome (outcome), 
### and the ranking of a hospital in that state for that outcome (num). The function reads the 
### outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has
### the ranking specified by the num argument.
## rankhospital.R function follows:

rankhospital <- function(state="NA", outcome="heart attack", num ="best") {
  
    ## First read in the data from unzipped folder in the working directory.
    OutcomeData <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    ## subset the data.frame to the factors needed and rename columns to easier factor names
    OutcomeSubset <- OutcomeData[c(2, 7, 11, 17, 23)]
    names(OutcomeSubset)[1] <- "Name"
    names(OutcomeSubset)[2] <- "State"
    names(OutcomeSubset)[3] <- "heart attack"
    names(OutcomeSubset)[4] <- "heart failure"
    names(OutcomeSubset)[5] <- "pneumonia"
    
    ## Check that state and outcome are valid
    # Valid State
    ValidState <- unique(OutcomeSubset[, 2]) # Get the list of valid states from the State factor
    if (state %in% ValidState == FALSE) stop("invalid state") # Exit if state is not valid
    
    # Valid outcome
    ValidOutcomes = c("heart attack", "heart failure", "pneumonia")
    matched<-match(outcome, ValidOutcomes) # Assigns 1, 2 or 3 for on of the three matched factors (see list above)
    MatchedColNum <- as.integer(matched + 2) # Assign the column num of interest for the chosen factor
    if (is.na(matched)) stop("invalid outcome") # Exit function with error message if outcome is not valid
    
    ## Return hospital name in that state with the given rank 30-day death rate
    # Subset the data further with our valid state and outcome and remove NAs
        if (outcome == "heart attack") OutcomeSubset<-OutcomeSubset[c(1, 2, 3)]
        if (outcome == "heart failure") OutcomeSubset<-OutcomeSubset[c(1, 2, 4)]
        if (outcome == "pneumonia") OutcomeSubset<-OutcomeSubset[c(1, 2, 5)]
        OutcomeSubset<-OutcomeSubset[OutcomeSubset$State==state & OutcomeSubset[outcome] != 'Not Available', ]
        OutcomeSubset[, 3] <- as.numeric(OutcomeSubset[, 3])
        OutcomeSubset<-OutcomeSubset[order(OutcomeSubset[,3], OutcomeSubset[,1] ),]
        
     ## Return the result
        if (num == "best") {
          result<-head(OutcomeSubset, n=1L)
          result<-as.character(result[1])
          return(result)
        } #ENDIF
        if (num == "worst") {
          result<-tail(OutcomeSubset, n=1L)
          result<-as.character(result[1])
          return(result)
        } #ENDIF
        if (as.integer(nrow(OutcomeSubset)) <= as.integer(num)) {
          return(NA)
        } else {
          result<-OutcomeSubset[num,1]
          return(result)
        } #ENDIF ELSE
        
} #END FUNCTION rankhospital.R