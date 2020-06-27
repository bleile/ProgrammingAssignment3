### function "rankallhospital" in script rankallhospital.R for hospital outcome data as 
### Part 4 of ProgrammingAssignment3, C. Bleile
### the function reads in data from an unzipped folder under the working directory, subsets the data
### and uses two arguments: an outcome (outcome), and the ranking of a hospital in that state for that 
### outcome (num). The function reads the outcome-of-care-measures.csv file and returns a dataframe with 
### with the hospital at the selected rank specified by the num argument.
## rankallhospital.R function follows:

rankall <- function(outcome="heart attack", num ="best") {
  
    ## First read in the data from unzipped folder in the working directory.
    OutcomeData <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    ## subset the data.frame to the factors needed and rename columns to easier factor names
    OutcomeSubset <- OutcomeData[c(2, 7, 11, 17, 23)]
    names(OutcomeSubset)[1] <- "Name"
    names(OutcomeSubset)[2] <- "State"
    names(OutcomeSubset)[3] <- "heart attack"
    names(OutcomeSubset)[4] <- "heart failure"
    names(OutcomeSubset)[5] <- "pneumonia"
    
    ## Load the list of valid states into a vector
    ValidStates <- unique(OutcomeSubset[, 2]) # Get the list of valid states from the State factor
    
    ## Check outcome is valid
    # Valid outcome
    ValidOutcomes = c("heart attack", "heart failure", "pneumonia")
    matched<-match(outcome, ValidOutcomes) # Assigns 1, 2 or 3 for on of the three matched factors (see list above)
    MatchedColNum <- as.integer(matched + 2) # Assign the column num of interest for the chosen factor
    if (is.na(matched)) stop("invalid outcome") # Exit function with error message if outcome is not valid
    
    ## Return hospital name in that state with the given rank 30-day death rate
    # Subset the data further with our outcome and remove NAs
    if (outcome == "heart attack") OutcomeSubset<-OutcomeSubset[c(1, 2, 3)]
    if (outcome == "heart failure") OutcomeSubset<-OutcomeSubset[c(1, 2, 4)]
    if (outcome == "pneumonia") OutcomeSubset<-OutcomeSubset[c(1, 2, 5)]
    OutcomeSubset<-OutcomeSubset[OutcomeSubset[outcome] != 'Not Available', ]
    OutcomeSubset[, 3] <- as.numeric(OutcomeSubset[, 3])
 
     ## Create the result
     result<-data.frame(hospital=character(0), state=character(0))  #init the null result dataframe
     for (state in ValidStates) {
       OutcomeStateSubset<-OutcomeSubset[OutcomeSubset$State==state, ]
       OutcomeStateSubset<-OutcomeStateSubset[order(OutcomeStateSubset[,3], OutcomeStateSubset[,1] ),]
        if (num == "best") {
          newrow<-head(OutcomeStateSubset, n=1L)
          result<-rbind(result, newrow[1:2])
          next
        } #ENDIF

        if (num == "worst") {
          newrow<-tail(OutcomeStateSubset, n=1L)
          result<-rbind(result, newrow[1:2])
          next
        } #ENDIF

        if (as.integer(nrow(OutcomeStateSubset)) <= as.integer(num)) {
          newrow<-c("<NA>", state)
          result<-rbind(result, newrow)

        } else {
          newrow<-OutcomeStateSubset[num,1:2]
          result<-rbind(result, newrow[1:2])

        } #ENDIF ELSE
     } #END FOR
     result<-result[order(result[,2]),]  # order the result by state
    return(result)
        
} #END FUNCTION rankall.R NOTE: Usage returns "Name and State" vice "hospital state"