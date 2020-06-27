### Script outcome.R to read in hospital outcome data as Part 1 of ProgrammingAssignment3, C. Bleile
## First read in the data from unzipped folder in the working directory.
outcome <- read.csv("rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")

## Coerce column 11 to numeric from character (Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
outcome[, 11] <- as.numeric(outcome[, 11])

## Plot the Histogram
hist(outcome[, 11], main="30-Day Mortality from Heart Attack", xlab="Numbers of Deaths")

### End Script Outcome.R