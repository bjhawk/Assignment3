best <- function (state, outcome) {
    ##read outcome data
    data <- read.csv('outcome-of-care-measures.csv', colClasses="character")
    names(data) <- tolower(names(data)) #this makes life easier
    state <- toupper(state)
    outcome <- tolower(outcome)
    
    ## check that state and outcome are valid
    if (!state %in% levels(as.factor(data$state))) {
        #error state
        stop('invalid state')
    }
    if (!outcome %in% c("pneumonia", "heart failure", "heart attack")) {
        # error outcome
        stop('invalid outcome')
    }
    
    ## return hospital in given state with lowest 30-day death rate
    outcome <- paste('hospital.30.day.death..mortality..rates.from.',
                     gsub(" ",".",outcome), sep="")
    columns <- c('hospital.name', outcome)
    data[[outcome]] <- as.numeric(data[[outcome]])
    data <- data[data$state == state & !is.na(data[[outcome]]),columns]
    data <- data[with(data, 
                     order(data[[outcome]],data$hospital.name, decreasing=F)),]
    return(data[1,'hospital.name'])
}
