rankhospital <- function (state, outcome, num = "best") {
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
    if (!num %in% c('worst', 'best') & !is.numeric(num)) {
        num <- 'best'
    }
    
    outcome <- paste('hospital.30.day.death..mortality..rates.from.',
                     gsub(" ",".",outcome), sep="")
    columns <- c('hospital.name', outcome)
    data[[outcome]] <- as.numeric(data[[outcome]])
    data <- data[data$state == state & !is.na(data[[outcome]]),columns]
    data <- data[with(data, 
                      order(data[[outcome]],data$hospital.name, decreasing=F)),]
    if (is.numeric(num)) {
        if (num > nrow(data)) {
            return('NA')
        } else {
            return(data[num,'hospital.name'])
        }
    } else if (num == 'best') {
        return(data[1,'hospital.name'])
    } else {
        data <- tail(data,1)
        return(data[1,'hospital.name'])
    }
}
