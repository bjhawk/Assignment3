rankall <- function (outcome, num = "best") {
    ##read outcome data
    data <- read.csv('outcome-of-care-measures.csv', colClasses="character")
    names(data) <- tolower(names(data)) #this makes life easier
    outcome <- tolower(outcome)
    
    ## check that state and outcome are valid
    if (!outcome %in% c("pneumonia", "heart failure", "heart attack")) {
        # error outcome
        stop('invalid outcome')
    }
    if (!num %in% c('worst', 'best') & !is.numeric(num)) {
        num <- 'best'
    }
    
    outcome <- paste('hospital.30.day.death..mortality..rates.from.',
                     gsub(" ",".",outcome), sep="")
    columns <- c('hospital.name', 'state', outcome)
    data[[outcome]] <- as.numeric(data[[outcome]])
    data <- data[!is.na(data[[outcome]]),columns]
    data <- split(data, data$state)
    data <- lapply(data, function(x, ail='', rating=''){
        x <- x[with(x,
                    order(x[[outcome]], x$hospital.name, decreasing=F)),]
        if (is.numeric(num)) {
            if (num > nrow(x)) {
                name <- 'NA'
                x <- x[1,]
                x[1,'hospital.name'] <- NA
            } else {
                x <- x[num,]
            }
        } else if (num == 'best') {
            x <- x[1,]
        } else {
            x <- tail(x,1)
        }
        return(x)
        #return(as.data.frame(list(hospital=name, state=state), row.names=state))
    }, ail=outcome, rating=num)
    #return(data)
    df <- makeDataFrame(data)
    return(df)
}

makeDataFrame <- function(x){
    states <- names(x)
    df <- data.frame('hospital'=character(length(states)),
                     'state'=character(length(states)),
                     stringsAsFactors=F, row.names=states)
    for(i in 1:length(states)) {
        stateFrame <- x[i][[states[i]]]
        stateFrame <- stateFrame[1,]
        df[stateFrame$state,'hospital'] <- stateFrame$hospital.name
        df[stateFrame$state,'state'] <- stateFrame$state
    }
    return(df)
}
