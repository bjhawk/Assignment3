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
    data <- apply(data, function(x, ail='', rating=''){
        state <- x[1,'state']
        x <- x[with(x,
                    order(x[[outcome]], x$hospital.name, decreasing=F)),]
        if (is.numeric(num)) {
            if (num > nrow(x)) {
                name <- 'NA'
            } else {
                name <- x[num,'hospital.name']
            }
        } else if (num == 'best') {
            name <- x[1,'hospital.name']
        } else {
            x <- tail(x,1)
            name <- x[1,'hospital.name']
        }
        
        return(as.data.frame(list(hospital=name, state=state), row.names=state))
    }, ail=outcome, rating=num)
    return(data)
}
