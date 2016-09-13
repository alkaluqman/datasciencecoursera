my_pollutantmean <- function(directory, pollutant, id = 1:332) {
        #setwd(paste("./",directory, sep=""))
        v <- numeric()
        index <- 1
        
        if(pollutant == 'sulphate'){
                for ( i in id ) {
                        filename <- sprintf("%03d",i)
                        filepath <- paste(directory,"/",filename,".csv",sep="")
                        v[index] <- read.csv(filepath, colClasses = c("NULL",NA,"NULL","NULL") ) 
                        index <- index + 1
                }
        }
        else if (pollutant == 'nitrate'){
                for ( i in id ) {
                        filename <- sprintf("%03d",i)
                        filepath <- paste(directory,"/",filename,".csv",sep="")
                        v[index] <- read.csv(filepath, colClasses = c("NULL","NULL",NA,"NULL") ) 
                        index <- index + 1
                }
        }
        
        mean_vector <- vector()
        for ( j in 1:length(v) ){
                mean_vector[j] <- mean(v[[j]], na.rm = TRUE)   
        }
        mean_vector
}


pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        filenames <- list.files(directory, full.names=TRUE)[id]
        mydata<- data.frame()
        for ( i in filenames ){
                mydata <- rbind( mydata,read.csv(i) )
        }
        if(pollutant == "sulfate")
                mean_value <- mean(mydata$sulfate, na.rm=TRUE)
        if(pollutant == "nitrate")
                mean_value <- mean(mydata$nitrate, na.rm=TRUE)
        mean_value
}

complete <- function(directory, id = 1:332){
        filenames <- list.files(directory, full.names=TRUE)[id]
        nobs <- numeric()
        index <- 1
        for( i in filenames ){
                temp_data <- read.csv(i)
                nobs[index] <- nrow( na.omit(temp_data) )
                #nobs[index] <- nrow( temp_data[complete.cases(temp_data),] )
                index<- index + 1
        }
        data.frame(id,nobs)
}

corr <- function(directory, threshold =0){
        filenames <- list.files(directory, full.names=TRUE)
        corr_vector <- numeric()
        index <- 1
        for( i in filenames ){
                temp_data <- read.csv(i)
                subset <- na.omit(temp_data)
                nobs <- nrow(subset)
                if(nobs > threshold){
                        corr_vector[index] <- cor(subset$sulfate, subset$nitrate)
                        index <- index + 1
                        }
        }
        corr_vector
}