#Map bos API Key
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoidGVvbW90dW4iLCJhIjoiY2tnaTJnb3cwMDl6MTJycGM1YWYxa3J0bSJ9.jOPKk54RaW3u_SgqZSpM-w')

getAnovaParameters <- function(specified_column, target_column, df) {
    #' Perform One way ANOVA between the target column and the specified column
    #'
    #' @param specified_column character indicating column to perform ANOVA analysis on
    #' @param target_column character indicating column we're trying to analyze the variance
    #' @param df data frame
    #' @return dataframe with statistical parameters like p-value and DOF
    #'
    temp_df <- broom::tidy(aov(df[,target_column] ~ df[,specified_column]))[1,]
    temp_df$term <-  specified_column
    temp_df
}


generateNewColumn <- function(df, newColumnVector, oldcolumn, delete=FALSE) {
    #' Generate one-hot encoding of a factor column
    #'
    #' @param df data frame
    #' @param newColumnVector character vector of unique values in the column
    #' @param oldcolumn character indicating old column to convert
    #' @param delete  logical specify whether to keep column or not
    #' @return dataframe with encoded columns
    #'
    ColumnList <- list()
    for (newColumn in newColumnVector) {
        x <- df[,c(oldcolumn)]
        mode <- calculate_mode(x)
        x[is.na(x)] <- mode
        ColumnList[gsub(" ", "_",newColumn)] <- as.data.frame(
            ifelse(grepl(newColumn, x, ignore.case = T)==T,1,0)
        )
    }
    
    if (delete==TRUE) {
        df[,c(oldcolumn)] <- NULL
    }
    do.call("cbind", ColumnList)
}


convertToNumeric <- function(colname) {
    #' Converts a column to numeric format
    #'
    #' @param colname character indicating the name of column to format
    #' @return column vector with properly formatted data
    #'
    column <- num_dataframe[,colname]
    #first convert each column into numeric if it is from factor
    column <- as.numeric(column)
    #convert the item with NA to median value from the column
    #column[is.na(column)] =median(column, na.rm=TRUE) 
    column
}

calculate_mode <- function(x) {
    #' Calculates mode of a numeric or character vector or dataframe column
    #'
    #' @param x character/numeric vector indicating what to format
    #' @return mode of input
    #'
    uniqx <- unique(na.omit(x))
    uniqx[which.max(tabulate(match(x, uniqx)))][1]
}


calcMissingRowPerc <- function(df){
    #' Gets columns with missing values in decreasing order
    #'
    #' @param df data frame with all columns to process
    #' @return dataframe that has columns with NAs and their percentage
    #'
    numMissingVal <-sapply(df, function(x) sum(length(which(is.na(x)))))  
    paramsMissingSomeRows <- numMissingVal[numMissingVal!=0]
    percRowsMissing <- round(paramsMissingSomeRows/nrow(df)*100,4) 
    numMissingValDf <-as.data.frame(percRowsMissing)
    numMissingValDf <- arrange(numMissingValDf, desc(percRowsMissing))
    numMissingValDf
}

pretty_print_table <- function(input_table) {
    #' Properly format a table with kabble function
    #'
    #' @param input_table data frame with all columns to process
    #' @return properly formatted dataframe
    #'
    ouput_table <- kable(input_table) %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>% 
        scroll_box(width = "100%", height = "250px")
    ouput_table
}


calculateIsolationVariables <- function(df, lon="longitude", lat="latitude") {
    #' Get isolation variables for a given location
    #'
    #' @param df data frame with all columns to process
    #' @param lon numeric vector indication longitude column
    #' @param lat numeric vector indication latitude column
    #' @return dataframe with columns for amount of neighbours in 50 miles and distance to closest neighbor
    #'
    data_sf <- st_as_sf(df, coords = c(lon,lat),
                        # Change to your CRS
                        crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    dist.mat <- st_distance(data_sf) # Great Circle distance since in lat/lon
    # Number within 50m: Subtract 1 to exclude the point itself
    num.50 <- apply(dist.mat, 1, function(x) {
        sum(x < 50) - 1
    })
    # Calculate nearest distance
    nn.dist <- apply(dist.mat, 1, function(x) {
        return(sort(x, partial = 2)[2])
    })
    # Get index for nearest distance
    nn.index <- apply(dist.mat, 1, function(x) { order(x, decreasing=F)[2] })
    
    n.data <- df[c("id", "host_id")]
    colnames(n.data)[1] <- "neighbor"
    colnames(n.data)[2:ncol(n.data)] <- 
        paste0("neighbor_", colnames(n.data)[2:ncol(n.data)])
    mydata2 <- data.frame(df,
                          n.data[nn.index, ],
                          n.distance = nn.dist,
                          radius50 = num.50)
    rownames(mydata2) <- seq(nrow(mydata2))
    mydata2
}


checkLonLat <- function(dat, loc, lon, lat){
    #' Check if GPS longitude and latitude coordinates
    #' are swapped
    #' @param dat data frame
    #' @param loc character string indicating the
    #' name of the location variable in \code{dat}
    #' @param lon character string indicating the
    #' name of the longitude variable in \code{dat}
    #' @param lat  character string indicating the
    #' name of the latitude variable in \code{dat}
    #'
    for (i in 1:nrow(dat)) {
        if(is.na(dat[[lat]][i]) | is.na(dat[[lon]][i]))
            next
        
        if(abs(dat[[lat]][i]) > 90){
            warning("Longitude/latitude coordinates appeared to be swapped for listing with id ", dat[[loc]][i],".",
                    "\nCoordinates have  been corrected ...")
            trueLat <- dat[[lon]][i]
            trueLon <- dat[[lat]][i]
            dat[[lat]][i] <- trueLat
            dat[[lon]][i]  <- trueLon
        }
    }
    return(dat)
}


checkReviewDates <- function(dat, loc, first_review, last_review){
    #' Check first_review and last_review dates for swapped values
    #' @param dat data frame
    #' @param loc character string indicating the
    #' name of the location variable in \code{dat}
    #' @param first_review character string indicating the
    #' name of the first review variable in \code{dat}
    #' @param last_review character string indicating the
    #' name of the last review variable in \code{dat}
    
    for (i in 1:nrow(dat)) {
        if(is.na(dat[[first_review]][i]) | is.na(dat[[last_review]][i]))
            next
        
        if(dat[[first_review]][i] >= dat[[last_review]][i]){
            warning("First Review and Last Review dates appeared to have been swapped for listing with id ", dat[[loc]][i],".",
                    "\nFirst Review and Last Review dates have  been corrected ...")
            trueFirst <- dat[[first_review]][i]
            trueLast <- dat[[last_review]][i]
            dat[[first_review]][i] <- trueFirst
            dat[[last_review]][i]  <- trueLast
        }
    }
    return(dat)
}


preProcessData <- function(data, missCutoff=0.1,
                           dropNearZero=TRUE,
                           dropCorrVars=FALSE,
                           corrCutoff=0.95){
    
    #' Process data frame to remove NA, near-zero variance
    #' variables, and highly correlated variables
    #'
    #' @param data data frame or matrix of  data
    #' @param missCutoff numeric indicating the cutoff percentage of missing
    #'  data allowed. missCutoff should be set to 0 when every column with
    #'  at least one missing observation should be dropped
    #' @param dropNearZero logical indicating whether to drop
    #' near-zero variance columns
    #' @param dropCorrVars logical indicating whether to drop one of
    #' each pair of highly correlated variables
    #' @param corrCutoff A numeric value for the pair-wise absolute
    #'  correlation cutoff. Used when \code{dropCorrVars = TRUE}
    #' 
    library(caret)
    
    if(!is.null(missCutoff)){
        missPct <- apply(data,2, function (x) {
            (sum(is.na(x)) + sum(is.infinite(x))) /length(x)})
        missV2Drop<- missPct[missPct>missCutoff]
        missVars <- names(missV2Drop)
        #missVarsCol <- paste(missVars, collapse = ", ")
        cat(paste(length(missVars), "variable/s", paste(missVars, collapse = ", "),
                  "have been dropped at the ",
                  missCutoff,"missing data cut-off.", sep=" "), "\n")
        missV2DropId <- missPct > missCutoff
        data <- data[,!missV2DropId]
        # Replace missing data with column means
        data[,1:ncol(data)] <- sapply(data[,1:ncol(data)], function(x)
            ifelse(is.na(x), mean(x, na.rm = T), x))
        
        data[,1:ncol(data)] <- sapply(data[,1:ncol(data)], function(x)
            ifelse(is.nan(x), mean(x, na.rm = T), x))
        
    }else{
        missVars <- NULL
    }
    
    # Replace any remaining missing data with column means
    data[,1:ncol(data)] <- sapply(data[,1:ncol(data)], function(x)
        ifelse(is.na(x), mean(x, na.rm = T), x))
    
    data[,1:ncol(data)] <- sapply(data[,1:ncol(data)], function(x)
        ifelse(is.nan(x), mean(x, na.rm = T), x))
    
    if(dropNearZero){
        nzv <- nearZeroVar(data)
        if(length(nzv>=1)){
            data <- data[, -nzv]}
        nzVars <-colnames(data)[nzv]
        #nzVars <- paste(nzVars, collapse = ",")
        cat(length(nzVars),"variable/s", paste(nzVars, collapse = ","),
            "was/were dropped because of near-zero variance\n")
    }else{
        nzVars <- NULL
    }
    
    if(dropCorrVars){
        descrCor <-  cor(data, use= "complete.obs")
        highlyCor <- findCorrelation(descrCor, cutoff = corrCutoff, exact = TRUE)
        CorrVars <- colnames(data)[highlyCor]
        data <- data[,-highlyCor]
        cat(length(CorrVars),"variable/s",  paste(CorrVars, collapse = ", "),
            "was/were dropped because of high correlation with
        other variables in the data\n")
        
    }else{
        CorrVars <- NULL
    }
    output <- list(data=data, missVars=missVars, nzVars=nzVars,
                   corVars=CorrVars )
    return(output)
    
}

###-- LASSO Regressions  ---###
## Lasso Model
lasso_model <- function(X,Y) {
    nbFolds <-  nrow(X) # leave-one-out
    nL <- 20
    cvFit <- cv.glmnet(X,Y,family="gaussian",alpha=1,
                       nlambda=nL, standardize = T, nfolds=nbFolds)
    lmin<-cvFit$lambda.min
    
    glmnetLasso <- glmnet(X,Y,family="gaussian",alpha=1,
                          lambda=lmin, standardize = T)                           
    l1Coeff <-  as.vector(t(coef(glmnetLasso,s=lmin)))
    l1Coeff <- l1Coeff[-1]
    names(l1Coeff) <- names(X)
    
    l1Betas <-data.frame(VarID=colnames(X),beta=l1Coeff,stringsAsFactors = F)
    pfit <- predict(glmnetLasso, X, s = lmin, type = "response")
    r2 <- summary(lm(Y ~ pfit))$r.squared
    r2_df<-data.frame("R-Square",r2)
    rownames(r2_df) <- "R-Square"
    names(r2_df) <- c("VarID", "beta")
    #l1Betas <- rbind(l1Betas, r2_df)
    rownames(l1Betas) <- NULL
    list(r2=r2_df, df=l1Betas)
}


imputeZillowData <- function(df) {
    #' Get isolation variables for a given location
    #'
    #' @param df data frame with all columns to group
    #' @return dataframe grouped columns
    #'
    
    df %>% group_by(neighbourhood_cleansed) %>% mutate(
        Median_home_value = ifelse(is.na(Median_home_value), mean(Median_home_value, na.rm = TRUE), Median_home_value),
        At.Least.High.School.Diploma = ifelse(is.na(At.Least.High.School.Diploma), mean(At.Least.High.School.Diploma, na.rm = TRUE), At.Least.High.School.Diploma),
        At.Least.Bachelor.s.Degree = ifelse(is.na(At.Least.Bachelor.s.Degree), mean(At.Least.Bachelor.s.Degree, na.rm = TRUE), At.Least.Bachelor.s.Degree),
        Graduate.Degree = ifelse(is.na(Graduate.Degree), mean(Graduate.Degree, na.rm = TRUE), Graduate.Degree),
        Median.Earnings.2010.dollars = ifelse(is.na(Median.Earnings.2010.dollars), mean(Median.Earnings.2010.dollars, na.rm = TRUE), Median.Earnings.2010.dollars),
        Service.occupations = ifelse(is.na(Service.occupations), mean(Service.occupations, na.rm = TRUE), Service.occupations),
        Children.Under.6.Living.in.Poverty = ifelse(is.na(Children.Under.6.Living.in.Poverty), mean(Children.Under.6.Living.in.Poverty, na.rm = TRUE), Children.Under.6.Living.in.Poverty),
        Management.professional.and.related.occupations = ifelse(is.na(Management.professional.and.related.occupations), mean(Management.professional.and.related.occupations, na.rm = TRUE), Management.professional.and.related.occupations),
        Adults.65.and.Older.Living.in.Poverty = ifelse(is.na(Adults.65.and.Older.Living.in.Poverty), mean(Adults.65.and.Older.Living.in.Poverty, na.rm = TRUE), Adults.65.and.Older.Living.in.Poverty),
        Poverty.Rate.below.federal.poverty.threshold = ifelse(is.na(Poverty.Rate.below.federal.poverty.threshold), mean(Poverty.Rate.below.federal.poverty.threshold, na.rm = TRUE), Poverty.Rate.below.federal.poverty.threshold),
        White = ifelse(is.na(White), mean(White, na.rm = TRUE), White),
        Black = ifelse(is.na(Black), mean(Black, na.rm = TRUE), Black),
        Hispanic = ifelse(is.na(Hispanic), mean(Hispanic, na.rm = TRUE), Hispanic),
        Asian = ifelse(is.na(Asian), mean(Asian, na.rm = TRUE), Asian),
        Amerindian = ifelse(is.na(Amerindian), mean(Amerindian, na.rm = TRUE), Amerindian),
        White_Asian = ifelse(is.na(White_Asian), mean(White_Asian, na.rm = TRUE), White_Asian),
        median_age = ifelse(is.na(median_age), mean(median_age, na.rm = TRUE), median_age),
        Sexually.transmitted.infections = ifelse(is.na(Sexually.transmitted.infections), mean(Sexually.transmitted.infections, na.rm = TRUE), Sexually.transmitted.infections),
        Adult.smoking = ifelse(is.na(Adult.smoking), mean(Adult.smoking, na.rm = TRUE), Adult.smoking),
        Unemployment = ifelse(is.na(Unemployment), mean(Unemployment, na.rm = TRUE), Unemployment),
        Violent.crime = ifelse(is.na(Violent.crime), mean(Violent.crime, na.rm = TRUE), Violent.crime),
        Homicide.rate = ifelse(is.na(Homicide.rate), mean(Homicide.rate, na.rm = TRUE), Homicide.rate),
        Injury.deaths = ifelse(is.na(Injury.deaths), mean(Injury.deaths, na.rm = TRUE), Injury.deaths),
        elevation = ifelse(is.na(elevation), mean(elevation, na.rm = TRUE), elevation),
        annual_PRCP = ifelse(is.na(annual_PRCP), mean(annual_PRCP, na.rm = TRUE), annual_PRCP),
        winter_PRCP = ifelse(is.na(winter_PRCP), mean(winter_PRCP, na.rm = TRUE), winter_PRCP),
        summer_PRCP = ifelse(is.na(summer_PRCP), mean(summer_PRCP, na.rm = TRUE), summer_PRCP),
        spring_PRCP = ifelse(is.na(spring_PRCP), mean(spring_PRCP, na.rm = TRUE), spring_PRCP),
        autumn_PRCP = ifelse(is.na(autumn_PRCP), mean(autumn_PRCP, na.rm = TRUE), autumn_PRCP),
        annual_TAVG = ifelse(is.na(annual_TAVG), mean(annual_TAVG, na.rm = TRUE), annual_TAVG),
        annual_TMAX = ifelse(is.na(annual_TMAX), mean(annual_TMAX, na.rm = TRUE), annual_TMAX),
        annual_TMIN = ifelse(is.na(annual_TMIN), mean(annual_TMIN, na.rm = TRUE), annual_TMIN),
        winter_TAVG = ifelse(is.na(winter_TAVG), mean(winter_TAVG, na.rm = TRUE), winter_TAVG),
        winter_TMAX = ifelse(is.na(winter_TMAX), mean(winter_TMAX, na.rm = TRUE), winter_TMAX),
        winter_TMIN = ifelse(is.na(winter_TMIN), mean(winter_TMIN, na.rm = TRUE), winter_TMIN),
        summer_TAVG = ifelse(is.na(summer_TAVG), mean(summer_TAVG, na.rm = TRUE), summer_TAVG),
        summer_TMAX = ifelse(is.na(summer_TMAX), mean(summer_TMAX, na.rm = TRUE), summer_TMAX),
        summer_TMIN = ifelse(is.na(summer_TMIN), mean(summer_TMIN, na.rm = TRUE), summer_TMIN),
        spring_TAVG = ifelse(is.na(spring_TAVG), mean(spring_TAVG, na.rm = TRUE), spring_TAVG),
        spring_TMAX = ifelse(is.na(spring_TMAX), mean(spring_TMAX, na.rm = TRUE), spring_TMAX),
        spring_TMIN = ifelse(is.na(spring_TMIN), mean(spring_TMIN, na.rm = TRUE), spring_TMIN),
        autumn_TAVG = ifelse(is.na(autumn_TAVG), mean(autumn_TAVG, na.rm = TRUE), autumn_TAVG),
        autumn_TMAX = ifelse(is.na(autumn_TMAX), mean(autumn_TMAX, na.rm = TRUE), autumn_TMAX),
        autumn_TMIN = ifelse(is.na(autumn_TMIN), mean(autumn_TMIN, na.rm = TRUE), autumn_TMIN)
    ) %>% group_by(neighbourhood_group_cleansed) %>% mutate(
        Median_home_value = ifelse(is.na(Median_home_value), mean(Median_home_value, na.rm = TRUE), Median_home_value),
        At.Least.High.School.Diploma = ifelse(is.na(At.Least.High.School.Diploma), mean(At.Least.High.School.Diploma, na.rm = TRUE), At.Least.High.School.Diploma),
        At.Least.Bachelor.s.Degree = ifelse(is.na(At.Least.Bachelor.s.Degree), mean(At.Least.Bachelor.s.Degree, na.rm = TRUE), At.Least.Bachelor.s.Degree),
        Graduate.Degree = ifelse(is.na(Graduate.Degree), mean(Graduate.Degree, na.rm = TRUE), Graduate.Degree),
        Median.Earnings.2010.dollars = ifelse(is.na(Median.Earnings.2010.dollars), mean(Median.Earnings.2010.dollars, na.rm = TRUE), Median.Earnings.2010.dollars),
        Service.occupations = ifelse(is.na(Service.occupations), mean(Service.occupations, na.rm = TRUE), Service.occupations),
        Children.Under.6.Living.in.Poverty = ifelse(is.na(Children.Under.6.Living.in.Poverty), mean(Children.Under.6.Living.in.Poverty, na.rm = TRUE), Children.Under.6.Living.in.Poverty),
        Management.professional.and.related.occupations = ifelse(is.na(Management.professional.and.related.occupations), mean(Management.professional.and.related.occupations, na.rm = TRUE), Management.professional.and.related.occupations),
        Adults.65.and.Older.Living.in.Poverty = ifelse(is.na(Adults.65.and.Older.Living.in.Poverty), mean(Adults.65.and.Older.Living.in.Poverty, na.rm = TRUE), Adults.65.and.Older.Living.in.Poverty),
        Poverty.Rate.below.federal.poverty.threshold = ifelse(is.na(Poverty.Rate.below.federal.poverty.threshold), mean(Poverty.Rate.below.federal.poverty.threshold, na.rm = TRUE), Poverty.Rate.below.federal.poverty.threshold),
        White = ifelse(is.na(White), mean(White, na.rm = TRUE), White),
        Black = ifelse(is.na(Black), mean(Black, na.rm = TRUE), Black),
        Hispanic = ifelse(is.na(Hispanic), mean(Hispanic, na.rm = TRUE), Hispanic),
        Asian = ifelse(is.na(Asian), mean(Asian, na.rm = TRUE), Asian),
        Amerindian = ifelse(is.na(Amerindian), mean(Amerindian, na.rm = TRUE), Amerindian),
        White_Asian = ifelse(is.na(White_Asian), mean(White_Asian, na.rm = TRUE), White_Asian),
        median_age = ifelse(is.na(median_age), mean(median_age, na.rm = TRUE), median_age),
        Sexually.transmitted.infections = ifelse(is.na(Sexually.transmitted.infections), mean(Sexually.transmitted.infections, na.rm = TRUE), Sexually.transmitted.infections),
        Adult.smoking = ifelse(is.na(Adult.smoking), mean(Adult.smoking, na.rm = TRUE), Adult.smoking),
        Unemployment = ifelse(is.na(Unemployment), mean(Unemployment, na.rm = TRUE), Unemployment),
        Violent.crime = ifelse(is.na(Violent.crime), mean(Violent.crime, na.rm = TRUE), Violent.crime),
        Homicide.rate = ifelse(is.na(Homicide.rate), mean(Homicide.rate, na.rm = TRUE), Homicide.rate),
        Injury.deaths = ifelse(is.na(Injury.deaths), mean(Injury.deaths, na.rm = TRUE), Injury.deaths),
        elevation = ifelse(is.na(elevation), mean(elevation, na.rm = TRUE), elevation),
        annual_PRCP = ifelse(is.na(annual_PRCP), mean(annual_PRCP, na.rm = TRUE), annual_PRCP),
        winter_PRCP = ifelse(is.na(winter_PRCP), mean(winter_PRCP, na.rm = TRUE), winter_PRCP),
        summer_PRCP = ifelse(is.na(summer_PRCP), mean(summer_PRCP, na.rm = TRUE), summer_PRCP),
        spring_PRCP = ifelse(is.na(spring_PRCP), mean(spring_PRCP, na.rm = TRUE), spring_PRCP),
        autumn_PRCP = ifelse(is.na(autumn_PRCP), mean(autumn_PRCP, na.rm = TRUE), autumn_PRCP),
        annual_TAVG = ifelse(is.na(annual_TAVG), mean(annual_TAVG, na.rm = TRUE), annual_TAVG),
        annual_TMAX = ifelse(is.na(annual_TMAX), mean(annual_TMAX, na.rm = TRUE), annual_TMAX),
        annual_TMIN = ifelse(is.na(annual_TMIN), mean(annual_TMIN, na.rm = TRUE), annual_TMIN),
        winter_TAVG = ifelse(is.na(winter_TAVG), mean(winter_TAVG, na.rm = TRUE), winter_TAVG),
        winter_TMAX = ifelse(is.na(winter_TMAX), mean(winter_TMAX, na.rm = TRUE), winter_TMAX),
        winter_TMIN = ifelse(is.na(winter_TMIN), mean(winter_TMIN, na.rm = TRUE), winter_TMIN),
        summer_TAVG = ifelse(is.na(summer_TAVG), mean(summer_TAVG, na.rm = TRUE), summer_TAVG),
        summer_TMAX = ifelse(is.na(summer_TMAX), mean(summer_TMAX, na.rm = TRUE), summer_TMAX),
        summer_TMIN = ifelse(is.na(summer_TMIN), mean(summer_TMIN, na.rm = TRUE), summer_TMIN),
        spring_TAVG = ifelse(is.na(spring_TAVG), mean(spring_TAVG, na.rm = TRUE), spring_TAVG),
        spring_TMAX = ifelse(is.na(spring_TMAX), mean(spring_TMAX, na.rm = TRUE), spring_TMAX),
        spring_TMIN = ifelse(is.na(spring_TMIN), mean(spring_TMIN, na.rm = TRUE), spring_TMIN),
        autumn_TAVG = ifelse(is.na(autumn_TAVG), mean(autumn_TAVG, na.rm = TRUE), autumn_TAVG),
        autumn_TMAX = ifelse(is.na(autumn_TMAX), mean(autumn_TMAX, na.rm = TRUE), autumn_TMAX),
        autumn_TMIN = ifelse(is.na(autumn_TMIN), mean(autumn_TMIN, na.rm = TRUE), autumn_TMIN)
    ) %>% ungroup()
}