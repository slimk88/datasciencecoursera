corr <- function(directory, threshold = 0, id = 1:332) {
  
  directory <- "/Users/macbookpro/Desktop/specdata"
  files_list <- list.files(directory, pattern="*.csv", full.names = TRUE)
  df <- data.frame()
  df2 <- data.frame()
  vec <- vector()
  
  ## all files must be accessed and a df is generated with all complete values and their id's
  for (i in id) {
    dfcom <- read.csv(files_list[i])
    dfcomcc <- complete.cases(dfcom)
    nobs <- sum(dfcomcc)
    v.inobs <- cbind(i, nobs)
    df <- rbind(df, v.inobs)
                }
 
  colnames(df) <- c("id", "nobs")
  Thresh.id <- df$id[df$nobs >= threshold]

  ## need to subset from df of complete values rows that meet the threshold requirement i.e nobs >= theshold
  ## get id of the subset of rows that meet this requirement and use it to access the data for nitrate and sulfate
  ## for each id calculate the cor() btw nitrate and sulfate and return the result
  
  for (i in Thresh.id){
    
    df2 <- rbind(df2, read.csv(files_list[i]))
    df2 <- na.omit(df2)
    vec <- cbind(vec, cor(df2$nitrate, df2$sulfate))
    
    ## for the result/output create a data frame or list or vector for which you can call head(), str() etc...
                      
  head(vec)
}

