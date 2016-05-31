complete <- function(directory, id = 1:332) {
    #set the path
    path = directory
    
    #get the file List in that directory
    fileList = list.files(path)
    
    j <- 1
    s <- vector()
    
    for(i in id) {
        #extract the file names and store as numeric for comparison
        file.names = as.numeric(sub("\\.csv$","",fileList))
        
        #select files to be imported based on the user input or default
        selected.files = fileList[match(i,file.names)]
        
        #import data
        Data = lapply(file.path(path,selected.files),read.csv)
        
        #convert into data frame
        Data = do.call(rbind.data.frame,Data)
        
        #sum complete cases
        s[j] <- sum(complete.cases(Data))
        j <- j + 1
    }
    
    data.frame(cbind(id, nobs = s))
}