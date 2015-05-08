best<-function(state,outcome){
    data<-read.csv("s1.csv",colClasses="character")
    
    statelist<-unique(data$State)                               ##valid state list
    outcomelist<-c("heart attack","heart failure","pneumonia")     ##valid outcome list
    
    ##loop to check if the state argument is valid
    if(!is.element(state,statelist))     stop('invalid state')
    
    ##loop to check if the outcome argumeent is valid
    if(!is.element(outcome,outcomelist)) stop('invalid outcome')
    
    ##changing the outcome values to numeric
    data[,11]<-as.numeric(data[,11])
    data[,19]<-as.numeric(data[,19])
    data[,23]<-as.numeric(data[,23])
    
    ##loop to assign outcome column to n
    n<-if(outcome=="heart attack"){
        11
    }else if(outcome=="heart failure"){
        19
    }else{
        23
    }
    
    shortlist<-data[!is.na(data[,n]),c(2,7,n)]  ##removing all the cases where particular outcomes are not available
    
    stshortlist<-shortlist[shortlist[,2]==state,] ##shortlisting for the particular state
    
    mini<-min(stshortlist[,3])                    ##assigning minimum death rate to mini
    
    crude<-stshortlist[stshortlist[,3]==mini,1]   ##extracting all the hospitals with minimum death rate
    
    crude<-sort(crude)                            ##sorting in alphabetic order
    
    ans<-crude[1]                                 ##assigning the first hospital name to ans
    
    ans                                           ##printing ans
      
}