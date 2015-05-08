rankhospital<-function(state,outcome,num="best"){
    
    data<-read.csv("s1.csv",colClasses="character")
    
    statelist<-unique(data$State)                                  ##valid state list
    outcomelist<-c("heart attack","heart failure","pneumonia")     ##valid outcome list
    
    ##loop to check if the state argument is valid
    if(!is.element(state,statelist))     stop('invalid state')
    
    ##loop to check if the outcome argumeent is valid
    if(!is.element(outcome,outcomelist)) stop('invalid outcome')
    
    ##changing the outcome values to numeric
    data[,11]<-as.numeric(data[,11])
    data[,19]<-as.numeric(data[,19])
    data[,23]<-as.numeric(data[,23])
    
    
    
    
    ##loop to assign outcome column to col
    col<-if(outcome=="heart attack"){
        11
    }else if(outcome=="heart failure"){
        19
    }else{
        23
    }
    
    shortlist<-data[!is.na(data[,col]),c(2,7,col)]  ##removing all the cases where particular outcomes are not available
    
    stshortlist<-shortlist[shortlist[,2]==state,]   ##shortlisting for the particular state
    
    ndx<-order(stshortlist[,1])                     ##indexing according to alphabetic order of hospital names
    
    hosprankedlist<-stshortlist[ndx,]               ##ordered list according to hospital names
    
    ndx1<-order(hosprankedlist[,3])                 ##indexing according to the deathrates
    
    rankeddata<-hosprankedlist[ndx1,]               ##ordered list according to deathrates
    
    
    ##loop to assign rank its value
    rank<-if(num=="best"){         
        1
    }else if(num=="worst"){
        nrow(rankeddata)
    }else{
        num
    }
    
    rankeddata[rank,1]
      
    
    
}