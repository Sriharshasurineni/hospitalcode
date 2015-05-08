rankall<-function(outcome,num="best"){
    
    data<-read.csv("s1.csv",colClasses="character")

    statelist<-unique(data$State)                                  ##valid state list
    outcomelist<-c("heart attack","heart failure","pneumonia")     ##valid outcome list

    ##loop to check if the state argument is valid
    if(!is.element(state,statelist))     stop('invalid state')

    ##loop to check if the outcome argumeent is valid
    if(!is.element(outcome,outcomelist)) stop('invalid outcome')

    ##changing the outcome values to numeric
    data[,11]<-as.numeric(data[,11])
    data[,17]<-as.numeric(data[,17])
    data[,23]<-as.numeric(data[,23])




    ##loop to assign outcome column to col
    col<-if(outcome=="heart attack"){
        11
    }else if(outcome=="heart failure"){
        17
    }else{
        23
    }

    shortlist<-data[!is.na(data[,col]),c(2,7,col)]  ##removing all the cases where particular outcomes are not available
    
    splitlist<-split(shortlist,shortlist[,2])       ##splittting the data into a list each element corresponding to a state
 
    ans<-data.frame()                               ##initialising ans dataframe
    
    ##loop for ranking and copying the data statewise into the ans dataframe
    for(n in seq_along(splitlist)){
        x<-as.data.frame(splitlist[n])
        x<-x[order(x[,1]),]
        x<-x[order(x[,3]),]
        
        ##loop to assign rank its value
        rank<-if(num=="best"){         
            1
        }else if(num=="worst"){
            nrow(x)
        }else{
            num
        }
        
        ans[n,1]<-x[rank,1]
        ans[n,2]<-x[1,2]
    }
    
    colnames(ans)<-c("hospital","state")
    rownames(ans)<-names(splitlist)
    ans
    
    
    
    
    
    
} 
