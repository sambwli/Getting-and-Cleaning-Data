run_analysis <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character")
        
        ## Check that state and outcome are valid
        data.all<-data[,c(2,7,11,17,23)]
        
        good <- complete.cases(data.all)
        
        data.good<-data.all[good, ]
        
        
        ## initialize result.state and result.hos
        list<-split(data.good,data.good$State)
        state<-vector()
        hospital<-vector()
        
        ## For each state, find the hospital of the given rank
        for (i in 1:54){
                
                datai<-as.data.frame(list[i])
                
                ## state names
                state<-c(state,datai[1,2])
                
                if (outcome=="heart attack") {
                        mob<-as.numeric(datai[,3])
                } else if(outcome=="heart failure") {
                        mob<-as.numeric(datai[,4])
                } else {
                        mob<-as.numeric(datai[,5])
                }
                
                
                best1<-datai[mob==min(mob,na.rm=TRUE),1]
                worst1<-datai[mob==max(mob,na.rm=TRUE),1]
                
                
                best<-best1[order(best1,na.last = NA)[1]]
                worst<-worst1[order(worst1,na.last = NA)[1]]
                
                if (num == "best"){
                        
                        hospital<-c(hospital,best)
                }
                else if (num == "worst"){
                        hospital<-c(hospital,worst)
                }    
                else {
                        threshold<-mob[order(mob,na.last=TRUE)[num]]
                        rank1<-datai[mob==threshold,1]
                        rank<-rank1[order(rank1,na.last = NA)[1]]
                        hospital<-c(hospital,rank)
                }
        }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        rankall<-data.frame(hospital,state)
        
}