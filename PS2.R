###PROBLEM SET 2###
#Benjamin Schneider

#Problem 1

a<-c(345,6758,4563,5968,128,457,589,783,90,274,346) #Example of an input vector of vote returns. Just me typing random keys.
leemis<-function(a){
  b<-as.numeric(substr(a, start=1, stop=1))
  proportions<-NULL
  for (i in 1:9){
    y<-sum(b==i)/length(a)
    proportions<-c(proportions,y)
  }
  xi<-proportions
  argument<-NULL
  for(i in 1:9){
    func<-(xi[i]-log((1+1/i), base=10))
    fullfunc<-c(argument,func)
  }
  maxfunc<-max(fullfunc)
  return(maxfunc)
}
leemis(a) #this is an example of running this equation
chovec<-NULL
chogains<-function(a){
  b<-as.numeric(substr(a, start=1, stop=1)) 
  proportions<-NULL
  for (i in 1:9){
    y<-sum(b==i)/length(a)
    proportions<-c(proportions,y)
  }
  xi<-proportions
  argument<-NULL
  for(i in 1:9){
    element<-((xi[i]-log((1+1/i), base=10))^2)
    chovec<-c(chovec,element)
    finalvec<-sum(chovec)
    output<-sqrt(finalvec)
  }
  return(output)
}
chogains(a) #output is number of the output vector

benford.function<-function(a,leemis,chogains){
  if(leemis==T){
    leemis<-leemis(a)} 
  else{
    leemis<-NULL}
  if(chogains==T){
    chogains<-chogains(a)} 
  else{
    chogains<-NULL}
  digitdist<-table(as.numeric(substr(a, start=1, stop=1)))
  answer<-list(leemis,chogains,digitdist) 
  names(answer)<-c("Leemis", 'Cho-Gains', "Distribution of Digits")
  return(answer)
}
benford.function(a,T,F)
benford.function(a,F,T)
benford.function(a,T,T)
benford.function(a,F,F)


#Problem 2



