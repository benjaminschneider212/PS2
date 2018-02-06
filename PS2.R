###PROBLEM SET 2###
#Benjamin Schneider

#Problem 1

a<-c(345,6758,4563,5968,128,457,589,783,90,274,346) #Example of an input vector of vote returns. Just me typing random keys.
leemis<-function(a){
  firstnumber<-as.numeric(substr(a, start=1, stop=1)) #this takes just the first number of the vector of vote totals and makes new vector.
  proportions<-NULL
  for (i in 1:9){ #creates x_i for the for loop
    y<-sum(firstnumber==i)/length(a)
    proportions<-c(proportions,y)}
  x_i<-proportions #x_i made
  argument<-NULL
  for(i in 1:9){
    func<-(x_i[i]-log((1+1/i), base=10)) #executing the equations
    fullfunc<-c(argument,func)}
  output<-max(fullfunc) #taking the max of the equation
  return(output)
}

leemis(a) #this is an example of running this function

chovec<-NULL
chogains<-function(a){
  firstnumber<-as.numeric(substr(a, start=1, stop=1)) #this takes just the first number of the vector of vote totals and makes new vector.
  proportions<-NULL
  for (i in 1:9){ #creates x_i for the for loop
    y<-sum(firstnumber==i)/length(a)
    proportions<-c(proportions,y)}
  x_i<-proportions #x_i made
  argument<-NULL
  for(i in 1:9){
    element<-((x_i[i]-log((1+1/i), base=10))^2) #calculation inside summation
    chovec<-c(chovec,element)
    finalvec<-sum(chovec) #the summation
    output<-sqrt(finalvec)} #squre root of the summation
  return(output)
}

chogains(a) #example of function

#Great, so now that those are done, I am going to use them into this new function that will do all of the above
#separating them is useful because when I tried doing them all together making mistakes was much easier.
benford.function<-function(a,Leemis, ChoGains){
  if(Leemis==T){ #if else used to print the leemis
    Leemis<-leemis(a)} #use of the leemis function written above
  else{
    Leemis<-"You did not request this output"} #inclusion of this for clarity
  if(ChoGains==T){ #if else for Cho
    ChoGains<-chogains(a)} #use of chogains function written above
  else{
    ChoGains<-"You did not request this output"} #once again included for clarity
  digitdist<-table(as.numeric(substr(a, start=1, stop=1))) #this shows the frequency of the various first digits
  output<-list(Leemis,ChoGains,digitdist) #output as a list
  names(output)<-c("Leemis", 'Cho-Gains', "Distribution of Digits")
  return(output)
}
#some examples of how the function works with its different arguments and outputs
benford.function(a,T,F) #Only the leemis is being printed
benford.function(a,F,T) #Only the Cho-Gains is being printed
benford.function(a,T,T) #both printed
benford.function(a,F,F) #neither printed


#Problem 2

print.benfords<-function(a){
  output<-as.data.frame(benford.function(a,T,T)) 
  pval<-benford.function(a,T,T)
  if (0.851 < pval[1] & pval[1] < 0.967){
    output$Leemis[1]<-paste(pval[1],"*" )}
  if (0.967 <= pval[1] & pval[1] < 1.212){
    output$Leemis[1]<-paste(pval[1],"**" )}
  if (1.212<= pval[1] ){
    output$Leemis[1]<-paste(pval[1],"***" )}
  if (1.212 < pval[2] & pval[2] <1.330){
    output$Cho-Gains[1]<-paste(pval[2],"*" )}
  if (1.330 <= pval[2] & pval[2] < 1.569){
    output$Cho-Gains[1]<-paste(pval[2],"**" )}
  if (1.569 <= pval[2]){
    output$Cho-Gains[1]<-paste(pval[2],"***" )}
  print(output)
  cat("Note: P-value <0.01 ***, <0.05 **, <0.1 *")
}
print.benfords(a)

save<-function(a){#save that stuff
  sink(file="benfordspvalues.csv")
  print.benfords(a)
  sink()
}
save(a)
