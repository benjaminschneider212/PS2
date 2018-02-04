###PROBLEM SET 2###
#Benjamin Schneider



vec<-c(1245,925,1622,2101,42)
x<-substr(vec, 1, 1)
new<-table(x)

leemis<- function(){
  #for (i=1){
  i=1
  y= new[1]/length(x)-log10(1+1/i)
  return(y)
}
y 

# return(max(y))
#}
leemis()
new
signif(vec, digits=1)
substr(c(123, 5664, 4445), 1, 1)
?substr

chogains<- function(){
  y<-(x)-log10(1+1/x)
  sqrt(sum(y))
  
}
?sum

elemsum<-N #Leemis' Statistic
#Create some toy data to test the loop
a<-c(548,265489,16514,651,864,31,54,231,648,23)
b<-NULL
xi<-NULL

###Leemis
leemis<-function(a){
  #argument a will be a vector of total votes
  b<-as.numeric(substr(a, start=1, stop=1)) #This extracts the first number of whatever values are fed into b.
  proportions<-NULL
  for (i in 1:9){ #This loop creates x_i, a proportion of occurrence of each significant digit
    y<-sum(b==i)/length(a)
    proportions<-c(proportions,y)
  }
  xi<-proportions
  argument<-NULL
  for(i in 1:9){ #This loop calculates the inside of the max function for Leemis m
    element<-(xi[i]-log((1+1/i), base=10))
    argument<-c(argument,element)
  }
  argmax<-max(argument) #This is m
  return(argmax)
}
leemis(a)

chosum<-NULL
cho<-function(c){
  #argument a will be a vector of total votes
  b<-as.numeric(substr(a, start=1, stop=1)) #This extracts the first number of whatever values are fed into b.
  proportions<-NULL
  for (i in 1:9){ #This loop creates x_i, a proportion of occurrence of each significant digit
    y<-sum(b==i)/length(a)
    proportions<-c(proportions,y)
  }
  xi<-proportions
  argument<-NULL
  for(i in 1:9){ #This loop calculates the inside of the max function for Leemis m
    element<-((xi[i]-log((1+1/i), base=10))^2)
    chosum<-c(chosum,element)
    finvec<-sum(chosum)
    d<-sqrt(finvec)
  }
  return(d)
}

return(chogains)
}
cho(a)ULL