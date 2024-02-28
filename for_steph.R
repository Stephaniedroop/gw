# 
# 
# logical_vec<-c(F,F, T, T)
# 
# original_vals<-c(1,1,1,1)
# 
# new_vals<-c(NA, NA, 0,0)
# 
# original_vals[logical_vec]<-new_vals[logical_vec]
# 
# final_vec<-original_vals
# 
# final_vec


set.seed(1)

#We optimize these parameters to match participants from our model induction study
#Here I just give arbitrary values as an example
Pe1<-.6
Pe2<-.2
Pe3<-.7
Pe23<-.25

#We take the observed values for the case (here I'm just giving example values)
c1<-1
c2<-1
c3<-1

#And again as an example, I'm sampling the values for the exogenous nodes
e1<-as.numeric(runif(1)<Pe1)
e2<-as.numeric(runif(1)<Pe2)
e3<-as.numeric(runif(1)<Pe3)
e23<-as.numeric(runif(1)<Pe23)

#And assuming the structural equation is a disjunct of all the causes and interaction terms
effect<- max( c(min(c1,e1), min(c2,e2), min(c3, e3), min(c2*c3, e23)))

#So here the effect occurs, and is "actually caused" by the action of c1 and also by c3


#What is the probabilistic behaviour of this? I think its a noisy OR (see cheng's work from causal cognitio)
test<-1-(1-Pe1*c1)*(1-Pe2*c2)*(1-Pe3*c3)*(1-Pe23*c2*c3)
  
#We can check this through simulation: effect should be occur 92.8% of the time for this set of causes
effects<-c()
for (i in 1:1000)
{
  effects[i]<- max( c(min(c1,as.numeric(runif(1)<Pe1)),
                      min(c2,as.numeric(runif(1)<Pe2)),
                          min(c3, as.numeric(runif(1)<Pe3)),
                              min(c2*c3, as.numeric(runif(1)<Pe23))))
}
mean(effects) #seems to work!
