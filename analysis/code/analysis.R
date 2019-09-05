#Calculate the quantiles distribution

#For weighted quantile, a new package
install.packages("reldist")
library(reldist)

quan=c(0,0.01,0.05,0.10,0.20,0.40,0.60,0.80,0.90,0.95,0.99,1)
incquan=c(0,1:11)
earnquan=c(0,1:11)
wlthquan=c(0,1:11)
for (i in 1:12){
  incquan[i]=wtd.quantile(income,quan[i],weight = myweight)
  earnquan[i]=wtd.quantile(earnings,quan[i],weight = myweight)
  wlthquan[i]=wtd.quantile(wealth,quan[i],weight = myweight)
}

#replicate "Table 1" 
quantile<-matrix(c(earnquan,incquan,wlthquan),ncol=12,nrow=3,byrow = TRUE)
colnames(quantile)<-c("0","1","5","10","20","40","60","80","90","95","99","100")
rownames(quantile)<-c("Earnings","Income","Wealth")
quantile<-as.table(quantile)
format(quantile,scientific = FALSE)
round(quantile,2)

#Concentration and skewness
#Coefficient of variation
install.packages("Weighted.Desc.Stat") #The package used for weighted statistics
library(Weighted.Desc.Stat)

incomeCV=w.cv(income, myweight)
earnCV=w.cv(earnings, myweight)
wlthCV=w.cv(wealth,myweight)

#Variance of logs
#earnings
earnvlma<-matrix(c(earnings, myweight),nrow = 22085,ncol=2,byrow = FALSE)
earnvlma<-earnvlma[-which(earnvlma<=0,arr.ind = TRUE)[,1],] ####remove all nonpositive earnings together with their weights
earnvl<-wtd.var(log(earnvlma[,1]),earnvlma[,2])

#income
incvlma<-matrix(c(income, myweight),nrow = 22085,ncol=2,byrow = FALSE)
incvlma<-incvlma[-which(incvlma<=0,arr.ind = TRUE)[,1],] ####remove all nonpositive income together with their weights
incvl<-wtd.var(log(incvlma[,1]),incvlma[,2])

#wealth
wlthvlma<-matrix(c(wealth, myweight),nrow = 22085,ncol=2,byrow = FALSE)
wlthvlma<-wlthvlma[-which(wlthvlma<=0,arr.ind = TRUE)[,1],] ####remove all nonpositive earnings together with their weights
wlthvl<-wtd.var(log(wlthvlma[,1]),wlthvlma[,2])

#Gini Index
install.packages("acid") ##The package can calculate gini index, taking weight into account
library(acid)
giniearn=weighted.gini(earnings,myweight)[1]
giniincome=weighted.gini(income,myweight)[1]
giniwlth=weighted.gini(wealth,myweight)[1]

#############top1%/lowest 40%####################
#earnings
em<-matrix(c(earnings,myweight),nrow=22085,ncol = 2,byrow = FALSE)  ########create an earnings-weight matrix
require(data.table)
em.dt <- data.table(em, key="V1")  ##############"table" the matrix to sort by earnings
em.dt<-as.matrix(em.dt) ################# "matrix" it back
totwgt=sum(myweight)
i=22085
tempwgt=0
repeat{
  tempwgt=tempwgt+em.dt[i,2]
  i=i-1
  if (tempwgt>=0.01*totwgt ) break
}                               ########## track the hhs from the top until the cumulative weights from top earning hhs exceeds the 1% of total weight
topearnings<-c(em.dt[c(i:22085),1]) 
topweighte<-c(em.dt[c(i:22085),2])
topearnings1pr<-sum(topearnings*topweighte) ##calculate the weight-adjusted total earnings of these hhs

j=1
tempwgt=0
repeat{
  tempwgt=tempwgt+em.dt[j,2]
  j=j+1
  if (tempwgt>=0.4*totwgt ) break
}                            ########## track the hhs from the bottom until the cumulative weights from bottom earning hhs exceeds the 40% of total weight
lowearnings<-c(em.dt[c(1:j),1])
lowweighte<-c(em.dt[c(1:j),2])
lowearnings40pr<-sum(lowearnings*lowweighte)
ratio_earning<-(topearnings1pr/(0.01*totwgt))/(lowearnings40pr/(0.4*totwgt))

#income : idea is the same, just changing earnings to income
im<-matrix(c(income,myweight),nrow=22085,ncol = 2,byrow = FALSE) ########create an income-weight matrix
require(data.table)
im.dt <- data.table(im, key="V1") ##############"table" the matrix to sort by income
im.dt<-as.matrix(im.dt) ################# "matrix" it back
totwgt=sum(myweight)
i=22085
tempwgt=0
repeat{
  tempwgt=tempwgt+im.dt[i,2]
  i=i-1
  if (tempwgt>=0.01*totwgt ) break
}
topincome<-c(im.dt[c(i:22085),1])
topweighti<-c(im.dt[c(i:22085),2])
topincome1pr<-sum(topincome*topweighti)

j=1
tempwgt=0
repeat{
  tempwgt=tempwgt+im.dt[j,2]
  j=j+1
  if (tempwgt>=0.4*totwgt ) break
}
lowincome<-c(im.dt[c(1:j),1])
lowweighti<-c(im.dt[c(1:j),2])
lowincome40pr<-sum(lowincome*lowweighti)
ratio_income<-(topincome1pr/(0.01*totwgt))/(lowincome40pr/(0.4*totwgt))


#wealth:  idea is the same, just changing earnings to wealth
wm<-matrix(c(wealth,myweight),nrow=22085,ncol = 2,byrow = FALSE) ########create an wealth-weight matrix
require(data.table)
wm.dt <- data.table(wm, key="V1") ##############"table" the matrix to sort by wealth
wm.dt<-as.matrix(wm.dt) ################# "matrix" it back
totwgt=sum(myweight)
i=22085
tempwgt=0
repeat{
  tempwgt=tempwgt+wm.dt[i,2]
  i=i-1
  if (tempwgt>=0.01*totwgt ) break
}
topwealth<-c(wm.dt[c(i:22085),1])
topweightw<-c(wm.dt[c(i:22085),2])
topwealth1pr<-sum(topwealth*topweightw)

j=1
tempwgt=0
repeat{
  tempwgt=tempwgt+wm.dt[j,2]
  j=j+1
  if (tempwgt>=0.4*totwgt ) break
}
lowwealth<-c(wm.dt[c(1:j),1])
lowweightw<-c(wm.dt[c(1:j),2])
lowwealth40pr<-sum(lowwealth*lowweightw)
ratio_wealth<-(topwealth1pr/(0.01*totwgt))/(lowwealth40pr/(0.4*totwgt))



################location of mean###################
locator<-function(target_value,data,wgt){
  i=0
  repeat{
    i=i+1
  if (wtd.quantile(data, i/100, weight=wgt)>=target_value ) break
    }
  return(i)
} #########The function help locate a value in the distribution, given its quantile
locameanearn=locator(wtd.mean(earnings,myweight),earnings,myweight)
locameaninc=locator(wtd.mean(income,myweight),income,myweight)
locameanwlth=locator(wtd.mean(wealth,myweight),wealth,myweight)

#Mean/median
MMearn=wtd.mean(earnings,myweight)/wtd.quantile(earnings,0.5,weight = myweight)
MMincome=wtd.mean(income,myweight)/wtd.quantile(income,0.5,weight = myweight)
MMwlth=wtd.mean(wealth,myweight)/wtd.quantile(wealth,0.5,weight = myweight)

########Replicate "Table 2"
concenvalue<-c(earnCV,incomeCV,wlthCV,earnvl,incvl,wlthvl,giniearn,giniincome,giniwlth,ratio_earning,ratio_income,ratio_wealth,locameanearn,locameaninc,locameanwlth,MMearn,MMincome,MMwlth)
concentration<-matrix(concenvalue,nrow = 6,ncol = 3,byrow = TRUE)
colnames(concentration)<-c("Earnings","Income","Wealth")
rownames(concentration)<-c("Coefficient of variation","Variance of logs","Gini Index","Top1%/lowest 40%","Location of mean%","Mean/Median")
concentration<-as.table(concentration)
format(concentration,scientific = FALSE)
round(concentration,2)


#Lorenz Curve
library(ineq) #######the package help measures the inequality
Lcearn<-Lc(earnings, myweight)
plot(Lcearn,main="Earnings Lorenz Curve")
Lcinc<-Lc(income, myweight)
plot(Lcinc,main="Income Lorenz Curve")
Lcwlth<-Lc(wealth,myweight)
plot(Lcwlth,main="Wealth Lorenz Curve")
