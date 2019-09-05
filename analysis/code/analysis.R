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
install.packages("Weighted.Desc.Stat")
library(Weighted.Desc.Stat)

incomeCV=w.cv(income, myweight)
earnCV=w.cv(earnings, myweight)
wlthCV=w.cv(wealth,myweight)

#Variance of logs
logincome=log(income)
vlgincome=(logincome, na.rm = TRUE)

#Gini Index
install.packages("acid")
library(acid)
giniearn=weighted.gini(earnings,myweight)[1]
giniincome=weighted.gini(income,myweight)[1]
giniwlth=weighted.gini(wealth,myweight)[1]

#top1%/lowest 40%
em<-matrix(c(earnings,myweight),nrow=22085,ncol = 2,byrow = FALSE)
require(data.table)
em.dt <- data.table(em, key="V1")
totwgt=sum(myweight)
i=22085
tempwgt=0
repeat{
  tempwgt=tempwgt+em.dt[i,2]
  i=i-1
  if (tempwgt>=0.1*totwgt ) break
}
topweighte<-c(em.dt[2,c(i:22805)])
topearn<-c(em.dt[1,c(i:22805)])
tottop1<-sum(topwealth*topwealth)
j=1
tempwgt=0
repeat{
  tempwgt=tempwgt+em.dt[j,2]
  j=j+1
  if (tempwgt=0.4*totwgt ) break
}
lowweighte<-c(em.dt[2,c(1:j)])
lowearn<-c(em.dt[1,c(1:j)])
totlow4<-sum(lowwealth*lowwealth)


#location of mean
locator<-function(target_value,data,wgt){
  i=0
  repeat{
    i=i+1
  if (wtd.quantile(data, i/100, weight=wgt)>=target_value ) break
    }
  return(i)
}
locameanearn=locator(wtd.mean(earnings,myweight),earnings,myweight)
locameaninc=locator(wtd.mean(income,myweight),income,myweight)
locameanwlth=locator(wtd.mean(wealth,myweight),wealth,myweight)

#Mean/median
MMearn=wtd.mean(earnings,myweight)/wtd.quantile(earnings,0.5,weight = myweight)
MMincome=wtd.mean(income,myweight)/wtd.quantile(income,0.5,weight = myweight)
MMwlth=wtd.mean(wealth,myweight)/wtd.quantile(wealth,0.5,weight = myweight)

#Lorenz Curve
library(ineq)
Lcearn<-Lc(earnings, myweight)
plot(Lcearn,main="Earnings Lorenz Curve")
Lcinc<-Lc(income, myweight)
plot(Lcinc,main="Income Lorenz Curve")
Lcwlth<-Lc(wealth,myweight)
plot(Lcwlth,main="Wealth Lorenz Curve")
