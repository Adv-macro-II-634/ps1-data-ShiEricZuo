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

#Concentration and skewness
#Coefficient of variation
install.packages("Weighted.Desc.Stat")
library(Weighted.Desc.Stat)

incomeCV=w.cv(income, myweight)
earnCV=w.cv(earnings, myweight)
wlthquan=w.cv(wealth,myweight)

#Variance of logs
logincome=log(income)
vlgincome=(logincome,na.rm = TRUE)

#Gini Index
install.packages("acid")
library(acid)
giniearn=weighted.gini(earnings,myweight)[1]
giniincome=weighted.gini(income,myweight)[1]
giniwlth=weighted.gini(wealth,myweight)[1]

#top1%/lowest 40%





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
