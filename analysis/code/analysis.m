%quantile calculation
incquan = wprctile(income,[0,1,5,10,20,40,60,80,90,95,99,100],weight)
weaquan = wprctile(wealth,[0,1,5,10,20,40,60,80,90,95,99,100],weight)
earnquan = wprctile(earnings,[0,1,5,10,20,40,60,80,90,95,99,100],weight)