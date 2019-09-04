%quantile calculation
incquan = quantile(income,[0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1])
weaquan = quantile(wealth,[0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1])
earnquan = quantile(earnings,[0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1])