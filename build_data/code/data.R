# The summary table of 2007 gives the amount adjusted to 2016 dollar.
# Using the inflation calculator given by BIS (from July 2016 to July 2007), the inflation factor of 1.16 is applied here to switch the value
# in summary table back to the 2007 price level.
inffac=1.16

#Wealth
#See net worth flowchart on https://www.federalreserve.gov/econres/scfindex.htm for the calculation of NETWORTH
#For simplicty, divide these values by 1000, so the unit here is a thousand dollars

wealth=scf$NETWORTH*0.001/inffac

#Earnings
#As in DGR, a fraction of 0.863 is applied to business and farm income when calculating the earnings
earnings=(scf$WAGEINC+scf$BUSSEFARMINC*0.863)*0.001/inffac

#Income
income=(scf$WAGEINC+scf$BUSSEFARMINC+scf$INTDIVINC+scf$KGINC+scf$SSRETINC+scf$TRANSFOTHINC)*0.001/inffac

#Weight
#To replicate the quantile distributions table in DGR, weighted quantile is used. 
weight=scf$WGT

