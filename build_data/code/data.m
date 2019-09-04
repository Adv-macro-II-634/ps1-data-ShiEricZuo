% The summary table of 2007 gives the amount adjusted to 2016 dollar.
% Using the inflation calculator given by BIS (from July 2016 to July
% 2007), the inflation factor of 1.16 is applied here to switch the value
% in summary table back to the 2007 price level.

% Wealth
% See net worth flowchart on
% https://www.federalreserve.gov/econres/scfindex.htm for the calculation
% of NETWORTH
inffac=1.16
%For simplicty, divide these values by 1000, so the unit here is a thousand
%dollars
wealth = 0.001*SCFP20072.NETWORTH/inffac

% Income
income = 0.001*SCFP20072.INCOME/inffac

%earnings are wage income plus business and farm income
earnings = 0.001*(SCFP20072.WAGEINC + 0.863* SCFP20072.BUSSEFARMINC)/inffac

weight=SCFP20072.WGT