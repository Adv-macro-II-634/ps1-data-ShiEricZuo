% The summary table of 2007 gives the amount adjusted to 2016 dollar.
% Using the inflation calculator given by BIS (from July 2016 to July
% 2007), the inflation factor of 1.16 is applied here to switch the value
% in summary table back to the 2007 price level.

% Wealth
% See net worth flowchart on
% https://www.federalreserve.gov/econres/scfindex.htm for the calculation
% of NETWORTH
wealth = SCFP20072.NETWORTH/1.16

% Income
income = SCFP20072.INCOME/1.16

%earnings are wage income plus business and farm income
earnings = (SCFP20072.WAGEINC + SCFP20072.BUSSEFARMINC)/1.16