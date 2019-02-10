# Approach
The attributes (min_price, max_price, modal price and max price) were converted into time series data.

Then the seasonality of the selected attribute was found using time series library in R.

The attribute was then deseasonalized by using time series library in R and the result was plotted.

ADF was performed on the attribute to check for the stability of the data.

If the attribute was found not stable it was appended in the flag data frame.

The final deseasonized attribute was then plotted with the MSP price of that particular commodity.



# Major Hurdles encountered
Commodities were not commom in both the database provided.

New to time series data always donE EDA and Statistical inference

NOTE : Used R not Python because according to me R has more feature for dataanalysis things and python has more supprt for development of Deep Learning models.




