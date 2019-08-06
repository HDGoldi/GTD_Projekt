As the boxplots above did already show, the observed variables are not distributed normally. 
This hyptohesis can be backed by running the Shapiro-Wilk Normality Test (shapiro.test) on a sample of 5000 rows.  
This function returns a p-value of 2.2∗10^-16 which far less than 0.05 implying that the distribution of the 
data is significantly different from normal distribution. 