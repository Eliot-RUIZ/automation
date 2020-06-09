# Package "automation" - Eliot RUIZ

The goal of this package is to provide functions automating usual time-consuming tasks in R.
$ \sum_{\forall i}{x_i^{2}} $

## Automation of usual statistical tests

The function *auto_stats* aims at choosing the appropriate statistical test for the data provided by the user. 

The data could consist in an independant variable (Y) explained by 0 to 2 factors (X).

After checking each assumptions, the function compute the main test and all usually associated tasks (e.g. effect size, post-hoc tests, interaction plot).

It then returns all the results in APA format to ease insertion in a text document, except for the post-hoc analysis results. The output is separated in 5 sections: 
  - Assumptions
  - Main test
  - Effect size and related 
  - Post-Hoc analysis
  - Messages

The last section is of major importance since many different messages have been implemented in the function for transparency of the analysis (e.g. advices, problems with the data).

Currently, this function is still in the making but will be functionnal in the next few days.

For more transparency due to the length of the function (approximately 1500 lines of code), I created on Xmind the decisional tree the algorithm follows, for the statistical part. 

This decision tree can be viewed online following this link: http://www.xmind.net/m/MikJA4

The decision tree can also be downloaded by clicking on the top right corner button and then "Download". The decision tree could then be opened in Xmind (prior download of the software necessary) to get access to the content of the boxes (R code), and also see it in the text preview (as it is organized in the function).


## Automation of the comparison and plotting of non-linear models

The function *compare_nlm* is fully functionnal and serves to choose the most appropriate non-linear model to a set of data consisting in 1 dependent variable (Y) and 1 factor. Both variables must be quantitative.

After 4 to 5s of computation (on a usual laptop), the function a table containing the coefficients (1 to 7) of 112 non-linear and 2 linear models (intercept = 0 or not).

The user can choose to order the table (*arrange* argument), according to the AIC, and/or the BIC, and/or the RMSE and/or the number of coefficients. 

The order of the estimators reflect their importance for the user because, the dataframe is first ordered using the 1st estimator, and then, for equal values of the 1st estimator, it uses the 2nd estimator etc. 

The models with the lowest estimators (i.e. best models) are then placed on the top of the dataframe (i.e. Rank 1, Rank 2 etc). However, the dataframe could also be ordered by descending order if the argument *increase* is shifted to *FALSE*.

Here is an example obtained with the *mtcars* package, and easily exported using the *kable* function in the "knitr" package:
```r
result = compare_nlm(mpg ~ wt, mtcars, digits = 1, increase = F, arrange = c("Nb_coeffs", "AIC"))
kable(head(result, 15), row.names = F)
```

|Rank  |ID   |    Function      |RMSE  | AIC   | BIC   |Nb_coeffs  | Coeff_1    | Coeff_2   |  Coeff_3     | Coeff_4   | Coeff_5   |Coeff_6   |Coeff_7  |
|:-----|:----|:-----------------|:-----|:------|:------|:----------|:-----------|:----------|:-------------|:----------|:----------|:---------|:--------|
|1     |65   |twophase          |2.5   |165.6  |177.3  |7 coeffs   |b1: 8       |c1: 9.3    |d1: 15.8      |e1: 0.3    |b2: 3.1    |d2: 26.3  |e2: 2.7  |
|2     |15   |CRS.6             |2.4   |161.9  |172.2  |6 coeffs   |b: 1.5      |c: 5.2     |d: 87.9       |e: 1.1     |f: -394.3  |g: -2.7   |         |
|3     |61   |multi2            |5.9   |216.8  |225.5  |5 coeffs   |b1: 1.6     |b2: 55.6   |b3: 46.4      |c: 17.7    |d: 20.1    |          |         |
|4     |93   |weibull2x         |5.3   |209.6  |218.4  |5 coeffs   |b: -5.1     |c: 20.9    |d: 11.8       |e: 4.4     |t0: 4.5    |          |         |
|5     |3    |baro5             |2.5   |161.5  |170.2  |5 coeffs   |b1: 4       |b2: 2.3    |c: 7.8        |d: 34.1    |e: 2.9     |          |         |
|6     |13   |CRS.5b            |2.5   |160.7  |169.5  |5 coeffs   |b: 2.8      |c: 8.9     |d: -343.6     |e: 1.1     |f: 955.3   |          |         |
|7     |14   |CRS.5c            |2.5   |160.7  |169.5  |5 coeffs   |b: 2.8      |c: 8.7     |d: -826.6     |e: 1.1     |f: 2259.1  |          |         |
|8     |57   |ml4b              |2.5   |160.7  |169.5  |5 coeffs   |b: 2.8      |c: 8.9     |d: -343.6     |e: 1.1     |f: 955.3   |          |         |
|9     |58   |ml4c              |2.5   |160.7  |169.5  |5 coeffs   |b: 2.8      |c: 8.7     |d: -826.6     |e: 1.1     |f: 2259.1  |          |         |
|10    |5    |BC.5              |2.5   |160.6  |169.4  |5 coeffs   |b: 3.3      |c: 8.5     |d: -163.1     |e: 1.1     |f: 170.6   |          |         |
|11    |7    |bcl4              |2.5   |160.6  |169.4  |5 coeffs   |b: 3.3      |c: 8.5     |d: -163.1     |e: 1.1     |f: 170.6   |          |         |
|12    |8    |braincousens      |2.5   |160.6  |169.4  |5 coeffs   |b: 3.3      |c: 8.5     |d: -163.1     |e: 1.1     |f: 170.6   |          |         |
|13    |12   |CRS.5a            |2.5   |160.6  |169.4  |5 coeffs   |b: 2.6      |c: 8.7     |d: -294.2     |e: 0.9     |f: 792.2   |          |         |
|14    |56   |ml4a              |2.5   |160.6  |169.4  |5 coeffs   |b: 2.6      |c: 8.7     |d: -294.2     |e: 0.9     |f: 792.2   |          |         |
|+99 other lines  |   |          |   |     |     |  |      |     |        |     |     |    |   |

In R, the results are printed in a convenient way to allows maximum visibility for detecting the best model rapidly:
```r
compare_nlm(mpg ~ wt, mtcars)
```

![Capture](https://user-images.githubusercontent.com/15387266/84198162-6d65ca00-aaa3-11ea-942e-534d4e07876e.PNG)

The argument *plot_model* allows to automatically plot 1 to 9 model by entering the ID:
```r
compare_nlm(mpg ~ wt, mtcars, plot_model = c(75,23,34,88,44,46))
```

![Rplot](https://user-images.githubusercontent.com/15387266/84199785-0b5a9400-aaa6-11ea-9080-672bdfbd2145.png)

Information on the non-linear model (e.g. formula, coefficients) can be directly accessed for 93 models (in package "drc") like this:
```r
?UCRS.5c
```
The 21 remaining model (in package "aomisc") are commonly used models thus information on them can easily be found on Internet by typing their names
|Aomisc functions |  | 
|:----------------|:------------------------------|
|DRC.asymReg      | Asymptotic Regression Model |
|DRC.beta         | Beta function |
|DRC.bragg.3      | Bragg equation (3 parameters) | 
|DRC.bragg.4      | Bragg equation (4 parameters) |
|DRC.cousens85    | Yield-Weed Density function (Cousens, 1985) |
|DRC.expoDecay    | Exponential Decay Model |
|DRC.expoGrowth   | Exponential Growth Model |
|DRC.linear       | Straight line |
|DRC.linearOrigin | Straight line through origin |
|DRC.logCurve     | Linear regression on log-transformed x |
|DRC.lorentz.3    | Lorentz equation (3 parameters) |
|DRC.lorentz.4    | Lorentz equation (4 parameters) |
|DRC.negExp       | Negative exponential function |
|DRC.poly2        | Second Order Polynomial |
|DRC.powerCurve   | Power curve (Freundlich equation) |
|DRC.SSasymp      | Asymptotic regression model |
|DRC.YL           | Yield-Loss function (Cousens, 1985) |
|E.3              | Modified Gompertz equation (3 parameters) |
|E.4              | Modified Gompertz equation (4 parameters) |
|L.2              | Logistic (ED50 as parameter) |

  ![equation]Y=b0+b1X+b2X2
