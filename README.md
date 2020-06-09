# Package "automation" - Eliot RUIZ

The goal of this package is to provide functions automating time-consuming tasks in R: statistical testing & non-linear model choice/plotting.



## Automation of usual statistical tests

```r
### Actual default values ###
auto_stats = function(data, Y, X1 = NULL, X2 = NULL, paired = "none", ID = NULL, theoric_mean = NULL, digits = 3)
```

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

For more transparency due to the length of the function (approximately 1500 lines of code), I created on Xmind the decision tree the algorithm follows, for the statistical part. 

This decision tree can be viewed online following this link: http://www.xmind.net/m/3QZV9X

The decision tree can also be downloaded by clicking on the top right corner button and then "Download". The decision tree could then be opened in Xmind (prior download of the software necessary) to get access to the content of the boxes (R code), and also see it in the text preview (as it is organized in the function).



## Automation of the comparison using different estimators + automatic plotting of non-linear models of interest

The function *compare_nlm* is fully functionnal and serves to choose the most appropriate non-linear model to a set of data consisting in 1 dependent variable (Y) and 1 factor. Both variables must be quantitative.

```r
### Default values ###
compare_nlm = function(formula, data, digits = 2, arrange = c("AIC", "RMSE", "BIC"), increase = T, plot_model = NA, package = F)
```

After 4 to 5s of computation (on a usual laptop), the function dispaly a table containing the best coefficients (1 to 7) of 112 non-linear and 2 linear models (intercept = 0 or not). The self-starters for all those models are found in the packages "drc" and "aomisc", and *compare_nlm* uses internally the function *drm* (package "drc) to run the computation. 

Sometimes, the convergence of the self-starters might fail (model not appropried to the data), but the function will jump to the other model and print a message with the ID of model (stored at the end of the table).

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

In R, the results are printed in a convenient way to allows maximum visibility for detecting the best model rapidly. They can also be printed using the *View* function:
```r
compare_nlm(mpg ~ wt, mtcars)
View(compare_nlm(mpg ~ wt, mtcars))
```

![Capture](https://user-images.githubusercontent.com/15387266/84198162-6d65ca00-aaa3-11ea-942e-534d4e07876e.PNG)

The argument *plot_model* allows to automatically plot 1 to 9 model with their RMSE by entering the ID, the partition of the graphical window being automatically adapted to the number of graphics to plot:
```r
compare_nlm(mpg ~ wt, mtcars, plot_model = c(75,23,34,88,44,46))
```

![Rplot](https://user-images.githubusercontent.com/15387266/84199785-0b5a9400-aaa6-11ea-9080-672bdfbd2145.png)

Information on the non-linear model (e.g. formula, coefficients) can be directly accessed for 93 models (in package "drc") by typing their names preceded by a question mark:
```r
?UCRS.5c
```

If the name of the package implementing the function appear in front of the name of the model if the argument *package* is switched to *TRUE*.

The names of the 21 remaining model (in package "aomisc") are displayed below and more informations on them can be obtained following this [link1](https://www.statforbiology.com/2020/stat_nls_usefulfunctions/#exponential-function) and this [link2](https://www.statforbiology.com/nonlinearregression/usefulequations):

|Aomisc functions used | Name of the model | Formula of the model  |
|:----------------|:----------------------------------------------|:-----------------|
|DRC.asymReg      | Asymptotic Regression Model | ![Y = a - (a - b) \, \exp (- c  X) \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20a%20-%20(a%20-%20b)%20%5C%2C%20%5Cexp%20(-%20c%20%20X)%20%5Cquad%20%5Cquad%20%5Cquad) |
|DRC.beta         | Beta function | ![Y = d \,\left\{  \left( \frac{X - X_b}{X_o - X_b} \right) \left( \frac{X_c - X}{X_c - X_o} \right) ^ {\frac{X_c - X_o}{X_o - X_b}} \right\}^b \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20d%20%5C%2C%5Cleft%5C%7B%20%20%5Cleft(%20%5Cfrac%7BX%20-%20X_b%7D%7BX_o%20-%20X_b%7D%20%5Cright)%20%5Cleft(%20%5Cfrac%7BX_c%20-%20X%7D%7BX_c%20-%20X_o%7D%20%5Cright)%20%5E%20%7B%5Cfrac%7BX_c%20-%20X_o%7D%7BX_o%20-%20X_b%7D%7D%20%5Cright%5C%7D%5Eb%20%5Cquad%20%5Cquad%20%5Cquad) |
|DRC.bragg.3      | Bragg equation (3 parameters) | ![Y = d \, \exp \left\[ - b (X - e)^2 \right\] \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20d%20%5C%2C%20%5Cexp%20%5Cleft%5B%20-%20b%20(X%20-%20e)%5E2%20%5Cright%5D%20%5Cquad%20%5Cquad%20%5Cquad) |
|DRC.bragg.4      | Bragg equation (4 parameters) | ![Y = c + (d - c) \, \exp \left\[ - b (X - e)^2 \right\] \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20c%20%2B%20(d%20-%20c)%20%5C%2C%20%5Cexp%20%5Cleft%5B%20-%20b%20(X%20-%20e)%5E2%20%5Cright%5D%20%5Cquad%20%5Cquad%20%5Cquad) |
|DRC.cousens85    | Yield-Weed Density function (Cousens, 1985) | ![Y_W  = Y_{WF} \left( 1 - \frac{i\, X}{100 \left( 1 + \frac{i \, X}{a} \right) } \right)](https://render.githubusercontent.com/render/math?math=Y_W%20%20%3D%20Y_%7BWF%7D%20%5Cleft(%201%20-%20%5Cfrac%7Bi%5C%2C%20X%7D%7B100%20%5Cleft(%201%20%2B%20%5Cfrac%7Bi%20%5C%2C%20X%7D%7Ba%7D%20%5Cright)%20%7D%20%5Cright)) |
|DRC.expoDecay    | Exponential Decay Model | ![Y = a  e^{k X} \quad (k < 0)](https://render.githubusercontent.com/render/math?math=Y%20%3D%20a%20%20e%5E%7Bk%20X%7D%20%5Cquad%20(k%20%3C%200)) |
|DRC.expoGrowth   | Exponential Growth Model | ![Y = a  e^{k X} \quad (k > 0)](https://render.githubusercontent.com/render/math?math=Y%20%3D%20a%20%20e%5E%7Bk%20X%7D%20%5Cquad%20(k%20%3E%200)) |
|DRC.linear       | Straight line | ![Y = b_0 + b_1 \, X \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20b_0%20%2B%20b_1%20%5C%2C%20X%20%5Cquad%20%5Cquad%20%5Cquad) | b0  is the value of Y when X=0, b1 is the slope |
|DRC.linearOrigin | Straight line through origin | ![Y = b_1 \, X \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20b_1%20%5C%2C%20X%20%5Cquad%20%5Cquad%20%5Cquad) |
|DRC.logCurve     | ![Y = a + b \, \log(X)](https://render.githubusercontent.com/render/math?math=Y%20%3D%20a%20%2B%20b%20%5C%2C%20%5Clog(X)) |
|DRC.lorentz.3    | Lorentz equation (3 parameters) | ![Y = \frac{d} { 1 + b (X - e)^2 } \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20%5Cfrac%7Bd%7D%20%7B%201%20%2B%20b%20(X%20-%20e)%5E2%20%7D%20%5Cquad%20%5Cquad%20%5Cquad) |
|DRC.lorentz.4    | Lorentz equation (4 parameters) | ![Y = c + \frac{d - c} { 1 + b (X - e)^2 } \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20c%20%2B%20%5Cfrac%7Bd%20-%20c%7D%20%7B%201%20%2B%20b%20(X%20-%20e)%5E2%20%7D%20%5Cquad%20%5Cquad%20%5Cquad) |
|DRC.negExp       | Negative exponential function | ![Y = a \[1 -  \exp (- c  X) \] \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20a%20%5B1%20-%20%20%5Cexp%20(-%20c%20%20X)%20%5D%20%5Cquad%20%5Cquad%20%5Cquad) |
|DRC.poly2        | Second Order Polynomial | ![Y = b_0 + b_1\, X + b_2 \, X^2 \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20b_0%20%2B%20b_1%5C%2C%20X%20%2B%20b_2%20%5C%2C%20X%5E2%20%5Cquad%20%5Cquad%20%5Cquad) |
|DRC.powerCurve   | Power curve (Freundlich equation) | ![Y = a \, X^b \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20a%20%5C%2C%20X%5Eb%20%5Cquad%20%5Cquad%20%5Cquad) |
|DRC.SSasymp      | Asymptotic regression model | ![Y = a - (a - b) \, \exp (- log(c)  X) \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20a%20-%20(a%20-%20b)%20%5C%2C%20%5Cexp%20(-%20log(c)%20%20X)%20%5Cquad%20%5Cquad%20%5Cquad) |
|DRC.YL           | Yield-Loss function (Cousens, 1985) | ![Y = \frac{i \, X}{1 + \frac{i \, X}{a}}](https://render.githubusercontent.com/render/math?math=Y%20%3D%20%5Cfrac%7Bi%20%5C%2C%20X%7D%7B1%20%2B%20%5Cfrac%7Bi%20%5C%2C%20X%7D%7Ba%7D%7D) |
|E.3              | Modified Gompertz equation (3 parameters) | ![Y = c + (1 - c) \left\{ 1 - \exp \left\{- \exp \left\[ b \, (X - e) \right\] \right\} \right\} \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20c%20%2B%20(1%20-%20c)%20%5Cleft%5C%7B%201%20-%20%5Cexp%20%5Cleft%5C%7B-%20%5Cexp%20%5Cleft%5B%20b%20%5C%2C%20(X%20-%20e)%20%5Cright%5D%20%5Cright%5C%7D%20%5Cright%5C%7D%20%5Cquad%20%5Cquad%20%5Cquad) |
|E.4              | Modified Gompertz equation (4 parameters) | ![Y = c + (d - c) \left\{ 1 - \exp \left\{- \exp \left\[ b \, (X - e) \right\] \right\} \right\} \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20c%20%2B%20(d%20-%20c)%20%5Cleft%5C%7B%201%20-%20%5Cexp%20%5Cleft%5C%7B-%20%5Cexp%20%5Cleft%5B%20b%20%5C%2C%20(X%20-%20e)%20%5Cright%5D%20%5Cright%5C%7D%20%5Cright%5C%7D%20%5Cquad%20%5Cquad%20%5Cquad) |
|L.2              | Logistic (2 parameters) | ![Y = \frac{1}{1 + exp(- b (X - e))} \quad \quad \quad](https://render.githubusercontent.com/render/math?math=Y%20%3D%20%5Cfrac%7B1%7D%7B1%20%2B%20exp(-%20b%20(X%20-%20e))%7D%20%5Cquad%20%5Cquad%20%5Cquad) |

In case of error, different messages will be printed to explain to the user why the error he made : error of syntax of the estimators, error in the number of digits or the ID of the models etc.



## To complete

```r
### Default values ###
compare_nlm = function(formula, data, digits = 2, arrange = c("AIC", "RMSE", "BIC"), increase = T, plot_model = NA, package = F)
```
