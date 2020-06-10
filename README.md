# Package "automation" - Eliot RUIZ

The goal of this package is to provide functions automating time-consuming tasks in R: statistical testing & non-linear model choice/plotting. The package can be installed using this code, but since the auto_stats function is not finished yet, the package cannot be installed yet.
```r
library(remotes)
install_github("Eliot-RUIZ/automation")
```
<br>
<br>

## Automation of usual statistical tests

The BaseR packages as well as the numerous external packages provide an extraordinary diversity of functions coding for statistical tests and associated operations, and allows to test the association between variables in almost every specific cases. However, this powerful tool requires a great knowledge of inferential statistics in order to use it fully and correctly. Indeed, finding the right statistical test with all its assumptions and associated computations, and finding how to run such analysis can be very tedious and complex in R. Even running a complete analysis requires a lot of coding lines, which increase the risk of making errors. Finally, reporting all the results in a correct and compact format for a scientific report (APA format) is also very cumbersome!

Doing all of that can takes days of hardwork when analysing a big dataset. I therefore decided to code a function running a complete analysis in a few seconds, and displaying the results in a format enabling to simply copy and paste it in a report. 

The function *auto_stats* aims at choosing the appropriate statistical test for the data provided by the user. The data could consist in an independant variable (Y) explained by 0 to 2 factors (X). After checking each assumptions, the function compute the main test and all usually associated tasks (e.g. effect size, post-hoc tests, interaction plot).
```r
### Actual default values ###
auto_stats = function(data, Y, X1 = NULL, X2 = NULL, paired = "none", ID = NULL, digits = 3)
```
It then returns all the results in APA format to ease insertion in a text document, except for the post-hoc analysis results. The output is separated in 5 sections:
  - (Contigency table used -> for qualitative Y only)
  - Assumptions
  - Main test
  - Effect size and related 
  - Post-Hoc analysis
  - Messages
  
The last section is of major importance since many different messages have been implemented in the function for transparency of the analysis (e.g. advices, problems with the data).
  
Currently, this function is still in the making but it will be functionnal in the next few days. You can view the actual code in the R file of this page.

For more transparency due to the length of the function (approximately 1500 lines of code yet), I created on Xmind the decision tree the algorithm follows, for the statistical part. 

This decision tree can be viewed online following this link: http://www.xmind.net/m/3QZV9X

The decision tree can also be downloaded by clicking on the top right corner button and then "Download". The decision tree could then be opened in Xmind (prior download of the software necessary) to get access to the content of the boxes (R code), and also see it in the text preview (as it is organized in the function).
<br>
<br>

## Automation of the comparison using different estimators + automatic plotting of non-linear models of interest

Finding the best models and the best parameters for nonlinear models on R can be very cumbersome, in my experience. Indeed, few Self-Starters (i.e. functions allowing to automatically determine "good" starting parameters), are implemented in BaseR. It takes a long time to find a package offering the appropriate functions, even for simple models such as the power laws. When your not an expert in mathematics, even thinking to a model who could fit well enough the data can be complicated. 

The method of groping search of good starting parameters is even more tedious since even with values close to the nearest tenth of the optimal values, the *nls* function (BaseR) sometimes don't converge after 50,000 iterations! Besides, once the right parameters have been found, it is still very long to calculate the model quality estimators and the confidence intervals around the coefficients. Finally, making a correct graph representing the model can be very hard in R.

To ease such procedure, I coded the function *compare_nlm* to compare many different models, compute their estimators, and finally plot them, for a set of data consisting in 1 quantitative dependent variable (Y) and 1 quantitative factor.
```r
### Default values ###
compare_nlm = function(formula, data, digits = 2, arrange = c("AIC", "RMSE", "BIC"), increase = T, plot_model = NA, package = F)
```
<br>

After 4 to 5s of computation (on a usual laptop), the function dispaly a table containing the best coefficients (1 to 7) of 112 non-linear and 2 linear models (intercept = 0 or not). The self-starters for all those models are found in the packages "drc" and "aomisc", and *compare_nlm* uses internally the function *drm* (package "drc") to run the computation. 

Sometimes, the convergence of the self-starters might fail (model not appropried to the data), but the function will jump to the other model and print a message with the ID of model (stored at the end of the table).
```r
compare_nlm(mpg ~ wt, mtcars)

Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
  non-finite finite-difference value [2]
[1] "Convergence failed - Self-Starter ID : 59"
Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
  non-finite finite-difference value [1]
[1] "Convergence failed - Self-Starter ID : 95"
```
<br>

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
<br>

In R, the results are printed in a convenient way to allows maximum visibility for detecting the best model rapidly. They can also be printed using the *View* function:
```r
View(compare_nlm(mpg ~ wt, mtcars)) # Easier to read and to search a model
compare_nlm(mpg ~ wt, mtcars)       # Compact format as you can see below

  Rank  ID      Function        AIC     BIC    RMSE   Nb_coeffs   Coeff_1      Coeff_2      Coeff_3       Coeff_4     Coeff_5    Coeff_6    Coeff_7 
 ===== ==== ================= ======= ======= ====== ========== ============ =========== ============== =========== =========== ========== =========
    1   71       UCRS.5c       156.46  165.26  2.31    5 coeffs  b: -274.58    c: 93.75     d: 30.07      e: 2.26    f: 158.28                      
    2   77        uml4c        156.46  165.26  2.31    5 coeffs  b: -274.58    c: 93.75     d: 30.07      e: 2.26    f: 158.28                      
    3   70       UCRS.5b       156.55  165.34  2.32    5 coeffs  b: -249.34    c: 69.84     d: 30.07      e: 2.26     f: 89.31                      
    4   76        uml4b        156.55  165.34  2.32    5 coeffs  b: -249.34    c: 69.84     d: 30.07      e: 2.26     f: 89.31                      
    5   69       UCRS.5a       157.11  165.91  2.34    5 coeffs  b: -201.18    c: 66.46     d: 30.07      e: 2.26     f: 65.33                      
    6   75        uml4a        157.11  165.91  2.34    5 coeffs  b: -201.18    c: 66.46     d: 30.07      e: 2.26     f: 65.33                      
    7   16        EXD.2        157.55  161.95  2.58    2 coeffs   d: 49.66     e: 3.41                                                              
    8   99    DRC.expoDecay    157.55  161.95  2.58    2 coeffs  init: 49.66   k: 0.29                                                              
    9   100   DRC.expoGrowth   157.55  161.95  2.58    2 coeffs  init: 49.66   k: -0.29                                                             
   10   103    DRC.logCurve    157.59  161.98  2.58    2 coeffs   a: 39.26    b: -17.09                                                             
   11   88        W2x.4        157.74  165.07  2.43    4 coeffs   c: 30.87     d: 9.57       e: 1.65      t0: 1.83                                  
   12   107     DRC.poly2      158.05  163.91  2.52    3 coeffs   a: 49.93    b: -13.38      c: 1.17                                                
   13   84         W2.3        158.17  164.03  2.53    3 coeffs   b: -1.38     d: 37.03      e: 2.54                                                
   14   17        EXD.3        158.39  164.25  2.54    3 coeffs    c: 6.78     d: 57.33      e: 2.27                                                
   15   94     DRC.asymReg     158.39  164.25  2.54    3 coeffs  init: 57.34   m: 0.44    plateau: 6.78                                             
   16   109    DRC.SSasymp     158.39  164.25  2.54    3 coeffs  Asym: 6.78   R0: 57.34    lrc: -0.82                                               
   17   30          l3         158.47  164.33  2.54    3 coeffs    b: 1.51     d: 48.49      e: 2.42                                                
   18   36         LL.3        158.47  164.33  2.54    3 coeffs    b: 1.51     d: 48.49      e: 2.42                                                
   19   41        LL2.3        158.47  164.33  2.54    3 coeffs    b: 1.51     d: 48.5       e: 0.88                                                
   20   104   DRC.lorentz.3    158.61  164.48  2.55    3 coeffs    b: 0.13    d: 102.25      e: -2.5                                                
 [ reached 'max' / getOption("max.print") -- omitted 93 rows ]
```
<br>

The confidence interval around the coefficients of the model of interest can easily be calculated using the *confint* function in the "drc" package:
```r
model = drm(mpg ~ wt, data = mtcars, fct = UCRS.5c())
confint(model, level = 0.95)

                     2.5 %      97.5 %
b:(Intercept) -2226.212509 1677.054628
c:(Intercept)    67.247223  120.259724
d:(Intercept)    27.957770   32.175538
e:(Intercept)     1.840954    2.688259
f:(Intercept)   103.096202  213.467964
```
<br>

The argument *plot_model* allows to automatically plot 1 to 9 model with their RMSE by entering the ID, the partition of the graphical window being automatically adapted to the number of graphics to plot:
```r
compare_nlm(mpg ~ wt, mtcars, plot_model = c(75,23,34,88,44,46))
```
![Hnet com-image (4)](https://user-images.githubusercontent.com/15387266/84263219-641a4300-ab1f-11ea-9be6-886e3cbb0fc7.jpg)
<br>

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
|DRC.logCurve     | Linear regression on log-transformed x | ![Y = a + b \, \log(X)](https://render.githubusercontent.com/render/math?math=Y%20%3D%20a%20%2B%20b%20%5C%2C%20%5Clog(X)) |
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
<br>

In case of error, different messages will be printed to explain to the user why the error he made : error of syntax of the estimators, error in the number of digits or the ID of the models etc.
<br>
<br>

## Quick calculation of non-linear model confidence interval and predictions

The package "drc" already provides methods to calculate the confidence interval around the non-linear models created with its own Self-Starters (*predict.drc*). However, this feature is not available for the model created from "aomisc" self-starters, though some of them are often the best models (e.g. linear, exponential, logarithmic).

The function *ci_nlm* was coded to generalize CI calculations for all models used in *compare_nlm*, but also to allows the user for plotting the predictions.
```r
### Default values ###
ci_nlm = function(formula, fct, data, method = "delta", level = 0.05, nb_boot = 200, expand_x = NA, keep_cols = NULL)
```
The *predict.drc* function computes the confidence interval using the Delta method, which is very fast to compute. However, as its calculation is very complicated, I decided to use the equivalent Bootstrapp method (Bertail, Boizot & Combris, 2003), which much more simple to code, but is slower to compute. 

A warning message appears if the "aomisc" self-starter is used with the Delta method, advising him to shift *method" to *"boot"*. Beforehand, it is really easy to check the source of the function, by switching *package = FALSE* to *package = TRUE* in the *compare_nlm* function.
<br>

The latence vary with the Self-Starter used, with the number of coefficients and with the number of iterations. However, it is situated between 2 and 4s "aomisc" Self-starters with the default value of 200 iterations, while it could be much longer for "drc" Self-starters.
```r
### DRC self-starter with the Delta method -> immediate result ###
result = ci_nlm(mpg~ wt, fct = gaussian(), data = mtcars)
head(result)

  Predictions Lower_CI Upper_CI
1    22.39356 20.79063 23.99648
2    20.68509 19.36893 22.00126
3    24.94405 22.85991 27.02818
4    18.81245 17.61229 20.01262
5    17.75349 16.52585 18.98113
6    17.66496 16.43295 18.89697

### DRC self-starter with the Bootstrap method (default = 200 iterations) -> 10s ###
result = ci_nlm(mpg~ wt, fct = gaussian(), data = mtcars, method = "boot")
head(result)

Predictions Lower_CI Upper_CI
1    22.39356 21.30726 23.78245
2    20.68509 19.65370 21.97412
3    24.94405 23.84398 26.62455
4    18.81245 17.87121 19.96312
5    17.75349 16.74902 18.81757
6    17.66496 16.64668 18.71966

### AOMISC self-starter with the Bootstrap method (default = 200 iterations) -> 2s ###
result = ci_nlm(mpg~ wt, fct = DRC.expoDecay(), data = mtcars, method = "boot") 

  Predictions Lower_CI Upper_CI
1    23.01207 22.19429 24.00978
2    21.35223 20.59005 22.25067
3    25.13077 24.03569 26.36981
4    19.32383 18.49125 20.24617
5    18.08863 17.25334 19.06617
6    17.98273 17.14739 18.96891
```
<br>

The argument *keep_col* is very useful for plotting since it allows to add any other columns to the new dataframe:
```r
ci_nlm(mpg~ wt, fct = gaussian(), data = mtcars, keep_cols = c("all"))  # To keep all the columns of the initial dataframe
result = ci_nlm(mpg~ wt, fct = gaussian(), data = mtcars, keep_cols = c("mpg", "wt"))  # To keep only the columns of interest
head(result)

   mpg    wt Predictions Lower_CI Upper_CI
1 21.0 2.620    22.39356 20.79063 23.99648
2 21.0 2.875    20.68509 19.36893 22.00126
3 22.8 2.320    24.94405 22.85991 27.02818
4 21.4 3.215    18.81245 17.61229 20.01262
5 18.7 3.440    17.75349 16.52585 18.98113
6 18.1 3.460    17.66496 16.43295 18.89697
```
<br>

Plotting the model and its confidence interval is then made really easy:
```r
### Plotting in Base R ###
data = result[order(result$wt), ]           # Dataframe must be ordered by X for plotting in Base R
plot(data$wt, data$mpg)
lines(data$wt, data$Predictions, lwd = 2)
lines(data$wt, data$Lower_CI, lty = 2)
lines(data$wt, data$Upper_CI, lty = 2)
```
![Hnet com-image](https://user-images.githubusercontent.com/15387266/84260516-7776df80-ab1a-11ea-8c1a-8095a1518201.jpg)
```r
### Plotting with the "ggplot" package ###
ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = 3) +
  geom_ribbon(data = result, aes(x = wt, ymin = Lower_CI, ymax = Upper_CI), alpha = 0.5, fill = "grey") +
  geom_line(data = result, aes(x = wt, y = Predictions), size = 1.2) +
  theme_minimal()
```
![regrg2](https://user-images.githubusercontent.com/15387266/84253071-c10dfd00-ab0f-11ea-9fa1-e88bcecff71b.png)
<br>

Another argument allows the user to do predictions using the model. The user only has to fill the argument *expand_x* with two values new limits of X incorporation the previous. 

In order to ensure the accuracy of the confidence intervals, the data related to the previous X is kept as it is. The new X is calculated to completely fill the new bounds, while being spaced by the same mean step as the previous values of X, so as not to artificially inflate the "density" of the measurements, which would reduce the confidence interval around the predictions.
```r
### Plotting with the "ggplot" package ###
ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = 3) +
  geom_ribbon(data = result, aes(x = wt, ymin = Lower_CI, ymax = Upper_CI), alpha = 0.5, fill = "grey") +
  geom_line(data = result, aes(x = wt, y = Predictions), size = 1.2) +
  theme_minimal()
```
![regrg3](https://user-images.githubusercontent.com/15387266/84255455-ef410c00-ab12-11ea-8da4-41b7d2c744cb.png)
<br>

If *keep_cols* is activated, the selected columns are repeated as much as necessary to fit the number of rows of the new dataframe. 
```r
result = ci_nlm(mpg~ wt, fct = gaussian(), data = mtcars, keep_cols = c("mpg","wt"), expand_x = c(-6,10))
tail(result)

     mpg    wt    New_X Predictions   Lower_CI Upper_CI
331 15.0 3.570 9.335000    5.679318  -9.279771 20.63841
341 21.4 2.780 9.457219    5.554060  -9.839035 20.94715
351 21.0 2.620 9.579438    5.431132 -10.397176 21.25944
361 21.0 2.875 9.701656    5.310468 -10.954134 21.57507
371 22.8 2.320 9.823875    5.192001 -11.509855 21.89386
381 21.4 3.215 9.946094    5.075669 -12.064286 22.21562
```
If this is problem in your case, you can just run this line of code and it will replace the repeated rows by NA in the selected columns.
```r
replace_NA = function(prev_df, new_df, names) { 
  new_df[(nrow(prev_df) + 1) : nrow(new_df), names] = NA
  print(new_df)
  }
result_without_NA = replace_NA(mtcars, result, names = c("mpg", "wt"))
tail(result_without_NA)

    mpg wt    New_X Predictions   Lower_CI Upper_CI
331  NA NA 9.335000    5.679318  -9.279771 20.63841
341  NA NA 9.457219    5.554060  -9.839035 20.94715
351  NA NA 9.579438    5.431132 -10.397176 21.25944
361  NA NA 9.701656    5.310468 -10.954134 21.57507
371  NA NA 9.823875    5.192001 -11.509855 21.89386
381  NA NA 9.946094    5.075669 -12.064286 22.21562
```
<br>

The performance of the Delta and the Bootstrap method are very similar when the number of iterations is sufficient: in general between 100 and 200.
![regrg](https://user-images.githubusercontent.com/15387266/84254332-5e1d6580-ab11-11ea-9b97-cda72db70810.png)

However, this is not the case when the X are expanded. The CI generated by the Bootstrap method follows the fitted line while they widen approaching the limits with Delta method. This could be due to the difference of method or to complementary calculations implemented in the "drc" package, but I still did not found the answer to my question and I would be glad if someone can share it with me. I hope this will be answered when I will implement the Delta method for "aomisc" functions. I believe the Delta method is more accurate in this case since it seems logic that the CI increase while going away for the measured points. A message pop if the bootstrap method is used with the *expand* argument to warn the user about that.
![regrg4](https://user-images.githubusercontent.com/15387266/84261992-156ba980-ab1d-11ea-86b3-8c6d698b2cf5.png)
