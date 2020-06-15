# Package "automation" - Eliot RUIZ

The goal of this package is to provide functions automating time-consuming tasks in R: statistical testing & non-linear modelling/plotting. Run the following lines of code to install it on your computer:
```r
### Usual installation ###

# External installation of the "aomisc" package not hosted on CRAN with the devtools::install_github function:
install.packages("devtools")
library(devtools)
install_github("OnofriAndreaPG/aomisc")

# Installation of the "automation" package and all its dependencies:
install_github("Eliot-RUIZ/automation") # Type multiple time on the Enter button to skip updates of package if asked
library(automation) # Loading

### In case of error during the installation, first run the lines below, and then re-run those above ###

# Fixing potential errors during installation of "aomisc" mentionning a problem of version:
install.packages("installr")
library(installr)
updateR() 

# Fixing errors mentionning the "curl" package: 
install.packages("curl")
library(curl)

# Fixing errors mentionning the "mixOmics" package:
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
if (BiocManager::version() >= package_version('3.11')) {
  BiocManager::install("mixOmicsTeam/mixOmics")
} else {
  message('Please update to the latest Bioconductor (https://www.bioconductor.org/install/) ',
          'to install the stable GitHub version') }
library(mixOmics) 
```
<br>

## Automation of usual statistical tests: *auto_stats*

The Base R packages as well, as the numerous external packages hosted on CRAN and Github (around 80,000) provide an extraordinary diversity of functions coding most statistical tests and associated operations. This powerful tool widely used across the scientific community allows to get a better understanding of the association between variables (inter alia) in almost every specific cases. However, this powerful tool requires a great knowledge of inferential statistics to use it fully and correctly. Indeed, finding the right statistical test with all its assumptions and associated computations while assessing how to run such analysis in R can be very tedious and complex. Even running a complete analysis requires a lot of coding lines, thus increasing the risk of errors and decreasing the time available for understanding the results. Finally, reporting all the results in a correct and compact format for a scientific report is also very cumbersome. For example, the official APA format asks to remove the zero (0.1 -> .1) only for numbers between -1 and 1, provide the results with the greek letters (e.g. chi, phi, eta), or write the p of p-value in italic (*p*)!

Doing all of that can takes days of hard work when analysing a big dataset from my own experience. Therefore, I decided to code a function running a complete analysis instantly and displaying the results in a format enabling to simply copy and paste it in a report. 

Currently, the function is fully operational for any type/number of factors (X), when the dependent variable (Y) is **qualitative**. I am working very hard on the quantitative part of the function, and it should, therefore, soon be released.

To do that, the function first chooses the right analysis between around 50 different main tests, 20 different measures of associations (e.g. effect size, odds ratio), and 15 different types of post-hoc analysis. I recommend to check the regularly updated decision tree (link might change) to get a better understanding on how the function works, and notably how it decides which test is the most appropriate, using the variable type and many different validity tests (around 15). 

**Actualised link: https://www.xmind.net/m/WdaeLv/**

The decision tree can also be downloaded by clicking on the top right corner button and then "Download". It can then be opened in Xmind (prior download of the software necessary) to get access to the content of the boxes, and also see it in the text preview (linear tree showing the content of the boxes). Each box is annotated with the associated piece of code because I understand that checking the 3000 lines of code (not all in Github yet) can be very difficult, even if I made an effort on the appearance of the text (well-spaced out and structured) and on the number of comments!

![Capture](https://user-images.githubusercontent.com/15387266/84702633-3dad3b00-af57-11ea-87a0-65544e7bc439.PNG)

Using the auto_stats function is very simple since the user only has to fill with the name of the variable(s) the *y* argument, and possibly the *x1* (and) *x2* arguments, while indicating the name of the dataframe in which they can be found in the *data* argument. If one (*"first"* or *"second"*) of both (*"both"*) factors (X) are repeated measures, the user must indicate it in the *paired* argument, the default being that the variables are independent (*"none"*). If the variables are paired, the user must provide the *id* argument with the name of one column of the dataframe containing the ID of the subjects. In case of an error of an entry error, I implemented multiple warning messages (displaying in red) covering most of the possible errors (I hope), to explain in details what went wrong and rapidly rectify it. 
```r
### Actual default values ###
auto_stats = function(data, y, x1 = NULL, x2 = NULL, paired = "none", id = NULL, digits = 3, apa = FALSE)
```
After a few micro-seconds/seconds (depending on the analysis and the size of the dataframe), the *auto_stat* function will display the result in a convenient form, separated in multiple subsections varying with the type of analysis (e.g. **TABLE** only for qualitative Y). The **MESSAGE(S)** section is of major importance since I implemented many different kinds of advice, general information, notes on the tests etc, for more transparency. 

Finally, the *digits* (number of decimals) and *apa* arguments, as their names indicate, serve to control the output. The latter only controls the **MAIN TEST(S)** and **MEASURE(S) OF ASSOCIATION** sections, since they are usually the only ones reported. For the main tests, the function will generate an "APA code" below, which just have to be pasted in an R Notebook (instructions below) to generate an appropriate result. Since the greek letters and mathematical symbols are characters, they can be copied/pasted in any kind of word processor. It will also round the measures of association results to 3 decimals, and remove the zero in front of numbers between -1 & 1. 

**Example: χ²(2) = 2.667, *p* = .264, *φ* = 0.577**
<br>
<br>
<br>
## Testing the *auto_stats* function: *test_auto_stats*

While I was developing the code, I had to create datasets, to test every single branch of the tree/code (21 in total). I stored them in the *test_auto_stats* function, designed to quickly provide the tests of interest. Besides being very useful when debugging the function, it can also serve to show how to use the function correctly, and notably in which form must be the dataframe to perform an analysis (e.g. long format instead of wide).

This function is also intuitive to use because you just have to precise the type of dependent variable, the number of factors and if they are repeated measures or not.
```r
test_auto_stats(y = "qualitative", nb_x = 1, paired = "first")

Tests:
          
          auto_stats(`QUALITATIVE: One paired X - Y & X1 with 2 levels`,
                      y = "Y", x1 = "X1", id = "ID", paired = "first")
          
          auto_stats(`QUALITATIVE: One paired X - Y with more than 2 levels & X1 with 2 levels`,
                      y = "Y", x1 = "X1", id = "ID", paired = "first")
          
          auto_stats(`QUALITATIVE: One paired X - Y with 2 levels & X1 with more than 2 levels`,
                      y = "Y", x1 = "X1", id = "ID", paired = "first")
          
          auto_stats(`QUALITATIVE: One paired X - Y & X1 with more than 2 levels`,
                      y = "Y", x1 = "X1", id = "ID", paired = "first")
```
As you can see, multiple commands running each part of the code in the section desired are displayed. Meanwhile, the different datasets have been loaded in the Global Environment and can be seen on the top right corner in RStudio. Their names aim at being the most explicit possible about which branch of the tree/code they were designed to test.

Then, just copy the name and run it to display the dataframe, or copy the entire command to get the final result:
```r
auto_stats(`QUALITATIVE: One paired X - Y with 2 levels & X1 with more than 2 levels`,
           y = "Y", x1 = "X1", id = "ID", paired = "first")
```
```        
------------------------------ TABLE -----------------------------
 
             During No Yes
After Before              
No    No             1   0
      Yes            0   1
Yes   No             0   1
      Yes            0   0
 
---------------------- ASSUMPTION(S) TESTING ---------------------
 
Variable type: Qualitative dependent variable with two levels (Y)
One factor (X1) with more than two levels of repeated measures.
 
-------------------------- MAIN TEST(S) --------------------------
 
Asymptotic General Symmetry Test: Z = 1, p-value = 0.577 (ns)
 
Cochran's Q test: Q = 1, df = 2, p-value = 0.607 (ns)
 
------------------------ POST-HOC ANALYSIS -----------------------
 
Pairwise two-sample permutation symmetry tests (fdr adjustment method)
 
           Comparison p-value
1  After - Before = 0   1.000
2  After - During = 0   0.476
3 Before - During = 0   0.476
 
------------------------------------------------------------------
```

Here is the results obtained when activating the *apa* argument for a different dataframe:
```r
auto_stats(`QUALITATIVE: One paired X - Y with more than 2 levels & X1 with 2 levels`,
            y = "Y", x1 = "X1", id = "ID", paired = "first", apa = TRUE)
```
```
------------------------------ TABLE -----------------------------
 
       Before
After   Maybe No Yes
  Maybe     0  1   0
  No        0  1   0
  Yes       1  1   0
 
---------------------- ASSUMPTION(S) TESTING ---------------------
 
Variable type: Qualitative dependent variable with more than two levels (Y)
One factor (X1) with two levels of repeated measures.
 
-------------------------- MAIN TEST(S) --------------------------
 
Asymptotic General Symmetry Test: Z = 1.414, p-value = 0.334 (ns)
 
APA code -> Asymptotic General Symmetry Test: Z = 1.414, *p* = .334
 
Stuart-Maxwell Marginal Homogeneity Test: X-squared = 2.667, df = 2, p-value = 0.264 (ns)
 
APA code -> Stuart-Maxwell Marginal Homogeneity Test: &chi;^2^(2) = 2.667, *p* = .264
 
(Copy it + Paste it in an R Notebook + Click on Preview and save it + Click on Knit + Copy it anywhere else)
 
-------------------- MEASURE(S) OF ASSOCIATION -------------------
 
Cohen's g = 0.5 (Large effect)
 
APA code -> Cohen's *g* = .5
 
Odds ratio = 1 (Negligible effect)
 
APA code -> OR = 1
 
             Comparison Odds-ratio Cohen's G
1   Maybe/Maybe : No/No        Inf       0.5
2 Maybe/Maybe : Yes/Yes        Inf       0.5
3       No/No : Yes/Yes        Inf       0.5
 
------------------------ POST-HOC ANALYSIS -----------------------
 

 
             Comparison p-value
1   Maybe/Maybe : No/No       1
2 Maybe/Maybe : Yes/Yes       1
3       No/No : Yes/Yes       1
 
------------------------------------------------------------------
```

We can also try the auto_stats function with two factors:
```r
test_auto_stats(y = "qualitative", nb_x = 2, paired = "none")   ### 7 different tests shown, I chose the 3rd one.

auto_stats(`QUALITATIVE: Two independent X - Y & X1 with 2 levels - Woolf test = NOT OK - Multiplicative model`,
           y = "Y", x1 = "X1", x2 = "X2", paired = "none")
```
```
------------------------------ TABLE -----------------------------
 
           X2 Group 1 Group 2 Group 3 Group 4 Group 5 Group 6
X1     Y                                                     
Female No          19       8     391     244     299     317
       Yes         89      17     202     131      94      24
Male   No         313     207     205     279     138     351
       Yes        512     353     120     138      53      22
 
---------------------- ASSUMPTION(S) TESTING ---------------------
 
Variable type: Qualitative dependent variable (Y) with two levels
Two independent factors (X).
 
Woolf's Test: X-squared = 17.902, df = 5, p-value = 0.003 -> Not satisfied (p-value <= 0.05)
Note: Heterogeneity of odds ratios across levels of X2 (strata).
 
Likelihood Ratio Test on Logistic Regression model:
Deviance = 20.204, p-value = 0.001 -> Multiplicative model (significant difference)
Note: Deviance between additive (Y ~ X1 + X2) & multiplicative model (Y ~ X1 * X2)
 
-------------------------- MAIN TEST(S) --------------------------
 
Likelihood Ratio Test on Logistic Regression model (Type III tests):
X1 -> X-squared = 19.054, df = 1, p-value = 0 (***)
X2 -> X-squared = 268.851, df = 5, p-value = 0 (***)
X1:X2 -> X-squared = 20.204, df = 5, p-value = 0.001 (**)
 
-------------------- MEASURE(S) OF ASSOCIATION -------------------
 
        Odds ratio            Magnitude
Group 1      0.349          (Less odds)
Group 2      0.803          (Less odds)
Group 3      1.133  (Negligible effect)
Group 4      0.921          (Less odds)
Group 5      1.222  (Negligible effect)
Group 6      0.828          (Less odds)
 
------------------------ POST-HOC ANALYSIS -----------------------
 
Groupewise Exact Fisher's Tests (fdr adjustment method):
 
    Group p-value
1 Group 1  0.0001
2 Group 2  0.6770
3 Group 3  0.6770
4 Group 4  0.6770
5 Group 5  0.6770
6 Group 6  0.6770
 
------------------------------------------------------------------
```

Finally, we can increase the number of digits. Notice the message at the end of the analysis:
```r
auto_stats(`QUALITATIVE: Two independent X - Y & X1 with 2 levels - Woolf test = OK`,
           y = "Y", x1 = "X1", x2 = "X2", paired = "none", digits = 5)
```
```
------------------------------ TABLE -----------------------------
 
            X2 Adult female Adult male Teenage boy Teenage girl
X1      Y                                                      
Group 1 No                1          1           1            1
        Yes               0          0           0            0
Group 2 No                1          0           0            1
        Yes               0          1           1            0
 
---------------------- ASSUMPTION(S) TESTING ---------------------
 
Variable type: Qualitative dependent variable (Y) with two levels
Two independent factors (X) and X1 has two levels.
 
Woolf's Test: X-squared = 0.90521, df = 3, p-value = 0.82417 -> Satisfied (p-value > 0.05)
Note: Homogeneity of odds ratios across levels of X2 (strata).
 
-------------------------- MAIN TEST(S) --------------------------
 
Exact conditional test of independence: S = 4, p-value = 0.5 (ns)
 
-------------------- MEASURE(S) OF ASSOCIATION -------------------
 
Odds ratio (conditional Maximum Likelihood Estimate) = Inf 95% CI [1.2066, Inf]
 
             Odds ratio            Magnitude
Adult female          1  (Negligible effect)
Adult male            9       (Large effect)
Teenage boy           9       (Large effect)
Teenage girl          1  (Negligible effect)
 
------------------------ POST-HOC ANALYSIS -----------------------
 
Groupewise Exact Fisher's Tests (fdr adjustment method):
 
         Group p-value
1 Adult female       1
2   Adult male       1
3  Teenage boy       1
4 Teenage girl       1
 
-------------------------- MESSAGE(S) ---------------------------
 
This test has been designed to know if there is an association between the 1st factor (X1)
and the dependant variable (Y). As the 2nd factor (X2) is used for adjustment, you should
change the position in the formula if this is the variable of main interest.
 
------------------------------------------------------------------
```
<br>
Finally, here is a table presenting the limits used to give a gross approximation of the magnitude (rule of thumb) of a measure of association (interpretation varying with the field of study):
| Measure of association | Other | No effect  | Negligible effect | Small effect | Medium effect | Large effect |
  |:------------------|:-------------|:----------------|:----------------------|:----------------------|:----------------------|:----------------------|
  | Cohen's *h* | NA | *h* = 0 | *h* < 0.2 | 0.2 ≥ *h* < 0.5 |0.5 ≥ *h* < 0.8 | *h* ≥ 0.8 |
| Phi coefficient | NA | *φ* = 0 | *φ* < 0.1 | 0.1 ≥ *φ* < 0.3 | 0.3 ≥ *φ* < 0.5 | *φ* ≥ 0.5 |
| Cramer's V | df = 2 | V = 0 | V < 0.07 | 0.07 ≥ V < 0.2 | 0.2 ≥ V < 0.35 | V ≥ 0.35 |
  | Cramer's V | df = 3 | V = 0 | V < 0.06 | 0.06 ≥ V < 0.17 | 0.17 ≥ V < 0.29 | V ≥ 0.29 |
| Cramer's V | df = 4 | V = 0 | V < 0.05 | 0.05 ≥ V < 0.15 | 0.15 ≥ V < 0.25 | V ≥ 0.25 |
  | Cramer's V | df ≥ 5 (Warning if df > 5) | V = 0 | V < 0.05 | 0.05 ≥ V < 0.13 | 0.13 ≥ V < 0.22 | V ≥ 0.22 |
| Cohen's *g* | NA | *g* = 0 | *g* < 0.05 | 0.05 ≥ *g* < 0.15 | 0.15 ≥ *g* < 0.25 | *g* ≥ 0.25 |
  | Odds ratio | Qualitative Y - Paired - N x N contigency tables | OR = 1 (OR < 1 -> "Less Odds") | OR < 1.22 | 1.22 ≥ OR < 1.86 | 1.86 ≥ OR < 3 | OR ≥ 3 |
  | Odds ratio | Qualitative Y - Independent | OR = 1 (OR < 1 -> "Less Odds") | OR < 1.55 | 1.55 ≥ OR < 2.8 | 2.8 ≥ OR < 5 | OR ≥ 5 |
<br>
<br>
<br>

## Automation of the comparison of non-linear models using different estimators & automatic plotting of the non-linear models of interest: *compare_nlm*

In my experience, finding the best models and the best parameters for nonlinear models on R can be very cumbersome. Indeed, few Self-Starters (i.e. functions allowing to automatically determine "good" starting parameters for each models), are implemented in Base R. It takes a long time to find a package offering the appropriate functions, even for simple models such as the power laws. When your not an expert in mathematics, even thinking to a model who could fit well enough the data can be complicated. 

The groping search method of good starting parameters is even more tedious since even with values close to the nearest tenth of the optimal values, the *nls* function (BaseR) sometimes don't converge after 50,000 iterations! Besides, once the right parameters have been found, it is still very long to calculate the model quality estimators and the confidence intervals around the coefficients. Finally, making a correct graph representing the model can be very hard in R.

To ease such procedure, I coded the function *compare_nlm* (see the commented code in french in the "R" file of this page) to compare many different models, compute their estimators, and finally plot them, for a set of data consisting in 1 quantitative dependent variable (Y) and 1 quantitative factor.
```r
### Default values ###
compare_nlm = function(formula, data, digits = 2, arrange = c("AIC", "RMSE", "BIC"), increase = T, plot_model = NA, package = F)
```
<br>

After 4 to 5s of computation (on a usual laptop), the function display a table containing the best coefficients (1 to 7) of 112 non-linear and 2 linear models. The self-starters for all those models are found in the packages "drc" and "aomisc", and *compare_nlm* uses internally the function *drm* (package "drc") to run the computation. 

Sometimes, the convergence of the self-starters might fail (model not appropriate for the data), but the function will jump to the other model and print a message with the ID of the model (found at the end of the table).
```r
compare_nlm(mpg ~ wt, mtcars)
```
```
Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
  non-finite finite-difference value [2]
[1] "Convergence failed - Self-Starter ID : 59"
Error in optim(startVec, opfct, hessian = TRUE, method = optMethod, control = list(maxit = maxIt,  : 
  non-finite finite-difference value [1]
[1] "Convergence failed - Self-Starter ID : 95"
```
<br>

The user can choose to order the table (*arrange* argument), according to the AIC, and/or the BIC, and/or the RMSE and/or the number of coefficients. 

The order of the estimators reflects their importance for the user because, the dataframe is first ordered using the 1st estimator, and then, for equal values of the 1st estimator, it uses the 2nd estimator etc. 

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
```
```
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

The confidence interval around the coefficients of the model of interest can easily be calculated using the *confint.drc* function in the "drc" package:
```r
model = drm(mpg ~ wt, data = mtcars, fct = UCRS.5c())
confint(model, level = 0.95)
```
```
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
<br>

The name of the package containing the function appear in front of the name of the model, if the argument *package* is switched to *TRUE*.
```r
compare_nlm(mpg ~ wt, mtcars, package = T)
```
```
# Part of the dataset obtained #
7   16          drc::EXD.2         157.55  161.95  2.58    2 coeffs   d: 49.66     e: 3.41                                               
8   99    aomisc::DRC.expoDecay    157.55  161.95  2.58    2 coeffs  init: 49.66   k: 0.29
```
<br>

The names of the 21 remaining model (in package "aomisc") are displayed below, and more informations on them can be obtained following this [link1](https://www.statforbiology.com/2020/stat_nls_usefulfunctions/#exponential-function) and this [link2](https://www.statforbiology.com/nonlinearregression/usefulequations):
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

In case of error, different messages will be printed to explain to the user what was the problem (e.g. error of syntax of the estimators, error in the number of digits or the ID of the models).
<br>
<br>
<br>

## Calculation of non-linear models' confidence interval & predictions: *ci_nlm*

The package "drc" already provides methods to calculate the confidence interval around the non-linear models created with its Self-Starters (*predict.drc*). However, this feature is not available for the model created from "aomisc" self-starters, though some of them are often the best models (e.g. linear, exponential, logarithmic).

The function *ci_nlm* was coded to generalize CI calculations for all models used in *compare_nlm*, but also to allows the user for plotting the predictions.
```r
### Default values ###
ci_nlm = function(formula, fct, data, method = "delta", level = 0.05, nb_boot = 200, expand_x = NA, keep_cols = NULL)
```
The *predict.drc* function computes the confidence interval using the Delta method, which is very fast. However, as its calculation is very complicated, I decided to use the equivalent Bootstrap method (Bertail, Boizot & Combris, 2003) for the moment, which is much more simple to code but is slower to compute. 

A warning message appears if the "aomisc" self-starter is used with the Delta method, advising to shift *method" to *"boot"*. Beforehand, it is really easy to check the source of the function, by switching *package = FALSE* to *package = TRUE* in the *compare_nlm* function (see above).
<br>

The latency vary with the Self-Starter used, with the number of coefficients and with the number of iterations. However, it is situated between 2 and 4s for "aomisc" Self-starters, with the default value of 200 iterations. Since the *drm* function calculate many supplementary things for "drc" Self-starters, the duration of computation is generally much longer.
```r
### DRC self-starter with the Delta method -> immediate result ###
result = ci_nlm(mpg~ wt, fct = gaussian(), data = mtcars)
head(result)
```
```
  Predictions Lower_CI Upper_CI
1    22.39356 20.79063 23.99648
2    20.68509 19.36893 22.00126
3    24.94405 22.85991 27.02818
4    18.81245 17.61229 20.01262
5    17.75349 16.52585 18.98113
6    17.66496 16.43295 18.89697
```
```r
### DRC self-starter with the Bootstrap method (default = 200 iterations) -> 10s ###
result = ci_nlm(mpg~ wt, fct = gaussian(), data = mtcars, method = "boot")
head(result)
```
```
Predictions Lower_CI Upper_CI
1    22.39356 21.30726 23.78245
2    20.68509 19.65370 21.97412
3    24.94405 23.84398 26.62455
4    18.81245 17.87121 19.96312
5    17.75349 16.74902 18.81757
6    17.66496 16.64668 18.71966
```
```r
### AOMISC self-starter with the Bootstrap method (default = 200 iterations) -> 2s ###
result = ci_nlm(mpg~ wt, fct = DRC.expoDecay(), data = mtcars, method = "boot") 
```
```
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
```
```
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
![Hnet com-image](https://user-images.githubusercontent.com/15387266/84265694-831ad400-ab23-11ea-83b0-9dd4551d1c60.jpg)
```r
### Plotting with the "ggplot" package ###
ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = 3) +
  geom_ribbon(data = result, aes(x = wt, ymin = Lower_CI, ymax = Upper_CI), alpha = 0.5, fill = "grey") +
  geom_line(data = result, aes(x = wt, y = Predictions), size = 1.2) +
  theme_minimal()
```
![regrg2](https://user-images.githubusercontent.com/15387266/84253071-c10dfd00-ab0f-11ea-9fa1-e88bcecff71b.png)
<br>

Another argument allows the user to do predictions using the model. The user only has to fill the argument *expand_x* with two new limits of X incorporation the previous. 

To ensure the accuracy of the confidence intervals, the data related to the previous X is kept as it is. The new X is calculated to fill the new bounds, while being spaced by the same mean step as the previous values of X, so as not to artificially inflate the "density" of the measurements, which would reduce the confidence interval around the predictions.
```r
ci_nlm(mpg~ wt, fct = gaussian(), data = mtcars, expand_x = c(-6,10))

ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = 3) +
  geom_ribbon(data = result, aes(x = wt, ymin = Lower_CI, ymax = Upper_CI), alpha = 0.5, fill = "grey") +
  geom_line(data = result, aes(x = wt, y = Predictions), size = 1.2) +
  theme_minimal()
```
![regrg3](https://user-images.githubusercontent.com/15387266/84255455-ef410c00-ab12-11ea-8da4-41b7d2c744cb.png)
<br>

If *keep_cols* is activated, the selected columns are repeated as much as necessary to fit the number of rows of the new dataframe. This is very useful when facetting in ggplot for example. 
```r
result = ci_nlm(mpg~ wt, fct = gaussian(), data = mtcars, keep_cols = c("mpg","wt"), expand_x = c(-6,10))
tail(result)
```
```
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
```
```
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
![regrg5](https://user-images.githubusercontent.com/15387266/84266961-a21a6580-ab25-11ea-9acf-a7b9374328ae.png)
However, this is not the case when the X are expanded. The CI generated by the Bootstrap method follows the fitted line while they widen approaching the limits with Delta method. This could be due to the difference of method or to complementary calculations implemented in the "drc" package, but I still did not found the answer to my question and I would be glad if someone can share it with me. I hope this will be answered when I will implement the Delta method for "aomisc" functions. 

I believe the Delta method is more accurate in this case since it seems logic that the CI increase while going away for the measured points. A message pop if the bootstrap method is used with the *expand* argument to warn the user about that.
![regrg4](https://user-images.githubusercontent.com/15387266/84261992-156ba980-ab1d-11ea-86b3-8c6d698b2cf5.png)
