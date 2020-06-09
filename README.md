# Package "automation" - Eliot RUIZ

The goal of this package is to provide functions automating usual time-consuming tasks in R.


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

The user can choose to order the table (*arrange* argument), according to the AIC, and/or the BIC, and/or the RMSE and/or the number of coefficients; their order reflecting the importance the used gives to each estimator. The models with the lowest estimators (i.e. best models) are then placed on the top of the dataframe (i.e. Rank 1, Rank 2 etc). However, the dataframe could also be ordered by descending order if the argument *increase* is shifted to *FALSE*.

Here is an example obtained with the *mtcars* package (*mpg ~ wt*):
```r
compare_nlm(mpg ~ wt, mtcars)
```

|Rank  |ID   |    Function      |RMSE   | AIC    | BIC    |Nb_coeffs  |  Coeff_1    | Coeff_2    |   Coeff_3     | Coeff_4    | Coeff_5    | Coeff_6   |Coeff_7   |
|:-----|:----|:-----------------|:------|:-------|:-------|:----------|:------------|:-----------|:--------------|:-----------|:-----------|:----------|:---------|
|1     |71   |UCRS.5c           |2.31   |156.46  |165.26  |5 coeffs   |b: -274.58   |c: 93.75    |d: 30.07       |e: 2.26     |f: 158.28   |           |          |
|2     |77   |uml4c             |2.31   |156.46  |165.26  |5 coeffs   |b: -274.58   |c: 93.75    |d: 30.07       |e: 2.26     |f: 158.28   |           |          |
|3     |70   |UCRS.5b           |2.32   |156.55  |165.34  |5 coeffs   |b: -249.34   |c: 69.84    |d: 30.07       |e: 2.26     |f: 89.31    |           |          |
|4     |76   |uml4b             |2.32   |156.55  |165.34  |5 coeffs   |b: -249.34   |c: 69.84    |d: 30.07       |e: 2.26     |f: 89.31    |           |          |
|5     |69   |UCRS.5a           |2.34   |157.11  |165.91  |5 coeffs   |b: -201.18   |c: 66.46    |d: 30.07       |e: 2.26     |f: 65.33    |           |          |
|6     |75   |uml4a             |2.34   |157.11  |165.91  |5 coeffs   |b: -201.18   |c: 66.46    |d: 30.07       |e: 2.26     |f: 65.33    |           |          |
|7     |23   |gaussian          |2.41   |158.98  |167.77  |5 coeffs   |b: 1.08      |c: -5.71    |d: 38.23       |e: 1.75     |f: 0.51     |           |          |
|8     |34   |lgaussian         |2.41   |159.2   |167.99  |5 coeffs   |b: 1.85      |c: -44.18   |d: 36.3        |e: 1.74     |f: 0.65     |           |          |
|9     |88   |W2x.4             |2.43   |157.74  |165.07  |4 coeffs   |c: 30.87     |d: 9.57     |e: 1.65        |t0: 1.83    |            |           |          |
|10    |44   |LL2.5             |2.44   |159.93  |168.73  |5 coeffs   |b: 33.35     |c: 2.05     |d: 31.13       |e: 0.62     |f: 0.03     |           |          |
|11    |46   |llogistic2        |2.44   |159.93  |168.73  |5 coeffs   |b: 33.35     |c: 2.05     |d: 31.13       |e: 0.62     |f: 0.03     |           |          |
|12    |33   |l5                |2.44   |159.98  |168.77  |5 coeffs   |b: 28.47     |c: 1.6      |d: 31.11       |e: 1.86     |f: 0.03     |           |          |
|13    |39   |LL.5              |2.44   |159.98  |168.77  |5 coeffs   |b: 28.47     |c: 1.6      |d: 31.11       |e: 1.86     |f: 0.03     |           |          |
|14    |45   |llogistic         |2.44   |159.98  |168.77  |5 coeffs   |b: 28.47     |c: 1.6      |d: 31.11       |e: 1.86     |f: 0.03     |           |          |
|15    |15   |CRS.6             |2.44   |161.94  |172.2   |6 coeffs   |b: 1.53      |c: 5.17     |d: 87.88       |e: 1.11     |f: -394.28  |g: -2.73   |          |
|16    |28   |L.5               |2.45   |160.18  |168.97  |5 coeffs   |b: 18.12     |c: 9.57     |d: 31.04       |e: 1.82     |f: 0.03     |           |          |
|+99 other lines  |   |          |   |     |     |  |      |     |        |     |     |    |   |











  
