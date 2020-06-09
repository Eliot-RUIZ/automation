# Package "automation" - Eliot RUIZ

The goal of this package is to provide functions automating usual time-consuming tasks in R.


## Automation of usual statistical tests

The function auto_stats aims at choosing the appropriate statistical test for the data provided by the user. 

The data could consist in an independant variable (Y) explained by 0 to 2 factors (X).

After checking each assumptions, the function compute the main test and all usually associated calculations (e.g. effect size, post-hoc tests).

It then returns all the results in APA format to ease insertion in a text document, except for the post-hoc analysis results. The output is separated in 5 sections: 
  - Assumptions
  - Main test
  - Effect size and related 
  - Post-Hoc analysis
  - Messages

The last section is of major importance since many different messages have been implemented in the function for transparency of the analysis (e.g. advices, problems with the data).

Currently, this function is still in the making but will be functionnal in the next few days.

For more transparency due to the length of the function (approximately 1500 lines of code), I created on Xmind the decisional tree the algorithm follows, for the statistical part (main part). 

This decision tree can be viewed online following this link: http://www.xmind.net/m/MikJA4

The decision tree can also be downloaded by clicking on the top right corner button and then "Download". The decision tree could then be opened in Xmind (prior download of the software necessary) to get access to the content of the boxes (R code), and also see it in the text preview (as it is organized in the function).


## Automation of the comparison and plotting of non-linear models

The function compare_nlm is fully functionnal and serves to choose the most appropriate non-linear model to a set of data consisting in 1 dependent variable (Y) and 1 factor. Both variables must be quantitative.

After 4 to 5s of computation (on a usual laptop), the function a table containing the coefficients (1 to 7) of 112 non-linear and 2 linear models (intercept = 0 or not).

The user can choose to order the table ("arrange" argument), according to the AIC, the BIC, the RMSE or the number of coefficients 

|    |Rank  |ID   |    Function      |RMSE   | AIC    | BIC    |Nb_coeffs  |  Coeff_1    | Coeff_2    |   Coeff_3     | Coeff_4    | Coeff_5    | Coeff_6   |Coeff_7   |
|:---|:-----|:----|:-----------------|:------|:-------|:-------|:----------|:------------|:-----------|:--------------|:-----------|:-----------|:----------|:---------|
|1   |===== |==== |================= |====== |======= |======= |========== |============ |=========== |============== |=========== |=========== |========== |========= |
|71  |1     |71   |UCRS.5c           |2.31   |156.46  |165.26  |5 coeffs   |b: -274.58   |c: 93.75    |d: 30.07       |e: 2.26     |f: 158.28   |           |          |
|77  |2     |77   |uml4c             |2.31   |156.46  |165.26  |5 coeffs   |b: -274.58   |c: 93.75    |d: 30.07       |e: 2.26     |f: 158.28   |           |          |
|70  |3     |70   |UCRS.5b           |2.32   |156.55  |165.34  |5 coeffs   |b: -249.34   |c: 69.84    |d: 30.07       |e: 2.26     |f: 89.31    |           |          |
|76  |4     |76   |uml4b             |2.32   |156.55  |165.34  |5 coeffs   |b: -249.34   |c: 69.84    |d: 30.07       |e: 2.26     |f: 89.31    |           |          |
|69  |5     |69   |UCRS.5a           |2.34   |157.11  |165.91  |5 coeffs   |b: -201.18   |c: 66.46    |d: 30.07       |e: 2.26     |f: 65.33    |           |          |
|75  |6     |75   |uml4a             |2.34   |157.11  |165.91  |5 coeffs   |b: -201.18   |c: 66.46    |d: 30.07       |e: 2.26     |f: 65.33    |           |          |
|23  |7     |23   |gaussian          |2.41   |158.98  |167.77  |5 coeffs   |b: 1.08      |c: -5.71    |d: 38.23       |e: 1.75     |f: 0.51     |           |          |
|34  |8     |34   |lgaussian         |2.41   |159.2   |167.99  |5 coeffs   |b: 1.85      |c: -44.18   |d: 36.3        |e: 1.74     |f: 0.65     |           |          |
|88  |9     |88   |W2x.4             |2.43   |157.74  |165.07  |4 coeffs   |c: 30.87     |d: 9.57     |e: 1.65        |t0: 1.83    |            |           |          |
|44  |10    |44   |LL2.5             |2.44   |159.93  |168.73  |5 coeffs   |b: 33.35     |c: 2.05     |d: 31.13       |e: 0.62     |f: 0.03     |           |          |
|46  |11    |46   |llogistic2        |2.44   |159.93  |168.73  |5 coeffs   |b: 33.35     |c: 2.05     |d: 31.13       |e: 0.62     |f: 0.03     |           |          |
|33  |12    |33   |l5                |2.44   |159.98  |168.77  |5 coeffs   |b: 28.47     |c: 1.6      |d: 31.11       |e: 1.86     |f: 0.03     |           |          |
|39  |13    |39   |LL.5              |2.44   |159.98  |168.77  |5 coeffs   |b: 28.47     |c: 1.6      |d: 31.11       |e: 1.86     |f: 0.03     |           |          |
|45  |14    |45   |llogistic         |2.44   |159.98  |168.77  |5 coeffs   |b: 28.47     |c: 1.6      |d: 31.11       |e: 1.86     |f: 0.03     |           |          |
|15  |15    |15   |CRS.6             |2.44   |161.94  |172.2   |6 coeffs   |b: 1.53      |c: 5.17     |d: 87.88       |e: 1.11     |f: -394.28  |g: -2.73   |          |
|28  |16    |28   |L.5               |2.45   |160.18  |168.97  |5 coeffs   |b: 18.12     |c: 9.57     |d: 31.04       |e: 1.82     |f: 0.03     |           |          |
|52  |17    |52   |logistic          |2.45   |160.18  |168.97  |5 coeffs   |b: 18.12     |c: 9.57     |d: 31.04       |e: 1.82     |f: 0.03     |           |          |
|5   |18    |5    |BC.5              |2.47   |160.62  |169.42  |5 coeffs   |b: 3.34      |c: 8.48     |d: -163.13     |e: 1.1      |f: 170.56   |           |          |
|7   |19    |7    |bcl4              |2.47   |160.62  |169.42  |5 coeffs   |b: 3.34      |c: 8.48     |d: -163.13     |e: 1.1      |f: 170.56   |           |          |
|8   |20    |8    |braincousens      |2.47   |160.62  |169.42  |5 coeffs   |b: 3.34      |c: 8.48     |d: -163.13     |e: 1.1      |f: 170.56   |           |          |
|12  |21    |12   |CRS.5a            |2.47   |160.64  |169.43  |5 coeffs   |b: 2.57      |c: 8.65     |d: -294.16     |e: 0.9      |f: 792.15   |           |          |
|56  |22    |56   |ml4a              |2.47   |160.64  |169.43  |5 coeffs   |b: 2.57      |c: 8.65     |d: -294.16     |e: 0.9      |f: 792.15   |           |          |
|14  |23    |14   |CRS.5c            |2.47   |160.66  |169.45  |5 coeffs   |b: 2.81      |c: 8.71     |d: -826.62     |e: 1.06     |f: 2259.05  |           |          |
|58  |24    |58   |ml4c              |2.47   |160.66  |169.45  |5 coeffs   |b: 2.81      |c: 8.71     |d: -826.62     |e: 1.06     |f: 2259.05  |           |          |
|13  |25    |13   |CRS.5b            |2.47   |160.69  |169.49  |5 coeffs   |b: 2.81      |c: 8.93     |d: -343.56     |e: 1.13     |f: 955.3    |           |          |
|57  |26    |57   |ml4b              |2.47   |160.69  |169.49  |5 coeffs   |b: 2.81      |c: 8.93     |d: -343.56     |e: 1.13     |f: 955.3    |           |          |
|86  |27    |86   |W2.4              |2.49   |159.23  |166.55  |4 coeffs   |b: -2.5      |c: 8.71     |d: 32.1        |e: 2.56     |            |           |          |
|92  |28    |92   |weibull2          |2.49   |159.23  |166.55  |4 coeffs   |b: -2.5      |c: 8.71     |d: 32.1        |e: 2.56     |            |           |          |
|105 |29    |105  |DRC.lorentz.4     |2.49   |159.31  |166.64  |4 coeffs   |b: 0.37      |c: 8.99     |d: 31.76       |e: 1.32     |            |           |          |
|3   |30    |3    |baro5             |2.5    |161.46  |170.25  |5 coeffs   |b1: 3.96     |b2: 2.26    |c: 7.79        |d: 34.14    |e: 2.89     |           |          |
|50  |31    |50   |LN.4              |2.51   |159.6   |166.92  |4 coeffs   |b: -2.14     |c: 10.27    |d: 34.37       |e: 2.71     |            |           |          |
|51  |32    |51   |lnormal           |2.51   |159.6   |166.92  |4 coeffs   |b: -2.14     |c: 10.27    |d: 34.37       |e: 2.71     |            |           |          |
|32  |33    |32   |l4                |2.51   |159.61  |166.94  |4 coeffs   |b: 3.14      |c: 9.3      |d: 35.58       |e: 2.69     |            |           |          |
|38  |34    |38   |LL.4              |2.51   |159.61  |166.94  |4 coeffs   |b: 3.14      |c: 9.3      |d: 35.58       |e: 2.69     |            |           |          |
|43  |35    |43   |LL2.4             |2.51   |159.61  |166.94  |4 coeffs   |b: 3.14      |c: 9.3      |d: 35.58       |e: 0.99     |            |           |          |
|66  |36    |66   |UCRS.4a           |2.51   |159.64  |166.97  |4 coeffs   |b: -3.02     |d: 36.03    |e: 2.8         |f: -10.39   |            |           |          |
|68  |37    |68   |UCRS.4c           |2.51   |159.64  |166.97  |4 coeffs   |b: -3.01     |d: 35.95    |e: 2.77        |f: -16.79   |            |           |          |
|72  |38    |72   |uml3a             |2.51   |159.64  |166.97  |4 coeffs   |b: -3.02     |d: 36.03    |e: 2.8         |f: -10.39   |            |           |          |
|74  |39    |74   |uml3c             |2.51   |159.64  |166.97  |4 coeffs   |b: -3.01     |d: 35.95    |e: 2.77        |f: -16.79   |            |           |          |
|67  |40    |67   |UCRS.4b           |2.51   |159.65  |166.98  |4 coeffs   |b: -2.98     |d: 36.06    |e: 2.8         |f: -13.13   |            |           |          |
|73  |41    |73   |uml3b             |2.51   |159.65  |166.98  |4 coeffs   |b: -2.98     |d: 36.06    |e: 2.8         |f: -13.13   |            |           |          |
|112 |42    |112  |E.4               |2.51   |159.69  |167.02  |4 coeffs   |b: -0.9      |c: 10.64    |d: 35.87       |e: 2.19     |            |           |          |
|4   |43    |4    |BC.4              |2.51   |159.73  |167.06  |4 coeffs   |b: 2.27      |d: -69.35   |e: 0.89        |f: 136.96   |            |           |          |
|6   |44    |6    |bcl3              |2.51   |159.73  |167.06  |4 coeffs   |b: 2.27      |d: -69.35   |e: 0.89        |f: 136.96   |            |           |          |
|27  |45    |27   |L.4               |2.51   |159.82  |167.15  |4 coeffs   |b: 0.95      |c: 10.62    |d: 45.64       |e: 1.99     |            |           |          |
|81  |46    |81   |W1.4              |2.51   |159.83  |167.16  |4 coeffs   |b: 1.92      |c: 11.01    |d: 39.38       |e: 2.84     |            |           |          |
|90  |47    |90   |w4                |2.51   |159.83  |167.16  |4 coeffs   |b: 1.92      |c: 11.01    |d: 39.38       |e: 2.84     |            |           |          |
|91  |48    |91   |weibull1          |2.51   |159.83  |167.16  |4 coeffs   |b: 1.92      |c: 11.01    |d: 39.38       |e: 2.84     |            |           |          |
|65  |49    |65   |twophase          |2.51   |165.61  |177.34  |7 coeffs   |b1: 8.03     |c1: 9.3     |d1: 15.8       |e1: 0.26    |b2: 3.14    |d2: 26.28  |e2: 2.69  |
|107 |50    |107  |DRC.poly2         |2.52   |158.05  |163.91  |3 coeffs   |a: 49.93     |b: -13.38   |c: 1.17        |            |            |           |          |
|97  |51    |97   |DRC.bragg.4       |2.52   |159.84  |167.17  |4 coeffs   |b: 0.12      |c: 11.18    |d: 38.76       |e: -0.02    |            |           |          |
|21  |52    |21   |G.4               |2.52   |159.95  |167.28  |4 coeffs   |b: 0.36      |c: 11.34    |d: 78.69       |e: 1.07     |            |           |          |
|24  |53    |24   |gompertz          |2.52   |159.95  |167.28  |4 coeffs   |b: 0.36      |c: 11.34    |d: 78.69       |e: 1.07     |            |           |          |
|11  |54    |11   |CRS.4c            |2.52   |159.97  |167.3   |4 coeffs   |b: 1.63      |d: -315.3   |e: 0.85        |f: 1055.36  |            |           |          |
|55  |55    |55   |ml3c              |2.52   |159.97  |167.3   |4 coeffs   |b: 1.63      |d: -315.3   |e: 0.85        |f: 1055.36  |            |           |          |
|10  |56    |10   |CRS.4b            |2.52   |160.02  |167.35  |4 coeffs   |b: 1.59      |d: -105.55  |e: 0.88        |f: 478.45   |            |           |          |
|54  |57    |54   |ml3b              |2.52   |160.02  |167.35  |4 coeffs   |b: 1.59      |d: -105.55  |e: 0.88        |f: 478.45   |            |           |          |
|84  |58    |84   |W2.3              |2.53   |158.17  |164.03  |3 coeffs   |b: -1.38     |d: 37.03    |e: 2.54        |            |            |           |          |
|9   |59    |9    |CRS.4a            |2.53   |160.09  |167.42  |4 coeffs   |b: 1.48      |d: 5.79     |e: 1.01        |f: 162.56   |            |           |          |
|53  |60    |53   |ml3a              |2.53   |160.09  |167.42  |4 coeffs   |b: 1.48      |d: 5.79     |e: 1.01        |f: 162.56   |            |           |          |
|17  |61    |17   |EXD.3             |2.54   |158.39  |164.25  |3 coeffs   |c: 6.78      |d: 57.33    |e: 2.27        |            |            |           |          |
|94  |62    |94   |DRC.asymReg       |2.54   |158.39  |164.25  |3 coeffs   |init: 57.34  |m: 0.44     |plateau: 6.78  |            |            |           |          |
|109 |63    |109  |DRC.SSasymp       |2.54   |158.39  |164.25  |3 coeffs   |Asym: 6.78   |R0: 57.34   |lrc: -0.82     |            |            |           |          |
|30  |64    |30   |l3                |2.54   |158.47  |164.33  |3 coeffs   |b: 1.51      |d: 48.49    |e: 2.42        |            |            |           |          |
|36  |65    |36   |LL.3              |2.54   |158.47  |164.33  |3 coeffs   |b: 1.51      |d: 48.49    |e: 2.42        |            |            |           |          |
|41  |66    |41   |LL2.3             |2.54   |158.47  |164.33  |3 coeffs   |b: 1.51      |d: 48.5     |e: 0.88        |            |            |           |          |
|104 |67    |104  |DRC.lorentz.3     |2.55   |158.61  |164.48  |3 coeffs   |b: 0.13      |d: 102.25   |e: -2.5        |            |            |           |          |
|48  |68    |48   |LN.3              |2.55   |158.63  |164.49  |3 coeffs   |b: -0.9      |d: 50.59    |e: 2.28        |            |            |           |          |
|79  |69    |79   |W1.3              |2.55   |158.79  |164.65  |3 coeffs   |b: 0.62      |d: 80.9     |e: 1.78        |            |            |           |          |
|89  |70    |89   |w3                |2.55   |158.79  |164.65  |3 coeffs   |b: 0.62      |d: 80.9     |e: 1.78        |            |            |           |          |
|60  |71    |60   |MM.3              |2.55   |158.8   |164.66  |3 coeffs   |c: 68.15     |d: -6.63    |e: 1.69        |            |            |           |          |
|16  |72    |16   |EXD.2             |2.58   |157.55  |161.95  |2 coeffs   |d: 49.66     |e: 3.41     |               |            |            |           |          |
|99  |73    |99   |DRC.expoDecay     |2.58   |157.55  |161.95  |2 coeffs   |init: 49.66  |k: 0.29     |               |            |            |           |          |
|100 |74    |100  |DRC.expoGrowth    |2.58   |157.55  |161.95  |2 coeffs   |init: 49.66  |k: -0.29    |               |            |            |           |          |
|103 |75    |103  |DRC.logCurve      |2.58   |157.59  |161.98  |2 coeffs   |a: 39.26     |b: -17.09   |               |            |            |           |          |
|111 |76    |111  |E.3               |2.59   |159.62  |165.48  |3 coeffs   |b: -0.3      |d: 527.38   |e: -7.75       |            |            |           |          |
|26  |77    |26   |L.3               |2.59   |159.84  |165.7   |3 coeffs   |b: 0.32      |d: 287.15   |e: -5.06       |            |            |           |          |
|108 |78    |108  |DRC.powerCurve    |2.63   |158.69  |163.08  |2 coeffs   |a: 47.09     |b: -0.79    |               |            |            |           |          |
|96  |79    |96   |DRC.bragg.3       |2.63   |160.82  |166.68  |3 coeffs   |b: 0.01      |d: 138.62   |e: -9.91       |            |            |           |          |
|19  |80    |19   |G.3               |2.67   |161.71  |167.57  |3 coeffs   |b: 0.12      |d: 263.61   |e: -5.08       |            |            |           |          |
|101 |81    |101  |DRC.linear        |2.95   |166.03  |170.43  |2 coeffs   |a: 37.29     |b: -5.34    |               |            |            |           |          |
|110 |82    |110  |DRC.YL            |3.36   |174.43  |178.83  |2 coeffs   |i: -13.65    |A: 13.08    |               |            |            |           |          |
|93  |83    |93   |weibull2x         |5.3    |209.6   |218.39  |5 coeffs   |b: -5.11     |c: 20.94    |d: 11.83       |e: 4.42     |t0: 4.47    |           |          |
|114 |84    |1    |AR.2              |5.93   |210.76  |215.15  |2 coeffs   |d: 20.09     |e: 0.03     |               |            |            |           |          |
|106 |85    |106  |DRC.negExp        |5.93   |210.78  |215.18  |2 coeffs   |a: 20.09     |c: 4.62     |               |            |            |           |          |
|2   |86    |2    |AR.3              |5.93   |212.76  |218.62  |3 coeffs   |c: 16.19     |d: 20.09    |e: 0.11        |            |            |           |          |
|20  |87    |20   |G.3u              |5.93   |212.76  |218.62  |3 coeffs   |b: 1.36      |c: 20.09    |e: -1.58       |            |            |           |          |
|31  |88    |31   |l3u               |5.93   |212.76  |218.62  |3 coeffs   |b: 8.97      |c: 20.09    |e: 0.3         |            |            |           |          |
|37  |89    |37   |LL.3u             |5.93   |212.76  |218.62  |3 coeffs   |b: 8.97      |c: 20.09    |e: 0.3         |            |            |           |          |
|42  |90    |42   |LL2.3u            |5.93   |212.76  |218.62  |3 coeffs   |b: 7.15      |c: 20.09    |e: -1.74       |            |            |           |          |
|49  |91    |49   |LN.3u             |5.93   |212.76  |218.62  |3 coeffs   |b: -5.01     |c: 20.09    |e: 0.54        |            |            |           |          |
|63  |92    |63   |NEC.3             |5.93   |212.76  |218.62  |3 coeffs   |b: 5.84      |d: 20.09    |e: 11.31       |            |            |           |          |
|80  |93    |80   |W1.3u             |5.93   |212.76  |218.62  |3 coeffs   |b: 3.36      |c: 20.09    |e: 0.59        |            |            |           |          |
|85  |94    |85   |W2.3u             |5.93   |212.76  |218.62  |3 coeffs   |b: -3.3      |c: 20.09    |e: 0.02        |            |            |           |          |
|87  |95    |87   |W2x.3             |5.93   |212.76  |218.62  |3 coeffs   |d: 20.09     |e: 0.12     |t0: -1.22      |            |            |           |          |
|98  |96    |98   |DRC.cousens85     |5.93   |212.76  |218.62  |3 coeffs   |YWF: 20.09   |i: 0.49     |a: 0           |            |            |           |          |
|22  |97    |22   |gammadr           |5.93   |214.76  |222.08  |4 coeffs   |b: -1.82     |c: 20.09    |d: 33.92       |e: 2.81     |            |           |          |
|64  |98    |64   |NEC.4             |5.93   |214.76  |222.08  |4 coeffs   |b: 5.84      |c: 10.38    |d: 20.09       |e: 11.31    |            |           |          |
|61  |99    |61   |multi2            |5.93   |216.76  |225.55  |5 coeffs   |b1: 1.61     |b2: 55.57   |b3: 46.38      |c: 17.74    |d: 20.09    |           |          |
|102 |100   |102  |DRC.linearOrigin  |11.09  |248.81  |251.74  |1 coeffs   |b: 5.29      |            |               |            |            |           |          |
|18  |101   |18   |G.2               |19.99  |288.51  |292.91  |2 coeffs   |b: 17.3      |e: 2218.92  |               |            |            |           |          |
|29  |102   |29   |l2                |19.99  |288.51  |292.91  |2 coeffs   |b: 6.04      |e: 34.12    |               |            |            |           |          |
|35  |103   |35   |LL.2              |19.99  |288.51  |292.91  |2 coeffs   |b: 6.04      |e: 34.12    |               |            |            |           |          |
|40  |104   |40   |LL2.2             |19.99  |288.51  |292.91  |2 coeffs   |b: 4.34      |e: 11.41    |               |            |            |           |          |
|47  |105   |47   |LN.2              |19.99  |288.51  |292.91  |2 coeffs   |b: -3.83     |e: 25.59    |               |            |            |           |          |
|62  |106   |62   |NEC.2             |19.99  |288.51  |292.91  |2 coeffs   |b: 5.84      |e: 11.31    |               |            |            |           |          |
|78  |107   |78   |W1.2              |19.99  |288.51  |292.91  |2 coeffs   |b: 363.87    |e: 3763.32  |               |            |            |           |          |
|82  |108   |82   |w2                |19.99  |288.51  |292.91  |2 coeffs   |b: 363.87    |e: 3763.32  |               |            |            |           |          |
|83  |109   |83   |W2.2              |19.99  |288.51  |292.91  |2 coeffs   |b: -136.57   |e: 19.77    |               |            |            |           |          |
|113 |110   |113  |L.2               |19.99  |288.51  |292.91  |2 coeffs   |b: 1.34      |e: 140.28   |               |            |            |           |          |
|25  |111   |25   |gompertzd         |20.95  |291.5   |295.9   |2 coeffs   |a: 33.9      |b: 1        |               |            |            |           |          |
|59  |112   |59   |MM.2              |NA     |NA      |NA      |NA         |NA           |            |               |            |            |           |          |
|95  |113   |95   |DRC.beta          |NA     |NA      |NA      |NA         |NA           |            |               |            |            |           |          |
















  
