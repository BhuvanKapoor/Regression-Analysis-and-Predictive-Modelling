# Correlation Analysis 
#### Bhuvan Kapoor 

``` r
#Experiment 1
#Correlation Analysis using scatter diagram, Karl Pearson's correlation 
#coefficient and drawing inferences for a given dataset

iris
```

    ##     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
    ## 1            5.1         3.5          1.4         0.2     setosa
    ## 2            4.9         3.0          1.4         0.2     setosa
    ## 3            4.7         3.2          1.3         0.2     setosa
    ## 4            4.6         3.1          1.5         0.2     setosa
    ## 5            5.0         3.6          1.4         0.2     setosa
    ## 6            5.4         3.9          1.7         0.4     setosa
    ## 7            4.6         3.4          1.4         0.3     setosa
    ## 8            5.0         3.4          1.5         0.2     setosa
    ## 9            4.4         2.9          1.4         0.2     setosa
    ## 10           4.9         3.1          1.5         0.1     setosa
    ## 11           5.4         3.7          1.5         0.2     setosa
    ## 12           4.8         3.4          1.6         0.2     setosa
    ## 13           4.8         3.0          1.4         0.1     setosa
    ## 14           4.3         3.0          1.1         0.1     setosa
    ## 15           5.8         4.0          1.2         0.2     setosa
    ## 16           5.7         4.4          1.5         0.4     setosa
    ## 17           5.4         3.9          1.3         0.4     setosa
    ## 18           5.1         3.5          1.4         0.3     setosa
    ## 19           5.7         3.8          1.7         0.3     setosa
    ## 20           5.1         3.8          1.5         0.3     setosa
    ## 21           5.4         3.4          1.7         0.2     setosa
    ## 22           5.1         3.7          1.5         0.4     setosa
    ## 23           4.6         3.6          1.0         0.2     setosa
    ## 24           5.1         3.3          1.7         0.5     setosa
    ## 25           4.8         3.4          1.9         0.2     setosa
    ## 26           5.0         3.0          1.6         0.2     setosa
    ## 27           5.0         3.4          1.6         0.4     setosa
    ## 28           5.2         3.5          1.5         0.2     setosa
    ## 29           5.2         3.4          1.4         0.2     setosa
    ## 30           4.7         3.2          1.6         0.2     setosa
    ## 31           4.8         3.1          1.6         0.2     setosa
    ## 32           5.4         3.4          1.5         0.4     setosa
    ## 33           5.2         4.1          1.5         0.1     setosa
    ## 34           5.5         4.2          1.4         0.2     setosa
    ## 35           4.9         3.1          1.5         0.2     setosa
    ## 36           5.0         3.2          1.2         0.2     setosa
    ## 37           5.5         3.5          1.3         0.2     setosa
    ## 38           4.9         3.6          1.4         0.1     setosa
    ## 39           4.4         3.0          1.3         0.2     setosa
    ## 40           5.1         3.4          1.5         0.2     setosa
    ## 41           5.0         3.5          1.3         0.3     setosa
    ## 42           4.5         2.3          1.3         0.3     setosa
    ## 43           4.4         3.2          1.3         0.2     setosa
    ## 44           5.0         3.5          1.6         0.6     setosa
    ## 45           5.1         3.8          1.9         0.4     setosa
    ## 46           4.8         3.0          1.4         0.3     setosa
    ## 47           5.1         3.8          1.6         0.2     setosa
    ## 48           4.6         3.2          1.4         0.2     setosa
    ## 49           5.3         3.7          1.5         0.2     setosa
    ## 50           5.0         3.3          1.4         0.2     setosa
    ## 51           7.0         3.2          4.7         1.4 versicolor
    ## 52           6.4         3.2          4.5         1.5 versicolor
    ## 53           6.9         3.1          4.9         1.5 versicolor
    ## 54           5.5         2.3          4.0         1.3 versicolor
    ## 55           6.5         2.8          4.6         1.5 versicolor
    ## 56           5.7         2.8          4.5         1.3 versicolor
    ## 57           6.3         3.3          4.7         1.6 versicolor
    ## 58           4.9         2.4          3.3         1.0 versicolor
    ## 59           6.6         2.9          4.6         1.3 versicolor
    ## 60           5.2         2.7          3.9         1.4 versicolor
    ## 61           5.0         2.0          3.5         1.0 versicolor
    ## 62           5.9         3.0          4.2         1.5 versicolor
    ## 63           6.0         2.2          4.0         1.0 versicolor
    ## 64           6.1         2.9          4.7         1.4 versicolor
    ## 65           5.6         2.9          3.6         1.3 versicolor
    ## 66           6.7         3.1          4.4         1.4 versicolor
    ## 67           5.6         3.0          4.5         1.5 versicolor
    ## 68           5.8         2.7          4.1         1.0 versicolor
    ## 69           6.2         2.2          4.5         1.5 versicolor
    ## 70           5.6         2.5          3.9         1.1 versicolor
    ## 71           5.9         3.2          4.8         1.8 versicolor
    ## 72           6.1         2.8          4.0         1.3 versicolor
    ## 73           6.3         2.5          4.9         1.5 versicolor
    ## 74           6.1         2.8          4.7         1.2 versicolor
    ## 75           6.4         2.9          4.3         1.3 versicolor
    ## 76           6.6         3.0          4.4         1.4 versicolor
    ## 77           6.8         2.8          4.8         1.4 versicolor
    ## 78           6.7         3.0          5.0         1.7 versicolor
    ## 79           6.0         2.9          4.5         1.5 versicolor
    ## 80           5.7         2.6          3.5         1.0 versicolor
    ## 81           5.5         2.4          3.8         1.1 versicolor
    ## 82           5.5         2.4          3.7         1.0 versicolor
    ## 83           5.8         2.7          3.9         1.2 versicolor
    ## 84           6.0         2.7          5.1         1.6 versicolor
    ## 85           5.4         3.0          4.5         1.5 versicolor
    ## 86           6.0         3.4          4.5         1.6 versicolor
    ## 87           6.7         3.1          4.7         1.5 versicolor
    ## 88           6.3         2.3          4.4         1.3 versicolor
    ## 89           5.6         3.0          4.1         1.3 versicolor
    ## 90           5.5         2.5          4.0         1.3 versicolor
    ## 91           5.5         2.6          4.4         1.2 versicolor
    ## 92           6.1         3.0          4.6         1.4 versicolor
    ## 93           5.8         2.6          4.0         1.2 versicolor
    ## 94           5.0         2.3          3.3         1.0 versicolor
    ## 95           5.6         2.7          4.2         1.3 versicolor
    ## 96           5.7         3.0          4.2         1.2 versicolor
    ## 97           5.7         2.9          4.2         1.3 versicolor
    ## 98           6.2         2.9          4.3         1.3 versicolor
    ## 99           5.1         2.5          3.0         1.1 versicolor
    ## 100          5.7         2.8          4.1         1.3 versicolor
    ## 101          6.3         3.3          6.0         2.5  virginica
    ## 102          5.8         2.7          5.1         1.9  virginica
    ## 103          7.1         3.0          5.9         2.1  virginica
    ## 104          6.3         2.9          5.6         1.8  virginica
    ## 105          6.5         3.0          5.8         2.2  virginica
    ## 106          7.6         3.0          6.6         2.1  virginica
    ## 107          4.9         2.5          4.5         1.7  virginica
    ## 108          7.3         2.9          6.3         1.8  virginica
    ## 109          6.7         2.5          5.8         1.8  virginica
    ## 110          7.2         3.6          6.1         2.5  virginica
    ## 111          6.5         3.2          5.1         2.0  virginica
    ## 112          6.4         2.7          5.3         1.9  virginica
    ## 113          6.8         3.0          5.5         2.1  virginica
    ## 114          5.7         2.5          5.0         2.0  virginica
    ## 115          5.8         2.8          5.1         2.4  virginica
    ## 116          6.4         3.2          5.3         2.3  virginica
    ## 117          6.5         3.0          5.5         1.8  virginica
    ## 118          7.7         3.8          6.7         2.2  virginica
    ## 119          7.7         2.6          6.9         2.3  virginica
    ## 120          6.0         2.2          5.0         1.5  virginica
    ## 121          6.9         3.2          5.7         2.3  virginica
    ## 122          5.6         2.8          4.9         2.0  virginica
    ## 123          7.7         2.8          6.7         2.0  virginica
    ## 124          6.3         2.7          4.9         1.8  virginica
    ## 125          6.7         3.3          5.7         2.1  virginica
    ## 126          7.2         3.2          6.0         1.8  virginica
    ## 127          6.2         2.8          4.8         1.8  virginica
    ## 128          6.1         3.0          4.9         1.8  virginica
    ## 129          6.4         2.8          5.6         2.1  virginica
    ## 130          7.2         3.0          5.8         1.6  virginica
    ## 131          7.4         2.8          6.1         1.9  virginica
    ## 132          7.9         3.8          6.4         2.0  virginica
    ## 133          6.4         2.8          5.6         2.2  virginica
    ## 134          6.3         2.8          5.1         1.5  virginica
    ## 135          6.1         2.6          5.6         1.4  virginica
    ## 136          7.7         3.0          6.1         2.3  virginica
    ## 137          6.3         3.4          5.6         2.4  virginica
    ## 138          6.4         3.1          5.5         1.8  virginica
    ## 139          6.0         3.0          4.8         1.8  virginica
    ## 140          6.9         3.1          5.4         2.1  virginica
    ## 141          6.7         3.1          5.6         2.4  virginica
    ## 142          6.9         3.1          5.1         2.3  virginica
    ## 143          5.8         2.7          5.1         1.9  virginica
    ## 144          6.8         3.2          5.9         2.3  virginica
    ## 145          6.7         3.3          5.7         2.5  virginica
    ## 146          6.7         3.0          5.2         2.3  virginica
    ## 147          6.3         2.5          5.0         1.9  virginica
    ## 148          6.5         3.0          5.2         2.0  virginica
    ## 149          6.2         3.4          5.4         2.3  virginica
    ## 150          5.9         3.0          5.1         1.8  virginica

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
ggplot(iris,aes(x=Sepal.Width,y=Sepal.Length))+facet_wrap(~Species,scales="free_x")+
  geom_point()+geom_smooth(formula = y~x,method="lm",se = FALSE)+
  labs(x="Sepal Width",y="Sepal Length",title="Sepal Width vs. Sepal Length",subtitle="Grouped by species")
```

![Iris Scatter Plot](./Images/iris_scatter_plot.png)

``` r
#Correlation Coefficient
my_data=mtcars
my_data
```

    ##                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    ## Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
    ## Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
    ## Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    ## Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
    ## Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
    ## Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
    ## Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
    ## Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
    ## Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
    ## Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
    ## Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
    ## Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    ## Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    ## Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
    ## Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
    ## Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
    ## AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
    ## Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
    ## Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
    ## Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
    ## Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    ## Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    ## Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    ## Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    ## Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
    ## Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2

``` r
cor_1=cor.test(my_data$wt,my_data$mpg,method = "pearson")
cor_1
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  my_data$wt and my_data$mpg
    ## t = -9.559, df = 30, p-value = 1.294e-10
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.9338264 -0.7440872
    ## sample estimates:
    ##        cor 
    ## -0.8676594

``` r
#H0: rho=0 vs H1: rho !=0
#As p-value is less than 0.05, we reject Ho
#There is a significant linear relationship between the variables
#r=-0.86765

#Correlation Matrix
my_data=mtcars[,c(1,3,4,5)]
#my_data
head(my_data,5)
```

    ##                    mpg disp  hp drat
    ## Mazda RX4         21.0  160 110 3.90
    ## Mazda RX4 Wag     21.0  160 110 3.90
    ## Datsun 710        22.8  108  93 3.85
    ## Hornet 4 Drive    21.4  258 110 3.08
    ## Hornet Sportabout 18.7  360 175 3.15

``` r
cor_2=round(cor(my_data,method="pearson"),4)
cor_2
```

    ##          mpg    disp      hp    drat
    ## mpg   1.0000 -0.8476 -0.7762  0.6812
    ## disp -0.8476  1.0000  0.7909 -0.7102
    ## hp   -0.7762  0.7909  1.0000 -0.4488
    ## drat  0.6812 -0.7102 -0.4488  1.0000

``` r
#Visualizing correlation matrix
#Correlogram
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
my_data=mtcars[,c(1,3,4,5)]
cor_3=corrplot(cor_2,type="upper",order="hclust",tl.col="black",tl.srt = 45)
```

![MT Cars Corr Plot](./Images/mtcars_corrplot.png)

``` r
#Performance Matrix
library(PerformanceAnalytics)
```

    ## Loading required package: xts
    ## Loading required package: zoo
    ## 
    ## Attaching package: 'zoo'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric
    ## 
    ## 
    ## ######################### Warning from 'xts' package ##########################
    ## #                                                                             #
    ## # The dplyr lag() function breaks how base R's lag() function is supposed to  #
    ## # work, which breaks lag(my_xts). Calls to lag(my_xts) that you type or       #
    ## # source() into this session won't work correctly.                            #
    ## #                                                                             #
    ## # Use stats::lag() to make sure you're not using dplyr::lag(), or you can add #
    ## # conflictRules('dplyr', exclude = 'lag') to your .Rprofile to stop           #
    ## # dplyr from breaking base R's lag() function.                                #
    ## #                                                                             #
    ## # Code in packages is not affected. It's protected by R's namespace mechanism #
    ## # Set `options(xts.warn_dplyr_breaks_lag = FALSE)` to suppress this warning.  #
    ## #                                                                             #
    ## ###############################################################################
    ## 
    ## Attaching package: 'xts'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last
    ## 
    ## 
    ## Attaching package: 'PerformanceAnalytics'
    ## 
    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend

``` r
cor_4=chart.Correlation(my_data,histogram=T,method="pearson")
```

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

![MT Cars Correlation](./Images/mtcars_correlation.png)

``` r
#Symbolic Representation
cor_5=cor(mtcars[,c(1,3,4,5)])
cor_5
```

    ##             mpg       disp         hp       drat
    ## mpg   1.0000000 -0.8475514 -0.7761684  0.6811719
    ## disp -0.8475514  1.0000000  0.7909486 -0.7102139
    ## hp   -0.7761684  0.7909486  1.0000000 -0.4487591
    ## drat  0.6811719 -0.7102139 -0.4487591  1.0000000

``` r
symnum(cor_5,abbr.colnames = F)
```

    ##      mpg disp hp drat
    ## mpg  1               
    ## disp +   1           
    ## hp   ,   ,    1      
    ## drat ,   ,    .  1   
    ## attr(,"legend")
    ## [1] 0 ' ' 0.3 '.' 0.6 ',' 0.8 '+' 0.9 '*' 0.95 'B' 1

``` r
#GG correlation plot
cor_6=round(cor(my_data),4)
cor_6
```

    ##          mpg    disp      hp    drat
    ## mpg   1.0000 -0.8476 -0.7762  0.6812
    ## disp -0.8476  1.0000  0.7909 -0.7102
    ## hp   -0.7762  0.7909  1.0000 -0.4488
    ## drat  0.6812 -0.7102 -0.4488  1.0000

``` r
library(ggcorrplot)
ggcorrplot(cor_6)
```

![MT Cars Heatmap - 1](./Images/mtcars_heatmap_1.png)

``` r
ggcorrplot(cor_6,hc.order = T,type="lower",lab = T)
```

![MT Cars Heatmap - 2](./Images/mtcars_heatmap_2.png)

``` r
#-----------------------------------#
#             Exercise              #
#-----------------------------------#

# Use the inbuilt data set "trees" and performance the following:
trees_data <- trees
trees_data
```

    ##    Girth Height Volume
    ## 1    8.3     70   10.3
    ## 2    8.6     65   10.3
    ## 3    8.8     63   10.2
    ## 4   10.5     72   16.4
    ## 5   10.7     81   18.8
    ## 6   10.8     83   19.7
    ## 7   11.0     66   15.6
    ## 8   11.0     75   18.2
    ## 9   11.1     80   22.6
    ## 10  11.2     75   19.9
    ## 11  11.3     79   24.2
    ## 12  11.4     76   21.0
    ## 13  11.4     76   21.4
    ## 14  11.7     69   21.3
    ## 15  12.0     75   19.1
    ## 16  12.9     74   22.2
    ## 17  12.9     85   33.8
    ## 18  13.3     86   27.4
    ## 19  13.7     71   25.7
    ## 20  13.8     64   24.9
    ## 21  14.0     78   34.5
    ## 22  14.2     80   31.7
    ## 23  14.5     74   36.3
    ## 24  16.0     72   38.3
    ## 25  16.3     77   42.6
    ## 26  17.3     81   55.4
    ## 27  17.5     82   55.7
    ## 28  17.9     80   58.3
    ## 29  18.0     80   51.5
    ## 30  18.0     80   51.0
    ## 31  20.6     87   77.0

``` r
# libraries used
library(ggplot2)                # plot()
library(corrplot)               # corrplot()
library(PerformanceAnalytics)   # chart.Correlation()
library(ggcorrplot)             # ggcorrplot()

# (i) Obtain the scatter diagram for the attributes Girth and Height. Interpret your findings.
scatter.smooth(trees_data$Girth, trees_data$Height)
```

![Trees Scatter Smooth](./Images/trees_scatter_smooth.png)

``` r
plot(trees_data$Girth, trees_data$Height, main = "Trees Height vs Trees Girth", xlab = "Girth", ylab = "Height")
abline(lm(trees_data$Height~trees_data$Girth))
```

![Trees Scatter Abline](./Images/trees_scatter_abline.png)

``` r
summary(lm(trees_data$Height~trees_data$Girth))
```

    ## 
    ## Call:
    ## lm(formula = trees_data$Height ~ trees_data$Girth)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -12.5816  -2.7686   0.3163   2.4728   9.9456 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       62.0313     4.3833  14.152 1.49e-14 ***
    ## trees_data$Girth   1.0544     0.3222   3.272  0.00276 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.538 on 29 degrees of freedom
    ## Multiple R-squared:  0.2697, Adjusted R-squared:  0.2445 
    ## F-statistic: 10.71 on 1 and 29 DF,  p-value: 0.002758

``` r
# (ii) Obtain the correlation between the attributes Height and Volume. Interpret your output.
cor_height_vol <- cor.test(trees_data$Height, trees_data$Volume, method = "pearson")
cor_height_vol
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  trees_data$Height and trees_data$Volume
    ## t = 4.0205, df = 29, p-value = 0.0003784
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.3095235 0.7859756
    ## sample estimates:
    ##       cor 
    ## 0.5982497

``` r
# (iii) Obtain the correlation matrix for the given dataset by restricting to 5 rows and perform various plots for the correlation matrix.
partial_trees_data <- head(trees_data,5)
partial_trees_data
```

    ##   Girth Height Volume
    ## 1   8.3     70   10.3
    ## 2   8.6     65   10.3
    ## 3   8.8     63   10.2
    ## 4  10.5     72   16.4
    ## 5  10.7     81   18.8

``` r
corr_matrix <- round(cor(partial_trees_data),4)
corr_matrix
```

    ##         Girth Height Volume
    ## Girth  1.0000 0.7757 0.9759
    ## Height 0.7757 1.0000 0.8940
    ## Volume 0.9759 0.8940 1.0000

``` r
corrplot(corr_matrix,type="lower",order="hclust",tl.col="black",tl.srt = 45)
```

![Trees Corr Plot](./Images/trees_corrplot.png)

``` r
chart.Correlation(corr_matrix,histogram=T,method="pearson")
```

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

    ## Warning in par(usr): argument 1 does not name a graphical parameter

![Trees Correlation](./Images/trees_correlation.png)

``` r
ggcorrplot(corr_matrix,hc.order = T,type="lower",lab = T)
```

![Trees Heatmap](./Images/trees_heatmap.png)

