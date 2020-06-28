
# Install packages if first time

``` r
install.packages('dplyr')
install.packages('readr')
install.packages('tidyr')
install.packages('stringr')
install.packages('forcats')
install.packages('purr')
install.packages("ggpubr")
install.packages('car')
install.packages('gdata')
```

# Read in Packages needed

``` r
library(dplyr) # manipulate dataframes
library(readr) # read/write dataframes
library(tidyr) # reshaping dataframes
library(stringr) # string manipulation
library(forcats) # factor manipulation
library(purrr) # iteration (instead of loops)
library(ggpubr) # for Correlation
library(ggplot2) # for Plots
library ('car')
library ('gdata')
```

Unzip the ANES data if needed

``` r
source("unzip-anes-data.R")
```

    ## 
    ## Attaching package: 'zip'

    ## The following objects are masked from 'package:utils':
    ## 
    ##     unzip, zip

# Import the data- ANES Survey Timeseries

``` r
library(haven)
anes_timeseries_cdf_stata13 <- read_dta("data-raw/anes_timeseries_cdf_stata13.dta")
```

# Filter to 2004

``` r
# Updated1 <-anes_timeseries_cdf_stata13[!(anes_timeseries_cdf_stata13$VCF0004<"2004"),]
Updated1 <- filter(anes_timeseries_cdf_stata13, VCF0004 >= 2004)
```

# select variables

``` r
Condensed_weights  = subset(Updated1, select = c(VCF0004, VCF0102, VCF0103, VCF0104,VCF0105a, VCF0105b,VCF0106,VCF0107, VCF0700,VCF0201,VCF0202, VCF0301 ,VCF0303, VCF0314 ,VCF0315 ,VCF0323 ,VCF0501,VCF9008,VCF9010 ,VCF9011,VCF9012, VCF9201,VCF9202 ,VCF9203,VCF9204,VCF9206,VCF0350 ,VCF0351 ,VCF0352,VCF0353 , VCF0354 ,VCF0355,VCF0356 ,VCF0357 ,VCF0358 ,VCF0363 ,VCF0364,VCF0365 ,VCF0366 ,VCF0367, VCF0368 ,VCF0369 ,VCF0370,VCF0371 ,VCF0372,VCF0373 ,VCF0412 ,VCF0414 ,VCF0906 ,VCF0907,VCF0908 ,VCF0909 ,VCF0450,VCF0451,VCF9217,VCF9218,  VCF9220,VCF9221,VCF0875 ,VCF0875a ,VCF0875b ,VCF9019,VCF9020 ,VCF9052,VCF9222,VCF0803,VCF9240, VCF0228 ,VCF0231 ,VCF0601,VCF0602, VCF0603, VCF0604 ,VCF0608 ,VCF0656,VCF9250,VCF9251,VCF9252,VCF9253,VCF9254,VCF9256,VCF9257,VCF0308,VCF0309,
  VCF0310,VCF0311,VCF0312,VCF0313,VCF0700,VCF0714,VCF0729 ,VCF0730,VCF0731,VCF0732,VCF0733,VCF0743,VCF0932,VCF0933,VCF0934,VCF0935,VCF0936,VCF0941, VCF0942, VCF0943, VCF0944, VCF0945 , VCF0946 ,VCF0947, VCF0948, VCF0949,VCF9064,VCF0717,VCF0723,VCF0723a,VCF0746,VCF0747 ,VCF9021 ,VCF9030,VCF9030a,
VCF0701 ,VCF0702 , VCF0703 , VCF0704 , VCF0704a , VCF0705, VCF0706, VCF0707 , VCF0711, VCF0708, VCF0713 , VCF0712, VCF0748, VCF0749,VCF0750,VCF9022,VCF9023,VCF0675,VCF0724,VCF0725,VCF0726,VCF0727,VCF0728,VCF0744,VCF0745,VCF9032,VCF9033,VCF9034,VCF9035, VCF9266, VCF0102, VCF0104, VCF9222, VCF0880, VCF9229, VCF0606, VCF9030a, VCF0322, VCF9206, VCF0105b, VCF0009z, VCF0900b,
VCF0870,
VCF9225,
VCF0607,
VCF0611))
```

# Filter out the columns that are not relevant- have all missings (questions from older surveys (two ways)

``` r
Updated_data <- Condensed_weights [ , colSums(is.na(Condensed_weights)) <nrow(Condensed_weights)]

data_new3 <- Condensed_weights [ , colSums(is.na(Condensed_weights)) <nrow(Condensed_weights)]
```

# attach to frame- checking for missings + descriptive stats

``` r
# attach(Condensed_weights)
# summary (VCF9021)
# summary (VCF9030a)
# summary (VCF0728)
# summary (VCF0745)
# summary (VCF9034)
# summary (VCF0724)
# summary (VCF0301)
# summary (VCF9206)
# summary (VCF9222)
# summary (VCF0870)
# summary (VCF0880)
# summary (VCF9225)
# summary (VCF9229)
# summary (VCF0606)
# summary (VCF0607)
# summary (VCF0611)
# summary (VCF0613)
# summary (VCF0617)
# summary (VCF0632)
# summary (VCF0650)
# summary (VCF0501)
# summary (VCF0731)
# summary (VCF0733)
# summary (VCF0310)
# summary (VCF0312)
# summary (VCF0311)
# summary (VCF0653)
# summary (VCF0359)
# summary (VCF0360)
# summary (VCF0361)
# summary (VCF0370)
# summary (VCF0371)
# summary (VCF0372)
# summary (VCF0373)
# summary(VCF0702)

# Many of the columns listed above aren't present in the dataset after the
# subset of columns, so it seems like you should instead use the dataframe
# "Updated1". Also attach can often cause some confusion about what is in your
# environment, so some of the tidyverse alternatives can make things a little
# easier. Here for example you can select this subset of columns and use skim to
# get summary of all the columns at once.

library(skimr) # install.packages("skimr")

Updated1 %>% 
  # The labels confuse this function, so we'll remove them before calling skim()
  zap_labels() %>% 
  select(
    VCF9021,
    VCF9030a,
    VCF0728,
    VCF0745,
    VCF9034,
    VCF0724,
    VCF0301,
    VCF9206,
    VCF9222,
    VCF0870,
    VCF0880,
    VCF9225,
    VCF9229,
    VCF0606,
    VCF0607,
    VCF0611,
    VCF0613,
    VCF0617,
    VCF0632,
    VCF0650,
    VCF0501,
    VCF0731,
    VCF0733,
    VCF0310,
    VCF0312,
    VCF0311,
    VCF0653,
    VCF0359,
    VCF0360,
    VCF0361,
    VCF0370,
    VCF0371,
    VCF0372,
    VCF0373,
    VCF0702
  ) %>% 
  skim()
```

|                                                  |            |
| :----------------------------------------------- | :--------- |
| Name                                             | Piped data |
| Number of rows                                   | 13718      |
| Number of columns                                | 35         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| numeric                                          | 35         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate | mean |   sd | p0 | p25 | p50 | p75 | p100 | hist  |
| :------------- | ---------: | -------------: | ---: | ---: | -: | --: | --: | --: | ---: | :---- |
| VCF9021        |       2463 |           0.82 | 3.13 | 2.00 |  1 |   1 |   5 |   5 |    5 | ▇▁▁▁▇ |
| VCF9030a       |       1443 |           0.89 | 1.60 | 0.49 |  1 |   1 |   2 |   2 |    2 | ▅▁▁▁▇ |
| VCF0728        |      12546 |           0.09 | 3.07 | 1.18 |  1 |   2 |   3 |   4 |    5 | ▂▇▇▆▃ |
| VCF0745        |      12652 |           0.08 | 3.13 | 2.00 |  1 |   1 |   5 |   5 |    5 | ▇▁▁▁▇ |
| VCF9034        |      13718 |           0.00 |  NaN |   NA | NA |  NA |  NA |  NA |   NA |       |
| VCF0724        |       6101 |           0.56 | 1.81 | 0.40 |  1 |   2 |   2 |   2 |    2 | ▂▁▁▁▇ |
| VCF0301        |        104 |           0.99 | 3.62 | 2.11 |  1 |   2 |   3 |   5 |    7 | ▇▃▃▂▅ |
| VCF9206        |        231 |           0.98 | 1.99 | 0.71 |  1 |   1 |   2 |   3 |    3 | ▅▁▇▁▅ |
| VCF9222        |        244 |           0.98 | 1.71 | 0.46 |  1 |   1 |   2 |   2 |    2 | ▃▁▁▁▇ |
| VCF0870        |         74 |           0.99 | 3.34 | 1.61 |  1 |   1 |   3 |   5 |    5 | ▅▁▆▁▇ |
| VCF0880        |         91 |           0.99 | 1.99 | 0.88 |  1 |   1 |   2 |   3 |    3 | ▇▁▅▁▇ |
| VCF9225        |        250 |           0.98 | 2.13 | 0.81 |  1 |   1 |   2 |   3 |    3 | ▆▁▆▁▇ |
| VCF9229        |        395 |           0.97 | 1.99 | 0.68 |  1 |   2 |   2 |   2 |    3 | ▃▁▇▁▃ |
| VCF0606        |        400 |           0.97 | 1.37 | 0.78 |  1 |   1 |   1 |   2 |    9 | ▇▁▁▁▁ |
| VCF0607        |      13718 |           0.00 |  NaN |   NA | NA |  NA |  NA |  NA |   NA |       |
| VCF0611        |      13718 |           0.00 |  NaN |   NA | NA |  NA |  NA |  NA |   NA |       |
| VCF0613        |       4961 |           0.64 | 1.68 | 0.77 |  1 |   1 |   2 |   2 |    9 | ▇▂▁▁▁ |
| VCF0617        |      13718 |           0.00 |  NaN |   NA | NA |  NA |  NA |  NA |   NA |       |
| VCF0632        |      13718 |           0.00 |  NaN |   NA | NA |  NA |  NA |  NA |   NA |       |
| VCF0650        |      13718 |           0.00 |  NaN |   NA | NA |  NA |  NA |  NA |   NA |       |
| VCF0501        |       1441 |           0.89 | 1.91 | 0.88 |  1 |   2 |   2 |   2 |    9 | ▇▁▁▁▁ |
| VCF0731        |       1850 |           0.87 | 2.18 | 1.91 |  1 |   1 |   1 |   5 |    7 | ▇▁▁▂▁ |
| VCF0733        |       3161 |           0.77 | 2.53 | 2.45 |  0 |   0 |   2 |   4 |    7 | ▇▃▃▁▃ |
| VCF0310        |       1148 |           0.92 | 2.32 | 0.71 |  1 |   2 |   2 |   3 |    9 | ▇▇▁▁▁ |
| VCF0312        |      10191 |           0.26 | 1.68 | 0.47 |  1 |   1 |   2 |   2 |    2 | ▃▁▁▁▇ |
| VCF0311        |       4301 |           0.69 | 1.83 | 0.38 |  1 |   2 |   2 |   2 |    2 | ▂▁▁▁▇ |
| VCF0653        |      13718 |           0.00 |  NaN |   NA | NA |  NA |  NA |  NA |   NA |       |
| VCF0359        |         71 |           0.99 | 1.61 | 0.49 |  1 |   1 |   2 |   2 |    2 | ▅▁▁▁▇ |
| VCF0360        |         86 |           0.99 | 1.40 | 0.49 |  1 |   1 |   1 |   2 |    2 | ▇▁▁▁▆ |
| VCF0361        |        112 |           0.99 | 1.46 | 0.50 |  1 |   1 |   1 |   2 |    2 | ▇▁▁▁▇ |
| VCF0370        |        104 |           0.99 | 1.46 | 0.50 |  1 |   1 |   1 |   2 |    2 | ▇▁▁▁▇ |
| VCF0371        |         99 |           0.99 | 1.54 | 0.50 |  1 |   1 |   2 |   2 |    2 | ▇▁▁▁▇ |
| VCF0372        |        116 |           0.99 | 1.57 | 0.50 |  1 |   1 |   2 |   2 |    2 | ▆▁▁▁▇ |
| VCF0373        |        133 |           0.99 | 1.63 | 0.48 |  1 |   1 |   2 |   2 |    2 | ▅▁▁▁▇ |
| VCF0702        |       1367 |           0.90 | 1.79 | 0.41 |  1 |   2 |   2 |   2 |    2 | ▂▁▁▁▇ |

\#Add Labels

``` r
names(Condensed_weights)[names(Condensed_weights) == "VCF0301"] <- "Party Identification of Respondent-7-point Scale"
Condensed_weights
```

    ## # A tibble: 13,718 x 165
    ##    VCF0004 VCF0102 VCF0103 VCF0104 VCF0105a VCF0105b VCF0106 VCF0107 VCF0700
    ##      <dbl> <dbl+l> <dbl+l> <dbl+l> <dbl+lb> <dbl+lb> <dbl+l> <dbl+l> <dbl+l>
    ##  1    2004 4 [4. … 4 [4. … 1 [1. … 1 [1. W… 1 [1. W… 1 [1. … 7 [7. … 1 [1. …
    ##  2    2004 4 [4. … 4 [4. … 1 [1. … 5 [5. H… 3 [3. H… 3 [3. … 3 [3. … 2 [2. …
    ##  3    2004 3 [3. … 3 [3. … 2 [2. … 1 [1. W… 1 [1. W… 1 [1. … 7 [7. … 2 [2. …
    ##  4    2004 6 [6. … 5 [5. … 1 [1. … 2 [2. B… 2 [2. B… 2 [2. … 7 [7. … 2 [2. …
    ##  5    2004 5 [5. … 5 [5. … 2 [2. … 1 [1. W… 1 [1. W… 1 [1. … 7 [7. … 2 [2. …
    ##  6    2004 4 [4. … 4 [4. … 1 [1. … 1 [1. W… 1 [1. W… 1 [1. … 7 [7. … 2 [2. …
    ##  7    2004 4 [4. … 4 [4. … 2 [2. … 1 [1. W… 1 [1. W… 1 [1. … 7 [7. … 2 [2. …
    ##  8    2004 5 [5. … 4 [4. … 2 [2. … 1 [1. W… 1 [1. W… 1 [1. … 7 [7. … 1 [1. …
    ##  9    2004 4 [4. … 4 [4. … 1 [1. … 2 [2. B… 2 [2. B… 2 [2. … 7 [7. … 1 [1. …
    ## 10    2004 4 [4. … 4 [4. … 2 [2. … 1 [1. W… 1 [1. W… 1 [1. … 7 [7. … 2 [2. …
    ## # … with 13,708 more rows, and 156 more variables: VCF0201 <dbl+lbl>,
    ## #   VCF0202 <dbl+lbl>, `Party Identification of Respondent-7-point
    ## #   Scale` <dbl+lbl>, VCF0303 <dbl+lbl>, VCF0314 <dbl+lbl>, VCF0315 <dbl+lbl>,
    ## #   VCF0323 <dbl+lbl>, VCF0501 <dbl+lbl>, VCF9008 <dbl+lbl>, VCF9010 <dbl+lbl>,
    ## #   VCF9011 <dbl+lbl>, VCF9012 <dbl+lbl>, VCF9201 <dbl+lbl>, VCF9202 <dbl+lbl>,
    ## #   VCF9203 <dbl+lbl>, VCF9204 <dbl+lbl>, VCF9206 <dbl+lbl>, VCF0350 <dbl+lbl>,
    ## #   VCF0351 <dbl+lbl>, VCF0352 <dbl+lbl>, VCF0353 <dbl+lbl>, VCF0354 <dbl+lbl>,
    ## #   VCF0355 <dbl+lbl>, VCF0356 <dbl+lbl>, VCF0357 <dbl+lbl>, VCF0358 <dbl+lbl>,
    ## #   VCF0363 <dbl+lbl>, VCF0364 <dbl+lbl>, VCF0365 <dbl+lbl>, VCF0366 <dbl+lbl>,
    ## #   VCF0367 <dbl+lbl>, VCF0368 <dbl+lbl>, VCF0369 <dbl+lbl>, VCF0370 <dbl+lbl>,
    ## #   VCF0371 <dbl+lbl>, VCF0372 <dbl+lbl>, VCF0373 <dbl+lbl>, VCF0412 <dbl+lbl>,
    ## #   VCF0414 <dbl+lbl>, VCF0906 <dbl+lbl>, VCF0907 <dbl+lbl>, VCF0908 <dbl+lbl>,
    ## #   VCF0909 <dbl+lbl>, VCF0450 <dbl+lbl>, VCF0451 <dbl+lbl>, VCF9217 <dbl+lbl>,
    ## #   VCF9218 <dbl+lbl>, VCF9220 <dbl+lbl>, VCF9221 <dbl+lbl>, VCF0875 <dbl+lbl>,
    ## #   VCF0875a <dbl+lbl>, VCF0875b <dbl+lbl>, VCF9019 <dbl+lbl>,
    ## #   VCF9020 <dbl+lbl>, VCF9052 <dbl+lbl>, VCF9222 <dbl+lbl>, VCF0803 <dbl+lbl>,
    ## #   VCF9240 <dbl+lbl>, VCF0228 <dbl+lbl>, VCF0231 <dbl+lbl>, VCF0601 <dbl+lbl>,
    ## #   VCF0602 <dbl+lbl>, VCF0603 <dbl+lbl>, VCF0604 <dbl+lbl>, VCF0608 <dbl+lbl>,
    ## #   VCF0656 <dbl+lbl>, VCF9250 <dbl+lbl>, VCF9251 <dbl+lbl>, VCF9252 <dbl+lbl>,
    ## #   VCF9253 <dbl+lbl>, VCF9254 <dbl+lbl>, VCF9256 <dbl+lbl>, VCF9257 <dbl+lbl>,
    ## #   VCF0308 <dbl+lbl>, VCF0309 <dbl+lbl>, VCF0310 <dbl+lbl>, VCF0311 <dbl+lbl>,
    ## #   VCF0312 <dbl+lbl>, VCF0313 <dbl+lbl>, VCF0700 <dbl+lbl>, VCF0714 <dbl+lbl>,
    ## #   VCF0729 <dbl+lbl>, VCF0730 <dbl+lbl>, VCF0731 <dbl+lbl>, VCF0732 <dbl+lbl>,
    ## #   VCF0733 <dbl+lbl>, VCF0743 <dbl+lbl>, VCF0932 <dbl+lbl>, VCF0933 <dbl+lbl>,
    ## #   VCF0934 <dbl+lbl>, VCF0935 <dbl+lbl>, VCF0936 <dbl+lbl>, VCF0941 <dbl+lbl>,
    ## #   VCF0942 <dbl+lbl>, VCF0943 <dbl+lbl>, VCF0944 <dbl+lbl>, VCF0945 <dbl+lbl>,
    ## #   VCF0946 <dbl+lbl>, VCF0947 <dbl+lbl>, VCF0948 <dbl+lbl>, …

# Tables/Cross Tabs

# Voted

``` r
Condensed_weights %>% with(table(VCF0702))
```

    ## VCF0702
    ##    1    2 
    ## 2620 9731

# Voted-Contacted by a Major Party?

``` r
Table1 <- Condensed_weights %>% with(table(VCF0702, VCF9030a))
Table1
```

    ##        VCF9030a
    ## VCF0702    1    2
    ##       1  542 2068
    ##       2 4327 5316

# Diff b/w parties?

``` r
attach(Condensed_weights)
table(VCF0501)
```

    ## VCF0501
    ##    1    2    9 
    ## 2137 9989  151

``` r
table(VCF0702, VCF0501)
```

    ##        VCF0501
    ## VCF0702    1    2    9
    ##       1  849 1664   80
    ##       2 1283 8312   67

``` r
Dem_Voters <- table(VCF0702, VCF0314)
round(prop.table(Dem_Voters,1),2)
```

    ##        VCF0314
    ## VCF0702    0    1    2    3    4    5
    ##       1 0.63 0.20 0.10 0.05 0.01 0.02
    ##       2 0.40 0.22 0.16 0.09 0.06 0.06

``` r
Diff_parties <- table(VCF0702, VCF0501)
Diff_parties
```

    ##        VCF0501
    ## VCF0702    1    2    9
    ##       1  849 1664   80
    ##       2 1283 8312   67

``` r
round(prop.table(Diff_parties,1),2)
```

    ##        VCF0501
    ## VCF0702    1    2    9
    ##       1 0.33 0.64 0.03
    ##       2 0.13 0.86 0.01

``` r
# (Less incentivized to vote if think there the same)

Dislike_DEM <- table(VCF0702, VCF9201)
Dislike_DEM
```

    ##        VCF9201
    ## VCF0702    0    1    2    3    4    5    6    7    8    9   10
    ##       1  134   83   99  165  190  646  207  249  272  111  338
    ##       2  832  442  667  780  592 1152  606  963 1245  709 1601

``` r
Dislike_REP <- table(VCF0702, VCF9202)

Rep_views <- table(VCF0702, VCF9203)
round(prop.table(Rep_views,1),2)
```

    ##        VCF9203
    ## VCF0702    1    2
    ##       1 0.46 0.54
    ##       2 0.68 0.32

``` r
which_party <- table(VCF0702, VCF9204)
round(prop.table(which_party,1),2)
```

    ##        VCF9204
    ## VCF0702    1    2    7
    ##       1 0.60 0.37 0.03
    ##       2 0.55 0.41 0.04

``` r
Party_pref <- table(VCF0702, VCF9206)
round(prop.table(Party_pref,1),2)
```

    ##        VCF9206
    ## VCF0702    1    2    3
    ##       1 0.18 0.47 0.36
    ##       2 0.28 0.50 0.22

``` r
Dem_Smart <- table(VCF0702, VCF0350)
round(prop.table(Dem_Smart,1),2)
```

    ##        VCF0350
    ## VCF0702    1    2    3    4
    ##       1 0.34 0.51 0.11 0.04
    ##       2 0.42 0.48 0.08 0.02

``` r
Dem_Angry <- table(VCF0702, VCF0358)
round(prop.table(Dem_Angry,1),2)
```

    ##        VCF0358
    ## VCF0702    1    2
    ##       1 0.41 0.59
    ##       2 0.50 0.50

``` r
REP_Angry <- table(VCF0702, VCF0370)
round(prop.table(REP_Angry,1),2)
```

    ##        VCF0370
    ## VCF0702    1    2
    ##       1 0.48 0.52
    ##       2 0.55 0.45

``` r
Approve_Pres <- table(VCF0702, VCF0450)
round(prop.table(Approve_Pres,1),2)
```

    ##        VCF0450
    ## VCF0702    1    2    8
    ##       1 0.48 0.49 0.02
    ##       2 0.49 0.50 0.01

``` r
Approve_Pres_rating <- table(VCF0702, VCF0451)
round(prop.table(Approve_Pres_rating,1),2)
```

    ##        VCF0451
    ## VCF0702    1    2    3    4    8
    ##       1 0.28 0.20 0.15 0.34 0.03
    ##       2 0.32 0.16 0.10 0.41 0.01

``` r
Pres_Econ <- table(VCF0702, VCF9220)
round(prop.table(Pres_Econ,1),2)
```

    ##        VCF9220
    ## VCF0702    1    2    3
    ##       1 0.29 0.31 0.40
    ##       2 0.38 0.23 0.38

``` r
Right_Track <- table(VCF0702, VCF9222)
round(prop.table(Right_Track,1),2)
```

    ##        VCF9222
    ## VCF0702    1    2
    ##       1 0.26 0.74
    ##       2 0.31 0.69

``` r
LIB_CON <- table(VCF0702, VCF0803)
round(prop.table(LIB_CON,1),2)
```

    ##        VCF0803
    ## VCF0702    1    2    3    4    5    6    7    9
    ##       1 0.02 0.07 0.09 0.31 0.11 0.09 0.02 0.28
    ##       2 0.03 0.12 0.11 0.25 0.13 0.19 0.04 0.13

``` r
Trust_Fed_Gov <- table(VCF0702, VCF0604)
round(prop.table(Trust_Fed_Gov,1),2)
```

    ##        VCF0604
    ## VCF0702    1    2    3    4    9
    ##       1 0.03 0.65 0.25 0.05 0.02
    ##       2 0.01 0.69 0.27 0.03 0.00

``` r
Crooked_FED <- table(VCF0702, VCF0608)
round(prop.table(Crooked_FED,1),2)
```

    ##        VCF0608
    ## VCF0702    1    2    3    9
    ##       1 0.58 0.34 0.06 0.02
    ##       2 0.57 0.36 0.06 0.01

``` r
TRUST_GOV_GEN <- table(VCF0702, VCF0656)
round(prop.table(TRUST_GOV_GEN,1),2)
```

    ##        VCF0656
    ## VCF0702    0    8   11   13   17   21   22   25   28   29   33   34   38   39
    ##       1 0.33 0.12 0.00 0.00 0.07 0.06 0.00 0.05 0.00 0.02 0.06 0.00 0.00 0.00
    ##       2 0.35 0.13 0.00 0.00 0.08 0.07 0.00 0.05 0.00 0.02 0.08 0.00 0.00 0.00
    ##        VCF0656
    ## VCF0702   42   44   46   50   54   56   58   59   61   63   67   71   72   75
    ##       1 0.02 0.00 0.02 0.06 0.02 0.00 0.01 0.00 0.00 0.00 0.05 0.01 0.00 0.04
    ##       2 0.02 0.00 0.02 0.05 0.02 0.00 0.01 0.00 0.00 0.00 0.04 0.00 0.00 0.03
    ##        VCF0656
    ## VCF0702   78   79   83   84   88   89   92  100
    ##       1 0.00 0.01 0.01 0.00 0.00 0.00 0.00 0.01
    ##       2 0.00 0.01 0.01 0.00 0.00 0.00 0.00 0.01

``` r
Volunteer <- table(VCF0702, VCF9256)
round(prop.table(Volunteer,1),2)
```

    ##        VCF9256
    ## VCF0702    1    2
    ##       1 0.29 0.71
    ##       2 0.47 0.53

``` r
Interst_Elec <- table(VCF0702, VCF0310)
round(prop.table(Interst_Elec,1),2)
```

    ##        VCF0310
    ## VCF0702    1    2    3    9
    ##       1 0.33 0.45 0.22 0.00
    ##       2 0.09 0.38 0.53 0.00

``` r
Care_win <- table(VCF0702, VCF0311)
round(prop.table(Care_win,1),2)
```

    ##        VCF0311
    ## VCF0702    1    2
    ##       1 0.42 0.58
    ##       2 0.10 0.90

``` r
Care_win_CONG <- table(VCF0702, VCF0312)
round(prop.table(Care_win_CONG,1),2)
```

    ##        VCF0312
    ## VCF0702    1    2
    ##       1 0.54 0.46
    ##       2 0.24 0.76

``` r
Pred_Close <- table(VCF0702, VCF0714)
round(prop.table(Pred_Close,1),2)
```

    ##        VCF0714
    ## VCF0702    1    2    9
    ##       1 0.25 0.74 0.01
    ##       2 0.21 0.79 0.01

``` r
Pred_Close
```

    ##        VCF0714
    ## VCF0702    1    2    9
    ##       1  647 1900   37
    ##       2 2008 7618   50

``` r
detach(Condensed_weights)
```

# Correlation

``` r
# A good alternative to attach() is with(), it does the same thing but is
# limited to just the one use, and won't change your environment for the rest of
# the session the way attach does

res <- Condensed_weights %>% 
  with(
     cor.test(VCF0702, VCF0102, 
                method = "pearson")
     )

res
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  VCF0702 and VCF0102
    ## t = 22.864, df = 12159, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1859255 0.2200076
    ## sample estimates:
    ##      cor 
    ## 0.203028

# rename variables

``` r
# attach(Condensed_weights)
# 
# names(Condensed_weights)[names(Condensed_weights)=="VCF0102"] <- "Age"
# names(Condensed_weights)[names(Condensed_weights)=="VCF0104"] <- "Gender"
# names(Condensed_weights)[names(Condensed_weights)=="VCF9229"] <- "UE"
# names(Condensed_weights)[names(Condensed_weights)=="VCF0880"] <- "Better_worse"
# names(Condensed_weights)[names(Condensed_weights)=="VCF0606"] <- "FED_Tax"
# names(Condensed_weights)[names(Condensed_weights)=="VCF0501"] <- "Bw_parties"
# names(Condensed_weights)[names(Condensed_weights)=="VCF9030a"] <- "contacted_by_party"
# names(Condensed_weights)[names(Condensed_weights)=="VCF9206"] <- "control_split"
# names(Condensed_weights)[names(Condensed_weights)=="VCF0105b"] <- "Race"
# names(Condensed_weights)[names(Condensed_weights)=="VCF9222"] <- "Country_direction"

# rename() can make this a bit easier to write/read
Condensed_weights_renamed <- Updated1 %>% 
  rename(
    Age = VCF0102,
    Gender = VCF0104,
    UE = VCF9229,
    Better_worse = VCF0880,
    FED_Tax = VCF0606,
    Bw_parties = VCF0501,
    contacted_by_party = VCF9030a,
    control_split = VCF9206,
    Race = VCF0105b,
    Country_direction = VCF9222
  )
```

``` r
Condensed_weights_renamed %>% zap_labels() %>% skim(Age)
```

|                                                  |            |
| :----------------------------------------------- | :--------- |
| Name                                             | Piped data |
| Number of rows                                   | 13718      |
| Number of columns                                | 1029       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| numeric                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate | mean |   sd | p0 | p25 | p50 | p75 | p100 | hist  |
| :------------- | ---------: | -------------: | ---: | ---: | -: | --: | --: | --: | ---: | :---- |
| Age            |        226 |           0.98 | 3.93 | 1.72 |  1 |   2 |   4 |   5 |    7 | ▇▅▆▆▆ |

# Regressions

# creating dummy variables

``` r
Condensed_weights_w_factors <- Condensed_weights_renamed %>% 
  mutate(
    Age_Dummy = factor(Age),
    Gender_DV = factor(Gender),
    Future_UE_Var = factor(UE),
    Better_worse_dummy = factor(Better_worse),
    FED_Gov_tax_dummy = factor(FED_Tax),
    bw_parties_DV = factor(Bw_parties),
    Contacted_by_party_DV = factor(contacted_by_party),
    Control_split_DV = factor(control_split),
    Race_DV = factor(Race),
    Dummy_dep_voting = ifelse (VCF0702 > 1, 1, 0),
    Dummy_right_dir = ifelse (Country_direction > 1, 0, 1),
  )

Condensed_weights_w_factors %>% 
  zap_labels() %>% 
  select(
    Age_Dummy,
    Gender_DV,
    Future_UE_Var,
    Better_worse_dummy,
    FED_Gov_tax_dummy,
    bw_parties_DV,
    Contacted_by_party_DV,
    Control_split_DV,
    Race_DV,
    Dummy_dep_voting,
    Dummy_right_dir
  ) %>% 
  skim()
```

|                                                  |            |
| :----------------------------------------------- | :--------- |
| Name                                             | Piped data |
| Number of rows                                   | 13718      |
| Number of columns                                | 11         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| factor                                           | 9          |
| numeric                                          | 2          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: factor**

| skim\_variable           | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                        |
| :----------------------- | ---------: | -------------: | :------ | --------: | :--------------------------------- |
| Age\_Dummy               |        226 |           0.98 | FALSE   |         7 | 5: 2652, 4: 2504, 2: 2200, 3: 2170 |
| Gender\_DV               |         41 |           1.00 | FALSE   |         3 | 2: 7269, 1: 6397, 3: 11            |
| Future\_UE\_Var          |        395 |           0.97 | FALSE   |         3 | 2: 7103, 1: 3168, 3: 3052          |
| Better\_worse\_dummy     |         91 |           0.99 | FALSE   |         3 | 1: 5284, 3: 5186, 2: 3157          |
| FED\_Gov\_tax\_dummy     |        400 |           0.97 | FALSE   |         4 | 1: 9302, 2: 3558, 3: 383, 9: 75    |
| bw\_parties\_DV          |       1441 |           0.89 | FALSE   |         3 | 2: 9989, 1: 2137, 9: 151           |
| Contacted\_by\_party\_DV |       1443 |           0.89 | FALSE   |         2 | 2: 7404, 1: 4871                   |
| Control\_split\_DV       |        231 |           0.98 | FALSE   |         3 | 2: 6672, 1: 3442, 3: 3373          |
| Race\_DV                 |        102 |           0.99 | FALSE   |         4 | 1: 8542, 2: 2160, 3: 2094, 4: 820  |

**Variable type: numeric**

| skim\_variable     | n\_missing | complete\_rate | mean |   sd | p0 | p25 | p50 | p75 | p100 | hist  |
| :----------------- | ---------: | -------------: | ---: | ---: | -: | --: | --: | --: | ---: | :---- |
| Dummy\_dep\_voting |       1367 |           0.90 | 0.79 | 0.41 |  0 |   1 |   1 |   1 |    1 | ▂▁▁▁▇ |
| Dummy\_right\_dir  |        244 |           0.98 | 0.29 | 0.46 |  0 |   0 |   0 |   1 |    1 | ▇▁▁▁▃ |

``` r
reg_Dummyvar <- lm(Dummy_dep_voting ~ Age_Dummy + Gender_DV + Dummy_right_dir + Better_worse_dummy + Future_UE_Var + FED_Gov_tax_dummy + bw_parties_DV  + Contacted_by_party_DV + Control_split_DV + Race_DV, weights= VCF0009z, data=Condensed_weights_w_factors)

summary(reg_Dummyvar)
```

    ## 
    ## Call:
    ## lm(formula = Dummy_dep_voting ~ Age_Dummy + Gender_DV + Dummy_right_dir + 
    ##     Better_worse_dummy + Future_UE_Var + FED_Gov_tax_dummy + 
    ##     bw_parties_DV + Contacted_by_party_DV + Control_split_DV + 
    ##     Race_DV, data = Condensed_weights_w_factors, weights = VCF0009z)
    ## 
    ## Weighted Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.00777  0.00149  0.10073  0.21193  1.38776 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             0.5891424  0.0193267  30.483  < 2e-16 ***
    ## Age_Dummy2              0.0730955  0.0140122   5.217 1.86e-07 ***
    ## Age_Dummy3              0.1407488  0.0142939   9.847  < 2e-16 ***
    ## Age_Dummy4              0.1644825  0.0141684  11.609  < 2e-16 ***
    ## Age_Dummy5              0.1883887  0.0143683  13.111  < 2e-16 ***
    ## Age_Dummy6              0.2133639  0.0159644  13.365  < 2e-16 ***
    ## Age_Dummy7              0.2157932  0.0188129  11.470  < 2e-16 ***
    ## Gender_DV2              0.0269978  0.0074105   3.643  0.00027 ***
    ## Gender_DV3             -0.1028558  0.1317010  -0.781  0.43483    
    ## Dummy_right_dir         0.0145280  0.0091800   1.583  0.11355    
    ## Better_worse_dummy2     0.0005955  0.0101500   0.059  0.95321    
    ## Better_worse_dummy3    -0.0282501  0.0089712  -3.149  0.00164 ** 
    ## Future_UE_Var2          0.0078438  0.0091757   0.855  0.39266    
    ## Future_UE_Var3          0.0260209  0.0115830   2.246  0.02469 *  
    ## FED_Gov_tax_dummy2     -0.0057772  0.0087752  -0.658  0.51032    
    ## FED_Gov_tax_dummy3     -0.0522199  0.0228331  -2.287  0.02221 *  
    ## FED_Gov_tax_dummy9     -0.2720643  0.0815313  -3.337  0.00085 ***
    ## bw_parties_DV2          0.1855489  0.0097932  18.947  < 2e-16 ***
    ## bw_parties_DV9         -0.0760975  0.0393402  -1.934  0.05310 .  
    ## Contacted_by_party_DV2 -0.1130875  0.0078796 -14.352  < 2e-16 ***
    ## Control_split_DV2      -0.0280436  0.0091172  -3.076  0.00210 ** 
    ## Control_split_DV3      -0.0961414  0.0106853  -8.998  < 2e-16 ***
    ## Race_DV2                0.0253385  0.0118225   2.143  0.03211 *  
    ## Race_DV3               -0.0725520  0.0122640  -5.916 3.40e-09 ***
    ## Race_DV4               -0.0742187  0.0159625  -4.650 3.36e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3913 on 11235 degrees of freedom
    ##   (2458 observations deleted due to missingness)
    ## Multiple R-squared:  0.1286, Adjusted R-squared:  0.1268 
    ## F-statistic: 69.11 on 24 and 11235 DF,  p-value: < 2.2e-16
