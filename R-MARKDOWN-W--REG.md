MZ Review
================

`r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE) # Would not
knit with this code- need to fix`

\#Install packages if first time

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
install.packages("skimr")
```

`r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

\#Read in Packages needed

``` r
library(dplyr) # manipulate dataframes
library(readr) # read/write dataframes
library(tidyr) # reshaping dataframes
library(stringr) # string manipulation
library(forcats) # factor manipulation
library(purrr) # iteration (instead of loops)
library(ggpubr) # for Correlation
library('ggplot2') # for Plots
library ('car')
library ('gdata')
library('skimr') 
```

# Does not read this- need to fix data import for reproducibilty

# Import the data- ANES Survey Timeseries

# Unzip the ANES data if needed

``` r}
source("unzip-anes-data.R")
```

``` r
library(haven)
anes_timeseries_cdf_stata13 <- read_dta("data-raw/anes_timeseries_cdf_stata13.dta")
```

# Filtering for the right years

``` r
June_DS <-anes_timeseries_cdf_stata13[!(anes_timeseries_cdf_stata13$VCF0004<"2004"),]
```

``` r
# Removing Labels

June_DS %>% 
  zap_labels() %>% 
  select(
    VCF0102,
    VCF0104, 
    VCF0880, 
    VCF0606, 
    VCF0501,
    VCF9030a,
    VCF9206, 
    VCF0105b, 
    VCF0450, 
    VCF0301,  
    VCF9206, 
    VCF9202,
    VCF0358, 
    VCF0360, 
    VCF0370, 
    VCF0372, 
    VCF9253, 
    VCF0714, 
    VCF9256, 
    VCF0900b, 
    VCF0114, 
    VCF9021,  
    VCF9030b,
    VCF9030c,     
    VCF9222, 
    VCF0901a,
    VCF0009z,
    VCF0702,
  ) %>% 
  skim()
```

|                                                  |            |
| :----------------------------------------------- | :--------- |
| Name                                             | Piped data |
| Number of rows                                   | 13718      |
| Number of columns                                | 27         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| numeric                                          | 27         |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |      sd |     p0 |     p25 |     p50 |    p75 |    p100 | hist  |
| :------------- | ---------: | -------------: | ------: | ------: | -----: | ------: | ------: | -----: | ------: | :---- |
| VCF0102        |        226 |           0.98 |    3.93 |    1.72 |   1.00 |    2.00 |    4.00 |    5.0 |    7.00 | ▇▅▆▆▆ |
| VCF0104        |         41 |           1.00 |    1.53 |    0.50 |   1.00 |    1.00 |    2.00 |    2.0 |    3.00 | ▇▁▇▁▁ |
| VCF0880        |         91 |           0.99 |    1.99 |    0.88 |   1.00 |    1.00 |    2.00 |    3.0 |    3.00 | ▇▁▅▁▇ |
| VCF0606        |        400 |           0.97 |    1.37 |    0.78 |   1.00 |    1.00 |    1.00 |    2.0 |    9.00 | ▇▁▁▁▁ |
| VCF0501        |       1441 |           0.89 |    1.91 |    0.88 |   1.00 |    2.00 |    2.00 |    2.0 |    9.00 | ▇▁▁▁▁ |
| VCF9030a       |       1443 |           0.89 |    1.60 |    0.49 |   1.00 |    1.00 |    2.00 |    2.0 |    2.00 | ▅▁▁▁▇ |
| VCF9206        |        231 |           0.98 |    1.99 |    0.71 |   1.00 |    1.00 |    2.00 |    3.0 |    3.00 | ▅▁▇▁▅ |
| VCF0105b       |        102 |           0.99 |    1.65 |    0.95 |   1.00 |    1.00 |    1.00 |    2.0 |    4.00 | ▇▂▁▂▁ |
| VCF0450        |        102 |           0.99 |    1.59 |    0.89 |   1.00 |    1.00 |    2.00 |    2.0 |    8.00 | ▇▁▁▁▁ |
| VCF0301        |        104 |           0.99 |    3.62 |    2.11 |   1.00 |    2.00 |    3.00 |    5.0 |    7.00 | ▇▃▃▂▅ |
| VCF9202        |       1574 |           0.89 |    4.58 |    2.90 |   0.00 |    2.00 |    5.00 |    7.0 |   10.00 | ▇▆▇▆▃ |
| VCF0358        |         78 |           0.99 |    1.52 |    0.50 |   1.00 |    1.00 |    2.00 |    2.0 |    2.00 | ▇▁▁▁▇ |
| VCF0360        |         86 |           0.99 |    1.40 |    0.49 |   1.00 |    1.00 |    1.00 |    2.0 |    2.00 | ▇▁▁▁▆ |
| VCF0370        |        104 |           0.99 |    1.46 |    0.50 |   1.00 |    1.00 |    1.00 |    2.0 |    2.00 | ▇▁▁▁▇ |
| VCF0372        |        116 |           0.99 |    1.57 |    0.50 |   1.00 |    1.00 |    2.00 |    2.0 |    2.00 | ▆▁▁▁▇ |
| VCF9253        |       2524 |           0.82 |    3.85 |    1.17 |   1.00 |    3.00 |    4.00 |    5.0 |    5.00 | ▁▂▅▆▇ |
| VCF0714        |        110 |           0.99 |    1.83 |    0.74 |   1.00 |    2.00 |    2.00 |    2.0 |    9.00 | ▇▁▁▁▁ |
| VCF9256        |       1410 |           0.90 |    1.57 |    0.50 |   1.00 |    1.00 |    2.00 |    2.0 |    2.00 | ▆▁▁▁▇ |
| VCF0900b       |        192 |           0.99 | 2807.86 | 1608.90 | 101.00 | 1222.00 | 2706.00 | 4206.0 | 5601.00 | ▇▇▅▇▇ |
| VCF0114        |        637 |           0.95 |    2.81 |    1.15 |   1.00 |    2.00 |    3.00 |    4.0 |    5.00 | ▅▃▇▆▁ |
| VCF9021        |       2463 |           0.82 |    3.13 |    2.00 |   1.00 |    1.00 |    5.00 |    5.0 |    5.00 | ▇▁▁▁▇ |
| VCF9030b       |       1486 |           0.89 |    1.70 |    0.46 |   1.00 |    1.00 |    2.00 |    2.0 |    2.00 | ▃▁▁▁▇ |
| VCF9030c       |       1486 |           0.89 |    1.75 |    0.43 |   1.00 |    1.00 |    2.00 |    2.0 |    2.00 | ▃▁▁▁▇ |
| VCF9222        |        244 |           0.98 |    1.71 |    0.46 |   1.00 |    1.00 |    2.00 |    2.0 |    2.00 | ▃▁▁▁▇ |
| VCF0901a       |          0 |           1.00 |   27.96 |   16.11 |   1.00 |   12.00 |   27.00 |   42.0 |   56.00 | ▇▅▅▇▆ |
| VCF0009z       |          0 |           1.00 |    1.00 |    0.78 |   0.02 |    0.45 |    0.79 |    1.3 |    6.81 | ▇▂▁▁▁ |
| VCF0702        |       1367 |           0.90 |    1.79 |    0.41 |   1.00 |    2.00 |    2.00 |    2.0 |    2.00 | ▂▁▁▁▇ |

# Filtering the Dataset for the right variables

``` r
July_DS_1 = subset(June_DS, select = c(VCF0102,
    VCF0104,
    VCF0702,
    VCF0880, 
    VCF0606, 
    VCF0501,
    VCF9030a,
    VCF9206, 
    VCF0105b, 
    VCF0450, 
    VCF0301,  
    VCF9202,
    VCF0358, 
    VCF0360, 
    VCF0370, 
    VCF0372, 
    VCF0714, 
    VCF9256, 
    VCF0900b, 
    VCF0114, 
    VCF9030b,
    VCF9030c,     
    VCF9222,
    VCF9021,
    VCF9201,
     VCF0901a,
    VCF0009z))
```

# Dealing w/ missings \# For many of the categories I added a “Dummy” category to signal the question was unanswered. If the regression shows these dummies are not significant, can assume that these missings were not strongly biased in a direction.

``` r
July_DS_1 <- July_DS_1 %>%
   mutate(
    VCF0501 = replace_na(VCF0501, 9),
    VCF9256 = replace_na(VCF9256, 3),
    VCF9030b =  replace_na(VCF9030b, 3),
    VCF9030c =  replace_na(VCF9030c, 3),
    VCF0114 = replace_na(VCF0114, 6),
    VCF9021 =  replace_na(VCF9021, 3))
```

summary(“Gender”)

``` r
July_DS_1 <- July_DS_1 %>% 
    rename(
    control_split = VCF9206,
    Voted_Y_N = VCF0702,
    Race = VCF0105b,
    Weights = VCF0009z,
    Dem_Pres_Angry = VCF0358,
    Dem_Pres_hopeful= VCF0360,
    Rep_Pres_Angry = VCF0370,
    Rep_Pres_hopeful = VCF0372,
    Election_Close = VCF0714,
    Age = VCF0102,
    Gender = VCF0104,
    Better_worse = VCF0880,
    FED_Tax = VCF0606,
    Bw_parties = VCF0501,
    contacted_by_party = VCF9030a,
    Volunteer_work = VCF9256,
    State_CD = VCF0900b,
    Income = VCF0114,
    Contacted_Dem_Party = VCF9030b,
    Contacted_Rep_Party =VCF9030c,                  
        Country_direction = VCF9222,
    State = VCF0901a,
        Approve_President = VCF0450, 
    Party = VCF0301,
        Anyone_talk_vote = VCF9021,
        Like_dislike_Rep_party = VCF9202,
        Like_dislike_Dem_party = VCF9201
)
```

# Creating Dummy Variables

``` r
    Final_DS_DV <- July_DS_1 %>% 
    mutate(
    Age_Dummy = factor(Age),
    Gender_DV = factor(Gender),
    bw_parties_DV = factor(Bw_parties),
    Contacted_by_party_DV = factor(contacted_by_party),
    Contacted_Dem_Party_DV = factor(Contacted_Dem_Party),
    Contacted_Rep_Party_DV= factor(Contacted_Rep_Party),
    Control_split_DV = factor(control_split),
    Race_DV = factor(Race),
    Election_Close_DV = factor(Election_Close),
    Better_worse_DV = factor(Better_worse),
    contacted_by_party_DV = factor(contacted_by_party),
    Volunteer_work_DV = factor(Volunteer_work),
    Income_DV = factor(Income),
    Anyone_talk_vote_DV =factor(Anyone_talk_vote),
    Appove_Pres = factor(Approve_President),
    Voted_Y_N_DV = ifelse (Voted_Y_N > 1, 1, 0),
    Country_direction_DV = ifelse (Country_direction > 1, 0, 1)
  )
```

# Regression

``` r
reg_DV <- lm(Voted_Y_N_DV ~ Age_Dummy + Gender_DV + Race_DV + bw_parties_DV + Contacted_by_party_DV + Control_split_DV + Election_Close_DV + Anyone_talk_vote_DV +Contacted_Dem_Party_DV+ Contacted_Rep_Party_DV+ Better_worse_DV +Volunteer_work_DV  + Income_DV + Country_direction_DV, weights= Weights, data=Final_DS_DV)

reg_Age <- lm(Voted_Y_N_DV ~ Age_Dummy, data=Final_DS_DV)
summary(reg_Age)
```

    ## 
    ## Call:
    ## lm(formula = Voted_Y_N_DV ~ Age_Dummy, data = Final_DS_DV)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8853  0.1147  0.1551  0.2397  0.4221 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.57790    0.01227  47.087   <2e-16 ***
    ## Age_Dummy2   0.12980    0.01521   8.532   <2e-16 ***
    ## Age_Dummy3   0.18243    0.01527  11.950   <2e-16 ***
    ## Age_Dummy4   0.23440    0.01487  15.759   <2e-16 ***
    ## Age_Dummy5   0.26698    0.01471  18.147   <2e-16 ***
    ## Age_Dummy6   0.30743    0.01571  19.565   <2e-16 ***
    ## Age_Dummy7   0.30355    0.01838  16.514   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3994 on 12154 degrees of freedom
    ##   (1557 observations deleted due to missingness)
    ## Multiple R-squared:  0.0459, Adjusted R-squared:  0.04543 
    ## F-statistic: 97.45 on 6 and 12154 DF,  p-value: < 2.2e-16

``` r
summary(reg_DV)
```

    ## 
    ## Call:
    ## lm(formula = Voted_Y_N_DV ~ Age_Dummy + Gender_DV + Race_DV + 
    ##     bw_parties_DV + Contacted_by_party_DV + Control_split_DV + 
    ##     Election_Close_DV + Anyone_talk_vote_DV + Contacted_Dem_Party_DV + 
    ##     Contacted_Rep_Party_DV + Better_worse_DV + Volunteer_work_DV + 
    ##     Income_DV + Country_direction_DV, data = Final_DS_DV, weights = Weights)
    ## 
    ## Weighted Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.91403 -0.02998  0.09545  0.21334  1.37869 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              0.5095213  0.0224057  22.741  < 2e-16 ***
    ## Age_Dummy2               0.0614657  0.0138025   4.453 8.54e-06 ***
    ## Age_Dummy3               0.1100762  0.0141547   7.777 8.08e-15 ***
    ## Age_Dummy4               0.1406761  0.0139928  10.053  < 2e-16 ***
    ## Age_Dummy5               0.1717060  0.0141018  12.176  < 2e-16 ***
    ## Age_Dummy6               0.2098671  0.0156286  13.428  < 2e-16 ***
    ## Age_Dummy7               0.2326485  0.0182850  12.723  < 2e-16 ***
    ## Gender_DV2               0.0310119  0.0072402   4.283 1.86e-05 ***
    ## Gender_DV3              -0.0612559  0.1295002  -0.473 0.636210    
    ## Race_DV2                 0.0592626  0.0118251   5.012 5.48e-07 ***
    ## Race_DV3                -0.0551375  0.0119987  -4.595 4.37e-06 ***
    ## Race_DV4                -0.0715891  0.0155562  -4.602 4.23e-06 ***
    ## bw_parties_DV2           0.1637273  0.0096255  17.010  < 2e-16 ***
    ## bw_parties_DV9          -0.1217700  0.0330790  -3.681 0.000233 ***
    ## Contacted_by_party_DV2  -0.1173090  0.0182222  -6.438 1.26e-10 ***
    ## Control_split_DV2       -0.0340136  0.0088693  -3.835 0.000126 ***
    ## Control_split_DV3       -0.0865804  0.0104138  -8.314  < 2e-16 ***
    ## Election_Close_DV2       0.0120117  0.0089555   1.341 0.179863    
    ## Election_Close_DV9      -0.1165672  0.0532586  -2.189 0.028638 *  
    ## Anyone_talk_vote_DV3    -0.0002679  0.0140502  -0.019 0.984788    
    ## Anyone_talk_vote_DV5    -0.0222387  0.0078867  -2.820 0.004814 ** 
    ## Contacted_Dem_Party_DV2  0.0148175  0.0145068   1.021 0.307079    
    ## Contacted_Dem_Party_DV3 -0.0456277  0.0609771  -0.748 0.454310    
    ## Contacted_Rep_Party_DV2  0.0213455  0.0135959   1.570 0.116443    
    ## Contacted_Rep_Party_DV3         NA         NA      NA       NA    
    ## Better_worse_DV2         0.0043514  0.0098410   0.442 0.658372    
    ## Better_worse_DV3        -0.0099796  0.0087433  -1.141 0.253728    
    ## Volunteer_work_DV2      -0.0621223  0.0074730  -8.313  < 2e-16 ***
    ## Volunteer_work_DV3      -0.3126782  0.1277496  -2.448 0.014397 *  
    ## Income_DV2               0.0506581  0.0131639   3.848 0.000120 ***
    ## Income_DV3               0.1300826  0.0114184  11.392  < 2e-16 ***
    ## Income_DV4               0.1807109  0.0121643  14.856  < 2e-16 ***
    ## Income_DV5               0.1979624  0.0192811  10.267  < 2e-16 ***
    ## Income_DV6               0.1269340  0.0227085   5.590 2.33e-08 ***
    ## Country_direction_DV     0.0161201  0.0085944   1.876 0.060730 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3848 on 11468 degrees of freedom
    ##   (2216 observations deleted due to missingness)
    ## Multiple R-squared:  0.1604, Adjusted R-squared:  0.158 
    ## F-statistic: 66.41 on 33 and 11468 DF,  p-value: < 2.2e-16

``` r
plot(reg_DV)
```

![](R-MARKDOWN-W--REG_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->![](R-MARKDOWN-W--REG_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->![](R-MARKDOWN-W--REG_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->![](R-MARKDOWN-W--REG_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

``` {r
# Not working with knit- (seemed to work otherwise)
Table1 <- table(Voted_Y_N, Race) 

table(VCF0501)

table(VCF0702, Bw_parties)

Dem_Voters <- table(VCF0702, VCF0314)
round(prop.table(Dem_Voters,1),2)

Diff_parties <- table(VCF0702, VCF0501)
Diff_parties
round(prop.table(Diff_parties,1),2)

Dislike_DEM <- table(VCF0702, VCF9201)
Dislike_DEM

Dislike_REP <- table(VCF0702, VCF9202)

Rep_views <- table(VCF0702, VCF9203)
round(prop.table(Rep_views,1),2)

which_party <- table(VCF0702, VCF9204)
round(prop.table(which_party,1),2)

Party_pref <- table(VCF0702, VCF9206)
round(prop.table(Party_pref,1),2)

Dem_Smart <- table(VCF0702, VCF0350)
round(prop.table(Dem_Smart,1),2)

Dem_Angry <- table(VCF0702, VCF0358)
round(prop.table(Dem_Angry,1),2)

REP_Angry <- table(VCF0702, VCF0370)
round(prop.table(REP_Angry,1),2)

Approve_Pres <- table(VCF0702, VCF0450)
round(prop.table(Approve_Pres,1),2)

Approve_Pres_rating <- table(VCF0702, VCF0451)
round(prop.table(Approve_Pres_rating,1),2)

Pres_Econ <- table(VCF0702, VCF9220)
round(prop.table(Pres_Econ,1),2)

Right_Track <- table(VCF0702, Country_direction)
round(prop.table(Right_Track,1),2)

LIB_CON <- table(VCF0702, VCF0803)
round(prop.table(LIB_CON,1),2)

Trust_Fed_Gov <- table(VCF0702, VCF0604)
round(prop.table(Trust_Fed_Gov,1),2)

Crooked_FED <- table(VCF0702, VCF0608)
round(prop.table(Crooked_FED,1),2)

TRUST_GOV_GEN <- table(VCF0702, VCF0656)
round(prop.table(TRUST_GOV_GEN,1),2)

Volunteer <- table(VCF0702, VCF9256)
round(prop.table(Volunteer,1),2)

Interst_Elec <- table(VCF0702, VCF0310)
round(prop.table(Interst_Elec,1),2)

Care_win <- table(VCF0702, VCF0311)
round(prop.table(Care_win,1),2)

Care_win_CONG <- table(VCF0702, VCF0312)
round(prop.table(Care_win_CONG,1),2)

Your_econ <- table(VCF0702, Better_worse)
round(prop.table(Your_econ,1),2)

Pred_Close <- table(VCF0702, VCF0714)
round(prop.table(Pred_Close,1),2)

barplot(Care_win, col=c("Purple","blue"),
       beside=T,
       main="What motivates voters?",
    legend= c("No Difference", "Difference"),  
       ylab= "Voted?",
       xlab= "Is there a difference between the two major parties", 
    las=1)

```

``` {r
# Need to fix code- will not plot 

barchart <- ggplot(July_DS_1, aes(Y = Voted_Y_N, x = Race)) +
    geom_bar(stat = "identity")

barchart
```
