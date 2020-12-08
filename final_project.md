Final\_Project\_group26(Fish market))
================
ChungKwai Wong
2020/11/29

``` r
fish <- read.csv("Fish.csv",fileEncoding = "UTF-8-BOM")

# add the new additional data point 
fish_b <- rbind(fish,"160" = c("Smelt", 8.0, 10.2, 11.7, 11.8, 1.9945, 1.4152)) 
fish_b$Species <- as.factor(fish_b$Species)
fish <- fish_b
fish$Weight  <- as.double(fish$Weight)
fish$Length1 <- as.double(fish$Length1)
fish$Length2 <- as.double(fish$Length2)
fish$Length3 <- as.double(fish$Length3)
fish$Height  <- as.double(fish$Height)
fish$Width   <- as.double(fish$Width)
```

# Scatterplot matrices

``` r
pairs(fish)
```

![](final_project_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# model 1

``` r
#full model
m1 <- lm(Height ~ Length1 + Length2 + Length3  + Weight + Width + Species ,data = fish)

summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = Height ~ Length1 + Length2 + Length3 + Weight + 
    ##     Width + Species, data = fish)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.93143 -0.25742 -0.01263  0.27204  1.86872 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       4.7537079  0.8364704   5.683 6.81e-08 ***
    ## Length1          -0.2783956  0.2209379  -1.260    0.210    
    ## Length2           0.1224601  0.2663392   0.460    0.646    
    ## Length3           0.2323394  0.1771879   1.311    0.192    
    ## Weight            0.0002154  0.0005190   0.415    0.679    
    ## Width             1.0626602  0.1218860   8.718 5.30e-15 ***
    ## SpeciesParkki    -1.8203186  0.4531272  -4.017 9.34e-05 ***
    ## SpeciesPerch     -5.1389241  0.6111284  -8.409 3.20e-14 ***
    ## SpeciesPike      -7.6633827  0.5735744 -13.361  < 2e-16 ***
    ## SpeciesRoach     -4.7605520  0.4169724 -11.417  < 2e-16 ***
    ## SpeciesSmelt     -5.3437497  0.6499237  -8.222 9.35e-14 ***
    ## SpeciesWhitefish -4.4472738  0.4749903  -9.363  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5909 on 148 degrees of freedom
    ## Multiple R-squared:  0.9825, Adjusted R-squared:  0.9812 
    ## F-statistic: 754.9 on 11 and 148 DF,  p-value: < 2.2e-16

``` r
anova(m1)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Height
    ##            Df  Sum Sq Mean Sq  F value    Pr(>F)    
    ## Length1     1 1176.62 1176.62 3370.015 < 2.2e-16 ***
    ## Length2     1  685.32  685.32 1962.862 < 2.2e-16 ***
    ## Length3     1  600.33  600.33 1719.442 < 2.2e-16 ***
    ## Weight      1   98.69   98.69  282.651 < 2.2e-16 ***
    ## Width       1  203.69  203.69  583.385 < 2.2e-16 ***
    ## Species     6  134.76   22.46   64.328 < 2.2e-16 ***
    ## Residuals 148   51.67    0.35                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# model 2 (interaction)

``` r
m2 <- lm(Height ~ Length1 + Length2 + Length3 + Weight + Width + Species + Length1 * Species + Length2 * Species + Length3 * Species + Weight * Species + Width *Species  ,data = fish)


summary(m2)
```

    ## 
    ## Call:
    ## lm(formula = Height ~ Length1 + Length2 + Length3 + Weight + 
    ##     Width + Species + Length1 * Species + Length2 * Species + 
    ##     Length3 * Species + Weight * Species + Width * Species, data = fish)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.9371 -0.1739  0.0000  0.1836  0.8275 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               3.0624043  1.6405010   1.867  0.06442 .  
    ## Length1                  -0.6350031  0.3404711  -1.865  0.06466 .  
    ## Length2                   0.3510791  0.3812468   0.921  0.35900    
    ## Length3                   0.4296143  0.2964506   1.449  0.14994    
    ## Weight                    0.0048081  0.0011048   4.352 2.89e-05 ***
    ## Width                     0.0540178  0.3108057   0.174  0.86232    
    ## SpeciesParkki            -0.6994786  3.4511908  -0.203  0.83974    
    ## SpeciesPerch             -2.7356999  1.6980080  -1.611  0.10982    
    ## SpeciesPike               1.4690409  2.7344007   0.537  0.59211    
    ## SpeciesRoach             -4.0407038  1.9825596  -2.038  0.04377 *  
    ## SpeciesSmelt             -2.4262802  3.3406615  -0.726  0.46910    
    ## SpeciesWhitefish         11.4242686 28.8467439   0.396  0.69280    
    ## Length1:SpeciesParkki    -0.5449564  5.7476959  -0.095  0.92462    
    ## Length1:SpeciesPerch      0.6958125  0.4262414   1.632  0.10525    
    ## Length1:SpeciesPike       1.5014187  0.9815203   1.530  0.12877    
    ## Length1:SpeciesRoach      0.0868616  0.5778051   0.150  0.88076    
    ## Length1:SpeciesSmelt      0.7132029  1.5171580   0.470  0.63916    
    ## Length1:SpeciesWhitefish  2.8207841  7.9570427   0.355  0.72360    
    ## Length2:SpeciesParkki    -4.1544092  7.5565331  -0.550  0.58351    
    ## Length2:SpeciesPerch     -0.3462941  0.5477359  -0.632  0.52846    
    ## Length2:SpeciesPike      -0.4453847  0.9367831  -0.475  0.63535    
    ## Length2:SpeciesRoach     -0.0490672  0.6808080  -0.072  0.94267    
    ## Length2:SpeciesSmelt     -0.3379158  0.6002762  -0.563  0.57455    
    ## Length2:SpeciesWhitefish  0.5024883  5.3698721   0.094  0.92561    
    ## Length3:SpeciesParkki     4.3013111  3.9515521   1.089  0.27859    
    ## Length3:SpeciesPerch     -0.3396825  0.3992452  -0.851  0.39660    
    ## Length3:SpeciesPike      -1.2049972  0.4122801  -2.923  0.00416 ** 
    ## Length3:SpeciesRoach     -0.0423051  0.4413260  -0.096  0.92380    
    ## Length3:SpeciesSmelt     -0.4401571  1.1410410  -0.386  0.70038    
    ## Length3:SpeciesWhitefish -3.3549908  5.9852528  -0.561  0.57617    
    ## Weight:SpeciesParkki      0.0083245  0.0145170   0.573  0.56744    
    ## Weight:SpeciesPerch      -0.0034765  0.0012567  -2.766  0.00658 ** 
    ## Weight:SpeciesPike       -0.0024257  0.0017242  -1.407  0.16211    
    ## Weight:SpeciesRoach      -0.0055119  0.0029536  -1.866  0.06450 .  
    ## Weight:SpeciesSmelt       0.0516065  0.1268345   0.407  0.68483    
    ## Weight:SpeciesWhitefish   0.0006414  0.0094557   0.068  0.94603    
    ## Width:SpeciesParkki      -1.2269960  2.4211905  -0.507  0.61326    
    ## Width:SpeciesPerch        0.5082393  0.3445149   1.475  0.14281    
    ## Width:SpeciesPike         1.2694930  0.4623800   2.746  0.00699 ** 
    ## Width:SpeciesRoach        0.6835914  0.6130380   1.115  0.26708    
    ## Width:SpeciesSmelt       -0.0203541  1.1183303  -0.018  0.98551    
    ## Width:SpeciesWhitefish    0.5589113  0.4535951   1.232  0.22033    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3861 on 118 degrees of freedom
    ## Multiple R-squared:  0.994,  Adjusted R-squared:  0.992 
    ## F-statistic: 479.9 on 41 and 118 DF,  p-value: < 2.2e-16

``` r
anova(m2)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Height
    ##                  Df  Sum Sq Mean Sq   F value Pr(>F)    
    ## Length1           1 1176.62 1176.62 7892.0267 <2e-16 ***
    ## Length2           1  685.32  685.32 4596.7029 <2e-16 ***
    ## Length3           1  600.33  600.33 4026.6530 <2e-16 ***
    ## Weight            1   98.69   98.69  661.9237 <2e-16 ***
    ## Width             1  203.69  203.69 1366.1938 <2e-16 ***
    ## Species           6  134.76   22.46  150.6455 <2e-16 ***
    ## Length1:Species   6   29.06    4.84   32.4852 <2e-16 ***
    ## Length2:Species   6    1.06    0.18    1.1852 0.3189    
    ## Length3:Species   6    1.55    0.26    1.7275 0.1205    
    ## Weight:Species    6    1.15    0.19    1.2865 0.2687    
    ## Width:Species     6    1.27    0.21    1.4142 0.2148    
    ## Residuals       118   17.59    0.15                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# model choice

``` r
anova(m1,m2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: Height ~ Length1 + Length2 + Length3 + Weight + Width + Species
    ## Model 2: Height ~ Length1 + Length2 + Length3 + Weight + Width + Species + 
    ##     Length1 * Species + Length2 * Species + Length3 * Species + 
    ##     Weight * Species + Width * Species
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1    148 51.673                                  
    ## 2    118 17.593 30    34.081 7.6197 3.049e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#p-value of the second final model is extremely small. Therefore, there is significant evidence that the interaction is significant in this model. Hence, we choose the model2
```

# centralized the variable with high vif

``` r
vif(m2)   
```

    ##                         GVIF Df GVIF^(1/(2*Df))
    ## Length1         1.247508e+04  1       111.69191
    ## Length2         1.796014e+04  1       134.01544
    ## Length3         1.277551e+04  1       113.02879
    ## Weight          1.670144e+02  1        12.92341
    ## Width           2.967425e+02  1        17.22622
    ## Species         5.084591e+17  6        29.88968
    ## Length1:Species 2.064480e+30  6       335.91874
    ## Length2:Species 1.085790e+30  6       318.40423
    ## Length3:Species 1.329637e+29  6       267.28695
    ## Weight:Species  5.263553e+13  6        13.91361
    ## Width:Species   1.137152e+16  6        21.77634

``` r
##center for Length1,Length2,Length3
fish_center <- cbind(Length1 = fish[,3]-mean(fish[,3]),Length2 = fish[,4]-mean(fish[,4]),Length3 = fish[,5]-mean(fish[,5]))

##new data with center of Length1,Length2,Length3
fish_new <- cbind(fish[,-c(3,4,5)],fish_center)
```

# m2 remake the model 2 with modified data

``` r
m2_mod <- lm(Height ~ Length1 + Length2 + Length3 + Weight + Width + Species + Length1 * Species + Length2 * Species + Length3 * Species + Weight * Species + Width *Species  ,data = fish_new)
summary(m2_mod)
```

    ## 
    ## Call:
    ## lm(formula = Height ~ Length1 + Length2 + Length3 + Weight + 
    ##     Width + Species + Length1 * Species + Length2 * Species + 
    ##     Length3 * Species + Weight * Species + Width * Species, data = fish_new)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.9371 -0.1739  0.0000  0.1836  0.8275 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               9.7619684  1.2130137   8.048 7.47e-13 ***
    ## Length1                  -0.6350031  0.3404711  -1.865 0.064655 .  
    ## Length2                   0.3510791  0.3812468   0.921 0.358997    
    ## Length3                   0.4296143  0.2964506   1.449 0.149936    
    ## Weight                    0.0048081  0.0011048   4.352 2.89e-05 ***
    ## Width                     0.0540178  0.3108057   0.174 0.862321    
    ## SpeciesParkki             1.2300676  7.6171908   0.161 0.871987    
    ## SpeciesPerch             -4.9124323  1.4250238  -3.447 0.000786 ***
    ## SpeciesPike              -9.3651397  1.7880716  -5.238 7.19e-07 ***
    ## SpeciesRoach             -4.4746254  2.6312742  -1.701 0.091661 .  
    ## SpeciesSmelt             -7.0364339  5.5568204  -1.266 0.207911    
    ## SpeciesWhitefish         -4.9540539  2.8461498  -1.741 0.084358 .  
    ## Length1:SpeciesParkki    -0.5449564  5.7476959  -0.095 0.924624    
    ## Length1:SpeciesPerch      0.6958125  0.4262414   1.632 0.105253    
    ## Length1:SpeciesPike       1.5014187  0.9815203   1.530 0.128771    
    ## Length1:SpeciesRoach      0.0868616  0.5778051   0.150 0.880761    
    ## Length1:SpeciesSmelt      0.7132029  1.5171580   0.470 0.639157    
    ## Length1:SpeciesWhitefish  2.8207841  7.9570427   0.355 0.723596    
    ## Length2:SpeciesParkki    -4.1544092  7.5565331  -0.550 0.583511    
    ## Length2:SpeciesPerch     -0.3462941  0.5477359  -0.632 0.528461    
    ## Length2:SpeciesPike      -0.4453847  0.9367831  -0.475 0.635352    
    ## Length2:SpeciesRoach     -0.0490672  0.6808080  -0.072 0.942667    
    ## Length2:SpeciesSmelt     -0.3379158  0.6002762  -0.563 0.574548    
    ## Length2:SpeciesWhitefish  0.5024883  5.3698721   0.094 0.925605    
    ## Length3:SpeciesParkki     4.3013111  3.9515521   1.089 0.278587    
    ## Length3:SpeciesPerch     -0.3396825  0.3992452  -0.851 0.396597    
    ## Length3:SpeciesPike      -1.2049972  0.4122801  -2.923 0.004160 ** 
    ## Length3:SpeciesRoach     -0.0423051  0.4413260  -0.096 0.923795    
    ## Length3:SpeciesSmelt     -0.4401571  1.1410410  -0.386 0.700376    
    ## Length3:SpeciesWhitefish -3.3549908  5.9852528  -0.561 0.576172    
    ## Weight:SpeciesParkki      0.0083245  0.0145170   0.573 0.567442    
    ## Weight:SpeciesPerch      -0.0034765  0.0012567  -2.766 0.006582 ** 
    ## Weight:SpeciesPike       -0.0024257  0.0017242  -1.407 0.162106    
    ## Weight:SpeciesRoach      -0.0055119  0.0029536  -1.866 0.064501 .  
    ## Weight:SpeciesSmelt       0.0516065  0.1268345   0.407 0.684833    
    ## Weight:SpeciesWhitefish   0.0006414  0.0094557   0.068 0.946030    
    ## Width:SpeciesParkki      -1.2269960  2.4211905  -0.507 0.613259    
    ## Width:SpeciesPerch        0.5082393  0.3445149   1.475 0.142814    
    ## Width:SpeciesPike         1.2694930  0.4623800   2.746 0.006987 ** 
    ## Width:SpeciesRoach        0.6835914  0.6130380   1.115 0.267079    
    ## Width:SpeciesSmelt       -0.0203541  1.1183303  -0.018 0.985510    
    ## Width:SpeciesWhitefish    0.5589113  0.4535951   1.232 0.220331    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3861 on 118 degrees of freedom
    ## Multiple R-squared:  0.994,  Adjusted R-squared:  0.992 
    ## F-statistic: 479.9 on 41 and 118 DF,  p-value: < 2.2e-16

``` r
vif(m2_mod)   ## same vif
```

    ##                         GVIF Df GVIF^(1/(2*Df))
    ## Length1         1.247508e+04  1       111.69191
    ## Length2         1.796014e+04  1       134.01544
    ## Length3         1.277551e+04  1       113.02879
    ## Weight          1.670144e+02  1        12.92341
    ## Width           2.967425e+02  1        17.22622
    ## Species         1.100297e+17  6        26.31030
    ## Length1:Species 1.095932e+26  6       147.90470
    ## Length2:Species 5.268248e+25  6       139.14647
    ## Length3:Species 4.476056e+24  6       113.30297
    ## Weight:Species  5.263553e+13  6        13.91361
    ## Width:Species   1.137152e+16  6        21.77634

``` r
par(mfrow= c(2,2))
plot(m2)
```

    ## Warning: not plotting observations with leverage one:
    ##   56, 57, 58, 59, 60, 61
    
    ## Warning: not plotting observations with leverage one:
    ##   56, 57, 58, 59, 60, 61

![](final_project_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# variable selection for model 2

``` r
# select model 'both'(stepwise regression)
step(m2, direction = 'both')
```

    ## Start:  AIC=-269.23
    ## Height ~ Length1 + Length2 + Length3 + Weight + Width + Species + 
    ##     Length1 * Species + Length2 * Species + Length3 * Species + 
    ##     Weight * Species + Width * Species
    ## 
    ##                   Df Sum of Sq    RSS     AIC
    ## - Length2:Species  6   0.14119 17.734 -279.95
    ## - Length1:Species  6   0.70363 18.296 -274.96
    ## - Width:Species    6   1.26507 18.858 -270.12
    ## <none>                         17.593 -269.23
    ## - Weight:Species   6   1.43702 19.030 -268.67
    ## - Length3:Species  6   1.88083 19.473 -264.98
    ## 
    ## Step:  AIC=-279.95
    ## Height ~ Length1 + Length2 + Length3 + Weight + Width + Species + 
    ##     Length1:Species + Length3:Species + Weight:Species + Width:Species
    ## 
    ##                   Df Sum of Sq    RSS     AIC
    ## - Length2          1   0.07158 17.805 -281.31
    ## <none>                         17.734 -279.95
    ## - Width:Species    6   1.54194 19.276 -278.61
    ## - Weight:Species   6   1.57419 19.308 -278.35
    ## - Length1:Species  6   1.84468 19.578 -276.12
    ## - Length3:Species  6   2.18655 19.920 -273.35
    ## + Length2:Species  6   0.14119 17.593 -269.23
    ## 
    ## Step:  AIC=-281.31
    ## Height ~ Length1 + Length3 + Weight + Width + Species + Length1:Species + 
    ##     Length3:Species + Weight:Species + Width:Species
    ## 
    ##                   Df Sum of Sq    RSS     AIC
    ## <none>                         17.805 -281.31
    ## - Width:Species    6   1.53882 19.344 -280.05
    ## + Length2          1   0.07158 17.734 -279.95
    ## - Weight:Species   6   1.63594 19.441 -279.24
    ## - Length1:Species  6   2.11088 19.916 -275.38
    ## - Length3:Species  6   2.43175 20.237 -272.82

    ## 
    ## Call:
    ## lm(formula = Height ~ Length1 + Length3 + Weight + Width + Species + 
    ##     Length1:Species + Length3:Species + Weight:Species + Width:Species, 
    ##     data = fish)
    ## 
    ## Coefficients:
    ##              (Intercept)                   Length1                   Length3  
    ##                2.8390447                -0.4329889                 0.5679984  
    ##                   Weight                     Width             SpeciesParkki  
    ##                0.0049808                 0.1112336                -0.0698559  
    ##             SpeciesPerch               SpeciesPike              SpeciesRoach  
    ##               -2.5113330                 1.6066930                -3.7564897  
    ##             SpeciesSmelt          SpeciesWhitefish     Length1:SpeciesParkki  
    ##               -2.2222480                13.3778200                -3.1101118  
    ##     Length1:SpeciesPerch       Length1:SpeciesPike      Length1:SpeciesRoach  
    ##                0.4960196                 1.2022204                 0.0808139  
    ##     Length1:SpeciesSmelt  Length1:SpeciesWhitefish     Length3:SpeciesParkki  
    ##                0.5315168                 3.4220296                 2.7598087  
    ##     Length3:SpeciesPerch       Length3:SpeciesPike      Length3:SpeciesRoach  
    ##               -0.4755035                -1.3417852                -0.0810769  
    ##     Length3:SpeciesSmelt  Length3:SpeciesWhitefish      Weight:SpeciesParkki  
    ##               -0.5835065                -3.4448816                 0.0101556  
    ##      Weight:SpeciesPerch        Weight:SpeciesPike       Weight:SpeciesRoach  
    ##               -0.0036497                -0.0026670                -0.0057025  
    ##      Weight:SpeciesSmelt   Weight:SpeciesWhitefish       Width:SpeciesParkki  
    ##                0.0506081                 0.0008167                -1.8627857  
    ##       Width:SpeciesPerch         Width:SpeciesPike        Width:SpeciesRoach  
    ##                0.4509554                 1.1918384                 0.6635906  
    ##       Width:SpeciesSmelt    Width:SpeciesWhitefish  
    ##               -0.0611935                 0.5049614

``` r
m2_new <- lm(formula = Height ~ Length1 + Length3 + Weight + Width + Species + Length1:Species + Length3:Species + Weight:Species + Width:Species, data = fish_new)
summary(m2_new)
```

    ## 
    ## Call:
    ## lm(formula = Height ~ Length1 + Length3 + Weight + Width + Species + 
    ##     Length1:Species + Length3:Species + Weight:Species + Width:Species, 
    ##     data = fish_new)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.9456 -0.1680  0.0009  0.1831  0.8275 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               9.1856824  1.0156923   9.044 2.43e-15 ***
    ## Length1                  -0.4329889  0.2545065  -1.701 0.091375 .  
    ## Length3                   0.5679984  0.2497780   2.274 0.024670 *  
    ## Weight                    0.0049808  0.0010642   4.680 7.34e-06 ***
    ## Width                     0.1112336  0.2976662   0.374 0.709271    
    ## SpeciesParkki             4.4560143  5.3525435   0.833 0.406712    
    ## SpeciesPerch             -4.3328021  1.2222135  -3.545 0.000553 ***
    ## SpeciesPike              -8.6960663  1.4153659  -6.144 9.91e-09 ***
    ## SpeciesRoach             -4.1654081  2.4501147  -1.700 0.091601 .  
    ## SpeciesSmelt             -6.4750810  5.3724391  -1.205 0.230387    
    ## SpeciesWhitefish         -4.3019958  2.6738070  -1.609 0.110152    
    ## Length1:SpeciesParkki    -3.1101118  3.2418035  -0.959 0.339221    
    ## Length1:SpeciesPerch      0.4960196  0.3094512   1.603 0.111480    
    ## Length1:SpeciesPike       1.2022204  0.3626636   3.315 0.001200 ** 
    ## Length1:SpeciesRoach      0.0808139  0.3808095   0.212 0.832284    
    ## Length1:SpeciesSmelt      0.5315168  1.2897011   0.412 0.680954    
    ## Length1:SpeciesWhitefish  3.4220296  6.0142027   0.569 0.570383    
    ## Length3:SpeciesParkki     2.7598087  2.7369509   1.008 0.315234    
    ## Length3:SpeciesPerch     -0.4755035  0.2971774  -1.600 0.112108    
    ## Length3:SpeciesPike      -1.3417852  0.3749942  -3.578 0.000493 ***
    ## Length3:SpeciesRoach     -0.0810769  0.3625491  -0.224 0.823410    
    ## Length3:SpeciesSmelt     -0.5835065  1.0923038  -0.534 0.594153    
    ## Length3:SpeciesWhitefish -3.4448816  5.8408993  -0.590 0.556399    
    ## Weight:SpeciesParkki      0.0101556  0.0136459   0.744 0.458138    
    ## Weight:SpeciesPerch      -0.0036497  0.0012140  -3.006 0.003198 ** 
    ## Weight:SpeciesPike       -0.0026670  0.0015610  -1.709 0.090022 .  
    ## Weight:SpeciesRoach      -0.0057025  0.0028810  -1.979 0.049974 *  
    ## Weight:SpeciesSmelt       0.0506081  0.1206719   0.419 0.675654    
    ## Weight:SpeciesWhitefish   0.0008167  0.0089907   0.091 0.927763    
    ## Width:SpeciesParkki      -1.8627857  2.0827500  -0.894 0.372833    
    ## Width:SpeciesPerch        0.4509554  0.3311808   1.362 0.175755    
    ## Width:SpeciesPike         1.1918384  0.4095306   2.910 0.004277 ** 
    ## Width:SpeciesRoach        0.6635906  0.5922474   1.120 0.264665    
    ## Width:SpeciesSmelt       -0.0611935  0.9345077  -0.065 0.947895    
    ## Width:SpeciesWhitefish    0.5049614  0.4387324   1.151 0.251947    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3774 on 125 degrees of freedom
    ## Multiple R-squared:  0.994,  Adjusted R-squared:  0.9923 
    ## F-statistic: 605.7 on 34 and 125 DF,  p-value: < 2.2e-16

``` r
# diagnostic plot 
par(mfrow= c(2,2))
plot(m2_new)
```

    ## Warning in sqrt(crit * p * (1 - hh)/hh): ²úÉúÁËNaNs
    
    ## Warning in sqrt(crit * p * (1 - hh)/hh): ²úÉúÁËNaNs

![](final_project_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

# m2 log transformation

``` r
m2_new_log <- lm(formula = log(Height) ~ Length1 + Length3 + Weight + Width + Species + Length1:Species + Length3:Species + Weight:Species + Width:Species, data = fish_new)
summary(m2_new_log)
```

    ## 
    ## Call:
    ## lm(formula = log(Height) ~ Length1 + Length3 + Weight + Width + 
    ##     Species + Length1:Species + Length3:Species + Weight:Species + 
    ##     Width:Species, data = fish_new)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.238607 -0.025787 -0.000327  0.026715  0.174085 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               2.349e+00  1.596e-01  14.721  < 2e-16 ***
    ## Length1                  -3.355e-02  3.999e-02  -0.839  0.40301    
    ## Length3                   4.449e-02  3.924e-02   1.134  0.25910    
    ## Weight                    3.006e-04  1.672e-04   1.797  0.07469 .  
    ## Width                    -1.071e-03  4.677e-02  -0.023  0.98176    
    ## SpeciesParkki             2.105e-01  8.410e-01   0.250  0.80272    
    ## SpeciesPerch             -5.468e-01  1.920e-01  -2.847  0.00515 ** 
    ## SpeciesPike              -1.256e+00  2.224e-01  -5.647 1.04e-07 ***
    ## SpeciesRoach             -6.130e-01  3.850e-01  -1.592  0.11383    
    ## SpeciesSmelt             -1.092e+00  8.441e-01  -1.293  0.19833    
    ## SpeciesWhitefish         -5.860e-01  4.201e-01  -1.395  0.16552    
    ## Length1:SpeciesParkki    -3.357e-01  5.093e-01  -0.659  0.51111    
    ## Length1:SpeciesPerch     -5.380e-03  4.862e-02  -0.111  0.91207    
    ## Length1:SpeciesPike       1.556e-01  5.698e-02   2.731  0.00724 ** 
    ## Length1:SpeciesRoach     -6.076e-03  5.983e-02  -0.102  0.91928    
    ## Length1:SpeciesSmelt      1.147e-01  2.026e-01   0.566  0.57247    
    ## Length1:SpeciesWhitefish  4.139e-01  9.449e-01   0.438  0.66209    
    ## Length3:SpeciesParkki     3.091e-01  4.300e-01   0.719  0.47363    
    ## Length3:SpeciesPerch      3.156e-02  4.669e-02   0.676  0.50038    
    ## Length3:SpeciesPike      -1.616e-01  5.892e-02  -2.744  0.00697 ** 
    ## Length3:SpeciesRoach      2.561e-02  5.696e-02   0.450  0.65374    
    ## Length3:SpeciesSmelt     -7.186e-02  1.716e-01  -0.419  0.67615    
    ## Length3:SpeciesWhitefish -4.084e-01  9.177e-01  -0.445  0.65709    
    ## Weight:SpeciesParkki      3.332e-04  2.144e-03   0.155  0.87674    
    ## Weight:SpeciesPerch      -9.318e-04  1.908e-04  -4.885 3.10e-06 ***
    ## Weight:SpeciesPike       -6.064e-05  2.453e-04  -0.247  0.80511    
    ## Weight:SpeciesRoach      -8.811e-04  4.527e-04  -1.947  0.05383 .  
    ## Weight:SpeciesSmelt       2.027e-02  1.896e-02   1.069  0.28708    
    ## Weight:SpeciesWhitefish   2.482e-04  1.413e-03   0.176  0.86079    
    ## Width:SpeciesParkki      -8.575e-02  3.272e-01  -0.262  0.79372    
    ## Width:SpeciesPerch        1.131e-01  5.203e-02   2.174  0.03156 *  
    ## Width:SpeciesPike         1.636e-01  6.434e-02   2.542  0.01224 *  
    ## Width:SpeciesRoach        1.237e-01  9.305e-02   1.330  0.18610    
    ## Width:SpeciesSmelt        7.723e-03  1.468e-01   0.053  0.95814    
    ## Width:SpeciesWhitefish    7.343e-02  6.893e-02   1.065  0.28882    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0593 on 125 degrees of freedom
    ## Multiple R-squared:  0.9915, Adjusted R-squared:  0.9892 
    ## F-statistic: 431.1 on 34 and 125 DF,  p-value: < 2.2e-16

``` r
# diagnostic plot 
par(mfrow= c(2,2))
plot(m2_new_log)
```

    ## Warning in sqrt(crit * p * (1 - hh)/hh): ²úÉúÁËNaNs
    
    ## Warning in sqrt(crit * p * (1 - hh)/hh): ²úÉúÁËNaNs

![](final_project_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# m2 sqrt transformation

``` r
m2_new_sqrt <- lm(formula = sqrt(Height) ~ Length1 + Length3 + Weight + Width + Species + Length1:Species + Length3:Species + Weight:Species + Width:Species, data = fish_new)
summary(m2_new_sqrt)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(Height) ~ Length1 + Length3 + Weight + Width + 
    ##     Species + Length1:Species + Length3:Species + Weight:Species + 
    ##     Width:Species, data = fish_new)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.19293 -0.03066 -0.00114  0.03244  0.19189 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               3.1516766  0.1827855  17.242  < 2e-16 ***
    ## Length1                  -0.0604867  0.0458014  -1.321 0.189037    
    ## Length3                   0.0797628  0.0449504   1.774 0.078421 .  
    ## Weight                    0.0006116  0.0001915   3.193 0.001780 ** 
    ## Width                     0.0059917  0.0535684   0.112 0.911120    
    ## SpeciesParkki             0.5097860  0.9632515   0.529 0.597581    
    ## SpeciesPerch             -0.8197133  0.2199513  -3.727 0.000292 ***
    ## SpeciesPike              -1.6786645  0.2547113  -6.590  1.1e-09 ***
    ## SpeciesRoach             -0.8322741  0.4409262  -1.888 0.061404 .  
    ## SpeciesSmelt             -1.4097205  0.9668320  -1.458 0.147326    
    ## SpeciesWhitefish         -0.8203850  0.4811822  -1.705 0.090690 .  
    ## Length1:SpeciesParkki    -0.5136277  0.5833997  -0.880 0.380330    
    ## Length1:SpeciesPerch      0.0494328  0.0556893   0.888 0.376432    
    ## Length1:SpeciesPike       0.2140264  0.0652655   3.279 0.001348 ** 
    ## Length1:SpeciesRoach      0.0002629  0.0685310   0.004 0.996946    
    ## Length1:SpeciesSmelt      0.1077012  0.2320965   0.464 0.643429    
    ## Length1:SpeciesWhitefish  0.5943708  1.0823247   0.549 0.583874    
    ## Length3:SpeciesParkki     0.4642417  0.4925457   0.943 0.347737    
    ## Length3:SpeciesPerch     -0.0302125  0.0534805  -0.565 0.573137    
    ## Length3:SpeciesPike      -0.2303602  0.0674845  -3.414 0.000865 ***
    ## Length3:SpeciesRoach      0.0128315  0.0652449   0.197 0.844408    
    ## Length3:SpeciesSmelt     -0.0930750  0.1965726  -0.473 0.636690    
    ## Length3:SpeciesWhitefish -0.5919312  1.0511367  -0.563 0.574352    
    ## Weight:SpeciesParkki      0.0011388  0.0024557   0.464 0.643651    
    ## Weight:SpeciesPerch      -0.0008691  0.0002185  -3.978 0.000117 ***
    ## Weight:SpeciesPike       -0.0002366  0.0002809  -0.842 0.401335    
    ## Weight:SpeciesRoach      -0.0010488  0.0005185  -2.023 0.045220 *  
    ## Weight:SpeciesSmelt       0.0163800  0.0217163   0.754 0.452104    
    ## Weight:SpeciesWhitefish   0.0002786  0.0016180   0.172 0.863558    
    ## Width:SpeciesParkki      -0.2191226  0.3748147  -0.585 0.559860    
    ## Width:SpeciesPerch        0.1193281  0.0595998   2.002 0.047432 *  
    ## Width:SpeciesPike         0.2230943  0.0736997   3.027 0.003000 ** 
    ## Width:SpeciesRoach        0.1472318  0.1065817   1.381 0.169621    
    ## Width:SpeciesSmelt        0.0043668  0.1681754   0.026 0.979326    
    ## Width:SpeciesWhitefish    0.0999413  0.0789549   1.266 0.207938    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06792 on 125 degrees of freedom
    ## Multiple R-squared:  0.9935, Adjusted R-squared:  0.9918 
    ## F-statistic: 565.3 on 34 and 125 DF,  p-value: < 2.2e-16

``` r
# diagnostic plot 
par(mfrow= c(2,2))
plot(m2_new_sqrt)
```

    ## Warning in sqrt(crit * p * (1 - hh)/hh): ²úÉúÁËNaNs
    
    ## Warning in sqrt(crit * p * (1 - hh)/hh): ²úÉúÁËNaNs

![](final_project_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

# model 2 (log transformation) with remove outlier

``` r
fish_infremove <- fish_new

m2_new_log2 <- lm(formula = log(Height) ~ Length1 + Length3 + Weight + Width + Species + Length1:Species + Length3:Species + Weight:Species + Width:Species, data = fish_infremove)
summary(m2_new_log2)
```

    ## 
    ## Call:
    ## lm(formula = log(Height) ~ Length1 + Length3 + Weight + Width + 
    ##     Species + Length1:Species + Length3:Species + Weight:Species + 
    ##     Width:Species, data = fish_infremove)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.238607 -0.025787 -0.000327  0.026715  0.174085 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               2.349e+00  1.596e-01  14.721  < 2e-16 ***
    ## Length1                  -3.355e-02  3.999e-02  -0.839  0.40301    
    ## Length3                   4.449e-02  3.924e-02   1.134  0.25910    
    ## Weight                    3.006e-04  1.672e-04   1.797  0.07469 .  
    ## Width                    -1.071e-03  4.677e-02  -0.023  0.98176    
    ## SpeciesParkki             2.105e-01  8.410e-01   0.250  0.80272    
    ## SpeciesPerch             -5.468e-01  1.920e-01  -2.847  0.00515 ** 
    ## SpeciesPike              -1.256e+00  2.224e-01  -5.647 1.04e-07 ***
    ## SpeciesRoach             -6.130e-01  3.850e-01  -1.592  0.11383    
    ## SpeciesSmelt             -1.092e+00  8.441e-01  -1.293  0.19833    
    ## SpeciesWhitefish         -5.860e-01  4.201e-01  -1.395  0.16552    
    ## Length1:SpeciesParkki    -3.357e-01  5.093e-01  -0.659  0.51111    
    ## Length1:SpeciesPerch     -5.380e-03  4.862e-02  -0.111  0.91207    
    ## Length1:SpeciesPike       1.556e-01  5.698e-02   2.731  0.00724 ** 
    ## Length1:SpeciesRoach     -6.076e-03  5.983e-02  -0.102  0.91928    
    ## Length1:SpeciesSmelt      1.147e-01  2.026e-01   0.566  0.57247    
    ## Length1:SpeciesWhitefish  4.139e-01  9.449e-01   0.438  0.66209    
    ## Length3:SpeciesParkki     3.091e-01  4.300e-01   0.719  0.47363    
    ## Length3:SpeciesPerch      3.156e-02  4.669e-02   0.676  0.50038    
    ## Length3:SpeciesPike      -1.616e-01  5.892e-02  -2.744  0.00697 ** 
    ## Length3:SpeciesRoach      2.561e-02  5.696e-02   0.450  0.65374    
    ## Length3:SpeciesSmelt     -7.186e-02  1.716e-01  -0.419  0.67615    
    ## Length3:SpeciesWhitefish -4.084e-01  9.177e-01  -0.445  0.65709    
    ## Weight:SpeciesParkki      3.332e-04  2.144e-03   0.155  0.87674    
    ## Weight:SpeciesPerch      -9.318e-04  1.908e-04  -4.885 3.10e-06 ***
    ## Weight:SpeciesPike       -6.064e-05  2.453e-04  -0.247  0.80511    
    ## Weight:SpeciesRoach      -8.811e-04  4.527e-04  -1.947  0.05383 .  
    ## Weight:SpeciesSmelt       2.027e-02  1.896e-02   1.069  0.28708    
    ## Weight:SpeciesWhitefish   2.482e-04  1.413e-03   0.176  0.86079    
    ## Width:SpeciesParkki      -8.575e-02  3.272e-01  -0.262  0.79372    
    ## Width:SpeciesPerch        1.131e-01  5.203e-02   2.174  0.03156 *  
    ## Width:SpeciesPike         1.636e-01  6.434e-02   2.542  0.01224 *  
    ## Width:SpeciesRoach        1.237e-01  9.305e-02   1.330  0.18610    
    ## Width:SpeciesSmelt        7.723e-03  1.468e-01   0.053  0.95814    
    ## Width:SpeciesWhitefish    7.343e-02  6.893e-02   1.065  0.28882    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0593 on 125 degrees of freedom
    ## Multiple R-squared:  0.9915, Adjusted R-squared:  0.9892 
    ## F-statistic: 431.1 on 34 and 125 DF,  p-value: < 2.2e-16

``` r
# diagnostic plot 
par(mfrow= c(2,2))
plot(m2_new_log2)
```

    ## Warning in sqrt(crit * p * (1 - hh)/hh): ²úÉúÁËNaNs
    
    ## Warning in sqrt(crit * p * (1 - hh)/hh): ²úÉúÁËNaNs

![](final_project_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# find the outlier by studentized residuals
plot(studres(m2_new_log2),main = 'Studentized residuals', ylab = 'residuals')
mean(studres(m2_new_log2))
```

    ## [1] -0.007062457

``` r
which(abs(studres(m2_new_log2))>3)   # 73 97 95 are outliers
```

    ## 73 97 
    ## 73 97

![](final_project_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

# model 2 robust regression with romoved outliers and log transformation

``` r
rr.m2_new_log2 <- rlm(formula = log(Height) ~ Length1 + Length3 + Weight + Width + Species + Length1:Species + Length3:Species + Weight:Species + Width:Species, data = fish_infremove, psi = psi.huber)

summary(rr.m2_new_log2)
```

    ## 
    ## Call: rlm(formula = log(Height) ~ Length1 + Length3 + Weight + Width + 
    ##     Species + Length1:Species + Length3:Species + Weight:Species + 
    ##     Width:Species, data = fish_infremove, psi = psi.huber)
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.3403479 -0.0251837 -0.0006914  0.0264659  0.1738283 
    ## 
    ## Coefficients:
    ##                          Value   Std. Error t value
    ## (Intercept)               2.3376  0.1243    18.8099
    ## Length1                  -0.0320  0.0311    -1.0265
    ## Length3                   0.0425  0.0306     1.3900
    ## Weight                    0.0003  0.0001     2.2847
    ## Width                     0.0030  0.0364     0.0820
    ## SpeciesParkki             0.1724  0.6549     0.2632
    ## SpeciesPerch             -0.6870  0.1495    -4.5940
    ## SpeciesPike              -1.2657  0.1732    -7.3087
    ## SpeciesRoach             -0.6253  0.2998    -2.0858
    ## SpeciesSmelt             -0.9918  0.6573    -1.5089
    ## SpeciesWhitefish         -0.5743  0.3272    -1.7556
    ## Length1:SpeciesParkki    -0.3280  0.3966    -0.8270
    ## Length1:SpeciesPerch      0.0239  0.0379     0.6303
    ## Length1:SpeciesPike       0.1557  0.0444     3.5097
    ## Length1:SpeciesRoach     -0.0013  0.0466    -0.0289
    ## Length1:SpeciesSmelt      0.1266  0.1578     0.8026
    ## Length1:SpeciesWhitefish  0.4124  0.7359     0.5604
    ## Length3:SpeciesParkki     0.3035  0.3349     0.9062
    ## Length3:SpeciesPerch     -0.0029  0.0364    -0.0785
    ## Length3:SpeciesPike      -0.1630  0.0459    -3.5520
    ## Length3:SpeciesRoach      0.0222  0.0444     0.5016
    ## Length3:SpeciesSmelt     -0.0774  0.1336    -0.5790
    ## Length3:SpeciesWhitefish -0.4064  0.7147    -0.5686
    ## Weight:SpeciesParkki      0.0001  0.0017     0.0597
    ## Weight:SpeciesPerch      -0.0008  0.0001    -5.1875
    ## Weight:SpeciesPike        0.0000  0.0002    -0.1710
    ## Weight:SpeciesRoach      -0.0010  0.0004    -2.9695
    ## Weight:SpeciesSmelt       0.0181  0.0148     1.2276
    ## Weight:SpeciesWhitefish   0.0003  0.0011     0.2284
    ## Width:SpeciesParkki      -0.0610  0.2548    -0.2394
    ## Width:SpeciesPerch        0.1199  0.0405     2.9587
    ## Width:SpeciesPike         0.1659  0.0501     3.3104
    ## Width:SpeciesRoach        0.1337  0.0725     1.8457
    ## Width:SpeciesSmelt        0.0075  0.1143     0.0659
    ## Width:SpeciesWhitefish    0.0694  0.0537     1.2923
    ## 
    ## Residual standard error: 0.03923 on 125 degrees of freedom

``` r
# diagnostic plot 
par(mfrow= c(2,2))
plot(rr.m2_new_log2)
```

    ## Warning in sqrt(crit * p * (1 - hh)/hh): ²úÉúÁËNaNs
    
    ## Warning in sqrt(crit * p * (1 - hh)/hh): ²úÉúÁËNaNs

![](final_project_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
rr.m2_new_log2$w
```

    ##   [1] 1.0000000 1.0000000 1.0000000 1.0000000 0.7403203 1.0000000 1.0000000
    ##   [8] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [15] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [22] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [29] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [36] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [43] 1.0000000 0.4014520 0.9447953 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [50] 1.0000000 1.0000000 1.0000000 0.4208085 1.0000000 1.0000000 1.0000000
    ##  [57] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [64] 1.0000000 1.0000000 1.0000000 0.7771607 1.0000000 1.0000000 1.0000000
    ##  [71] 1.0000000 1.0000000 0.1550470 0.6528681 0.5821569 1.0000000 1.0000000
    ##  [78] 1.0000000 1.0000000 0.5210396 1.0000000 1.0000000 1.0000000 0.9031758
    ##  [85] 1.0000000 0.5068267 1.0000000 1.0000000 1.0000000 1.0000000 0.4089174
    ##  [92] 1.0000000 0.4166829 1.0000000 0.3450644 1.0000000 0.3035421 1.0000000
    ##  [99] 1.0000000 1.0000000 0.9441599 1.0000000 0.7905309 1.0000000 1.0000000
    ## [106] 1.0000000 0.7188990 1.0000000 0.8574856 0.6658661 1.0000000 1.0000000
    ## [113] 1.0000000 1.0000000 1.0000000 1.0000000 0.8880230 1.0000000 1.0000000
    ## [120] 1.0000000 1.0000000 0.7632625 1.0000000 0.3647147 1.0000000 1.0000000
    ## [127] 0.4384405 0.6688139 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ## [134] 1.0000000 0.6410229 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ## [141] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ## [148] 0.5214814 0.7365571 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ## [155] 0.6668792 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000

``` r
which(rr.m2_new_log2$w <0.4)   # 44, 53, 73, 74, 90, 92, 121, 124 is problemic observation, which correspond to 44, 53, 74, 75, 91, 93, 124,127
```

    ## [1]  73  95  97 124

# new data without outliers

``` r
fish_infremove2 <- fish_new[c(-73,-97,-95,-44, -53, -74, -75, -91, -93, -124,-127),]
m2_new_log3 <- lm(formula = log(Height) ~ Length1 + Length3 + Weight + Width + Species + Length1:Species + Length3:Species + Weight:Species + Width:Species, data = fish_infremove2)
summary(m2_new_log3)
```

    ## 
    ## Call:
    ## lm(formula = log(Height) ~ Length1 + Length3 + Weight + Width + 
    ##     Species + Length1:Species + Length3:Species + Weight:Species + 
    ##     Width:Species, data = fish_infremove2)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.094233 -0.022239 -0.001038  0.023741  0.093894 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               2.349e+00  1.042e-01  22.544  < 2e-16 ***
    ## Length1                  -3.355e-02  2.611e-02  -1.285 0.201381    
    ## Length3                   4.449e-02  2.563e-02   1.736 0.085242 .  
    ## Weight                    3.006e-04  1.092e-04   2.753 0.006881 ** 
    ## Width                    -1.071e-03  3.054e-02  -0.035 0.972075    
    ## SpeciesParkki             2.105e-01  5.491e-01   0.383 0.702136    
    ## SpeciesPerch             -7.966e-01  1.289e-01  -6.181 1.01e-08 ***
    ## SpeciesPike              -1.256e+00  1.452e-01  -8.648 3.81e-14 ***
    ## SpeciesRoach             -6.401e-01  2.576e-01  -2.485 0.014402 *  
    ## SpeciesSmelt             -1.092e+00  5.512e-01  -1.980 0.050067 .  
    ## SpeciesWhitefish         -5.860e-01  2.743e-01  -2.136 0.034802 *  
    ## Length1:SpeciesParkki    -3.357e-01  3.326e-01  -1.009 0.315011    
    ## Length1:SpeciesPerch      4.680e-02  3.319e-02   1.410 0.161300    
    ## Length1:SpeciesPike       1.556e-01  3.721e-02   4.182 5.70e-05 ***
    ## Length1:SpeciesRoach      7.525e-03  3.936e-02   0.191 0.848734    
    ## Length1:SpeciesSmelt      1.147e-01  1.323e-01   0.867 0.387951    
    ## Length1:SpeciesWhitefish  4.139e-01  6.170e-01   0.671 0.503662    
    ## Length3:SpeciesParkki     3.091e-01  2.808e-01   1.101 0.273333    
    ## Length3:SpeciesPerch     -3.121e-02  3.214e-02  -0.971 0.333596    
    ## Length3:SpeciesPike      -1.616e-01  3.847e-02  -4.202 5.29e-05 ***
    ## Length3:SpeciesRoach      1.460e-02  3.728e-02   0.391 0.696186    
    ## Length3:SpeciesSmelt     -7.186e-02  1.121e-01  -0.641 0.522665    
    ## Length3:SpeciesWhitefish -4.084e-01  5.992e-01  -0.681 0.496954    
    ## Weight:SpeciesParkki      3.332e-04  1.400e-03   0.238 0.812295    
    ## Weight:SpeciesPerch      -6.385e-04  1.450e-04  -4.402 2.43e-05 ***
    ## Weight:SpeciesPike       -6.064e-05  1.601e-04  -0.379 0.705639    
    ## Weight:SpeciesRoach      -1.192e-03  3.078e-04  -3.871 0.000181 ***
    ## Weight:SpeciesSmelt       2.027e-02  1.238e-02   1.637 0.104331    
    ## Weight:SpeciesWhitefish   2.482e-04  9.224e-04   0.269 0.788323    
    ## Width:SpeciesParkki      -8.575e-02  2.137e-01  -0.401 0.688958    
    ## Width:SpeciesPerch        1.290e-01  3.537e-02   3.648 0.000400 ***
    ## Width:SpeciesPike         1.636e-01  4.202e-02   3.893 0.000167 ***
    ## Width:SpeciesRoach        1.459e-01  6.173e-02   2.364 0.019775 *  
    ## Width:SpeciesSmelt        7.723e-03  9.588e-02   0.081 0.935940    
    ## Width:SpeciesWhitefish    7.343e-02  4.501e-02   1.631 0.105575    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.03872 on 114 degrees of freedom
    ## Multiple R-squared:  0.9965, Adjusted R-squared:  0.9954 
    ## F-statistic: 942.7 on 34 and 114 DF,  p-value: < 2.2e-16

``` r
# diagnostic plot 
par(mfrow= c(2,2))
plot(m2_new_log3)
```

    ## Warning in sqrt(crit * p * (1 - hh)/hh): ²úÉúÁËNaNs
    
    ## Warning in sqrt(crit * p * (1 - hh)/hh): ²úÉúÁËNaNs

![](final_project_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
## find the outlier by studentized residuals
studres(m2_new_log3)
```

    ##           1           2           3           4           5           6 
    ## -0.65314180  0.44284848 -0.16169403 -0.29565297 -1.93627948 -0.18852110 
    ##           7           8           9          10          11          12 
    ##  0.80281999 -1.26236257  0.82423988  0.34742654  0.53051581  0.79846284 
    ##          13          14          15          16          17          18 
    ## -0.28759363  0.78372329  0.50129267  1.41116342 -0.89453888 -1.01952008 
    ##          19          20          21          22          23          24 
    ##  1.31088815 -1.18101495 -0.00196554  1.05489100  0.26128900 -1.15327799 
    ##          25          26          27          28          29          30 
    ##  0.24387603 -0.34479367  0.26384062  0.11704956 -0.40794539  1.07146850 
    ##          31          32          33          34          35          36 
    ## -0.21040190 -0.30842772  0.47886626  0.29588601 -1.41126381 -1.63195927 
    ##          37          38          39          40          41          42 
    ## -0.04631733  0.50495074 -1.25652650  1.22222870 -1.13440301  0.15804412 
    ##          43          45          46          47          48          49 
    ## -0.18471921  1.51877716  0.77125580  1.24359014 -0.90089573  0.89249464 
    ##          50          51          52          54          55          56 
    ## -0.16276319  0.59313676 -1.00652582 -0.33573434 -1.19079383  0.14973848 
    ##          57          58          59          60          61          62 
    ##  0.14973848 -0.14973848 -0.14973848  0.14973848 -0.14973848  0.97698888 
    ##          63          64          65          66          67          68 
    ## -1.40185050 -0.65204871  0.53415679  1.24024402 -2.12240251  0.78555890 
    ##          69          70          71          72          76          77 
    ##  1.49457439  0.50691013 -1.32655604  0.02062964 -0.45870856 -2.12129904 
    ##          78          79          80          81          82          83 
    ## -0.02834608  0.05010709  2.08039801 -0.75964050 -0.37029252 -0.59636798 
    ##          84          85          86          87          88          89 
    ##  1.34403599 -0.84945319  2.56954895  1.18138680 -1.35702221  0.86558396 
    ##          90          92          94          96          98          99 
    ## -0.13013898 -0.94866845 -0.53574243  0.19399375 -0.29832734 -0.07709057 
    ##         100         101         102         103         104         105 
    ##  0.05766820 -1.53964682  0.46980338  1.52206770 -0.28408110 -0.66989844 
    ##         106         107         108         109         110         111 
    ## -0.73906477  2.09474638 -0.41060054 -1.43801336  1.77582507 -0.08971278 
    ##         112         113         114         115         116         117 
    ##  0.28769387 -0.58454442 -0.44491658  1.18393958  0.08785246 -1.96354247 
    ##         118         119         120         121         122         123 
    ##  1.01000202  0.82390734  0.35607772 -0.15928281  1.66422327 -0.27902420 
    ##         125         126         128         129         130         131 
    ## -0.96795700  0.63273293 -1.81803330 -0.32518259  0.25617617  0.16221198 
    ##         132         133         134         135         136         137 
    ## -0.47902556  0.19321881 -0.28202459  2.26059142 -0.66231311  0.40712038 
    ##         138         139         140         141         142         143 
    ## -0.77042439  0.36677052 -0.95922624 -0.79995291  1.16688810 -0.12294529 
    ##         144         145         146         147         148         149 
    ##  0.28151401 -0.37006868 -1.43216349  0.99068254 -2.80236542  2.18836509 
    ##         150         151         152         153         154         155 
    ##  0.19524862 -0.74015998  0.97311369  1.06312999  0.87672589 -2.16646445 
    ##         156         157         158         159         160 
    ##  1.27520460 -1.14380020 -0.32078446 -0.46658156  0.79728826

``` r
plot(studres(m2_new_log3),main = 'Studentized residuals', ylab = 'residuals')

# histogram of studentized residuals
hist(studres(m2_new_log3),main = 'histogram of Studentized residuals',xlab = 'residuals')
mean(studres(m2_new_log3))
```

    ## [1] -0.007549379

``` r
which(abs(studres(m2_new_log3))>3) 
```

    ## named integer(0)

``` r
anova(m2_new_log3)
```

    ## Analysis of Variance Table
    ## 
    ## Response: log(Height)
    ##                  Df  Sum Sq Mean Sq    F value    Pr(>F)    
    ## Length1           1 21.7693 21.7693 14519.4242 < 2.2e-16 ***
    ## Length3           1 14.6477 14.6477  9769.5444 < 2.2e-16 ***
    ## Weight            1  0.0104  0.0104     6.9599 0.0095000 ** 
    ## Width             1  7.4687  7.4687  4981.3598 < 2.2e-16 ***
    ## Species           6  3.9284  0.6547   436.6806 < 2.2e-16 ***
    ## Length1:Species   6  0.1449  0.0241    16.1063 2.377e-13 ***
    ## Length3:Species   6  0.0191  0.0032     2.1217 0.0560683 .  
    ## Weight:Species    6  0.0371  0.0062     4.1185 0.0008847 ***
    ## Width:Species     6  0.0318  0.0053     3.5339 0.0030296 ** 
    ## Residuals       114  0.1709  0.0015                         
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](final_project_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

# multicollinearity for final model

``` r
vif(m2_new_log3)
```

    ##                         GVIF Df GVIF^(1/(2*Df))
    ## Length1         6.743672e+03  1        82.11986
    ## Length3         8.769305e+03  1        93.64457
    ## Weight          1.496073e+02  1        12.23141
    ## Width           2.593501e+02  1        16.10435
    ## Species         2.311278e+16  6        23.10225
    ## Length1:Species 2.913187e+23  6        90.23272
    ## Length3:Species 5.436983e+23  6        95.04877
    ## Weight:Species  4.102141e+13  6        13.62754
    ## Width:Species   4.276361e+15  6        20.07195
