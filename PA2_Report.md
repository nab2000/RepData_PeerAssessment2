Analysis of weather event data shows that Hurricanes and/or Typhoons have the greatest cost of property and crop damage associated with them and that Heat, Tornados, and Ice Storms have the highest human health costs
-----------------------------------------------------------------------------

## Synopsis
Storm data obtained from NOAA was analyzed to examine what storm events casued
the most proeprty and crop damage and had th greatest imapct on human health by
injuries or fatalities. The data was first processed to remove issues in the
names of the sotrm types and then converted into two data sets: one for damage
expenses and the other for human health impacts. In the damage data set, the
multipliers for cost (given as alphabetical symbols) were converted into
numeric quantities and applied to the cost. A column totalling the cost of both
property and crop damage was created and used to develop tables with relevant
summary statistics for each event type. Similarlly, the human health impacts
data set was manipulated to generate a column that quantitatively combined
death and injury, assuming injuries were 0.25 a costly as death, and converted
to a table with the relevant statistics by event type. Based on a review of the top 5 storm events by max, mean and upper 95% CI of the mean for damages one of the events was consistently in the top 5 for all categories and stood out in a review of the box and whisker plot. This event, Hurrican/Typhoon was chosen as the most likely to cause high damage costs. Because there was no single storm event that stood out above the rest in regards to human health, the 3 event types with the highest maximum human cost were selected. These events are Heat, Ice Storms, and Torandos.   



## Data Processing
The first step in our data analysis process was to set our libraries 

```r
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.2.2
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.2
```

```r
library(lattice)
opts_chunk$set(echo= TRUE)
options(scipen = 2)
```
Then we read in the data and removed summary, none, and ? envtypes as these 
appear to be data entry errors or, in the case of summary, consist of multiple 
envent types and will not be used in our analysis

```r
df <- read.csv("repdata_data_StormData.csv.bz2")
df <- df[!grepl("summary", df$EVTYP, ignore.case = TRUE),]
df <- df[!df$EVTYP == "?",]
df <- df[!grepl("none", df$EVTYPE, ignore.case = T),]
```
After this, we made two different subset data frames. One for our Human Health 
Impact and one for Cost Estimates. 

### Cost Estimate
Because the damage data has different multipliers we will actually use those 
multipliers to generate comparable numbers. The mutipliers in our data are h or 
H (hundreds), k or K (thousand), M (million), B (billion), or 0 (no multiplier). 
There are a few rows where the multiplier is not any of these 5 possiblities 
these observations were converted to NA. If both the Crop Damage and Property 
Damage values were NA, the observation was removed.

```r
multiplier <- function(data){
        if (data == "k"){data <- 10^3}
        else if (data == "K"){data <- 10^3}
        else if (data == "h"){data <- 10^2}
        else if (data == "H"){data <- 10^2}
        else if (data == "M"){data <- 10^6}
        else if (data == "m"){data <- 10^6}
        else if (data == "B"){10^9}
        else if (data == 0){data <- 1}
        else {data <- NA}
}
PROPDMGEXP <- sapply(df$PROPDMGEXP, multiplier)
CROPDMGEXP <- sapply(df$CROPDMGEXP, multiplier)
df_damage <- data.frame(EVTYPE = df$EVTYPE, 
CROPDMG = df$CROPDMG*CROPDMGEXP,
PROPDMG = df$PROPDMG*PROPDMGEXP)
for (i in 1:nrow(df_damage)){
    if(is.na(df_damage$PROPDMG[i]) & is.na(df_damage$CROPDMG[i])){
        df_damage$TOTALDMG[i] <- NA
    }
    else if (is.na(df_damage$PROPDMG[i])){
        df_damage$TOTALDMG[i] <- df_damage$CROPDMG[i]
    }
    else if (is.na(df_damage$CROPDMG[i])){
        df_damage$TOTALDMG[i] <- df_damage$PROPDMG[i]
    }
    else df_damage$TOTALDMG[i] <- df_damage$PROPDMG[i] + 
            df_damage$CROPDMG[i]
}
df_damage <- df_damage[!is.na(df_damage$TOTALDMG), ]
df_damage$EVTYPE <- factor(df_damage$EVTYPE)
```
Then we generated a table to look at the mean, upper and lower 95% CI of the 
mean and the max cost for each event type

```r
tbl_exp_mean <- sapply(split(df_damage$TOTALDMG, df_damage$EVTYP), mean)
tbl_exp_max <- sapply(split(df_damage$TOTALDMG, df_damage$EVTYP), max)
tbl_exp_sd <- sapply(split(df_damage$TOTALDMG, df_damage$EVTYP), sd)
tbl_exp_n <- sapply(split(df_damage$TOTALDMG, df_damage$EVTYP), length)
tbl_exp <- data.frame(lower = tbl_exp_mean - qt(.975, df = tbl_exp_n -1)*tbl_exp_sd/sqrt(tbl_exp_n), mean = tbl_exp_mean, upper = tbl_exp_mean + 
                          qt(.975, df = tbl_exp_n -1)*tbl_exp_sd/sqrt(tbl_exp_n), 
                      max = tbl_exp_max, n = tbl_exp_n)
```

```
## Warning in qt(0.975, df = tbl_exp_n - 1): NaNs produced
```

```
## Warning in qt(0.975, df = tbl_exp_n - 1): NaNs produced
```


### Human Health Estimate
Because the human health imapcts have both fatality and injury data but there is 
no quantification of the seriousness of the injuries, we will make a single 
metric giving the injuries a quarter of weight of fatalities. Then similar to 
the cost estimate, we calculated general statistics by event type

```r
df_health <- data.frame(EVTYP = as.character(df$EVTYPE), FATAL = df$FATALITIES, INJU = df$INJURIES, COMBINED = df$FATALITIES+0.25*df$INJURIES)
tbl_health_mean <- sapply(split(df_health$COMBINED, df_health$EVTYP), mean)
tbl_health_max <- sapply(split(df_health$COMBINED, df_health$EVTYP), max)
tbl_health_sd <- sapply(split(df_health$COMBINED, df_health$EVTYP), sd)
tbl_health_n <- sapply(split(df_health$COMBINED, df_health$EVTYP), length)
tbl_health <- data.frame(lower = tbl_health_mean - qt(.975, df = tbl_health_n -1)*tbl_health_sd/sqrt(tbl_health_n), mean = tbl_health_mean, upper = tbl_health_mean + qt(.975, df = tbl_health_n -1)*tbl_health_sd/sqrt(tbl_health_n), max = tbl_health_max, n = tbl_health_n)
```

```
## Warning in qt(0.975, df = tbl_health_n - 1): NaNs produced
```

```
## Warning in qt(0.975, df = tbl_health_n - 1): NaNs produced
```


## Results
### Cost Esimate
To visualize the cost of the storm types we looked at the storm events associated with the top 10 highest costs  based on mean, max, and upper 95% CI of the mean 

```r
tbl_exp <- tbl_exp[order(tbl_exp$max), ]
max_list <- tail(tbl_exp, n = 5)
max_list
```

```
##                        lower       mean      upper          max     n
## TROPICAL STORM      -3188527   14628685   32445898   5150000000   573
## RIVER FLOOD        -90421779   94844902  280111583  10000000000   107
## HURRICANE/TYPHOON  368958282 1027338754 1685719227  16930000000    70
## STORM SURGE       -126875422  247563091  622001604  31300000000   175
## FLOOD               -4312604    8604939   21522482 115032500000 17469
```

```r
tbl_exp <- tbl_exp[order(tbl_exp$mean), ]
mean_list <- tail(tbl_exp, n = 5)
mean_list
```

```
##                                 lower       mean      upper         max
## STORM SURGE                -126875422  247563091  622001604 31300000000
## HURRICANE OPAL             -245597055  398980750 1043558555  2105000000
## HURRICANE/TYPHOON           368958282 1027338754 1685719227 16930000000
## TORNADOES, TSTM WIND, HAIL        NaN 1602500000        NaN  1602500000
## HEAVY RAIN/SEVERE WEATHER         NaN 2500000000        NaN  2500000000
##                              n
## STORM SURGE                175
## HURRICANE OPAL               8
## HURRICANE/TYPHOON           70
## TORNADOES, TSTM WIND, HAIL   1
## HEAVY RAIN/SEVERE WEATHER    1
```

```r
tbl_exp_sub <- tbl_exp[!is.na(tbl_exp$upper), ]
tbl_exp_sub <- tbl_exp_sub[order(tbl_exp_sub$upper), ]
upper_list <- tail(tbl_exp_sub, n = 5)           
upper_list
```

```
##                          lower       mean      upper         max   n
## SEVERE THUNDERSTORM -246927215  172222857  591372929  1200000000   7
## STORM SURGE         -126875422  247563091  622001604 31300000000 175
## WILD FIRES          -335120062  156025000  647170062   619000000   4
## HURRICANE OPAL      -245597055  398980750 1043558555  2105000000   8
## HURRICANE/TYPHOON    368958282 1027338754 1685719227 16930000000  70
```

As we can see Hurricanes/Typhons cost the most on average (note: the top two 
means were single time events, thus their average is also their max and is 
evalauted there) with Sotrm Surge in the top 5. The top three most costly events 
ever were Hurricane/Typhoon, Storm Surge, and Floods. Looking at the upper 95% 
CI of the mean, we see that Hurricane/Typhon and Storm Surge are again in the 
top five. Thus  consistently the most epensive events are Hurricanes/Typhoons 
and Storm Surges.  

The data for the top 5  storm events for each of the three measures (mean, max, 
and upper) can be visualed in a box and whiskers plot


```r
top_5_list <- unique(c(rownames(max_list), rownames(mean_list), rownames(upper_list)))
sub<- df_damage[df_damage$EVTYPE == top_5_list[1], ]
for (i in 1:length(top_5_list)){
    sub <- rbind(sub, df_damage[df_damage$EVTYPE == top_5_list[i], ])
}
sub$EVTYPE <- factor(sub$EVTYPE)
g <- ggplot(data = sub, aes (x = EVTYPE, y = log(TOTALDMG)))
g <- g + geom_boxplot()
g <- g + ylab("log Total Damage ($)") + xlab("Event Type") + ggtitle("Top 10 Weather Events by Damage")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g
```

```
## Warning: Removed 7748 rows containing non-finite values (stat_boxplot).
```

![](PA2_Report_files/figure-html/unnamed-chunk-7-1.png) 

Consistent with our results this plot shows that Hurricane/Typhoons appear to 
cause the most damage. It also shows that Storm Surge damage appears to be 
similar with the other 8 storm types (note: Hurricane Opal is a specific 
Hurricane/Typhoon) 

###Human Health Estimate
Similar to what was done with the damage costs, we also looked at the top 5 
events based on mean, max, and 95% upper CI for our combined human health 
statistic


```r
tbl_health <- tbl_health[order(tbl_health$max), ]
max_list <- tail(tbl_health, n = 5)
max_list
```

```
##                         lower       mean     upper max     n
## FLOOD              0.05101463 0.08557411 0.1201336 202 25326
## HURRICANE/TYPHOON -0.60203742 4.34943182 9.3009011 202    88
## ICE STORM         -0.09473319 0.29050349 0.6757402 393  2006
## TORNADO            0.42618519 0.46939095 0.5125967 467 60652
## HEAT               0.35824938 1.90612777 3.4540062 583   767
```

```r
tbl_health <- tbl_health[order(tbl_health$mean), ]
mean_list <- tail(tbl_health, n = 5)
mean_list
```

```
##                                lower   mean    upper   max n
## WILD FIRES                 -22.09727 10.125 42.34727 40.50 4
## COLD AND SNOW                    NaN 14.000      NaN 14.00 1
## Heat Wave                        NaN 17.500      NaN 17.50 1
## TROPICAL STORM GORDON            NaN 18.750      NaN 18.75 1
## TORNADOES, TSTM WIND, HAIL       NaN 25.000      NaN 25.00 1
```

```r
tbl_health_sub <- tbl_health[!is.na(tbl_health$upper), ]
tbl_health_sub <- tbl_health_sub[order(tbl_health_sub$upper), ]
upper_list <- tail(tbl_health_sub, n = 5)           
upper_list
```

```
##                           lower      mean    upper  max n
## MARINE MISHAP         -19.69913  4.125000 27.94913  6.0 2
## RECORD/EXCESSIVE HEAT -18.71503  5.666667 30.04837 17.0 3
## HEAT WAVES            -29.26551  2.500000 34.26551  5.0 2
## WILD FIRES            -22.09727 10.125000 42.34727 40.5 4
## SNOW/HIGH WINDS       -46.32482  4.500000 55.32482  8.5 2
```
The results are a little less consistant than for damage costs, but heat waves 
and wild fires show up in 2 of the 3 categories (mean and upper CI) and heat 
(likely similar to heat waves) is in the top 5 for max human health loss. 
We can again visualize this to see if it makes things clearer.


```r
top_5_list <- unique(c(rownames(max_list), rownames(mean_list), rownames(upper_list)))
sub<- df_health[df_health$EVTYP == top_5_list[1], ]
for (i in 1:length(top_5_list)){
    sub <- rbind(sub, df_health[df_health$EVTYP == top_5_list[i], ])
}
sub$EVTYP <- factor(sub$EVTYP)
g <- ggplot(data = sub, aes (x = EVTYP, y = log(COMBINED)))
g <- g + geom_boxplot()
g <- g + ylab("log Total Damage ($)") + ggtitle("Top 14 Weather Events by Damage")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g
```

```
## Warning: Removed 105093 rows containing non-finite values (stat_boxplot).
```

![](PA2_Report_files/figure-html/unnamed-chunk-9-1.png) 

In looking at the plot we see that some of the event types have only one or
two observations making their mean higher than the mean of other events. They
all seem to be pretty similar, though heat, Tornado and ice storm seem to have
significantly higher single events, consistent with the max table. Due to the 
similarity of means for these events, we will utilize the max to select the three major event types (Heat, Tornado, Ice Storm) that have the highest human health cost.  
