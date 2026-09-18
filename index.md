Assignment 1 Data Exploration
================
Sydney Whitlock
2026-09-16

# Researchers examined mercury (Hg) and corticosterone (CORT) levels in feathers and compared them with birds’ body condition. Findings suggest that mercury exposure in the Amazon during winter may have carryover effects on Purple Martins after they return to North America, potentially affecting migration, survival, and population declines.

``` r
setwd("C:/Users/sgwhi/OneDrive/Biometry 2026/effects-of-mercury-contamination-on-purple-martins-main")

# Load packages
library(readxl)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Load data
my_data <- read.csv("FloridaWisconsinVirginia.csv") 

class(my_data)
```

    ## [1] "data.frame"

``` r
# Dropping Sample.ID and Band.ID from dataset

df_new <- my_data %>% select(Mass:Cort)

# Variables of interest: Mass, Sex, Hg, Blood.Hg, Cort
str(df_new)
```

    ## 'data.frame':    79 obs. of  5 variables:
    ##  $ Mass    : num  59 51 56 52 52 53 55 55 53 52 ...
    ##  $ Sex     : chr  "M" "M" "F" "M" ...
    ##  $ Hg      : num  5.86 1.57 2.56 3.4 2.59 ...
    ##  $ Blood.Hg: num  0.617 0.254 0.353 0.428 0.356 0.315 0.334 0.292 0.394 0.444 ...
    ##  $ Cort    : num  10.07 5.87 7.31 9.1 5.29 ...

``` r
# Cleanup
glimpse(df_new)
```

    ## Rows: 79
    ## Columns: 5
    ## $ Mass     <dbl> 59.0, 51.0, 56.0, 52.0, 52.0, 53.0, 55.0, 55.0, 53.0, 52.0, 5…
    ## $ Sex      <chr> "M", "M", "F", "M", "M", "F", "F", "F", "M", "F", "M", "M", "…
    ## $ Hg       <dbl> 5.863, 1.570, 2.556, 3.400, 2.594, 2.156, 2.354, 1.927, 3.013…
    ## $ Blood.Hg <dbl> 0.617, 0.254, 0.353, 0.428, 0.356, 0.315, 0.334, 0.292, 0.394…
    ## $ Cort     <dbl> 10.07, 5.87, 7.31, 9.10, 5.29, 7.09, 5.92, 8.08, 6.53, 3.53, …

``` r
#Summary Statistics
summary(df_new)
```

    ##       Mass              Sex           Hg           Blood.Hg     
    ##  Min.   :40.10   Length   :79   Min.   :1.103   Min.   :0.2000  
    ##  1st Qu.:49.60   N.unique : 2   1st Qu.:1.866   1st Qu.:0.2855  
    ##  Median :52.00   N.blank  : 0   Median :2.459   Median :0.3440  
    ##  Mean   :52.13   Min.nchar: 1   Mean   :2.808   Mean   :0.3676  
    ##  3rd Qu.:54.30   Max.nchar: 1   3rd Qu.:3.404   3rd Qu.:0.4280  
    ##  Max.   :62.10                  Max.   :8.740   Max.   :0.8070  
    ##       Cort        
    ##  Min.   :  3.530  
    ##  1st Qu.:  5.875  
    ##  Median :  7.570  
    ##  Mean   : 11.887  
    ##  3rd Qu.: 12.915  
    ##  Max.   :134.090

``` r
#Figure 1
data <- df_new[c("Mass","Hg","Blood.Hg","Cort")]
round(cor(data),2)
```

    ##           Mass    Hg Blood.Hg  Cort
    ## Mass      1.00 -0.16    -0.18 -0.09
    ## Hg       -0.16  1.00     0.99  0.20
    ## Blood.Hg -0.18  0.99     1.00  0.18
    ## Cort     -0.09  0.20     0.18  1.00

``` r
pairs(data,
      main = "Figure 1: Correlations between Key Variables",
      pch=16, 
      cex =0.3, 
      col=rgb(0,0,0,1))
```

![](index_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# Figure 1: Scatter-plot matrix. Each diagonal box contains the name of a variable, acting as the label for its corresponding row and column. "Mass" corresponds to body mass, "Hg" to mercury levels in feathers, "Blood.Hg" to blood mercury levels, and "Cort" to corticosterone levels.

# Figure 2
boxplot(Hg ~ State, data = my_data,
        xlab = "State",
        ylab = "Hg (ug/g)",
        main = "Figure 2: Mercury Levels by State",
        col = "lightgray",
        las = 1)
```

![](index_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
# Figure 2: Figure 2 shows a box-and-whisker plot comparing mercury (Hg) levels across three states (Florida, Virginia, Wisconsin).Median mercury levels are similar across all three states, sitting between 2.3 and 2.7 ug/g.

# Figure 3
male_data <- df_new %>% filter(Sex == "M")
hist(male_data$Hg,
main = "Figure 3: Mercury (Hg) content: Males",
xlab = "Hg (ug/g)",
col = "lightblue")
```

![](index_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
# Figure 3 is a histogram showing the distribution of mercury (Hg) content in the males sampled. The x-axis shows the mercury concentration measured in micro-grams per gram (ug/g). The y-axis shows the total count of individual males in each concentration range. 

#Figure 4
female_data <- df_new %>% filter(Sex == "F")
hist(female_data$Hg,
     main = "Figure 4: Mercury (Hg) content: Females",
     xlab = "Hg (ug/g)",
     col = "lightgreen")
```

![](index_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
# Figure 4 is a histogram showing the distribution of mercury (Hg) content in the females sampled. The x-axis shows the mercury concentration measured in micro-grams per gram (ug/g). The y-axis shows the total count of individual females in each concentration range. 
```
