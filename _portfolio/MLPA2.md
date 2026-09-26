MLPA_KELS
================
Seongmin Park
2025-09-13

# MLPA_KELS

## Data Preparation

Using data form KELS 2013 7th

``` r
library(plyr)
library(tidyverse)
library(MplusAutomation)
library(haven)

path <- setwd("/Users/seongminpark/Desktop/KEMS/25-2/KELS/Analysis")
```

Load your SPSS Data. Check your dataset already included in your
designated path; if not, just specify each path of your data.

``` r
stu_data <- read_spss("/Users/seongminpark/Desktop/KEMS/25-2/KELS/KELS2013_7차/1. L2Y7S_학생.sav")
sch_data <- read_spss("/Users/seongminpark/Desktop/KEMS/25-2/KELS/KELS2013_7차/5. L2Y7DB_학교DB.sav")
par_data <- read_spss("/Users/seongminpark/Desktop/KEMS/25-2/KELS/KELS2013_7차/3. L2Y7P_학부모.sav")
stuf_data <- read_spss("/Users/seongminpark/Desktop/KEMS/25-2/KELS/KELS2013_7차/2. L2Y7LT_학습자특성.sav")
```

Below is my personal data tidying function:

``` r
# Overall Missing Num Check
missing_num_check <- function(data) {
  colSums(is.na(data))
}

# Listwise Deletion
listwise_del <- function(data) {
  data %>%
  filter(complete.cases(.))
}

# NO haven
NO_haven <- function(data) {
  data %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))
}

# NA = -999 function
NA_to_999 <- function(data) {
  data[is.na(data)] <- -999
  return(data)
}
```

## Variable Selection

Starts from checking the most adequate class number. All missing values
are already assigned as NA.

At first, **you need to change the covariate lists for the better
explaining model.**

``` r
## Add covariates here

stu_cov_list <- c("L2GENDER")
sch_cov_list <- c("L2Y7_REG")
```

Below is the quick data tidying process. I just remove the row which
includes all missings for LPA indicators; also with SCHID.

``` r
# Variable Selection and excluding out the missing data
LPA_data <- stu_data %>% 
  select(L2SID, L2Y7_SCHID,
         starts_with("L2Y7S19"),
         all_of(stu_cov_list),
         all_of(sch_cov_list)) %>% 
  filter(!if_all(starts_with("L2Y7S19"), is.na)) %>% # n = 6,182
  filter(!is.na(L2Y7_SCHID)) # n = 6,148

# How many students and schools?
## School: n = 1,657, Student: n = 6,148. Mean cluster size = 3.71
nrow(LPA_data)
```

    ## [1] 6148

``` r
LPA_data %>%
  summarise(n_schools = n_distinct(L2Y7_SCHID))
```

    ## # A tibble: 1 × 1
    ##   n_schools
    ##       <int>
    ## 1      1657

``` r
# Remove haven label
LPA_data <- NO_haven(LPA_data) 

# NA to -999 for using data in mplus
LPA_data <- NA_to_999(LPA_data) 

# Write .dat file for mplus
write.table(LPA_data, "LPA_data.dat", sep = " ", col.names = F, row.names = F)
```

Data file needs to be cleaned more… since the list of variable is not
yet organized.

------------------------------------------------------------------------

## Latent Profile Analysis using MplusAutomation

Too annoying to run all classes separately; Just use MplusAutomation :D\
Revise the following codes for automatic mplus input files.

For running the model in this analysis, you only need to change
‘**outputDirectory**’ for your personal laptop, and **‘VARIABLE’,
‘USEVARIABLES’**, depending on your selected covariates.

Check your outputdirectory and covariate list (colnames):

``` r
# outputdirectory
getwd()
```

    ## [1] "/Users/seongminpark/Desktop/KEMS/25-2/KELS/Analysis"

``` r
# full variable list in order
## VARIABLE: NAMES ARE
cat(gsub('"', '', colnames(LPA_data)), sep = " ")
```

    ## L2SID L2Y7_SCHID L2Y7S1901 L2Y7S1902 L2Y7S1903 L2Y7S1904 L2Y7S1905 L2Y7S1906 L2Y7S1907 L2Y7S1908 L2GENDER L2Y7_REG

``` r
# After deciding the number of classes, you need to add covariates syntax in variable section, which indicates the within and between level covariates. Just copy the code result of below syntax:
cat("within =", gsub('"', '', stu_cov_list), collapse = "")
```

    ## within = L2GENDER

``` r
cat("between =", gsub('"', '', sch_cov_list), collapse = "")
```

    ## between = L2Y7_REG

## 1. Single-level LPA

    [[init]]
    iterators = classes;
    classes = 1:6;
    outputDirectory = "/Users/seongminpark/Desktop/KEMS/25-2/KELS/Analysis";
    filename = "LPA_Analysis_class_[[classes]].inp";
    [[/init]]

    TITLE: SLPA_Analysis_class_[[classes]]
    DATA: FILE IS LPA_data.dat;

    VARIABLE: NAMES ARE 
    SID SCHID ex1-ex4 ac1-ac4
    ST_SEX !within covariates
    SC_REG; !between covariates
    USEVARIABLES = ex1-ex4 ac1-ac4;
    CLASSES = c([[classes]]);
    MISSING = all(-999);

    ANALYSIS: TYPE = MIXTURE;
    STARTS = 100 20;

    OUTPUT: TECH11 TECH14;

``` r
# simple copy & paste of above text
## [init] section is for building multiple mplus input syntax simultaneously.

SLPA_text <- '
[[init]]
iterators = classes;
classes = 1:6;
outputDirectory = "/Users/seongminpark/Desktop/KEMS/25-2/KELS/Analysis";
filename = "SLPA_Analysis_class_[[classes]].inp";
[[/init]]

TITLE: SLPA_Analysis_class_[[classes]]
DATA: FILE IS LPA_data.dat;

VARIABLE: NAMES ARE 
SID SCHID ex1-ex4 ac1-ac4
ST_SEX !within covariates
SC_REG; !between covariates
USEVARIABLES = ex1-ex4 ac1-ac4;
CLASSES = c([[classes]]);
MISSING = all(-999);

ANALYSIS: TYPE = MIXTURE;
STARTS = 100 20;

OUTPUT: TECH11 TECH14;
'

writeLines(SLPA_text, "SLPA_text.txt")
```

Now, use `createModels()` to make multiple input files:

``` r
createModels("SLPA_text.txt")
```

Next, run all models in following directory: since I already attached
the file directory by `setwd()`, simply running the following function
still works:

``` r
# Use this method, since filefilter cannot recognize the regex ;
# SLPA_file_list <- list.files(".", pattern = "^SLPA.*.inp$")
# runModels(SLPA_file_list)

# Run simultaneously
runModels(filefilter = "SLPA")
```

And… readmodels.

``` r
SLPA_output_list <- readModels(filefilter = "SLPA")
SLPA_summaryStats <- do.call("rbind.fill", sapply(SLPA_output_list, "[", "summaries"))
SLPA_summary <- SLPA_summaryStats %>% 
  select(Title, LL, AIC, BIC, Entropy, T11_LMR_PValue, BLRT_PValue) %>%
  mutate(AIC_diff = lag(AIC) - AIC) %>% 
  mutate(BIC_diff = lag(BIC) - BIC)

SLPA_summary
```

    ##                   Title        LL      AIC      BIC Entropy T11_LMR_PValue
    ## 1 SLPA_Analysis_class_1 -81914.22 163860.4 163968.0      NA             NA
    ## 2 SLPA_Analysis_class_2 -71782.14 143614.3 143782.4   0.927         0.0000
    ## 3 SLPA_Analysis_class_3 -68336.90 136741.8 136970.4   0.876         0.0000
    ## 4 SLPA_Analysis_class_4 -67157.01 134400.0 134689.1   0.864         0.0000
    ## 5 SLPA_Analysis_class_5 -66462.50 133029.0 133378.6   0.827         0.2117
    ## 6 SLPA_Analysis_class_6 -65942.91 132007.8 132418.0   0.831         0.0000
    ##   BLRT_PValue  AIC_diff  BIC_diff
    ## 1          NA        NA        NA
    ## 2           0 20246.163 20185.648
    ## 3           0  6872.465  6811.950
    ## 4           0  2341.792  2281.277
    ## 5           0  1371.019  1310.504
    ## 6           0  1021.166   960.651

From the summary table, we could decide **class 3** (Largest BIC_diff)
or **class 4** (LMR LRT is not significant at class 5)

``` r
# Class 3

SLPA3_class_df <- SLPA_output_list[["SLPA_Analysis_class_3.out"]]$class_counts$posteriorProb

SLPA3_n_class_df <- SLPA_output_list[["SLPA_Analysis_class_3.out"]]$class_counts$mostLikely

SLPA3_means_df <- SLPA_output_list[["SLPA_Analysis_class_3.out"]]$parameters$unstandardized %>%
  filter(paramHeader == "Means", LatentClass != "Categorical.Latent.Variables") %>%
  select(LatentClass, param, est)

SLPA3_means_vars_df <- SLPA_output_list[["SLPA_Analysis_class_3.out"]]$parameters$unstandardized %>%
  filter(paramHeader == "Means" | paramHeader == "Variances", LatentClass != "Categorical.Latent.Variables") %>%
  select(LatentClass, paramHeader, param, est)

# Class Proportions (Posterior probabilities)
SLPA3_class_df
```

    ##   class    count proportion
    ## 1     1 1359.965    0.22120
    ## 2     2 2622.519    0.42656
    ## 3     3 2165.516    0.35223

``` r
# Class Proportions (Most Likely probabilities)
SLPA3_n_class_df
```

    ##   class count proportion
    ## 1     1  1360    0.22121
    ## 2     2  2615    0.42534
    ## 3     3  2173    0.35345

``` r
# Class 3 Mean & Var

as.data.frame(matrix(SLPA3_means_vars_df$est, ncol = 8, byrow = TRUE,
                     dimnames = list(c("c1_mean", "c1_var", "c2_mean", "c2_var", "c3_mean", "c3_var"), c("ex1", "ex2", "ex3", "ex4", "ac1", "ac2", "ac3", "ac4"))))
```

    ##           ex1   ex2   ex3   ex4   ac1   ac2   ac3   ac4
    ## c1_mean 1.558 1.374 1.422 1.535 1.726 1.709 1.282 1.516
    ## c1_var  0.498 0.585 0.546 0.701 1.269 0.653 0.929 1.143
    ## c2_mean 3.225 2.862 3.133 3.194 2.684 3.483 2.069 2.393
    ## c2_var  0.498 0.585 0.546 0.701 1.269 0.653 0.929 1.143
    ## c3_mean 4.291 4.110 4.385 4.286 3.481 4.470 3.296 3.356
    ## c3_var  0.498 0.585 0.546 0.701 1.269 0.653 0.929 1.143

``` r
# LPA plot
ggplot(SLPA3_means_df, aes(x = param, y = est, color = factor(LatentClass), group = LatentClass)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(title = "Latent Profile Means by Class (3)",
       x = "Indicator",
       y = "Estimated Mean",
       color = "Latent Class") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](MLPA2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Class 4

SLPA4_class_df <- SLPA_output_list[["SLPA_Analysis_class_4.out"]]$class_counts$posteriorProb

SLPA4_n_class_df <- SLPA_output_list[["SLPA_Analysis_class_4.out"]]$class_counts$mostLikely

SLPA4_means_df <- SLPA_output_list[["SLPA_Analysis_class_4.out"]]$parameters$unstandardized %>%
  filter(paramHeader == "Means", LatentClass != "Categorical.Latent.Variables") %>%
  select(LatentClass, param, est)

# Class Proportions (Most Likely probabilities)
SLPA4_n_class_df
```

    ##   class count proportion
    ## 1     1  1614    0.26252
    ## 2     2  1160    0.18868
    ## 3     3   808    0.13142
    ## 4     4  2566    0.41737

``` r
ggplot(SLPA4_means_df, aes(x = param, y = est, color = factor(LatentClass), group = LatentClass)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(title = "Latent Profile Means by Class (4)",
       x = "Indicator",
       y = "Estimated Mean",
       color = "Latent Class") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](MLPA2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Since there is no graphical difference between Class 3 and Class 4, I
will choose **Class 3,** for more parsimonious model.

------------------------------------------------------------------------

## 2. Multi-level LPA

    [[init]]
    iterators = classes;
    classes = 1:6;
    outputDirectory = "/Users/seongminpark/Desktop/KEMS/25-2/KELS/Analysis";
    filename = "MLPA_Analysis_class_[[classes]].inp";
    [[/init]]

    TITLE: MLPA_Analysis_class_[[classes]]
    DATA: FILE IS LPA_data.dat;

    VARIABLE: NAMES ARE 
    SID SCHID ex1-ex4 ac1-ac4
    ST_SEX !within covariates
    SC_REG; !between covariates
    USEVARIABLES = ex1-ex4 ac1-ac4;
    CLASSES = bc([[classes]]) wc(3); ! fix the best single-level LPA result
    CLUSTER = SCHID;
    WITHIN ARE ex1-ex4 ac1-ac4;
    BETWEEN ARE bc;
    MISSING = all(-999);

    ANALYSIS: TYPE = MIXTURE TWOLEVEL;
    STARTS = 100 20; ! need to increase the number

    MODEL:
    %WITHIN%
    %OVERALL%

    %BETWEEN%

    %OVERALL%
    wc ON bc;

    MODEL wc:
    %WITHIN%
    %wc#1%
    [ex1-ex4 ac1-ac4];
    %wc#2%
    [ex1-ex4 ac1-ac4];
    %wc#3%
    [ex1-ex4 ac1-ac4];

``` r
MLPA_text <- '
[[init]]
iterators = classes;
classes = 1:6;
outputDirectory = "/Users/seongminpark/Desktop/KEMS/25-2/KELS/Analysis";
filename = "MLPA_Analysis_class_[[classes]].inp";
[[/init]]

TITLE: MLPA_Analysis_class_[[classes]]
DATA: FILE IS LPA_data.dat;

VARIABLE: NAMES ARE 
SID SCHID ex1-ex4 ac1-ac4
ST_SEX !within covariates
SC_REG; !between covariates
USEVARIABLES = ex1-ex4 ac1-ac4;
CLASSES = bc([[classes]]) wc(3); ! fix the best single-level LPA result
CLUSTER = SCHID;
WITHIN ARE ex1-ex4 ac1-ac4;
BETWEEN ARE bc;
MISSING = all(-999);

ANALYSIS: TYPE = MIXTURE TWOLEVEL;
STARTS = 100 20; ! need to increase the number

MODEL:
%WITHIN%
%OVERALL%

%BETWEEN%

%OVERALL%
wc ON bc;

MODEL wc:
%WITHIN%
%wc#1%
[ex1-ex4 ac1-ac4];
%wc#2%
[ex1-ex4 ac1-ac4];
%wc#3%
[ex1-ex4 ac1-ac4];
'

writeLines(MLPA_text, "MLPA_text.txt")
createModels("MLPA_text.txt")
runModels(filefilter = "MLPA")
```

``` r
# Readmodels
MLPA_output_list <- readModels(filefilter = "MLPA")
MLPA_summaryStats <- do.call("rbind.fill", sapply(MLPA_output_list, "[", "summaries"))
MLPA_summary <- MLPA_summaryStats %>% 
  select(Title, LL, AIC, BIC, aBIC, Entropy) %>%
  mutate(AIC_diff = lag(AIC) - AIC) %>% 
  mutate(BIC_diff = lag(BIC) - BIC) 

MLPA_summary # class 2 shows the lowest BIC, but low entropy.
```

    ##                     Title        LL      AIC      BIC     aBIC Entropy AIC_diff
    ## 1 Covariates_BC2_WC3_MLPA -68308.52 136643.0 136730.5 136689.1   0.638       NA
    ## 2   MLPA_Analysis_class_1 -68336.91 136741.8 136970.4 136862.4   0.876  -98.771
    ## 3   MLPA_Analysis_class_2 -68308.52 136691.0 136939.8 136822.2   0.642   50.770
    ## 4   MLPA_Analysis_class_3 -68303.49 136687.0 136955.9 136828.8   0.708    4.068
    ## 5   MLPA_Analysis_class_4 -68302.50 136691.0 136980.1 136843.5   0.691   -4.033
    ## 6   MLPA_Analysis_class_5 -68302.38 136696.8 137006.1 136859.9   0.645   -5.752
    ## 7   MLPA_Analysis_class_6 -68302.42 136702.8 137032.3 136876.6   0.511   -6.078
    ##   BIC_diff
    ## 1       NA
    ## 2 -239.972
    ## 3   30.599
    ## 4  -16.105
    ## 5  -24.204
    ## 6  -25.924
    ## 7  -26.250

``` r
# Class 2

MLPA_output_list[["MLPA_Analysis_class_2.out"]]$output
```

    ##   [1] "Mplus VERSION 8 (Mac)"                                                                              
    ##   [2] "MUTHEN & MUTHEN"                                                                                    
    ##   [3] "09/18/2025   2:31 PM"                                                                               
    ##   [4] ""                                                                                                   
    ##   [5] "INPUT INSTRUCTIONS"                                                                                 
    ##   [6] ""                                                                                                   
    ##   [7] ""                                                                                                   
    ##   [8] "  TITLE: MLPA_Analysis_class_2"                                                                     
    ##   [9] "  DATA: FILE IS LPA_data.dat;"                                                                      
    ##  [10] ""                                                                                                   
    ##  [11] "  VARIABLE: NAMES ARE"                                                                              
    ##  [12] "  SID SCHID ex1-ex4 ac1-ac4"                                                                        
    ##  [13] "  ST_SEX !within covariates"                                                                        
    ##  [14] "  SC_REG; !between covariates"                                                                      
    ##  [15] "  USEVARIABLES = ex1-ex4 ac1-ac4;"                                                                  
    ##  [16] "  CLASSES = bc(2) wc(3); ! fix the best single-level LPA result"                                    
    ##  [17] "  CLUSTER = SCHID;"                                                                                 
    ##  [18] "  WITHIN ARE ex1-ex4 ac1-ac4;"                                                                      
    ##  [19] "  BETWEEN ARE bc;"                                                                                  
    ##  [20] "  MISSING = all(-999);"                                                                             
    ##  [21] ""                                                                                                   
    ##  [22] "  ANALYSIS: TYPE = MIXTURE TWOLEVEL;"                                                               
    ##  [23] "  STARTS = 100 20;"                                                                                 
    ##  [24] ""                                                                                                   
    ##  [25] "  MODEL:"                                                                                           
    ##  [26] "  %WITHIN%"                                                                                         
    ##  [27] "  %OVERALL%"                                                                                        
    ##  [28] ""                                                                                                   
    ##  [29] "  %BETWEEN%"                                                                                        
    ##  [30] ""                                                                                                   
    ##  [31] "  %OVERALL%"                                                                                        
    ##  [32] "  wc ON bc;"                                                                                        
    ##  [33] ""                                                                                                   
    ##  [34] "  MODEL wc:"                                                                                        
    ##  [35] "  %WITHIN%"                                                                                         
    ##  [36] "  %wc#1%"                                                                                           
    ##  [37] "  [ex1-ex4 ac1-ac4];"                                                                               
    ##  [38] "  %wc#2%"                                                                                           
    ##  [39] "  [ex1-ex4 ac1-ac4];"                                                                               
    ##  [40] "  %wc#3%"                                                                                           
    ##  [41] "  [ex1-ex4 ac1-ac4];"                                                                               
    ##  [42] ""                                                                                                   
    ##  [43] ""                                                                                                   
    ##  [44] ""                                                                                                   
    ##  [45] ""                                                                                                   
    ##  [46] "*** WARNING in MODEL command"                                                                       
    ##  [47] "  Variable is uncorrelated with all other variables within class: EX1"                              
    ##  [48] "*** WARNING in MODEL command"                                                                       
    ##  [49] "  Variable is uncorrelated with all other variables within class: EX2"                              
    ##  [50] "*** WARNING in MODEL command"                                                                       
    ##  [51] "  Variable is uncorrelated with all other variables within class: EX3"                              
    ##  [52] "*** WARNING in MODEL command"                                                                       
    ##  [53] "  Variable is uncorrelated with all other variables within class: EX4"                              
    ##  [54] "*** WARNING in MODEL command"                                                                       
    ##  [55] "  Variable is uncorrelated with all other variables within class: AC1"                              
    ##  [56] "*** WARNING in MODEL command"                                                                       
    ##  [57] "  Variable is uncorrelated with all other variables within class: AC2"                              
    ##  [58] "*** WARNING in MODEL command"                                                                       
    ##  [59] "  Variable is uncorrelated with all other variables within class: AC3"                              
    ##  [60] "*** WARNING in MODEL command"                                                                       
    ##  [61] "  Variable is uncorrelated with all other variables within class: AC4"                              
    ##  [62] "*** WARNING in MODEL command"                                                                       
    ##  [63] "  At least one variable is uncorrelated with all other variables within class."                     
    ##  [64] "  Check that this is what is intended."                                                             
    ##  [65] "*** WARNING"                                                                                        
    ##  [66] "  One or more individual-level variables have no variation within a"                                
    ##  [67] "  cluster for the following clusters."                                                              
    ##  [68] ""                                                                                                   
    ##  [69] "     Variable   Cluster IDs with no within-cluster variation"                                       
    ##  [70] ""                                                                                                   
    ##  [71] "      EX1         6849 8048 6301 8022 8057 7272 8279 8281 7316 6355 7129 6971 8314 6517 8009 7108"  
    ##  [72] "                  6276 6959 6430 7325 7078 6968 8304 8023 6221 6175 6191 7329 8335 7017 6446 6189"  
    ##  [73] "                  8327 7001 7219 7268 7110 6405 6075 8089 7073 6249 6735 6550 6734 6692 6062 6679"  
    ##  [74] "                  6008 6626 8162 8241 7165 6744 6320 6265 6255 8158 7180 6815 8108 7303 6281 6317"  
    ##  [75] "                  7225 6102 6802 6623 8109 8271 6458 8020 6832 6445 8049 6061 6291 6411 6906 6295"  
    ##  [76] "                  7330 6459 6243 8186 7137 8062 6346 6976 6666 6752 6203 7134 6980 6381 8037 8041"  
    ##  [77] "                  6140 6826 8209 7269 6020 6979 6589 6923"                                          
    ##  [78] "      EX2         6849 8274 6301 8295 7153 6197 8281 8133 6897 7126 6927 6932 6957 6959 7078 8304"  
    ##  [79] "                  6028 6221 6175 7041 8335 6189 6405 7070 7073 6550 6734 8096 6310 7226 6679 6229"  
    ##  [80] "                  6008 8216 7244 6597 8162 8102 8241 7165 7260 6236 6744 6320 6265 6256 6761 6231"  
    ##  [81] "                  7085 6238 7180 7170 8108 7227 7303 6281 7217 6313 6317 6629 7225 6623 8109 8107"  
    ##  [82] "                  8271 8118 6076 7304 8265 8275 8020 6445 8038 8268 6194 8263 7277 6233 8290 7243"  
    ##  [83] "                  6459 6888 8186 6975 6346 7313 6666 6448 8249 6805 7094 8037 8041 7092 6491 6867"  
    ##  [84] "                  6105"                                                                             
    ##  [85] "      EX3         6849 8022 8295 8057 7272 7153 8281 6897 7126 6974 6350 6971 6348 8314 6940 6951"  
    ##  [86] "                  6506 7221 6027 8318 6204 6935 6957 6959 7325 6483 8304 6028 6135 6221 6175 7049"  
    ##  [87] "                  7017 6189 8327 7001 7219 8015 6134 6075 6402 7070 7073 6093 6249 7224 6310 7244"  
    ##  [88] "                  8162 8241 7246 8156 6235 6236 6744 6769 6255 7142 7133 6231 6238 7180 8108 6281"  
    ##  [89] "                  7300 8123 6313 6802 8107 8271 6493 6297 7304 8275 8020 6832 7097 6390 6061 6865"  
    ##  [90] "                  6411 8263 7169 6908 6708 7168 6768 6346 7313 6666 6945 6646 6770 7134 6220 6521"  
    ##  [91] "                  8084 8037 6705 6491 6867 6796 8079"                                               
    ##  [92] "      EX4         6849 6499 8022 8295 8057 8289 7153 6365 6897 6107 7316 6348 6940 8067 8009 6276"  
    ##  [93] "                  8318 6957 6959 6430 7201 8304 6028 7049 6191 7329 7000 7017 6189 6200 7110 6134"  
    ##  [94] "                  6222 8025 7073 7083 6553 6249 6735 6550 6734 6692 6062 8096 7289 6229 6008 7244"  
    ##  [95] "                  6597 8162 8241 8156 6744 6761 6231 7085 6238 6809 8108 6153 6820 7303 6281 6313"  
    ##  [96] "                  6623 8109 8271 6493 8106 6076 7304 8046 8275 6702 7097 7185 6070 8263 6690 7330"  
    ##  [97] "                  8105 6768 7313 6754 6248 8112 7134 7029 8041 6111 7092 6728 6084"                 
    ##  [98] "      AC1         6092 8274 8048 7255 6499 8022 8057 8280 7272 7315 7153 8281 8133 7126 6974 6517"  
    ##  [99] "                  7221 8318 6932 6957 6959 8312 6483 7201 8304 6198 7049 6191 7329 6056 7000 8337"  
    ## [100] "                  7017 6625 7219 8015 7111 6133 6222 8025 7073 6730 8350 6739 8180 6553 6550 6616"  
    ## [101] "                  7224 6229 6008 7244 8162 8241 7165 6235 6744 6256 6255 6481 8158 7085 6531 6238"  
    ## [102] "                  7180 6800 8108 6773 6281 8247 8207 6313 8107 7004 7304 6702 6857 8052 8151 7119"  
    ## [103] "                  6908 8117 8290 8058 6463 6914 7168 6346 6770 6981 6695 7280 8037 7327 8149"       
    ## [104] "      AC2         6092 8274 8055 8048 8022 6882 7272 6159 7153 8281 6974 8315 8067 6927 6951 6921"  
    ## [105] "                  7221 8318 6395 6935 6957 6959 6430 8304 6198 8071 6028 8023 6221 6175 7049 7017"  
    ## [106] "                  7219 6133 7110 6405 8089 6402 7070 7073 8180 6249 6735 7224 6062 7289 6310 7226"  
    ## [107] "                  6679 6008 7244 6626 8162 8102 8241 8156 8193 6744 6265 6256 6761 7142 8270 7085"  
    ## [108] "                  6238 7180 8108 7227 6281 6313 6102 6623 8107 8271 8106 6297 8265 6825 8004 6305"  
    ## [109] "                  7097 6445 7185 7171 6325 6233 8062 6346 6752 6551 6140 7223 6002 6705 7192 8040"  
    ## [110] "                  7138"                                                                             
    ## [111] "      AC3         6849 6092 8274 8048 7255 8022 6882 8279 8042 8281 6897 6355 7108 6204 6935 6957"  
    ## [112] "                  6959 7201 8304 6135 6056 7041 7016 8337 6177 8335 8066 8015 7270 6222 7070 7073"  
    ## [113] "                  9006 6165 8180 6553 6692 7224 8045 6681 8097 7244 6597 8162 8241 6618 7260 7298"  
    ## [114] "                  8193 6744 6765 8167 7142 8270 8165 7085 6531 6238 8108 6820 7182 6281 8247 6283"  
    ## [115] "                  8107 6660 8271 8118 6076 6297 7304 8161 8275 8020 6702 6837 6806 6045 8268 6726"  
    ## [116] "                  6340 6336 6342 6941 6546 8329 6630 6491 8079"                                     
    ## [117] "      AC4         6849 6092 8122 8274 8048 6301 8022 8297 8281 6897 7126 6107 7108 6276 6506 6959"  
    ## [118] "                  8312 6483 8304 8023 7027 6191 7000 7016 6189 7219 6200 6134 6222 7070 7073 6093"  
    ## [119] "                  6201 8180 6553 6249 8045 6062 6310 6681 6229 6008 7244 8162 8241 8156 6236 7142"  
    ## [120] "                  8165 8158 6531 6800 7167 8108 6820 6281 7300 6313 7004 6660 8271 6076 6297 8189"  
    ## [121] "                  8265 6309 6825 8266 8275 6702 7097 6837 6045 8268 8263 6071 6340 6336 8019 6975"  
    ## [122] "                  7313 6342 6945 6357 6981 7264 6980 7045 8213 7166 8037 6683 8329 6511 6979"       
    ## [123] ""                                                                                                   
    ## [124] "  10 WARNING(S) FOUND IN THE INPUT INSTRUCTIONS"                                                    
    ## [125] ""                                                                                                   
    ## [126] ""                                                                                                   
    ## [127] ""                                                                                                   
    ## [128] "MLPA_Analysis_class_2"                                                                              
    ## [129] ""                                                                                                   
    ## [130] "SUMMARY OF ANALYSIS"                                                                                
    ## [131] ""                                                                                                   
    ## [132] "Number of groups                                                 1"                                 
    ## [133] "Number of observations                                        6148"                                 
    ## [134] ""                                                                                                   
    ## [135] "Number of dependent variables                                    8"                                 
    ## [136] "Number of independent variables                                  0"                                 
    ## [137] "Number of continuous latent variables                            0"                                 
    ## [138] "Number of categorical latent variables                           2"                                 
    ## [139] ""                                                                                                   
    ## [140] "Observed dependent variables"                                                                       
    ## [141] ""                                                                                                   
    ## [142] "  Continuous"                                                                                       
    ## [143] "   EX1         EX2         EX3         EX4         AC1         AC2"                                 
    ## [144] "   AC3         AC4"                                                                                 
    ## [145] ""                                                                                                   
    ## [146] "Categorical latent variables"                                                                       
    ## [147] "   BC          WC"                                                                                  
    ## [148] ""                                                                                                   
    ## [149] "Variables with special functions"                                                                   
    ## [150] ""                                                                                                   
    ## [151] "  Cluster variable      SCHID"                                                                      
    ## [152] ""                                                                                                   
    ## [153] "  Within variables"                                                                                 
    ## [154] "   EX1         EX2         EX3         EX4         AC1         AC2"                                 
    ## [155] "   AC3         AC4"                                                                                 
    ## [156] ""                                                                                                   
    ## [157] ""                                                                                                   
    ## [158] "Estimator                                                      MLR"                                 
    ## [159] "Information matrix                                        OBSERVED"                                 
    ## [160] "Optimization Specifications for the Quasi-Newton Algorithm for"                                     
    ## [161] "Continuous Outcomes"                                                                                
    ## [162] "  Maximum number of iterations                                 100"                                 
    ## [163] "  Convergence criterion                                  0.100D-05"                                 
    ## [164] "Optimization Specifications for the EM Algorithm"                                                   
    ## [165] "  Maximum number of iterations                                 500"                                 
    ## [166] "  Convergence criteria"                                                                             
    ## [167] "    Loglikelihood change                                 0.100D-02"                                 
    ## [168] "    Relative loglikelihood change                        0.100D-05"                                 
    ## [169] "    Derivative                                           0.100D-02"                                 
    ## [170] "Optimization Specifications for the M step of the EM Algorithm for"                                 
    ## [171] "Categorical Latent variables"                                                                       
    ## [172] "  Number of M step iterations                                    1"                                 
    ## [173] "  M step convergence criterion                           0.100D-02"                                 
    ## [174] "  Basis for M step termination                           ITERATION"                                 
    ## [175] "Optimization Specifications for the M step of the EM Algorithm for"                                 
    ## [176] "Censored, Binary or Ordered Categorical (Ordinal), Unordered"                                       
    ## [177] "Categorical (Nominal) and Count Outcomes"                                                           
    ## [178] "  Number of M step iterations                                    1"                                 
    ## [179] "  M step convergence criterion                           0.100D-02"                                 
    ## [180] "  Basis for M step termination                           ITERATION"                                 
    ## [181] "  Maximum value for logit thresholds                            15"                                 
    ## [182] "  Minimum value for logit thresholds                           -15"                                 
    ## [183] "  Minimum expected cell size for chi-square              0.100D-01"                                 
    ## [184] "Maximum number of iterations for H1                           2000"                                 
    ## [185] "Convergence criterion for H1                             0.100D-03"                                 
    ## [186] "Optimization algorithm                                         EMA"                                 
    ## [187] "Integration Specifications"                                                                         
    ## [188] "  Type                                                    STANDARD"                                 
    ## [189] "  Number of integration points                                  15"                                 
    ## [190] "  Dimensions of numerical integration                            0"                                 
    ## [191] "  Adaptive quadrature                                           ON"                                 
    ## [192] "Random Starts Specifications"                                                                       
    ## [193] "  Number of initial stage random starts                        100"                                 
    ## [194] "  Number of final stage optimizations                           20"                                 
    ## [195] "  Number of initial stage iterations                            10"                                 
    ## [196] "  Initial stage convergence criterion                    0.100D+01"                                 
    ## [197] "  Random starts scale                                    0.500D+01"                                 
    ## [198] "  Random seed for generating random starts                       0"                                 
    ## [199] "Parameterization                                             LOGIT"                                 
    ## [200] "Cholesky                                                       OFF"                                 
    ## [201] ""                                                                                                   
    ## [202] "Input data file(s)"                                                                                 
    ## [203] "  LPA_data.dat"                                                                                     
    ## [204] "Input data format  FREE"                                                                            
    ## [205] ""                                                                                                   
    ## [206] ""                                                                                                   
    ## [207] "SUMMARY OF DATA"                                                                                    
    ## [208] ""                                                                                                   
    ## [209] "     Number of missing data patterns             6"                                                 
    ## [210] "     Number of y missing data patterns           6"                                                 
    ## [211] "     Number of u missing data patterns           0"                                                 
    ## [212] "     Number of clusters                       1657"                                                 
    ## [213] ""                                                                                                   
    ## [214] ""                                                                                                   
    ## [215] ""                                                                                                   
    ## [216] "COVARIANCE COVERAGE OF DATA"                                                                        
    ## [217] ""                                                                                                   
    ## [218] "Minimum covariance coverage value   0.100"                                                          
    ## [219] ""                                                                                                   
    ## [220] ""                                                                                                   
    ## [221] "     PROPORTION OF DATA PRESENT FOR Y"                                                              
    ## [222] ""                                                                                                   
    ## [223] ""                                                                                                   
    ## [224] "           Covariance Coverage"                                                                     
    ## [225] "              EX1           EX2           EX3           EX4           AC1"                          
    ## [226] "              ________      ________      ________      ________      ________"                     
    ## [227] " EX1            1.000"                                                                              
    ## [228] " EX2            1.000         1.000"                                                                
    ## [229] " EX3            1.000         1.000         1.000"                                                  
    ## [230] " EX4            1.000         1.000         1.000         1.000"                                    
    ## [231] " AC1            0.999         0.999         0.999         0.999         0.999"                      
    ## [232] " AC2            1.000         1.000         1.000         0.999         0.999"                      
    ## [233] " AC3            1.000         1.000         1.000         1.000         0.999"                      
    ## [234] " AC4            1.000         1.000         1.000         1.000         0.999"                      
    ## [235] ""                                                                                                   
    ## [236] ""                                                                                                   
    ## [237] "           Covariance Coverage"                                                                     
    ## [238] "              AC2           AC3           AC4"                                                      
    ## [239] "              ________      ________      ________"                                                 
    ## [240] " AC2            1.000"                                                                              
    ## [241] " AC3            0.999         1.000"                                                                
    ## [242] " AC4            0.999         1.000         1.000"                                                  
    ## [243] ""                                                                                                   
    ## [244] ""                                                                                                   
    ## [245] ""                                                                                                   
    ## [246] "UNIVARIATE SAMPLE STATISTICS"                                                                       
    ## [247] ""                                                                                                   
    ## [248] ""                                                                                                   
    ## [249] "     UNIVARIATE HIGHER-ORDER MOMENT DESCRIPTIVE STATISTICS"                                         
    ## [250] ""                                                                                                   
    ## [251] "         Variable/         Mean/     Skewness/   Minimum/ % with                Percentiles"        
    ## [252] "        Sample Size      Variance    Kurtosis    Maximum  Min/Max      20%/60%    40%/80%    Median"
    ## [253] ""                                                                                                   
    ## [254] "     EX1                   3.232      -0.436       1.000   13.27%       2.000      3.000      3.000"
    ## [255] "            6148.000       1.514      -0.793       5.000   13.63%       4.000      4.000"           
    ## [256] "     EX2                   2.973      -0.117       1.000   17.21%       2.000      3.000      3.000"
    ## [257] "            6148.000       1.611      -1.065       5.000   11.47%       3.000      4.000"           
    ## [258] "     EX3                   3.196      -0.317       1.000   15.65%       2.000      3.000      3.000"
    ## [259] "            6148.000       1.741      -1.049       5.000   17.42%       4.000      4.000"           
    ## [260] "     EX4                   3.212      -0.320       1.000   15.05%       2.000      3.000      3.000"
    ## [261] "            6147.000       1.730      -1.037       5.000   17.88%       4.000      4.000"           
    ## [262] "     AC1                   2.753       0.129       1.000   22.92%       1.000      2.000      3.000"
    ## [263] "            6144.000       1.691      -1.100       5.000   10.63%       3.000      4.000"           
    ## [264] "     AC2                   3.438      -0.585       1.000   13.03%       2.000      3.000      4.000"
    ## [265] "            6145.000       1.690      -0.745       5.000   22.73%       4.000      5.000"           
    ## [266] "     AC3                   2.327       0.615       1.000   32.85%       1.000      2.000      2.000"
    ## [267] "            6146.000       1.529      -0.642       5.000    6.88%       2.000      3.000"           
    ## [268] "     AC4                   2.538       0.364       1.000   27.49%       1.000      2.000      2.000"
    ## [269] "            6147.000       1.618      -0.926       5.000    8.64%       3.000      4.000"           
    ## [270] ""                                                                                                   
    ## [271] "RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST LOGLIKELIHOOD VALUES"                       
    ## [272] ""                                                                                                   
    ## [273] "Final stage loglikelihood values at local maxima, seeds, and initial stage start numbers:"          
    ## [274] ""                                                                                                   
    ## [275] "          -68308.521  399671           13"                                                          
    ## [276] "          -68308.523  650371           14"                                                          
    ## [277] "          -68308.534  754100           56"                                                          
    ## [278] "          -68308.543  887676           22"                                                          
    ## [279] "          -68308.543  107446           12"                                                          
    ## [280] "          -68308.544  784664           75"                                                          
    ## [281] "          -68308.545  804561           59"                                                          
    ## [282] "          -68308.545  573096           20"                                                          
    ## [283] "          -68308.547  645664           39"                                                          
    ## [284] "          -68308.548  372176           23"                                                          
    ## [285] "          -68308.549  366706           29"                                                          
    ## [286] "          -68308.554  207896           25"                                                          
    ## [287] "          -68308.559  569833           85"                                                          
    ## [288] "          -68308.566  120506           45"                                                          
    ## [289] "          -68308.571  481835           57"                                                          
    ## [290] "          -68308.582  551639           55"                                                          
    ## [291] "          -68308.593  93468            3"                                                           
    ## [292] "          -68308.640  467339           66"                                                          
    ## [293] "          -68308.641  124999           96"                                                          
    ## [294] "          -68308.641  576596           99"                                                          
    ## [295] ""                                                                                                   
    ## [296] ""                                                                                                   
    ## [297] ""                                                                                                   
    ## [298] "THE BEST LOGLIKELIHOOD VALUE HAS BEEN REPLICATED.  RERUN WITH AT LEAST TWICE THE"                   
    ## [299] "RANDOM STARTS TO CHECK THAT THE BEST LOGLIKELIHOOD IS STILL OBTAINED AND REPLICATED."               
    ## [300] ""                                                                                                   
    ## [301] ""                                                                                                   
    ## [302] "THE MODEL ESTIMATION TERMINATED NORMALLY"                                                           
    ## [303] ""                                                                                                   
    ## [304] ""                                                                                                   
    ## [305] ""                                                                                                   
    ## [306] "MODEL FIT INFORMATION"                                                                              
    ## [307] ""                                                                                                   
    ## [308] "Number of Free Parameters                       37"                                                 
    ## [309] ""                                                                                                   
    ## [310] "Loglikelihood"                                                                                      
    ## [311] ""                                                                                                   
    ## [312] "          H0 Value                      -68308.521"                                                 
    ## [313] "          H0 Scaling Correction Factor      1.3342"                                                 
    ## [314] "            for MLR"                                                                                
    ## [315] ""                                                                                                   
    ## [316] "Information Criteria"                                                                               
    ## [317] ""                                                                                                   
    ## [318] "          Akaike (AIC)                  136691.042"                                                 
    ## [319] "          Bayesian (BIC)                136939.825"                                                 
    ## [320] "          Sample-Size Adjusted BIC      136822.249"                                                 
    ## [321] "            (n* = (n + 2) / 24)"                                                                    
    ## [322] ""                                                                                                   
    ## [323] ""                                                                                                   
    ## [324] ""                                                                                                   
    ## [325] "MODEL RESULTS USE THE LATENT CLASS VARIABLE ORDER"                                                  
    ## [326] ""                                                                                                   
    ## [327] "   BC  WC"                                                                                          
    ## [328] ""                                                                                                   
    ## [329] "  Latent Class Variable Patterns"                                                                   
    ## [330] ""                                                                                                   
    ## [331] "         BC        WC"                                                                              
    ## [332] "      Class     Class"                                                                              
    ## [333] ""                                                                                                   
    ## [334] "         1         1"                                                                               
    ## [335] "         1         2"                                                                               
    ## [336] "         1         3"                                                                               
    ## [337] "         2         1"                                                                               
    ## [338] "         2         2"                                                                               
    ## [339] "         2         3"                                                                               
    ## [340] ""                                                                                                   
    ## [341] ""                                                                                                   
    ## [342] "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS"                                   
    ## [343] "BASED ON ESTIMATED POSTERIOR PROBABILITIES"                                                         
    ## [344] ""                                                                                                   
    ## [345] "  Latent Class"                                                                                     
    ## [346] "    Pattern"                                                                                        
    ## [347] ""                                                                                                   
    ## [348] "    1  1       1057.77865          0.17205"                                                         
    ## [349] "    1  2        839.89339          0.13661"                                                         
    ## [350] "    1  3        576.32610          0.09374"                                                         
    ## [351] "    2  1       1553.97784          0.25276"                                                         
    ## [352] "    2  2        513.35306          0.08350"                                                         
    ## [353] "    2  3       1606.67096          0.26133"                                                         
    ## [354] ""                                                                                                   
    ## [355] ""                                                                                                   
    ## [356] "FINAL CLASS COUNTS AND PROPORTIONS FOR EACH LATENT CLASS VARIABLE"                                  
    ## [357] "BASED ON ESTIMATED POSTERIOR PROBABILITIES"                                                         
    ## [358] ""                                                                                                   
    ## [359] "  Latent Class"                                                                                     
    ## [360] "    Variable    Class"                                                                              
    ## [361] ""                                                                                                   
    ## [362] "    BC             1      2473.99829          0.40241"                                              
    ## [363] "                   2      3674.00171          0.59759"                                              
    ## [364] "    WC             1      2611.75659          0.42481"                                              
    ## [365] "                   2      1353.24646          0.22011"                                              
    ## [366] "                   3      2182.99707          0.35507"                                              
    ## [367] ""                                                                                                   
    ## [368] ""                                                                                                   
    ## [369] "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS"                                   
    ## [370] "BASED ON THEIR MOST LIKELY LATENT CLASS PATTERN"                                                    
    ## [371] ""                                                                                                   
    ## [372] "Class Counts and Proportions"                                                                       
    ## [373] ""                                                                                                   
    ## [374] "  Latent Class"                                                                                     
    ## [375] "    Pattern"                                                                                        
    ## [376] ""                                                                                                   
    ## [377] "    1  1              789          0.12833"                                                         
    ## [378] "    1  2              969          0.15761"                                                         
    ## [379] "    1  3              274          0.04457"                                                         
    ## [380] "    2  1             1801          0.29294"                                                         
    ## [381] "    2  2              384          0.06246"                                                         
    ## [382] "    2  3             1931          0.31409"                                                         
    ## [383] ""                                                                                                   
    ## [384] ""                                                                                                   
    ## [385] "FINAL CLASS COUNTS AND PROPORTIONS FOR EACH LATENT CLASS VARIABLE"                                  
    ## [386] "BASED ON THEIR MOST LIKELY LATENT CLASS PATTERN"                                                    
    ## [387] ""                                                                                                   
    ## [388] "  Latent Class"                                                                                     
    ## [389] "    Variable    Class"                                                                              
    ## [390] ""                                                                                                   
    ## [391] "    BC             1            2032          0.33051"                                              
    ## [392] "                   2            4116          0.66949"                                              
    ## [393] "    WC             1            2590          0.42128"                                              
    ## [394] "                   2            1353          0.22007"                                              
    ## [395] "                   3            2205          0.35865"                                              
    ## [396] ""                                                                                                   
    ## [397] ""                                                                                                   
    ## [398] "CLASSIFICATION QUALITY"                                                                             
    ## [399] ""                                                                                                   
    ## [400] "     Entropy                         0.642"                                                         
    ## [401] ""                                                                                                   
    ## [402] ""                                                                                                   
    ## [403] "Average Latent Class Probabilities for Most Likely Latent Class Pattern (Row)"                      
    ## [404] "by Latent Class Pattern (Column)"                                                                   
    ## [405] ""                                                                                                   
    ## [406] "  Latent Class Variable Patterns"                                                                   
    ## [407] ""                                                                                                   
    ## [408] "  Latent Class         BC        WC"                                                                
    ## [409] "   Pattern No.      Class     Class"                                                                
    ## [410] ""                                                                                                   
    ## [411] "         1             1         1"                                                                 
    ## [412] "         2             1         2"                                                                 
    ## [413] "         3             1         3"                                                                 
    ## [414] "         4             2         1"                                                                 
    ## [415] "         5             2         2"                                                                 
    ## [416] "         6             2         3"                                                                 
    ## [417] ""                                                                                                   
    ## [418] "           1        2        3        4        5        6"                                          
    ## [419] ""                                                                                                   
    ## [420] "    1   0.668    0.014    0.025    0.273    0.002    0.017"                                         
    ## [421] "    2   0.017    0.719    0.000    0.013    0.251    0.000"                                         
    ## [422] "    3   0.040    0.000    0.657    0.010    0.000    0.293"                                         
    ## [423] "    4   0.255    0.008    0.008    0.678    0.010    0.041"                                         
    ## [424] "    5   0.007    0.305    0.000    0.037    0.651    0.000"                                         
    ## [425] "    6   0.021    0.000    0.188    0.046    0.000    0.745"                                         
    ## [426] ""                                                                                                   
    ## [427] ""                                                                                                   
    ## [428] "MODEL RESULTS"                                                                                      
    ## [429] ""                                                                                                   
    ## [430] "                                                    Two-Tailed"                                     
    ## [431] "                    Estimate       S.E.  Est./S.E.    P-Value"                                      
    ## [432] ""                                                                                                   
    ## [433] "Within Level"                                                                                       
    ## [434] ""                                                                                                   
    ## [435] "Latent Class Pattern 1 1"                                                                           
    ## [436] ""                                                                                                   
    ## [437] " Means"                                                                                             
    ## [438] "    EX1                3.218      0.043     75.532      0.000"                                      
    ## [439] "    EX2                2.856      0.044     65.119      0.000"                                      
    ## [440] "    EX3                3.125      0.051     61.182      0.000"                                      
    ## [441] "    EX4                3.188      0.043     73.858      0.000"                                      
    ## [442] "    AC1                2.681      0.026    102.951      0.000"                                      
    ## [443] "    AC2                3.477      0.039     89.108      0.000"                                      
    ## [444] "    AC3                2.067      0.023     91.338      0.000"                                      
    ## [445] "    AC4                2.390      0.027     89.251      0.000"                                      
    ## [446] ""                                                                                                   
    ## [447] " Variances"                                                                                         
    ## [448] "    EX1                0.498      0.014     35.214      0.000"                                      
    ## [449] "    EX2                0.585      0.015     38.329      0.000"                                      
    ## [450] "    EX3                0.546      0.014     39.971      0.000"                                      
    ## [451] "    EX4                0.701      0.018     39.071      0.000"                                      
    ## [452] "    AC1                1.269      0.023     56.092      0.000"                                      
    ## [453] "    AC2                0.651      0.021     31.297      0.000"                                      
    ## [454] "    AC3                0.932      0.027     34.644      0.000"                                      
    ## [455] "    AC4                1.144      0.024     46.840      0.000"                                      
    ## [456] ""                                                                                                   
    ## [457] "Latent Class Pattern 1 2"                                                                           
    ## [458] ""                                                                                                   
    ## [459] " Means"                                                                                             
    ## [460] "    EX1                1.554      0.034     46.253      0.000"                                      
    ## [461] "    EX2                1.371      0.023     60.138      0.000"                                      
    ## [462] "    EX3                1.419      0.026     54.002      0.000"                                      
    ## [463] "    EX4                1.532      0.035     43.219      0.000"                                      
    ## [464] "    AC1                1.723      0.038     45.413      0.000"                                      
    ## [465] "    AC2                1.703      0.046     37.197      0.000"                                      
    ## [466] "    AC3                1.280      0.019     66.548      0.000"                                      
    ## [467] "    AC4                1.513      0.028     53.397      0.000"                                      
    ## [468] ""                                                                                                   
    ## [469] " Variances"                                                                                         
    ## [470] "    EX1                0.498      0.014     35.214      0.000"                                      
    ## [471] "    EX2                0.585      0.015     38.329      0.000"                                      
    ## [472] "    EX3                0.546      0.014     39.971      0.000"                                      
    ## [473] "    EX4                0.701      0.018     39.071      0.000"                                      
    ## [474] "    AC1                1.269      0.023     56.092      0.000"                                      
    ## [475] "    AC2                0.651      0.021     31.297      0.000"                                      
    ## [476] "    AC3                0.932      0.027     34.644      0.000"                                      
    ## [477] "    AC4                1.144      0.024     46.840      0.000"                                      
    ## [478] ""                                                                                                   
    ## [479] "Latent Class Pattern 1 3"                                                                           
    ## [480] ""                                                                                                   
    ## [481] " Means"                                                                                             
    ## [482] "    EX1                4.288      0.024    175.501      0.000"                                      
    ## [483] "    EX2                4.105      0.034    120.323      0.000"                                      
    ## [484] "    EX3                4.381      0.028    154.488      0.000"                                      
    ## [485] "    EX4                4.283      0.026    162.646      0.000"                                      
    ## [486] "    AC1                3.477      0.040     87.358      0.000"                                      
    ## [487] "    AC2                4.467      0.023    197.572      0.000"                                      
    ## [488] "    AC3                3.287      0.063     52.583      0.000"                                      
    ## [489] "    AC4                3.350      0.051     66.121      0.000"                                      
    ## [490] ""                                                                                                   
    ## [491] " Variances"                                                                                         
    ## [492] "    EX1                0.498      0.014     35.214      0.000"                                      
    ## [493] "    EX2                0.585      0.015     38.329      0.000"                                      
    ## [494] "    EX3                0.546      0.014     39.971      0.000"                                      
    ## [495] "    EX4                0.701      0.018     39.071      0.000"                                      
    ## [496] "    AC1                1.269      0.023     56.092      0.000"                                      
    ## [497] "    AC2                0.651      0.021     31.297      0.000"                                      
    ## [498] "    AC3                0.932      0.027     34.644      0.000"                                      
    ## [499] "    AC4                1.144      0.024     46.840      0.000"                                      
    ## [500] ""                                                                                                   
    ## [501] "Latent Class Pattern 2 1"                                                                           
    ## [502] ""                                                                                                   
    ## [503] " Means"                                                                                             
    ## [504] "    EX1                3.218      0.043     75.532      0.000"                                      
    ## [505] "    EX2                2.856      0.044     65.119      0.000"                                      
    ## [506] "    EX3                3.125      0.051     61.182      0.000"                                      
    ## [507] "    EX4                3.188      0.043     73.858      0.000"                                      
    ## [508] "    AC1                2.681      0.026    102.951      0.000"                                      
    ## [509] "    AC2                3.477      0.039     89.108      0.000"                                      
    ## [510] "    AC3                2.067      0.023     91.338      0.000"                                      
    ## [511] "    AC4                2.390      0.027     89.251      0.000"                                      
    ## [512] ""                                                                                                   
    ## [513] " Variances"                                                                                         
    ## [514] "    EX1                0.498      0.014     35.214      0.000"                                      
    ## [515] "    EX2                0.585      0.015     38.329      0.000"                                      
    ## [516] "    EX3                0.546      0.014     39.971      0.000"                                      
    ## [517] "    EX4                0.701      0.018     39.071      0.000"                                      
    ## [518] "    AC1                1.269      0.023     56.092      0.000"                                      
    ## [519] "    AC2                0.651      0.021     31.297      0.000"                                      
    ## [520] "    AC3                0.932      0.027     34.644      0.000"                                      
    ## [521] "    AC4                1.144      0.024     46.840      0.000"                                      
    ## [522] ""                                                                                                   
    ## [523] "Latent Class Pattern 2 2"                                                                           
    ## [524] ""                                                                                                   
    ## [525] " Means"                                                                                             
    ## [526] "    EX1                1.554      0.034     46.253      0.000"                                      
    ## [527] "    EX2                1.371      0.023     60.138      0.000"                                      
    ## [528] "    EX3                1.419      0.026     54.002      0.000"                                      
    ## [529] "    EX4                1.532      0.035     43.219      0.000"                                      
    ## [530] "    AC1                1.723      0.038     45.413      0.000"                                      
    ## [531] "    AC2                1.703      0.046     37.197      0.000"                                      
    ## [532] "    AC3                1.280      0.019     66.548      0.000"                                      
    ## [533] "    AC4                1.513      0.028     53.397      0.000"                                      
    ## [534] ""                                                                                                   
    ## [535] " Variances"                                                                                         
    ## [536] "    EX1                0.498      0.014     35.214      0.000"                                      
    ## [537] "    EX2                0.585      0.015     38.329      0.000"                                      
    ## [538] "    EX3                0.546      0.014     39.971      0.000"                                      
    ## [539] "    EX4                0.701      0.018     39.071      0.000"                                      
    ## [540] "    AC1                1.269      0.023     56.092      0.000"                                      
    ## [541] "    AC2                0.651      0.021     31.297      0.000"                                      
    ## [542] "    AC3                0.932      0.027     34.644      0.000"                                      
    ## [543] "    AC4                1.144      0.024     46.840      0.000"                                      
    ## [544] ""                                                                                                   
    ## [545] "Latent Class Pattern 2 3"                                                                           
    ## [546] ""                                                                                                   
    ## [547] " Means"                                                                                             
    ## [548] "    EX1                4.288      0.024    175.501      0.000"                                      
    ## [549] "    EX2                4.105      0.034    120.323      0.000"                                      
    ## [550] "    EX3                4.381      0.028    154.488      0.000"                                      
    ## [551] "    EX4                4.283      0.026    162.646      0.000"                                      
    ## [552] "    AC1                3.477      0.040     87.358      0.000"                                      
    ## [553] "    AC2                4.467      0.023    197.572      0.000"                                      
    ## [554] "    AC3                3.287      0.063     52.583      0.000"                                      
    ## [555] "    AC4                3.350      0.051     66.121      0.000"                                      
    ## [556] ""                                                                                                   
    ## [557] " Variances"                                                                                         
    ## [558] "    EX1                0.498      0.014     35.214      0.000"                                      
    ## [559] "    EX2                0.585      0.015     38.329      0.000"                                      
    ## [560] "    EX3                0.546      0.014     39.971      0.000"                                      
    ## [561] "    EX4                0.701      0.018     39.071      0.000"                                      
    ## [562] "    AC1                1.269      0.023     56.092      0.000"                                      
    ## [563] "    AC2                0.651      0.021     31.297      0.000"                                      
    ## [564] "    AC3                0.932      0.027     34.644      0.000"                                      
    ## [565] "    AC4                1.144      0.024     46.840      0.000"                                      
    ## [566] ""                                                                                                   
    ## [567] "Between Level"                                                                                      
    ## [568] ""                                                                                                   
    ## [569] "Categorical Latent Variables"                                                                       
    ## [570] ""                                                                                                   
    ## [571] "Within Level"                                                                                       
    ## [572] ""                                                                                                   
    ## [573] " Intercepts"                                                                                        
    ## [574] "    WC#1              -0.033      0.185     -0.180      0.857"                                      
    ## [575] "    WC#2              -1.140      0.455     -2.505      0.012"                                      
    ## [576] ""                                                                                                   
    ## [577] "Between Level"                                                                                      
    ## [578] ""                                                                                                   
    ## [579] " WC#1       ON"                                                                                     
    ## [580] "    BC#1               0.641      0.174      3.683      0.000"                                      
    ## [581] ""                                                                                                   
    ## [582] " WC#2       ON"                                                                                     
    ## [583] "    BC#1               1.517      0.165      9.191      0.000"                                      
    ## [584] ""                                                                                                   
    ## [585] " Means"                                                                                             
    ## [586] "    BC#1              -0.298      1.160     -0.257      0.797"                                      
    ## [587] ""                                                                                                   
    ## [588] ""                                                                                                   
    ## [589] "QUALITY OF NUMERICAL RESULTS"                                                                       
    ## [590] ""                                                                                                   
    ## [591] "     Condition Number for the Information Matrix              0.113E-03"                            
    ## [592] "       (ratio of smallest to largest eigenvalue)"                                                   
    ## [593] ""                                                                                                   
    ## [594] ""                                                                                                   
    ## [595] "     Beginning Time:  14:31:04"                                                                     
    ## [596] "        Ending Time:  14:31:53"                                                                     
    ## [597] "       Elapsed Time:  00:00:49"                                                                     
    ## [598] ""                                                                                                   
    ## [599] ""                                                                                                   
    ## [600] ""                                                                                                   
    ## [601] "MUTHEN & MUTHEN"                                                                                    
    ## [602] "3463 Stoner Ave."                                                                                   
    ## [603] "Los Angeles, CA  90066"                                                                             
    ## [604] ""                                                                                                   
    ## [605] "Tel: (310) 391-9971"                                                                                
    ## [606] "Fax: (310) 391-8971"                                                                                
    ## [607] "Web: www.StatModel.com"                                                                             
    ## [608] "Support: Support@StatModel.com"                                                                     
    ## [609] ""                                                                                                   
    ## [610] "Copyright (c) 1998-2017 Muthen & Muthen"

``` r
MLPA2_class_df <- MLPA_output_list[["MLPA_Analysis_class_2.out"]]$class_counts$posteriorProb

MLPA2_n_class_df <- MLPA_output_list[["MLPA_Analysis_class_2.out"]]$class_counts$mostLikely
MLPA2_pattern_df <- MLPA_output_list[["MLPA_Analysis_class_2.out"]]$class_counts$mostLikely.patterns

MLPA2_means_df <- MLPA_output_list[["MLPA_Analysis_class_2.out"]]$parameters$unstandardized %>%
  filter(paramHeader == "Means", LatentClass != "Categorical.Latent.Variables") %>%
  select(LatentClass, param, est)

MLPA2_means_vars_df <- MLPA_output_list[["MLPA_Analysis_class_2.out"]]$parameters$unstandardized %>%
  filter(paramHeader == "Means" | paramHeader == "Variances", LatentClass != "Categorical.Latent.Variables") %>%
  select(LatentClass, paramHeader, param, est)

MLPA2_means_vars_df <- as.data.frame(matrix(MLPA2_means_vars_df$est, ncol = 8, byrow = TRUE,
                     dimnames = list(c("c1_1mean", "c1_1var", "c1_2mean", "c1_2var", "c1_3mean", "c1_3var","c2_1mean", "c2_1var", "c2_2mean", "c2_2var", "c2_3mean", "c2_3var"), c("ex1", "ex2", "ex3", "ex4", "ac1", "ac2", "ac3", "ac4"))))

# Class Proportions (Most Likely probabilities)
MLPA2_n_class_df
```

    ##   variable class count proportion
    ## 1       BC     1  2032    0.33051
    ## 2       BC     2  4116    0.66949
    ## 3       WC     1  2590    0.42128
    ## 4       WC     2  1353    0.22007
    ## 5       WC     3  2205    0.35865

``` r
# Class 2 Mean & Var
MLPA2_means_vars_df
```

    ##            ex1   ex2   ex3   ex4   ac1   ac2   ac3   ac4
    ## c1_1mean 3.218 2.856 3.125 3.188 2.681 3.477 2.067 2.390
    ## c1_1var  0.498 0.585 0.546 0.701 1.269 0.651 0.932 1.144
    ## c1_2mean 1.554 1.371 1.419 1.532 1.723 1.703 1.280 1.513
    ## c1_2var  0.498 0.585 0.546 0.701 1.269 0.651 0.932 1.144
    ## c1_3mean 4.288 4.105 4.381 4.283 3.477 4.467 3.287 3.350
    ## c1_3var  0.498 0.585 0.546 0.701 1.269 0.651 0.932 1.144
    ## c2_1mean 3.218 2.856 3.125 3.188 2.681 3.477 2.067 2.390
    ## c2_1var  0.498 0.585 0.546 0.701 1.269 0.651 0.932 1.144
    ## c2_2mean 1.554 1.371 1.419 1.532 1.723 1.703 1.280 1.513
    ## c2_2var  0.498 0.585 0.546 0.701 1.269 0.651 0.932 1.144
    ## c2_3mean 4.288 4.105 4.381 4.283 3.477 4.467 3.287 3.350
    ## c2_3var  0.498 0.585 0.546 0.701 1.269 0.651 0.932 1.144

``` r
MLPA2_pattern_df <- MLPA_output_list[["MLPA_Analysis_class_2.out"]]$class_counts$mostLikely.patterns

MLPA2_pattern_df <- MLPA2_pattern_df %>% 
  dplyr::group_by(BC) %>% 
  dplyr::mutate(B_proportion = proportion / sum(proportion)) %>%
  ungroup()

# Plot (Within)

within_plot <- MLPA2_means_df %>% 
  filter(LatentClass == 1.1 | LatentClass == 1.2 | LatentClass == 1.3) %>% 
  mutate(LatentClass = case_when(
    LatentClass == 1.1 ~ "c1 (Medium)",
    LatentClass == 1.2 ~ "c2 (Low)",
    LatentClass == 1.3 ~ "c3 (High)"
  )
)

ggplot(within_plot, aes(x = param, y = est, color = factor(LatentClass), group = LatentClass)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(title = "Latent Profile Means by Class (3) - within",
       x = "Indicator",
       y = "Estimated Mean",
       color = "Latent Class") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](MLPA2_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
# Plot (Between)

df_labels <- MLPA2_pattern_df %>%
  dplyr::group_by(BC) %>%
  dplyr::arrange(BC, desc(WC)) %>%  # Arrange so labels don't overlap
  dplyr::mutate(
    ypos = cumsum(B_proportion) - 0.5 * B_proportion,
    label = scales::percent(B_proportion, accuracy = 1), 
    WC = case_when(
    WC == 1 ~ "c1 (Medium)",
    WC == 2 ~ "c2 (Low)",
    WC == 3 ~ "c3 (High)"
    )
    )
  
ggplot(df_labels, aes(x = factor(BC), y = B_proportion, fill = factor(WC))) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(y = ypos, label = label), color = "white", size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, 1)) +  #  sets y-axis to 0–1 (0–100%)
  labs(
    title = "BC-WC plot",
    x = "Between-Class (BC)",
    y = "Within-BC Proportion (%)",
    fill = "Within-Class (WC)"
  ) +
  theme_minimal(base_size = 14)
```

![](MLPA2_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

Hence, I’ll conclude nonparametric BC(2) WC(3) model. Base on the plot
above, **school group 2** includes schools with more academic stress and
exam stress, compared to **school group 1.**

------------------------------------------------------------------------

## 3. L1-L2 covariates

**`!!!!!!!!!!!!!We need to update this part to find the best fitting model!!!!!!!!!!!!!!`**

Bring the var_list again:

``` r
# full variable list in order
## VARIABLE: NAMES ARE
cat(gsub('"', '', colnames(LPA_data)), sep = " ")
```

    ## L2SID L2Y7_SCHID L2Y7S1901 L2Y7S1902 L2Y7S1903 L2Y7S1904 L2Y7S1905 L2Y7S1906 L2Y7S1907 L2Y7S1908 L2GENDER L2Y7_REG

``` r
# After deciding the number of classes, you need to add covariates syntax in variable section, which indicates the within and between level covariates. Just copy the code result of below syntax:
cat("WITHIN ARE", gsub('"', '', stu_cov_list), ";", collapse = "")
```

    ## WITHIN ARE L2GENDER ;

``` r
cat("BETWEEN ARE bc", gsub('"', '', sch_cov_list), ";", collapse = "")
```

    ## BETWEEN ARE bc L2Y7_REG ;

To fix the class mean, use this syntax and copy & paste:

``` r
# Fix the observed mean and variance for each class
## c1
paste0(colnames(MLPA2_means_vars_df), "@", as.vector(MLPA2_means_vars_df[1,]), collapse = " ")
```

    ## [1] "ex1@3.218 ex2@2.856 ex3@3.125 ex4@3.188 ac1@2.681 ac2@3.477 ac3@2.067 ac4@2.39"

``` r
## c2
paste0(colnames(MLPA2_means_vars_df), "@", as.vector(MLPA2_means_vars_df[3,]), collapse = " ")
```

    ## [1] "ex1@1.554 ex2@1.371 ex3@1.419 ex4@1.532 ac1@1.723 ac2@1.703 ac3@1.28 ac4@1.513"

``` r
## c3
paste0(colnames(MLPA2_means_vars_df), "@", as.vector(MLPA2_means_vars_df[5,]), collapse = " ")
```

    ## [1] "ex1@4.288 ex2@4.105 ex3@4.381 ex4@4.283 ac1@3.477 ac2@4.467 ac3@3.287 ac4@3.35"

``` r
cov_text <- '
[[init]]
iterators = classes;
classes = 3;
outputDirectory = "/Users/seongminpark/Desktop/KEMS/25-2/KELS/Analysis";
filename = "Covariate_MLPA_bc2_wc[[classes]].inp";
[[/init]]

TITLE: Covariates_BC2_WC3_MLPA
DATA: FILE IS LPA_data.dat;

VARIABLE: NAMES ARE 
SID SCHID ex1-ex4 ac1-ac4
ST_SEX !within covariates need to be updated
SC_REG; !between covariates need to be updated
USEVARIABLES = ex1-ex4 ac1-ac4 ST_SEX SC_SEX;
CLASSES = bc(2) wc([[classes]]); ! fix the best single-level LPA result
CLUSTER = SCHID;
WITHIN ARE ex1-ex4 ac1-ac4 ST_SEX;
BETWEEN ARE bc SC_SEX;
MISSING = all(-999);

DEFINE:
SC_SEX = CLUSTER_MEAN(ST_SEX);

ANALYSIS: TYPE = MIXTURE TWOLEVEL;
STARTS = 0;
OPTSEED = 399671; ! Need to be improved

MODEL:
%WITHIN%
%OVERALL%
wc on ST_SEX;! Add L1 covariates

%BETWEEN%
%OVERALL%
wc on bc;
bc ON SC_SEX; ! Add L2 covariates


MODEL wc:
%WITHIN%
%wc#1%
[ex1@3.218 ex2@2.856 ex3@3.125 ex4@3.188 ac1@2.681 ac2@3.477 ac3@2.067 ac4@2.39];
%wc#2%
[ex1@1.554 ex2@1.371 ex3@1.419 ex4@1.532 ac1@1.723 ac2@1.703 ac3@1.28 ac4@1.513];
%wc#3%
[ex1@4.288 ex2@4.105 ex3@4.381 ex4@4.283 ac1@3.477 ac2@4.467 ac3@3.287 ac4@3.35];
'

writeLines(cov_text, "cov_text.txt")
createModels("cov_text.txt")
runModels(filefilter = "Covariate")
```

``` r
Covariate_output <- readModels(filefilter = "Covariate")
Covariate_output$output
```

    ##   [1] "Mplus VERSION 8 (Mac)"                                                                              
    ##   [2] "MUTHEN & MUTHEN"                                                                                    
    ##   [3] "09/19/2025   2:52 PM"                                                                               
    ##   [4] ""                                                                                                   
    ##   [5] "INPUT INSTRUCTIONS"                                                                                 
    ##   [6] ""                                                                                                   
    ##   [7] ""                                                                                                   
    ##   [8] "  TITLE: Covariates_BC2_WC3_MLPA"                                                                   
    ##   [9] "  DATA: FILE IS LPA_data.dat;"                                                                      
    ##  [10] ""                                                                                                   
    ##  [11] "  VARIABLE: NAMES ARE"                                                                              
    ##  [12] "  SID SCHID ex1-ex4 ac1-ac4"                                                                        
    ##  [13] "  ST_SEX !within covariates need to be updated"                                                     
    ##  [14] "  SC_REG; !between covariates need to be updated"                                                   
    ##  [15] "  USEVARIABLES = ex1-ex4 ac1-ac4;"                                                                  
    ##  [16] "  CLASSES = bc(2) wc(3); ! fix the best single-level LPA result"                                    
    ##  [17] "  CLUSTER = SCHID;"                                                                                 
    ##  [18] "  WITHIN ARE ex1-ex4 ac1-ac4;"                                                                      
    ##  [19] "  BETWEEN ARE bc;"                                                                                  
    ##  [20] "  MISSING = all(-999);"                                                                             
    ##  [21] ""                                                                                                   
    ##  [22] "  !DEFINE:"                                                                                         
    ##  [23] "  !SC_SEX = CLUSTER_MEAN(ST_SEX);"                                                                  
    ##  [24] ""                                                                                                   
    ##  [25] "  ANALYSIS: TYPE = MIXTURE TWOLEVEL;"                                                               
    ##  [26] "  STARTS = 0;"                                                                                      
    ##  [27] "  OPTSEED = 399671;"                                                                                
    ##  [28] ""                                                                                                   
    ##  [29] "  MODEL:"                                                                                           
    ##  [30] "  %WITHIN%"                                                                                         
    ##  [31] "  %OVERALL%"                                                                                        
    ##  [32] "  !wc on ST_SEX;"                                                                                   
    ##  [33] ""                                                                                                   
    ##  [34] "  %BETWEEN%"                                                                                        
    ##  [35] "  %OVERALL%"                                                                                        
    ##  [36] "  wc on bc;"                                                                                        
    ##  [37] "  !bc ON SC_SEX;"                                                                                   
    ##  [38] ""                                                                                                   
    ##  [39] ""                                                                                                   
    ##  [40] "  MODEL wc:"                                                                                        
    ##  [41] "  %WITHIN%"                                                                                         
    ##  [42] "  %wc#1%"                                                                                           
    ##  [43] "  [ex1@3.218 ex2@2.856 ex3@3.125 ex4@3.188 ac1@2.681 ac2@3.477 ac3@2.067 ac4@2.39];"                
    ##  [44] "  %wc#2%"                                                                                           
    ##  [45] "  [ex1@1.554 ex2@1.371 ex3@1.419 ex4@1.532 ac1@1.723 ac2@1.703 ac3@1.28 ac4@1.513];"                
    ##  [46] "  %wc#3%"                                                                                           
    ##  [47] "  [ex1@4.288 ex2@4.105 ex3@4.381 ex4@4.283 ac1@3.477 ac2@4.467 ac3@3.287 ac4@3.35];"                
    ##  [48] ""                                                                                                   
    ##  [49] "  SAVEDATA:"                                                                                        
    ##  [50] "  file = cprob_mlpa.dat;"                                                                           
    ##  [51] "  save = cprob;"                                                                                    
    ##  [52] ""                                                                                                   
    ##  [53] ""                                                                                                   
    ##  [54] ""                                                                                                   
    ##  [55] "*** WARNING in MODEL command"                                                                       
    ##  [56] "  Variable is uncorrelated with all other variables within class: EX1"                              
    ##  [57] "*** WARNING in MODEL command"                                                                       
    ##  [58] "  Variable is uncorrelated with all other variables within class: EX2"                              
    ##  [59] "*** WARNING in MODEL command"                                                                       
    ##  [60] "  Variable is uncorrelated with all other variables within class: EX3"                              
    ##  [61] "*** WARNING in MODEL command"                                                                       
    ##  [62] "  Variable is uncorrelated with all other variables within class: EX4"                              
    ##  [63] "*** WARNING in MODEL command"                                                                       
    ##  [64] "  Variable is uncorrelated with all other variables within class: AC1"                              
    ##  [65] "*** WARNING in MODEL command"                                                                       
    ##  [66] "  Variable is uncorrelated with all other variables within class: AC2"                              
    ##  [67] "*** WARNING in MODEL command"                                                                       
    ##  [68] "  Variable is uncorrelated with all other variables within class: AC3"                              
    ##  [69] "*** WARNING in MODEL command"                                                                       
    ##  [70] "  Variable is uncorrelated with all other variables within class: AC4"                              
    ##  [71] "*** WARNING in MODEL command"                                                                       
    ##  [72] "  At least one variable is uncorrelated with all other variables within class."                     
    ##  [73] "  Check that this is what is intended."                                                             
    ##  [74] "*** WARNING"                                                                                        
    ##  [75] "  One or more individual-level variables have no variation within a"                                
    ##  [76] "  cluster for the following clusters."                                                              
    ##  [77] ""                                                                                                   
    ##  [78] "     Variable   Cluster IDs with no within-cluster variation"                                       
    ##  [79] ""                                                                                                   
    ##  [80] "      EX1         6849 8048 6301 8022 8057 7272 8279 8281 7316 6355 7129 6971 8314 6517 8009 7108"  
    ##  [81] "                  6276 6959 6430 7325 7078 6968 8304 8023 6221 6175 6191 7329 8335 7017 6446 6189"  
    ##  [82] "                  8327 7001 7219 7268 7110 6405 6075 8089 7073 6249 6735 6550 6734 6692 6062 6679"  
    ##  [83] "                  6008 6626 8162 8241 7165 6744 6320 6265 6255 8158 7180 6815 8108 7303 6281 6317"  
    ##  [84] "                  7225 6102 6802 6623 8109 8271 6458 8020 6832 6445 8049 6061 6291 6411 6906 6295"  
    ##  [85] "                  7330 6459 6243 8186 7137 8062 6346 6976 6666 6752 6203 7134 6980 6381 8037 8041"  
    ##  [86] "                  6140 6826 8209 7269 6020 6979 6589 6923"                                          
    ##  [87] "      EX2         6849 8274 6301 8295 7153 6197 8281 8133 6897 7126 6927 6932 6957 6959 7078 8304"  
    ##  [88] "                  6028 6221 6175 7041 8335 6189 6405 7070 7073 6550 6734 8096 6310 7226 6679 6229"  
    ##  [89] "                  6008 8216 7244 6597 8162 8102 8241 7165 7260 6236 6744 6320 6265 6256 6761 6231"  
    ##  [90] "                  7085 6238 7180 7170 8108 7227 7303 6281 7217 6313 6317 6629 7225 6623 8109 8107"  
    ##  [91] "                  8271 8118 6076 7304 8265 8275 8020 6445 8038 8268 6194 8263 7277 6233 8290 7243"  
    ##  [92] "                  6459 6888 8186 6975 6346 7313 6666 6448 8249 6805 7094 8037 8041 7092 6491 6867"  
    ##  [93] "                  6105"                                                                             
    ##  [94] "      EX3         6849 8022 8295 8057 7272 7153 8281 6897 7126 6974 6350 6971 6348 8314 6940 6951"  
    ##  [95] "                  6506 7221 6027 8318 6204 6935 6957 6959 7325 6483 8304 6028 6135 6221 6175 7049"  
    ##  [96] "                  7017 6189 8327 7001 7219 8015 6134 6075 6402 7070 7073 6093 6249 7224 6310 7244"  
    ##  [97] "                  8162 8241 7246 8156 6235 6236 6744 6769 6255 7142 7133 6231 6238 7180 8108 6281"  
    ##  [98] "                  7300 8123 6313 6802 8107 8271 6493 6297 7304 8275 8020 6832 7097 6390 6061 6865"  
    ##  [99] "                  6411 8263 7169 6908 6708 7168 6768 6346 7313 6666 6945 6646 6770 7134 6220 6521"  
    ## [100] "                  8084 8037 6705 6491 6867 6796 8079"                                               
    ## [101] "      EX4         6849 6499 8022 8295 8057 8289 7153 6365 6897 6107 7316 6348 6940 8067 8009 6276"  
    ## [102] "                  8318 6957 6959 6430 7201 8304 6028 7049 6191 7329 7000 7017 6189 6200 7110 6134"  
    ## [103] "                  6222 8025 7073 7083 6553 6249 6735 6550 6734 6692 6062 8096 7289 6229 6008 7244"  
    ## [104] "                  6597 8162 8241 8156 6744 6761 6231 7085 6238 6809 8108 6153 6820 7303 6281 6313"  
    ## [105] "                  6623 8109 8271 6493 8106 6076 7304 8046 8275 6702 7097 7185 6070 8263 6690 7330"  
    ## [106] "                  8105 6768 7313 6754 6248 8112 7134 7029 8041 6111 7092 6728 6084"                 
    ## [107] "      AC1         6092 8274 8048 7255 6499 8022 8057 8280 7272 7315 7153 8281 8133 7126 6974 6517"  
    ## [108] "                  7221 8318 6932 6957 6959 8312 6483 7201 8304 6198 7049 6191 7329 6056 7000 8337"  
    ## [109] "                  7017 6625 7219 8015 7111 6133 6222 8025 7073 6730 8350 6739 8180 6553 6550 6616"  
    ## [110] "                  7224 6229 6008 7244 8162 8241 7165 6235 6744 6256 6255 6481 8158 7085 6531 6238"  
    ## [111] "                  7180 6800 8108 6773 6281 8247 8207 6313 8107 7004 7304 6702 6857 8052 8151 7119"  
    ## [112] "                  6908 8117 8290 8058 6463 6914 7168 6346 6770 6981 6695 7280 8037 7327 8149"       
    ## [113] "      AC2         6092 8274 8055 8048 8022 6882 7272 6159 7153 8281 6974 8315 8067 6927 6951 6921"  
    ## [114] "                  7221 8318 6395 6935 6957 6959 6430 8304 6198 8071 6028 8023 6221 6175 7049 7017"  
    ## [115] "                  7219 6133 7110 6405 8089 6402 7070 7073 8180 6249 6735 7224 6062 7289 6310 7226"  
    ## [116] "                  6679 6008 7244 6626 8162 8102 8241 8156 8193 6744 6265 6256 6761 7142 8270 7085"  
    ## [117] "                  6238 7180 8108 7227 6281 6313 6102 6623 8107 8271 8106 6297 8265 6825 8004 6305"  
    ## [118] "                  7097 6445 7185 7171 6325 6233 8062 6346 6752 6551 6140 7223 6002 6705 7192 8040"  
    ## [119] "                  7138"                                                                             
    ## [120] "      AC3         6849 6092 8274 8048 7255 8022 6882 8279 8042 8281 6897 6355 7108 6204 6935 6957"  
    ## [121] "                  6959 7201 8304 6135 6056 7041 7016 8337 6177 8335 8066 8015 7270 6222 7070 7073"  
    ## [122] "                  9006 6165 8180 6553 6692 7224 8045 6681 8097 7244 6597 8162 8241 6618 7260 7298"  
    ## [123] "                  8193 6744 6765 8167 7142 8270 8165 7085 6531 6238 8108 6820 7182 6281 8247 6283"  
    ## [124] "                  8107 6660 8271 8118 6076 6297 7304 8161 8275 8020 6702 6837 6806 6045 8268 6726"  
    ## [125] "                  6340 6336 6342 6941 6546 8329 6630 6491 8079"                                     
    ## [126] "      AC4         6849 6092 8122 8274 8048 6301 8022 8297 8281 6897 7126 6107 7108 6276 6506 6959"  
    ## [127] "                  8312 6483 8304 8023 7027 6191 7000 7016 6189 7219 6200 6134 6222 7070 7073 6093"  
    ## [128] "                  6201 8180 6553 6249 8045 6062 6310 6681 6229 6008 7244 8162 8241 8156 6236 7142"  
    ## [129] "                  8165 8158 6531 6800 7167 8108 6820 6281 7300 6313 7004 6660 8271 6076 6297 8189"  
    ## [130] "                  8265 6309 6825 8266 8275 6702 7097 6837 6045 8268 8263 6071 6340 6336 8019 6975"  
    ## [131] "                  7313 6342 6945 6357 6981 7264 6980 7045 8213 7166 8037 6683 8329 6511 6979"       
    ## [132] ""                                                                                                   
    ## [133] "  10 WARNING(S) FOUND IN THE INPUT INSTRUCTIONS"                                                    
    ## [134] ""                                                                                                   
    ## [135] ""                                                                                                   
    ## [136] ""                                                                                                   
    ## [137] "Covariates_BC2_WC3_MLPA"                                                                            
    ## [138] ""                                                                                                   
    ## [139] "SUMMARY OF ANALYSIS"                                                                                
    ## [140] ""                                                                                                   
    ## [141] "Number of groups                                                 1"                                 
    ## [142] "Number of observations                                        6148"                                 
    ## [143] ""                                                                                                   
    ## [144] "Number of dependent variables                                    8"                                 
    ## [145] "Number of independent variables                                  0"                                 
    ## [146] "Number of continuous latent variables                            0"                                 
    ## [147] "Number of categorical latent variables                           2"                                 
    ## [148] ""                                                                                                   
    ## [149] "Observed dependent variables"                                                                       
    ## [150] ""                                                                                                   
    ## [151] "  Continuous"                                                                                       
    ## [152] "   EX1         EX2         EX3         EX4         AC1         AC2"                                 
    ## [153] "   AC3         AC4"                                                                                 
    ## [154] ""                                                                                                   
    ## [155] "Categorical latent variables"                                                                       
    ## [156] "   BC          WC"                                                                                  
    ## [157] ""                                                                                                   
    ## [158] "Variables with special functions"                                                                   
    ## [159] ""                                                                                                   
    ## [160] "  Cluster variable      SCHID"                                                                      
    ## [161] ""                                                                                                   
    ## [162] "  Within variables"                                                                                 
    ## [163] "   EX1         EX2         EX3         EX4         AC1         AC2"                                 
    ## [164] "   AC3         AC4"                                                                                 
    ## [165] ""                                                                                                   
    ## [166] ""                                                                                                   
    ## [167] "Estimator                                                      MLR"                                 
    ## [168] "Information matrix                                        OBSERVED"                                 
    ## [169] "Optimization Specifications for the Quasi-Newton Algorithm for"                                     
    ## [170] "Continuous Outcomes"                                                                                
    ## [171] "  Maximum number of iterations                                 100"                                 
    ## [172] "  Convergence criterion                                  0.100D-05"                                 
    ## [173] "Optimization Specifications for the EM Algorithm"                                                   
    ## [174] "  Maximum number of iterations                                 500"                                 
    ## [175] "  Convergence criteria"                                                                             
    ## [176] "    Loglikelihood change                                 0.100D-02"                                 
    ## [177] "    Relative loglikelihood change                        0.100D-05"                                 
    ## [178] "    Derivative                                           0.100D-02"                                 
    ## [179] "Optimization Specifications for the M step of the EM Algorithm for"                                 
    ## [180] "Categorical Latent variables"                                                                       
    ## [181] "  Number of M step iterations                                    1"                                 
    ## [182] "  M step convergence criterion                           0.100D-02"                                 
    ## [183] "  Basis for M step termination                           ITERATION"                                 
    ## [184] "Optimization Specifications for the M step of the EM Algorithm for"                                 
    ## [185] "Censored, Binary or Ordered Categorical (Ordinal), Unordered"                                       
    ## [186] "Categorical (Nominal) and Count Outcomes"                                                           
    ## [187] "  Number of M step iterations                                    1"                                 
    ## [188] "  M step convergence criterion                           0.100D-02"                                 
    ## [189] "  Basis for M step termination                           ITERATION"                                 
    ## [190] "  Maximum value for logit thresholds                            15"                                 
    ## [191] "  Minimum value for logit thresholds                           -15"                                 
    ## [192] "  Minimum expected cell size for chi-square              0.100D-01"                                 
    ## [193] "Maximum number of iterations for H1                           2000"                                 
    ## [194] "Convergence criterion for H1                             0.100D-03"                                 
    ## [195] "Optimization algorithm                                         EMA"                                 
    ## [196] "Integration Specifications"                                                                         
    ## [197] "  Type                                                    STANDARD"                                 
    ## [198] "  Number of integration points                                  15"                                 
    ## [199] "  Dimensions of numerical integration                            0"                                 
    ## [200] "  Adaptive quadrature                                           ON"                                 
    ## [201] "Random Starts Specifications"                                                                       
    ## [202] "  Random seed for analysis                                  399671"                                 
    ## [203] "Parameterization                                             LOGIT"                                 
    ## [204] "Cholesky                                                       OFF"                                 
    ## [205] ""                                                                                                   
    ## [206] "Input data file(s)"                                                                                 
    ## [207] "  LPA_data.dat"                                                                                     
    ## [208] "Input data format  FREE"                                                                            
    ## [209] ""                                                                                                   
    ## [210] ""                                                                                                   
    ## [211] "SUMMARY OF DATA"                                                                                    
    ## [212] ""                                                                                                   
    ## [213] "     Number of missing data patterns             6"                                                 
    ## [214] "     Number of y missing data patterns           6"                                                 
    ## [215] "     Number of u missing data patterns           0"                                                 
    ## [216] "     Number of clusters                       1657"                                                 
    ## [217] ""                                                                                                   
    ## [218] ""                                                                                                   
    ## [219] ""                                                                                                   
    ## [220] "COVARIANCE COVERAGE OF DATA"                                                                        
    ## [221] ""                                                                                                   
    ## [222] "Minimum covariance coverage value   0.100"                                                          
    ## [223] ""                                                                                                   
    ## [224] ""                                                                                                   
    ## [225] "     PROPORTION OF DATA PRESENT FOR Y"                                                              
    ## [226] ""                                                                                                   
    ## [227] ""                                                                                                   
    ## [228] "           Covariance Coverage"                                                                     
    ## [229] "              EX1           EX2           EX3           EX4           AC1"                          
    ## [230] "              ________      ________      ________      ________      ________"                     
    ## [231] " EX1            1.000"                                                                              
    ## [232] " EX2            1.000         1.000"                                                                
    ## [233] " EX3            1.000         1.000         1.000"                                                  
    ## [234] " EX4            1.000         1.000         1.000         1.000"                                    
    ## [235] " AC1            0.999         0.999         0.999         0.999         0.999"                      
    ## [236] " AC2            1.000         1.000         1.000         0.999         0.999"                      
    ## [237] " AC3            1.000         1.000         1.000         1.000         0.999"                      
    ## [238] " AC4            1.000         1.000         1.000         1.000         0.999"                      
    ## [239] ""                                                                                                   
    ## [240] ""                                                                                                   
    ## [241] "           Covariance Coverage"                                                                     
    ## [242] "              AC2           AC3           AC4"                                                      
    ## [243] "              ________      ________      ________"                                                 
    ## [244] " AC2            1.000"                                                                              
    ## [245] " AC3            0.999         1.000"                                                                
    ## [246] " AC4            0.999         1.000         1.000"                                                  
    ## [247] ""                                                                                                   
    ## [248] ""                                                                                                   
    ## [249] ""                                                                                                   
    ## [250] "UNIVARIATE SAMPLE STATISTICS"                                                                       
    ## [251] ""                                                                                                   
    ## [252] ""                                                                                                   
    ## [253] "     UNIVARIATE HIGHER-ORDER MOMENT DESCRIPTIVE STATISTICS"                                         
    ## [254] ""                                                                                                   
    ## [255] "         Variable/         Mean/     Skewness/   Minimum/ % with                Percentiles"        
    ## [256] "        Sample Size      Variance    Kurtosis    Maximum  Min/Max      20%/60%    40%/80%    Median"
    ## [257] ""                                                                                                   
    ## [258] "     EX1                   3.232      -0.436       1.000   13.27%       2.000      3.000      3.000"
    ## [259] "            6148.000       1.514      -0.793       5.000   13.63%       4.000      4.000"           
    ## [260] "     EX2                   2.973      -0.117       1.000   17.21%       2.000      3.000      3.000"
    ## [261] "            6148.000       1.611      -1.065       5.000   11.47%       3.000      4.000"           
    ## [262] "     EX3                   3.196      -0.317       1.000   15.65%       2.000      3.000      3.000"
    ## [263] "            6148.000       1.741      -1.049       5.000   17.42%       4.000      4.000"           
    ## [264] "     EX4                   3.212      -0.320       1.000   15.05%       2.000      3.000      3.000"
    ## [265] "            6147.000       1.730      -1.037       5.000   17.88%       4.000      4.000"           
    ## [266] "     AC1                   2.753       0.129       1.000   22.92%       1.000      2.000      3.000"
    ## [267] "            6144.000       1.691      -1.100       5.000   10.63%       3.000      4.000"           
    ## [268] "     AC2                   3.438      -0.585       1.000   13.03%       2.000      3.000      4.000"
    ## [269] "            6145.000       1.690      -0.745       5.000   22.73%       4.000      5.000"           
    ## [270] "     AC3                   2.327       0.615       1.000   32.85%       1.000      2.000      2.000"
    ## [271] "            6146.000       1.529      -0.642       5.000    6.88%       2.000      3.000"           
    ## [272] "     AC4                   2.538       0.364       1.000   27.49%       1.000      2.000      2.000"
    ## [273] "            6147.000       1.618      -0.926       5.000    8.64%       3.000      4.000"           
    ## [274] ""                                                                                                   
    ## [275] ""                                                                                                   
    ## [276] "THE MODEL ESTIMATION TERMINATED NORMALLY"                                                           
    ## [277] ""                                                                                                   
    ## [278] ""                                                                                                   
    ## [279] ""                                                                                                   
    ## [280] "MODEL FIT INFORMATION"                                                                              
    ## [281] ""                                                                                                   
    ## [282] "Number of Free Parameters                       13"                                                 
    ## [283] ""                                                                                                   
    ## [284] "Loglikelihood"                                                                                      
    ## [285] ""                                                                                                   
    ## [286] "          H0 Value                      -68308.521"                                                 
    ## [287] "          H0 Scaling Correction Factor      1.3505"                                                 
    ## [288] "            for MLR"                                                                                
    ## [289] ""                                                                                                   
    ## [290] "Information Criteria"                                                                               
    ## [291] ""                                                                                                   
    ## [292] "          Akaike (AIC)                  136643.041"                                                 
    ## [293] "          Bayesian (BIC)                136730.452"                                                 
    ## [294] "          Sample-Size Adjusted BIC      136689.141"                                                 
    ## [295] "            (n* = (n + 2) / 24)"                                                                    
    ## [296] ""                                                                                                   
    ## [297] ""                                                                                                   
    ## [298] ""                                                                                                   
    ## [299] "MODEL RESULTS USE THE LATENT CLASS VARIABLE ORDER"                                                  
    ## [300] ""                                                                                                   
    ## [301] "   BC  WC"                                                                                          
    ## [302] ""                                                                                                   
    ## [303] "  Latent Class Variable Patterns"                                                                   
    ## [304] ""                                                                                                   
    ## [305] "         BC        WC"                                                                              
    ## [306] "      Class     Class"                                                                              
    ## [307] ""                                                                                                   
    ## [308] "         1         1"                                                                               
    ## [309] "         1         2"                                                                               
    ## [310] "         1         3"                                                                               
    ## [311] "         2         1"                                                                               
    ## [312] "         2         2"                                                                               
    ## [313] "         2         3"                                                                               
    ## [314] ""                                                                                                   
    ## [315] ""                                                                                                   
    ## [316] "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS"                                   
    ## [317] "BASED ON ESTIMATED POSTERIOR PROBABILITIES"                                                         
    ## [318] ""                                                                                                   
    ## [319] "  Latent Class"                                                                                     
    ## [320] "    Pattern"                                                                                        
    ## [321] ""                                                                                                   
    ## [322] "    1  1       1113.78031          0.18116"                                                         
    ## [323] "    1  2        868.95203          0.14134"                                                         
    ## [324] "    1  3        618.05697          0.10053"                                                         
    ## [325] "    2  1       1497.94825          0.24365"                                                         
    ## [326] "    2  2        484.19641          0.07876"                                                         
    ## [327] "    2  3       1565.06602          0.25457"                                                         
    ## [328] ""                                                                                                   
    ## [329] ""                                                                                                   
    ## [330] "FINAL CLASS COUNTS AND PROPORTIONS FOR EACH LATENT CLASS VARIABLE"                                  
    ## [331] "BASED ON ESTIMATED POSTERIOR PROBABILITIES"                                                         
    ## [332] ""                                                                                                   
    ## [333] "  Latent Class"                                                                                     
    ## [334] "    Variable    Class"                                                                              
    ## [335] ""                                                                                                   
    ## [336] "    BC             1      2600.78931          0.42303"                                              
    ## [337] "                   2      3547.21069          0.57697"                                              
    ## [338] "    WC             1      2611.72852          0.42481"                                              
    ## [339] "                   2      1353.14844          0.22010"                                              
    ## [340] "                   3      2183.12305          0.35509"                                              
    ## [341] ""                                                                                                   
    ## [342] ""                                                                                                   
    ## [343] "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS"                                   
    ## [344] "BASED ON THEIR MOST LIKELY LATENT CLASS PATTERN"                                                    
    ## [345] ""                                                                                                   
    ## [346] "Class Counts and Proportions"                                                                       
    ## [347] ""                                                                                                   
    ## [348] "  Latent Class"                                                                                     
    ## [349] "    Pattern"                                                                                        
    ## [350] ""                                                                                                   
    ## [351] "    1  1              891          0.14493"                                                         
    ## [352] "    1  2             1066          0.17339"                                                         
    ## [353] "    1  3              395          0.06425"                                                         
    ## [354] "    2  1             1694          0.27554"                                                         
    ## [355] "    2  2              290          0.04717"                                                         
    ## [356] "    2  3             1812          0.29473"                                                         
    ## [357] ""                                                                                                   
    ## [358] ""                                                                                                   
    ## [359] "FINAL CLASS COUNTS AND PROPORTIONS FOR EACH LATENT CLASS VARIABLE"                                  
    ## [360] "BASED ON THEIR MOST LIKELY LATENT CLASS PATTERN"                                                    
    ## [361] ""                                                                                                   
    ## [362] "  Latent Class"                                                                                     
    ## [363] "    Variable    Class"                                                                              
    ## [364] ""                                                                                                   
    ## [365] "    BC             1            2352          0.38256"                                              
    ## [366] "                   2            3796          0.61744"                                              
    ## [367] "    WC             1            2585          0.42046"                                              
    ## [368] "                   2            1356          0.22056"                                              
    ## [369] "                   3            2207          0.35898"                                              
    ## [370] ""                                                                                                   
    ## [371] ""                                                                                                   
    ## [372] "CLASSIFICATION QUALITY"                                                                             
    ## [373] ""                                                                                                   
    ## [374] "     Entropy                         0.638"                                                         
    ## [375] ""                                                                                                   
    ## [376] ""                                                                                                   
    ## [377] "Average Latent Class Probabilities for Most Likely Latent Class Pattern (Row)"                      
    ## [378] "by Latent Class Pattern (Column)"                                                                   
    ## [379] ""                                                                                                   
    ## [380] "  Latent Class Variable Patterns"                                                                   
    ## [381] ""                                                                                                   
    ## [382] "  Latent Class         BC        WC"                                                                
    ## [383] "   Pattern No.      Class     Class"                                                                
    ## [384] ""                                                                                                   
    ## [385] "         1             1         1"                                                                 
    ## [386] "         2             1         2"                                                                 
    ## [387] "         3             1         3"                                                                 
    ## [388] "         4             2         1"                                                                 
    ## [389] "         5             2         2"                                                                 
    ## [390] "         6             2         3"                                                                 
    ## [391] ""                                                                                                   
    ## [392] "           1        2        3        4        5        6"                                          
    ## [393] ""                                                                                                   
    ## [394] "    1   0.667    0.013    0.024    0.277    0.002    0.017"                                         
    ## [395] "    2   0.017    0.716    0.000    0.013    0.253    0.000"                                         
    ## [396] "    3   0.040    0.000    0.623    0.011    0.000    0.325"                                         
    ## [397] "    4   0.262    0.008    0.008    0.671    0.010    0.041"                                         
    ## [398] "    5   0.007    0.275    0.000    0.042    0.676    0.000"                                         
    ## [399] "    6   0.022    0.000    0.186    0.047    0.000    0.746"                                         
    ## [400] ""                                                                                                   
    ## [401] ""                                                                                                   
    ## [402] "MODEL RESULTS"                                                                                      
    ## [403] ""                                                                                                   
    ## [404] "                                                    Two-Tailed"                                     
    ## [405] "                    Estimate       S.E.  Est./S.E.    P-Value"                                      
    ## [406] ""                                                                                                   
    ## [407] "Within Level"                                                                                       
    ## [408] ""                                                                                                   
    ## [409] "Latent Class Pattern 1 1"                                                                           
    ## [410] ""                                                                                                   
    ## [411] " Means"                                                                                             
    ## [412] "    EX1                3.218      0.000    999.000    999.000"                                      
    ## [413] "    EX2                2.856      0.000    999.000    999.000"                                      
    ## [414] "    EX3                3.125      0.000    999.000    999.000"                                      
    ## [415] "    EX4                3.188      0.000    999.000    999.000"                                      
    ## [416] "    AC1                2.681      0.000    999.000    999.000"                                      
    ## [417] "    AC2                3.477      0.000    999.000    999.000"                                      
    ## [418] "    AC3                2.067      0.000    999.000    999.000"                                      
    ## [419] "    AC4                2.390      0.000    999.000    999.000"                                      
    ## [420] ""                                                                                                   
    ## [421] " Variances"                                                                                         
    ## [422] "    EX1                0.498      0.012     40.403      0.000"                                      
    ## [423] "    EX2                0.585      0.014     42.098      0.000"                                      
    ## [424] "    EX3                0.546      0.013     42.051      0.000"                                      
    ## [425] "    EX4                0.701      0.016     44.434      0.000"                                      
    ## [426] "    AC1                1.269      0.021     59.068      0.000"                                      
    ## [427] "    AC2                0.651      0.017     38.628      0.000"                                      
    ## [428] "    AC3                0.932      0.018     51.093      0.000"                                      
    ## [429] "    AC4                1.144      0.021     54.549      0.000"                                      
    ## [430] ""                                                                                                   
    ## [431] "Latent Class Pattern 1 2"                                                                           
    ## [432] ""                                                                                                   
    ## [433] " Means"                                                                                             
    ## [434] "    EX1                1.554      0.000    999.000    999.000"                                      
    ## [435] "    EX2                1.371      0.000    999.000    999.000"                                      
    ## [436] "    EX3                1.419      0.000    999.000    999.000"                                      
    ## [437] "    EX4                1.532      0.000    999.000    999.000"                                      
    ## [438] "    AC1                1.723      0.000    999.000    999.000"                                      
    ## [439] "    AC2                1.703      0.000    999.000    999.000"                                      
    ## [440] "    AC3                1.280      0.000    999.000    999.000"                                      
    ## [441] "    AC4                1.513      0.000    999.000    999.000"                                      
    ## [442] ""                                                                                                   
    ## [443] " Variances"                                                                                         
    ## [444] "    EX1                0.498      0.012     40.403      0.000"                                      
    ## [445] "    EX2                0.585      0.014     42.098      0.000"                                      
    ## [446] "    EX3                0.546      0.013     42.051      0.000"                                      
    ## [447] "    EX4                0.701      0.016     44.434      0.000"                                      
    ## [448] "    AC1                1.269      0.021     59.068      0.000"                                      
    ## [449] "    AC2                0.651      0.017     38.628      0.000"                                      
    ## [450] "    AC3                0.932      0.018     51.093      0.000"                                      
    ## [451] "    AC4                1.144      0.021     54.549      0.000"                                      
    ## [452] ""                                                                                                   
    ## [453] "Latent Class Pattern 1 3"                                                                           
    ## [454] ""                                                                                                   
    ## [455] " Means"                                                                                             
    ## [456] "    EX1                4.288      0.000    999.000    999.000"                                      
    ## [457] "    EX2                4.105      0.000    999.000    999.000"                                      
    ## [458] "    EX3                4.381      0.000    999.000    999.000"                                      
    ## [459] "    EX4                4.283      0.000    999.000    999.000"                                      
    ## [460] "    AC1                3.477      0.000    999.000    999.000"                                      
    ## [461] "    AC2                4.467      0.000    999.000    999.000"                                      
    ## [462] "    AC3                3.287      0.000    999.000    999.000"                                      
    ## [463] "    AC4                3.350      0.000    999.000    999.000"                                      
    ## [464] ""                                                                                                   
    ## [465] " Variances"                                                                                         
    ## [466] "    EX1                0.498      0.012     40.403      0.000"                                      
    ## [467] "    EX2                0.585      0.014     42.098      0.000"                                      
    ## [468] "    EX3                0.546      0.013     42.051      0.000"                                      
    ## [469] "    EX4                0.701      0.016     44.434      0.000"                                      
    ## [470] "    AC1                1.269      0.021     59.068      0.000"                                      
    ## [471] "    AC2                0.651      0.017     38.628      0.000"                                      
    ## [472] "    AC3                0.932      0.018     51.093      0.000"                                      
    ## [473] "    AC4                1.144      0.021     54.549      0.000"                                      
    ## [474] ""                                                                                                   
    ## [475] "Latent Class Pattern 2 1"                                                                           
    ## [476] ""                                                                                                   
    ## [477] " Means"                                                                                             
    ## [478] "    EX1                3.218      0.000    999.000    999.000"                                      
    ## [479] "    EX2                2.856      0.000    999.000    999.000"                                      
    ## [480] "    EX3                3.125      0.000    999.000    999.000"                                      
    ## [481] "    EX4                3.188      0.000    999.000    999.000"                                      
    ## [482] "    AC1                2.681      0.000    999.000    999.000"                                      
    ## [483] "    AC2                3.477      0.000    999.000    999.000"                                      
    ## [484] "    AC3                2.067      0.000    999.000    999.000"                                      
    ## [485] "    AC4                2.390      0.000    999.000    999.000"                                      
    ## [486] ""                                                                                                   
    ## [487] " Variances"                                                                                         
    ## [488] "    EX1                0.498      0.012     40.403      0.000"                                      
    ## [489] "    EX2                0.585      0.014     42.098      0.000"                                      
    ## [490] "    EX3                0.546      0.013     42.051      0.000"                                      
    ## [491] "    EX4                0.701      0.016     44.434      0.000"                                      
    ## [492] "    AC1                1.269      0.021     59.068      0.000"                                      
    ## [493] "    AC2                0.651      0.017     38.628      0.000"                                      
    ## [494] "    AC3                0.932      0.018     51.093      0.000"                                      
    ## [495] "    AC4                1.144      0.021     54.549      0.000"                                      
    ## [496] ""                                                                                                   
    ## [497] "Latent Class Pattern 2 2"                                                                           
    ## [498] ""                                                                                                   
    ## [499] " Means"                                                                                             
    ## [500] "    EX1                1.554      0.000    999.000    999.000"                                      
    ## [501] "    EX2                1.371      0.000    999.000    999.000"                                      
    ## [502] "    EX3                1.419      0.000    999.000    999.000"                                      
    ## [503] "    EX4                1.532      0.000    999.000    999.000"                                      
    ## [504] "    AC1                1.723      0.000    999.000    999.000"                                      
    ## [505] "    AC2                1.703      0.000    999.000    999.000"                                      
    ## [506] "    AC3                1.280      0.000    999.000    999.000"                                      
    ## [507] "    AC4                1.513      0.000    999.000    999.000"                                      
    ## [508] ""                                                                                                   
    ## [509] " Variances"                                                                                         
    ## [510] "    EX1                0.498      0.012     40.403      0.000"                                      
    ## [511] "    EX2                0.585      0.014     42.098      0.000"                                      
    ## [512] "    EX3                0.546      0.013     42.051      0.000"                                      
    ## [513] "    EX4                0.701      0.016     44.434      0.000"                                      
    ## [514] "    AC1                1.269      0.021     59.068      0.000"                                      
    ## [515] "    AC2                0.651      0.017     38.628      0.000"                                      
    ## [516] "    AC3                0.932      0.018     51.093      0.000"                                      
    ## [517] "    AC4                1.144      0.021     54.549      0.000"                                      
    ## [518] ""                                                                                                   
    ## [519] "Latent Class Pattern 2 3"                                                                           
    ## [520] ""                                                                                                   
    ## [521] " Means"                                                                                             
    ## [522] "    EX1                4.288      0.000    999.000    999.000"                                      
    ## [523] "    EX2                4.105      0.000    999.000    999.000"                                      
    ## [524] "    EX3                4.381      0.000    999.000    999.000"                                      
    ## [525] "    EX4                4.283      0.000    999.000    999.000"                                      
    ## [526] "    AC1                3.477      0.000    999.000    999.000"                                      
    ## [527] "    AC2                4.467      0.000    999.000    999.000"                                      
    ## [528] "    AC3                3.287      0.000    999.000    999.000"                                      
    ## [529] "    AC4                3.350      0.000    999.000    999.000"                                      
    ## [530] ""                                                                                                   
    ## [531] " Variances"                                                                                         
    ## [532] "    EX1                0.498      0.012     40.403      0.000"                                      
    ## [533] "    EX2                0.585      0.014     42.098      0.000"                                      
    ## [534] "    EX3                0.546      0.013     42.051      0.000"                                      
    ## [535] "    EX4                0.701      0.016     44.434      0.000"                                      
    ## [536] "    AC1                1.269      0.021     59.068      0.000"                                      
    ## [537] "    AC2                0.651      0.017     38.628      0.000"                                      
    ## [538] "    AC3                0.932      0.018     51.093      0.000"                                      
    ## [539] "    AC4                1.144      0.021     54.549      0.000"                                      
    ## [540] ""                                                                                                   
    ## [541] "Between Level"                                                                                      
    ## [542] ""                                                                                                   
    ## [543] "Categorical Latent Variables"                                                                       
    ## [544] ""                                                                                                   
    ## [545] "Within Level"                                                                                       
    ## [546] ""                                                                                                   
    ## [547] " Intercepts"                                                                                        
    ## [548] "    WC#1              -0.044      0.169     -0.261      0.794"                                      
    ## [549] "    WC#2              -1.174      0.475     -2.469      0.014"                                      
    ## [550] ""                                                                                                   
    ## [551] "Between Level"                                                                                      
    ## [552] ""                                                                                                   
    ## [553] " WC#1       ON"                                                                                     
    ## [554] "    BC#1               0.633      0.161      3.938      0.000"                                      
    ## [555] ""                                                                                                   
    ## [556] " WC#2       ON"                                                                                     
    ## [557] "    BC#1               1.514      0.144     10.497      0.000"                                      
    ## [558] ""                                                                                                   
    ## [559] " Means"                                                                                             
    ## [560] "    BC#1              -0.216      1.202     -0.179      0.858"                                      
    ## [561] ""                                                                                                   
    ## [562] ""                                                                                                   
    ## [563] "QUALITY OF NUMERICAL RESULTS"                                                                       
    ## [564] ""                                                                                                   
    ## [565] "     Condition Number for the Information Matrix              0.206E-03"                            
    ## [566] "       (ratio of smallest to largest eigenvalue)"                                                   
    ## [567] ""                                                                                                   
    ## [568] ""                                                                                                   
    ## [569] "SAVEDATA INFORMATION"                                                                               
    ## [570] ""                                                                                                   
    ## [571] ""                                                                                                   
    ## [572] "  Save file"                                                                                        
    ## [573] "    cprob_mlpa.dat"                                                                                 
    ## [574] ""                                                                                                   
    ## [575] "  Order and format of variables"                                                                    
    ## [576] ""                                                                                                   
    ## [577] "    EX1            F10.3"                                                                           
    ## [578] "    EX2            F10.3"                                                                           
    ## [579] "    EX3            F10.3"                                                                           
    ## [580] "    EX4            F10.3"                                                                           
    ## [581] "    AC1            F10.3"                                                                           
    ## [582] "    AC2            F10.3"                                                                           
    ## [583] "    AC3            F10.3"                                                                           
    ## [584] "    AC4            F10.3"                                                                           
    ## [585] "    CPROB1         F10.3"                                                                           
    ## [586] "    CPROB2         F10.3"                                                                           
    ## [587] "    CPROB3         F10.3"                                                                           
    ## [588] "    CPROB4         F10.3"                                                                           
    ## [589] "    CPROB5         F10.3"                                                                           
    ## [590] "    CPROB6         F10.3"                                                                           
    ## [591] "    BC             F10.3"                                                                           
    ## [592] "    WC             F10.3"                                                                           
    ## [593] "    MLCJOINT       F10.3"                                                                           
    ## [594] "    SCHID          I5"                                                                              
    ## [595] ""                                                                                                   
    ## [596] "  Save file format"                                                                                 
    ## [597] "    17F10.3 I5"                                                                                     
    ## [598] ""                                                                                                   
    ## [599] "  Save file record length    10000"                                                                 
    ## [600] ""                                                                                                   
    ## [601] ""                                                                                                   
    ## [602] "     Beginning Time:  14:52:42"                                                                     
    ## [603] "        Ending Time:  14:52:44"                                                                     
    ## [604] "       Elapsed Time:  00:00:02"                                                                     
    ## [605] ""                                                                                                   
    ## [606] ""                                                                                                   
    ## [607] ""                                                                                                   
    ## [608] "MUTHEN & MUTHEN"                                                                                    
    ## [609] "3463 Stoner Ave."                                                                                   
    ## [610] "Los Angeles, CA  90066"                                                                             
    ## [611] ""                                                                                                   
    ## [612] "Tel: (310) 391-9971"                                                                                
    ## [613] "Fax: (310) 391-8971"                                                                                
    ## [614] "Web: www.StatModel.com"                                                                             
    ## [615] "Support: Support@StatModel.com"                                                                     
    ## [616] ""                                                                                                   
    ## [617] "Copyright (c) 1998-2017 Muthen & Muthen"
