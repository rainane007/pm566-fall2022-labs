Lab 04
================
Yuhong Hu
2022-09-21

# Read in data

``` r
if (!file.exists("/Users/rain/Desktop/PhD course/pm 566/pm566-fall2022-labs/lab04/met_all.gz"))
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
    destfile = "met_all.gz",
    method   = "libcurl",
    timeout  = 60
    )
met <- data.table::fread("/Users/rain/Desktop/PhD course/pm 566/pm566-fall2022-labs/lab04/met_all.gz")


# Prepare the data
## Remove temperatures less than -17C and change elev 9999 to NA

met <- met[temp > -17][elev == 9999.0,elev := NA]
```

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

merge data with stations

``` r
met <- merge(
  # Data
  x     = met,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  ) 
```

# Question 1: Representative station for the US

``` r
# Compute mean temperature, wind speed, and ap for each weather station, and pick the weather station with the average value closest to th median for the US.

station_average <-
  met[, .(temp = mean(temp,na.rm=T),
          windsp=mean(wind.sp,na.rm=T),
          atm.press=mean(atm.press,na.rm=T)),by=USAFID]

# now get the median value for each variable

statmedian <-
  met[,.(
  temp50=median(temp,na.rm = T),
  windsp50=median(wind.sp,na.rm=T),
  atmpress50=median(atm.press,na.rm=T)
)]

# A helpful function 'which.min()' we might want to use
station_average[, temp_dist50:=abs(temp-statmedian$temp50)][order(temp_dist50)]
```

    ##       USAFID      temp    windsp atm.press  temp_dist50
    ##    1: 725830 23.498627  2.966084  1012.675  0.001372998
    ##    2: 720549 23.496276  1.953681       NaN  0.003724395
    ##    3: 724769 23.489852  3.214057  1013.090  0.010148233
    ##    4: 723114 23.520054  1.846268       NaN  0.020053957
    ##    5: 726813 23.478088  2.435372  1012.315  0.021912351
    ##   ---                                                  
    ## 1584: 722788 36.852459  3.393852       NaN 13.352459016
    ## 1585: 722787 37.258907  2.847381       NaN 13.758907363
    ## 1586: 723805 37.625391  3.532935  1005.207 14.125390625
    ## 1587: 726130  9.189602 12.239908       NaN 14.310397554
    ## 1588: 720385  8.044959  7.298963       NaN 15.455040872

``` r
station_average[which.min(temp_dist50)]
```

    ##    USAFID     temp   windsp atm.press temp_dist50
    ## 1: 725830 23.49863 2.966084  1012.675 0.001372998

# Question 2: Representative station per state

# Question 3: In the middle?
