For this analysis, we need to install/load the tseries R Package.

    ##install.packages("tseries")
    library(tseries)

I have been assigned Penumbra, Inc. Ticker Symbol "PEN"

Step 1: Load the closing quotes

    tickerdata<-get.hist.quote('PEN', quote="Close")

    ## Warning in download.file(url, destfile, method = method, quiet = quiet):
    ## downloaded length 12495 != reported length 200

    ## time series starts 2015-09-21

    head(tickerdata)

    ##            Close
    ## 2015-09-21 40.00
    ## 2015-09-22 40.36
    ## 2015-09-23 41.67
    ## 2015-09-24 41.68
    ## 2015-09-25 40.28
    ## 2015-09-28 39.04

Step 2: Compute the Log Returns

    tickerLogReturn<-log(lag(tickerdata))-log(tickerdata)
    head(tickerLogReturn)

    ##                   Close
    ## 2015-09-21  0.008959766
    ## 2015-09-22  0.031942177
    ## 2015-09-23  0.000240000
    ## 2015-09-24 -0.034166354
    ## 2015-09-25 -0.031268256
    ## 2015-09-28  0.014242281

Step3: Compute Volatility

    TickerVolatility<- sd(tickerLogReturn) * sqrt(250) * 100
    TickerVolatility

    ## [1] 48.51677

    Vol <- function(d,logrets) {
      var = 0
      lam = 0
      varlist <- c()
      for (r in logrets) {
        lam = lam * (1-1/d) + 1
      var = (1-1/lam)*var + (1/lam) * r^2
        varlist <- c(varlist, var)
        
      }
      sqrt(varlist)
    }

    volest  <- Vol(10,tickerLogReturn)
    head(volest)

    ## [1] 0.008959766 0.023979732 0.019048924 0.024429275 0.026264169 0.024204929

    volest2 <- Vol(30,tickerLogReturn)
    head(volest2)

    ## [1] 0.008959766 0.023627449 0.019127147 0.024010880 0.025734976 0.024064105

    volest3 <- Vol(100,tickerLogReturn)
    head(volest3)

    ## [1] 0.008959766 0.023508550 0.019146788 0.023870745 0.025554622 0.024002254

Step 4: Plot Volatility

    plot(volest,type="l")
    lines(volest2, type="l", col="red")
    lines(volest3, type="l", col="blue")

![](MSDS6306_Mod9.5_PlottingVolatility_files/figure-markdown_strict/plotVolatility-1.png)<!-- -->
