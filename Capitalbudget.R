##Capital Budget Code##
library(FinCal)

#NPV Project 1 & Project 2
NPV.Project1 <- npv(r=.06, cf=c(-1000, 1250, 10, 10, 20, 20)) 
NPV.Project2 <- npv(r=.06, cf=c(-1000,-10, 0, 10, 20, 2000))

#Solution: NPV2 with 509 > NPV1 with 227 -> Pick Project 2


#IRR Project 1 & Project 2
IRR.Project1 <- irr(cf=c(-1000,1250, 10, 10, 20, 20))
IRR.Project2 <- irr(cf=c(-1000,-10, 0, 10, 20, 2000))

#Solution: IRR.Project1 with 28% > IRR.Project2 with 15% -> Pick Project 1

#PB Project 1 & Project 2
#PB Project 1
cf0 <- -1000
cf <- c(1250, 10, 10, 20, 20)
t <- 5

temp <- 0

#Calculating the Payback period
for (i in 1:5){
  temp[i]=sum(cf[1:i])
}
for (i in 1:5){
  if ((temp[i]+cf0) > 0){
    payback.Project1 <- i
    break
  }
  print(payback.Project1)
}
#Solution: 1


#PB Project 2
cf0 <- -1000
cf <- c(-10, 0, 10, 20, 2000)
t <- 5

temp <- 0

#Calculating the Payback period
for (i in 1:5){
  temp[i]=sum(cf[1:i])
}
for (i in 1:5){
  if ((temp[i]+cf0) > 0){
    payback.Project2 <- i
    break
  } 
  print(payback.Project2)
}







dcf <- function(x, r, t0=FALSE){
  # calculates discounted cash flows (DCF) given cash flow and discount rate
  #
  # x - cash flows vector
  # r - vector or discount rates, in decimals. Single values will be recycled
  # t0 - cash flow starts in year 0, default is FALSE, i.e. discount rate in first period is zero.
  if(length(r)==1){
    r <- rep(r, length(x))
    if(t0==TRUE){r[1]<-0}
  }
  x/cumprod(1+r)
}


npv <- function(x, r, t0=FALSE){
  # calculates net present value (NPV) given cash flow and discount rate
  #
  # x - cash flows vector
  # r - discount rate, in decimals
  # t0 - cash flow starts in year 0, default is FALSE
  sum(dcf(x, r, t0))
}

pbp <- function(x, ...){
  # calculates payback period (PBP)
  #
  # x - cash flows vector
  # ... - ignored
  i <- match(1, sign(cumsum(x)))
  i-2+(-cumsum(x)[i-1]/x[i])
}

dpbp <- function(x, r, t0=FALSE){
  # calculates discounted payback period (DPBP) given cash flow and discount rate
  #
  # x - cash flows vector
  # r - discount rate, in decimals
  # t0 - cash flow starts in year 0, default is FALSE
  pbp(dcf(x, r, t0))
}

irr <- function(x, t0=FALSE, ...){
  # calculates internal rate of return (IRR) given cash flow 
  #
  # x - cash flows vector
  # t0 - cash flow starts in year 0, default is FALSE
  tryCatch(uniroot(f=function(i){sum(dcf(x, i, t0))}, 
                   interval=c(0,1))$root,
           error=function(e) return(NA)
  )
}




###############

library(dplyr)
library(tidyr)
library(forcats)

cf_df %>%
  summarise_all(funs(NPV=npv,PBP=pbp, DPBP=dpbp, IRR=irr), r=0.06, t0=TRUE) %>% 
  gather(key=key, value = value) %>% 
  separate(key, into = c("Project", "Metric")) %>% 
  spread(key=Project, value=value) %>% 
  mutate(Metric=fct_relevel(Metric, "NPV", "IRR", "PBP", "DPBP"),
         Metric=fct_recode(Metric, 
                           `Net Present Value (NPV), USD mln`="NPV", 
                           `Internal Rate of Return (IRR), %`="IRR",
                           `Payback Period, years` = "PBP",
                           `Discounted Payback Period, years`="DPBP")) %>% 
  arrange(as.numeric(Metric))

#                            Metric    Project1    Project2
#1 Net Present Value (NPV), USD mln 227.3284770 509.3204496
#2 Internal Rate of Return (IRR), %   0.2808485   0.1508404
#3            Payback Period, years   0.8000000   4.4900000
#4 Discounted Payback Period, years   0.8480000   4.6592072

