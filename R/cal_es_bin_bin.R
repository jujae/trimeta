cal_es_bin <- function(y, x, measure, design = "between-subject"){

  Yk    <- array(data=NA, dim = (length(measure)))   # measure of effect
  Sk    <- array(data=NA, dim = (length(measure)))   # standard error
  dfk   <- array(data=NA, dim = (length(measure)))   # degrees of freedom
  n     <- nrow(DF)                                  # nr of pairs of observations
  index <- 1

  # Between-subject design
  DF$exposure <- factor(DF$exposure)
  LVLS        <- levels(DF$exposure)
  V1          <- which(DF$exposure==LVLS[1])
  n10         <- length(which(DF[V1,]$Y==0))
  n11         <- length(which(DF[V1,]$Y==1))
  I1          <- length(V1)                          # sample size of sample with exposure 1
  V2          <- which(DF$exposure==LVLS[2])
  n20         <- length(which(DF[V2,]$Y==0))
  n21         <- length(which(DF[V2,]$Y==1))
  I2          <- length(V2) # sample size of sample with exposure 2

  # Within-subject design
  I           <- nrow(DF)
  V1_w        <- which(DF$outcome1==0)
  n10_w       <- length(which(DF[V1_w,]$outcome2==0))
  n20_w       <- length(which(DF[V1_w,]$outcome2==1))
  V2_w        <- which(DF$outcome1==1)
  n11_w       <- length(which(DF[V1_w,]$outcome2==0))
  n21_w       <- length(which(DF[V1_w,]$outcome2==1))
  # I = n10_w+n20_w+n11_w+n21_w

  if("RiskDiff" %in% measure){

    if(design == "between-subject"){
      p1        <- (n11+1)/(I1+2)  # estimated proportion for y_1i = 1 in sample 1
      p2        <- (n21+1)/(I2+2)  # estimated proportion for y_2i = 1 in sample 2
      Yk[index] <- p2 - p1
      S2        <- p1*(1-p1)/(I1+2) + p2*(1-p2)/(I2+2)
      Sk[index] <- sqrt(S2)              # this is the variance?
      warning("Degrees of freedom for risk difference are usually not reported, therefore NA.")
      dfk[index]<- NA
    }else{
      p11       <- (n11+1)/(I+2)
      p20       <- (n20+1)/(I+2)
      Yk[index] <- p20 - p11
      S2        <- (p11+p20-Yk[index]^2)/(I+2)
      Sk[index] <- sqrt(S2)              # this is the variance? (standard deviation)
      warning("Degrees of freedom for risk difference are usually not reported, therefore NA.")
      dfk[index]<- NA
    }
    index     = index+1
  }

  if("RelativeRisk" %in% measure){

    if(design == "between-subject"){
      if(n11==0 | n21==0){
        p1_RR     <- (n11+0.5)/(I1+1)
        p2_RR     <- (n21+0.5)/(I2+1)
        S2_RR     <- 1/(n11+0.5) - 1/(I1+1) + 1/(n21+0.5) - 1/(I2+1)
        warning("Correction made because some counts are zero")
      }else{
        p1_RR     <- n11/I1
        p2_RR     <- n21/I2
        S2_RR     <- 1/n11 - 1/I1 + 1/n21 - 1/I2
      }
      Yk[index] <- log(p2/p1)
      Sk[index] <- sqrt(S2_RR)      # standard error?
      warning("Degrees of freedom for relative risk are usually not reported, therefore NA.")
      dfk[index]<- NA
    }else{
      if(n11_w==0 | n21_w==0){
        p1_w     <- (n11_w+0.5)/(n10_w+n11_w+1)
        p2_w     <- (n21_w+0.5)/(n20_w+n21_w+1)
        S2_RR_w  <- (n11_w+n20_w+1)/((n10_w+n11_w+1)*(n20_w+n21_w+1))
      }else{
        p1_w     <- n11_w/(n10_w+n11_w)
        p2_w     <- n21_w/(n20_w+n21_w)
        S2_RR_w  <- (n11_w+n20_w)/((n10_w+n11_w)*(n20_w+n21_w))
      }
      Yk[index] <- log(p2_w/p1_w)
      Sk[index] <- sqrt(S2_RR_w)       # standard error?
      warning("Degrees of freedom for risk difference are usually not reported, therefore NA.")
      dfk[index]<- NA
    }
    index     = index+1
  }
  if("RelativeRisk" %in% measure){
    if(design=="between-subject"){
      if(n10==0 | n21==0 | n11==0 | n20==0){
        OR        <- (n10+0.5)*(n21+0.5)/(n11+0.5)*(n20+0.5)
        S2        <- 1/(n10+n11+1)+1/(n20+n21+1)+1/(n10+n20+1)+1/(n11+n21+1)
        warning("Correction made because some counts are zero")
      }else{
        OR        <- (n10*n21)/(n11*n20)
        S2        <- 1/(n10+n11)+1/(n20+n21)+1/(n10+n20)+1/(n11+n21)
      }
      Yk[index] <- log(OR)
      Sk[index] <- sqrt(S2)
      warning("Degrees of freedom for risk difference are usually not reported, therefore NA.")
      dfk[index]<- NA
    }else{
      if(n10_w==0 | n21_w==0 | n11_w==0 | n20_w==0){
        OR   <- (n20_w+0.5)/(n11_w+0.5)
        S2_w <- 1/(n20_w+0.5)+1/(n11_w+0.5)
      }else{
        OR   <- n20_w/n11_w
        S2_w <- 1/n20_w+1/n11_w
      }
      Yk[index] <- log(OR)
      Sk[index] <- sqrt(S2_w)
      warning("Degrees of freedom for risk difference are usually not reported, therefore NA.")
      dfk[index]<- NA
    }
    index     = index+1
  }
}
