# strength of dependency continuous/numerical vs binary variables
cal_es_cont <- function(y, x, ref, measure, hetsk = FALSE){

  if(missing(ref)) ref <- levels(x)[1]
  # initialize the output vectors
  Yk    <- array(data=NA, dim = (length(measure)))   # measure of effect
  Sk    <- array(data=NA, dim = (length(measure)))   # standard error
  dfk   <- array(data=NA, dim = (length(measure)))   # degrees of freedom
  index <- 1                                         # keep index for writing the values

  mean_exp <- aggregate(y, by = list(x), FUN = mean)
  var_exp  <- aggregate(y, by = list(x), FUN = var)
  colnames(mean_exp) <- c("exposure", "group_mean")
  colnames(var_exp)  <- c("exposure", "group_var")

  MD = diff(mean_exp$group_mean)
  exp_levels <- levels(x)            # Coding of the exposure is left open
  I1         <- length(which(x==exp_levels[1]))
  I2         <- length(which(x==exp_levels[2]))
  sp2        <- ((I1-1)*var_exp$group_var[1] + (I2-1)*var_exp$group_var[2])/(I1+I2-2)

  if("MD" %in% measure){
    Yk[index]     <- MD
    if(hetsk){    # heterogeneous case
      Sk[index]   <- sqrt(var_exp$group_var[1]/I1+var_exp$group_var[2]/I2)
      dfk[index]  <- (var_exp$group_var[1]/I1 + var_exp$group_var[2]/I2)^2/(var_exp$group_var[1]^2/(I1^2*(I1-1))+var_exp$group_var[2]^2/(I2^2*(I2-1))) # Satterthwaite
    } else {      # homogeneous case
      sp2         <- ((I1-1)*var_exp$group_var[1] + (I2-1)*var_exp$group_var[2])/(I1+I2-2)
      Sk[index]   <- sqrt(sp2*(1/I1+1/I2))
      dfk[index]  <- I1 + I2 - 2
    }
    index         <- index+1
  }

  if("Cohen" %in% measure){   # assumption of equal variances
    Yk[index]   <- MD/sqrt(sp2)
    dfk[index]  <- I1 + I2 - 2
    if(hetsk){
      Sk[index] <- sqrt(sum(var_exp$group_var)/2)
    }else{
      Sk[index] <- sqrt((I1+I2)/(I1*I2) + 1/2*Yk[index]^2/(I1+I2))
    }
    index       <- index+1
  }

  if("Glass" %in% measure){   # assumption of equal variances
    sp2_ref     <- var_exp[var_exp$exposure==ref, ]$group_var  # standard deviation of the reference population
    Yk[index]   <- MD/sqrt(sp2_ref)
    dfk[index]  <- I1 + I2 - 2
    if(hetsk){
      Sk[index] <- sqrt(sum(var_exp$group_var)/2)
    }else{
      Sk[index] <- sqrt((I1+I2)/(I1*I2) + 1/2*Yk[index]^2/(I1+I2))
    }
    index       <- index+1
  }

  if("CL" %in% measure){
    sp_het3     <- sqrt(sum(var_exp$group_var)) # common language effect size
    Yk[index]   <- diff(mean_exp$group_mean)/sp_het3
    Sk[index]   <- sqrt(sp2*(1/I1+1/I2))        # check this
    dfk[index]  <- I1+I2-2                      # check this
    index       <- index+1
  }

  if("Hedge" %in% measure){
    if(hetsk){warning("Hedge's G is designed for homogeneous samples")}
    dfk[index]  <- I1+I2-2
    J           <- gamma(0.5*dfk[index])/(gamma(0.5*(dfk[index]-1))*sqrt(0.5*dfk[index]))
    J_approx    <- 4*(dfk[index]-1)/(4*dfk[index]-1)
    Yk[index]   <- J*MD/sqrt(sp2)
    HedgesG_var <- (4*(I1+I2-3))^2/(4*(I1+I2-9))^2*((I1+I2)/(I1*I2)+MD^2/(2*(I1+I2)*sp2))
    Sk[index]   <- sqrt((I1+I2)/(I1*I2)+Yk[index]^2/(2*(I1+I2)))   # check this
    index       <- index+1
  }

  if("Cliff" %in% measure){
    Y1        <- y[x==levels(x)[1]]
    Y2        <- y[x==levels(x)[2]]
    avg_rank1 <- mean(order(Y1))
    avg_rank2 <- mean(order(Y2))
    sp2_1     <- sum((order(Y1)-avg_rank1)^2)/(I1-1)
    sp2_2     <- sum((order(Y2)-avg_rank2)^2)/(I1-1)
    if(hetsk){
      Yk[index] <- 2*(avg_rank1-avg_rank2)/(I1+I2)
      Sk[index] <- 2*sqrt(sp2_1/I1+sp2_2/I2)/(I1+I2)
      dfk[index]<- (sp2_1/I1+sp2_2/I2)^2/(sp2_1^2/(I1^2*(I1-1))+sp2_2^2/(I2^2*(I2-1)))
    }else{
      warning("Cliff's Delta is computed under the assumption of heteroskedasticity")
    }
    index <- index+1
  }

  DF_measuresofeffect = data.frame(cbind(measure), Yk, Sk, dfk)
  colnames(DF_measuresofeffect) <- c("Measure of effect", "Y", "StdErr", "DoF")
  return(MeasuresOfEffect = DF_measuresofeffect)

  # return(list(MeasuresOfEffect=measures, Yk=Yk, Sk=Sk, dfk=dfk))
}
