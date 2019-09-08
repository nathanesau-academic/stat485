mom <- function(x, model = NA) {
  if(model == "MA1") 
  {
    r = TSA::acf(x, plot = FALSE)$acf[1]
    theta = ifelse(abs(r) < 0.5, (-1 + sqrt(1 - 4*r^2))/(2*r), NA)
    sigma2 = var(x)/(1 + theta^2)
    
    return(list(theta = theta, sigma2 = sigma2))
  } 
  else if (model == "AR1")
  {
    output = ar(x, order.max = 1, AIC = FALSE, method = 'yw')
    
    return(list(phi = output$ar, mu = output$x.mean,
                sigma2 = output$var.pred))
  }
  else if (model == "ARMA11")
  {
    r = TSA::acf(x, plot = FALSE)$acf
    
    z1 = r[1] - r[2]/r[1]
    z2 = 2*r[2] - 1 - r[2]^2/r[1]^2
    z3 = r[1] - r[2]/r[1]
    
    roots = polyroot(z = c(z1, z2, z3))
    re = Re(roots)
    im = Im(roots)
    
    rootIndex = which(abs(re) < 1)[1]
    theta = - ifelse(im[rootIndex] < .Machine$double.eps,
                     re[rootIndex], NA)
    
    phi = r[2]/r[1]
    
    sigma2 = (1 - phi^2) / (1 + theta^2 - 2*theta*phi) * var(x)
    
    return(list(phi = phi, theta = theta, sigma2 = sigma2))
  }
  else
  {
    return(NA)
  }
}

css <- function(x, model = NA) {
  if (model == "MA1")
  {
    output = arima(x, order = c(0, 0, 1), method = "CSS",
                   include.mean = FALSE)
    
    return(list(theta = - as.numeric(output$coef),
                sigma2 = output$sigma2))
  }
  else if(model == "AR1")
  {
    output = arima(x, order = c(1, 0, 0), method = "CSS")
    
    return(list(phi = as.numeric(output$coef)[1],
                mu = as.numeric(output$coef)[2],
                sigma2 = output$sigma2))
  }
  else if(model == "ARMA11")
  {
    output = arima(x, order = c(1, 0, 1), method = "CSS",
                   include.mean = FALSE)
    
    return(list(phi = as.numeric(output$coef)[1],
                theta = - as.numeric(output$coef)[2],
                sigma2 = output$sigma2))
  }
  else
  {
    return(NA)
  }
}

uls <- function(x, model = NA) {
  if (model == "MA1")
  {
    output = arima(x, order = c(0, 0, 1), method = "ML",
                   include.mean = FALSE)
    
    return(list(theta = - as.numeric(output$coef),
                sigma2 = output$sigma2,
                aic = output$aic))
  }
  else if(model == "AR1")
  {
    output = arima(x, order = c(1, 0, 0), method = "ML")
    
    return(list(phi = as.numeric(output$coef)[1],
                mu = as.numeric(output$coef)[2],
                sigma2 = output$sigma2,
                aic = output$aic))
  }
  else if(model == "ARMA11")
  {
    output = arima(x, order = c(1, 0, 1), method = "ML",
                   include.mean = FALSE)
    
    return(list(phi = as.numeric(output$coef)[1],
                theta = - as.numeric(output$coef)[2],
                sigma2 = output$sigma2,
                aic = output$aic))
  }
  else
  {
    return(NA)
  }
}