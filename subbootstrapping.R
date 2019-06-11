#' Mench-Hooker Subsample based Bootstrapping
#' Performs subsample bootstrapping for Boosting algorithms based on the paper by Menth and Hooker (2014).
#' 
subsample_ci <- function(formula, data, control, families, B, n_z){
  for ( i in 1:n_z){
    
  }
}


#' Subbagging
#' @examples 
#' 
preds <- subbagging(formula = stunting ~ bbs(mage) + bbs(mbmi) + bbs(cage) + bbs(cbmi) 
 ,  data = india,
families = GaussianLSS(stabilization = "MAD"), control = ctrl, kn = 100, mn = 1000, coef = "cbmi")


subbagging <- function(formula, data, control, families, kn, mn, coef){
  y_hat_sim <- c()
  newx <- data.frame(seq(from = min(data[, coef]),to = max(data[, coef]), length = 100), data[1,colnames(data) != coef])
  colnames(newx)[1] <- coef
  for (i in 1:mn){
    boot_sam <- dplyr::sample_n(data, size = kn, replace = TRUE)
    mod  <- tryCatch(gamboostLSS(formula, data = boot_sam,
                                     families = families, control = control),
                         error = function(err) {
                           
                           # error handler picks up where error was generated
                           print(paste("Boosting not workder", err))
                         },
                         warning = function(war){
                           print("A warning: ", war)
                         },
                         finally = {
                           message(i)
                         })
    y_pred <- tryCatch(t(predict(mod, newx)$mu),
                       error = function(err) {
                         
                         rep(NA, nrow(newx))
                       })
    y_hat_sim <- rbind(y_hat_sim, y_pred)
     ### error handling einbauen
    cat(i)
  }
  return(y_hat_sim)
}





variance_1kn <- function(formula, data, control, families, kn, mn, coef, nz, nMC){
  vec <- sample(1:nrow(data), size = nz)
  nz_avg <- c()
  for (i in vec){
    y_hat_sim <- c()
    newx <- data.frame(seq(from = min(data[, coef]), to = max(data[, coef]), length = 100), data[1,colnames(data) != coef])
    colnames(newx)[1] <- coef
    for (j in 1:nMC){
      boot_sam <- rbind(data[i,], sample_n(data[-i,], size = kn - 1))
      mod  <- tryCatch(gamboostLSS(formula, data = boot_sam,
                                   families = families, control = control),
                       error = function(err) {
                         
                         # error handler picks up where error was generated
                         print(paste("Boosting not workder", err))
                       },
                       warning = function(war){
                         print("A warning: ", war)
                       },
                       finally = {
                         message(paste0("nx: ", which(vec == i),"nMC: ",j))
                       })
      y_pred <- tryCatch(t(predict(mod, newx)$mu),
                            error = function(err) {

                              rep(NA, nrow(newx))
                            })
      y_hat_sim <- rbind(y_hat_sim, y_pred)
    }
    mu_sub <- c()
    for (k in 1:ncol(y_hat_sim)){
      mu_sub[k] <- mean(y_hat_sim[,k], na.rm = TRUE)
    }
    nz_avg <- rbind(nz_avg, mu_sub)
  }
  out <- c()
  for (l in 1:ncol(nz_avg)){
    out[l] <- var(nz_avg[,l])
  }
  return(out)
}



#'
#'
#'
#'
#' @examples 
#'  y = ci_predict(y_hat = preds)
ci_predict <- function(y_hat, alpha, quant = TRUE){
  mu_sub <- c()
  sd_sub <- c()
  quant_lo <- c()
  quant_up <- c() 
  variance_knkn <- c()
  if (quant == TRUE){
  for (i in 1:ncol(y_hat)){
    mu_sub[i] <- mean(y_hat[,i], na.rm = TRUE)
    sd_sub[i] <- sd(y_hat[,i], na.rm = TRUE)
    variance_knkn[i] <- var(y_hat[,i], na.rm = TRUE)
    quant_lo[i] <- quantile(y_hat[,i], probs =  alpha/2, na.rm = TRUE)
    quant_up[i] <- quantile(y_hat[,i], probs = 1 - alpha/2, na.rm = TRUE)
  }
  } else {
    for (i in 1:ncol(y_hat)){
      mu_sub[i] <- mean(y_hat[,i])
      sd_sub[i] <- sd(y_hat[,i])
      variance_knkn[i] <- var(y_hat[,i])
    }
  }
  
  out <- data.frame(mu_sub = mu_sub-mean(mu_sub), sd_sub, quant_lo = quant_lo-mean(mu_sub), quant_up = quant_up-mean(mu_sub), variance_knkn)
  return(out)
}

# coef <- "cbmi"
# data = india
#  x <- seq(from = min(data[, coef]),to = max(data[, coef]), length = 100)
# plot.gamboostLSS.ci(x = x, y =y[,1], sd =y[,2], lower = y[,3], upper = y[,4], ylim = c(-3,3))
plot.gamboostLSS.ci <- function(x, y,x_obs, sd, xlim, ylim, lower, upper, quant = TRUE, alpha){
  plot(x_obs, rep(ylim[1], length(x_obs)), ylim = ylim, xlab = coef, ylab = latex2exp::TeX("$f_{partial}$"), pch = "|")
  if (quant == TRUE){
    lines(x = x, y = y)
    lines(x = x, y = y+qnorm(1-alpha/2)*sd , col = "red", lty = "dashed")
    lines(x = x, y = y-qnorm(1-alpha/2)*sd , col = "red", lty = "dashed")
    lines(x = x, y = lower, col = "blue", lty = "dashed")
    lines(x = x, y = upper, col = "blue", lty = "dashed")
  } else {
    lines(x = x, y = y)
    lines(x = x, y = y+qnorm(1-alpha/2)*sd , col = "red", lty = "dashed")
    lines(x = x, y = y-qnorm(1-alpha/2)*sd , col = "red", lty = "dashed")
  }
}
 
