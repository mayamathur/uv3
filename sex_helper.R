
# find order of best-fitting model
polyfit = function(i, dat) AIC( lm( lik ~ poly(mhc, i, raw = TRUE) +
                                      mean.emot,
                                    weights = 1/(dat$lik_sd^2),
                                    data = dat ) )

# return best-fitting poly for given subset of data
best_model = function(dat) {
  # order of best-fitting poly
  poly.order = as.integer( optimize( function(i) polyfit(i = i, dat = dat),
                                     interval = c( 1, 10 ) )$minimum )
  
  if (poly.order == 10) warning("Best order was 10. Increase upper limit.")
  
  # fit the winning model
  lm( lik ~ poly(mhc, poly.order, raw = TRUE) +
        mean.emot,
      weights = 1/(dat$lik_sd^2),
      data = dat)
}
# (I confirmed that using this fn on the non-stratified f2 from main scripts
#  reproduces the main results with poly.order = 6)


make_predframe = function(dat) {
  # note that we're setting emotion to 0 in calculating the SEs
  # make fake design matrix for which to plot the fitted Ys and calculate the SEs 
  # mh.grid is uncentered here for plotting reasons
  mh.grid = seq(-100, 100, 1)
  nfake = length(mh.grid)
  
  # from manually looking at coefficients in best.agg.mods
  poly.order = as.integer( optimize( function(i) polyfit(i = i, dat = dat),
                                     interval = c( 1, 10 ) )$minimum )
  print( poly.order )  # should be 6 and 8 respectively
  
  # center mh when making design matrix
  # since we fit the poly models using mhc
  X = matrix( c( rep(1, nfake),
                 poly(mh.grid - mean(dat$mh), poly.order, raw = TRUE),
                 rep(0, nfake) ),
              nrow = nfake )
  
  # get the relevant model
  sex = unique(dat$w1_sex)  # should only take 1 value within subset
  
  best.agg.mod = best.agg.mods$best.agg.mod[ best.agg.mods$w1_sex == sex ][[1]]
  
  # Cov(XB) = X Cov(B) X' since X is fixed
  # e.g., http://www.stat.ucla.edu/~nchristo/introeconometrics/introecon_fitted.pdf
  CovYhat = X %*% vcov(best.agg.mod) %*% t(X)
  
  predframe.agg = data.frame( lik = X %*% coef(best.agg.mod),
                              se = sqrt( diag(CovYhat) ) )
  
  predframe.agg$lwr = predframe.agg$lik - qnorm(.975) * predframe.agg$se
  predframe.agg$upr = predframe.agg$lik + qnorm(.975) * predframe.agg$se
  
  # for plotting joy, include centered mh score in the dataframe
  predframe.agg$mhc = mh.grid - mean(dat$mh)
  return(predframe.agg)
}
