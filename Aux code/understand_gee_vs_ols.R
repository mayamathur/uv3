
# Goal: Figure out why fitted values are so different for GEE vs. OLS. 


fitted_y = Vectorize( function( x, model, model.type ) {
  if ( model.type == "lmer" ) {
    return( fixef(model)[1] +
              fixef(best.mod)[2]*x +
              fixef(best.mod)[3]*(x^2) +
              fixef(best.mod)[4]*(x^3) )
  }
  
  if ( model.type == "lm" ) {
    return( coef(model)[1] +
              coef(model)[2]*x +
              coef(model)[3]*(x^2) +
              coef(model)[4]*(x^3) )
  }

}, vectorize.args = "x" )


# p: row of parameters dataframe containing k, mean, V, V.gam, per.cluster, eta
sim_data = function(p, seed = NA ) {
  
  #browser()
  
  N = p$k * p$per.cluster
  
  # generate cluster random intercepts
  if( !is.na(seed) ) set.seed(seed)
  gam1 = rnorm( n = p$k, mean = 0, sd = sqrt( p$V.gam ) )
  gam1i = rep( gam1, each = p$per.cluster )
  
  # generate cluster means for X
  if( !is.na(seed) ) set.seed(seed + 1)
  EX = runif( n = p$k, -100, 100 )
  EXi = rep( EX, each = p$per.cluster )
  
  # observed X
  if( !is.na(seed) ) set.seed(seed + 1)
  X = rnorm( n = N, mean = EXi, sd = p$sdX )
  
  # coefficients from actual data (OLS on trial-level data)
  linpred = -2.629e+01 + 0.18*EXi + 4.414e-03*(EXi^2) + 2.671e-05*(EXi^3)
  
  #plot(X, linpred)
  
  if( !is.na(seed) ) set.seed(seed + 2)
  eps = rnorm( n = N, mean = 0, sd = p$eps.sd )
  Y = linpred + gam1i + eps
  
  #plot(X,Y)
  
  d = data.frame( cluster = rep(1:p$k, each = p$per.cluster),
                  X,
                  Y,
                 EX = EXi,
                  EY = linpred )
  return(d)
}

##### Sanity Check #####
d = sim_data( p = data.frame( k = 183,  # faces
                              per.cluster = 45,  # subjects
                              V.gam = 50,
                              sdX = 30,
                              eps.sd = 10 ),
              seed = 1)



# aggregate model
library(dplyr)
agg = d %>% group_by(cluster) %>%
  summarise( meanX = mean(X),
             meanY = mean(Y) )

# plot(d$X, d$Y)
# plot(d$EX, d$EY)
# plot(agg$meanX, agg$meanY)


library(lme4)
poly3.lmer = lmer( Y ~ X + I(X^2) + I(X^3) +
                           + (1|cluster),
                         data = d )

# independence = good even when X has clusters
# unstructured = CRAZY
# exchangeable = CRAZY
library(gee)
poly3.gee = gee( Y ~ X + I(X^2) + I(X^3),
                     id = as.factor(d$cluster), 
                     corstr="independence",
                     data = d )

# OLS (unbiased but likely wrong inference)
poly3.lm = lm( Y ~ X + I(X^2) + I(X^3),
                   data = d )


poly3.lm.agg = lm( meanY ~ meanX + I(meanX^2) + I(meanX^3),
               data = agg )



library(ggplot2)
ggplot(d, aes(x=X, y=Y) ) +
  geom_hline(yintercept=0, color="gray", lwd=.6) +  # reference line for neutrality
  #geom_point(alpha = 0.1) +  # can comment out to improve speed
  theme_classic() +
  

  stat_function(fun = function(x) fitted_y(x = x, model = poly3.gee, model.type = "lm"),
                aes( color="GEE" ),
                lwd = 1, lty=1) +  

  geom_smooth(lty = 2,
              aes(color = "Unclustered GAM") ) +  # LOESS (another sanity check)
  
  
  stat_function(fun = function(x) fitted_y(x = x, model = poly3.lm, model.type = "lm"),
                aes( color="OLS" ),
                lwd=1, lty = 2) + 

  stat_function(fun = function(x) fitted_y(x = x, model = poly3.lmer, model.type = "lmer"),
                aes( color="LMER" ),
                lwd=1, lty = 2) +
  
  stat_function( fun = function(x) -2.629e+01 + 0.18*x + 4.414e-03*(x^2) + 2.671e-05*(x^3),
                 aes( color = "True" ),
                      lwd = 1, lty = 1 )


# CLUSTERING IN X MATTERS A LOT!! 
# TRIED ITS SD = 0 VS 10
