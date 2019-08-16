
# Goal: See what mediation package does with polynomial terms. 

library(mediation)

n = 200
x1 = rnorm( n = n, mean = 1, sd = 1 )

m = rnorm( n = n,
           mean = 0 * x1 + 2 * x1^2,
           sd = 1 )

plot(x1, m)

y = rnorm( n = n,
           mean = 0 * x1 + 0 * x1^2 + 
             m + 2 * m^2,
           sd = 1 )

plot( x1, y)


d = data.frame( x1, 
                x2 = x1^2,
                m, 
                m2 = m^2,
                y )


##### Model 1 #####

# this should be wrong
m.mod1 = lm( m ~ x1 + x2, data = d )
y.mod1 = lm( y ~ m + m2 + x1 + x2, data = d )

m1 = mediate( model.m = m.mod1,
         model.y = y.mod1,
         boot = FALSE,
         treat="x1",
         mediator="m" )
summary(m1)


##### Model 2 #####

# correctly specified
m.mod2 = lm( m ~ x1 + I(x1^2), data = d )
y.mod2 = lm( y ~ m + I(m^2) + x1 + I(x1^2), data = d )

# # plot yhats
# newdat = data.frame( x1 = 0,
#                      m = seq(-3,3,.1) )
# yhat = predict(y.mod2, newdata = newdat)
# 
# plot(x1,y)
# plot( newdat$m, yhat )


m2 = mediate( model.m = m.mod2,
              model.y = y.mod2,
              boot = FALSE,
              treat="x1",
              mediator="m" )
summary(m2)

# hmmm...very similar to wrong results of Model 1 results...suspicious


##### Model 3 #####

# coefs exactly the same as in Model 2 because raw = TRUE
m.mod = lm( m ~ poly(x1, 2, raw = TRUE), data = d )
y.mod = lm( y ~ poly(m, 2, raw = TRUE) + poly(x1, 2, raw = TRUE), data = d )

m3 = mediate( model.m = m.mod,
              model.y = y.mod,
              #boot = FALSE,
              treat="x1",
              mediator="m" )

# mediate doesn't understand how to deal with poly() terms :(


##### Model 4 - GAM #####
#   NOTE: DO NOT USE BS = "RE"! GIVES STUPID ANSWERS.
library(mgcv)
m.mod4 = gam( m ~ s(x1), data = d )
y.mod4 = gam( y ~ s(x1) + s(m), data = d )


# sanity check: fitted vs. true
plot( d$x1, predict(m.mod4) ); points(d$x1, d$m, col="red")
plot( d$x1, predict(y.mod4) ); points(d$x1, d$y, col="red")



m4 = mediate( model.m = m.mod4,
              model.y = y.mod4,
              boot = TRUE,
              treat="x1",
              mediator="m" )


summary(m1)
summary(m2)
summary(m4)

# CONCLUSION: HERE I SIMULATED DATA WITH NO DIRECT EFFECT BUT WITH A STRONG 
#  INDIRECT EFFECT. ONLY GAM DOES WELL AT THIS. 
