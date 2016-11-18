
# generate 10 values of x, evenly spread between 0 and 1

x <- seq(0,1,len=10)

# generate corresponding values of t, including the random error
t <- sin(2*pi*x)+rnorm(n=10,mean=0,sd=0.3)


# plot the training set. You might want to adjust the value of ylim, depending on drawn data points and fitted functions
plot(x,t,ylim=c(-2,2))

# generate data for plotting graphs (R only plots data, not functions)
x.graph <- seq(0,1,by=0.001)
t.graph <- sin(2*pi*x.graph)

# plot the training set. You might want to adjust the value of ylim, depending on drawn data points and fitted functions
plot(x,t,ylim=c(-2,2))

# plot the "true" function
lines(x.graph,t.graph,lwd=2,col=4)

# fit a 3rd order polynomial to the data
poly.3 <- lm(t~x+I(x^2)+I(x^3),data.frame(t=t,x=x))


# give a summary of the fitted model
# you don't need to understand all the output: the fitted coefficients are in the column "Estimates"
# Note: you will get different output! Why?
summary(poly.3)

# generate data for plotting the fitted function
y.poly.3.graph <- predict(poly.3,data.frame(x=x.graph))


# plot the fitted function
lines(x.graph,y.poly.3.graph,lwd=2,col=2)

# same thing for 9th order polynomial
poly.9 <- lm(t~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8)+I(x^9),data.frame(t=t,x=x))
summary(poly.9)

y.poly.9.graph <- predict(poly.9,data.frame(x=x.graph))
lines(x.graph,y.poly.9.graph,lwd=2,col=6)



# generate target values for the testdata
t.testset <- sin(2*pi*x.graph)+rnorm(n=1001,mean=0,sd=0.3)


# compute sum of squared error of 3rd order polynomial on the testset
error.poly.3 <- sum((y.poly.3.graph-t.testset)^2)
error.poly.3

error.poly.9 <- sum((y.poly.9.graph-t.testset)^2)
error.poly.9