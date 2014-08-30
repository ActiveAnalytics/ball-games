require(deSolve)

# Function for the dynamic model
ball <- function(t, y, parms)
{
  with(as.list(c(parms, y)), {
    W <- c(W1, W2, W3)
    v_W = y[c(2, 4, 6)] - W
    nv_W = norm(v_W, "2")
    coeff <- (-.5/m)*rho*A*nv_W
    dy1 <- y[2]
    spin1 <- coeff*CL*(vw[2]*(y[6] - W[3]) - vw[3]*(y[4] - W[2]))/w
    if(is.na(spin1)) # prevents NaN
      spin1 <- 0
    dy2 <- coeff*CD*(y[2] - W[1]) - spin1
    dy3 <- y[4]
    spin2 <- CL*(vw[3]*(y[2] - W[1]) - vw[1]*(y[6] - W[3]))/w
    if(is.na(spin2))
      spin2 <- 0
    dy4 <- coeff*CD*(y[4] - W[2]) - spin2
    dy5 <- y[6]
    spin3 <- coeff*CL*(vw[1]*(y[4] - W[2]) - vw[2]*(y[2] - W[1]))/w
    if(is.na(spin3))
      spin3 <- 0
    # no more accelation if ball hits the ground
    if(y[5] <= 0 & t != 0){
      dy6 <- coeff*CD*(y[6] - W[3]) - spin3
    }else{
      dy6 <- coeff*CD*(y[6] - W[3]) - spin3 - g
    }
    list(c(dy1, dy2, dy3, dy4, dy5, dy6))
  })
}

elev <- 12 #degrees
elev <- elev*pi/180

yini <- c(y1 = 0, y2 = 60*cos(elev), y3 = 0, y4 = 0, y5 = 0, y6 = 60*sin(elev))
times <- seq(0, 10, .01)
vw = c(0, 0, 0) # angular vel
w = norm(vw, "2") # manitude of angular velocity
CL = .319*(1 - exp(-2.48E-3 * w))
pars <- c(rho = 1.22, w = w, vw = vw, W = c(0, 0, 0), m = 4.59E-2, CD = .45,
          A = pi*(4.27E-2/2)^2, g = 9.81, CL = CL)

# To state that when the ball hits the ground velocities go to zero
rootfunc <- function(t, y, parms) y[5] # displacement in dir 3
eventfunc <- function(t, y, parms) {
  y[5] <- y[5]
  y[2] <- 0
  y[4] <- 0
  y[6] <- 0
  return(y)
}

out <- ode(func = ball, y = yini, times = times, parms = pars, 
           rootfun = rootfunc, 
           events = list(func = eventfunc, root = TRUE))

# Plot of one of the curves from figure 11 in the paper
plot(out[,c("y1", "y5")], t = "l", xlab = "position, x(m)", ylab = "poisition, z(m)")
