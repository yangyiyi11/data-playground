# 1D
# Set total number trail walkers
walker.number <- 100

# Set total trail steps for each walker
steps.total <-1000

# the total count of walkers that come back to the origin
to.zero.n <- 0

# random walk function
for (i in 1 : walker.number){
  x.total <- 0  # set initial position in x direction
  dx <- 0 
  distance <- 1
  for (j in 1: steps.total){
    if (distance == 0){
      to.zero.n <- to.zero.n+1
      break
    } # when a walker come back to the origin, stop and count+1
    else {
      dx = floor(runif (1, min=1, max=3))
      if (dx == 1){
        x.total <- x.total+dx
      }  # go right
      else {x.total <- x.total+dx-3
      }  #go left
      distance <- x.total^2 # calculate the distance from origin
    }
  }
}
Prob <- to.zero.n/walker.number # the probability of walkers that ever come back to the origin

#2D
# Set total number trail walkers
walker.number <- 1000

# Set total trail steps for each walker
steps.total <-1000

to.zero.n <- 0

# random walk function
for (i in 1 : walker.number){
  x.total <- 0 # set initial position in x direction
  y.total <- 0 # set initial position in y direction
  dx <- 0
  dy <- 0
  distance <- 1
  for (j in 1: steps.total){
    if (distance == 0){
      to.zero.n <- to.zero.n+1
      break
    } # when a walker come back to the origin, stop and count+1
    else {
      dx = floor(runif (1, min=1, max=3))
      if (dx == 1){
        x.total <- x.total+dx} # go right
      else {x.total <- x.total+dx-3
      } # go left
      dy = floor(runif (1, min=1, max=3))
      if (dy == 1){
        y.total <- y.total+dy} # go up
      else {y.total <- y.total+dy-3
      } # go down
      distance <- x.total^2+y.total^2
    }
  }
}
Prob <- to.zero.n/walker.number

# 3D
# Set total number trail walkers
walker.number <- 100

# Set total trail steps for each walker
steps.total <-100

to.zero.n <- 0

for (i in 1 : walker.number){
  x.total <- 0
  y.total <- 0
  z.total <- 0
  dx <- 0
  dy <- 0
  dz <- 0
  distance <- 1
  for (j in 1: steps.total){
    if (distance == 0){
      to.zero.n <- to.zero.n+1
      break
    }
    else {
      dx = floor(runif (1, min=1, max=3))
      if (dx == 1){
        x.total <- x.total+dx} # go forward
      else {x.total <- x.total+dx-3
      } # go backward
      dy = floor(runif (1, min=1, max=3))
      if (dy == 1){
        y.total <- y.total+dy} # go right
      else {y.total <- y.total+dy-3
      } # go left
      dz = floor(runif (1, min=1, max=3))
      if (dz == 1){
        z.total <- z.total+dz} # go up
      else {z.total <- z.total+dz-3
      } # go down
      distance <- x.total^2+y.total^2+z.total^2
    }
  }
}
Prob <- to.zero.n/walker.number