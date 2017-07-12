###################################################
### R Code of the paper
###   Thiele, Jan C.
###   R Marries NetLogo: Introduction to the RNetLogo Package
###################################################

###################################################
## version info:
##     January, 19, 2014
##     for RNetLogo 0.9.7 and NetLogo 5.0.5
###################################################

###################################################
# R and NetLogo must be installed
# several additional R packages must be available:
# - RNetLogo (+ rJava)
# - sp
# - gstat
# - RSQLite
# - Ryacas (+ Yacas)
# - rpanel
# furthermore, the variables nl.path and
# database.path must be adapted
###################################################


#--------------------------------------------------
#--------------------------------------------------
# Section 3. Using RNetLogo - Hands On
#--------------------------------------------------
#--------------------------------------------------

#--------------------------------------------------
# Subsection 3.2. Loading NetLogo
#--------------------------------------------------

###################################################
### Load the RNetLogo package
###################################################
library("RNetLogo")


###################################################
### Start NetLogo
### (for faster production run it headless, i.e. gui=FALSE)
###################################################
# ToDo: adapt this path
# Current setting assumes that NetLogo is installed
# in the current working directory

# todo: also adapt

# knb 20170712
curdir <- "/home/knb/code/svn/eclipse38_dynlang/R_one-offs/R_grabbag"
#getwd()
nl.path <- "/opt/smallapps/netlogo-5.0.3"
# ToDo: adapt nl.version when using different NetLogo version
NLStart(nl.path, gui = FALSE, nl.jarname = "NetLogo.jar")


#--------------------------------------------------
# Subsection 3.3. Loading a model
#--------------------------------------------------

###################################################
### Load the Forest Fire Model
###################################################
model.path <- file.path("models", "Sample Models", "Earth Science", "Fire.nlogo")
NLLoadModel(file.path(nl.path, model.path))


#--------------------------------------------------
# Subsection 3.4. Principles of controlling a model
#--------------------------------------------------

###################################################
### Set the density (slider) to 77 percent
###################################################
NLCommand("set density 77")


###################################################
### Call the setup procedure
###################################################
# maybe set a random seed
# NLCommand("random-seed 123456789")
NLCommand("setup")


###################################################
### Call the go procedure
###################################################
NLCommand("go")


###################################################
### Print a string into NetLogo's Command Center
###################################################
NLCommand("print \"Hello NetLogo, I called you from R.\"")


###################################################
### Set the density slider from R variable
###################################################
density.in.r <- 88
NLCommand("set density ", density.in.r, "setup", "go")


###################################################
### Run the go procedure for 10 times
###################################################
NLDoCommand(10, "go")


###################################################
### Get the number of ticks
###################################################
NLReport("ticks")


###################################################
### Get the number of ticks and write it into an R variable
###################################################
ticks <- NLReport("ticks")
print(ticks)


###################################################
### Close NetLogo
###################################################
# this will close the main session,
# only execute this if you want to stop here
## NLQuit()


#--------------------------------------------------
# Subsection 3.5. Advanced controlling functionalities
#--------------------------------------------------

###################################################
### Reset the Forest Fire Model and run and
### report the precentage of burned trees for 10 time steps
###################################################
# maybe set a random seed
# NLCommand("random-seed 123456789")
NLCommand("setup")
burned <- NLDoReport(10, "go", "(burned-trees / initial-trees) * 100")
print(unlist(burned))


###################################################
### Reset the Forest Fire Model and run and
### report the precentage of burned trees until
### no trees are left
###################################################
NLCommand("setup")
burned <- NLDoReportWhile("any? turtles",
                          "go",
                          c("ticks",
                            "(burned-trees / initial-trees) * 100"),
                          as.data.frame = TRUE,
                          df.col.names = c("tick", "percent burned"))
plot(burned, type = "s")


###################################################
### Load, setup and run (20 steps) the Tumor Model
###################################################
model.path <- file.path("models", "Sample Models", "Biology", "Tumor.nlogo")
NLLoadModel(file.path(nl.path, model.path))
NLCommand("setup")
NLDoCommand(20, "go")


###################################################
### Get the tumor cells (turtles) with their coordinates
### sorted by the who variable and plot their position
###################################################
cells <- NLGetAgentSet(c("xcor", "ycor"), "turtles")
x.minmax <- NLReport("(list min-pxcor max-pxcor)")
y.minmax <- NLReport("(list min-pycor max-pycor)")
plot(cells, xlim = x.minmax, ylim = y.minmax, xlab = "x", ylab = "y")


###################################################
### Get only the metastatic cells and plot them
###################################################
cells.metastatic <- NLGetAgentSet(c("xcor", "ycor"),
                                  "turtles with [metastatic? = True]")
plot(cells.metastatic, xlim = x.minmax, ylim = y.minmax,
     xlab = "x", ylab = "y")


###################################################
### Load, setup and run (5 steps) the Fur Model and
### get the patches with their color value
### sorted by their coordinates
###################################################
model.path <- file.path("models", "Sample Models", "Biology", "Fur.nlogo")
NLLoadModel(file.path(nl.path, model.path))
NLCommand("setup")
NLDoCommand(5, "go")
patches.matrix <- NLGetPatches("pcolor", "patches", as.matrix = TRUE)


###################################################
### Transform the patch matrix to make it fit
### for the image function
### and define the patch colors
###################################################
patches.matrix.rot <- t(patches.matrix)
patches.matrix.rot <- as.data.frame(patches.matrix.rot)
patches.matrix.rot <- rev(patches.matrix.rot)
patches.matrix.rot <- as.matrix(patches.matrix.rot)
col <- c("black", "white")


###################################################
### Get the NetLogo World/View dimensions
### and create an image from the patch matrix
###################################################
x.minmax <- NLReport("(list min-pxcor max-pxcor)")
y.minmax <- NLReport("(list min-pycor max-pycor)")
image(x.minmax[1]:x.minmax[2], y.minmax[1]:y.minmax[2],
      patches.matrix.rot, col = col, xlab = "x", ylab = "y")


###################################################
### Do the same as before but as a spatial object
### instead of a simple raster image.
### Start with loading the required packages
### and get the patches as data.frame
###################################################
library("sp", "gstat")
patches <- NLGetPatches(c("pxcor", "pycor", "pcolor"), "patches")


###################################################
### Make the patches data.frame a SpatialPointsDataFrame
### and afterwards a SpatialPixelsDataFrame
###################################################
coordinates(patches) <- ~ pxcor + pycor
gridded(patches) <- TRUE


###################################################
### Define the patches color variable as a factor
### and assign a variable storing the colors black
### and white and finally plot the spatial object
###################################################
patches$pcolor <- factor(patches$pcolor)
col <- c("black", "white")
spplot(patches, "pcolor", col.regions = col, xlab = "x", ylab = "y")


###################################################
### Reuse the patches.matrix from NLGetPatches and
### change values from 0 (black) to 15 (red)
### (Attention: This is relatively slow due to the
### drawing procedure) and send the new color
### matrix to NetLogo's patches
###################################################
my.matrix <- replace(patches.matrix,
                     patches.matrix == 0,
                     15)
NLSetPatches("pcolor", my.matrix)


###################################################
### Load and setup the Small Worlds Model
### and get the NetLogo link network into
### an (directed) igraph network
###################################################
model.path <- file.path("models", "Sample Models", "Networks", "Small Worlds.nlogo")
NLLoadModel(file.path(nl.path, model.path))
NLCommand("setup", "rewire-all")
my.network <- NLGetGraph()


###################################################
### Plot the igraph network
###################################################
plot(my.network, layout = layout.circle,
     vertex.label = V(my.network)$name,
     vertex.label.cex = 0.7,
     asp = FALSE)


#--------------------------------------------------
# Subsection 3.6. Headless mode/Multiple NetLogo instances
#--------------------------------------------------

###################################################
### Start NetLogo headless (into the standard variable nl.intern)
###################################################
# this only works when NetLogo wasn't started before in the current R session
# ToDo: adapt nl.version when using different NetLogo version
## NLStart(nl.path, gui = FALSE, nl.version = 5)


###################################################
### Start NetLogo headless three times
### in variable my.netlogo1, my.netlogo2, and
### my.netlogo3
###################################################
# adapt nl.version when using different NetLogo version
my.netlogo1 <- "my.netlogo1"
NLStart(nl.path,
        gui = FALSE,
        nl.version = 5,
        nl.obj = my.netlogo1)

my.netlogo2 <- "my.netlogo2"
NLStart(nl.path,
        gui = FALSE,
        nl.version = 5,
        nl.obj = my.netlogo2)

my.netlogo3 <- "my.netlogo3"
NLStart(nl.path,
        gui = FALSE,
        nl.version = 5,
        nl.obj = my.netlogo3)


###################################################
### Load the Forest Fire Model into the three
### new NetLogo instance
###################################################
model.path <- file.path("models", "Sample Models", "Earth Science", "Fire.nlogo")
NLLoadModel(file.path(nl.path, model.path),
            nl.obj = my.netlogo1)
NLLoadModel(file.path(nl.path, model.path),
            nl.obj = my.netlogo2)
NLLoadModel(file.path(nl.path, model.path),
            nl.obj = my.netlogo3)


###################################################
### Setup and run the first Forest Fire Model
### (my.netlogo1) for 25 steps
###################################################
NLCommand("setup", nl.obj = my.netlogo1)
NLDoCommand(25, "go", nl.obj = my.netlogo1)


###################################################
### Setup and run the second Forest Fire Model
### (my.netlogo2) for 15 steps
###################################################
NLCommand("setup", nl.obj = my.netlogo2)
NLDoCommand(15, "go", nl.obj = my.netlogo2)


###################################################
### Setup and run the third Forest Fire Model
### (my.netlogo3) for 5 steps
###################################################
NLCommand("setup", nl.obj = my.netlogo3)
NLDoCommand(5, "go", nl.obj = my.netlogo3)


###################################################
### Get the value of burned-trees from all three
### NetLogo instances (my.netlogo1 - my.netlogo3)
###################################################
NLReport("burned-trees", nl.obj = my.netlogo1)
NLReport("burned-trees", nl.obj = my.netlogo2)
NLReport("burned-trees", nl.obj = my.netlogo3)


###################################################
### Quit the NetLogo instances
###################################################
NLQuit(nl.obj = my.netlogo3)
NLQuit(nl.obj = my.netlogo2)
NLQuit(nl.obj = my.netlogo1)
# this will close the main session, only execute this if you want to stop here
## NLQuit()
## alternatively use:
## NLQuit(all=TRUE)
## to close all active NetLogo instances


#--------------------------------------------------
#--------------------------------------------------
# Section 4. Application examples
#--------------------------------------------------
#--------------------------------------------------

#--------------------------------------------------
# Subsection 4.1. Exploratory analysis
#--------------------------------------------------

###################################################
### Initialize RNetLogo (if not already done)
### and load the Forest Fire Model
###################################################
## library("RNetLogo")
# ToDo: adapt this path
## nl.path <- file.path(getwd(), "NetLogo 5.0.5")
# ToDo: adapt nl.version when using different NetLogo version
## NLStart(nl.path, gui = FALSE, nl.version = 5)
model.path <- file.path("models", "Sample Models", "Earth Science", "Fire.nlogo")
NLLoadModel(file.path(nl.path, model.path))


###################################################
### Define a function which sets the submitted
### density value, runs the simulation until no
### turtles are left and reports the proportion
### of burned trees
###################################################
sim <- function(density) {
    NLCommand("set density ", density, "setup")
    NLDoCommandWhile("any? turtles", "go");
    ret <- NLReport("(burned-trees / initial-trees) * 100")
    return(ret)
}


###################################################
### Run the simulation for density values from 1 to
### 100 percent (stepsize 1) and plot the corresponding
### proportion of burned trees
###################################################
d <- seq(1, 100, 1)
pb <- sapply(d, function(dens) {sim(dens)})
plot(d, pb, xlab = "density", ylab = "percent burned")


###################################################
### Define another function which calls the first one
### (sim) for multiple times (rep)
###################################################
rep.sim <- function(density, rep) {
  return(
    lapply(density, function(dens) {
      replicate(rep, sim(dens))
    })
  )
}


###################################################
### Call the second function (rep.sim) for density
### values between 45 and 70 percent (stepsize 5)
### and with 10 repeated simulations each
### and create boxplots for the percentage of
### burned trees for each density value
###################################################
d <- seq(45, 70, 5)
res <- rep.sim(d, 10)
boxplot(res, names = d, xlab = "density", ylab = "percent burned")


###################################################
### Same as before but for density values between
### 55 and 65 percent (stepsize 1) and 20
### repetitions
###################################################
d <- seq(55, 65, 1)
res <- rep.sim(d, 20)
boxplot(res,names = d, xlab = "density", ylab = "percent burned")


#--------------------------------------------------
# Subsection 4.2. Database connection
#--------------------------------------------------

###################################################
### Initialize RNetLogo (if not already done),
### load the Forest Fire Model and set it up
###################################################
## library("RNetLogo")
# ToDo: adapt this path
## nl.path <- file.path(getwd(), "NetLogo 5.0.5")
# ToDo: adapt nl.version when using different NetLogo version
## NLStart(nl.path, gui = FALSE, nl.version = 5)
model.path <- file.path("models", "Sample Models", "Earth Science", "Fire.nlogo")
NLLoadModel(file.path(nl.path, model.path))
NLCommand("setup")


###################################################
### Load the RSQLite package, define the database
### driver, select a path and name for the
### test database, and connect to the selected
### database (if the database file does not exist
### it will be created)
###################################################
library("RSQLite")
m <- dbDriver("SQLite")
# ToDo: adapt this path (writing permission required!)
database.path = "test_netlogo.db"
con <- dbConnect(m, dbname = database.path)


###################################################
### Run the simulation for 10 steps and save the
### tick and the corresponding number of burned
### trees for every simulation step in a data.frame
### which is directly pasted into a table called
### Burned1
###################################################
dbWriteTable(con, "Burned1",
             NLDoReport(10, "go", c("ticks", "burned-trees"),
             as.data.frame = TRUE, df.col.names = c("tick", "burned")),
             row.names = FALSE, append = FALSE)


###################################################
### Ask for the number of rows in the table Burned1
###################################################
dbGetQuery(con, "select count(*) from Burned1")[[1]]


###################################################
### Ask for all columns of rows where the time is
### greater than 5
###################################################
rs <- dbSendQuery(con, "select * from Burned1 where tick > 5")


###################################################
### Get the result of the query
###################################################
data <- fetch(rs, n = -1)
str(data)


###################################################
### Clear the query
###################################################
dbClearResult(rs)


###################################################
### Run 10 more simulation steps and append the
### resulting data.frame to the existing
### Burned1 table
###################################################
dbWriteTable(con, "Burned1",
             NLDoReport(10, "go", c("ticks", "burned-trees"),
             as.data.frame = TRUE, df.col.names = c("tick", "burned")),
             row.names = FALSE, append = TRUE)


###################################################
### Ask for all columns of all rows of table Burned1
###################################################
select.all <- dbGetQuery(con, "select * from Burned1")
str(select.all)


###################################################
### Run 20 simulation steps and a newly initialized
### simulation and save the results in the table
### Burned2
### Repead this 10 times so we should have 10 * 20 =
### 200 rows in the table (if the table was empty before)
###################################################
for (x in 1:10)
{
  NLCommand("setup")
  dbWriteTable(con, "Burned2",
               NLDoReport(20, "go", c("ticks", "burned-trees"),
               as.data.frame = TRUE, df.col.names = c("tick", "burned")),
               row.names = FALSE, append = TRUE)
}


###################################################
### Ask for the mean number of burned trees for every
### simulation step out of the 10 repetitions and
### get the result of the query
###################################################
rs <- dbSendQuery(con, "select avg(burned) as mean_burned
                        from Burned2 group by tick")
data <- fetch(rs, n = -1)
str(data)


###################################################
### Clear the query and close the database connection
###################################################
dbClearResult(rs)
dbDisconnect(con)
# remove database ?
# if (file.exists(database.path)) {
#  file.remove(database.path)
# }


#--------------------------------------------------
# Subsection 4.3. Analytical comparison
#--------------------------------------------------

###################################################
### Initialize RNetLogo (if not already done)
### and load and initialize the Gas Lab Model
###################################################
## library("RNetLogo")
# ToDo: adapt this path
## nl.path <- file.path(getwd(), "NetLogo 5.0.5")
# ToDo: adapt nl.version when using different NetLogo version
## NLStart(nl.path, gui = FALSE, nl.version = 5)
model.path1 <- file.path("models", "Sample Models", "Chemistry & Physics", "GasLab")
model.path2 <- "GasLab Free Gas.nlogo"
NLLoadModel(file.path(nl.path, model.path1, model.path2))
NLCommand("set number-of-particles 500", "no-display", "setup")


###################################################
### Run the simulation for 40 times of 50 steps
### (= 2000 simulation steps), save the speed
### of particles after every 50 simulation step
### interval and finally flat the list of lists
### (one list for each of the 40 runs) to one
### big vector
###################################################
particles.speed <- NLDoReport(40, "repeat 50 [go]",
                              "[speed] of particles")
particles.speed.vector <- unlist(particles.speed)


###################################################
### Load the Ryacas package
###################################################
library("Ryacas")


###################################################
### on Windows: run the Yacas automatic installation
### once
### on other OS: see Ryacas/Yacas documentation on
### how to install
###################################################
## yacasInstall()


###################################################
### Get the mean energy of the particles and
### register equation B in Yacas
###################################################
energy.mean <- NLReport("mean [energy] of particles")
B <- function(v, m = 1, k = 1)
              v * exp((-m * v^2) / (2 * k * energy.mean))
yacas(B)


###################################################
### Define the integration of equation B
### from 0 to endless
###################################################
B.integr <- expression(integrate(B, 0, Infinity))
yacas(B.integr)


###################################################
### Calculate a numerical approximation using
### Yacas function N() and get the result from
### Yacas in R
###################################################
normalizer.yacas <- yacas(N(B.integr))
normalizer <- Eval(normalizer.yacas)
print(normalizer$value)

###################################################
### Get the max. speed from the particles speed vector
###################################################
maxspeed <- max(particles.speed.vector)


###################################################
### Calculate the theoretical distribution
###################################################
stepsize <- 0.25
v.vec <- seq(0, maxspeed, stepsize)
theoretical <- B(v.vec) / normalizer$value


###################################################
### Plot the results of the theoretical Maxwell
### distribution and the empirical histogram of
### the agent-based simulation together
###################################################
hist(particles.speed.vector, breaks = max(particles.speed.vector) * 5,
     freq = FALSE, xlim = c(0, as.integer(maxspeed) + 5),
     ylab = "density", xlab = "speed of particles", main = "")
lines(v.vec, theoretical, lwd = 2, col = "blue")


#--------------------------------------------------
# Subsection 4.4. Advanced plotting functionalities
#--------------------------------------------------

###################################################
### Initialize RNetLogo (if not already done) and
### load the Urban Suite (Sprawl Effect) Model
###################################################
## library("RNetLogo")
# ToDo: adapt this path
## nl.path <- file.path(getwd(), "NetLogo 5.0.5")
# ToDo: adapt nl.version when using different NetLogo version
## NLStart(nl.path, gui = FALSE, nl.version = 5)
model.path <- "models/Curricular Models/Urban Suite"
model.name <- "Urban Suite - Sprawl Effect.nlogo"
NLLoadModel(file.path(nl.path, model.path, model.name))
# maybe set a random seed
# NLCommand("random-seed 2147483647")


###################################################
### Set the parameters for the simulation
###################################################
NLCommand("resize-world -20 20 -20 20")
NLCommand("set smoothness 10",
          "set max-attraction 5",
          "set population 500",
          "set seeker-search-angle 200",
          "set seeker-patience 15",
          "set wait-between-seeking 5")


###################################################
### Setup the model and run it for 150 steps
###################################################
NLCommand("setup")
NLDoCommand(150, "go")


###################################################
### Get the values of the patch variable attraction
### as a matrix as well as the World dimensions
###################################################
attraction <- NLGetPatches("attraction", as.matrix = TRUE)
pxcor <- NLReport(c("min-pxcor", "max-pxcor"))
pycor <- NLReport(c("min-pycor", "max-pycor"))


###################################################
### Define a plot function for a combined perspective
### and contour plot
###################################################
kde2dplot <- function(d, ncol = 50, zlim = c(0, max(z)),
                      nlevels = 20, theta = 30, phi = 30)
{
  z   <- d$z
  nrz <- nrow(z)
  ncz <- ncol(z)
  couleurs  <- tail(topo.colors(trunc(1.4 * ncol)), ncol)
  fcol      <- couleurs[trunc(z / zlim[2] * (ncol - 1)) + 1]
  dim(fcol) <- c(nrz, ncz)
  fcol      <- fcol[-nrz, -ncz]
  par.default <- par(no.readonly = TRUE)
  par(mfrow = c(1, 2), mar = c(0, 0, 0, 0), cex = 1.5)
  persp(d, col = fcol, zlim = zlim, theta = theta, phi = phi,
        zlab = "attraction", xlab = "x", ylab = "y")

  par(mar = c(2, 2, 0.5, 0.5))
  image(d, col = couleurs)
  contour(d, add = TRUE, nlevels = nlevels)
  box()
  par(par.default)
}


###################################################
### Reorganize the simulation result and send it to
### the plot function
###################################################
d <- list(x = seq(pxcor[[1]], pxcor[[2]]),
     y = seq(pycor[[1]], pycor[[2]]),
     z = attraction)
kde2dplot(d)


#--------------------------------------------------
# Subsection 4.5. Time sliding visualization
#--------------------------------------------------

###################################################
### Load the rpanel package and define a helper
### function to set a logical variable to colors
###################################################
library("rpanel")
color.func <- function(color.var, colors, timedata) {
  color <- NULL
  if (!is.null(color.var)) {
    index.color <- which(names(timedata) == color.var)
    color <- timedata[[index.color]]
    color[color == FALSE] <- colors[1]
    color[color == TRUE] <- colors[2]
  }
  return(color)
}


###################################################
### Define a custom plot function using rp.slider
### of the rpanel package to browse through the plots
###################################################
plottimedata <- function(timedata.list, x.var, y.var, boxplot.var1,
                         boxplot.var2, color.var1 = NULL,
                         colors1 = "black", color.var2 = NULL,
                         colors2 = "black", mains = NULL, ...)
{
  # the drawing function, called when the slider position is changed
  timeslider.draw <- function(panel) {
    index.x <- which(names(timedata.list[[panel$t]]) == x.var)
    index.y <- which(names(timedata.list[[panel$t]]) == y.var)
    index.b1 <- which(names(timedata.list[[panel$t]]) == boxplot.var1)
    index.b2 <- which(names(timedata.list[[panel$t]]) == boxplot.var2)

    # if a color variable (logical) is given set the colors
    # using function defined above
    color1 <- color.func(color.var1, colors1, timedata.list[[panel$t]])
    color2 <- color.func(color.var2, colors2, timedata.list[[panel$t]])

    # 4 figures arranged in 2 rows and 2 columns with one title text line
    par(mfrow = c(2, 2), oma = c(0, 0, 1, 0))
    # create current plot
    plot(timedata.list[[panel$t]][[index.x]],
         timedata.list[[panel$t]][[index.y]],
         col = color1, main = mains[1], ...)
    plot(timedata.list[[panel$t]][[index.x]],
         timedata.list[[panel$t]][[index.y]],
         col = color2, main = mains[2], ...)
    boxplot(timedata.list[[panel$t]][[index.b1]], main = mains[3])
    boxplot(timedata.list[[panel$t]][[index.b2]], main = mains[4])
    title(paste("at time ",panel$t), outer = TRUE)
    panel
  }

  # create a control panel (hosting the slider)
  panel <- rp.control()

  # create a slider to switch the plot data
  rp.slider(panel, resolution = 1, var = t, from = 1,
            to = length(timedata.list), title = "Time",
            showvalue = TRUE, action = timeslider.draw)
}


###################################################
### Initialize RNetLogo (if not already done),
### load as well as set up the Virus Model, and
### run the model for 100 steps and save the
### turtles of every step in one entry of the
### timedata list
###################################################
## library("RNetLogo")
# adapt the path
## nl.path <- file.path(getwd(), "NetLogo 5.0.5")
# adapt nl.version when using different NetLogo version
## NLStart(nl.path, gui = TRUE, nl.version = 5)
model.path <- "models/Sample Models/Biology/Virus.nlogo"
NLLoadModel(file.path(nl.path, model.path))
NLCommand("setup")

nruns <- 100
timedata <- list()
for(i in 1:nruns) {
  NLCommand("go")
  timedata[[i]] <- NLGetAgentSet(c("who", "xcor", "ycor", "age",
                                   "sick?", "immune?", "sick-count"),
                                   "turtles")
}


###################################################
### Get the world dimensions to use for the plot,
### define colors to be used for turtle visualization
### and call the plottimedata function to brwose
### through the timedata list
###################################################
world.dim <- NLReport(c("(list min-pxcor max-pxcor)",
                        "(list min-pycor max-pycor)"))
colors1 <- c("green", "red")
colors2 <- c("red", "green")
plottimedata(timedata.list = timedata, x.var = "xcor", y.var = "ycor",
             xlab = "x", ylab = "y", color.var1 = "sick?",
             color.var2 = "immune?", boxplot.var1 = "sick-count",
             boxplot.var2 = "age", colors1 = colors1, colors2 = colors2,
             mains = c("Sick", "Immune", "Stick-count", "Age"),
             xlim = world.dim[[1]], ylim = world.dim[[2]])


#--------------------------------------------------
#--------------------------------------------------
# Section 5. Pitfalls
#--------------------------------------------------
#--------------------------------------------------

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Attention: The code chunks of this section
#            demonstrate pitfalls in using RNetLogo.
#            Be very careful in executing them.
#            Some of them result in an error.
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#--------------------------------------------------
# Subsection 5.3. Data type
#--------------------------------------------------

###################################################
### Report a list of numbers
###################################################
NLReport("(list 24 23 22)")


###################################################
### Report a list of strings
###################################################
NLReport("(list \"foo1\" \"foo2\" \"foo3\")")


###################################################
### Report a list of mixed strings and numbers (will fail)
###################################################
try(NLReport("(list 24 \"foo\" 22)"))


###################################################
### load and reconfigure the forst fire model,
### this is unvisible in the paper
###################################################
model.path <- file.path("models", "Sample Models", "Earth Science", "Fire.nlogo")
NLLoadModel(file.path(nl.path, model.path))
NLCommand("resize-world 0 4 0 2")
NLCommand("random-seed 3452134")
NLCommand("setup")


###################################################
### These are the codes of the Table
###################################################
NLDoReport(2, "go", "(list count fires count embers)",
           as.data.frame = TRUE)
NLDoReport(2, "go", c("count fires","count embers"),
           as.data.frame = TRUE)
NLDoReport(2, "go", c("count turtles", "(list count fires count embers)"),
           as.data.frame = TRUE)


###################################################
### Get a data.frame of turtle variables
###################################################
res <- NLDoReport(3, "go", "[who] of turtles",
                  as.data.frame = TRUE)
str(res)


###################################################
### Close NetLogo (if still open)
###################################################
NLQuit()
