#clear the memory
rm(list=ls())

# #clear the console
cat("\014")

##############################
# Import the Egg field data#
##############################

# 1988
#Field.Egg <- read.table("Field_Egg_CIV1988.txt", sep = "\t", header = FALSE, dec =".")
#day.Egg   <- read.table("Day_Egg_CIV1988.txt", sep = "\t", header = FALSE, dec =".")

# 1989
#Field.Egg <- read.table("Field_Egg_CIV1989.txt", sep = "\t", header = FALSE, dec =".")
#day.Egg   <- read.table("Day_Egg_CIV1989.txt", sep = "\t", header = FALSE, dec =".")

# 1990
#Field.Egg <- read.table("Field_Egg_CIV1990.txt", sep = "\t", header = FALSE, dec =".")
#day.Egg   <- read.table("Day_Egg_CIV1990.txt", sep = "\t", header = FALSE, dec =".")

#Field.Egg <- as.data.frame(Field.Egg)

#day.Egg   <-  as.data.frame(day.Egg)

#Egg.data  <- cbind(day.Egg , Field.Egg)

#colnames(Egg.data) <- c("day", "egg")

#head(Egg.data)

##############################
# Import the Nymph field data#
##############################

# 1988
#Field.Nymph <- read.table("Field_Nymph_CIV1988.txt", sep = "\t", header = FALSE, dec =".")
#day.Nymph <- read.table("Day_Nymph_CIV1988.txt", sep = "\t", header = FALSE, dec =".")

# 1989
#Field.Nymph <- read.table("Field_Nymph_CIV1989.txt", sep = "\t", header = FALSE, dec =".")
#day.Nymph <- read.table("Day_Nymph_CIV1989.txt", sep = "\t", header = FALSE, dec =".")

# 1990
Field.Nymph <- read.table("Field_Nymph_CIV1990.txt", sep = "\t", header = FALSE, dec =".")
day.Nymph <- read.table("Day_Nymph_CIV1990.txt", sep = "\t", header = FALSE, dec =".")

Field.Nymph <- as.data.frame(Field.Nymph)

day.Nymph <-  as.data.frame(day.Nymph)

Nymph.data <- cbind(day.Nymph , Field.Nymph)

colnames(Nymph.data) <- c("day", "nymph")

head(Nymph.data)

##############################
# Import the Adult field data#
##############################

# 1988
#Field.Adult <- read.table("Field_Adult_CIV1988.txt", sep = "\t", header = FALSE, dec =".")
#day.Adult <- read.table("Day_Adult_CIV1988.txt", sep = "\t", header = FALSE, dec =".")

# 1989
#Field.Adult <- read.table("Field_Adult_CIV1989.txt", sep = "\t", header = FALSE, dec =".")
#day.Adult <- read.table("Day_Adult_CIV1989.txt", sep = "\t", header = FALSE, dec =".")

# 1990
Field.Adult <- read.table("Field_Adult_CIV1990.txt", sep = "\t", header = FALSE, dec =".")
day.Adult <- read.table("Day_Adult_CIV1990.txt", sep = "\t", header = FALSE, dec =".")

Field.Adult <- as.data.frame(Field.Adult)

day.Adult <-  as.data.frame(day.Adult)

Adult.data <- cbind(day.Adult , Field.Adult)

colnames(Adult.data) <- c("day", "adult")

head(Adult.data)



####################################
# Import Fortran simulation result #
####################################

#simulation.output <- read.table("Best_PopulationDensity_CIV1988.txt")
#simulation.output.no.age <- read.table("No_Age_PopulationDensity_CIV1988.txt")

#simulation.output <- read.table("Best_PopulationDensity_CIV1989.txt")
#simulation.output.no.age <- read.table("No_Age_PopulationDensity_CIV1989.txt")

simulation.output <- read.table("Best_PopulationDensity_CIV1990.txt")
simulation.output.no.age <- read.table("No_Age_PopulationDensity_CIV1990.txt")

colnames(simulation.output) <- c("day", "egg", "nymph" , "adult")
colnames(simulation.output.no.age) <- c("day", "egg",  "nymph" , "adult")

simulation.output <- as.data.frame(simulation.output)
simulation.output.no.age <- as.data.frame(simulation.output.no.age)

head(simulation.output)
head(simulation.output.no.age)




###################################
#  Compute RSS - chi_sqr - R_sqr  #
###################################

# Chi - squared calculator
Chisqr<-function(observed,expected){
  
  observed <- observed + 0.01
  expected <- expected + 0.01
  
  Chi_squared <- sum(((observed-expected)^2)/expected)  # Chi-squared measure  
  
  return(Chi_squared )
  
}

#  Residual sum of squares calculator
RSS <-function(observed,expected){
  
  RSS <- sum((observed-expected)^2)  # RSS measure  
  
  return(RSS)
  
}

# R squared calculator
Rsqr<-function(observed,expected){
  
  RSS.p <- RSS(observed,expected)  # Residual sum of squares
  
  TSS <- sum((observed - mean(expected))^2)   # Total sum of squares
  
  R_squared <- 1.0 - (RSS.p/TSS)  # R-squared measure  
  
  return(R_squared)
}

#########################################################################
# -------------------  with No age calibration--------------------------#
#########################################################################


#observed.egg <-  Egg.data$egg
#expected.egg <-  simulation.output.no.age$egg[Egg.data$day[]]

observed.nymph <-  Nymph.data$nymph
expected.nymph <-  simulation.output.no.age$nymph[Nymph.data$day[]]

observed.adult <-  Adult.data$adult
expected.adult <-  simulation.output.no.age$adult[Adult.data$day[]]

#Chisqr.egg <- Chisqr(observed.egg,expected.egg)
#RSS.egg    <- RSS(observed.egg,expected.egg)
#Rsqr.egg   <- Rsqr(observed.egg,expected.egg)

Chisqr.nymph <- Chisqr(observed.nymph,expected.nymph)
RSS.nymph    <- RSS(observed.nymph,expected.nymph)
Rsqr.nymph   <- Rsqr(observed.nymph,expected.nymph)

Chisqr.adult <- Chisqr(observed.adult,expected.adult)
RSS.adult    <- RSS(observed.adult,expected.adult)
Rsqr.adult   <- Rsqr(observed.adult,expected.adult)


print(paste0(" With No Age  "))
#print(paste0("Egg  "," RSS= ",RSS.egg," Chisqr= ",Chisqr.egg," Rsqr= ",Rsqr.egg))
print(paste0("Nymph  "," RSS= ",RSS.nymph," Chisqr= ",Chisqr.nymph," Rsqr= ",Rsqr.nymph))
print(paste0("Adult  "," RSS= ",RSS.adult," Chisqr= ",Chisqr.adult," Rsqr= ",Rsqr.adult)) 
print(paste0(" RSS = ", RSS.nymph+RSS.adult))


####################################################################
# ----------------  with age calibration --------------------------#
####################################################################

#observed.egg <-  Egg.data$egg
#expected.egg <-  simulation.output$egg[Egg.data$day[]]

observed.nymph <-  Nymph.data$nymph
expected.nymph <-  simulation.output$nymph[Nymph.data$day[]]

observed.adult <-  Adult.data$adult
expected.adult <-  simulation.output$adult[Adult.data$day[]]

#Chisqr.egg <- Chisqr(observed.egg,expected.egg)
#RSS.egg    <- RSS(observed.egg,expected.egg)
#Rsqr.egg   <- Rsqr(observed.egg,expected.egg)

Chisqr.nymph <- Chisqr(observed.nymph,expected.nymph)
RSS.nymph    <- RSS(observed.nymph,expected.nymph)
Rsqr.nymph   <- Rsqr(observed.nymph,expected.nymph)

Chisqr.adult <- Chisqr(observed.adult,expected.adult)
RSS.adult    <- RSS(observed.adult,expected.adult)
Rsqr.adult   <- Rsqr(observed.adult,expected.adult)

print(paste0(" With Age  "))
#print(paste0("Egg  "," RSS= ",RSS.egg," Chisqr= ",Chisqr.egg," Rsqr= ",Rsqr.egg))
print(paste0("Nymph  "," RSS= ",RSS.nymph," Chisqr= ",Chisqr.nymph," Rsqr= ",Rsqr.nymph))
print(paste0("Adult  "," RSS= ",RSS.adult," Chisqr= ",Chisqr.adult," Rsqr= ",Rsqr.adult))  
print(paste0(" RSS = ", RSS.nymph+RSS.adult))



############################################
# Graphics to compare data and simulations #
############################################


# 1988
#Optim.result  <- 6.8235575151724881e-3

# 1989
#Optim.result  <- 0.52095838112989723

# 1990
#Optim.result  <- 9.558451304030433e-2


# 1988
#day.of.start <- round(13.126192835541286 , 0 )

#day.of.start.no.age <- round(13.3785674590269 , 0 ) <-- Careful !

# 1989
#day.of.start <- round(60.516150702197116 , 0 )

#day.of.start.no.age <- round( 60.4726398532158, 0 ) <-- Careful !

# 1990
#day.of.start <- round(17.1428899957183 , 0 ) 

#day.of.start.no.age <- round( 17.0000000000000, 0 ) <-- Careful !

#print(paste0(" day of start after 1st Nov = ", day.of.start))


# #########################
# #####    Egg Plot  ####
# #########################
# 
# library(latex2exp)
# #Default margins of R plot
# bottom <- 5.1
# left   <- 4.1
# top    <- 4.1
# right  <- 2.1
# 
# par(mar = c(bottom, left + 0.7, top, right) )#increase left margin with 0.7
# 
# plot( simulation.output.no.age$day, simulation.output.no.age$egg,
#       lty=1, type="l", lwd = 4,
#       xlab= "Days from 18 November 1990",
#       ylab= "Total Egg (#)",
#       xlim =c(1,max(simulation.output.no.age$day)+20),
#       ylim =c(0,max(simulation.output.no.age$egg)+500),
#       col="black",
#       cex.lab=2,
#       frame.plot = FALSE,
#       axes=FALSE,
#       font.lab=2)
# 
# axis(side = 1, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)
# 
# axis(side = 2, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)
# 
# 
# points(Egg.data$day, Egg.data$egg, 
#        pch = 17, col = "blue" , cex=3)
# 
# legend("bottomright",
#        inset     = c(0,1),
#        xpd       = TRUE,
#        border    = "black",
#        legend    = c("Simulation","Field data"),
#        col       = c("black","blue"),
#        pch       = c(NA,16),
#        lty       = c(2,NA),
#        lwd       = c(3,NA),
#        cex       = 2.0,
#        #title     = "Nymph" ,
#        horiz     =  TRUE,
#        bty       = "n",
#        #bg        ='lightgray',
#        text.font = 2)
# 
# 
# 
# #############################
# #####    RSS Egg Plot  ####
# #############################
# 
# expected.egg <-  simulation.output.no.age$egg[Egg.data$day[]]
# 
# y <- (expected.egg - Egg.data$egg)^2
# 
# library(latex2exp)
# #Default margins of R plot
# bottom <- 5.1
# left   <- 4.1
# top    <- 4.1
# right  <- 2.1
# 
# par(mar = c(bottom, left + 0.7, top, right) )#increase left margin with 0.7
# 
# plot(Egg.data$day , y ,
#      lty=1, type="l", lwd = 4,
#      xlab= "Days from 18 November 1990",
#      ylab= "SOR - Egg",
#      xlim =c(1,max(Egg.data$day)+20),
#      ylim =c(0, max(y) + 100),
#      col="red",
#      cex.lab=2,
#      frame.plot = FALSE,
#      axes=FALSE,
#      font.lab=2)
# 
# axis(side = 1, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)
# 
# axis(side = 2, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)



#########################
#####    Nymph Plot  ####
#########################

#tiff(file="Figure1.tiff",
#     width=935, height=665, res=100)

library(latex2exp)
#Default margins of R plot
bottom <- 5.1
left   <- 4.1
top    <- 4.1
right  <- 2.1

par(mar = c(bottom, left + 0.7, top, right) )#increase left margin with 0.7

plot( simulation.output.no.age$day, simulation.output.no.age$nymph,
      lty=1, type="l", lwd = 6,
      xlab= "Days from 17 November 1990",
      ylab= "Total Nymph (#)",
      xlim =c(1,max(simulation.output.no.age$day)+20),
      ylim =c(0,max(simulation.output.no.age$nymph)+100),
      col="black",
      cex.lab=2,
      frame.plot = FALSE,
      axes=FALSE,
      font.lab=2)

axis(side = 1, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)

axis(side = 2, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)


points(Nymph.data$day, Nymph.data$nymph, 
       pch = 17, col = "blue" , cex=3)

#dev.off()


legend("bottomright",
       inset     = c(0,1),
       xpd       = TRUE,
       border    = "black",
       legend    = c("Simulation","Field data"),
       col       = c("black","red"),
       pch       = c(NA,16),
       lty       = c(2,NA),
       lwd       = c(3,NA),
       cex       = 2.0,
       #title     = "Nymph" ,
       horiz     =  TRUE,
       bty       = "n",
       #bg        ='lightgray',
       text.font = 2)



#############################
#####    RSS Nymph Plot  ####
#############################

#tiff(file="Figure2.tiff",
#     width=935, height=665, res=100)

expected.nymph <-  simulation.output.no.age$nymph[Nymph.data$day[]]

y <- (expected.nymph - Nymph.data$nymph)^2

library(latex2exp)
#Default margins of R plot
bottom <- 5.1
left   <- 4.1
top    <- 4.1
right  <- 2.1

par(mar = c(bottom, left + 0.7, top, right) )#increase left margin with 0.7

plot(Nymph.data$day , y ,
     lty=1, type="l", lwd = 4,
     xlab= "Days from 17 November 1990",
     ylab= "SOR - Nymph",
     xlim =c(1,max(Nymph.data$day)+20),
     ylim =c(0, max(y) + 1000),
     col="blue",
     cex.lab=2,
     frame.plot = FALSE,
     axes=FALSE,
     font.lab=2)

axis(side = 1, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)

axis(side = 2, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)

#dev.off()

#########################
#####    Adult Plot  ####
#########################

#tiff(file="Figure3.tiff",
#     width=935, height=665, res=100)

#Default margins of R plot
bottom <- 5.1
left   <- 4.1
top    <- 4.1
right  <- 2.1

par(mar = c(bottom, left + 0.7, top, right) )#increase left margin with 0.7


plot( simulation.output.no.age$day, simulation.output.no.age$adult,
      lty=1, type="l", lwd = 6,
      xlab= "Days from 17 November 1990",
      ylab= "Total Adult (#)",
      xlim =c(1,max(simulation.output.no.age$day)+20),
      ylim =c(0,max(simulation.output.no.age$adult)+15),
      col="black",
      cex.lab=2,
      frame.plot = FALSE,
      axes=FALSE,
      font.lab=2)

axis(side = 1, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)

axis(side = 2, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)


points(Adult.data$day, Adult.data$adult, 
       pch = 19, col = "darkgrey" , cex=3)

#dev.off()

legend("bottomright",
       inset     = c(0,1),
       xpd       = TRUE,
       border    = "black",
       legend    = c("Simulation","Field data"),
       col       = c("black","darkgrey"),
       pch       = c(NA,15),
       lty       = c(4,NA),
       lwd       = c(4,NA),
       cex       = 2.0,
       #title     = "Nymph" ,
       horiz     =  TRUE,
       bty       = "n",
       #bg        ='lightgray',
       text.font = 2)


#############################
#####    RSS Adult Plot  ####
#############################

expected.adult <-  simulation.output.no.age$adult[Adult.data$day[]]

y <- (expected.adult - Adult.data$adult)^2

#tiff(file="Figure4.tiff",
#     width=935, height=665, res=100)

library(latex2exp)
#Default margins of R plot
bottom <- 5.1
left   <- 4.1
top    <- 4.1
right  <- 2.1

par(mar = c(bottom, left + 0.7, top, right) )#increase left margin with 0.7

plot(Adult.data$day , y ,
     lty=1, type="l", lwd = 4,
     xlab= "Days from 17 November 1990",
     ylab= "SOR - Adult",
     xlim =c(1,max(Adult.data$day)+10),
     ylim =c(0, max(y) +100),
     col="darkgrey",
     cex.lab=2,
     frame.plot = FALSE,
     axes=FALSE,
     font.lab=2)

axis(side = 1, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)

axis(side = 2, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)

#dev.off()

####################################################################
#####  Model Precision Assessment With Linear Regression ###########
####################################################################

reg.conf.intervals <- function(x, y) {
  
  n <- length(y) # Find length of y to use as sample size
  
  lm.out <- lm(y ~ x) # Fit linear model
  
  newx = seq(min(x), max(x), by = 0.05 )
  
  conf_interval <- predict(lm.out, 
                           newdata=data.frame(x=newx), 
                           interval="confidence",
                           level = 0.95)
  
  #plot(x, y, xlab="x", ylab="y", main="Regression")
  #abline(lm.out, col="lightblue")
  #lines(newx, conf_interval[,2], col="blue", lty=2)
  #lines(newx, conf_interval[,3], col="blue", lty=2)
  
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(conf_interval[,2], conf_interval[,3], newx))
  colnames(bands) <- c('Lower.Confidence.Band', 'Upper.Confidence.Band','newx')
  
  return(bands)
  
}


lin.reg.stats <- function(x, y) {
  
  lm.r<-lm(y ~ x)
  
  R_squared_linear <- summary(lm.r)$r.squared
  
  p_Value_linear   <- anova(lm.r)$'Pr(>F)'[1]
  
  AIC_linear       <- AIC(lm.r)
  
  statistics <- data.frame(cbind(R_squared_linear, p_Value_linear, AIC_linear))
  
  colnames(statistics) <- c('R.Sqr', 'p.val' , 'AIC' )
  
  return(statistics)
  
}

#x.egg <- observed.egg 

#y.egg <- expected.egg

x.nymph <- observed.nymph 

y.nymph <- expected.nymph 

x.adult <- observed.adult

y.adult <- expected.adult

#  Egg
#lm.r1<-lm(y.egg ~ x.egg)
#conf.intervals.1 <- reg.conf.intervals(x.egg, y.egg)
#reg.stats.1 <- lin.reg.stats(x.egg, y.egg)
#summary(lm.r1)
#head(reg.stats.1)

#  Nymph
lm.r2<-lm(y.nymph ~ x.nymph)
conf.intervals.2 <- reg.conf.intervals(x.nymph, y.nymph)
reg.stats.2 <- lin.reg.stats(x.nymph, y.nymph)
summary(lm.r2)
head(reg.stats.2)

# Adult
lm.r3<-lm(y.adult ~ x.adult)
conf.intervals.3 <- reg.conf.intervals(x.adult, y.adult)
reg.stats.3 <- lin.reg.stats(x.adult, y.adult)
summary(lm.r3)
head(reg.stats.3)


# # Egg plot
# bottom <- 5.1
# left   <- 4.1
# top    <- 4.1
# right  <- 2.1
# 
# par(mar = c(bottom+0.8, left + 0.7, top, right)) #increase left margin with 0.7
# 
# 
# plot(x.egg,
#      y.egg,
#      pch = 19,
#      col = "darkgrey",
#      cex = 3,
#      ylab= "Modelled data (Egg)",
#      xlab= "Observed data (Egg)",
#      # xlim =c(1,MAXI_x+5),
#      # ylim =c(-0.5,1.5),
#      cex.lab=2,
#      cex.axis=2,
#      frame.plot = FALSE,
#      axes=FALSE,
#      font.lab=2)
# 
# lines(x.egg, predict(lm.r1, data.frame(x.egg = x.egg )),
#       col = 'black', lty = 1, lwd = 4)
# 
# lines( conf.intervals.1$newx, 
#        conf.intervals.1$Lower.Confidence.Band,
#        col = 'red', lty = 3, lwd = 4)
# 
# lines(conf.intervals.1$newx, 
#       conf.intervals.1$Upper.Confidence.Band, 
#       col = 'red', lty = 3, lwd = 4)
# 
# axis(side = 1, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)
# 
# axis(side = 2, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)

#x <- c(10,20,25,35)
#ypos <- 1.25
#y <- rep(ypos, times = length(x) )
#points <- c("1", "2", "3", "4")
#text(x, y, labels = points, pos = 3, col = "blue", font.lab=1, cex=3)

#tiff(file="Figure5.tiff",
#     width=935, height=665, res=100)

# Nymph plot
bottom <- 5.1
left   <- 4.1
top    <- 4.1
right  <- 2.1

par(mar = c(bottom+0.8, left + 0.7, top, right)) #increase left margin with 0.7


plot(x.nymph,
     y.nymph,
     pch = 17,
     col = "blue",
     cex = 3,
     ylab= "Modelled data (Nymph)",
     xlab= "Observed data (Nymph)",
     xlim =c(0,max(x.nymph)+50),
     ylim =c(0,max(y.nymph)+50),
     cex.lab=2,
     cex.axis=2,
     frame.plot = FALSE,
     axes=FALSE,
     font.lab=2)

lines(x.nymph, predict(lm.r2, data.frame(x.nymph = x.nymph )),
      col = 'black', lty = 1, lwd = 4)

lines( conf.intervals.2$newx, 
       conf.intervals.2$Lower.Confidence.Band,
       col = 'red', lty = 3, lwd = 6)

lines(conf.intervals.2$newx, 
      conf.intervals.2$Upper.Confidence.Band, 
      col = 'red', lty = 3, lwd = 6)

axis(side = 1, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)

axis(side = 2, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)

#dev.off()
#x <- c(10,20,25,35)
#ypos <- 1.25
#y <- rep(ypos, times = length(x) )
#points <- c("1", "2", "3", "4")
#text(x, y, labels = points, pos = 3, col = "blue", font.lab=1, cex=3)


#tiff(file="Figure6.tiff",
#     width=935, height=665, res=100)
# Adult plot
bottom <- 5.1
left   <- 4.1
top    <- 4.1
right  <- 2.1

par(mar = c(bottom+0.8, left + 0.7, top, right)) #increase left margin with 0.7


plot(x.adult,
     y.adult,
     pch = 19,
     col = "darkgrey",
     cex = 3,
     ylab= "Modelled data (Adult)",
     xlab= "Observed data (Adult)",
     xlim =c(0,max(x.adult)+10),
     ylim =c(0,max(y.adult)+10),
     cex.lab=2,
     cex.axis=2,
     frame.plot = FALSE,
     axes=FALSE,
     font.lab=2)

lines(x.adult, predict(lm.r3, data.frame(x.adult = x.adult )),
      col = 'black', lty = 1, lwd = 4)

lines( conf.intervals.3$newx, 
       conf.intervals.3$Lower.Confidence.Band,
       col = 'red', lty = 3, lwd = 6)

lines(conf.intervals.3$newx, 
      conf.intervals.3$Upper.Confidence.Band, 
      col = 'red', lty = 3, lwd = 6)

axis(side = 1, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)

axis(side = 2, lwd = 3 ,cex.lab=1.5, cex.axis =2, font.lab=2)

#dev.off()

#x <- c(10,20,25,35)
#ypos <- 1.25
#y <- rep(ypos, times = length(x) )
#points <- c("1", "2", "3", "4")
#text(x, y, labels = points, pos = 3, col = "blue", font.lab=1, cex=3)

####################################################################
#####  Estimate of the invasion date                     ###########
#################################################################### 

# Eggs developmental rate 

Eggs.developmental.rate <- function(temperature){
  
  a  = 8.426e-3
  
  b  = 2.119
  
  Tmin  = 1.015e+1
  
  Tmax  =  3.802e+1
  
  Upper_Dev_Threshold = 36.0
  
  Lower_Dev_Threshold = 10.18476
  
  Egg_Devel_Rate = a*(temperature-Tmin)/( 1.0 + (b^(temperature-Tmax)) )
  
  if(temperature < Lower_Dev_Threshold) {Egg_Devel_Rate <- 0.0}
  
  if(temperature > Upper_Dev_Threshold) {Egg_Devel_Rate <- 0.0}
  
  return(Egg_Devel_Rate)
  
}


#Adiopodoumé, 20KM west of Abidjan, Cote d'Ivoire
#Latitude:   5.3166666667
#Longitude: -4.2166666667

# 13 Jan 1988 to 13 Nov 1988
#date <- c("1988-01-13", "1988-11-13") # day.of.start = 13

# 01 Jan 1989 to 01 Jan 1990 
# date <- c("1989-01-01", "1990-01-01") # day.of.start = 61 

# 01 Nov 1990 to 31 Dec 1991
# date <- c("1990-10-01", "1990-11-30") # day.of.start = 30 

# The location of the Experimental site
lonlat.data <- c(-4.21, 5.31)

# Here we load the R file containing the function
get_weather_data <- function(lonlat, dates) {
  
  require(nasapower)
  
  wth <- as.data.frame( nasapower::get_power(
    community    = "ag",
    lonlat       = lonlat,
    pars         = c("T2M",
                     "T2M_MAX",
                     "T2M_MIN",
                     "RH2M",
                     "PRECTOTCORR"),
    dates        = dates,
    temporal_api = "daily"
  ) )
  
  
  wth <- wth[,c(7, 6, 8, 10, 9, 11, 12)]
  
  names(wth) <- c("YYYYMMDD", "DOY", "T2M", "T2MN", "T2MX", "RH2M", "RAIN")
  
  return(wth)
  
}

weather <- get_weather_data (lonlat.data, date)

last_date <- as.numeric(nrow(weather))

# initial physiological age from optimization
# 1988
r0  <- 6.8235575151724881e-3

# 1989
#r0 <- 0.52095838112989723

# 1990
#r0  <- 9.558451304030433e-2


repeat{
  
  last_date <- last_date - 1
  
  r0 <- r0 - Eggs.developmental.rate(weather$T2M[last_date])
  
  if(r0 <= 0){
    
    break
  }
  
}

print(paste0("The possible invasion date is = ",  weather$YYYYMMDD[last_date]))


############### <------------- Show OverEstimation ------------------>
#Optim.result <- read.table("OptmizationResult_with_Age_CIV1988.txt")
#Optim.result  <- as.data.frame(Optim.result)

#No_Optim.result <- read.table("OptmizationResult_No_Age_CIV1988.txt")
#No_Optim.result  <- as.data.frame(No_Optim.result)

#No_Optim.result <- read.table("OptmizationResult_No_Age_CIV1989.txt")
#No_Optim.result  <- as.data.frame(No_Optim.result)

#No_Optim.result <- read.table("OptmizationResult_No_Age_CIV1990.txt")
#No_Optim.result  <- as.data.frame(No_Optim.result)