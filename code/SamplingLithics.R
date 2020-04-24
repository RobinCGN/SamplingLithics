###################################################################################################################
#
#           SamplingLithics - Using Running Averages for Optimal Sampling/Data Recording of Lithic Artefacts
#
#              based on EasyGilf: Linstädter/Richter/Linstädter 2002
#
##################################################################################################################


#-- 0. Load Packages ----
require(plyr)

#-- 1. Import Data ----

data <- read.csv2("/home/Programmieren/R/SamplingLithics/data/data.csv")

head(data)

# feature = number of feature
# length = length of artifact in mm
# width = width of artifact in mm
# thickness = thickness of artifact in mm
# weight = weight of artifact in g
# type = type of blank (flake, blade or other type of blank)
# cortex = presence/absence of cortex
# burned = presence/absence of burning or traces of fire
# mod = presence/absence of modification (mod = artefact is a tool)

nrow(data)

# 520 artefacts in our sample data set

#-- 2. Prepare Data -----

# Recoding Missing Values ("999" to NA)
data$length[data$length==999] <- NA 
data$width[data$width==999] <- NA 
data$thickness[data$thickness==999] <- NA
data$weight[data$weight==999] <- NA 


# Summary statistics per field (numeric variables)
summary(data$length)
summary(data$width)
summary(data$thickness)
summary(data$weight)

# Counts per attribute (qualitativ variables/factors)
table(data$type)
table(data$cortex)
table(data$burned)
table(data$mod)


# add ID column:
data <- cbind(data, SampLithicsID = seq(1, nrow(data)))
head(data)


#-- 3. Define Sampling Variables ----

# Define nummber of batches/training sets (n_batch) and number of artefacts per batch (A)

n_batch = 20
A = 5


# Function to produce training sets
copy.data <- data
train <- data.frame()

for (i in 1:n_batch){
  dt = sort(sample(nrow(copy.data), A))
  train  <- rbind(train , cbind(copy.data[dt,], "Train_Set" = rep(i,A)))
  copy.data<-copy.data[-dt,]
  print(dt)
}

# Have a look at train set 1:
train[train$Train_Set==1,]
nrow(train)

# IDs should be unique / no dublicates:
nrow(train)==length(unique(train$SampLithicsID))

# IDs per batch
table(train$Train_Set)



#---- 4. Get running means of measurements (numeric variables) ----


# Function get running mean of measurments


run_mean <- numeric()
c <- NULL

get_run_var <- function(my_variable){
  for (i in 1:n_batch){
  select <- seq(1,i)
  c = mean(my_variable[train$Train_Set%in%select],na.rm = T)
  #print(c)
  run_mean <- c(run_mean, c)
  }
  return(run_mean)
}


get_run_diff <- function(my_variable){
  diff_prc <- (diff(my_variable)/(my_variable[-n_batch]/100))
  return(diff_prc)
}



weight_mean <- get_run_var(train$weight)
weight_diff_prc <- get_run_diff(weight_mean)

length_mean <- get_run_var(train$length)
length_diff_prc <- get_run_diff(length_mean)

width_mean <- get_run_var(train$width)
width_diff_prc <- get_run_diff(width_mean)

thickness_mean <- get_run_var(train$thickness)
thickness_diff_prc <- get_run_diff(thickness_mean)


# Compare to manual computing:
mean(train$length[train$Train_Set==1],na.rm = T)
mean(train$length[train$Train_Set%in%c(1,2)],na.rm = T)
mean(train$length[train$Train_Set%in%seq(1,n_batch)],na.rm = T)

length_mean[1]
length_mean[2]
length_mean[n_batch]



#-- 5. Get running proportions/shares of qualitative variables (factors) ----



# Proportions/share of numerical variables

t <- plyr::ddply(train, ~Train_Set, summarize, 
           blade_n = sum(table(type, exclude=c("flake", "other"))),
           burned_n = sum(table(burned, exclude=c("unburned"))),
           cortex_n = sum(table(cortex, exclude=c("no cortex"))),
           mod_n = sum(table(mod, exclude=c("unmod"))),
           all_n = sum(table(feature))
)

t


# Blades
blade_prc <- cumsum(t$blade_n)/(cumsum(t$all_n)/100)
blade_prc_diff <- diff(blade_prc)

# Burned
burned_prc <- cumsum(t$burned_n)/(cumsum(t$all_n)/100)
burned_prc_diff <- diff(burned_prc)

# Cortex
cortex_prc <- cumsum(t$cortex_n)/(cumsum(t$all_n)/100)
cortex_prc_diff <- diff(cortex_prc)

# Modification
mod_prc <- cumsum(t$mod_n)/(cumsum(t$all_n)/100)
mod_prc_diff <- diff(mod_prc)



#-- 6. Plotting ----


# Quantitative Values
par(mfrow=c(1,2))
plot(1, type="n", xlim=c(1, n_batch-1), ylim=c(-25, 25), ylab="Difference from last Batch (%)", xlab = "Batch No", xaxt = 'n')
axis(1, at = seq(1, n_batch-1), label=seq(2, n_batch))
polygon(c(0,n_batch,n_batch,0),c(5,5,-5,-5), col="lightgreen",border = NA)
lines(length_diff_prc, pch = 0, type="b", lty=4)
lines(weight_diff_prc, pch = 1, type="b", lty=3)
lines(width_diff_prc, pch = 2, type="b", lty=2)
lines(thickness_diff_prc, pch = 3, type="b", lty=5)
main_title <- paste0("Artef.= ",A," , batches = ", n_batch, ", n = ", A*n_batch,", N = ",nrow(data))
title(main=paste0("Quantitative Values\n", main_title))
legend("topright", c("length", "weight", "width", "thickness","+/- 5 %"),
       pch = c(0,1,2,3,15), col=c(rep("black",4),"lightgreen"),box.lty = 0)





# Qualitative Values
plot(1, type="n", xlim=c(1, n_batch-1), ylim=c(-25, 25), ylab="Difference from last Batch (%)", xlab = "Batch No", xaxt = 'n')
axis(1, at = seq(1, n_batch-1), label=seq(2, n_batch))
polygon(c(0,n_batch,n_batch,0),c(5,5,-5,-5), col="lightgreen",border = NA)
lines(blade_prc_diff, pch = 0, type="b", lty=4)
lines(burned_prc_diff, pch = 1, type="b", lty=3)
lines(cortex_prc_diff, pch = 2, type="b", lty=2)
lines(mod_prc_diff, pch = 3, type="b", lty=5)
#text(n_batch-1,blade_prc_diff[n_batch-1], paste(round(blade_prc[n_batch-1],1),"%") ,pos = 3)
#text(n_batch-1,burned_prc_diff[n_batch-1], paste(round(burned_prc[n_batch-1],1),"%") ,pos = 3)
#text(n_batch-1,cortex_prc_diff[n_batch-1], paste(round(cortex_prc[n_batch-1],1),"%") ,pos = 3)
text(n_batch-1,mod_prc_diff[n_batch-1], paste(round(mod_prc[n_batch-1],1),"%") ,pos = 3)
title(main=paste0("Qualitative Values\n", main_title))
legend("topright", c("Blade_prc", "Burned_prc", "Cortex_prc", "Mod_prc","+/- 5 %"),
       pch = c(0,1,2,3,15), col=c(rep("black",4),"lightgreen"),box.lty = 0)




#-- 6. Compare Estimates (samples) and overall values (original data set) ----



# Percentage of Blades in overall data set:
a <- prop.table(table(data$type))*100
a[1]


# Percentage of burned pieces in overall data set:
b <- prop.table(table(data$burned))*100
b[1]


# Percentage of burned pieces in overall data set:
c <- prop.table(table(data$cortex))*100
c[1]


# Percentage of tools in overall data set:
d <- prop.table(table(data$mod))*100
d
d[1]




results <- data.frame(A = rep(A, 4), n_batch = rep(n_batch,4),
                      
                      qual = c("blade_prc","burned_prc","cortx_prc","mod_prc"),
                      
                      qual_estimates = 
                        c(round(blade_prc[n_batch],1),
                          round(burned_prc[n_batch],1),
                          round(cortex_prc[n_batch],1),
                          round(mod_prc[n_batch],1)),
                      
                      qual_real = 
                        round(c(a[1],b[1],c[1],d[1]),1),
                      
                      quant = c("length", "width", "thickness", "weight"),
                      
                      quant_estimates = 
                        c(round(length_mean[n_batch],1),
                          round(width_mean[n_batch],1),
                          round(thickness_mean[n_batch],1),
                          round(weight_mean[n_batch],1)),
                      
                      quant_real = 
                        c(round(mean(data$length,na.rm = T),1),
                          round(mean(data$width,na.rm = T),1),
                          round(mean(data$thickness,na.rm = T),1),
                          round(mean(data$weight,na.rm = T),1))
                      
                      )

results <- cbind(results, qual_diff_prc = round(results$qual_real-results$qual_estimates,1))
results <- cbind(results, quant_diff_prc = round((results$quant_estimates-results$quant_real)/(results$quant_real/100),1))

results
