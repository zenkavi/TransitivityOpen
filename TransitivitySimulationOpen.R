#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#Usage:
#Rscript --vanilla TransitivitySimulationOpen.R n noise output_dir choice_type

#test if all arguments are supplied
# test if there is at least one argument: if not, return an error
if (length(args)<4) {
  stop("Arguments are missing. Usage: Rscript --vanilla TransitivitySimulationOpen.R n noise output_dir choice_type", call.=FALSE)
}

n <- args[1]
noise <- args[2]
output_dir <- args[3]
choice_type <- args[4]


####################
#Setup
####################

library(dplyr)

# setwd()

set.seed(9998877)

####################
#Simulation functions
####################

#Row level intransitive calculation (simplified)
comb.row.fn.sim <- function (df, comb.row) {
  # INPUT 
  #   df: individual participants data
  #   comb.row: comb data frame split by row
  j <- which((df$Image_left == comb.row$A | df$Image_right == comb.row$A) &
               (df$Image_left == comb.row$B | df$Image_right == comb.row$B))
  #Record choice for A vs B
  ApreftoB <- ifelse(df$Choice.image[j] == comb.row$A, 1, 0)
  
  #Find trial row in df that has the B vs C choice in comb
  k <- which((df$Image_left == comb.row$B | df$Image_right == comb.row$B) &
               (df$Image_left == comb.row$C | df$Image_right == comb.row$C))
  #Record choice for B vs C
  BpreftoC <- ifelse(df$Choice.image[k] == comb.row$B, 1, 0)
  
  #Find trial row in df that has the A vs C choice in comb
  l <- which((df$Image_left == comb.row$A | df$Image_right == comb.row$A) &
               (df$Image_left == comb.row$C | df$Image_right == comb.row$C))
  #Record choice for A vs C
  CpreftoA <- ifelse(df$Choice.image[l] == comb.row$C, 1, 0)
  
  #A triplet is intransitive if either A>B, B>C and C>A OR A<B, B<C and C<A (the second is the reverse)
  Intrans <- ifelse (ApreftoB == 1 & BpreftoC == 1 & CpreftoA == 1, 1, 
                     ifelse(ApreftoB == 0 & BpreftoC == 0 & CpreftoA == 0, 1, 0))
  
  # setup data to return
  ret.dat <- data.frame(A = comb.row$A, 
                        B = comb.row$B,
                        C = comb.row$C,
                        ApreftoB = ApreftoB,
                        BpreftoC = BpreftoC,
                        CpreftoA = CpreftoA,
                        Intrans = Intrans)
  return(ret.dat)
}

#Apply row level intransitive calculation to each subject (simplified) 
comb.fn.sim <- function(Data.cut) {
  images <- rep(1:20)
  comb.sim <- t(combn(images,3))
  comb.sim <- as.data.frame(comb.sim)
  names(comb.sim) <- c("A", "B", "C")
  comb.sim$ApreftoB <- rep(NA, nrow(comb.sim))
  comb.sim$BpreftoC <- rep(NA, nrow(comb.sim))
  comb.sim$CpreftoA <- rep(NA, nrow(comb.sim))
  comb.sim$Intrans <- rep(NA, nrow(comb.sim))
  comb.sim <- mutate(comb.sim, 
                     id = 1:nrow(comb.sim))
  comb.sim %>% 
    group_by(id) %>%
    do(comb.row.fn.sim(df = Data.cut, .))
}

#Generate random values for one person
create.new.data <- function(){
  values <- rnorm(20, 0, 1)
  images <- rep(1:20)
  noise.right <- rnorm(190, 0, 1)
  noise.left <-  rnorm(190, 0, 1)
  new.data <- as.data.frame(cbind(t(combn(values,2)), t(combn(images,2)), noise.right, noise.left))
  return(new.data)
}


#Create choices based on values adding noise for n number of subjects
#noise levels: 0.01, 0.05, 0.10, 0.25, 0.50
# simulate.choice <- function(n, alpha){
simulate.choice <- function(alpha){  
  # new.data <- plyr::ldply(as.data.frame(replicate(n, create.new.data())),data.frame)
  new.data <- create.new.data()
  
  # names(new.data) <- c("f.id", "Value_left", "Value_right", "Image_left", "Image_right", "noise.left", "noise.right")
  names(new.data) <- c("Value_left", "Value_right", "Image_left", "Image_right", "noise.left", "noise.right")
  
  new.data$Choice.left.P <- 1/(1+exp(new.data$Value_right - new.data$Value_left))
  
  new.data$Choice.left1.right0 <- round(new.data$Choice.left.P)
  
  new.data$Choice.left.P.noise <- 1/(1+exp(((1-alpha)*new.data$Value_right + (alpha * new.data$noise.right)) -
                                             ((1-alpha)*new.data$Value_left + (alpha * new.data$noise.left))))
  
  #hardmax
  new.data$Choice.left1.right0.noise <- ifelse(new.data$Choice.left.P.noise >= 0.5, 1, 0)
  
  #flip a coin with that p
  new.data$Choice.left1.right0.rbinom <- rbinom(190,1, prob = new.data$Choice.left.P.noise)
  
  if(choice_type == "hardmax"){
    new.data$Choice.image <- ifelse(new.data$Choice.left1.right0.noise == 1, new.data$Image_left, new.data$Image_right)
  }
  else if(choice_type == "rbinom"){
    new.data$Choice.image <- ifelse(new.data$Choice.left1.right0.rbinom == 1, new.data$Image_left, new.data$Image_right)
  }
  
  return(new.data)
}

####################
#Simulate
####################

output_df = data.frame(pct_Intrans = unlist(replicate(n, comb.fn.sim(simulate.choice(noise)) %>% summarise(pct_Intrans = sum(Intrans)/1140*100))))

####################
#Save output
####################

write.csv(output_df, paste0(output_dir,"sim_n_",n,"_noise_",noise, "_choice_", choice_type.csv))


