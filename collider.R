################################################################
########## gw Feb 2024 - testing a classic collider ############

# Script to get minimal case going for exogenous SEM.
# Generates the variables of a collider.
# And a df called collider to be used in predictions. 

# Prelim
library(tidyverse)
rm(list = ls())
set.seed(88)

# Set of vars used
A
B
epsA
epsB

# Define strength of vars. Just pick some to get it working then do full range later 
p_A <- c(.1,.9) # ie A usually has value 1...
p_epsA <- c(.7,.3) #... but the 1's edge is weak
p_B <- c(.8,.2) # B rarely fires 1... 
p_epsB <- c(.3,.7) # but when it does it is strong


# Define prior probabilities
# Set up dataframe of all the variables we need and the possible values they can take
# (Would this be collider equivalent of pChoice?)
df <- data.frame(expand.grid(list(A = c(0,1),
                                  B = c(0,1),
                                  epsA = c(0,1),
                                  epsB = c(0,1),
                                  E = c(0,1))))
                                  #,structure=structure
# ))) %>% mutate(p = NA) # need it 0 and 1

# Prior - it sums to 1. Before any observations. Prior just repeats for the setup values, regardless of Effect 0/1
# need the +1 as otherwise it thinks '1' is the lefthand (first) index position as it starts from 1 not 0
df <- df %>% mutate(P = p_A[df$A+1] *
                      p_B[df$B+1] *
                      p_epsA[df$epsA+1] *
                      p_epsB[df$epsB+1]) 

write.csv(df, 'colliderprior.csv', row.names = F) # Remove rownames=F if you want the 1:nrow numbered


# Once I have the prior... what to do? Get the equivalent of pAction by running conjunctive or disjunctive?

# Function for conjunctive condition
conj_twovars_exognoise <- function(var1, eps1, var2, eps2) {
  (var1 & eps1) & (var2 & eps2)
}

# Function for disjunctive condition CHECK THIS - CAN IT DEF NOT BE BOTH SIDES ALL ON AT ONCE?
disj_twovars_exognoise <- function(var1, eps1, var2, eps2) {
  (var1 & eps1) | (var2 & eps2)
}

# Apply this function over df
df$disj <- disj_twovars_exognoise(df$A, df$epsA, df$B, df$epsB)

# Multiply by prior? Or by E to get what actually happened.
df$effect <- df$P*df$disj

# NEXT AT 20 FEB --- HOW TO USE PRIOR OF OBS? HOW TO ADD UP CF WORLDS


# Define some vars to help simulate outcomes
N <- 100 # number of samples, low for now, change later
# n <- 1:N         # number of samples
structure <- c('Conjunctive', 'Disjunctive') # Might not need this as only doing conjunctive structure


# WHAT NEXT?? START THE GENERIC ECESM CF SIMULATION TO DO CAUSAL SELECTION OVER OBSERVED OUTCOMES??





######## OLD ##############
# Sampling from uniform distribution for strength of nodes
Aran <- runif(N) # A vector of N random samples 0:1 for node A
Bran <- runif(N) # Same for node B
# Indicator function turns it to 1 if within the strength of node and 0 if outwith
Avals <- 0 + (Aran <= Ast)
Bvals <- 0 + (Bran <= Bst)

# Now do we do the same for the noise nodes??
epsAran <- runif(N) # A vector of N random samples 0:1 for node epsA
epsBran <- runif(N) # A vector of N random samples 0:1 for node epsB
epsAvals <- 0 + (epsAran <= epsAstr)
epsBvals <- 0 + (epsBran <= epsBstr)






# do it w both maths and simulation to get clear on how it works, see it comes out the same
# (1-base rate) for 0; base rate for 1s. Then THIS is the equivalent of pchoice to then feed to cf function

# Neil's hint: eps is never observed; (?..?). BUT they can be imputed in our example


# --------------- Now run minimal version of ecesm on this collider --------------------
# Idea to see what is transferable across the two and maybe later make a function 
# which could be applied to any set of causes



# NEXT STEPS
# 1. Generate counterfactuals and do the causal selection 
#       (can earlier script be repackaged? Do it in functions? Like tadeg's functions?) utils script? functions within fucntions?
# 2. Try with different strength and base rates (ours is set to 0.5 so less important). But where in this collider is 0.5
#       could repurpose K Oneil sampling increments
# Then:
# 3. Use the toy case to get good understanding of Tadeg's and Icard's models as we know what they predict because there is so much work on them
# 4. Expand to more fiddly cases

