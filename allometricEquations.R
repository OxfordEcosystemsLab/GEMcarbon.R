### This script is to define a set of allometric equations that can be called from
## various pieces of the Carbon-Use-Efficiency code.

# Sebastian Sippel, 15.02.2014

## Chave et al, 2005 gives different allometric models to estimate aboveground biomass (AGB),
# three of which will be used in the R-Code:

# p. 91, Chave et al, 2005: model I.3; dry: 
Chave2005_dry <- function(diax, density, height) {
  AGB_est <- 0.112*(density*((diax)^2)*height)^0.916
  return(AGB_est)
}

# p. 90 and p. 95, Chave et al, 2005: model I.6; the 'simplest' model:
Chave2005_moist <- function(diax, density, height) {
  AGB_est <- 0.0509*density*((diax)^2)*height
  return(AGB_est)
}

Chave2005_wet <- function(diax, density, height) {
  AGB_est <- 0.0776*(density*((diax)^2)*height)^0.940
  return(AGB_est)
}

# These are the equations recommended in (Marthews et al. in review, 5 C Pools paper). 
#2=dry:  0.4730*0.001*0.112*(densitysA[tree_ind]*((diaxs)^2)*heightsa)^0.916 , 
#3=moist:  0.4730*0.001*0.0509*densitysA[tree_ind]*((diaxs)^2)*heightsa,
#4=wet:  0.4730*0.001*0.0776*(densitysA[tree_ind]*((diaxs)^2))*heightsa)^0.940

######### TO DO ############
## ADD Chave et al. 2014: http://onlinelibrary.wiley.com/doi/10.1111/gcb.12629/abstract
######### TO DO ############

## Chambers et al, 2004: estimate surface area of a tree:
# see also RAINFOR manual, p. 52; diameter in cm 
Chambers2004_surfaceArea <- function(diameter) {
  surface_area = 10^(-0.015-0.686*log10(diameter)+(2.208*log10(diameter)^2)-(0.627*log10(diameter)^3))
  return(surface_area)
}