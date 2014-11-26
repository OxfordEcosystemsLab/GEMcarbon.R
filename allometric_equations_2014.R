### This script is to define a set of allometric equations that can be called from various functions of the GEM code.
# Sebastian Sippel 15.02.2014

## Chave et al, 2005 gives different allometric models to estimate aboveground biomass (AGB), three of which will be used in the R-Code:
# p. 91, Chave et al, 2005: model I.3 dry: 
Chave2005_dry <- function(diax, density, height) {
  AGB_est <- 0.112*(density*((diax)^2)*height)^0.916
  return(AGB_est)
}

# p. 90, Chave et al, 2005: model I.6 moist:
Chave2005_moist <- function(diax, density, height) {
  AGB_est <- 0.0509*density*((diax)^2)*height
  return(AGB_est)
}

# p. 95, Chave et al, 2005: model I.6 wet:
Chave2005_wet <- function(diax, density, height) {
  AGB_est <- 0.0776*(density*((diax)^2)*height)^0.940
  return(AGB_est)
}

# Chave et al, 2014:
Chave2014 <- function(diax, density, height) {
  AGB_est <- 0.0673*(density*((diax)^2)*height)^0.976 
  return(AGB_est)
}

# Tree ferns Farfan et al., in prep:
Farfan2015 <- function() {
  
}

## Chambers et al, 2004: estimate surface area of a tree:
# see also RAINFOR manual, p. 52; diameter in cm 
Chambers2004_surfaceArea <- function(diameter) {
  surface_area = 10^(-0.015-0.686*log10(diameter)+(2.208*log10(diameter)^2)-(0.627*log10(diameter)^3))
  return(surface_area)
}