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
# Farfan2015 <- function() {}

## Chambers et al, 2004: estimate surface area of a tree:
# see also RAINFOR manual, p. 52; diameter in cm 
# Note: A. Shenkin is developing our own surface area equation. 
Chambers2004_surfaceArea <- function(diameter) {
  x = (log10(diameter))
  surface_area = 10^(-0.015 - 0.686*x + (2.208*x)^2 - (0.627*x)^3)
  return(surface_area)
}


# find.wsg is a function that finds density data (wsg) from Chave's density database (wsg.txt, it is saved in the same directory as this function). It fits the density to species / if no species, to genus / if no genus, to family level.

find.wsg = function(family, genus, species, wsg){
  capply = function(str, ff) {sapply(lapply(strsplit(str, NULL), ff), paste, collapse="") }
  cap = function(char) {if (any(ind <- letters==char)) LETTERS[ind]    else char}
  capitalize = function(str) {ff <- function(x) paste(lapply(unlist(strsplit(x, NULL)),cap),collapse="")
                              capply(str,ff)}
  lower = function(char) {if (any(ind <- LETTERS==char)) letters[ind]    else char}
  lowerize = function(str) {ff <- function(x) paste(lapply(unlist(strsplit(x, NULL)),lower),collapse="")
                            capply(str,ff)}
  
  family=(as.character(family))
  genus=(as.character(genus))
  species=(as.character(species))
  
  w=which(is.na(family))
  family[w]='unknown'
  w=which(is.na(genus))
  genus[w]='unknown'
  w=which(is.na(species))
  species[w]='unknown'
  
  family=capitalize((family))
  genus=capitalize((genus))
  species=capitalize((species))
  
  family2=capitalize(as.character(wsg$FAMILY))
  genus2=capitalize(as.character(wsg$GENUS))
  species2=capitalize(as.character(wsg$SPECIES))
  
  fam.wsg=tapply(wsg$WSG, family2, mean,na.rm=T)
  gen.wsg=tapply(wsg$WSG, genus2, mean, na.rm=T)
  
  family2=names(fam.wsg)
  genus2=names(gen.wsg)
  
  m=match(family, family2)
  wsg.est=fam.wsg[m]
  
  m=match(genus, genus2)
  w=which(!is.na(m))
  wsg.est[w]=gen.wsg[m][w]
  
  m=match(species, species2)
  w=which(!is.na(m))
  wsg.est[w]=wsg$WSG[m][w]
  
  names(wsg.est)=species
  wsg.est
}

