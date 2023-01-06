
get_K_breaking_turn_per_inch_Millman <- function(turn,A) turn * sqrt(A)


breaking_turn_per_inch_Millman <- function(A,K=get_K_breaking_turn_per_inch_Millman(107,.250)) K/sqrt(A)#p178
torque_from_rubber_cross_sectional_area_ <- function(A,C) C*A^1.5
get_C_torque_from_rubber_cross_sectional_area <- function(torque,A) torque*A^(-1.5)

breaking_turn_per_inch_Sherman <- function(N,W) 160*(1-2*W)/sqrt(N)
torque_Sherman <- function(N,W) (.45+10*W)*N^1.38

#W width in [in]
#A area in [in^2]
#torque is in [in * Oz]






