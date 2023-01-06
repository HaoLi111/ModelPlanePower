dia_propeller_efficiency_Russell <- function(RPM,MPH,J=.5) 88*MPH/RPM/J
#provided that J in flight is .5


efficiency_J_default=.7
pitch_propellerp_efficiency_Russell<- function(RPM,MPH,D,efficiency_J=efficiency_J_default) MPH/efficiency_J_default * 1.46667 / (RPM/60)

#pitch_propellerp_efficiency_Russell(3000,25,dia_propeller_efficiency_Russell(3000,25),
 #                                   efficiency_J = .7)


design_propeller_Russell<- function(RPM,MPH,max_J=.5,max_efficiency=.7){
  D = dia_propeller_efficiency_Russell(RPM,MPH,J=max_J)
  P = pitch_propellerp_efficiency_Russell(RPM,MPH,D,efficiency_J = max_efficiency)
  Width = D * .05 #p68
  R = D/2
  list(D=D,
       R=R,
       P=P,
       Width = Width,
  Thickness_Wood = .125 * Width,
  Thickness_Metal = .075 * Width,
  Width_Thickness_measure_pt = .75 * D/2,
  R_u = .8*R,
  R_l = .6*R,
  R_boss = .15*R)

}
#size in ft
plotyz_propeller = function(x){
  plot(c(-x$R,x$R),c(-x$R,x$R),type='n',asp=1)
  plotrix::draw.circle(0,0,x$R)
  plotrix::draw.circle(0,0,x$R_u,col='purple')
  plotrix::draw.circle(0,0,x$Width_Thickness_measure_pt,border = 'red')

  plotrix::draw.circle(0,0,x$R_l,col='white')
    plotrix::draw.circle(0,0,x$R_boss)
    abline(v=0)
    abline(h=0)
    abline(v=c(-x$Width,x$Width),col = 'brown')
    abline(h=c(-x$Width,x$Width),col = 'brown')
    abline(v=c(-x$Width,x$Width)/2,col = 'grey')
    abline(h=c(-x$Width,x$Width)/2,col = 'grey')
}

plotxz_propeller = function(x){
  plot(c(-x$R,x$R),c(-x$R,x$R),type='n',asp=1)
  abline(v=0)
  abline(h=0)

  abline(v=x$Thickness_Wood,col = 'brown')

  abline(v=x$Thickness_Metal,col = 'grey')
}

pointsxz_pitch_propeller = function(x,n=10){
  l = seq(from=x$R_boss,to=x$R,length.out=n)

  tan_pitchAngle = x$P / 2/pi/l
  print(tan_pitchAngle)
  print(l)
  for(i in 1:n) abline(coef=c(l[i],pi-tan_pitchAngle[i]),col = 'grey')
  for(i in 1:n) abline(coef=-c(l[i],pi-tan_pitchAngle[i]),col = 'grey')

  x_base = tan_pitchAngle*x$Width
  lines(x_base,l)
  lines(x_base,-l)
}


design_propeller_Russell_plot = function(RPM,MPH,max_J=.5,max_efficiency=.7){
  x= design_propeller_Russell(RPM,MPH,max_J=max_J,max_efficiency = max_efficiency)

  layout(t(1:2))
  plotyz_propeller(x)
  plotxz_propeller(x)
  pointsxz_pitch_propeller(x)
  layout(1)
  x
}

#pls also look at https://m-selig.ae.illinois.edu/props/volume-1/propDB-volume-1.html
# and https://m-selig.ae.illinois.edu/pubs/BrandtSelig-2011-AIAA-2011-1255-LRN-Propellers.pdf
