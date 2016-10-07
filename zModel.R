rk4Zombies <- function(f,z0) {
  a=0.0 # Início do domínio computacional
  b=365  # Fim do domínio computacional
  N=730 # Número de subintervalos de tempo
  
  vt<-double(N+1)
  vz<-double(N+1)
  
  vt[1]<-t<-a
  vz[1]<-z<-z0
  h<-(b-a)/N
  
  for(i in 1:N) {
    k1<-h*f(t, z)
    k2<-h*f(t+0.5*h, z+0.5*k1)
    k3<-h*f(t+0.5*h, z+0.5*k2)
    k4<-h*f(t+h, z+k3)
    vt[i+1]<-t<-a+i*h
    vz[i+1]<-z<-z+(k1+2*k2+2*k3+k4)/6 }
  
  cbind(vt,vz) }
#=============================================
zMod1 <- function(t,z){
  #Parâmetros do modelo
  p <- c(0.7671, # taxa de transformação
         27,     # capacidade suporte inicial
         6.75,   # variação da capacidade suporte
         0.4     # probabilidade de humanos vencer o encontro
  )
  #Imprime resultado
  return(
    as.numeric(
      p[1]*( z/( 1+ z/ ( p[2]+p[3]*sin(t) ) ) ) 
      - (1-p[4])*z
    )
  )
}
#=============================================
zMod2 <- function(t,z){
  #Parâmetros do modelo
  p <- c(0.7671,# taxa de transformação
         27,    # capacidade suporte inicial
         6.75,  # variação da capacidade suporte
         0.4,   # probabilidade de humanos vencer o encontro
         0.3671 # taxa de controle
  )
  #Imprime resultado
  return(
    as.numeric(
      p[1]*( z/( 1+ z/ ( p[2]+p[3]*sin(t) ) ) ) 
      - (1-p[4])*z
      - p[5]*(1-cos(t))
    )
  )
}
#=============================================
zMod3 <- function(t,z){
  #Parâmetros do modelo
  p <- c(0.7671,# taxa de transformação
         27,    # capacidade suporte inicial
         6.75,  # variação da capacidade suporte
         0.4,   # probabilidade de humanos vencer o encontro
         0.3671 # taxa de controle
  )
  #Controle
  if (t < 5) {
    nope <- 0
  }
  else if (t >= 5) {
    nope <- (1-cos(t)) 
  }
  #Imprime resultado
  return(
    as.numeric(
      p[1]*( z/( 1+ z/ ( p[2]+p[3]*sin(t) ) ) ) 
      - (1-p[4])*z
      - p[5]*(1-cos(t))
    )
  )
}
#=============================================
#Condições iniciais
z0=10

# subsetting zombies
simulZombies<-data.frame(rk4Zombies(zMod1,z0),
rk4Zombies(zMod2,z0), rk4Zombies(zMod3,z0))
dataZombies<-simulZombies[c("vt","vz","vz.1","vz.2")]
attach(dataZombies)
subsetZombies <- subset(dataZombies, subset = vt <= 60)

# dados do subsetZombies
summary(subsetZombies)

#=============================================
# Plot
matplot(subsetZombies[,1],subsetZombies[,2:4], type="o",
xlab="Tempo [dias]", ylab="População de zumbis",
xlim=c(0,15), ylim=c(0,15), lty=1:3, pch=1:3, col=1:3)
legend('topright', legend=c("Modelo 1","Modelo 2","Modelo 3"),
lty=1:3, pch=1:3, col=1:3, bty='n')
detach(dataZombies)