# Corrélation Ne et nombre de sites polymorphe
vecteur_pol=c(500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,1205,2118,2833,3484,4363,5052,5557,1065,2050,2761,3666,4367,4814,5653,1091,1988,2643,3595,4240,4613,5722,1283,2300,2712,3685,4027,4939,5457)
tableaupolymorphisme= matrix(vecteur_pol,ncol=2,nrow=28)
colnames(tableaupolymorphisme)=c("Ne","polymorphisme")
x = as.data.frame(tableaupolymorphisme)
typeof(x)
library("ggplot2")
ggplot(x, mapping=aes(x=x[,1], y=x[,2]))+ geom_point()+ geom_smooth(method=lm, se=FALSE)

# Corrélation Ne et polymorphisme de Waterson
vecteur_wpol=c(500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,0.000438,0.000785,0.000926,0.00126,0.00137,0.00168,0.00186,0.000372,0.000678,0.000920,0.00123,0.00148,0.00157,0.00195,0.000441,0.000723,0.000967,0.00119,0.00149,0.00172,0.00189,0.000363,0.000699,0.000942,0.00125,0.00149,0.00164,0.00193,0.00034,0.00068,0.001,0.00136,0.0017,0.00204,0.00239,0.00034,0.00068,0.001,0.00136,0.0017,0.00204,0.00239,0.00034,0.00068,0.001,0.00136,0.0017,0.00204,0.00239,0.00034,0.00068,0.001,0.00136,0.0017,0.00204,0.00239)
tableaupolymorphisme_w= matrix(vecteur_wpol,ncol=3,nrow=28)
colnames(tableaupolymorphisme_w)=c("Ne","polymorphisme Waterson","polymorphisme attendu")
y = as.data.frame(tableaupolymorphisme_w)
typeof(y)
library("ggplot2")
ggplot(y, mapping=aes(x=y[,1], y=y[,2]))+ geom_point()+ geom_smooth(method=lm, se=FALSE)+ geom_point(aes(y = y[,3]), color="red")
# Fidélité corrélation
mod1 <- lm( y[,2]~ y[,1], data = y )
summary(mod1)
cor(y[,2],y[,1], method="pearson")

#Corrélation Ne et polymorphisme synonyme corrigé par waterson 
vecteur_wpolsyn=c(500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,0.000819,0.00156,0.00193,0.00271,0.00298,0.00371,0.00411,0.000691,0.00136,0.00185,0.00262,0.00321,0.00343,0.00435,0.000780,0.00141,0.002,0.00253,0.00316,0.00376,0.00424,0.000699,0.00136,0.00198,0.00262,0.00325,0.00361,0.00432,0.00034,0.00068,0.001,0.00136,0.0017,0.00204,0.00239,0.00034,0.00068,0.001,0.00136,0.0017,0.00204,0.00239,0.00034,0.00068,0.001,0.00136,0.0017,0.00204,0.00239,0.00034,0.00068,0.001,0.00136,0.0017,0.00204,0.00239)
tableaupolymorphisme_wsyn= matrix(vecteur_wpolsyn,ncol=3,nrow=28)
colnames(tableaupolymorphisme_wsyn)=c("Ne","polymorphisme synonyme Waterson","polymorphisme attendu")
f = as.data.frame(tableaupolymorphisme_wsyn)
typeof(f)
library("ggplot2")
ggplot(f, mapping=aes(x=f[,1], y=f[,2]))+ geom_point()+ geom_smooth(method=lm, se=FALSE)+ geom_point(aes(y = f[,3]), color="red")
# Fidélité corrélation
mod1 <- lm( f[,2]~ f[,1], data = f )
summary(mod1)
cor(f[,2],f[,1], method="pearson")

# Corrélation Ne et Dn/Ds
vecteur_d=c(500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,1279,2557,4036,5252,6534,7882,9338,1256,2574,3909,5214,6566,8034,9345,1249,2676,3809,5095,6641,7837,9293,1250,2594,4038,5242,6549,8052,9268,639,1093,1498,1850,2149,2495,2866,653,1119,1471,1861,2114,2520,2852,704,1072,1511,1778,2136,2438,2813,662,1066,1536,1885,2188,2477,2820,0.214,0.183,0.159,0.150,0.141,0.135,0.131,0.222,0.186,0.161,0.153,0.138,0.134,0.130,0.241,0.171,0.170,0.149,0.139,0.133,0.129,0.227,0.176,0.163,0.154,0.143,0.132,0.130)
tableauDn_Ds= matrix(vecteur_d,ncol=4,nrow=28)
colnames(tableauDn_Ds)=c("Ne","Ds","Dn","Dn/Ds")
t=as.data.frame(tableauDn_Ds)
library("ggplot2")
par(mfrow=c(2,2))
ggplot(t, mapping=aes(x=t[,1], y=t[,4]))+ geom_point()+ geom_smooth(method=lm, se=FALSE)
# Fidélité corrélation
mod2 <- lm( t[,4]~ t[,1], data = t )
summary(mod2)
cor(t[,4],t[,1], method="pearson")

# polymorphisme Dn/Ds
vecteur_g=c(0.000819,0.00156,0.00193,0.00271,0.00298,0.00371,0.00411,0.000691,0.00136,0.00185,0.00262,0.00321,0.00343,0.00435,0.000780,0.00141,0.002,0.00253,0.00316,0.00376,0.00424,0.000699,0.00136,0.00198,0.00262,0.00325,0.00361,0.00432,0.214,0.183,0.159,0.150,0.141,0.135,0.131,0.222,0.186,0.161,0.153,0.138,0.134,0.130,0.241,0.171,0.170,0.149,0.139,0.133,0.129,0.227,0.176,0.163,0.154,0.143,0.132,0.130)
tableaucomp= matrix(vecteur_g,ncol=2,nrow=28)
colnames(tableaucomp)=c("polymorphisme","Dn/Ds")
g=as.data.frame(tableaucomp)
library("ggplot2")
par(mfrow=c(2,2))
ggplot(g, mapping=aes(x=g[,1], y=g[,2]))+ geom_point()+ geom_smooth(method=lm, se=FALSE)
# Fidélité corrélation
mod2 <- lm( g[,2]~ g[,1], data = g )
summary(mod2)
cor(g[,2],g[,1], method="spearman")


library(dplyr)
DnDs3500C4 %>%
  select(V3) %>%
  filter(V3=="m1")  

DnDs3500C4 %>%
  select(V3) %>%
  filter(V3!="m")


library(dplyr)
polymorphisme3500C4 %>%
  select(V3) %>%
  filter(V3=="m1")  

polymorphisme2000C %>%
  select(V3) %>%
  filter(V3!="m1")
