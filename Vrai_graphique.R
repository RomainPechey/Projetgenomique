
# Représentation grahique polymorphisme Dn/Ds entre E et J
vecteur_g=c(0.000749,0.00154,0.00198,0.00263,0.00316,0.00373,0.00418,0.000798,0.00146,0.00189,0.00263,0.00347,0.00422,0.000658,0.00147,0.00200,0.00241,0.00336,0.00375,0.00438,0.000779,0.00145,0.00195,0.00255,0.00310,0.00384,0.00430,0.000690,0.00131,0.00196,0.00256,0.00315,0.00372,0.00427,0.000716,0.00142,0.00194,0.00259,0.00304,0.00359,0.00391,0.000754,0.00140,0.00184,0.00251,0.00311,0.00357,0.00432,0.000806,0.00141,0.00193,0.00246,0.00315,0.00360,0.00408,0.217,0.180,0.164,0.151,0.140,0.134,0.127,0.217,0.181,0.164,0.150,0.135,0.128,0.219,0.183,0.163,0.149,0.141,0.134,0.128,0.219,0.182,0.163,0.149,0.141,0.134,0.129,0.362,0.349,0.347,0.340,0.335,0.332,0.328,0.360,0.349,0.346,0.340,0.336,0.332,0.324,0.360,0.353,0.347,0.339,0.337,0.333,0.329,0.359,0.351,0.344,0.343,0.336,0.332,0.326,"sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","sans fluctuations","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles","avec fluctuations non proportionelles")
tableaucomp= matrix(vecteur_g,ncol=3,nrow=55)
colnames(tableaucomp)=c("polymorphisme","Dn/Ds","simulation")
g=as.data.frame(tableaucomp)
library(dplyr)
g=g %>% 
  mutate(polymorphisme=as.numeric(polymorphisme),
         `Dn/Ds`=as.numeric(`Dn/Ds`),
         simulation=as.factor(simulation))
library("ggplot2")
ggplot(g, mapping=aes(x=g[,1],y=g[,2]))+ geom_point(aes(color=g[,3]))+ xlab("Polymorphisme")+ ylab("Dn/Ds")+geom_smooth(aes(color=g[,3]),method=lm, se=FALSE)+theme(legend.position = "top")+ theme(plot.title = element_text(size=10,hjust = 0.5))+scale_colour_colorblind(name="Type de simulation")+scale_fill_discrete(breaks=c("plus forte pour les grandes pop","proportionnel","moins forte pour les grandes pop"))+ theme_linedraw()


# Représentation grahique polymorphisme Dn/Ds B
vecteur_g=c(0.000974,0.00181,0.00253,0.00344,0.00417,0.00481,0.00542,0.000936,0.00175,0.00255,0.00335,0.00413,0.00464,0.00546,0.000943,0.00191,0.00241,0.00327,0.00412,0.00494,0.00554,0.000999,0.00180,0.00260,0.00322,0.00429,0.00494,0.00546,0.203,0.176,0.147,0.132,0.129,0.113,0.114,0.178,0.151,0.138,0.139,0.136,0.127,0.121,0.226,0.165,0.150,0.137,0.132,0.125,0.116,0.205,0.180,0.153,0.142,0.118,0.120,0.113)
tableaucomp= matrix(vecteur_g,ncol=2,nrow=28)
colnames(tableaucomp)=c("polymorphisme","Dn/Ds")
g=as.data.frame(tableaucomp)
library("ggplot2")
ggplot(g, mapping=aes(x=g[,1]))+ geom_point(aes(y=g[,2]),color="black")+geom_smooth(aes(y=g[,2]),method=lm, se=FALSE,color="blue")+ xlab("Polymorphisme")+ ylab("Dn/Ds")+ theme(plot.title = element_text(size=10,hjust = 0.5))+ scale_y_continuous(limits = c(0.1,0.25))+ scale_x_continuous(limits = c(0.0006,0.0055))+ theme_linedraw(base_size = 15)


# Représentation grahique polymorphisme Dn/Ds C
vecteur_g=c(0.000819,0.00156,0.00193,0.00271,0.00298,0.00371,0.00411,0.000691,0.00136,0.00185,0.00262,0.00321,0.00343,0.00435,0.000780,0.00141,0.002,0.00253,0.00316,0.00376,0.00424,0.000699,0.00136,0.00198,0.00262,0.00325,0.00361,0.00432,0.214,0.183,0.159,0.150,0.141,0.135,0.131,0.222,0.186,0.161,0.153,0.138,0.134,0.130,0.241,0.171,0.170,0.149,0.139,0.133,0.129,0.227,0.176,0.163,0.154,0.143,0.132,0.130)
tableaucomp= matrix(vecteur_g,ncol=2,nrow=28)
colnames(tableaucomp)=c("polymorphisme","Dn/Ds")
g=as.data.frame(tableaucomp)
library("ggplot2")
ggplot(g, mapping=aes(x=g[,1]))+ geom_point(aes(y=g[,2]),color="black")+geom_smooth(aes(y=g[,2]),method=lm, se=FALSE,color="red")+ xlab("Polymorphisme")+ ylab("Dn/Ds")+ theme(plot.title = element_text(size=10,hjust = 0.5))+ scale_y_continuous(limits = c(0.1,0.25))+ scale_x_continuous(limits = c(0.0006,0.0055))+ theme_linedraw(base_size = 15)

# Représentation grahique polymorphisme Dn/Ds E
vecteur_g=c(0.000749,0.00154,0.00198,0.00263,0.00316,0.00373,0.00418,0.000798,0.00146,0.00189,0.00263,0.00347,0.00422,0.000658,0.00147,0.00200,0.00241,0.00336,0.00375,0.00438,0.000779,0.00145,0.00195,0.00255,0.00310,0.00384,0.00430,0.217,0.180,0.164,0.151,0.140,0.134,0.127,0.217,0.181,0.164,0.150,0.135,0.128,0.219,0.183,0.163,0.149,0.141,0.134,0.128,0.219,0.182,0.163,0.149,0.141,0.134,0.129)
tableaucomp= matrix(vecteur_g,ncol=2,nrow=27)
colnames(tableaucomp)=c("polymorphisme","Dn/Ds")
g=as.data.frame(tableaucomp)
library("ggplot2")
ggplot(g, mapping=aes(x=g[,1]))+ geom_point(aes(y=g[,2]),color="black")+geom_smooth(aes(y=g[,2]),method=lm, se=FALSE,color="green")+ xlab("Polymorphisme")+ ylab("Dn/Ds")+ theme(plot.title = element_text(size=10,hjust = 0.5))+ scale_y_continuous(limits = c(0.1,0.25))+ scale_x_continuous(limits = c(0.0006,0.0055))+ theme_linedraw(base_size = 15)


# Représentation graphique polymorphisme C
vecteur_wpolsyn=c(500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,0.000819,0.00156,0.00193,0.00271,0.00298,0.00371,0.00411,0.000691,0.00136,0.00185,0.00262,0.00321,0.00343,0.00435,0.000780,0.00141,0.002,0.00253,0.00316,0.00376,0.00424,0.000699,0.00136,0.00198,0.00262,0.00325,0.00361,0.00432,0.00034,0.00068,0.001,0.00136,0.0017,0.00204,0.00239,0.00034,0.00068,0.001,0.00136,0.0017,0.00204,0.00239,0.00034,0.00068,0.001,0.00136,0.0017,0.00204,0.00239,0.00034,0.00068,0.001,0.00136,0.0017,0.00204,0.00239)
tableaupolymorphisme_wsyn= matrix(vecteur_wpolsyn,ncol=3,nrow=28)
colnames(tableaupolymorphisme_wsyn)=c("Ne","polymorphisme synonyme Waterson","polymorphisme attendu")
f = as.data.frame(tableaupolymorphisme_wsyn)
typeof(f)
library("ggplot2")
ggplot(f, mapping=aes(x=f[,1]))+ geom_point(aes(y=f[,2]),color="black")+geom_smooth(aes(y=f[,2]),method=lm, se=FALSE,color="red")+ xlab("Taille efficace")+ ylab("Polymorphisme")+ theme(plot.title = element_text(size=10,hjust = 0.5))+ scale_y_continuous(limits = c(0.0005,0.006))


# Représentation graphique Dn/Ds C
vecteur_d=c(500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,500,1000,1500,2000,2500,3000,3500,1279,2557,4036,5252,6534,7882,9338,1256,2574,3909,5214,6566,8034,9345,1249,2676,3809,5095,6641,7837,9293,1250,2594,4038,5242,6549,8052,9268,639,1093,1498,1850,2149,2495,2866,653,1119,1471,1861,2114,2520,2852,704,1072,1511,1778,2136,2438,2813,662,1066,1536,1885,2188,2477,2820,0.214,0.183,0.159,0.150,0.141,0.135,0.131,0.222,0.186,0.161,0.153,0.138,0.134,0.130,0.241,0.171,0.170,0.149,0.139,0.133,0.129,0.227,0.176,0.163,0.154,0.143,0.132,0.130)
tableauDn_Ds= matrix(vecteur_d,ncol=4,nrow=28)
colnames(tableauDn_Ds)=c("Ne","Ds","Dn","Dn/Ds")
t=as.data.frame(tableauDn_Ds)
library("ggplot2")
ggplot(t, mapping=aes(x=t[,1]))+ geom_point(aes(y=t[,4]),color="black")+geom_smooth(aes(y=t[,4]),method=lm, se=FALSE,color="red")+ xlab("Taille efficace")+ ylab("Dn/Ds")+ ggtitle("Evolution du Dn/Ds en fonction de la taille efficace au bout de 10Ne \n (u et r = 10-6 / tailla de pop de l'ordre de 10^3 / alpha =-0,25)")+ theme(plot.title = element_text(size=10,hjust = 0.5))+ scale_y_continuous(limits = c(0.1,0.25))

# Représentation grahique polymorphisme Dn/Ds N1
vecteur_g=c(0.000653,0.00113,0.00251,0.00108,0.00202,0.00175,0.000470,0.000657,0.000597,0.00163,0.00226,0.00196,0.00240,0.00244,0.000737,0.000947,0.00141,0.00223,0.00255,0.000129,0.000375,0.000756,0.00132,0.000343,0.000662,0.00207,0.00275,0.00277,0.000772,0.00126,0.00171,0.00203,0.000628,0.00157,0.00268,0.000848,0.00137,0.00146,0.00197,0.00260,0.00110,0.00264,0.000400,0.000746,0.00157,0.00188,0.00252,0.00269,0.00171,0.000666,0.00119,0.000864,0.00161,0.00225,0.000198,0.00256,0.000698,0.00127,0.00162,0.00223,0.00242,0.00198,0.00253,0.368,0.355,0.351,0.345,0.342,0.335,0.330,0.365,0.355,0.351,0.346,0.339,0.334,0.330,0.365,0.356,0.351,0.344,0.340,0.339,0.332,0.362,0.356,0.345,0.344,0.338,0.335,0.330,0.366,0.357,0.346,0.344,0.342,0.337,0.331,0.366,0.356,0.351,0.344,0.339,0.335,0.333,0.365,0.356,0.350,0.347,0.337,0.334,0.331,0.369,0.356,0.351,0.346,0.339,0.337,0.329,0.365,0.354,0.350,0.346,0.338,0.334,0.332)
tableaucomp= matrix(vecteur_g,ncol=2,nrow=63)
colnames(tableaucomp)=c("polymorphisme","Dn/Ds")
g=as.data.frame(tableaucomp)
library("ggplot2")
ggplot(g, mapping=aes(x=g[,1]))+ geom_point(aes(y=g[,2]),color="black")+geom_smooth(aes(y=g[,2]),method=lm, se=FALSE,color="violet")+ xlab("Polymorphisme")+ ylab("Dn/Ds")+ theme(plot.title = element_text(size=10,hjust = 0.5))+ scale_y_continuous(limits = c(0.3,0.4))+ scale_x_continuous(limits = c(0.0006,0.003))+ theme_linedraw(base_size = 15)


# Représentation grahique polymorphisme Dn/Ds S1
vecteur_g=c(0.000738,0.000704,0.000281,0.00162,0.00193,0.000913,0.00223,0.000596,0.00107,0.00148,0.00176,0.00213,0.00196,0.00155,0.000691,0.00130,0.00123,0.000449,0.00164,0.00119,0.00187,0.000627,0.00116,0.00148,0.000696,0.00189,0.000922,0.00229,0.000678,0.00118,0.00147,0.000786,0.00199,0.00182,0.00214,0.000749,0.00122,0.00150,0.00175,0.00186,0.00182,0.00136,0.000468,0.000880,0.000119,0.00120,0.00185,0.000384,0.00202,0.000242,0.00121,0.00157,0.00149,0.00114,0.00197,0.00161,0.000710,0.00120,0.00167,0.00144,0.00166,0.00121,0.00110,0.365,0.359,0.354,0.352,0.348,0.347,0.348,0.364,0.359,0.356,0.357,0.349,0.349,0.349,0.368,0.361,0.360,0.353,0.352,0.351,0.350,0.370,0.359,0.358,0.358,0.349,0.352,0.348,0.366,0.358,0.356,0.353,0.354,0.353,0.348,0.368,0.362,0.356,0.355,0.353,0.348,0.350,0.367,0.360,0.357,0.351,0.353,0.349,0.349,0.365,0.361,0.360,0.353,0.352,0.350,0.351,0.368,0.364,0.354,0.356,0.349,0.352,0.350)
tableaucomp= matrix(vecteur_g,ncol=2,nrow=63)
colnames(tableaucomp)=c("polymorphisme","Dn/Ds")
g=as.data.frame(tableaucomp)
library("ggplot2")
ggplot(g, mapping=aes(x=g[,1]))+ geom_point(aes(y=g[,2]),color="black")+geom_smooth(aes(y=g[,2]),method=lm, se=FALSE,color="brown")+ xlab("Polymorphisme")+ ylab("Dn/Ds")+ theme(plot.title = element_text(size=10,hjust = 0.5))+ scale_y_continuous(limits = c(0.3,0.4))+ scale_x_continuous(limits = c(0.0006,0.003))+ theme_linedraw(base_size = 15)

# Représentation grahique polymorphisme Dn/Ds U
vecteur_g=c(0.000298,0.00176,0.00217,0.00148,0.00282,0.00248,0.000998,0.000665,0.000671,0.00257,0.000844,0.00222,0.00165,0.00289,0.000907,0.00130,0.00194,0.00153,0.00303,0.00196,0.00319,0.000265,0.00183,0.00192,0.00146,0.00215,0.00156,0.00175,0.00114,0.000710,0.000772,0.00236,0.00138,0.00172,0.00240,0.000196,0.00179,0.00127,0.00104,0.00300,0.00300,0.00106,0.000565,0.001,0.000154,0.00142,0.00273,0.000427,0.00292,0.000303,0.00160,0.00201,0.00192,0.00138,0.00244,0.00235,0.00105,0.00177,0.00241,0.00184,0.00240,0.00149,0.00139,0.337,0.330,0.330,0.320,0.321,0.324,0.324,0.336,0.332,0.325,0.326,0.322,0.323,0.322,0.339,0.330,0.328,0.324,0.321,0.318,0.321,0.335,0.332,0.329,0.324,0.324,0.322,0.321,0.337,0.330,0.328,0.327,0.322,0.321,0.320,0.336,0.333,0.326,0.324,0.322,0.320,0.322,0.339,0.331,0.330,0.324,0.326,0.322,0.319,0.335,0.329,0.324,0.324,0.322,0.321,0.321,0.339,0.332,0.326,0.326,0.321,0.323,0.325)
tableaucomp= matrix(vecteur_g,ncol=2,nrow=63)
colnames(tableaucomp)=c("polymorphisme","Dn/Ds")
g=as.data.frame(tableaucomp)
library("ggplot2")
ggplot(g, mapping=aes(x=g[,1]))+ geom_point(aes(y=g[,2]),color="black")+geom_smooth(aes(y=g[,2]),method=lm, se=FALSE,color="orange")+ xlab("Polymorphisme")+ ylab("Dn/Ds")+ theme(plot.title = element_text(size=10,hjust = 0.5))+ scale_y_continuous(limits = c(0.3,0.4))+ scale_x_continuous(limits = c(0.0006,0.003))+ theme_linedraw(base_size = 15)


# Représentation grahique polymorphisme Dn/Ds entre E et J
vecteur_g <- c(0.000298,0.00176,0.00217,0.00148,0.00282,0.00248,0.000998,0.000665,0.000671,0.00257,0.000844,0.00222,0.00165,0.00289,0.000907,0.00130,0.00194,0.00153,0.00303,0.00196,0.00319,0.000265,0.00183,0.00192,0.00146,0.00215,0.00156,0.00175,0.00114,0.000710,0.000772,0.00236,0.00138,0.00172,0.00240,0.000196,0.00179,0.00127,0.00104,0.00300,0.00300,0.00106,0.000565,0.001,0.000154,0.00142,0.00273,0.000427,0.00292,0.000303,0.00160,0.00201,0.00192,0.00138,0.00244,0.00235,0.00105,0.00177,0.00241,0.00184,0.00240,0.00149,0.00139,0.00110,0.000393,0.00217,0.000945,0.00289,0.00269,0.00140,0.000499,0.00361,0.00276,0.00280,0.000892,0.00316,0.00283,0.000993,0.00189,0.00198,0.00117,0.00208,0.00346,0.00180,0.00114,0.00352,0.00345,0.000481,0.00110,0.00380,0.00186,0.00111,0.00104,0.00257,0.00155,0.00288,0.00240,0.00329,0.00377,0.00329,0.00336,0.00381,0.00345,0.00339,0.000674,0.000525,0.00100,0.000143,0.00147,0.00164,0.000350,0.00289,0.00379,0.00183,0.000974,0.00119,0.00193,0.00166,0.00182,0.000234,0.00152,0.00194,0.00186,0.00133,0.00249,0.00227,0.000352,0.00338,0.00386,0.00387,0.000718,0.00328,0.00282,0.000965,0.00160,0.00250,0.00195,0.00241,0.00157,0.00132,0.000576,0.00379,0.000376,0.00273,0.00179,0.00232,0.00316,0.337,0.330,0.330,0.320,0.321,0.324,0.324,0.336,0.332,0.325,0.326,0.322,0.323,0.322,0.339,0.330,0.328,0.324,0.321,0.318,0.321,0.335,0.332,0.329,0.324,0.324,0.322,0.321,0.337,0.330,0.328,0.327,0.322,0.321,0.320,0.336,0.333,0.326,0.324,0.322,0.320,0.322,0.339,0.331,0.330,0.324,0.326,0.322,0.319,0.335,0.329,0.324,0.324,0.322,0.321,0.321,0.339,0.332,0.326,0.326,0.321,0.323,0.325,0.387,0.365,0.324,0.351,0.334,0.409,0.337,0.358,0.325,0.460,0.358,0.345,0.359,0.325,0.421,0.436,0.361,0.361,0.322,0.330,0.344,0.347,0.336,0.349,0.325,0.449,0.455,0.384,0.441,0.375,0.415,0.325,0.397,0.339,0.422,0.334,0.401,0.324,0.326,0.382,0.380,0.382,0.422,0.447,0.338,0.428,0.330,0.437,0.339,0.433,0.347,0.417,0.327,0.440,0.341,0.427,0.343,0.379,0.410,0.414,0.435,0.397,0.342,0.361,0.444,0.361,0.385,0.333,0.352,0.329,0.341,0.335,0.380,0.352,0.388,0.327,0.446,0.342,0.326,0.365,0.357,0.337,0.341,0.397,"fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","fixe","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire","aleatoire")
tableaucomp= matrix(vecteur_g,ncol=3,nrow=147)
colnames(tableaucomp)=c("polymorphisme","Dn/Ds","simulation")
g=as.data.frame(tableaucomp)
library(dplyr)
g=g %>% 
  mutate(polymorphisme=as.numeric(polymorphisme),
         `Dn/Ds`=as.numeric(`Dn/Ds`),
         simulation=as.factor(simulation))
library("ggplot2")
ggplot(g, mapping=aes(x=g[,1],y=g[,2]))+ geom_point(aes(color=g[,3]))+ xlab("Polymorphisme")+ ylab("Dn/Ds")+geom_smooth(aes(color=g[,3]),method=lm, se=FALSE)+ theme(plot.title = element_text(size=10,hjust =0.5))+theme(legend.position = "bottom")+scale_colour_colorblind(name="Type de simulation")+scale_fill_discrete(breaks=c("plus forte pour les grandes pop","proportionnel","moins forte pour les grandes pop"))+ theme_linedraw(base_size = 15)


vecteur_o=c("A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",0.00110,0.000393,0.00217,0.000945,0.00289,0.00269,0.00140,0.000499,0.00361,0.00276,0.00280,0.000892,0.00316,0.00283,0.000993,0.00189,0.00198,0.00117,0.00208,0.00346,0.00180,0.00114,0.00352,0.00345,0.000481,0.00110,0.00380,0.00186,0.00111,0.00104,0.00257,0.00155,0.00288,0.00240,0.00329,0.00377,0.00329,0.00336,0.00381,0.00345,0.00339,0.000674,0.000525,0.00100,0.000143,0.00147,0.00164,0.000350,0.00289,0.00379,0.00183,0.000974,0.00119,0.00193,0.00166,0.00182,0.000234,0.00152,0.00194,0.00186,0.00133,0.00249,0.00227,0.000352,0.00338,0.00386,0.00387,0.000718,0.00328,0.00282,0.000965,0.00160,0.00250,0.00195,0.00241,0.00157,0.00132,0.000576,0.00379,0.000376,0.00273,0.00179,0.00232,0.00316,0.387,0.365,0.324,0.351,0.334,0.409,0.337,0.358,0.325,0.460,0.358,0.345,0.359,0.325,0.421,0.436,0.361,0.361,0.322,0.330,0.344,0.347,0.336,0.349,0.325,0.449,0.455,0.384,0.441,0.375,0.415,0.325,0.397,0.339,0.422,0.334,0.401,0.324,0.326,0.382,0.380,0.382,0.422,0.447,0.338,0.428,0.330,0.437,0.339,0.433,0.347,0.417,0.327,0.440,0.341,0.427,0.343,0.379,0.410,0.414,0.435,0.397,0.342,0.361,0.444,0.361,0.385,0.333,0.352,0.329,0.341,0.335,0.380,0.352,0.388,0.327,0.446,0.342,0.326,0.365,0.357,0.337,0.341,0.397,"Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation","Simulation")
tableaucomp= matrix(vecteur_o,ncol=4,nrow=84)
colnames(tableaucomp)=c("Espèces","Polymorphisme","dN.dS","Clade")
g2=as.data.frame(tableaucomp)
Top = rbind(Tableau_final,g2)
library(dplyr)
Top=Top %>% 
  mutate(Polymorphisme=as.numeric(Polymorphisme),
         dN.dS=as.numeric(dN.dS),
         Clade =as.factor(Clade))
library("ggplot2")
ggplot(Top, mapping=aes(x=Top[,2],y=Top[,3]))+ geom_point(aes(color=Top[,4]))+ xlab("Polymorphisme")+ ylab("Dn/Ds")+geom_smooth(aes(color=Top[,4]),method=lm, se=FALSE)+theme(legend.position = "bottom")+ scale_colour_colorblind(name="Clade")
+ theme(plot.title = element_text(size=10,hjust = 0.5))+ theme_linedraw(base_size = 15)

+scale_colour_colorblind(name="Clade")


vecteur_g=c(0.00110,0.000393,0.00217,0.000945,0.00289,0.00269,0.00140,0.000499,0.00361,0.00276,0.00280,0.000892,0.00316,0.00283,0.000993,0.00189,0.00198,0.00117,0.00208,0.00346,0.00180,0.00114,0.00352,0.00345,0.000481,0.00110,0.00380,0.00186,0.00111,0.00104,0.00257,0.00155,0.00288,0.00240,0.00329,0.00377,0.00329,0.00336,0.00381,0.00345,0.00339,0.000674,0.000525,0.00100,0.000143,0.00147,0.00164,0.000350,0.00289,0.00379,0.00183,0.000974,0.00119,0.00193,0.00166,0.00182,0.000234,0.00152,0.00194,0.00186,0.00133,0.00249,0.00227,0.000352,0.00338,0.00386,0.00387,0.000718,0.00328,0.00282,0.000965,0.00160,0.00250,0.00195,0.00241,0.00157,0.00132,0.000576,0.00379,0.000376,0.00273,0.00179,0.00232,0.00316,0.387,0.365,0.324,0.351,0.334,0.409,0.337,0.358,0.325,0.460,0.358,0.345,0.359,0.325,0.421,0.436,0.361,0.361,0.322,0.330,0.344,0.347,0.336,0.349,0.325,0.449,0.455,0.384,0.441,0.375,0.415,0.325,0.397,0.339,0.422,0.334,0.401,0.324,0.326,0.382,0.380,0.382,0.422,0.447,0.338,0.428,0.330,0.437,0.339,0.433,0.347,0.417,0.327,0.440,0.341,0.427,0.343,0.379,0.410,0.414,0.435,0.397,0.342,0.361,0.444,0.361,0.385,0.333,0.352,0.329,0.341,0.335,0.380,0.352,0.388,0.327,0.446,0.342,0.326,0.365,0.357,0.337,0.341,0.397,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000)
tableaucomp= matrix(vecteur_g,ncol=3,nrow=84)
colnames(tableaucomp)=c("polymorphisme","Dn/Ds","Taille population")
g=as.data.frame(tableaucomp)
library("ggplot2")
ggplot(g, mapping=aes(x=g[,1],y=g[,2],color=g[,3])) +geom_point(cex=3)+scale_color_gradient(low="green",high="red")+geom_smooth(method = lm, se=FALSE,color="black")+ xlab("Polymorphisme")+ ylab("Dn/Ds")+ theme(plot.title = element_text(size=8,hjust = 0.5))+ theme_linedraw(base_size = 15)+theme(legend.position = "bottom")+labs(color="Taille minimale     ")



vecteur_g=c(0.00110,0.000393,0.00217,0.000945,0.00289,0.00269,0.00140,0.000499,0.00361,0.00276,0.00280,0.000892,0.00316,0.00283,0.000993,0.00189,0.00198,0.00117,0.00208,0.00346,0.00180,0.00114,0.00352,0.00345,0.000481,0.00110,0.00380,0.00186,0.00111,0.00104,0.00257,0.00155,0.00288,0.00240,0.00329,0.00377,0.00329,0.00336,0.00381,0.00345,0.00339,0.000674,0.000525,0.00100,0.000143,0.00147,0.00164,0.000350,0.00289,0.00379,0.00183,0.000974,0.00119,0.00193,0.00166,0.00182,0.000234,0.00152,0.00194,0.00186,0.00133,0.00249,0.00227,0.000352,0.00338,0.00386,0.00387,0.000718,0.00328,0.00282,0.000965,0.00160,0.00250,0.00195,0.00241,0.00157,0.00132,0.000576,0.00379,0.000376,0.00273,0.00179,0.00232,0.00316,0.387,0.365,0.324,0.351,0.334,0.409,0.337,0.358,0.325,0.460,0.358,0.345,0.359,0.325,0.421,0.436,0.361,0.361,0.322,0.330,0.344,0.347,0.336,0.349,0.325,0.449,0.455,0.384,0.441,0.375,0.415,0.325,0.397,0.339,0.422,0.334,0.401,0.324,0.326,0.382,0.380,0.382,0.422,0.447,0.338,0.428,0.330,0.437,0.339,0.433,0.347,0.417,0.327,0.440,0.341,0.427,0.343,0.379,0.410,0.414,0.435,0.397,0.342,0.361,0.444,0.361,0.385,0.333,0.352,0.329,0.341,0.335,0.380,0.352,0.388,0.327,0.446,0.342,0.326,0.365,0.357,0.337,0.341,0.397,28,34,50,36,42,19,38,31,47,10,31,36,30,49,21,17,35,32,50,46,36,36,39,34,49,11,10,22,17,30,19,50,21,39,16,42,19,48,46,37,23,23,20,15,44,16,46,14,40,14,35,16,45,12,38,14,47,29,20,18,15,22,38,29,12,30,22,42,34,46,45,47,28,36,23,46,12,36,46,28,30,35,38,19)
tableaucomp= matrix(vecteur_g,ncol=3,nrow=84)
colnames(tableaucomp)=c("polymorphisme","Dn/Ds","Taille population")
g=as.data.frame(tableaucomp)
library("ggplot2")
s= ggplot(g, mapping=aes(x=g[,1],y=g[,2],color=g[,3])) +geom_point(cex=3)+scale_color_gradient(low="green",high="red")+geom_smooth(method = lm, se=FALSE,color="black")+ xlab("Polymorphisme")+ ylab("Dn/Ds")
s = s+theme(plot.title = element_text(size=10,hjust = 0.5))+ theme_linedraw(base_size = 15)
s




# polymorphisme Dn/Ds
vecteur_o=c(0.000566,0.000910,0.000143,0.00127,0.00214,0.000398,0.00209,0.00251,0.00151,0.000891,0.00117,0.00158,0.00137,0.00151,0.000262,0.00135,0.00168,0.00160,0.00123,0.00188,0.00180,0.000352,0.00228,0.00245,0.00251,0.000726,0.00209,0.00186,0.000961,0.00152,0.00205,0.00175,0.00204,0.00135,0.00125,0.000565,0.00235,0.000447,0.00200,0.00117,0.00174,0.00217,0.434,0.462,0.360,0.463,0.365,0.482,0.395,0.497,0.423,0.500,0.418,0.542,0.452,0.548,0.349,0.394,0.439,0.447,0.468,0.439,0.398,0.430,0.521,0.444,0.477,0.434,0.463,0.452,0.355,0.349,0.398,0.382,0.427,0.376,0.501,0.414,0.402,0.449,0.454,0.423,0.454,0.521)
tableaucomp= matrix(vecteur_o,ncol=2,nrow=42)
colnames(tableaucomp)=c("polymorphisme","Dn/Ds")
g2=as.data.frame(tableaucomp)
library("ggplot2")
par(mfrow=c(2,2))
ggplot(g2, mapping=aes(x=g2[,1], y=g2[,2]))+ geom_point()+ geom_smooth(method=lm, se=FALSE)
# Fidélité corrélation
mod3 <- lm( g2[,2]~ g2[,1], data = g2 )
summary(mod3)
cor(g2[,2],g2[,1], method="spearman")
ggplot(g2, mapping=aes(x=g2[,1]))+ geom_point(aes(y=g2[,2]),color="black")+geom_smooth(aes(y=g2[,2]),method=lm, se=FALSE,color="blue")+ xlab("Polymorphisme")+ ylab("Dn/Ds")+ theme(plot.title = element_text(size=10,hjust = 0.5))+ theme_linedraw(base_size = 15)