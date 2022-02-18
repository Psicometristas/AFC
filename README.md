# Función ipak: instala y carga múltiples paquetes de R#
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# indicar los paquetes que va instaklar y/o llamar la función ipak#
packages <- c("lavaan", "semPlot")
ipak(packages)


#Modelo 1 (3 factores correlacionados)#
Modelo_1<-'
REl_Posi =~ RP_1	+ RP_2 + RP_3	+ RP_4
Compromiso  =~ C_1	+ C_2 + C_3	+ C_4
Emo_posit =~ EP_1	+ EP_2 + EP_3	+ EP_4
'

#Modelo 2 (estructura Unifactorial)#
Modelo_2<-'
Bienestar =~ RP_1	+ RP_2 + RP_3	+ RP_4
            + C_1	+ C_2 + C_3	+ C_4
            + EP_1	+ EP_2 + EP_3	+ EP_4
'
#Modelo 2 (estructura con 3 factores de primer orden y 1 factor de segundo orden)#
Modelo_3<-'
REL_Posi =~ RP_1	+ RP_2 + RP_3	+ RP_4
Compromiso  =~ C_1	+ C_2 + C_3	+ C_4
Emo_posit =~ EP_1	+ EP_2 + EP_3	+ EP_4
FSO =~ REl_Posi + Compromiso + Emo_posit
'

#Análsis de una estructura con 6 factores#
AFC_Modelo_1 <- cfa(Modelo_1,orthogonal=F, data=Base_AFC, estimator="ULS", ordered =names(Base_AFC))
summary(AFC_Modelo_1, fit.measures=TRUE, standardized=T, rsquare=T)
fitMeasures(AFC_Modelo_1, fit.measures = c("chisq", "df","srmr", "rmsea", "tli", "cfi"))
semPaths(AFC_Modelo_1, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.3, optimizeLatRes = T, groups = "lat",pastel = T, sizeInt=5,edge.color ="black",esize = 5, label.prop=0,sizeLat = 11,"std",layout="circle3", exoVar = F)
fitMeasures(AFC_Modelo_1)

#Educación comunitaria#
AFC_Modelo_2 <- cfa(Modelo_2,orthogonal=F, data=Base_AFC, estimator="uls", ordered =names(Base_AFC))
summary(AFC_Modelo_1, fit.measures=TRUE, standardized=T, rsquare=T)
fitMeasures(AFC_Modelo_2, fit.measures = c("chisq", "df","srmr", "rmsea", "tli", "cfi"))
semPaths(AFC_Modelo_2, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.3, optimizeLatRes = T, groups = "lat",pastel = T, sizeInt=5,edge.color ="blue",esize = 5, label.prop=0,sizeLat = 11,"std",layout="circle3", exoVar = F)

#Análsis de una estructura con 6 factores de primer orden y 1 factor de segundo orden#
AFC_Modelo_3 <- cfa(Modelo_3,orthogonal=T, data=Base_AFC, estimator="uls", ordered =names(Base_AFC))
summary(AFC_Modelo_3, fit.measures=TRUE, standardized=T, rsquare=T)
fitMeasures(AFC_Modelo_3, fit.measures = c("chisq", "df","srmr", "rmsea", "tli", "cfi"))
semPaths(AFC_Modelo_3, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.3, optimizeLatRes = T, groups = "lat",pastel = T, sizeInt=5,edge.color ="blue",esize = 5, label.prop=0,sizeLat = 11,"std",layout="tree", exoVar = F)

#Resumen de los índices de ajuste de los 3 modelos competidores#
resumen_fit <- rbind(fitmeasures(AFC_Modelo_1,fit.measures = c("chisq", "df","srmr", "tli", "cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper")),
                     fitmeasures(AFC_Modelo_2, fit.measures = c("chisq", "df","srmr", "tli", "cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper")),
                     fitmeasures(AFC_Modelo_3, fit.measures = c("chisq", "df","srmr", "tli", "cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper")))
rownames(resumen_fit) <- c("6 factores oblicuos", "Unifactorial","2do Orden")
round(resumen_fit, digits = 3)
