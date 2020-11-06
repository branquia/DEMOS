#Directorio de trabajo
setwd("C:/Users/usuario/Desktop/JAVIER/0.0.DATOS_ESTRUCTURA_CARPETAS_SGC/1.Estructura_Organizativa_Raiz/PROYECTOS/00_DEMOS_2020")

#CARGAR LIBRERIAS
library(readxl)
library(xlsx)
library(openxlsx)
library(tidyverse)
library(corrplot)
library(pca3d)
library(factoextra)
library("FactoMineR")
library(NbClust)
library(cluster)
library("clValid")

#Crear funciones
std=function(x){
  return(sqrt(var(x)/length(x)))
}

rango=function(x){
  return(max(x)-min(x))
}

CV=function(x){
  return(sqrt(var(x))/mean(x))
}
#

PobTot_Envej_UEA <- read_excel("01_Datos/DATOS FM/PobTot_Envej_UEA_N_prom.xlsx", 
                               sheet = "Prom_1996_2019")
datos=data.frame(PobTot_Envej_UEA)
datos=mutate(datos,Tamaño=ifelse(N_prom<14000,"Pequeño","Mediano"))
datos=mutate(datos,Tamaño=ifelse(N_prom>40000,"Grande",Tamaño))
datos=mutate(datos,Tamaño=ifelse(N_prom>100000,"Muy Grande",Tamaño))
datos=mutate(datos,Tamaño=as.factor(Tamaño))
datos$Tamaño=fct_relevel(datos$Tamaño,c("Pequeño","Mediano","Grande","Muy Grande"))
table(datos$Tamaño)


datos_18=select(datos[-692],UEA,PM,PMJ,PMM,PMA,PHM,PHMJ,PHMM,PHMA,PMuM,PMuMJ,PMuMM,PMuMA,PMAM,PHMAM,PMuMAM,N_prom,NH_prom,NMu_prom)
datos_15=select(datos[-692],UEA,PM,PMJ,PMM,PMA,PHM,PHMJ,PHMM,PHMA,PMuM,PMuMJ,PMuMM,PMuMA,PMAM,PHMAM,PMuMAM)
datos_12=select(datos[-692],UEA,PM,PMJ,PMM,PMA,PHM,PHMJ,PHMM,PHMA,PMuM,PMuMJ,PMuMM,PMuMA)

datos_18_norm=scale(datos_18[,-1])
datos_15_norm=scale(datos_15[,-1])
datos_12_norm=scale(datos_12[,-1])

############18#########
correlacion_pe=cor(datos_18_norm)
correlacion_spe=cor(datos_18_norm,method = "spearman")
corrplot(correlacion_pe,method = "number")
corrplot(correlacion_spe,method = "number")

#PCA 18
pca_datos_18=PCA(datos_18,quali.sup = 1,graph = F,ncp = 3,scale.unit = T)
get_eig(pca_datos_18)
fviz_screeplot(pca_datos_18,addlabels=T)+
  xlab("Número de componentes principales")+
  ylab("Proporción de varianza explicada")

get_eigenvalue(pca_datos_18)
pca_datos_18$var
# Extract the results for variables

fviz_pca_var(pca_datos_18, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
             )
fviz_pca_var(pca_datos_18, col.var="contrib",
             axes = c(1,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping,
             )
             
# Contributions of variables to PC1
fviz_contrib(pca_datos_18, choice = "var", axes = 1, top = 18)
# Contributions of variables to PC2
fviz_contrib(pca_datos_18, choice = "var", axes = 2, top = 18)
# Contributions of variables to PC3
fviz_contrib(pca_datos_18, choice = "var", axes = 3, top = 18)

# Extract the results for individuals
ind <- get_pca_ind(pca_datos_18)
head(ind$coord)

#fviz_pca_ind(pca_datos_18, col.ind = "cos2",
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#             repel = TRUE # Avoid text overlapping (slow if many points))


##

fviz_pca_biplot(pca_datos_18, repel = F,axes = c(1,2))
fviz_pca_biplot(pca_datos_18, repel = F,axes = c(1,3))

datos_acp_18=ind$coord
#Cluster
# #mirar que método cluster es mejor
# intern <- clValid(datos_acp_18, nClust = 2:6, 
#                   clMethods = c("hierarchical","kmeans","pam",'clara'),
#                   validation = "internal")
# plot(intern)
# summary(intern)
# #hierarchical  es el mejor
# numero_cluster=NbClust(datos_acp_18,method = "average",index = "all")
# numero_cluster$Best.partition

den_datos_18 <- hcut(datos_acp_18, k = 5, stand = TRUE,hc_func = "diana",hc_metric = "euclidean",hc_method = "average")
den_datos_18$cluster
table(den_datos_18$cluster)
table(den_datos_18$cluster,datos$Tamaño)
# Visualize
windows(width = 27,height = 27)
fviz_dend(den_datos_18, rect = TRUE, cex = 0.5,
          k_colors = c(1,2,3,4,5))

datos_acp_18_cluster=data.frame(UEA=datos_18[,1],datos_acp_18,cluster=den_datos_18$cluster)
head(datos_acp_18_cluster)
fviz_pca_biplot(pca_datos_18,
                repel = F,
                axes = c(1,2),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =as.factor(datos_acp_18_cluster$cluster),
                palette = "jco",
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T)

fviz_pca_biplot(pca_datos_18,
                repel = F,
                axes = c(1,3),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =as.factor(datos_acp_18_cluster$cluster),
                palette = "jco",
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T)

fviz_pca_biplot(pca_datos_18,
                repel = F,
                axes = c(1,2),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =datos$Tamaño,
                palette = "jco",
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T)

datos_18_cluster=data.frame(datos_18,cluster=as.factor(den_datos_18$cluster))

head(datos_18_cluster)
d_18=datos_18_cluster %>% group_by(cluster) %>% summarise(Num_UEA=n(),
                                                          PM_med=mean(PM)
                                                         ,PMJ_med=mean(PMJ)
                                                         ,PMM_med=mean(PMM)
                                                         ,PMA_med=mean(PMA)
                                                         ,PHM_med=mean(PHM)
                                                         ,PHMJ_med=mean(PHMJ)
                                                         ,PHMM_med=mean(PHMM)
                                                         ,PHMA_med=mean(PHMA)
                                                         ,PMuM_med=mean(PMuM)
                                                         ,PMuMJ_med=mean(PMuMJ)
                                                         ,PMuMM_med=mean(PMuMM)
                                                         ,PMuMA_med=mean(PMuMA)
                                                         ,PMAM_med=mean(PMAM)
                                                         ,PHMAM_med=mean(PHMAM)
                                                         ,PMuMAM_med=mean(PMuMAM)
                                                         ,N_prom_med=mean(N_prom)
                                                         ,NH_prom_med=mean(NH_prom)
                                                         ,NMu_prom_med=mean(NMu_prom),
                                                         #
                                                         PM_rango=rango(PM)
                                                         ,PMJ_rango=rango(PMJ)
                                                         ,PMM_rango=rango(PMM)
                                                         ,PMA_rango=rango(PMA)
                                                         ,PHM_rango=rango(PHM)
                                                         ,PHMJ_rango=rango(PHMJ)
                                                         ,PHMM_rango=rango(PHMM)
                                                         ,PHMA_rango=rango(PHMA)
                                                         ,PMuM_rango=rango(PMuM)
                                                         ,PMuMJ_rango=rango(PMuMJ)
                                                         ,PMuMM_rango=rango(PMuMM)
                                                         ,PMuMA_rango=rango(PMuMA)
                                                         ,PMAM_rango=rango(PMAM)
                                                         ,PHMAM_rango=rango(PHMAM)
                                                         ,PMuMAM_rango=rango(PMuMAM)
                                                         ,N_prom_rango=rango(N_prom)
                                                         ,NH_prom_rango=rango(NH_prom)
                                                         ,NMu_prom_rango=rango(NMu_prom),
                                                         #
                                                         PM_std=std(PM)
                                                         ,PMJ_std=std(PMJ)
                                                         ,PMM_std=std(PMM)
                                                         ,PMA_std=std(PMA)
                                                         ,PHM_std=std(PHM)
                                                         ,PHMJ_std=std(PHMJ)
                                                         ,PHMM_std=std(PHMM)
                                                         ,PHMA_std=std(PHMA)
                                                         ,PMuM_std=std(PMuM)
                                                         ,PMuMJ_std=std(PMuMJ)
                                                         ,PMuMM_std=std(PMuMM)
                                                         ,PMuMA_std=std(PMuMA)
                                                         ,PMAM_std=std(PMAM)
                                                         ,PHMAM_std=std(PHMAM)
                                                         ,PMuMAM_std=std(PMuMAM)
                                                         ,N_prom_std=std(N_prom)
                                                         ,NH_prom_std=std(NH_prom)
                                                         ,NMu_prom_std=std(NMu_prom),
                                                         #
                                                         PM_CV=CV(PM)
                                                         ,PMJ_CV=CV(PMJ)
                                                         ,PMM_CV=CV(PMM)
                                                         ,PMA_CV=CV(PMA)
                                                         ,PHM_CV=CV(PHM)
                                                         ,PHMJ_CV=CV(PHMJ)
                                                         ,PHMM_CV=CV(PHMM)
                                                         ,PHMA_CV=CV(PHMA)
                                                         ,PMuM_CV=CV(PMuM)
                                                         ,PMuMJ_CV=CV(PMuMJ)
                                                         ,PMuMM_CV=CV(PMuMM)
                                                         ,PMuMA_CV=CV(PMuMA)
                                                         ,PMAM_CV=CV(PMAM)
                                                         ,PHMAM_CV=CV(PHMAM)
                                                         ,PMuMAM_CV=CV(PMuMAM)
                                                         ,N_prom_CV=CV(N_prom)
                                                         ,NH_prom_CV=CV(NH_prom)
                                                         ,NMu_prom_CV=CV(NMu_prom)
                                                         )

d_18=t(d_18)
colnames(d_18)=c("Grupo 1","Grupo 2","Grupo 3","Grupo 4","Grupo 5")
head(d_18)
write.xlsx2(d_18,file="Resumen.xlsx",sheetName = "Matriz A",append = F)
########15############
correlacion_pe=cor(datos_15_norm)
correlacion_spe=cor(datos_15_norm,method = "spearman")
corrplot(correlacion_pe,method = "number")
corrplot(correlacion_spe,method = "number")

#PCA 15
pca_datos_15=PCA(datos_15,quali.sup = 1,graph = F,ncp = 2,scale.unit = T)
get_eig(pca_datos_15)
fviz_screeplot(pca_datos_15,addlabels=T)+
  xlab("Número de componentes principales")+
  ylab("Proporción de varianza explicada")

# Extract the results for variables
var <- get_pca_var(pca_datos_15)
var
head(var$coord)


fviz_pca_var(pca_datos_15, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)


# Contributions of variables to PC1
fviz_contrib(pca_datos_15, choice = "var", axes = 1, top = 18)
# Contributions of variables to PC2
fviz_contrib(pca_datos_15, choice = "var", axes = 2, top = 18)


# Extract the results for individuals
ind <- get_pca_ind(pca_datos_15)
datos_acp_15=ind$coord

fviz_pca_biplot(pca_datos_15, repel = F,axes = c(1,2))

##Cluster
den_datos_15 <- hcut(datos_acp_15, k = 4, stand = TRUE,hc_func = "diana",hc_metric = "euclidean",hc_method = "average")
den_datos_15$cluster
table(den_datos_15$cluster)
table(den_datos_15$cluster,datos$Tamaño)
# Visualize
windows(width = 27,height = 27)
fviz_dend(den_datos_15, rect = TRUE, cex = 0.5,
          k_colors = c(1,2,3,4))

datos_acp_15_cluster=data.frame(UEA=datos_15[,1],datos_acp_15,cluster=den_datos_15$cluster)
head(datos_acp_15_cluster)
fviz_pca_biplot(pca_datos_15,
                repel = F,
                axes = c(1,2),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =as.factor(datos_acp_15_cluster$cluster),
                palette = "jco",
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T)

fviz_pca_biplot(pca_datos_15,
                repel = F,
                axes = c(1,2),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =datos$Tamaño,
                palette = "jco",
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T)


datos_15_cluster=data.frame(datos_15,cluster=as.factor(den_datos_15$cluster))

head(datos_15_cluster)
d_15=datos_15_cluster %>% group_by(cluster) %>% summarise(Num_UEA=n(),
                                                          PM_med=mean(PM)
                                                          ,PMJ_med=mean(PMJ)
                                                          ,PMM_med=mean(PMM)
                                                          ,PMA_med=mean(PMA)
                                                          ,PHM_med=mean(PHM)
                                                          ,PHMJ_med=mean(PHMJ)
                                                          ,PHMM_med=mean(PHMM)
                                                          ,PHMA_med=mean(PHMA)
                                                          ,PMuM_med=mean(PMuM)
                                                          ,PMuMJ_med=mean(PMuMJ)
                                                          ,PMuMM_med=mean(PMuMM)
                                                          ,PMuMA_med=mean(PMuMA)
                                                          ,PMAM_med=mean(PMAM)
                                                          ,PHMAM_med=mean(PHMAM)
                                                          ,PMuMAM_med=mean(PMuMAM)
                                                          ,N_prom_med=mean(N_prom)
                                                          ,NH_prom_med=mean(NH_prom)
                                                          ,NMu_prom_med=mean(NMu_prom),
                                                          #
                                                          PM_rango=rango(PM)
                                                          ,PMJ_rango=rango(PMJ)
                                                          ,PMM_rango=rango(PMM)
                                                          ,PMA_rango=rango(PMA)
                                                          ,PHM_rango=rango(PHM)
                                                          ,PHMJ_rango=rango(PHMJ)
                                                          ,PHMM_rango=rango(PHMM)
                                                          ,PHMA_rango=rango(PHMA)
                                                          ,PMuM_rango=rango(PMuM)
                                                          ,PMuMJ_rango=rango(PMuMJ)
                                                          ,PMuMM_rango=rango(PMuMM)
                                                          ,PMuMA_rango=rango(PMuMA)
                                                          ,PMAM_rango=rango(PMAM)
                                                          ,PHMAM_rango=rango(PHMAM)
                                                          ,PMuMAM_rango=rango(PMuMAM)
                                                          ,N_prom_rango=rango(N_prom)
                                                          ,NH_prom_rango=rango(NH_prom)
                                                          ,NMu_prom_rango=rango(NMu_prom),
                                                          #
                                                          PM_std=std(PM)
                                                          ,PMJ_std=std(PMJ)
                                                          ,PMM_std=std(PMM)
                                                          ,PMA_std=std(PMA)
                                                          ,PHM_std=std(PHM)
                                                          ,PHMJ_std=std(PHMJ)
                                                          ,PHMM_std=std(PHMM)
                                                          ,PHMA_std=std(PHMA)
                                                          ,PMuM_std=std(PMuM)
                                                          ,PMuMJ_std=std(PMuMJ)
                                                          ,PMuMM_std=std(PMuMM)
                                                          ,PMuMA_std=std(PMuMA)
                                                          ,PMAM_std=std(PMAM)
                                                          ,PHMAM_std=std(PHMAM)
                                                          ,PMuMAM_std=std(PMuMAM)
                                                          ,N_prom_std=std(N_prom)
                                                          ,NH_prom_std=std(NH_prom)
                                                          ,NMu_prom_std=std(NMu_prom),
                                                          #
                                                          PM_CV=CV(PM)
                                                          ,PMJ_CV=CV(PMJ)
                                                          ,PMM_CV=CV(PMM)
                                                          ,PMA_CV=CV(PMA)
                                                          ,PHM_CV=CV(PHM)
                                                          ,PHMJ_CV=CV(PHMJ)
                                                          ,PHMM_CV=CV(PHMM)
                                                          ,PHMA_CV=CV(PHMA)
                                                          ,PMuM_CV=CV(PMuM)
                                                          ,PMuMJ_CV=CV(PMuMJ)
                                                          ,PMuMM_CV=CV(PMuMM)
                                                          ,PMuMA_CV=CV(PMuMA)
                                                          ,PMAM_CV=CV(PMAM)
                                                          ,PHMAM_CV=CV(PHMAM)
                                                          ,PMuMAM_CV=CV(PMuMAM)
                                                          ,N_prom_CV=CV(N_prom)
                                                          ,NH_prom_CV=CV(NH_prom)
                                                          ,NMu_prom_CV=CV(NMu_prom)
)

d_15=t(d_15)
colnames(d_15)=c("Grupo 1","Grupo 2","Grupo 3","Grupo 4")
head(d_15)
write.xlsx2(d_15,file="Resumen.xlsx",sheetName = "Matriz B",append = T)


#########12######
correlacion_pe=cor(datos_12_norm)
correlacion_spe=cor(datos_12_norm,method = "spearman")
corrplot(correlacion_pe,method = "number")
corrplot(correlacion_spe,method = "number")


#PCA 12
pca_datos_12=PCA(datos_12,quali.sup = 1,graph = F,ncp = 2,scale.unit = T)
get_eig(pca_datos_12)
fviz_screeplot(pca_datos_12,addlabels=T)+
  xlab("Número de componentes principales")+
  ylab("Proporción de varianza explicada")

# Extract the results for variables
var <- get_pca_var(pca_datos_12)
var
head(var$coord)


fviz_pca_var(pca_datos_12, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)


# Contributions of variables to PC1
fviz_contrib(pca_datos_12, choice = "var", axes = 1, top = 18)
# Contributions of variables to PC2
fviz_contrib(pca_datos_12, choice = "var", axes = 2, top = 18)


# Extract the results for individuals
ind <- get_pca_ind(pca_datos_12)
datos_acp_12=ind$coord

fviz_pca_biplot(pca_datos_12, repel = F,axes = c(1,2))

##Cluster
den_datos_12 <- hcut(datos_acp_12, k = 4, stand = TRUE,hc_func = "diana",hc_metric = "euclidean",hc_method = "average")
den_datos_12$cluster
table(den_datos_12$cluster)
table(den_datos_12$cluster,datos$Tamaño)


# Visualize
plot(den_datos_12)
windows(width = 27,height = 27)
fviz_dend(den_datos_12, rect = TRUE, cex = 0.5,
          k_colors = c(1,2,3,4))

datos_acp_12_cluster=data.frame(UEA=datos_12[,1],datos_acp_12,cluster=den_datos_12$cluster)
head(datos_acp_12_cluster)
fviz_pca_biplot(pca_datos_12,
                repel = F,
                axes = c(1,2),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =as.factor(datos_acp_12_cluster$cluster),
                palette = "jco",
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T)

fviz_pca_biplot(pca_datos_12,
                repel = F,
                axes = c(1,2),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =datos$Tamaño,
                palette = "jco",
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T)

datos_12_cluster=data.frame(datos_12,cluster=as.factor(den_datos_12$cluster))

head(datos_12_cluster)
d_12=datos_12_cluster %>% group_by(cluster) %>% summarise(Num_UEA=n(),
                                                          PM_med=mean(PM)
                                                          ,PMJ_med=mean(PMJ)
                                                          ,PMM_med=mean(PMM)
                                                          ,PMA_med=mean(PMA)
                                                          ,PHM_med=mean(PHM)
                                                          ,PHMJ_med=mean(PHMJ)
                                                          ,PHMM_med=mean(PHMM)
                                                          ,PHMA_med=mean(PHMA)
                                                          ,PMuM_med=mean(PMuM)
                                                          ,PMuMJ_med=mean(PMuMJ)
                                                          ,PMuMM_med=mean(PMuMM)
                                                          ,PMuMA_med=mean(PMuMA)
                                                          ,PMAM_med=mean(PMAM)
                                                          ,PHMAM_med=mean(PHMAM)
                                                          ,PMuMAM_med=mean(PMuMAM)
                                                          ,N_prom_med=mean(N_prom)
                                                          ,NH_prom_med=mean(NH_prom)
                                                          ,NMu_prom_med=mean(NMu_prom),
                                                          #
                                                          PM_rango=rango(PM)
                                                          ,PMJ_rango=rango(PMJ)
                                                          ,PMM_rango=rango(PMM)
                                                          ,PMA_rango=rango(PMA)
                                                          ,PHM_rango=rango(PHM)
                                                          ,PHMJ_rango=rango(PHMJ)
                                                          ,PHMM_rango=rango(PHMM)
                                                          ,PHMA_rango=rango(PHMA)
                                                          ,PMuM_rango=rango(PMuM)
                                                          ,PMuMJ_rango=rango(PMuMJ)
                                                          ,PMuMM_rango=rango(PMuMM)
                                                          ,PMuMA_rango=rango(PMuMA)
                                                          ,PMAM_rango=rango(PMAM)
                                                          ,PHMAM_rango=rango(PHMAM)
                                                          ,PMuMAM_rango=rango(PMuMAM)
                                                          ,N_prom_rango=rango(N_prom)
                                                          ,NH_prom_rango=rango(NH_prom)
                                                          ,NMu_prom_rango=rango(NMu_prom),
                                                          #
                                                          PM_std=std(PM)
                                                          ,PMJ_std=std(PMJ)
                                                          ,PMM_std=std(PMM)
                                                          ,PMA_std=std(PMA)
                                                          ,PHM_std=std(PHM)
                                                          ,PHMJ_std=std(PHMJ)
                                                          ,PHMM_std=std(PHMM)
                                                          ,PHMA_std=std(PHMA)
                                                          ,PMuM_std=std(PMuM)
                                                          ,PMuMJ_std=std(PMuMJ)
                                                          ,PMuMM_std=std(PMuMM)
                                                          ,PMuMA_std=std(PMuMA)
                                                          ,PMAM_std=std(PMAM)
                                                          ,PHMAM_std=std(PHMAM)
                                                          ,PMuMAM_std=std(PMuMAM)
                                                          ,N_prom_std=std(N_prom)
                                                          ,NH_prom_std=std(NH_prom)
                                                          ,NMu_prom_std=std(NMu_prom),
                                                          #
                                                          PM_CV=CV(PM)
                                                          ,PMJ_CV=CV(PMJ)
                                                          ,PMM_CV=CV(PMM)
                                                          ,PMA_CV=CV(PMA)
                                                          ,PHM_CV=CV(PHM)
                                                          ,PHMJ_CV=CV(PHMJ)
                                                          ,PHMM_CV=CV(PHMM)
                                                          ,PHMA_CV=CV(PHMA)
                                                          ,PMuM_CV=CV(PMuM)
                                                          ,PMuMJ_CV=CV(PMuMJ)
                                                          ,PMuMM_CV=CV(PMuMM)
                                                          ,PMuMA_CV=CV(PMuMA)
                                                          ,PMAM_CV=CV(PMAM)
                                                          ,PHMAM_CV=CV(PHMAM)
                                                          ,PMuMAM_CV=CV(PMuMAM)
                                                          ,N_prom_CV=CV(N_prom)
                                                          ,NH_prom_CV=CV(NH_prom)
                                                          ,NMu_prom_CV=CV(NMu_prom)
)

d_12=t(d_12)
colnames(d_12)=c("Grupo 1","Grupo 2","Grupo 3","Grupo 4")
head(d_12)
write.xlsx2(d_12,file="Resumen.xlsx",sheetName = "Matriz C",append = T)


