#ACTUALIZADO ULTIMA VEZ:2020-10-20
#TAREA:TIPIFICACIÓN DE POBLACIONES DE ESPAÑA SEGÚN SU TAMAÑO Y GRADO DE ENVEJECIMIENTO
#CODIGO:05
library(dplyr)
library(janitor)
library(openxlsx)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(corrplot)
library("FactoMineR")
library("factoextra")
#00__FUNCIONES----
#Funcion para crear las etiquetas en la categorizacion de los tamanhos 
#poblacionales, segun los cortes indicados.
crear_categorias_grupos <- function(cortes) {
  #Quitar el primer valor y el ultimo: 0 e Inf
  cortes_truncado <- 
    cortes[c(-1,-length(cortes))]
  #Primera etiqueta
  primera_etiqueta <- 
    paste0("Menos de ",
           cortes_truncado[1]-1,
           " hab.")
  #Ultima etiqueta
  ultima_etiqueta <- 
    paste0("Más de ",
           cortes_truncado[length(cortes_truncado)],
           " hab.")
  #Etiquetas intermedias
  etiquetas_int <- 
    unlist(map(1:(length(cortes_truncado)-1),~paste0("De ",cortes_truncado[.x]," a ",cortes_truncado[.x+1]-1)))
  #Vector completo de etiquetas
  c(primera_etiqueta,etiquetas_int,ultima_etiqueta)
}
#funcion para realizar las tablas descriptivas de cada una de las matrices
#propuestas y sus clusters obtenidos
tabla_descriptiva_cluster <- function(tabla_ini,gcluster) {
  #Incluir la variable cluster que introduce los grupos creados 
  tabla_ini$cluster <- as.factor(gcluster)
  #Calculos de media, rango,std y cv
  pre_tabla_descriptiva_18 <- 
    tabla_ini %>% 
    select(UEA,cluster,everything()) %>% 
    gather("var","val",PM:ncol(.)) %>% 
    group_by(cluster,var) %>% 
    summarise(media=round(mean(val),3),
              rango=paste0("[",round(min(val),3),"-",
                           round(max(val),3),"]"),
              std=round(sqrt(var(val)/length(val)),3),
              cv=round(sqrt(var(val))/mean(val),3)) %>% 
    ungroup() %>% 
    gather("calculo","val",media:cv) %>% 
    spread(cluster,val) %>% 
    mutate(calculo=
             forcats::fct_relevel(as.factor(calculo),
                                  levels=c("media","rango","std","cv"))) %>% 
    arrange(calculo) %>% 
    mutate(variable=paste0(var,"_",calculo)) %>% 
    relocate(variable,.before=`1`) %>% 
    select(-c(var,calculo)) %>% 
    setNames(c(" ",paste0("Grupo",names(.)[-1]))) %>% 
    mutate_at(vars(names(.)[2:ncol(.)]),
              ~str_replace_all(.,c("[.]"=",")))
  #Tamanho de cada cluster
  n_cluster <- 
    tabla_ini %>% 
    group_by(cluster) %>% 
    summarise(variable=n()) %>% 
    spread(cluster,variable) %>% 
    mutate(variable="N") %>% 
    select(variable,everything()) %>% 
    setNames(c(" ",paste0("Grupo",names(.)[-1])))
  
  #TABLA FINAL
  rbind(
    # matrix(c("variable",1:(ncol(n_cluster)-1)),nrow=1) %>% 
    #       as.data.frame() %>% 
    #       setNames(c("variable",1:(ncol(n_cluster)-1))),
    n_cluster,
        pre_tabla_descriptiva_18) 
}
#01_ABRIR_DATOS----
#Ruta archivo excel de poblacion UEA 1996-2019
ruta_pob_uea_96_19 <- 
    here::here("01_DATOS",
               "0.4.Pob_Total_Sexo_UEA_1996_2019.xlsx")
#Nombres de las hojas del archivo inicial 
nombres_hojas_pob_uea_96_19 <- 
    readxl::excel_sheets(ruta_pob_uea_96_19)
#Ruta archivo excel de envejecimiento UEA 1996-2019. Los n de las UEA
#se calculan con el sumatorio de los n_municipales
ruta_envej_uea_96_19_sum <- 
  here::here("01_DATOS",
             "10.3.2.PobTot_Envej_UEA_8.10.20_2_sum.xlsx")
#Ruta archivo excel de envejecimiento UEA 1996-2019. Los n de las UEA
#se calculan con el promedio de los n_municipales
ruta_envej_uea_96_19_prom <- 
  here::here("01_DATOS",
             "10.3.2.PobTot_Envej_UEA_8.10.20_2_promed.xlsx")
#Abrir
envej_uea_96_19 <- 
  readxl::read_xlsx(ruta_envej_uea_96_19_prom,sheet="Prom_1996_2019") %>% 
  clean_names()
#Nombres de las hojas del archivo de envejecimiento de UEA 1996-2019
nombres_hojas_envej_uea_96_19 <- 
  readxl::excel_sheets(ruta_envej_uea_96_19_prom)
#Tabla de correspondencia UEA-nombre/codigo del municipio
codigos_uea_nom <- 
  readxl::read_xlsx(ruta_envej_uea_96_19_prom,sheet="RELACION_MUNIS_UEA") %>% 
  clean_names() %>% 
  mutate(codigo_ine=as.character(codigo_ine))
#TASAS DE CRECIMIENTO
ruta_tasa_crecimiento <- 
  here::here("01_DATOS","1.TC_PM_MetodoB_16_10_2020.xlsx")
#Nombres de las hojas que nos interesan
hojas_tasa_crecimiento <- 
readxl::excel_sheets(ruta_tasa_crecimiento)[-1]
#Abrir las hojas que nos interesan
tasa_crecimiento <- 
  map(hojas_tasa_crecimiento,
       ~readxl::read_xlsx(ruta_tasa_crecimiento,sheet=.x) %>% 
  select(UEA,TC1,CLASES_TC1) %>% 
    mutate(categoria=.x,
           sexo=unlist(str_extract_all(categoria,
        "AMBOS SEXOS|HOMBRES|MUJERES")),
        categoria=str_replace_all(categoria,
      c("AMBOS SEXOS|HOMBRES|MUJERES"="")) %>% 
        str_trim(),
      categoria=case_when(
        categoria=="Total"~"M",
        TRUE~categoria)) %>%
  clean_names()) %>% 
  setNames(str_replace_all(hojas_tasa_crecimiento,c("Total"="M")))
#02_FASE-2:ANALIS MULTIVARIANTE: PCA y clustering jerarquico----
#Datos de envejecimiento por uea 1996-2019 
envej_uea_96_19 <- 
  readxl::read_xlsx(ruta_envej_uea_96_19_prom,sheet="Prom_1996_2019")
  # mutate(Tamanho=as.factor(cut(N_prom,cortes_categorias_pob_muni_b,
  #                           labels=c("Pequeño","Mediano","Grande"),
  #                           include.lowest = TRUE)))

#conformar tabla de analisis
datos=envej_uea_96_19
#4 OPCIONES
#A
datos_21=select(datos[-692,],UEA,PM,PMJ,PMM,PMA,PHM,PHMJ,PHMM,
                PHMA,PMuM,PMuMJ,PMuMM,PMuMA,PMAM,PHMAM,
                PMuMAM,N_prom,NH_prom,NMu_prom)
#A2
datos_21b=select(datos[-692,],UEA,PM,PMJ,PMM,PMA,PHM,PHMJ,PHMM,
                 PHMA,PMuM,PMuMJ,PMuMM,PMuMA,PMAM,PHMAM,
                 PMuMAM,N_prom,NH_prom,NMu_prom) %>% 
  #Eliminar Madrid y Barcelona
  filter(!UEA%in%c("28079",#Madrid
                  "8019")#Barcelona
         )
#B
datos_15=select(datos[-692,],UEA,PM,PMJ,PMM,PMA,PHM,PHMJ,
                PHMM,PHMA,PMuM,PMuMJ,PMuMM,PMuMA,
                PMAM,PHMAM,PMuMAM)
#B2
datos_18=select(datos[-692,],UEA,PM,PMJ,PMM,PMA,PHM,PHMJ,
                PHMM,PHMA,PMuM,PMuMJ,PMuMM,PMuMA,
                PMAM,PHMAM,PMuMAM,PMJM,PHMJM,PMuMJM)
#C
datos_12=select(datos[-692,],UEA,PM,PMJ,PMM,PMA,PHM,PHMJ,
                PHMM,PHMA,PMuM,PMuMJ,PMuMM,PMuMA)
#Unificar las escalas de los datos
datos_21_norm=scale(datos_21[,-1])
datos_21b_norm=scale(datos_21b[,-1])
datos_15_norm=scale(datos_15[,-1])
datos_18_norm=scale(datos_18[,-1])
datos_12_norm=scale(datos_12[,-1])
#OPCION-A----
#Calcular correlaciones entre las variables
correlacion_pe=cor(datos_21_norm)
correlacion_spe=cor(datos_21_norm,method = "spearman")
#PCA 21 variables
pca_datos_21=PCA(datos_21,quali.sup = 1,
                 graph = F,ncp = 3,scale.unit = T)
#Componentes varibilidad
prop_pca_comp_21 <- 
fviz_screeplot(pca_datos_21,addlabels=T,labelsize = 40)+
  xlab("Número de componentes principales")+
  ylab("Proporción de varianza explicada")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))

# ggplot2::ggsave(here::here("03_RESULTADOS/GRAFICOS/prop_pca_comp_21.svg"),
#                 prop_pca_comp_21)
# Extract the results for variables
var_21 <- get_pca_var(pca_datos_21)
round(var_21$cor,3)
#guardar tabla para dar formato
# openxlsx::write.xlsx(round(var_21$cor,3),
#                      "cousa.xlsx",rowNames=TRUE)

#Graficos de contribución
fviz_pca_var(pca_datos_21, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)+
  labs(title="Contribución de las variables en la ACP")
fviz_pca_var(pca_datos_21, col.var="coord",
             axes = c(1,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping,
)+
  labs(title="Contribución de las variables en la ACP")

# Contribuciones de las variables a la Dim1
fviz_contrib(pca_datos_21, choice = "var", axes = 1, top = 18)
# Contribuciones de las variables a la Dim2
fviz_contrib(pca_datos_21, choice = "var", axes = 2, top = 18)
# Contribuciones de las variables a la Dim3
fviz_contrib(pca_datos_21, choice = "var", axes = 3, top = 18)
# Extraer los resultados de los individuos
ind_21 <- get_pca_ind(pca_datos_21)
##BIPLOTS
#Dim1-Dim2
fviz_pca_biplot(pca_datos_21, repel = F,axes = c(1,2),labelsize=5)+
  labs(title="Biplot")+
  labs(title="Biplot del ACP")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))
#Dim1-Dim3
fviz_pca_biplot(pca_datos_21, repel = F,axes = c(1,3),labelsize=6)+
  labs(title="Biplot del ACP")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))

#Datos PCA 18
datos_acp_21=ind_21$coord
#Numero optimo de clusters
# res.nbclust <- datos_acp_21%>%
#   NbClust::NbClust(distance = "euclidean",
#           min.nc = 2, max.nc = 10,
#           method = "average", index ="all")
#Cluster
den_datos_21 <- hcut(datos_acp_21, k = 5, stand = TRUE,
                     hc_func = "diana",hc_metric = "euclidean",
                     hc_method = "average")
#N de cada grupo
table(den_datos_21$cluster)
#Ver dendrograma
dendrograma_21 <- 
fviz_dend(den_datos_21,  rect=TRUE,cex = 0.5,horiz=TRUE,palette="jco")

dendrograma_21+
  labs(title="Dendrograma")+
  theme_void()+
  theme(plot.title = element_text(size=30))

#Unir indicadores municipios, valores dimensiones ACP y clusters
datos_acp_21_cluster=data.frame(UEA=datos_21[,1],datos_acp_21,
                                cluster=den_datos_21$cluster)

datos_acp_21_cluster %>% filter(UEA=="28079")

filter(!UEA%in%c("28079",#Madrid
                 "8019")#Barcelona

#Biplot con clusters 
#Dim1-2
fviz_pca_biplot(pca_datos_21,
                repel = F,
                axes = c(1,2),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =as.factor(datos_acp_21_cluster$cluster),
                palette = "jco",
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T,labelsize=7)+
  labs(title="Biplot con los cluster")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))
#Dim 1-3
fviz_pca_biplot(pca_datos_21,
                repel = F,
                axes = c(1,3),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =as.factor(datos_acp_21_cluster$cluster),
                palette = "jco",
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T,labelsize=7)+
  labs(title="Biplot con los cluster")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))

#3D biplot
pca_datos_18_3d=prcomp(datos_21_norm)
pca3d::pca3d(pca_datos_18_3d,
             radius = 2,
             axes.color="#00aa44ff",
             show.shapes=TRUE,
             shape="sphere",
             palette = ggsci::pal_jco("default", alpha = 0.7)(5),
             group=den_datos_21$cluster)

pca3d::snapshotPCA3d(file=here::here("03_RESULTADOS/GRAFICOS/biplot_b2_3d.png"))
#Silhouette 
den_datos_21$silinfo
#OPCION-A2----
#Calcular correlaciones entre las variables
correlacion_pe=cor(datos_21b_norm)
correlacion_spe=cor(datos_21b_norm,method = "spearman")
#PCA 21 variables
pca_datos_21b=PCA(datos_21b,quali.sup = 1,
                 graph = F,ncp = 3,scale.unit = T)
#Componentes varibilidad
prop_pca_comp_21b <- 
  fviz_screeplot(pca_datos_21b,addlabels=T,labelsize = 40)+
  xlab("Número de componentes principales")+
  ylab("Proporción de varianza explicada")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))

 # ggplot2::ggsave(here::here("03_RESULTADOS/GRAFICOS/prop_pca_comp_21b.svg"),
 #                 prop_pca_comp_21b)
# Extract the results for variables
var_21b <- get_pca_var(pca_datos_21b)
round(var_21b$cor,3)
#guardar tabla para dar formato
 # openxlsx::write.xlsx(round(var_21b$cor,3),
 #                     "cousa.xlsx",rowNames=TRUE)

#Graficos de contribución
fviz_pca_var(pca_datos_21b, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)+
  labs(title="Contribución de las variables en la ACP")
fviz_pca_var(pca_datos_21b, col.var="coord",
             axes = c(1,3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping,
)+
  labs(title="Contribución de las variables en la ACP")

# Contribuciones de las variables a la Dim1
fviz_contrib(pca_datos_21b, choice = "var", axes = 1, top = 18)
# Contribuciones de las variables a la Dim2
fviz_contrib(pca_datos_21b, choice = "var", axes = 2, top = 18)
# Contribuciones de las variables a la Dim3
fviz_contrib(pca_datos_21b, choice = "var", axes = 3, top = 18)
# Extraer los resultados de los individuos
ind_21b <- get_pca_ind(pca_datos_21b)
##BIPLOTS
#Dim1-Dim2
fviz_pca_biplot(pca_datos_21b, repel = F,axes = c(1,2),labelsize=5)+
  labs(title="Biplot")+
  labs(title="Biplot del ACP")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))
#Dim1-Dim3
fviz_pca_biplot(pca_datos_21b, repel = F,axes = c(1,3),labelsize=6)+
  labs(title="Biplot del ACP")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))

#Datos PCA 18
datos_acp_21b=ind_21b$coord
#Numero optimo de clusters
# res.nbclust <- datos_acp_21b%>%
#   NbClust::NbClust(distance = "euclidean",
#           min.nc = 2, max.nc = 10,
#           method = "average", index ="all")
#Cluster
den_datos_21b <- hcut(datos_acp_21b, k = 5, stand = TRUE,
                     hc_func = "diana",hc_metric = "euclidean",
                     hc_method = "average")
#N de cada grupo
table(den_datos_21b$cluster)
#Ver dendrograma
dendrograma_21b <- 
  fviz_dend(den_datos_21b,  rect=TRUE,cex = 0.5,horiz=TRUE,
            palette=c(ggsci::pal_jco("default", alpha = 0.7)(4),"#4a9632ff"))

dendrograma_21b+
  labs(title="Dendrograma")+
  theme_void()+
  theme(plot.title = element_text(size=30))

#Unir indicadores municipios, valores dimensiones ACP y clusters
datos_acp_21b_cluster=data.frame(UEA=datos_21b[,1],datos_acp_21b,
                                cluster=den_datos_21b$cluster)

#Biplot con clusters 
#Dim1-2
fviz_pca_biplot(pca_datos_21b,
                repel = F,
                axes = c(1,2),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =as.factor(datos_acp_21b_cluster$cluster),
                palette = c(ggsci::pal_jco("default", alpha = 0.7)(4),"#4a9632ff"),
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T,labelsize=7)+
  labs(title="Biplot con los cluster")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))


#Dim 1-3
fviz_pca_biplot(pca_datos_21b,
                repel = F,
                axes = c(1,3),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =as.factor(datos_acp_21b_cluster$cluster),
                palette =  c(ggsci::pal_jco("default", alpha = 0.7)(4),"#4a9632ff"),
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T,labelsize=7)+
  labs(title="Biplot con los cluster")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))

#3D biplot
pca_datos_21b_3d=prcomp(as.matrix(datos_21b[,-1]),scale.=TRUE,center=TRUE)
pca3d::pca3d(pca_datos_21b_3d,
             radius = 1,
             axes.color="black",
             show.shapes=TRUE,
             show.scale = TRUE,
             shape="sphere",
             palette = c(ggsci::pal_jco("default", alpha = 0.7)(4),"#4a9632ff"),
             group=den_datos_21b$cluster)
pca3d::snapshotPCA3d(file=here::here("03_RESULTADOS/GRAFICOS/biplot_b2_3d.png"))

#3d PLOTLY
clusters_esp <- 
  c("Envejecimiento alto - Sobrenvejecimiento ligero"="1",
    "Envejecimiento moderado - Sobrenvejecimiento moderado"="2",
    "Joven"="3",
    "Envejecimiento muy alto"="4",
    "Envejecimiento moderado - Sobrenvejecimiento ligero"="5")

clusters_eng <- 
  c("Old population - Slight over-ageing"="1",
    "Moderately old population  - Moderate over-ageing"="2",
    "Young population"="3",
    "Very old population "="4",
    "Moderately old population - Slight over-ageing"="5")

datos_biplot3d <- 
  datos_acp_21b_cluster %>% 
  mutate(cluster=as.factor(cluster),
         cluster_esp=forcats::fct_recode(cluster,
                                         !!!clusters_esp),
         cluster_eng=forcats::fct_recode(cluster,
                                         !!!clusters_eng))


figura_3d <- 
  plotly::plot_ly(datos_biplot3d,
                  x = ~Dim.1, y = ~Dim.3, z = ~Dim.2,
                  color = ~cluster_esp,
                  colors =c(ggsci::pal_jco("default", alpha = 0.7)(4),"#4a9632ff"),
                  marker = list(
                    size = 5,
                    line = list(color = "black",
                                width = 1))) %>% 
  plotly::layout(scene = list(xaxis = list(range = c(-10,15),
                                   nticks = 16,
                                   showgrid = F,
                                   title = 'CP 1',
                                   gridcolor =  'grey70'),
                      yaxis = list(range = c(-8,5),
                                   nticks = 8,
                                   showgrid = F,
                                   title = 'CP 3',
                                   gridcolor =  'grey70'),
                      zaxis = list(range = c(-4,20),
                                   nticks = 16,
                                   showgrid = F,
                                   title = 'CP 2',
                                   gridcolor =  'grey70'),
                      zerolinewidth = 1,
                      ticklen = 5))
#Silhouette 
den_datos_21b$silinfo
#Ubicar Madrid y Barcelona
datos_acp_21b_cluster %>% 
  .[c(570,#Valencia
      506,#Sevilla
      597,#Zaragoza
      350#Málaga
      ),]
#OPCION-B----
#Calcular correlaciones entre las variables
correlacion_pe=cor(datos_15_norm)
correlacion_spe=cor(datos_15_norm,method = "spearman")
#PCA 15
pca_datos_15=PCA(datos_15,quali.sup = 1,
                 graph = F,ncp = 2,scale.unit = T)
prop_pca_comp_15 <- 
fviz_screeplot(pca_datos_15,addlabels=T,labelsize = 40)+
  xlab("Número de componentes principales")+
  ylab("Proporción de varianza explicada")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))

# ggplot2::ggsave(here::here("03_RESULTADOS/GRAFICOS/prop_pca_comp_15.svg"),
#                 prop_pca_comp_15)
#Extraer resultados por variables
var_15 <- get_pca_var(pca_datos_15)
round(var_15$cor,3)
#guardar tabla para dar formato
# openxlsx::write.xlsx(round(var_15$cor,3),
#                      "cousa.xlsx",rowNames=TRUE)
#Graficos de contribución
fviz_pca_var(pca_datos_15, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)+
  labs(title="Contribución de las variables en la ACP")

# Contribuciones de las variables a la Dim1
fviz_contrib(pca_datos_15, choice = "var", axes = 1, top = 18)
# Contribuciones de las variables a la Dim2
fviz_contrib(pca_datos_15, choice = "var", axes = 2, top = 18)
# Extraer los resultados de los individuos
ind_15 <- get_pca_ind(pca_datos_15)
##BIPLOTS
#Dim1-Dim2
fviz_pca_biplot(pca_datos_15, repel = F,axes = c(1,2),labelsize=7)+
  labs(title="Biplot")+
  labs(title="Biplot del ACP")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=20),
        axis.title = element_text(size=30))
#Datos PCA 15
datos_acp_15=ind_15$coord
#Cluster
den_datos_15 <- hcut(datos_acp_15, k = 4, stand = TRUE,
                     hc_func = "diana",hc_metric = "euclidean",
                     hc_method = "average")
#N de cada grupo
table(den_datos_15$cluster)
#Ver dendrograma
dendrograma_15 <- 
fviz_dend(den_datos_15, rect = TRUE, cex = 0.5,
          palette="jco",horiz=TRUE)

dendrograma_15+
  labs(title="Dendrograma")+
  theme_void()+
  theme(plot.title = element_text(size=30))
#Unir indicadores municipios, valores dimensiones ACP y clusters
datos_acp_15_cluster=data.frame(UEA=datos_15[,1],datos_acp_15,cluster=den_datos_15$cluster)
#Ubicar Madrid y Barcelona
datos_acp_15_cluster %>% 
filter(UEA%in%c("28079",#Madrid
                 "8019"))#Barcelona

#Biplot con clusters 
fviz_pca_biplot(pca_datos_15,
                repel = F,
                axes = c(1,2),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =as.factor(datos_acp_15_cluster$cluster),
                palette = "jco",
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T,labelsize=7)+
  labs(title="Biplot con los cluster")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=20),
        axis.title = element_text(size=30))
#Silhouette 
den_datos_15$silinfo
#OPCION-B2----
#B2
#Calcular correlaciones entre las variables
correlacion_pe=cor(datos_15b_norm)
correlacion_spe=cor(datos_15b_norm,method = "spearman")
#PCA 18
pca_datos_18=PCA(datos_18,quali.sup = 1,graph = F,ncp = 2,scale.unit = T)

prop_pca_comp_18 <-
fviz_screeplot(pca_datos_18,addlabels=T,labelsize = 40)+
  xlab("Número de componentes principales")+
  ylab("Proporción de varianza explicada")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))
# ggplot2::ggsave(here::here("03_RESULTADOS/GRAFICOS/prop_pca_comp_18.svg"),
#                 prop_pca_comp_18)
#Extraer resultados por variables
var_18 <- get_pca_var(pca_datos_18)
tab_cor_18 <- 
round(var_18$cor,3)
# openxlsx::write.xlsx(round(var_18$cor,3),
#                      "cousa.xlsx",rowNames=TRUE)
#Graficos de contribución
fviz_pca_var(pca_datos_18, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)+
  labs(title="Contribución de las variables en la ACP")

# Contribuciones de las variables a la Dim1
fviz_contrib(pca_datos_18, choice = "var", axes = 1, top = 18)
# Contribuciones de las variables a la Dim2
fviz_contrib(pca_datos_18, choice = "var", axes = 2, top = 18)
# Extraer los resultados de los individuos
ind_18 <- get_pca_ind(pca_datos_18)
##BIPLOTS
#Dim1-Dim2
fviz_pca_biplot(pca_datos_18, repel = F,axes = c(1,2),labelsize=7)+
  labs(title="Biplot")+
  labs(title="Biplot del ACP")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=20),
        axis.title = element_text(size=30))

#Datos PCA 18
datos_acp_18=ind_18$coord
#Numero optimo de clusters
# res.nbclust <- datos_acp_15b%>%
#   NbClust::NbClust(distance = "euclidean",
#           min.nc = 2, max.nc = 10,
#           method = "average", index ="all")

#Cluster
den_datos_18 <- hcut(datos_acp_18, k = 7, stand = TRUE,
                     hc_func = "diana",hc_metric = "euclidean",
                     hc_method = "average")
#N de cada grupo
table(den_datos_18$cluster)
#Ver dendrograma
dendrograma_18 <- 
  fviz_dend(den_datos_18, rect = TRUE, cex = 0.5,
            palette="jco",horiz=TRUE)

dendrograma_18+
  labs(title="Dendrograma")+
  theme_void()+
  theme(plot.title = element_text(size=30))
#Unir indicadores municipios, valores dimensiones ACP y clusters
datos_acp_18_cluster=data.frame(UEA=datos_18[,1],datos_acp_18,cluster=den_datos_18$cluster)
#Ubicar Madrid y Barcelona
datos_acp_18_cluster %>% 
  filter(UEA%in%c("28079",#Madrid
                  "8019"))#Barcelona
#Biplot con clusters 
fviz_pca_biplot(pca_datos_18,
                repel = F,
                axes = c(1,2),
                #label = "none",
                geom.ind = "point", 
                col.ind =as.factor(datos_acp_18_cluster$cluster),
                palette = "jco",
                legend.title="Grupos",
                col.var = 1,
                gradient.cols=3,
                addEllipses = T,labelsize=7)+
  labs(title="Biplot con los cluster")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=20),
        axis.title = element_text(size=30))

#Silhouette 
den_datos_18$silinfo


as_tibble(datos_acp_18_cluster) %>% 
  left_join(codigos_uea_nom,by=c("UEA"="codigo_ine")) %>% 
  mutate(nombre=case_when(
    is.na(nombre_municipio)~UEA,
    TRUE~nombre_municipio
  )) %>% select(nombre,Dim.1,Dim.2,cluster) %>% 
  ggplot(aes(Dim.1,Dim.2,label=nombre,
             colour=as.character(cluster)))+
  geom_point()+
  geom_text(size=4)+
  ggsci::scale_color_jco()+
  labs(x=paste("Dimensión 1 (",round(pca_datos_18$eig[,2][1],1),"%)"),
       y=paste("Dimensión 2 (",round(pca_datos_18$eig[,2][2],1),"%)"))+
  guides(colour=guide_legend(title="Grupos"))+
  theme_minimal()+
  theme(text=element_text(size=30),
        axis.text = element_text(size=20),
        axis.title = element_text(size=30))
#OPCION-D----
#Calcular correlaciones entre las variables
correlacion_pe=cor(datos_12_norm)
correlacion_spe=cor(datos_12_norm,method = "spearman")
#PCA 12
pca_datos_12=PCA(datos_12,quali.sup = 1,graph = F,ncp = 2,scale.unit = T)

prop_pca_comp_12 <-
fviz_screeplot(pca_datos_12,addlabels=T,labelsize = 40)+
  xlab("Número de componentes principales")+
  ylab("Proporción de varianza explicada")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))
# ggplot2::ggsave(here::here("03_RESULTADOS/GRAFICOS/prop_pca_comp_12.svg"),
#                 prop_pca_comp_12)
#Extraer resultados por variables
var_12 <- get_pca_var(pca_datos_12)
round(var_12$cor,3)
# openxlsx::write.xlsx(round(var_12$cor,3),
#                      "cousa.xlsx",rowNames=TRUE)
#Graficos de contribución
fviz_pca_var(pca_datos_12, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)+
  labs(title="Contribución de las variables en la ACP")

# Contribuciones de las variables a la Dim1
fviz_contrib(pca_datos_12, choice = "var", axes = 1, top = 18)
# Contribuciones de las variables a la Dim2
fviz_contrib(pca_datos_12, choice = "var", axes = 2, top = 18)


# Extraer los resultados de los individuos
ind_12 <- get_pca_ind(pca_datos_12)
##BIPLOTS
#Dim1-Dim2
fviz_pca_biplot(pca_datos_12, repel = F,axes = c(1,2),labelsize=7)+
  labs(title="Biplot")+
  labs(title="Biplot del ACP")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=20),
        axis.title = element_text(size=30))
#Datos PCA 1
datos_acp_12=ind_12$coord
#Cluster
den_datos_12 <- hcut(datos_acp_12, k = 4, stand = TRUE,hc_func = "diana",hc_metric = "euclidean",hc_method = "average")
#N de cada grupo
table(den_datos_12$cluster)
#Ver dendrograma
dendrograma_12 <- 
fviz_dend(den_datos_12, rect = TRUE, cex = 0.5,
          palette="jco",horiz=TRUE)

dendrograma_12+
  labs(title="Dendrograma")+
  theme_void()+
  theme(plot.title = element_text(size=30))
#Unir indicadores municipios, valores dimensiones ACP y clusters
datos_acp_12_cluster=data.frame(UEA=datos_12[,1],datos_acp_12,cluster=den_datos_12$cluster)
#Ubicar Madrid y Barcelona
datos_acp_12_cluster %>% 
  filter(UEA%in%c("28079",#Madrid
                  "8019"))#Barcelona
#Biplot con clusters 
fviz_pca_biplot(pca_datos_12,
                repel = F,
                axes = c(1,2),
                #label = "none",
                geom.ind = "point", # show points only (nbut not "text")
                col.ind =as.factor(datos_acp_12_cluster$cluster),
                palette = "jco",
                legend.title="Grupos",
                col.var = 1,
                addEllipses = T,labelsize=7)+
  labs(title="Biplot con los cluster")+
  theme(text=element_text(size=30),
        axis.text = element_text(size=20),
        axis.title = element_text(size=30))
  
#Silhouette 
den_datos_12$silinfo
#TABLAS DESCRIPTIVAS GRUPOS (JAVIER)----
#En estas tablas se calculara:
#' Media
#' Rango (minimo-maximo)
#' Desviacion estándar (std)
#' Coeficiente de variacion (CV)

#Crear libro de Excel
excel_tablas_descriptivas <- createWorkbook("tablas_descriptivas_clusters")
nombres_hojas_excel <- 
  paste0("MATRIZ ",
  c("A","A2","B","B2","C"))
#Realizar las tablas descriptivas con la funcion "tabla_descriptiva_cluster"
tablas_descriptivas_cluster <- 
map2(list(datos_21,datos_21b,datos_15,datos_18,datos_12),
     list(den_datos_21$cluster,den_datos_21b$cluster,den_datos_15$cluster,
          den_datos_18$cluster,den_datos_12$cluster),
     ~tabla_descriptiva_cluster(.x,.y)) 

#Rellenar las hojas del libro de Excel
for(i in 1:length(nombres_hojas_excel)){
addWorksheet(wb=excel_tablas_descriptivas,
             sheetName = nombres_hojas_excel[i]
             )

writeData(wb=excel_tablas_descriptivas, x=tablas_descriptivas_cluster[[i]],
          sheet= nombres_hojas_excel[i],
          withFilter = FALSE,colNames = TRUE)

setColWidths(excel_tablas_descriptivas, sheet = nombres_hojas_excel[i],
             cols=1:ncol(tablas_descriptivas_cluster[[i]]),
             widths = "auto")

# for(n in 1:nrow(tablas_descriptivas_cluster[[i]])){
  addStyle(wb = excel_tablas_descriptivas,
         sheet = nombres_hojas_excel[i],
         createStyle(numFmt = "#,##0;-#,##0;-"),
         cols=2:ncol(tablas_descriptivas_cluster[[i]]),
         rows=3)

}

#GUARDAR EL LIBRO DE EXCEL
saveWorkbook(excel_tablas_descriptivas,
             file = paste0("03_RESULTADOS/TABLAS/",
              "0.5.Tabla_Desc_Tipificación UEA_Tamaño_Pob_Envej.xlsx"),
             overwrite = TRUE)
#Anhadir los clusters a los dato iniciales
matrices_grupos <- 
map2(list(datos_21,datos_21b,datos_15,datos_18,datos_12),
     list(den_datos_21$cluster,den_datos_21b$cluster,den_datos_15$cluster,
          den_datos_18$cluster,den_datos_12$cluster),~cbind(.x,.y) %>% 
       as_tibble() %>%
       rename("Grupo"=".y") %>% 
       arrange(Grupo) %>% 
       select(UEA, Grupo,everything())) %>% 
  setNames(nombres_hojas_excel) 
#GUARDAR
  # write.xlsx(matrices_grupos,
  #              file = paste0("03_RESULTADOS/TABLAS/",
  #                            "Tablas_Matrices_Grupos.xlsx"),
  #              overwrite = TRUE)
#DESCRIPCION_GRUPOS_CLUSTER----
#Abrir los datos de las tablas descriptivas
ruta_tipificacion_desc <- 
  here::here("03_RESULTADOS/TABLAS/",
             "0.5.Tabla_Desc_Tipificación UEA_Tamaño_Pob_Envej.xlsx") 
#Abrir
tipificacion_desc <- 
  readxl::read_xlsx(ruta_tipificacion_desc,sheet="MATRIZ A2") %>% 
  clean_names() %>% 
  .[1:19,] %>% 
  mutate_at(vars(names(.)[2:ncol(.)]),
            ~as.numeric(str_replace_all(.,c("[,]"="."))))

#Tabla descriptiva por sexos
tabla_descriptiva <- 
t(as.matrix(tipificacion_desc))[-1,] %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var="grupo") %>% 
  setNames(c("cluster",t(as.matrix(tipificacion_desc))[1,] )) %>% 
  as_tibble() %>% 
  mutate_at(vars(names(.)[2:ncol(.)] ),~as.numeric(.)) %>% 
  gather("var","val",2:ncol(.) ) %>% 
  mutate(sexo=case_when(
    stringr::str_detect(var,"H")~"hombre",
    stringr::str_detect(var,"Mu")~"mujer",
    TRUE~NA_character_
  )) %>% 
  filter(!is.na(sexo)) %>% 
  mutate(var=case_when(
    stringr::str_detect(var,"H")~stringr::str_replace_all(var,c("H"="")),
    stringr::str_detect(var,"Mu")~stringr::str_replace_all(var,c("Mu"="")),
  )) %>% 
  filter(!str_detect(var,"N")) %>% 
  mutate(var=forcats::fct_relevel(as.factor(var),
                                  levels=c("PM_media","PMJ_media",
                                           "PMM_media","PMA_media",
                                           "PMJM_media","PMAM_media")))


  #Grafico de barras
grafico_tabla_descriptiva <- 
  tabla_descriptiva %>% 
  ggplot(.,aes(cluster,val,fill=sexo))+
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~var,nrow=1)+
  labs(x="Cluster",y="Porcentaje")+
  scale_fill_manual(values=c("#F4BA3B","#33ADBB"))+
  guides(fill = guide_legend(title.position = "top"))+
  theme_minimal()+
  theme(axis.title = element_text(size=15),
        strip.text = element_text(size=12),
        legend.position = "bottom",
        legend.title.align = .5,
        axis.text = element_text(size=15))

#02_PROCESO:FASE1----
#Categorias de clasificacion del tamanho poblacional por UEA---- 
#Derivado de lo anterior queremos establecer 3 categorias:
#'- Categoria 1: Menos de 1.000 hab.
#'- Categoria 2: De 1.001 a 5.000 hab.
#'- Categoria 3: De 5.001 a 10.000 hab.
#'- Categoria 4: De 10.001 a 20.000 hab.
#'- Categoria 5: De 20.001 a 50.000 hab.
#'- Categoria 6: Mas de 50.001 hab.
categorias_pob_muni <- 
  c("Menos de 9.000 hab.", "De 9.001 a 15.000 hab.",
    "De 15.001 a 50.000 hab.", "Más de 50.001 hab.")
#Cortes para establecer las categorias
cortes_categorias_pob_muni <- 
  c(0,1001,5001,10001,20001,50001,Inf)
categorias_pob_muni<- 
  crear_categorias_grupos(cortes_categorias_pob_muni)
#Datos poblacionales totales por uea 1996-2019 categorizadas
#Hojas seleccionadas
hojas_envej_uea_sel <- 
  which(nombres_hojas_envej_uea_96_19%in%c(1996,1998:2019)==TRUE)
#segun las categorias B
envej_uea_96_19_cat <- 
  map(hojas_envej_uea_sel,~ readxl::read_xlsx(ruta_envej_uea_96_19_prom,sheet=.x) %>% 
        clean_names() %>% filter(uea!="Total")) %>% 
  setNames(nombres_hojas_envej_uea_96_19[hojas_envej_uea_sel]) %>% 
  imap(.,~.x %>% 
         mutate(ano=.y)%>% 
         select(uea,n_prom,ano) %>% 
         mutate(categorias_pob=cut(n_prom,cortes_categorias_pob_muni,
                                   labels=categorias_pob_muni,
                                   include.lowest = TRUE)) %>% 
         group_by(ano,categorias_pob) %>% 
         count() %>% 
         ungroup()) %>% 
  bind_rows() 
#Representacion grafica de la cantidad de UEA por categoria de tamanho pob
#segun las categorias 
envej_uea_96_19_cat %>% 
  filter(!is.na(categorias_pob)==TRUE) %>% 
  ggplot(.,aes(reorder(categorias_pob,n),n))+
  geom_bar(stat="identity")+
  coord_flip()+
  facet_wrap(~ano,nrow=3)+
  labs(x="Tamaño poblacional por grupos",y="n")

#CATEGORIAS POBLACIONALES en el valor medio----
#Abrir datos medios y crear agrupaciones
categorias_envej_uea_96_19<- 
  openxlsx::read.xlsx(ruta_envej_uea_96_19_prom,sheet="Prom_1996_2019") %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(categorias_pob=cut(n_prom,cortes_categorias_pob_muni,
                            labels=categorias_pob_muni,
                            include.lowest = TRUE)) 

#Tamanho de los grupos en num de UEA
grafico_tam_uea_categorias <- 
  categorias_envej_uea_96_19 %>% 
  select(uea,n_prom) %>% 
  group_by(categorias_pob) %>% 
  count() %>% 
  ggplot(aes(categorias_pob,n))+
  geom_bar(stat="identity")
#Categorizar la tasa de crecimiento de mayores, 3 grupos
c(-2.90,-0.11)#declive 
c(-0.10, 0.10)#Estabilidad
c(0.11,10.9)#Crecimiento
#Categorizar la tasa de crecimiento de mayores, 5 grupos
c(-2.90,-1.11)#declive moderado alto
c(-1.10,-0.11)#declive leve
c(-0.10, 0.10)#Estabilidad
c(0.11,3)#Crecimiento leve-moderado
c(3.01,10.9)#crecimiento alto-extremo
#Unir tabla de datos con la tabla de tasas de crecimiento de mayores por UEA
tab_cont_tam_uea_96_19 <- 
  categorias_envej_uea_96_19 %>% 
  select(uea,n_prom) %>% 
  inner_join(tasa_crecimiento,by="uea") %>% 
  filter(!uea=="Total") %>% 
  mutate(tc2_cat=case_when(
    tc2<(-1.11)~"Declive moderado-alto",
    between(tc2,-1.11,-0.11)~"Declive leve",
    between(tc2,-0.10,.10)~"Estabilidad",
    between(tc2,0.11,3)~"Crecimiento leve-moderado",
    tc2>3.01~"Crecimiento alto-extremo",
    TRUE~NA_character_)) 
#Densidad de la tasa de crecimiento de mayores en las categorias de poblacion
grafico_densidad_tam_tc2_uea <- 
tab_cont_tam_uea_96_19 %>% 
  ggplot(aes(tc2,fill=categorias_pob))+
  geom_density(alpha=0.4,colour="transparent")+
  scale_x_continuous(breaks=seq(-3,10,1.5))+
  theme_minimal()
#Tabla de contingencia
tabla_contingencia_tam <- 
  table(tab_cont_tam_uea_96_19$categorias_pob,
        tab_cont_tam_uea_96_19$tc2_cat)    

#p-valor del Chi cuadrado test
chisq.test(tabla_contingencia_tam)$p.value
chisq.test(tabla_contingencia_tam)$statistic
##CATEGORIAS ENVEJECIMIENTO en el valor medio, clusters----
#TABLA DESCRIPTIVA por grupos y tasas de crecimiento de mayores de 65 (TC65)----
#Media de la poblacion en cada uno de los grupos
tabla_descriptiva_tasas_medpob <- 
  categorias_envej_uea_96_19 %>% 
  inner_join(tasa_crecimiento$`Total AMBOS SEXOS`,by="uea") %>% 
  filter(!uea=="Total") %>% 
  mutate(tc2_cat=case_when(
    tc2<=(-1.11)~"Declive moderado-alto",
    between(tc2,-1.11,-0.11)~"Declive leve",
    between(tc2,-0.10,.10)~"Estabilidad",
    between(tc2,0.11,3)~"Crecimiento leve-moderado",
    tc2>(3)~"Crecimiento alto-extremo",
    TRUE~NA_character_)) %>% 
  categorias_envej_uea_96_19 %>% 
  pull(n_prom) %>% 
  sum()
  sum(.$n_prom)
  group_by(tc2_cat) %>% 
  summarise(n_prom=mean(n_prom,na.rm=TRUE)) %>% 
  spread(tc2_cat,n_prom) %>% 
  relocate(Estabilidad, .before=`Declive leve`) %>% 
  mutate(var="N_prom") %>% 
  select(var,everything())
#Sumatorio de la poblacion de cada grupo en los UEA en 2019
tabla_descriptiva_tasas_sumpob_2019<- 
  readxl::read_xlsx(ruta_envej_uea_96_19,sheet="2019") %>% 
  clean_names() %>% 
  select(uea,n_prom) %>% 
  mutate(categorias_pob=cut(n_prom,cortes_categorias_pob_muni,
                            labels=categorias_pob_muni,
                            include.lowest = TRUE))%>% 
  inner_join(tasa_crecimiento,by="uea") %>% 
  filter(!uea=="Total") %>% 
  mutate(tc2_cat=case_when(
    tc2<=(-1.11)~"Declive moderado-alto",
    between(tc2,-1.11,-0.11)~"Declive leve",
    between(tc2,-0.10,.10)~"Estabilidad",
    between(tc2,0.11,3)~"Crecimiento leve-moderado",
    tc2>(3)~"Crecimiento alto-extremo",
    TRUE~NA_character_)) %>% 
  group_by(tc2_cat) %>% 
  summarise(N_tot=sum(n_prom)) %>% 
  spread(tc2_cat,N_tot) %>% 
  relocate(Estabilidad, .before=`Declive leve`) %>% 
  mutate(var="N_tot_2019") %>% 
  select(var,everything())
#TABLA DESCIPTIVA FINAL
rbind(
tabla_descriptiva_tasas_medpob,
tabla_descriptiva_tasas_sumpob_2019) %>% 
  write.xlsx(.,"03_RESULTADOS/TABLAS/0.5.Tabla_Desc_Tipificación UEA_TC65.xlsx")

#TABLAS CONTINGENCIA----
#A----
#Unir tabla de datos con la tabla de tasas de crecimiento de mayores por UEA
tab_cont_envej_uea_96_16a <- 
  categorias_envej_uea_96_19 %>% 
  select(uea,n_prom) %>% 
  filter(uea!="Total")  %>% 
  mutate(cluster=den_datos_21$cluster) %>% 
  inner_join(tasa_crecimiento$`Total AMBOS SEXOS`,by="uea") %>% 
  filter(!uea=="Total") %>% 
  mutate(tc_cat=case_when(
    tc1<=(-1.11)~"Declive moderado-alto",
    between(tc1,-1.11,-0.11)~"Declive leve",
    between(tc1,-0.10,.10)~"Estabilidad",
    between(tc1,0.11,3)~"Crecimiento leve-moderado",
    tc1>(3)~"Crecimiento alto-extremo",
    TRUE~NA_character_))
#Tabla de contingencia
tabla_contingencia_envej_a <- 
  table(tab_cont_envej_uea_96_16a$tc2_cat,
        tab_cont_envej_uea_96_16a$cluster)   
#p-valor del Chi cuadrado test
chisq.test(tabla_contingencia_envej_a)$p.value
chisq.test(tabla_contingencia_envej_a)$statistic
#A2, este es el grupo escogido----
#Dos opciones:
#OPCION_A: todas las UEA sin Madrid y Barcelona
#OPCION_B: todas las UEA
#Funcion para calcular las tablas de contingencia entre los clusters anteriores
# y las clases de las tasas de crecimiento
tab_cont_envej_crec <- function(cat_mayores,opcion) {
  if(str_detect(cat_mayores,"mu_")){
    cat_mayores_crec <- str_replace_all(cat_mayores,c("mu_"=""))
    sexo <- "MUJERES"
  }else if(str_detect(cat_mayores,"h")){
    cat_mayores_crec <- str_replace_all(cat_mayores,c("h"=""))
    sexo <- "HOMBRES"
  }else{
    sexo <- "AMBOS SEXOS"
  }
#Nombre de la lista para escoger en las tasas de crecimiento
nombre_categoria_sexo_lista <- 
  paste(cat_mayores_crec,sexo) %>% 
  toupper()
#Tabla de tasas de crecimineto de mayores con las variables deseadas
tabla_tasas_crec <- 
  tasa_crecimiento[nombre_categoria_sexo_lista][[1]]
#Ubicar nombre de la categoria de mayores escogida
posicion_cat_mayores <- which(names(categorias_envej_uea_96_19)%in%
                                c("uea",cat_mayores))
if(opcion=="A"){
#Tabla de envejecimineto con las variables deseadas y los clusters
tabla_envejecimiento <- 
  categorias_envej_uea_96_19 [,posicion_cat_mayores] %>% 
  filter(uea!="Total") %>% 
  filter(!uea%in%c("28079",#Madrid
                   "8019"#Barcelona
  )) %>% 
  select(-2) %>% 
  mutate(cluster=den_datos_21b$cluster,
         cluster_nom=
           case_when(cluster==1~"Envejecimiento alto - Sobrenvejecimiento ligero",
                     cluster==2~"Envejecimiento moderado - Sobrenvejecimiento moderado",
                     cluster==3~"Joven",
                     cluster==4~"Envejecimiento muy alto",
                     cluster==5~"Envejecimiento moderado - Sobrenvejecimiento ligero",
                     TRUE~NA_character_)) 
#Unir tabla de datos con la tabla de tasas de crecimiento de mayores por UEA
pretab_cont_envej_tasas <- 
  tabla_envejecimiento   %>% 
  inner_join(tabla_tasas_crec,by="uea")
#Tabla de contingencia
tabla_contingencia <- 
  table(pretab_cont_envej_tasas$clases_tc1,
        pretab_cont_envej_tasas$cluster_nom) %>% 
  .[,c(4,1,3,2,5)]
}else if(opcion=="B"){
  tabla_envejecimiento <- 
    categorias_envej_uea_96_19 [,posicion_cat_mayores] %>% 
    filter(uea!="Total") %>% 
    filter(!uea%in%c("28079",#Madrid
                     "8019"#Barcelona
    )) %>% 
    select(-2) %>% 
    mutate(cluster=den_datos_21b$cluster,
           cluster_nom=
             case_when(cluster==1~"Envejecimiento alto - Sobrenvejecimiento ligero",
                       cluster==2~"Envejecimiento moderado - Sobrenvejecimiento moderado",
                       cluster==3~"Joven",
                       cluster==4~"Envejecimiento muy alto",
                       cluster==5~"Envejecimiento moderado - Sobrenvejecimiento ligero",
                       TRUE~NA_character_)) %>% 
    add_row(uea = c("28079","8019"), cluster=c(6,6),
    cluster_nom=rep("Envejecimiento alto - Sobrenvejecimiento moderado",2))

#Unir tabla de datos con la tabla de tasas de crecimiento de mayores por UEA
pretab_cont_envej_tasas <- 
  tabla_envejecimiento   %>% 
  inner_join(tabla_tasas_crec,by="uea")
#Tabla de contingencia
tabla_contingencia <- 
  table(pretab_cont_envej_tasas$clases_tc1,
        pretab_cont_envej_tasas$cluster_nom) %>% 
    .[,c(5,2,1,4,3,6)]
}
#p-valor del Chi cuadrado test
chi_cuadrado_valores <- 
  tibble(p_valor=
c(chisq.test(tabla_contingencia)$p.value,rep(NA,nrow(tabla_contingencia)-1)),
#Valor del estadistico
estadistico=
c(chisq.test(tabla_contingencia)$statistic,rep(NA,nrow(tabla_contingencia)-1))
)
#Tabla final
cbind(as.matrix(tabla_contingencia),
      as.matrix(chi_cuadrado_valores)) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(.,var=" ")
}
#OPCION_A
#Categorias de las que deseamos calcular las tablas de contingencia
cat_buscadas <- 
  c("m","hm","mu_m","mj","hmj","mu_mj","mm","hmm","mu_mm","ma","hma","mu_ma")
tablas_contingencia_a <- 
map(
  cat_buscadas,
  ~tab_cont_envej_crec(.x,opcion="A")) %>%
  setNames(toupper(cat_buscadas))
#GUARDAR TABLAS CONTINGENCIA
write.xlsx(tablas_contingencia_a,
                        file =   
                          paste0("03_RESULTADOS/TABLAS/",
                   "0.5.Tablas_Cont_Tipificación UEA_TC65_A2_VersionA.xlsx"),
)
#OPCION B
#Categorias de las que deseamos calcular las tablas de contingencia
cat_buscadas <- 
  c("m","hm","mu_m","mj","hmj","mu_mj","mm","hmm","mu_mm","ma","hma","mu_ma")
tablas_contingencia_b <- 
  map(
    cat_buscadas,
    ~tab_cont_envej_crec(.x,opcion="B")) %>%
  setNames(toupper(cat_buscadas))
#GUARDAR TABLAS CONTINGENCIA
write.xlsx(tablas_contingencia_b,
           file =   
             paste0("03_RESULTADOS/TABLAS/",
                    "0.5.Tablas_Cont_Tipificación UEA_TC65_A2_VersionB.xlsx"),
)
#Tabla de contingencia
tabla_contingencia_envej_a2 <- 
  tab_cont_envej_crec("m")[,1:5] %>% 
  as.table()
#p-valor del Chi cuadrado test
chisq.test(tabla_contingencia_envej_a2)$p.value
chisq.test(tabla_contingencia_envej_a2)$statistic
#B----
#Unir tabla de datos con la tabla de tasas de crecimiento de mayores por UEA
tab_cont_envej_uea_96_19b <- 
  categorias_envej_uea_96_19 %>% 
  select(uea,n_prom) %>% 
  filter(uea!="Total") %>% 
  mutate(cluster=den_datos_15$cluster) %>% 
  inner_join(tasa_crecimiento,by="uea") %>% 
  filter(!uea=="Total") %>% 
  mutate(tc2_cat=case_when(
    tc2<=(-1.11)~"Declive moderado-alto",
    between(tc2,-1.11,-0.11)~"Declive leve",
    between(tc2,-0.10,.10)~"Estabilidad",
    between(tc2,0.11,3)~"Crecimiento leve-moderado",
    tc2>(3)~"Crecimiento alto-extremo",
    TRUE~NA_character_))

#Tabla de contingencia
tabla_contingencia_envej_b <- 
  table(tab_cont_envej_uea_96_19b$tc2_cat,
        tab_cont_envej_uea_96_19b$cluster)   
#p-valor del Chi cuadrado test
chisq.test(tabla_contingencia_envej_b)$p.value
chisq.test(tabla_contingencia_envej_b)$statistic
#B2----
#Unir tabla de datos con la tabla de tasas de crecimiento de mayores por UEA
tab_cont_envej_uea_96_19b2 <- 
  categorias_envej_uea_96_19 %>% 
  filter(uea!="Total") %>% 
  mutate(cluster=den_datos_18$cluster) %>% 
  inner_join(tasa_crecimiento,by="uea") %>% 
  filter(!uea=="Total") %>% 
  mutate(tc2_cat=case_when(
    tc2<=(-1.11)~"Declive moderado-alto",
    between(tc2,-1.11,-0.11)~"Declive leve",
    between(tc2,-0.10,.10)~"Estabilidad",
    between(tc2,0.11,3)~"Crecimiento leve-moderado",
    tc2>(3)~"Crecimiento alto-extremo",
    TRUE~NA_character_))

#Tabla de contingencia
tabla_contingencia_envej_b2 <- 
  table(tab_cont_envej_uea_96_19b2$tc2_cat,
        tab_cont_envej_uea_96_19b2$cluster)   
#p-valor del Chi cuadrado test
chisq.test(tabla_contingencia_envej_b2)$p.value
chisq.test(tabla_contingencia_envej_b2)$statistic
#C----
#Unir tabla de datos con la tabla de tasas de crecimiento de mayores por UEA
tab_cont_envej_uea_96_19c <- 
  categorias_envej_uea_96_19 %>% 
  filter(uea!="Total") %>% 
  mutate(cluster=den_datos_12$cluster) %>% 
  inner_join(tasa_crecimiento,by="uea") %>% 
  filter(!uea=="Total") %>% 
  mutate(tc2_cat=case_when(
    tc2<=(-1.11)~"Declive moderado-alto",
    between(tc2,-1.11,-0.11)~"Declive leve",
    between(tc2,-0.10,.10)~"Estabilidad",
    between(tc2,0.11,3)~"Crecimiento leve-moderado",
    tc2>(3)~"Crecimiento alto-extremo",
    TRUE~NA_character_))
#Tabla de contingencia
tabla_contingencia_envej_c <- 
  table(tab_cont_envej_uea_96_19c$tc2_cat,
        tab_cont_envej_uea_96_19c$cluster)   
#p-valor del Chi cuadrado test
chisq.test(tabla_contingencia_envej_c)$p.value
chisq.test(tabla_contingencia_envej_c)$statistic
#GUARDAR DATOS EN RESULTADOS----
#Crear libro de Excel
excel_tablas_contingencia<- createWorkbook("tablas_descriptivas_clusters")
nombres_hojas_excel <- 
  c("Tamaño","Envejecimiento_A","Envejecimiento_A2",
    "Envejecimiento_B",
    "Envejecimiento_B2","Envejecimiento_C")
#Realizar las tablas descriptivas con la funcion "tabla_descriptiva_cluster"
tablas_contingencia_envej <- 
  list(tabla_contingencia_tam,
       tabla_contingencia_envej_a,tabla_contingencia_envej_a2,
       tabla_contingencia_envej_b,
            tabla_contingencia_envej_b2,tabla_contingencia_envej_c)

#Rellenar las hojas del libro de Excel
for(i in 1:length(nombres_hojas_excel)){
  addWorksheet(wb=excel_tablas_contingencia,
               sheetName = nombres_hojas_excel[i]
  )
  
  writeData(wb=excel_tablas_contingencia, x=tablas_contingencia_envej[[i]],
            sheet= nombres_hojas_excel[i],
            withFilter = FALSE,colNames = TRUE)
}
#GUARDAR EL LIBRO DE EXCEL
saveWorkbook(excel_tablas_contingencia,
             file =   
               paste0("03_RESULTADOS/TABLAS/",
              "0.5.Tablas_Cont_Tipificación UEA_Tamaño_Pob_Envej.xlsx"),
             overwrite = TRUE)


#ANHADIDOS----
#Sumar analisis descriptivo al caso de la matriz A2 en el anho 2019
#Datos de envejecimiento por uea 1996-2019 
envej_uea_19 <- 
  readxl::read_xlsx(ruta_envej_uea_96_19_prom,sheet="2019")
#conformar tabla de analisis
datos_2019=envej_uea_19
#A2
datos_2019_21b=select(datos_2019[-692,],UEA,PM,PMJ,PMM,PMA,PHM,PHMJ,PHMM,
                 PHMA,PMuM,PMuMJ,PMuMM,PMuMA,PMAM,PHMAM,
                 PMuMAM,N_prom,NH_prom,NMu_prom) %>% 
  #Eliminar Madrid y Barcelona
  filter(!UEA%in%c("28079",#Madrid
                   "8019")#Barcelona
  ) %>% 
  mutate(Grupo=den_datos_21b$cluster) %>% 
  select(UEA,Grupo,everything()) 
#Suma de las poblaciones medias de las UEA por Grupo en 2019
datos_2019_21b_sumagr <- 
datos_2019_21b %>% 
  group_by(Grupo) %>% 
  summarise(N_tot=sum(N_prom)) %>% 
  spread(Grupo,N_tot)
rowSums(datos_2019_21b_sumagr)
#MATRIZ A2 y TC65, tablas descriptivas en 2019
tabla_descriptiva_gr_a2_tc <- 
envej_uea_19 %>% 
inner_join(tasa_crecimiento,by=c("UEA"="uea")) %>% 
  filter(!UEA%in%
           c("Total","28079",#Madrid
             "8019"#Barcelona
  )) %>% 
  mutate(tc2_cat=case_when(
    tc2<=(-1.11)~"Declive moderado-alto",
    between(tc2,-1.11,-0.11)~"Declive leve",
    between(tc2,-0.10,.10)~"Estabilidad",
    between(tc2,0.11,3)~"Crecimiento leve-moderado",
    tc2>(3)~"Crecimiento alto-extremo",
    TRUE~NA_character_),
    grupo=den_datos_21b$cluster) %>% 
  select(UEA,tc2_cat,grupo,everything()) 

#Valores medios diferentes variables segun grupo y categorias de TC65

#POBLACION TOTAL
tabla_desc_pobtot <- 
tabla_descriptiva_gr_a2_tc %>% 
  group_by(tc2_cat,grupo) %>% 
  summarise(n_media=mean(N_prom,na.rm=TRUE)) %>% 
  spread(grupo,n_media) %>% 
  ungroup() %>% 
  mutate_at(vars(2:ncol(.)),~replace_na(.,0))
#POBLACION DE MAYORES
tabla_desc_pobmay <- 
tabla_descriptiva_gr_a2_tc %>% 
  group_by(tc2_cat,grupo) %>% 
  summarise(n_media=mean(M,na.rm=TRUE)) %>% 
  spread(grupo,n_media) %>% 
  ungroup() %>% 
  mutate_at(vars(2:ncol(.)),~replace_na(.,0))
#POBLACION DE MAYORES JOVENES
tabla_desc_pobmay_jov <- 
tabla_descriptiva_gr_a2_tc %>% 
  group_by(tc2_cat,grupo) %>% 
  summarise(n_media=mean(MJ,na.rm=TRUE)) %>% 
  spread(grupo,n_media) %>% 
  ungroup() %>% 
  mutate_at(vars(2:ncol(.)),~replace_na(.,0))
#POBLACION DE MAYORES DE EDAD INTERMEDIA
tabla_desc_pobmay_int <- 
tabla_descriptiva_gr_a2_tc %>% 
  group_by(tc2_cat,grupo) %>% 
  summarise(n_media=mean(MM,na.rm=TRUE)) %>% 
  spread(grupo,n_media) %>% 
  ungroup() %>% 
  mutate_at(vars(2:ncol(.)),~replace_na(.,0))
#POBLACION DE MAYORES DE EDAD AVANAZADA
tabla_desc_pobmay_av <- 
tabla_descriptiva_gr_a2_tc %>% 
  group_by(tc2_cat,grupo) %>% 
  summarise(n_media=mean(MA,na.rm=TRUE)) %>% 
  spread(grupo,n_media) %>% 
  ungroup() %>% 
  mutate_at(vars(2:ncol(.)),~replace_na(.,0))
#BOXPLOTS TASAS DE CRECIMIENTO

tasas_crecimiento_ <- 
tasa_crecimiento %>% 
  bind_rows() %>% 
  mutate(categoria=forcats::fct_relevel(as.factor(categoria),
                                        levels=c("M","MJ","MM","MA")),
         categoria=forcats::fct_recode(categoria,
                                       "Mayores"="M","Mayores jóvenes"="MJ",
                                       "Mayores de edad \n intermedia"="MM",
                                       "Mayores de edad \n avanzada"="MA"),
         sexo=forcats::fct_relevel(as.factor(sexo),
                                   levels=c("MUJERES","HOMBRES","AMBOS SEXOS"))) 


tasas_crecimiento_%>% 
  ggplot(.,aes(sexo,tc1,alpha=tc1,colour=sexo))+
  geom_jitter()+
  geom_boxplot(fill="grey90",alpha=0.5,colour="black")+
  scale_colour_manual(values=c("#33c8bdff","#f6c34bff","grey90"))+
  facet_wrap(~categoria,nrow=1)+
  scale_y_continuous(breaks=seq(-4,14,1))+
  theme_minimal()+
  labs(y=expression(TC["65"]~italic("(Tasa de crecimiento de mayores)")),
       x="")+
  theme(legend.position = "none",
        strip.text = element_text(size=12,colour="grey70"))

tasas_crecimiento_%>% 
  ggplot(.,aes(sexo,tc1,alpha=tc1,fill=sexo))+
  geom_boxplot(colour="black")+
  scale_fill_manual(values=c("#33c8bdff","#f6c34bff","grey90"))+
  facet_wrap(~categoria,nrow=1)+
  scale_y_continuous(breaks=seq(-4,14,1))+
  theme_minimal()+
  labs(y=expression(TC["65"]~italic("(Tasa de crecimiento de mayores)")),
       x="")+
  theme(legend.position = "none",
        strip.text = element_text(size=12,colour="black"))

tasas_crecimiento_%>% 
  ggplot(.,aes(categoria,tc1,alpha=tc1,colour=sexo))+
  geom_jitter()+
  geom_boxplot(fill="grey90",alpha=0.5,colour="black")+
  scale_colour_manual(values=c("grey90","#f6c34bff","#33c8bdff"))+
  scale_y_continuous(breaks=seq(-4,14,1))+
  facet_wrap(~sexo,nrow=1)+
  theme_minimal()+
  labs(y=expression(TC["65"]~italic("(Tasa de crecimiento de mayores)")),
       x="")+
  theme(legend.position = "none",
        strip.text = element_text(size=12,colour="grey70"))

azul <- "#33c8bdff"
naranja <- "#f6c34bff"

tasas_crecimiento_%>% 
  ggplot(.,aes(categoria,tc1,fill=sexo))+
  geom_boxplot(colour="black",alpha=0.8)+
  scale_fill_manual(values=c("#33c8bdff","#f6c34bff","grey90"))+
  scale_y_continuous(breaks=seq(-4,14,1))+
  facet_wrap(~sexo,nrow=1)+
  theme_minimal()+
  labs(y=expression(TC["65"]~italic("(Tasa de crecimiento de mayores)")),
       x="")+
  theme(legend.position = "none",
        strip.text = element_text(size=12,colour="black"))
#GUARDAR DATOS EN RESULTADOS----
#Crear libro de Excel
excel_tablas_descriptivas<- createWorkbook("tablas_descriptivas_envejecimiento")
nombres_hojas_excel <- 
  c("POB TOTAL","POB MAYORES","POB MAYORES JOVENES",
    "POB MAYORES INT",
    "POB MAYORES AVANZ")
#Realizar las tablas descriptivas con la funcion "tabla_descriptiva_cluster"
tablas_descriptivas_envej <- 
  list(tabla_desc_pobtot,
       tabla_desc_pobmay,tabla_desc_pobmay_jov,
       tabla_desc_pobmay_int,
       tabla_desc_pobmay_av)

#Rellenar las hojas del libro de Excel
for(i in 1:length(nombres_hojas_excel)){
  addWorksheet(wb=excel_tablas_descriptivas,
               sheetName = nombres_hojas_excel[i]
  )
  
  writeData(wb=excel_tablas_descriptivas, x=tablas_descriptivas_envej[[i]],
            sheet= nombres_hojas_excel[i],
            withFilter = FALSE,colNames = TRUE)
}
#GUARDAR EL LIBRO DE EXCEL
saveWorkbook(excel_tablas_descriptivas,
             file =   
               paste0("03_RESULTADOS/TABLAS/",
                      "0.5.Tablas_Desc_Tipificación UEA_Envej_tc65.xlsx"),
             overwrite = TRUE)

tabla_descriptiva_gr_a2_tc %>% 
  select(tc2_cat,grupo,M,MJ,MM,MA) %>% 
  gather("var","val",M:MA) %>% 
  ggplot(aes(var,val))+
  geom_boxplot()+
  facet_wrap(tc2_cat~grupo,scales="free")
