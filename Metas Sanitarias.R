#Metas Sanitarias

# Librerias ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(dplyr)
library(openxlsx)
library(xlsx)


#Fechas para filtrar las BBDD
anio <- "2021-01-01"
Horas_Programadas_No_medicos <- 8652
Horas_Programadas_medicos <- 72012
Horas_Programadas_Telemedicina <- 7644 #son con la programacion de febrero


# 18834 -------------------------------------------------------------------


#Programción
A07_nomed <- read_excel("BBDD Produccion/Ambulatorio/A06 BBDD.xlsx")
A07_nomed$Fecha=as.character(A07_nomed$Fecha)
MS18834 <- A07_nomed %>% 
  filter(Fecha >= anio & REM == "A07") %>% 
  group_by(Fecha) %>% 
  summarise(Numerador = sum(Total)) %>% 
  mutate(Denominador = round(Horas_Programadas_No_medicos/12), indicador = "Programación Profesionales No Médicos") %>% 
  select(indicador, Fecha, Numerador, Denominador)


#ESI
A08_BBDD_02 <- read_excel("BBDD Produccion/Urgencia/A08 BBDD_02.xlsx")
A08_BBDD_02$Fecha=as.character(A08_BBDD_02$Fecha)
A08_BBDD_02 <- A08_BBDD_02 %>% 
  filter(Fecha >= anio) %>% 
  group_by(Fecha) %>% 
  summarise(Numerador = sum(Total)) %>% 
  mutate(indicador = "Categorización ESI") %>% 
  select(indicador, Fecha, Numerador)


A08_BBDD_01 <- read_excel("BBDD Produccion/Urgencia/A08 BBDD_01.xlsx")
A08_BBDD_01$Fecha=as.character(A08_BBDD_01$Fecha)
A08_BBDD_01 <- A08_BBDD_01 %>% 
  filter(Fecha >= anio) %>% 
  group_by(Fecha) %>% 
  summarise(Denominador = sum(Total)) %>% 
  select(Denominador)

A08_BBDD_01 <- cbind(A08_BBDD_02, A08_BBDD_01)
MS18834 <- rbind(MS18834, A08_BBDD_01)

#Indicación Hospitalización

urgencia <- read_excel("BBDD Produccion/Urgencia/A08 BBDD_04.xlsx")
urgencia$Fecha=as.character(urgencia$Fecha)
urgencia1 <- urgencia %>% 
  filter(Fecha >= anio & `Categoria pacientes` == "MENOS DE 12 HORAS") %>%
  group_by(Fecha) %>% 
  summarise("Numerador" = sum(Total)) %>% 
  mutate(indicador = "Indicación Hospitalización desde UEH") %>% 
  select(indicador, Fecha, Numerador)

urgencia2 <- urgencia %>% 
  filter(Fecha >= anio) %>% 
  filter(`Categoria pacientes` == "MENOS DE 12 HORAS" |`Categoria pacientes` == "12-24 HORAS"
         |`Categoria pacientes` == "MAYOR A 24 HORAS" | `Categoria pacientes` == "PACIENTES QUE PERMANECEN EN UEH") %>%
  group_by(Fecha) %>% 
  summarise("Denominador" = sum(Total)) %>% 
  select(Denominador)


urgencia <- cbind(urgencia1, urgencia2)
MS18834 <- rbind(MS18834, urgencia)

MS19664 <- urgencia


# 19664 -------------------------------------------------------------------

#Suspensiones
suspensiones <- read_excel("BBDD Produccion/Quirurgico/A21_3 BBDD.xlsx")
suspensiones$Fecha=as.character(suspensiones$Fecha)

suspensiones <- suspensiones %>% filter(Fecha >= anio) %>% 
  select(Fecha, Especialidad, `Pacientes Suspendidos`, `Pacientes Programados`) %>% 
  group_by(Fecha) %>% 
  summarise("Numerador" = sum(`Pacientes Suspendidos`), 
            "Denominador" = sum(`Pacientes Programados`)) %>% 
  mutate(indicador = "Suspensiones") %>% 
  select(indicador, Fecha, Numerador, Denominador)

MS19664 <- rbind(MS19664, suspensiones)

#Altas odontologicas
odo <- read_excel("BBDD Produccion/Ambulatorio/A09 BBDD_03.xlsx")
odo$Fecha=as.character(odo$Fecha)

altas_odo <- odo %>% filter(Fecha >= anio & `TIPO DE INGRESO O EGRESO` == "ALTAS DE TRATAMIENTO") %>% 
  group_by(Fecha) %>% 
  summarise("Numerador" = sum(Total)) %>% 
  mutate(indicador = "altas odontologicas") %>% 
  select(indicador, Fecha, Numerador)

ingreso_odo <- odo %>% filter(Fecha >= anio & `TIPO DE INGRESO O EGRESO` == "INGRESOS A TRATAMIENTO") %>% 
  group_by(Fecha) %>% 
  summarise("Denominador" = sum(Total)) %>% 
  select(Denominador)

altas_odo <- cbind(altas_odo, ingreso_odo)
MS19664 <- rbind(MS19664, altas_odo)

#Programación
cons_amb <- read_excel("BBDD Produccion/Ambulatorio/A07 BBDD.xlsx")
cons_amb$Fecha=as.character(cons_amb$Fecha)

cons_amb <- cons_amb %>% 
  filter(Fecha >= anio) %>% 
  group_by(Fecha) %>% 
  summarise("Numerador" = sum(Total))  %>%
  mutate(indicador = "Prog Consultas", Denominador = round(Horas_Programadas_medicos/12)) %>%           
  select(indicador, Fecha, Numerador, Denominador)


#Telemedicina

telemedicina <- read_excel("BBDD Produccion/Ambulatorio/A30 BBDD.xlsx")
telemedicina$Fecha=as.character(telemedicina$Fecha)

telemedicina <- telemedicina %>% 
  filter(Fecha >= anio) %>% 
  group_by(Fecha) %>% 
  summarise("Numerador" = sum(Telemedicina_Nueva)+ sum(Telemedicina_Control),+sum(Telemedicina_Hospitalizados)) %>% 
  mutate(indicador = "Prog Telemedicina", Denominador = round(Horas_Programadas_Telemedicina/12)) %>% 
  select(indicador, Fecha, Numerador, Denominador)

MS19664 <- rbind(MS19664, cons_amb, telemedicina)


#Pivoteo


MS188342 <- MS18834 %>% pivot_wider(indicador, names_from = Fecha, values_from = Numerador) %>% 
  add_column(Variable = "Numerador", .after = 1)
MS188341 <- MS18834 %>% pivot_wider(indicador, names_from = Fecha, values_from = Denominador) %>% 
  add_column(Variable = "Denominador", .after = 1)

MS18834 <-rbind(MS188342, MS188341)
MS18834 <- MS18834 %>% arrange(indicador)


MS196642 <- MS19664 %>% pivot_wider(indicador, names_from = Fecha, values_from = Numerador) %>% 
  add_column(Variable = "Numerador", .after = 1)
MS196641 <- MS19664 %>% pivot_wider(indicador, names_from = Fecha, values_from = Denominador) %>% 
  add_column(Variable = "Denominador", .after = 1)

MS19664 <-rbind(MS196642, MS196641)
MS19664 <- MS19664 %>% arrange(indicador)


openxlsx::write.xlsx(MS18834, "BBDD Produccion/Indicadores/2021/Metas Sanitarias/BBDD18834.xlsx", colNames = TRUE, sheetName = "18834", overwrite = T)
openxlsx::write.xlsx(MS19664, "BBDD Produccion/Indicadores/2021/Metas Sanitarias/BBDD19664.xlsx", colNames = TRUE, sheetName = "19664", overwrite = T)

rm(A07_nomed, A08_BBDD_01, A08_BBDD_02, urgencia, urgencia1, urgencia2, altas_odo, cons_amb, ingreso_odo, odo, 
   suspensiones, telemedicina, anio, Horas_Programadas_medicos, Horas_Programadas_No_medicos, 
   Horas_Programadas_Telemedicina, MS196641, MS196642, MS188341, MS188342, MS18834, MS19664)






