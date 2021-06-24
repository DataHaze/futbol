# dat = Results for ALL countries, home and away
# r1 = Mean results for home and away, for ALL countries
# local_2, visit_2 = mean for only two teams for each country, home and away. 
# visit country_a = First semester (only in Latin America)
# visit country_b = Second semester (only in Latin America)
#  agregar temporada 2020/21 a los sudamericanos
#  agregar temorara 14/15 en europeos


# libraries -----------------
library(tidyverse)
library(rvest)
library(xml2)
library(ggthemes)
library(ggrepel)
library(matrixStats)
library(gridExtra)
library(broom)
library(caret)

options(digits=3)
setwd('C:/Users/jgarc/Documents/R/futbol')

#define table_maker function which returns tables --------------
table_maker<-function(url){
  web<-read_html(url)
  node<-html_nodes(web, 'table')
  tab<-html_table(node[[5]])
  names(tab)<-c('Pos', 'Equipo', 'J', 'G','E','P','GF','GC','Dif','PTS', 'Ef')
  tab<-tab[-1,]
  tab<-tab %>% mutate_at(3:10, as.numeric)
  return (tab)
}

# So, I start passing all the websites containing the results I want through my function, "table_maker". This may look cumbersome, but It's a preety
# straight forward process once you get used to Its gramatic. 

# Argentina----------------------------------------------------------------------------
#argentina 2021/22
url_local_ar21<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=1&camp=1465'
url_visit_ar21<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=1&camp=1465'
local_ar21<-table_maker(url_local_ar21)
visit_ar21<-table_maker(url_visit_ar21)

#argentina 2020/21
url_local_ar20<-'http://www.universofutbol.com.ar/plantillas/archivos/posiciones.php?div=1&camp=1355'
url_visit_ar20<-'http://www.universofutbol.com.ar/plantillas/archivos/posiciones.php?div=1&camp=1355'
local_ar20<-table_maker(url_local_ar20)
visit_ar20<-table_maker(url_visit_ar20)


#argentina 2019/20
url_local_ar<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=1&camp=1444'
url_visit_ar<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=1&camp=1444'
local_ar<-table_maker(url_local_ar)
visit_ar<-table_maker(url_visit_ar)

#argentina 2018/19
url_local_ar18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=1&camp=1279'
url_visit_ar18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=1&camp=1279'
local_ar18<-table_maker(url_local_ar18)
visit_ar18<-table_maker(url_visit_ar18)


#argentina 2017/18
url_local_ar17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=1&camp=1213'
url_visit_ar17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=1&camp=1213'     
local_ar17<-table_maker(url_local_ar17)
visit_ar17<-table_maker(url_visit_ar17)

#argentina 2016/17
url_local_ar16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=1&camp=1093'
url_visit_ar16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=1&camp=1093'     
local_ar16<-table_maker(url_local_ar16)
visit_ar16<-table_maker(url_visit_ar16)


#argentina 2015/16
url_local_ar15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=1&camp=945'
url_visit_ar15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=1&camp=945' 
local_ar15<-table_maker(url_local_ar15)
visit_ar15<-table_maker(url_visit_ar15)

# argentina 2014/15
url_local_ar14_a<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=1&camp=896'
url_visit_ar14_a<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=1&camp=896'
local_ar14_a<-table_maker(url_local_ar14_a)
visit_ar14_a<-table_maker(url_visit_ar14_a)


url_local_ar14_b<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=1&camp=856'
url_visit_ar14_b<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=1&camp=856'
local_ar14_b<-table_maker(url_local_ar14_b)
visit_ar14_b<-table_maker(url_visit_ar14_b)

local_ar14<-bind_rows(local_ar14_a, local_ar14_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

local_ar14<-local_ar14[duplicated(local_ar14$Equipo),]


visit_ar14<-bind_rows(visit_ar14_a, visit_ar14_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

visit_ar14<-visit_ar14[duplicated(visit_ar14$Equipo),]

# Brasil------------------------------------------------------

#brasil 2020/21
url_local_br20<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=23&camp=1411'
url_visit_br20<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=23&camp=1411'
local_br20<-table_maker(url_local_br20)
visit_br20<-table_maker(url_visit_br20)

#brasil 2019/20
url_local_br<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=23&camp=1345'
url_visit_br<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=23&camp=1345'
local_br<-table_maker(url_local_br)
visit_br<-table_maker(url_visit_br)


#brasil 2018/19
url_local_br18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=23&camp=1251'
url_visit_br18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=23&camp=1251'
local_br18<-table_maker(url_local_br18)
visit_br18<-table_maker(url_visit_br18)

#brasil 2017/18
url_local_br17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=23&camp=1171'
url_visit_br17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=23&camp=1171'
local_br17<-table_maker(url_local_br17)
visit_br17<-table_maker(url_visit_br17)

#brasil 2016/17
url_local_br16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=23&camp=1083'
url_visit_br16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=23&camp=1083'
local_br16<-table_maker(url_local_br16)
visit_br16<-table_maker(url_visit_br16)

#brasil 2015/16
url_local_br15<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=23&camp=998'
url_visit_br15<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=23&camp=998'
local_br15<-table_maker(url_local_br15)
visit_br15<-table_maker(url_visit_br15)

#brasil 2015/16
url_local_br14<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=23&camp=881'
url_visit_br14<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=23&camp=881'
local_br14<-table_maker(url_local_br14)
visit_br14<-table_maker(url_visit_br14)

# Chile----------------------------------------------

#chile 2020/21
url_local_ch20<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=28&camp=1480'
url_visit_ch20<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=28&camp=1480'
local_ch20<-table_maker(url_local_ch20)
visit_ch20<-table_maker(url_visit_ch20)


#chile 2019/20
url_local_ch<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=28&camp=1399'
url_visit_ch<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=28&camp=1399'
local_ch<-table_maker(url_local_ch)
visit_ch<-table_maker(url_visit_ch)


#chile 2018/19
url_local_ch18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=28&camp=1241'
url_visit_ch18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=28&camp=1241'
local_ch18<-table_maker(url_local_ch18)
visit_ch18<-table_maker(url_visit_ch18)


#chile 2017/18
url_local_ch17_a<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=28&camp=1160'
url_visit_ch17_a<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=28&camp=1160'
local_ch17_a<-table_maker(url_local_ch17_a)
visit_ch17_a<-table_maker(url_visit_ch17_a)


url_local_ch17_b<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=28&camp=1192'
url_visit_ch17_b<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=28&camp=1192'
local_ch17_b<-table_maker(url_local_ch17_b)
visit_ch17_b<-table_maker(url_visit_ch17_b)

local_ch17<-bind_rows(local_ch17_a, local_ch17_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

local_ch17<-local_ch17[duplicated(local_ch17$Equipo),]

visit_ch17<-bind_rows(visit_ch17_a, visit_ch17_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

visit_ch17<-visit_ch17[duplicated(visit_ch17$Equipo),]

#chile 2016/17
url_local_ch16_a<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=28&camp=1102'
url_visit_ch16_a<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=28&camp=1102'
local_ch16_a<-table_maker(url_local_ch16_a)
visit_ch16_a<-table_maker(url_visit_ch16_a)


url_local_ch16_b<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=28&camp=1064'
url_visit_ch16_b<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=28&camp=1064'
local_ch16_b<-table_maker(url_local_ch16_b)
visit_ch16_b<-table_maker(url_visit_ch16_b)

local_ch16<-bind_rows(local_ch16_a, local_ch16_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

local_ch16<-local_ch16[duplicated(local_ch16$Equipo),]

visit_ch16<-bind_rows(visit_ch16_a, visit_ch16_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

visit_ch16<-visit_ch16[duplicated(visit_ch16$Equipo),]

#chile 2015/16
url_local_ch15_a<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=28&camp=962'
url_visit_ch15_a<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=28&camp=962'
local_ch15_a<-table_maker(url_local_ch15_a)
visit_ch15_a<-table_maker(url_visit_ch15_a)

url_local_ch15_b<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=28&camp=1016'
url_visit_ch15_b<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=28&camp=1016'
local_ch15_b<-table_maker(url_local_ch15_b)
visit_ch15_b<-table_maker(url_visit_ch15_b)

local_ch15<-bind_rows(local_ch15_a, local_ch15_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

local_ch15<-local_ch15[duplicated(local_ch15$Equipo),]

visit_ch15<-bind_rows(visit_ch15_a, visit_ch15_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

visit_ch15<-visit_ch15[duplicated(visit_ch15$Equipo),]

#chile 2015/16
url_local_ch14_a<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=28&camp=908'
url_visit_ch14_a<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=28&camp=908'
local_ch14_a<-table_maker(url_local_ch14_a)
visit_ch14_a<-table_maker(url_visit_ch14_a)

url_local_ch14_b<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=28&camp=824'
url_visit_ch14_b<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=28&camp=824'
local_ch14_b<-table_maker(url_local_ch14_b)
visit_ch14_b<-table_maker(url_visit_ch14_b)

local_ch14<-bind_rows(local_ch14_a, local_ch14_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

local_ch14<-local_ch14[duplicated(local_ch14$Equipo),]

visit_ch14<-bind_rows(visit_ch14_a, visit_ch14_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

visit_ch14<-visit_ch14[duplicated(visit_ch14$Equipo),]
# Uruguay ---------------------------------------
#uruguay 2020/21
url_local_ur20<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=33&camp=1461'
url_visit_ur20<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=33&camp=1461'
local_ur20<-table_maker(url_local_ur20)
visit_ur20<-table_maker(url_visit_ur20)


#uruguay 2019/20
url_local_ur<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=33&camp=1443'
url_visit_ur<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=33&camp=1443'
local_ur<-table_maker(url_local_ur)
visit_ur<-table_maker(url_visit_ur)

#uruguay 2018/19
url_local_ur18<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=33&camp=1247'
url_visit_ur18<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=33&camp=1247'
local_ur18<-table_maker(url_local_ur18)
visit_ur18<-table_maker(url_visit_ur18)

#uruguay 2017/18
url_local_ur17_a<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=33&camp=1161'
url_visit_ur17_a<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=33&camp=1161'
local_ur17_a<-table_maker(url_local_ur17_a)
visit_ur17_a<-table_maker(url_visit_ur17_a)

url_local_ur17_b<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=33&camp=1223'
url_visit_ur17_b<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=33&camp=1223'
local_ur17_b<-table_maker(url_local_ur17_b)
visit_ur17_b<-table_maker(url_visit_ur17_b)

local_ur17<-bind_rows(local_ur17_a, local_ur17_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

local_ur17<-local_ur17[duplicated(local_ur17$Equipo),]


visit_ur17<-bind_rows(visit_ur17_a, visit_ur17_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

visit_ur17<-visit_ur17[duplicated(visit_ur17$Equipo),]

#uruguay 2016/17
url_local_ur16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=33&camp=1078'
url_visit_ur16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=33&camp=1078'
local_ur16<-table_maker(url_local_ur16)
visit_ur16<-table_maker(url_visit_ur16)


#uruguay 2015/16
url_local_ur15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=33&camp=970'
url_visit_ur15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=33&camp=970'
local_ur15<-table_maker(url_local_ur15)
visit_ur15<-table_maker(url_visit_ur15)

#uruguay 2014/15
url_local_ur14_a<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=33&camp=934'
url_visit_ur14_a<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=33&camp=934'
local_ur14_a<-table_maker(url_local_ur14_a)
visit_ur14_a<-table_maker(url_visit_ur14_a)

url_local_ur14_b<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=33&camp=871'
url_visit_ur14_b<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=33&camp=871'
local_ur14_b<-table_maker(url_local_ur14_b)
visit_ur14_b<-table_maker(url_visit_ur14_b)

local_ur14<-bind_rows(local_ur14_a, local_ur14_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

local_ur14<-local_ur14[duplicated(local_ur14$Equipo),]


visit_ur14<-bind_rows(visit_ur14_a, visit_ur14_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

visit_ur14<-visit_ur14[duplicated(visit_ur14$Equipo),]

# Paraguay -------------------------------------------

#paraguay 2019/20
url_local_pr20<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=34&camp=1463'
url_visit_pr20<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=34&camp=1463'
local_pr20<-table_maker(url_local_pr20)
visit_pr20<-table_maker(url_visit_pr20)


#paraguay 2019/20
url_local_pr<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=34&camp=1440'
url_visit_pr<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=34&camp=1440'
local_pr<-table_maker(url_local_pr)
visit_pr<-table_maker(url_visit_pr)


#paraguay 2018/19
url_local_pr18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=34&camp=1232'
url_visit_pr18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=34&camp=1232'
local_pr18<-table_maker(url_local_pr18)
visit_pr18<-table_maker(url_visit_pr18)


#paraguay 2017/18
url_local_pr17_a<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=34&camp=1146'
url_visit_pr17_a<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=34&camp=1146'
local_pr17_a<-table_maker(url_local_pr17_a)
visit_pr17_a<-table_maker(url_visit_pr17_a)

url_local_pr17_b<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=34&camp=1193'
url_visit_pr17_b<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=34&camp=1193'
local_pr17_b<-table_maker(url_local_pr17_b)
visit_pr17_b<-table_maker(url_visit_pr17_b)

local_pr17<-bind_rows(local_pr17_a, local_pr17_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

local_pr17<-local_pr17[duplicated(local_pr17$Equipo),]


visit_pr17<-bind_rows(visit_pr17_a, visit_pr17_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

visit_pr17<-visit_pr17[duplicated(visit_pr17$Equipo),]


#paraguay 2016/17
url_local_pr16_a<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=34&camp=1066'
url_visit_pr16_a<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=34&camp=1066'
local_pr16_a<-table_maker(url_local_pr16_a)
visit_pr16_a<-table_maker(url_visit_pr16_a)

url_local_pr16_b<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=34&camp=1101'
url_visit_pr16_b<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=34&camp=1101'
local_pr16_b<-table_maker(url_local_pr16_b)
visit_pr16_b<-table_maker(url_visit_pr16_b)

local_pr16<-bind_rows(local_pr16_a, local_pr16_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

local_pr16<-local_pr16[duplicated(local_pr16$Equipo),]


visit_pr16<-bind_rows(visit_pr16_a, visit_pr16_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

visit_pr16<-visit_pr16[duplicated(visit_pr16$Equipo),]

#paraguay 2015/16
url_local_pr15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=34&camp=1010'
url_visit_pr15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=34&camp=1010'
local_pr15<-table_maker(url_local_pr15)
visit_pr15<-table_maker(url_visit_pr15)

#paraguay 2014/15
url_local_pr14_a<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=34&camp=869'
url_visit_pr14_a<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=34&camp=869'
local_pr14_a<-table_maker(url_local_pr14_a)
visit_pr14_a<-table_maker(url_visit_pr14_a)

url_local_pr14_b<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=34&camp=910'
url_visit_pr14_b<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=34&camp=910'
local_pr14_b<-table_maker(url_local_pr14_b)
visit_pr14_b<-table_maker(url_visit_pr14_b)

local_pr14<-bind_rows(local_pr14_a, local_pr14_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

local_pr14<-local_pr14[duplicated(local_pr14$Equipo),]


visit_pr14<-bind_rows(visit_pr14_a, visit_pr14_b) %>%
  group_by(Equipo) %>% 
  mutate_at(3:10, sum) %>%
  arrange(desc(PTS))

visit_pr14<-visit_pr14[duplicated(visit_pr14$Equipo),]

# Alemania --------------------
#alemania 2020/21   ano = 2021
url_local_al<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=11&camp=1425'
url_visit_al<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=11&camp=1425'
local_al<-table_maker(url_local_al)
visit_al<-table_maker(url_visit_al)

#alemania 2019/20   ano = 2020
url_local_al19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=11&camp=1354'
url_visit_al19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=11&camp=1354'
local_al19<-table_maker(url_local_al19)
visit_al19<-table_maker(url_visit_al19)


#alemania 2018/19
url_local_al18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=11&camp=1297'
url_visit_al18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=11&camp=1297'
local_al18<-table_maker(url_local_al18)
visit_al18<-table_maker(url_visit_al18)

#alemania 2017/18
url_local_al17<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=11&camp=1202'
url_visit_al17<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=11&camp=1202'
local_al17<-table_maker(url_local_al17)
visit_al17<-table_maker(url_visit_al17)

#alemania 2016/17
url_local_al16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=11&camp=1099'
url_visit_al16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=11&camp=1099'
local_al16<-table_maker(url_local_al16)
visit_al16<-table_maker(url_visit_al16)

#alemania 2015/16
url_local_al15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=11&camp=1031'
url_visit_al15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=11&camp=1031'
local_al15<-table_maker(url_local_al15)
visit_al15<-table_maker(url_visit_al15)

#alemania 2014/15
url_local_al14<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=11&camp=911'
url_visit_al14<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=11&camp=911'
local_al14<-table_maker(url_local_al14)
visit_al14<-table_maker(url_visit_al14)


# Espana -----------------------------------
#espana 2020/21
url_local_es<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=14&camp=1431'
url_visit_es<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=14&camp=1431'
local_es<-table_maker(url_local_es)
visit_es<-table_maker(url_visit_es)

#espana 2019/20
url_local_es19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=14&camp=1366'
url_visit_es19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=14&camp=1366'
local_es19<-table_maker(url_local_es19)
visit_es19<-table_maker(url_visit_es19)

#espana 2018/19
url_local_es18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=14&camp=1301'
url_visit_es18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=14&camp=1301'
local_es18<-table_maker(url_local_es18)
visit_es18<-table_maker(url_visit_es18)

#espana 2017/18
url_local_es17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=14&camp=1205'
url_visit_es17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=14&camp=1205'
local_es17<-table_maker(url_local_es17)
visit_es17<-table_maker(url_visit_es17)

#espana 2016/17
url_local_es16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=14&camp=1110'
url_visit_es16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=14&camp=1110'
local_es16<-table_maker(url_local_es16)
visit_es16<-table_maker(url_visit_es16)

#espana 2015/16
url_local_es15<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=14&camp=1032'
url_visit_es15<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=14&camp=1032'
local_es15<-table_maker(url_local_es15)
visit_es15<-table_maker(url_visit_es15)

#espana 2014/15
url_local_es14<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=14&camp=926'
url_visit_es14<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=14&camp=926'
local_es14<-table_maker(url_local_es14)
visit_es14<-table_maker(url_visit_es14)

# Francia ----------------------------------
#francia 2020/21
url_local_fr<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=15&camp=1418'
url_visit_fr<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=15&camp=1418'
local_fr<-table_maker(url_local_fr)
visit_fr<-table_maker(url_visit_fr)

#francia 2019/20
url_local_fr19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=15&camp=1375'
url_visit_fr19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=15&camp=1375'
local_fr19<-table_maker(url_local_fr19)
visit_fr19<-table_maker(url_visit_fr19)


#francia 2018/19
url_local_fr18<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=15&camp=1278'
url_visit_fr18<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=15&camp=1278'
local_fr18<-table_maker(url_local_fr18)
visit_fr18<-table_maker(url_visit_fr18)

#francia 2017/18
url_local_fr17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=15&camp=1203'
url_visit_fr17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=15&camp=1203'
local_fr17<-table_maker(url_local_fr17)
visit_fr17<-table_maker(url_visit_fr17)

#francia 2016/17
url_local_fr16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=15&camp=1107'
url_visit_fr16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=15&camp=1107'
local_fr16<-table_maker(url_local_fr16)
visit_fr16<-table_maker(url_visit_fr16)

#francia 2015/16
url_local_fr15<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=15&camp=1024'
url_visit_fr15<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=15&camp=1024'
local_fr15<-table_maker(url_local_fr15)
visit_fr15<-table_maker(url_visit_fr15)

#francia 2014/15
url_local_fr14<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=15&camp=923'
url_visit_fr14<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=15&camp=923'
local_fr14<-table_maker(url_local_fr14)
visit_fr14<-table_maker(url_visit_fr14)

# Inglaterra ---------------------------------
#inglaterra 2020/21
url_local_ing<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=17&camp=1428'
url_visit_ing<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=17&camp=1428'
local_ing<-table_maker(url_local_ing)
visit_ing<-table_maker(url_visit_ing)

#inglaterra 2019/20
url_local_ing19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=17&camp=1352'
url_visit_ing19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=17&camp=1352'
local_ing19<-table_maker(url_local_ing19)
visit_ing19<-table_maker(url_visit_ing19)

#inglaterra 2018/19
url_local_ing18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=17&camp=1284'
url_visit_ing18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=17&camp=1284'
local_ing18<-table_maker(url_local_ing18)
visit_ing18<-table_maker(url_visit_ing18)

#inglaterra 2017/18
url_local_ing17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=17&camp=1185'
url_visit_ing17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=17&camp=1185'
local_ing17<-table_maker(url_local_ing17)
visit_ing17<-table_maker(url_visit_ing17)

#inglaterra 2016/17
url_local_ing16<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=17&camp=1098'
url_visit_ing16<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=17&camp=1098'
local_ing16<-table_maker(url_local_ing16)
visit_ing16<-table_maker(url_visit_ing16)

#inglaterra 2015/16
url_local_ing15<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=17&camp=1025'
url_visit_ing15<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=17&camp=1025'
local_ing15<-table_maker(url_local_ing15)
visit_ing15<-table_maker(url_visit_ing15)

#inglaterra 2015/16
url_local_ing14<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=17&camp=907'
url_visit_ing14<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=17&camp=907'
local_ing14<-table_maker(url_local_ing14)
visit_ing14<-table_maker(url_visit_ing14)

# Italia ----------------------------
#italia 2020/21
url_local_it<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=9&camp=1434'
url_visit_it<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=9&camp=1434'
local_it<-table_maker(url_local_it)
visit_it<-table_maker(url_visit_it)

#italia 2019/20
url_local_it19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=9&camp=1373'
url_visit_it19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=9&camp=1373'
local_it19<-table_maker(url_local_it19)
visit_it19<-table_maker(url_visit_it19)

#italia 2018/19
url_local_it18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=9&camp=1300'
url_visit_it18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=9&camp=1300'
local_it18<-table_maker(url_local_it18)
visit_it18<-table_maker(url_visit_it18)

#italia 2017/18
url_local_it17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=9&camp=1210'
url_visit_it17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=9&camp=1210'
local_it17<-table_maker(url_local_it17)
visit_it17<-table_maker(url_visit_it17)

#italia 2016/17
url_local_it16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=9&camp=1112'
url_visit_it16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=9&camp=1112'
local_it16<-table_maker(url_local_it16)
visit_it16<-table_maker(url_visit_it16)

#italia 2015/16
url_local_it15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=9&camp=1034'
url_visit_it15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=9&camp=1034'
local_it15<-table_maker(url_local_it15)
visit_it15<-table_maker(url_visit_it15)

#italia 2014/15
url_local_it14<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=9&camp=930'
url_visit_it14<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=9&camp=930'
local_it14<-table_maker(url_local_it14)
visit_it14<-table_maker(url_visit_it14)

# Holanda -------------------------------
#holanda 2020/21
url_local_ho<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=16&camp=1424'
url_visit_ho<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=16&camp=1424'
local_ho<-table_maker(url_local_ho)
visit_ho<-table_maker(url_visit_ho)

#holanda 2019/20
url_local_ho19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=16&camp=1368'
url_visit_ho19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=16&camp=1368'
local_ho19<-table_maker(url_local_ho19)
visit_ho19<-table_maker(url_visit_ho19)

#holanda 2018/19
url_local_ho18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=16&camp=1287'
url_visit_ho18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=16&camp=1287'
local_ho18<-table_maker(url_local_ho18)
visit_ho18<-table_maker(url_visit_ho18)

#holanda 2017/18
url_local_ho17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=16&camp=1208'
url_visit_ho17<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=16&camp=1208'
local_ho17<-table_maker(url_local_ho17)
visit_ho17<-table_maker(url_visit_ho17)

#holanda 2016/17
url_local_ho16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=16&camp=1106'
url_visit_ho16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=16&camp=1106'
local_ho16<-table_maker(url_local_ho16)
visit_ho16<-table_maker(url_visit_ho16)

#holanda 2015/16
url_local_ho15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=16&camp=1019'
url_visit_ho15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=16&camp=1019'
local_ho15<-table_maker(url_local_ho15)
visit_ho15<-table_maker(url_visit_ho15)

#holanda 2014/15
url_local_ho14<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=16&camp=914'
url_visit_ho14<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=16&camp=914'
local_ho14<-table_maker(url_local_ho14)
visit_ho14<-table_maker(url_visit_ho14)

# Belgica --------------------------
#belgica 2020/21
url_local_bl<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=13&camp=1421'
url_visit_bl<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=13&camp=1421'
local_bl<-table_maker(url_local_bl)
visit_bl<-table_maker(url_visit_bl)

#belgica 2019/20
url_local_bl19<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=13&camp=1370'
url_visit_bl19<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=13&camp=1370'
local_bl19<-table_maker(url_local_bl19)
visit_bl19<-table_maker(url_visit_bl19)

#belgica 2018/19
url_local_bl18<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=13&camp=1286'
url_visit_bl18<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=13&camp=1286'
local_bl18<-table_maker(url_local_bl18)
visit_bl18<-table_maker(url_visit_bl18)

#belgica 2017/18
url_local_bl17<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=13&camp=1197'
url_visit_bl17<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=13&camp=1197'
local_bl17<-table_maker(url_local_bl17)
visit_bl17<-table_maker(url_visit_bl17)

#belgica 2016/17
url_local_bl16<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=13&camp=1105'
url_visit_bl16<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=13&camp=1105'
local_bl16<-table_maker(url_local_bl16)
visit_bl16<-table_maker(url_visit_bl16)

#belgica 2015/16
url_local_bl15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=13&camp=1017'
url_visit_bl15<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=13&camp=1017'
local_bl15<-table_maker(url_local_bl15)
visit_bl15<-table_maker(url_visit_bl15)

#belgica 2014/15
url_local_bl14<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=13&camp=912'
url_visit_bl14<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=13&camp=912'
local_bl14<-table_maker(url_local_bl14)
visit_bl14<-table_maker(url_visit_bl14)

# Portugal -------------------------
#portugal 2020/21
url_local_po<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=12&camp=1435'
url_visit_po<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=12&camp=1435'
local_po<-table_maker(url_local_po)
visit_po<-table_maker(url_visit_po)

#portugal 2019/20
url_local_po19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=12&camp=1374'
url_visit_po19<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=12&camp=1374'
local_po19<-table_maker(url_local_po19)
visit_po19<-table_maker(url_visit_po19)


#portugal 2018/19
url_local_po18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=12&camp=1298'
url_visit_po18<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=12&camp=1298'
local_po18<-table_maker(url_local_po18)
visit_po18<-table_maker(url_visit_po18)

#portugal 2017/18
url_local_po17<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=12&camp=1199'
url_visit_po17<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=12&camp=1199'
local_po17<-table_maker(url_local_po17)
visit_po17<-table_maker(url_visit_po17)

#portugal 2016/17
url_local_po16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=12&camp=1111'
url_visit_po16<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=12&camp=1111'
local_po16<-table_maker(url_local_po16)
visit_po16<-table_maker(url_visit_po16)

#portugal 2015/16
url_local_po15<-'http://universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=12&camp=1029'
url_visit_po15<-'http://universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=12&camp=1029'
local_po15<-table_maker(url_local_po15)
visit_po15<-table_maker(url_visit_po15)

#portugal 2014/15
url_local_po14<-'http://www.universofutbol.com.ar/plantillas/archivos/posicioneslocal.php?div=12&camp=924'
url_visit_po14<-'http://www.universofutbol.com.ar/plantillas/archivos/posicionesvisitante.php?div=12&camp=924'
local_po14<-table_maker(url_local_po14)
visit_po14<-table_maker(url_visit_po14)





# Table ---------------------------------------------------------
dat<-local_ar20 %>%
  mutate(Cond = 'local', Year = 2021, Pais = 'Argentina') %>%
  bind_rows(visit_ar20 %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Argentina')) %>%
  bind_rows(local_ar %>% mutate(Cond = 'local', Year = 2020, Pais = 'Argentina')) %>%
  bind_rows(visit_ar %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Argentina')) %>%
  bind_rows(local_ar18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Argentina')) %>%
  bind_rows(visit_ar18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Argentina')) %>%
  bind_rows(local_ar17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Argentina')) %>%
  bind_rows(visit_ar17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Argentina')) %>%
  bind_rows(local_ar16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Argentina')) %>%
  bind_rows(visit_ar16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Argentina')) %>%
  bind_rows(local_ar15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Argentina')) %>%
  bind_rows(visit_ar15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Argentina')) %>%
  bind_rows(local_ar14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Argentina')) %>%
  bind_rows(visit_ar14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Argentina')) %>%
  bind_rows(local_ar21 %>% mutate(Cond = 'local', Year = 2022, Pais = 'Argentina')) %>%
  bind_rows(visit_ar21 %>% mutate(Cond = 'visit', Year = 2022, Pais = 'Argentina')) %>%
  
  bind_rows(local_br20 %>% mutate(Cond = 'local', Year = 2021, Pais = 'Brasil')) %>%
  bind_rows(visit_br20 %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Brasil')) %>%
  bind_rows(local_br %>% mutate(Cond = 'local', Year = 2020, Pais = 'Brasil')) %>%
  bind_rows(visit_br %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Brasil')) %>%
  bind_rows(local_br18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Brasil')) %>%
  bind_rows(visit_br18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Brasil')) %>%
  bind_rows(local_br17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Brasil')) %>%
  bind_rows(visit_br17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Brasil')) %>%
  bind_rows(local_br16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Brasil')) %>%
  bind_rows(visit_br16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Brasil')) %>%
  bind_rows(local_br15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Brasil')) %>%
  bind_rows(visit_br15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Brasil')) %>%
  bind_rows(local_br14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Brasil')) %>%
  bind_rows(visit_br14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Brasil')) %>%
  
  bind_rows(local_pr20 %>% mutate(Cond = 'local', Year = 2021, Pais = 'Paraguay')) %>%
  bind_rows(visit_pr20 %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Paraguay')) %>%
  bind_rows(local_pr %>% mutate(Cond = 'local', Year = 2020, Pais = 'Paraguay')) %>%
  bind_rows(visit_pr %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Paraguay')) %>%
  bind_rows(local_pr18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Paraguay')) %>%
  bind_rows(visit_pr18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Paraguay')) %>%
  bind_rows(local_pr17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Paraguay')) %>%
  bind_rows(visit_pr17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Paraguay')) %>%
  bind_rows(local_pr16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Paraguay')) %>%
  bind_rows(visit_pr16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Paraguay')) %>%
  bind_rows(local_pr15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Paraguay')) %>%
  bind_rows(visit_pr15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Paraguay')) %>%
  bind_rows(local_pr14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Paraguay')) %>%
  bind_rows(visit_pr14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Paraguay')) %>%
  
  bind_rows(local_ur20 %>% mutate(Cond = 'local', Year = 2021, Pais = 'Uruguay')) %>%
  bind_rows(visit_ur20 %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Uruguay')) %>%
  bind_rows(local_ur %>% mutate(Cond = 'local', Year = 2020, Pais = 'Uruguay')) %>%
  bind_rows(visit_ur %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Uruguay')) %>%
  bind_rows(local_ur18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Uruguay')) %>%
  bind_rows(visit_ur18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Uruguay')) %>%
  bind_rows(local_ur17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Uruguay')) %>%
  bind_rows(visit_ur17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Uruguay')) %>%
  bind_rows(local_ur16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Uruguay')) %>%
  bind_rows(visit_ur16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Uruguay')) %>%
  bind_rows(local_ur15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Uruguay')) %>%
  bind_rows(visit_ur15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Uruguay')) %>%
  bind_rows(local_ur14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Uruguay')) %>%
  bind_rows(visit_ur14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Uruguay')) %>%
  
  bind_rows(local_ch20 %>% mutate(Cond = 'local', Year = 2021, Pais = 'Chile')) %>%
  bind_rows(visit_ch20 %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Chile')) %>%
  bind_rows(local_ch %>% mutate(Cond = 'local', Year = 2020, Pais = 'Chile')) %>%
  bind_rows(visit_ch %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Chile')) %>%
  bind_rows(local_ch18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Chile')) %>%
  bind_rows(visit_ch18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Chile')) %>%
  bind_rows(local_ch17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Chile')) %>%
  bind_rows(visit_ch17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Chile')) %>%
  bind_rows(local_ch16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Chile')) %>%
  bind_rows(visit_ch16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Chile')) %>%
  bind_rows(local_ch15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Chile')) %>%
  bind_rows(visit_ch15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Chile')) %>%
  bind_rows(local_ch14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Chile')) %>%
  bind_rows(visit_ch14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Chile')) %>%
  
  bind_rows(local_al %>% mutate(Cond = 'local', Year = 2021, Pais = 'Alemania')) %>%
  bind_rows(visit_al %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Alemania')) %>%
  bind_rows(local_al19 %>% mutate(Cond = 'local', Year = 2020, Pais = 'Alemania')) %>%
  bind_rows(visit_al19 %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Alemania')) %>%
  bind_rows(local_al18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Alemania')) %>%
  bind_rows(visit_al18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Alemania')) %>%
  bind_rows(local_al17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Alemania')) %>%
  bind_rows(visit_al17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Alemania')) %>%
  bind_rows(local_al16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Alemania')) %>%
  bind_rows(visit_al16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Alemania')) %>%
  bind_rows(local_al15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Alemania')) %>%
  bind_rows(visit_al15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Alemania')) %>%
  bind_rows(local_al14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Alemania')) %>%
  bind_rows(visit_al14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Alemania')) %>%
  
  bind_rows(local_es %>% mutate(Cond = 'local', Year = 2021, Pais = 'Espana')) %>%
  bind_rows(visit_es %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Espana')) %>%
  bind_rows(local_es19 %>% mutate(Cond = 'local', Year = 2020, Pais = 'Espana')) %>%
  bind_rows(visit_es19 %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Espana')) %>%
  bind_rows(local_es18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Espana')) %>%
  bind_rows(visit_es18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Espana')) %>%
  bind_rows(local_es17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Espana')) %>%
  bind_rows(visit_es17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Espana')) %>%
  bind_rows(local_es16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Espana')) %>%
  bind_rows(visit_es16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Espana')) %>%
  bind_rows(local_es15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Espana')) %>%
  bind_rows(visit_es15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Espana')) %>%
  bind_rows(local_es14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Espana')) %>%
  bind_rows(visit_es14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Espana')) %>%
  
  bind_rows(local_fr %>% mutate(Cond = 'local', Year = 2021, Pais = 'Francia')) %>%
  bind_rows(visit_fr %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Francia')) %>%
  bind_rows(local_fr19 %>% mutate(Cond = 'local', Year = 2020, Pais = 'Francia')) %>%
  bind_rows(visit_fr19 %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Francia')) %>%
  bind_rows(local_fr18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Francia')) %>%
  bind_rows(visit_fr18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Francia')) %>%
  bind_rows(local_fr17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Francia')) %>%
  bind_rows(visit_fr17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Francia')) %>%
  bind_rows(local_fr16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Francia')) %>%
  bind_rows(visit_fr16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Francia')) %>%
  bind_rows(local_fr15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Francia')) %>%
  bind_rows(visit_fr15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Francia')) %>%
  bind_rows(local_fr14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Francia')) %>%
  bind_rows(visit_fr14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Francia')) %>%
  
  bind_rows(local_ing %>% mutate(Cond = 'local', Year = 2021, Pais = 'Inglaterra')) %>%
  bind_rows(visit_ing %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Inglaterra')) %>%
  bind_rows(local_ing19 %>% mutate(Cond = 'local', Year = 2020, Pais = 'Inglaterra')) %>%
  bind_rows(visit_ing19 %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Inglaterra')) %>%
  bind_rows(local_ing18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Inglaterra')) %>%
  bind_rows(visit_ing18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Inglaterra')) %>%
  bind_rows(local_ing17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Inglaterra')) %>%
  bind_rows(visit_ing17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Inglaterra')) %>%
  bind_rows(local_ing16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Inglaterra')) %>%
  bind_rows(visit_ing16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Inglaterra')) %>%
  bind_rows(local_ing15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Inglaterra')) %>%
  bind_rows(visit_ing15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Inglaterra')) %>%
  bind_rows(local_ing14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Inglaterra')) %>%
  bind_rows(visit_ing14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Inglaterra')) %>%

  bind_rows(local_it %>% mutate(Cond = 'local', Year = 2021, Pais = 'Italia')) %>%
  bind_rows(visit_it %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Italia')) %>%
  bind_rows(local_it19 %>% mutate(Cond = 'local', Year = 2020, Pais = 'Italia')) %>%
  bind_rows(visit_it19 %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Italia')) %>%
  bind_rows(local_it18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Italia')) %>%
  bind_rows(visit_it18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Italia')) %>%
  bind_rows(local_it17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Italia')) %>%
  bind_rows(visit_it17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Italia')) %>%
  bind_rows(local_it16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Italia')) %>%
  bind_rows(visit_it16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Italia')) %>%
  bind_rows(local_it15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Italia')) %>%
  bind_rows(visit_it15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Italia')) %>%
  bind_rows(local_it14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Italia')) %>%
  bind_rows(visit_it14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Italia')) %>%

  bind_rows(local_ho %>% mutate(Cond = 'local', Year = 2021, Pais = 'Holanda')) %>%
  bind_rows(visit_ho %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Holanda')) %>%
  bind_rows(local_ho19 %>% mutate(Cond = 'local', Year = 2020, Pais = 'Holanda')) %>%
  bind_rows(visit_ho19 %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Holanda')) %>%
  bind_rows(local_ho18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Holanda')) %>%
  bind_rows(visit_ho18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Holanda')) %>%
  bind_rows(local_ho17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Holanda')) %>%
  bind_rows(visit_ho17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Holanda')) %>%
  bind_rows(local_ho16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Holanda')) %>%
  bind_rows(visit_ho16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Holanda')) %>%
  bind_rows(local_ho15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Holanda')) %>%
  bind_rows(visit_ho15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Holanda')) %>%
  bind_rows(local_ho14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Holanda')) %>%
  bind_rows(visit_ho14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Holanda')) %>%

  bind_rows(local_po %>% mutate(Cond = 'local', Year = 2021, Pais = 'Portugal')) %>%
  bind_rows(visit_po %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Portugal')) %>%
  bind_rows(local_po19 %>% mutate(Cond = 'local', Year = 2020, Pais = 'Portugal')) %>%
  bind_rows(visit_po19 %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Portugal')) %>%
  bind_rows(local_po18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Portugal')) %>%
  bind_rows(visit_po18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Portugal')) %>%
  bind_rows(local_po17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Portugal')) %>%
  bind_rows(visit_po17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Portugal')) %>%
  bind_rows(local_po16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Portugal')) %>%
  bind_rows(visit_po16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Portugal')) %>%
  bind_rows(local_po15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Portugal')) %>%
  bind_rows(visit_po15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Portugal')) %>%
  bind_rows(local_po14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Portugal')) %>%
  bind_rows(visit_po14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Portugal')) %>%

  bind_rows(local_bl %>% mutate(Cond = 'local', Year = 2021, Pais = 'Belgica')) %>%
  bind_rows(visit_bl %>% mutate(Cond = 'visit', Year = 2021, Pais = 'Belgica')) %>%
  bind_rows(local_bl19 %>% mutate(Cond = 'local', Year = 2020, Pais = 'Belgica')) %>%
  bind_rows(visit_bl19 %>% mutate(Cond = 'visit', Year = 2020, Pais = 'Belgica')) %>%
  bind_rows(local_bl18 %>% mutate(Cond = 'local', Year = 2019, Pais = 'Belgica')) %>%
  bind_rows(visit_bl18 %>% mutate(Cond = 'visit', Year = 2019, Pais = 'Belgica')) %>%
  bind_rows(local_bl17 %>% mutate(Cond = 'local', Year = 2018, Pais = 'Belgica')) %>%
  bind_rows(visit_bl17 %>% mutate(Cond = 'visit', Year = 2018, Pais = 'Belgica')) %>%
  bind_rows(local_bl16 %>% mutate(Cond = 'local', Year = 2017, Pais = 'Belgica')) %>%
  bind_rows(visit_bl16 %>% mutate(Cond = 'visit', Year = 2017, Pais = 'Belgica')) %>%
  bind_rows(local_bl15 %>% mutate(Cond = 'local', Year = 2016, Pais = 'Belgica')) %>%
  bind_rows(visit_bl15 %>% mutate(Cond = 'visit', Year = 2016, Pais = 'Belgica')) %>%
  bind_rows(local_bl14 %>% mutate(Cond = 'local', Year = 2015, Pais = 'Belgica')) %>%
  bind_rows(visit_bl14 %>% mutate(Cond = 'visit', Year = 2015, Pais = 'Belgica'))


# Two teams by Country ----------------------
# I choose two teams, the first historic team, and the median one. 
#  Then I choose 

ing<-dat %>% filter(Pais == 'Inglaterra') %>% 
  group_by(Equipo) %>%
  summarise(PTS = median(sum(PTS))) %>%
  arrange(desc(PTS))
# median(ing$PTS)
# intersect(local_ing$Equipo,c(local_ing15$Equipo,local_ing16$Equipo,local_ing17$Equipo,local_ing18$Equipo))
# View(ing) #Manchester City, West Bromwich Albion 

fra<-dat %>% filter(Pais == 'Francia') %>% 
  group_by(Equipo) %>%
  summarise(PTS = median(sum(PTS))) %>%
  arrange(desc(PTS))
# median(fra$PTS)
# intersect(local_fr$Equipo,c(local_fr15$Equipo,local_fr16$Equipo,local_fr17$Equipo,local_fr18$Equipo))
# View(fra) #Angers SCO, PSG

ita<-dat %>% filter(Pais == 'Italia') %>% 
  group_by(Equipo) %>%
  summarise(PTS = median(sum(PTS))) %>%
  arrange(desc(PTS))
# median(ita$PTS)
# intersect(local_it$Equipo,c(local_it15$Equipo,local_it16$Equipo,local_it17$Equipo,local_it18$Equipo))
# View(ita) #Genoa, Juventus

bel<-dat %>% filter(Pais == 'Belgica') %>% 
  group_by(Equipo) %>%
  summarise(PTS = median(sum(PTS))) %>%
  arrange(desc(PTS))
# median(bel$PTS)
# intersect(local_bl$Equipo,c(local_bl15$Equipo,local_bl16$Equipo,local_bl17$Equipo,local_bl18$Equipo))
# View(bel) #'Waasland-Beveren' , 'Club Brugge'

por<-dat %>% filter(Pais == 'Portugal') %>% 
  group_by(Equipo) %>%
  summarise(PTS = median(sum(PTS))) %>%
  arrange(desc(PTS))
median(por$PTS)
# intersect(local_po$Equipo,c(local_po15$Equipo,local_po16$Equipo,local_po17$Equipo,local_po18$Equipo))
# View(por) #'Paços de Ferreira' , 'Benfica'

hol<-dat %>% filter(Pais == 'Holanda') %>% 
  group_by(Equipo) %>%
  summarise(PTS = median(sum(PTS))) %>%
  arrange(desc(PTS))
median(hol$PTS)
# intersect(local_ho$Equipo,c(local_ho15$Equipo,local_ho16$Equipo,local_ho17$Equipo,local_ho18$Equipo))
# View(hol) #'Willem II' , 'Ajax'

esp<-dat %>% filter(Pais == 'Espana') %>% 
  group_by(Equipo) %>%
  summarise(PTS = median(sum(PTS))) %>%
  arrange(desc(PTS))
median(esp$PTS)
# intersect(local_es$Equipo,c(local_es15$Equipo,local_es16$Equipo,local_es17$Equipo,local_es18$Equipo))
# View(esp) # 'Barcelona' , 'Levante'

ale<-dat %>% filter(Pais == 'Alemania') %>% 
  group_by(Equipo) %>%
  summarise(PTS = median(sum(PTS))) %>%
  arrange(desc(PTS))
# median(ale$PTS)
# intersect(local_al$Equipo,c(local_al15$Equipo,local_al16$Equipo,local_al17$Equipo,local_al18$Equipo))
# View(ale) # 'Bayern Münich' , 'Augsburg'


# local_2 = dos equipos por pais de local
#Dos equipos de local !=2020
local_2<-dat %>% filter(Equipo %in% c('Boston River', 'Peñarol', 'Boca Juniors', 'Rosario Central',
                             'Deportes Iquique', 'U. Católica', 'Sportivo Luqueño', 'Olimpia',
                             'Ponte Preta', 'Palmeiras', 'Manchester City', 'West Bromwich Albion',
                             'Angers SCO', 'PSG', 'Genoa', 'Juventus', 'Paços de Ferreira' , 'Benfica',
                             'Willem II' , 'Ajax', 'Barcelona' , 'Levante' , 'Bayern Münich' , 'Augsburg',
                             'Waasland-Beveren' , 'Club Brugge') & Year != c(2020,2021) & Cond == 'local') %>%
  mutate(Cond = 'local') %>%
  group_by(Pais, Cond) %>%
  summarize(AVG =mean(G/J))

# visit_2 = dos equipos por pais de visitante
#Dos equipode visitante !=2020
visit_2<-dat %>% filter(Equipo %in% c('Boston River', 'Peñarol', 'Boca Juniors', 'Rosario Central',
                             'Deportes Iquique', 'U. Católica', 'Sportivo Luqueño', 'Olimpia',
                             'Ponte Preta', 'Palmeiras', 'Manchester City', 'West Bromwich Albion',
                             'Angers SCO', 'PSG', 'Genoa', 'Juventus', 'Paços de Ferreira' , 'Benfica',
                             'Willem II' , 'Ajax', 'Barcelona' , 'Levante' , 'Bayern Münich' , 'Augsburg',
                             'Waasland-Beveren' , 'Club Brugge') & Year != c(2020,2021) & Cond == 'visit') %>%
  mutate(Cond = 'visit') %>%
  group_by(Pais, Cond) %>%
  summarize(AVG =mean(G/J))


# loccal_2_2020 = dos equipos por pais de local
#Dos equipos de local !=2020
local_2_2020<-dat %>% filter(Equipo %in% c('Boston River', 'Peñarol', 'Boca Juniors', 'Rosario Central',
                                      'Deportes Iquique', 'U. Católica', 'Sportivo Luqueño', 'Olimpia',
                                      'Ponte Preta', 'Palmeiras', 'Manchester City', 'West Bromwich Albion',
                                      'Angers SCO', 'PSG', 'Genoa', 'Juventus', 'Paços de Ferreira' , 'Benfica',
                                      'Willem II' , 'Ajax', 'Barcelona' , 'Levante' , 'Bayern Münich' , 'Augsburg',
                                      'Waasland-Beveren' , 'Club Brugge') & Year %in% c(2020,2021) & Cond == 'local') %>%
  mutate(Cond = 'local') %>%
  group_by(Pais, Cond) %>%
  summarize(AVG =mean(G/J))

# visit_2_2020 = dos equipos por pais de visitante
# Dos equipos de visitante = 2020
visit_2_2020<-dat %>% filter(Equipo %in% c('Boston River', 'Peñarol', 'Boca Juniors', 'Rosario Central',
                                      'Deportes Iquique', 'U. Católica', 'Sportivo Luqueño', 'Olimpia',
                                      'Ponte Preta', 'Palmeiras', 'Manchester City', 'West Bromwich Albion',
                                      'Angers SCO', 'PSG', 'Genoa', 'Juventus', 'Paços de Ferreira' , 'Benfica',
                                      'Willem II' , 'Ajax', 'Barcelona' , 'Levante' , 'Bayern Münich' , 'Augsburg',
                                      'Waasland-Beveren' , 'Club Brugge') & Year %in% c(2020,2021) & Cond == 'visit') %>%
  mutate(Cond = 'visit') %>%
  group_by(Pais, Cond) %>%
  summarize(AVG =mean(G/J))

