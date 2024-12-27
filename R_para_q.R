library(readr)
library(dplyr)
library(tidyr)



data <- read.csv("datos/1976_2020_president.csv")

library(tidyverse)
data <- data %>%
  separate(col = 1,  
           into = c("year", "state", "state_po", "state_fips", "state_cen", 
                    "state_ic", "office", "candidate", "party_detailed", 
                    "writein", "candidatevotes", "totalvotes", "version", 
                    "notes", "party_simplified"),
           sep = ",")


my_data <- data 
           
my_data <- data %>%
         mutate(full_name = paste(party_detailed, candidate))

my_data <- my_data %>%
  select(-(4:6) , -(13:15))

my_data <- my_data %>%
  select(-(5:6))

library(stringr)

my_data$state_po <- gsub('"', '', my_data$state_po)
my_data$office <- gsub('"', '', my_data$office)
my_data$writein <- gsub('"', '', my_data$writein)
my_data$full_name <- gsub('"', '', my_data$full_name)
my_data$state <- gsub ('"' , '' , my_data$state)

my_data <- my_data %>%
  mutate(year = as.numeric(year))

my_data <- my_data %>%
  mutate(totalvotes = as.numeric(totalvotes))



str(my_data)

my_data <- my_data %>%
  filter(writein %in% c("DEMOCRAT", "REPUBLICAN")) %>%
           filter(year > 1990) %>%
           drop_na()


My_final_data <- my_data %>%
  rename(party = writein) %>%
  select(-candidatevotes)

My_final_data$state_po <- gsub('"', '', My_final_data$state_po)
My_final_data$office <- gsub('"', '', My_final_data$office)
My_final_data$party <- gsub('"', '', My_final_data$party)
My_final_data$full_name <- gsub('"', '', My_final_data$full_name)
My_final_data$state <- gsub ('"' , '' , My_final_data$state)

str(My_final_data)

My_final_data <- My_final_data %>%
  mutate(totalvotes = as.numeric(totalvotes))
##################################################################################

#ELECCIONES 1992, BILL CLINTON VS GEORGE W. BUSH

data_1992 <- My_final_data %>%
  filter(year == 1992)


str(data_1992)

data_1992 <- data_1992 %>%
  mutate(totalvotes = as.numeric(totalvotes))

str(data_1992)

# Sumar votos por partido a nivel nacional
resultados_nacionales_1992 <- data_1992 %>%
  group_by(party , full_name) %>%
  summarise(total_votos = sum(totalvotes)) %>%
  arrange(desc(total_votos)) %>%
  ungroup() %>%
mutate (porcentaje = round(total_votos / sum(total_votos) * 100, 2), # Redondear porcentaje
total_votos = format(total_votos, big.mark = ","))
           

str(resultados_nacionales_1992)

#HACER GRAFICO

library(ggplot2)
library(ggthemes)

head(ggthemes_data)


ggplot(resultados_nacionales_1992, aes(x = full_name, y = total_votos, fill = party)) +
  geom_bar(stat = "identity") +
  labs (title = "Elecciones 1992",
  subtitle = "(Votos por candidato)", 
  caption = " Datos de MIT Election Data and Science Lab, 2017, Harvard Dataverse, V8",
  x = "Candidato", 
  y = "Número de votos", 
  fill = "Partido", 
  tag = "plot 1") +
  theme_clean() +
  theme(plot.subtitle = element_text(hjust = 1)) + 
  theme(plot.title = element_text(size = 22, hjust = 0.9)) +
  theme(panel.background = element_rect(fill = "navy", colour = "firebrick", 
                                       linewidth = 2)) +
  theme(axis.text.x = element_text(colour = "black", size = 10)) + 
  geom_text(aes(label = porcentaje),  color = "white", vjust = -0.3) +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red"))
        
 
###############################################################################

#ELECCIONES 1996 ROBERT DOLE VS BILL CLINTON

data_1996 <- My_final_data %>%
  filter(year == 1996)

str(data_1996)

resultados_nacionales_1996 <- data_1996 %>%
  group_by(party, full_name) %>%
  summarise(total_votos = sum(totalvotes))%>%
              arrange(desc(total_votos))%>%
              ungroup() %>%
  mutate (porcentaje = round(total_votos / sum(total_votos) * 100, 2), # Redondear porcentaje
          total_votos = format(total_votos, big.mark = ","))

#HACER GRAFICO

ggplot(resultados_nacionales_1996, aes(x = full_name, y = total_votos, fill = party)) +
  geom_bar(stat = "identity") +
  labs (title = "Elecciones 1996",
        subtitle = "(Votos por candidato)", 
        caption = " Datos de MIT Election Data and Science Lab, 2017, Harvard Dataverse, V8",
        x = "Candidato", 
        y = "Número de votos", 
        fill = "Partido", 
        tag = "plot 2") +
  theme_clean() +
  theme(plot.subtitle = element_text(hjust = 1)) + 
  theme(plot.title = element_text(size = 22, hjust = 0.9)) +
  theme(panel.background = element_rect(fill = "navy", colour = "firebrick", 
                                        linewidth = 2)) +
  theme(axis.text.x = element_text(colour = "black", size = 10)) + 
  geom_text(aes(label = porcentaje),  color = "white", vjust = -0.3) +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red"))

##############################################################################

#ELECCIONES 2000 AL GORE VS GEORFE W. BUSH

data_2000 <- My_final_data %>%
  filter(year == 2000)

str(data_2000)

resultados_nacionales_2000 <- data_2000 %>%
  group_by(party, full_name) %>%
  summarise(total_votos = sum(totalvotes))%>%
  arrange(desc(total_votos))%>%
  ungroup() %>%
  mutate (porcentaje = round(total_votos / sum(total_votos) * 100, 2), # Redondear porcentaje
          total_votos = format(total_votos, big.mark = ","))


#HACER GRAFICO

ggplot(resultados_nacionales_2000, aes(x = full_name, y = total_votos, fill = party)) +
  geom_bar(stat = "identity") +
  labs (title = "Elecciones 2000",
        subtitle = "(Votos por candidato)", 
        caption = " Datos de MIT Election Data and Science Lab, 2017, Harvard Dataverse, V8",
        x = "Candidato", 
        y = "Número de votos", 
        fill = "Partido", 
        tag = "plot 3") +
  theme_clean() +
  theme(plot.subtitle = element_text(hjust = 1)) + 
  theme(plot.title = element_text(size = 22, hjust = 0.9)) +
  theme(panel.background = element_rect(fill = "navy", colour = "firebrick", 
                                        linewidth = 2)) +
  theme(axis.text.x = element_text(colour = "black", size = 10)) + 
  geom_text(aes(label = porcentaje),  color = "white", vjust = -0.3) +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red")) + 
                      scale_y_continuous(
                        breaks = seq(0, 100000000, 25000000))
                   


##############################################################################

#ELECCIONES 2004 JOHN KERRY VS GEORGE W. BUSH

data_2004 <- My_final_data %>%
  filter(year == 2004)

str(data_2004)

resultados_nacionales_2004 <- data_2004 %>%
  group_by(party, full_name) %>%
  summarise(total_votos = sum(totalvotes))%>%
  arrange(desc(total_votos))%>%
  ungroup() %>%
  mutate (porcentaje = round(total_votos / sum(total_votos) * 100, 2), # Redondear porcentaje
          total_votos = format(total_votos, big.mark = ","))

#HACER GRAFICO
ggplot(resultados_nacionales_2004, aes(x = full_name, y = total_votos, fill = party)) +
  geom_bar(stat = "identity") +
  labs (title = "Elecciones 2004",
        subtitle = "(Votos por candidato)", 
        caption = " Datos de MIT Election Data and Science Lab, 2017, Harvard Dataverse, V8",
        x = "Candidato", 
        y = "Número de votos", 
        fill = "Partido", 
        tag = "plot 4") +
  theme_clean() +
  theme(plot.subtitle = element_text(hjust = 1)) + 
  theme(plot.title = element_text(size = 22, hjust = 0.9)) +
  theme(panel.background = element_rect(fill = "navy", colour = "firebrick", 
                                        linewidth = 2)) +
  theme(axis.text.x = element_text(colour = "black", size = 10)) + 
  geom_text(aes(label = porcentaje),  color = "white", vjust = -0.3) +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red"))


##############################################################################

#ELECCIONES 2008 JOHN MCCAIN VS BARACK H. OBAMA

data_2008 <- My_final_data %>%
  filter(year == 2008)

str(data_2008)

resultados_nacionales_2008 <- data_2008 %>%
  group_by(party, full_name) %>%
  summarise(total_votos = sum(totalvotes))%>%
  arrange(desc(total_votos))%>%
  ungroup() %>%
  mutate (porcentaje = round(total_votos / sum(total_votos) * 100, 2), # Redondear porcentaje
          total_votos = format(total_votos, big.mark = ","))

#HACER GRAFICO

ggplot(resultados_nacionales_2008, aes(x = full_name, y = total_votos, fill = party)) +
  geom_bar(stat = "identity") +
  labs (title = "Elecciones 2008",
        subtitle = "(Votos por candidato)", 
        caption = " Datos de MIT Election Data and Science Lab, 2017, Harvard Dataverse, V8",
        x = "Candidato", 
        y = "Número de votos", 
        fill = "Partido", 
        tag = "plot 5") +
  theme_clean() +
  theme(plot.subtitle = element_text(hjust = 1)) + 
  theme(plot.title = element_text(size = 22, hjust = 0.9)) +
  theme(panel.background = element_rect(fill = "navy", colour = "firebrick", 
                                        linewidth = 2)) +
  theme(axis.text.x = element_text(colour = "black", size = 10)) + 
  geom_text(aes(label = porcentaje),  color = "white", vjust = -0.3) +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red"))


##############################################################################

#ELECCIONES 2012 MITT ROMNEY VS BARACK H. OBAMA



data_2012 <- My_final_data %>%
  filter(year == 2012)

str(data_2012)

resultados_nacionales_2012 <- data_2012 %>%
  group_by(party, full_name) %>%
  summarise(total_votos = sum(totalvotes))%>%
  arrange(desc(total_votos))%>%
  ungroup() %>%
  mutate (porcentaje = round(total_votos / sum(total_votos) * 100, 2), # Redondear porcentaje
          total_votos = format(total_votos, big.mark = ","))

#HACER GRAFICO

ggplot(resultados_nacionales_2012, aes(x = full_name, y = total_votos, fill = party)) +
  geom_bar(stat = "identity") +
  labs (title = "Elecciones 2012",
        subtitle = "(Votos por candidato)", 
        caption = " Datos de MIT Election Data and Science Lab, 2017, Harvard Dataverse, V8",
        x = "Candidato", 
        y = "Número de votos", 
        fill = "Partido", 
        tag = "plot 6") +
  theme_clean() +
  theme(plot.subtitle = element_text(hjust = 1)) + 
  theme(plot.title = element_text(size = 22, hjust = 0.9)) +
  theme(panel.background = element_rect(fill = "navy", colour = "firebrick", 
                                        linewidth = 2)) +
  theme(axis.text.x = element_text(colour = "black", size = 10)) + 
  geom_text(aes(label = porcentaje),  color = "white", vjust = -0.3) +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red"))


##############################################################################

#ELECCIONES 2016 DONALD J. TRUMP VS HILLARY CLINTON

data_2016 <- My_final_data %>%
  filter(year == 2016)

str(data_2016)

resultados_nacionales_2016 <- data_2016 %>%
  group_by(party, full_name) %>%
  summarise(total_votos = sum(totalvotes))%>%
  arrange(desc(total_votos))%>%
  ungroup() %>%
  mutate (porcentaje = round(total_votos / sum(total_votos) * 100, 2), # Redondear porcentaje
          total_votos = format(total_votos, big.mark = ","))

#HACER GRAFICO

ggplot(resultados_nacionales_2016, aes(x = full_name, y = total_votos, fill = party)) +
  geom_bar(stat = "identity") +
  labs (title = "Elecciones 2016",
        subtitle = "(Votos por candidato)", 
        caption = " Datos de MIT Election Data and Science Lab, 2017, Harvard Dataverse, V8",
        x = "Candidato", 
        y = "Número de votos", 
        fill = "Partido", 
        tag = "plot 7") +
  theme_clean() +
  theme(plot.subtitle = element_text(hjust = 1)) + 
  theme(plot.title = element_text(size = 22, hjust = 0.9)) +
  theme(panel.background = element_rect(fill = "navy", colour = "firebrick", 
                                        linewidth = 2)) +
  theme(axis.text.x = element_text(colour = "black", size = 10)) + 
  geom_text(aes(label = porcentaje),  color = "white", vjust = -0.3) +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red"))


##################################################################33

#ELECCIONES 2020 TRUMP VS BIDEN

data_2020 <- My_final_data %>%
  filter(year == 2020)

resultados_nacionales_2020 <- data_2020 %>%
  group_by(party, full_name) %>%
  summarise(total_votos = sum(totalvotes))%>%
  arrange(desc(total_votos))%>%
  ungroup() %>%
  mutate (porcentaje = round(total_votos / sum(total_votos) * 100, 2), # Redondear porcentaje
          total_votos = format(total_votos, big.mark = ","))


ggplot(resultados_nacionales_2020, aes(x = full_name, y = total_votos, fill = party)) +
  geom_bar(stat = "identity") +
  labs (title = "Elecciones 2020",
        subtitle = "(Votos por candidato)", 
        caption = " Datos de MIT Election Data and Science Lab, 2017, Harvard Dataverse, V8",
        x = "Candidato", 
        y = "Número de votos", 
        fill = "Partido", 
        tag = "plot 8") +
  theme_clean() +
  theme(plot.subtitle = element_text(hjust = 1)) + 
  theme(plot.title = element_text(size = 22, hjust = 0.9)) +
  theme(panel.background = element_rect(fill = "navy", colour = "firebrick", 
                                        linewidth = 2)) +
  theme(axis.text.x = element_text(colour = "black", size = 10)) + 
  geom_text(aes(label = porcentaje),  color = "white", vjust = -0.3) +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red"))






install.packages("usmap")
library(ggplot2)
library(usmap)

Estados_democratas <- c("CA","WA","OR","NV","AZ","NM","CO","MN","MI","WI","GA","VA","DC","MD","DE","PA","NJ","NY","CT","RI","VT","ME","NH","MA", "IL","HI")

Estados_republicanos <- c("ID","UT","MT","ND","SD","TX","OK","WY","NE","IA","KS","MO","AR","LA","MS","AL","TN","FL","NC","SC","KY","IN","OH","WV","AK")



datitos <- data.frame(
  state = c(Estados_democratas, Estados_republicanos),
  group = c(rep("Demócrata", length(Estados_democratas)), rep("Republicano", length(Estados_republicanos)))
)

#USAREMOS REP Y LENGHT, REP PARA QUE DEMOCRATAS Y REPUBLICANOS SE ASOCIEN A UN DATA FRAME SEGUN
#SEGUN LA CANTIDAD DE OBSERVACIONES QUE HAYA, CON LO QUE PARA USAR TODAS LAS QUE HAYA, USAMOS 
#LENGHT, DONDE TOMA TODAS LAS OBSERVACIONES ASOCIADAS A UNA VARIABLE

plot_usmap(data = datitos, values = "group", regions = "states", labels = TRUE, label_color = "white") + 
  scale_fill_manual(
    values = c("Demócrata" = "blue", "Republicano" = "red"),
    name = "Partido"
  ) +
  theme(legend.position = "right") +
  labs(title = "Mapa estatal 2020",
       tag  = "Plot 8",
       caption = " Datos de MIT Election Data and Science Lab, 2017, Harvard Dataverse, V8",) +
  theme(plot.title = element_text(size = 22))



ggplot(data = data_2020, aes(x = state_po, y = totalvotes, fill = party)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red")) +
  coord_flip() +  
  labs(
    title = "Comparación de votos por estado y partido",
    x = "Estado",
    y = "Número total de votos",
    fill = "Partido",
    tag = "Plot 10",
    caption = "Datos de MIT Election Data and Science Lab, 2017, Harvard Dataverse, V8"
  ) +
  theme_test() +
  theme(
    axis.text.x = element_blank(),  
    axis.text.y = element_text(size = 7))

