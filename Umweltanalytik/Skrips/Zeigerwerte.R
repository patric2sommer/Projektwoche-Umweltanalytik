# UMAN Gruppe 7 - Auswertung Zeigerwerte (Boxplot)

# Packages einlesen
library(tidyverse)

# Daten einlesen
df = read.csv("Zeigerwerte.csv", header = TRUE, sep = ";")

#Dataframe mutieren
dfneu = df %>%
          pivot_longer(cols = c(Plot.1,Plot.2, Plot.3, Plot.4, Plot.5, Plot.6, Plot.7, Plot.8, Plot.9), names_to = "Plot") 

#Value 0 herausfiltern  
df1 = filter(dfneu, value > 0)

#Standort hinzufügen
plot = df1$Plot

plot[plot == "Plot.1"] <- "Gemaehte Wiese"
plot[plot == "Plot.2"] <- "Gemaehte Wiese"
plot[plot == "Plot.3"] <- "Gemaehte Wiese"
plot[plot == "Plot.4"] <- "Ungemaehte Wiese"
plot[plot == "Plot.5"] <- "Ungemaehte Wiese"
plot[plot == "Plot.6"] <- "Ungemaehte Wiese"
plot[plot == "Plot.7"] <- "Ruderalweg"
plot[plot == "Plot.8"] <- "Ruderalweg"
plot[plot == "Plot.9"] <- "Ruderalweg"

df1$Standort = plot

#Dataframe mutieren (Zeigerwerte in eine Spalte)

dfzeiger = df1 %>%
  pivot_longer(cols = c(Feuchtezahl, Reaktionzahl, Naehrstoffzahl), values_to = "ZW")

#Plot Feuchtezahl
df1 %>%
  ggplot(aes(x=Standort, y=Feuchtezahl)) + 
  geom_boxplot()+
  geom_jitter(aes(col=Standort), height = 0,)+
  theme_bw()+
  scale_color_manual(values = c("Gemaehte Wiese" = "red",
                                "Ungemaehte Wiese" = "yellow",
                                "Ruderalweg" = "blue"))+
  ggtitle("Feuchtezahl")

#Plot Reaktionszahl
df1 %>%
  ggplot(aes(x=Standort, y=Reaktionzahl)) + 
  geom_boxplot()+
  geom_jitter(aes(col=Standort), height = 0,)+
  theme_bw()+
  scale_color_manual(values = c("Gemaehte Wiese" = "red",
                                "Ungemaehte Wiese" = "yellow",
                                "Ruderalweg" = "blue"))+
  ggtitle("Reaktionszahl")

#Plot Nährstoffzahl
df1 %>%
  ggplot(aes(x=Standort, y=Naehrstoffzahl)) + 
  geom_boxplot()+
  geom_jitter(aes(col=Standort), height = 0,)+
  theme_bw()+
  scale_color_manual(values = c("Gemaehte Wiese" = "red",
                                "Ungemaehte Wiese" = "yellow",
                                "Ruderalweg" = "blue"))+
  ggtitle("Nährstoffzahl")


# neue plots aller Zeigerwerte

dfzeiger %>%
  ggplot(aes(x=name, y=ZW)) + 
  geom_boxplot()+
  geom_jitter(aes(col=Standort), height = 0,)+
  theme_bw()+
  scale_color_manual(values = c("Gemaehte Wiese" = "red",
                                "Ungemaehte Wiese" = "yellow",
                                "Ruderalweg" = "blue"))+
  ggtitle("Zeigerwerte")+
  labs(x = "Zeigerwerte", y = "Zahl")
  
  
