# UMAN Gruppe 7 - Auswertung Zeigerwerte (FRN) gewichtet (Boxplot)

setwd("E:\\Dokumente\\ZHAW\\ZHAW_Github\\Projektwoche-Umweltanalytik\\data")

# Packages einlesen
library(tidyverse)

# Daten einlesen
DF = read.csv("cwm.csv", header = TRUE, sep = ";")

#### NA zu 0 umwandeln (hokuspokus) ####

DF[is.na(DF)] <- 0

#### Mean ausrechnen (N) ####
N = DF %>%
  summarise(
    plot1 = sum(p1 * Naehrstoffzahl)/sum(p1),
    plot2 = sum(p2 * Naehrstoffzahl)/sum(p2),
    plot3 = sum(p3 * Naehrstoffzahl)/sum(p3),
    plot4 = sum(p4 * Naehrstoffzahl)/sum(p4),
    plot5 = sum(p5 * Naehrstoffzahl)/sum(p5),
    plot6 = sum(p6 * Naehrstoffzahl)/sum(p6),
    plot7 = sum(p7 * Naehrstoffzahl)/sum(p7),
    plot8 = sum(p8 * Naehrstoffzahl)/sum(p8),
    plot9 = sum(p9 * Naehrstoffzahl)/sum(p9),
  )
#### Mean ausrechnen (R) ####
R = DF %>%
  summarise(
    plot1 = sum(p1 * Reaktionzahl)/sum(p1),
    plot2 = sum(p2 * Reaktionzahl)/sum(p2),
    plot3 = sum(p3 * Reaktionzahl)/sum(p3),
    plot4 = sum(p4 * Reaktionzahl)/sum(p4),
    plot5 = sum(p5 * Reaktionzahl)/sum(p5),
    plot6 = sum(p6 * Reaktionzahl)/sum(p6),
    plot7 = sum(p7 * Reaktionzahl)/sum(p7),
    plot8 = sum(p8 * Reaktionzahl)/sum(p8),
    plot9 = sum(p9 * Reaktionzahl)/sum(p9),
  )
#### Mean ausrechnen (F) ####
F = DF %>%
  summarise(
    plot1 = sum(p1 * Feuchtezahl)/sum(p1),
    plot2 = sum(p2 * Feuchtezahl)/sum(p2),
    plot3 = sum(p3 * Feuchtezahl)/sum(p3),
    plot4 = sum(p4 * Feuchtezahl)/sum(p4),
    plot5 = sum(p5 * Feuchtezahl)/sum(p5),
    plot6 = sum(p6 * Feuchtezahl)/sum(p6),
    plot7 = sum(p7 * Feuchtezahl)/sum(p7),
    plot8 = sum(p8 * Feuchtezahl)/sum(p8),
    plot9 = sum(p9 * Feuchtezahl)/sum(p9),
  )

#### Mean zusammenf?gen ####
ZW = rbind(F,N,R)

rownames(ZW) <- c("Feuchtezahl",
                  "Naehrstoffzahl",
                  "Reaktionszahl"
                  )

ZW_box = ZW%>%
          pivot_longer(cols = c(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9),
                       names_to = "Plot"
                       )




#### Column "Zeigerwert" einf?gen ####
Zeigerwert = c("Feuchtezahl","Feuchtezahl","Feuchtezahl","Feuchtezahl","Feuchtezahl","Feuchtezahl","Feuchtezahl","Feuchtezahl","Feuchtezahl",
               "Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl",
               "Reaktionszahl","Reaktionszahl","Reaktionszahl","Reaktionszahl","Reaktionszahl","Reaktionszahl","Reaktionszahl","Reaktionszahl","Reaktionszahl")

ZW_box$Zeigerwert = Zeigerwert

#### Standort hinzuf?gen ####

plot = ZW_box$Plot #Vektor aus Dataframe "herauslesen"

plot[plot == "plot1"] <- "Gemaehte Wiese"     # Werte des Vektors ?ndern
plot[plot == "plot2"] <- "Gemaehte Wiese"
plot[plot == "plot3"] <- "Gemaehte Wiese"
plot[plot == "plot4"] <- "Ungemaehte Wiese"
plot[plot == "plot5"] <- "Ungemaehte Wiese"
plot[plot == "plot6"] <- "Ungemaehte Wiese"
plot[plot == "plot7"] <- "Ruderalweg"
plot[plot == "plot8"] <- "Ruderalweg"
plot[plot == "plot9"] <- "Ruderalweg"

ZW_box$Standort = plot # Vektor wieer in Dataframe einf?gen

#### Mittelwerte der Zeigerwerte pro Standort ausrechnen ####

# Daten sortieren
f_gemaeht = ZW_box[c(1,2,3), "value"]
f_ungemaeht = ZW_box[c(4,5,6), "value"]
f_ruderalweg = ZW_box[c(7,8,9), "value"]

r_gemaeht = ZW_box[c(19,20,21), "value"]
r_ungemaeht = ZW_box[c(22,23,24), "value"]
r_ruderalweg = ZW_box[c(25,26,27), "value"]

n_gemaeht = ZW_box[c(10,11,12), "value"]
n_ungemaeht = ZW_box[c(13,14,15), "value"]
n_ruderalweg = ZW_box[c(16,17,18), "value"]

# Mean bestimmen
mean_f_gemaeht = mean(f_gemaeht$value)
mean_f_ungemaeht = mean(f_ungemaeht$value)
mean_f_ruderalweg = mean(f_ruderalweg$value)

mean_r_gemaeht = mean(r_gemaeht$value)
mean_r_ungemaeht = mean(r_ungemaeht$value)
mean_r_ruderalweg = mean(r_ruderalweg$value)

mean_n_gemaeht = mean(n_gemaeht$value)
mean_n_ungemaeht = mean(n_ungemaeht$value)
mean_n_ruderalweg = mean(n_ruderalweg$value)

#Tabelle erstellen

mean_f = c(mean_f_gemaeht, mean_f_ungemaeht, mean_f_ruderalweg)
mean_r = c(mean_r_gemaeht, mean_r_ungemaeht, mean_r_ruderalweg)
mean_n = c(mean_n_gemaeht, mean_n_ungemaeht, mean_n_ruderalweg)

mean_table = data.frame(mean_f, mean_r, mean_n)

rownames(mean_table) <- c("Gemaehte Wiese",
                          "Ungemaehte Wiese",
                          "Ruderalweg"
                          )


#### Boxplot ####
ZW_box%>%
  ggplot(aes(x=Zeigerwert, y=value)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(col=Standort), width =0.2, height = 0, size = 3)+ #damit die Punkte nicht auf einem Huafen liegen
  theme_bw()+
  scale_color_manual(values = c("Gemaehte Wiese" = "red", #Farben ?ndern
                                "Ungemaehte Wiese" = "yellow",
                                "Ruderalweg" = "blue"))+
  ggtitle("Zeigerwerte")+
  labs(x = "Zeigerwerte", y = "Zahl")



##### ALT ##########################################################################################################
pf1 = weighted.mean(DF$p1,DF$Feuchtezahl, na.rm = TRUE)
pf2 = weighted.mean(DF$p2,DF$Feuchtezahl, na.rm = TRUE)
pf3 = weighted.mean(DF$p3,DF$Feuchtezahl, na.rm = TRUE)
pf4 = weighted.mean(DF$p4,DF$Feuchtezahl, na.rm = TRUE)
pf5 = weighted.mean(DF$p5,DF$Feuchtezahl, na.rm = TRUE)
pf6 = weighted.mean(DF$p6,DF$Feuchtezahl, na.rm = TRUE)
pf7 = weighted.mean(DF$p7,DF$Feuchtezahl, na.rm = TRUE)
pf8 = weighted.mean(DF$p8,DF$Feuchtezahl, na.rm = TRUE)
pf9 = weighted.mean(DF$p9,DF$Feuchtezahl, na.rm = TRUE)

pr1 = weighted.mean(DF$p1,DF$Reaktionzahl, na.rm = TRUE)
pr2 = weighted.mean(DF$p2,DF$Reaktionzahl, na.rm = TRUE)
pr3 = weighted.mean(DF$p3,DF$Reaktionzahl, na.rm = TRUE)
pr4 = weighted.mean(DF$p4,DF$Reaktionzahl, na.rm = TRUE)
pr5 = weighted.mean(DF$p5,DF$Reaktionzahl, na.rm = TRUE)
pr6 = weighted.mean(DF$p6,DF$Reaktionzahl, na.rm = TRUE)
pr7 = weighted.mean(DF$p7,DF$Reaktionzahl, na.rm = TRUE)
pr8 = weighted.mean(DF$p8,DF$Reaktionzahl, na.rm = TRUE)
pr9 = weighted.mean(DF$p9,DF$Reaktionzahl, na.rm = TRUE)

pn1 = weighted.mean(DF$p1,DF$Naehrstoffzahl, na.rm = TRUE)
pn2 = weighted.mean(DF$p2,DF$Naehrstoffzahl, na.rm = TRUE)
pn3 = weighted.mean(DF$p3,DF$Naehrstoffzahl, na.rm = TRUE)
pn4 = weighted.mean(DF$p4,DF$Naehrstoffzahl, na.rm = TRUE)
pn5 = weighted.mean(DF$p5,DF$Naehrstoffzahl, na.rm = TRUE)
pn6 = weighted.mean(DF$p6,DF$Naehrstoffzahl, na.rm = TRUE)
pn7 = weighted.mean(DF$p7,DF$Naehrstoffzahl, na.rm = TRUE)
pn8 = weighted.mean(DF$p8,DF$Naehrstoffzahl, na.rm = TRUE)
pn9 = weighted.mean(DF$p9,DF$Naehrstoffzahl, na.rm = TRUE)


Feuchtezahl_mean = c(pf1,pf2,pf3,pf4,pf5,pf6,pf7,pf8,pf9)
Reaktionszahl_mean = c(pr1,pr2,pr3,pr4,pr5,pr6,pr7,pr8,pr9)
Naehrstoffzahl_mean = c(pn1,pn2,pn3,pn4,pn5,pn6,pn7,pn8,pn9)


cwm = data.frame(
  Standort = c("Gemaehte Wiese","Gemaehte Wiese","Gemaehte Wiese",
               "Ungemaehte Wiese","Ungemaehte Wiese","Ungemaehte Wiese",
               "Ruderalweg","Ruderalweg","Ruderalweg"),
  Feuchtezahl = Feuchtezahl_mean,
  Reaktionszahl = Reaktionszahl_mean,
  Naehrstoffzahl = Naehrstoffzahl_mean)


CWM = cwm %>%
  pivot_longer(cols = c(Feuchtezahl, Reaktionszahl, Naehrstoffzahl))


CWM%>%
  ggplot(aes(x=name, y=value)) + 
  geom_boxplot()+
  geom_jitter(aes(col=Standort), width =0.2, height = 0, size = 3)+
  theme_bw()+
  scale_color_manual(values = c("Gemaehte Wiese" = "red",
                                "Ungemaehte Wiese" = "yellow",
                                "Ruderalweg" = "blue"))+
  ggtitle("Zeigerwerte")+
  labs(x = "Zeigerwerte", y = "Zahl")











