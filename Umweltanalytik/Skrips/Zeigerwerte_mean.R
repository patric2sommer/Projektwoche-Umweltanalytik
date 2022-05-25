library(tidyverse)
library(vegan)

Daten_Feldarbeit = read.csv("Daten_Feldarbeit.csv", header = TRUE, sep = ";")


Daten_Feldarbeit[15] = NULL
Daten_Feldarbeit

Daten_Feldarbeit[is.na(Daten_Feldarbeit)] <- 0

head(Daten_Feldarbeit)

# Evtl dataframe erstellen
diversity(Daten_Feldarbeit$`Plot.1`)
diversity(Daten_Feldarbeit$`Plot.2`)
diversity(Daten_Feldarbeit$`Plot.3`)
diversity(Daten_Feldarbeit$`Plot.4`)
diversity(Daten_Feldarbeit$`Plot.5`)
diversity(Daten_Feldarbeit$`Plot.6`)
diversity(Daten_Feldarbeit$`Plot.7`)
diversity(Daten_Feldarbeit$`Plot.8`)
diversity(Daten_Feldarbeit$`Plot.9`)

#Mean ausrechnen (N)
N = Daten_Feldarbeit %>%
summarise(
  plot1 = sum(Plot.1 * Naehrstoffzahl)/sum(Plot.1),
  plot2 = sum(Plot.2 * Naehrstoffzahl)/sum(Plot.2),
  plot3 = sum(Plot.3 * Naehrstoffzahl)/sum(Plot.3),
  plot4 = sum(Plot.4 * Naehrstoffzahl)/sum(Plot.4),
  plot5 = sum(Plot.5 * Naehrstoffzahl)/sum(Plot.5),
  plot6 = sum(Plot.6 * Naehrstoffzahl)/sum(Plot.6),
  plot7 = sum(Plot.7 * Naehrstoffzahl)/sum(Plot.7),
  plot8 = sum(Plot.8 * Naehrstoffzahl)/sum(Plot.8),
  plot9 = sum(Plot.9 * Naehrstoffzahl)/sum(Plot.9),
)
#Mean ausrechnen (R)
R = Daten_Feldarbeit %>%
  summarise(
    plot1 = sum(Plot.1 * Reaktionszahl)/sum(Plot.1),
    plot2 = sum(Plot.2 * Reaktionszahl)/sum(Plot.2),
    plot3 = sum(Plot.3 * Reaktionszahl)/sum(Plot.3),
    plot4 = sum(Plot.4 * Reaktionszahl)/sum(Plot.4),
    plot5 = sum(Plot.5 * Reaktionszahl)/sum(Plot.5),
    plot6 = sum(Plot.6 * Reaktionszahl)/sum(Plot.6),
    plot7 = sum(Plot.7 * Reaktionszahl)/sum(Plot.7),
    plot8 = sum(Plot.8 * Reaktionszahl)/sum(Plot.8),
    plot9 = sum(Plot.9 * Reaktionszahl)/sum(Plot.9),
  )
#Mean ausrechnen (F)
F = Daten_Feldarbeit %>%
  summarise(
    plot1 = sum(Plot.1 * Feuchtezahl)/sum(Plot.1),
    plot2 = sum(Plot.2 * Feuchtezahl)/sum(Plot.2),
    plot3 = sum(Plot.3 * Feuchtezahl)/sum(Plot.3),
    plot4 = sum(Plot.4 * Feuchtezahl)/sum(Plot.4),
    plot5 = sum(Plot.5 * Feuchtezahl)/sum(Plot.5),
    plot6 = sum(Plot.6 * Feuchtezahl)/sum(Plot.6),
    plot7 = sum(Plot.7 * Feuchtezahl)/sum(Plot.7),
    plot8 = sum(Plot.8 * Feuchtezahl)/sum(Plot.8),
    plot9 = sum(Plot.9 * Feuchtezahl)/sum(Plot.9),
  )

# Mean zusammenfügen
ZW = rbind(F,N,R)

rownames(ZW) <- c("Feuchtezahl","Naehrstoffzahl","Reaktionszahl")

ZW_box = ZW%>%
  pivot_longer(cols = c(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9), names_to = "Plot")

#Col "Zeigerwert" einfügen
Zeigerwert = c("Feuchtezahl","Feuchtezahl","Feuchtezahl","Feuchtezahl","Feuchtezahl","Feuchtezahl","Feuchtezahl","Feuchtezahl","Feuchtezahl",
               "Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl","Naehrstoffzahl",
               "Reaktionszahl","Reaktionszahl","Reaktionszahl","Reaktionszahl","Reaktionszahl","Reaktionszahl","Reaktionszahl","Reaktionszahl","Reaktionszahl")

ZW_box$Zeigerwert = Zeigerwert

# Standort hinzufügen
plot = ZW_box$Plot

plot[plot == "plot1"] <- "Gemaehte Wiese"
plot[plot == "plot2"] <- "Gemaehte Wiese"
plot[plot == "plot3"] <- "Gemaehte Wiese"
plot[plot == "plot4"] <- "Ungemaehte Wiese"
plot[plot == "plot5"] <- "Ungemaehte Wiese"
plot[plot == "plot6"] <- "Ungemaehte Wiese"
plot[plot == "plot7"] <- "Ruderalweg"
plot[plot == "plot8"] <- "Ruderalweg"
plot[plot == "plot9"] <- "Ruderalweg"

ZW_box$Standort = plot



# Boxplot
ZW_box%>%
  ggplot(aes(x=Zeigerwert, y=value)) + 
  geom_boxplot()+
  geom_jitter(aes(col=Standort), width =0.2, height = 0, size = 3)+
  theme_bw()+
  scale_color_manual(values = c("Gemaehte Wiese" = "red",
                                "Ungemaehte Wiese" = "yellow",
                                "Ruderalweg" = "blue"))+
  ggtitle("Zeigerwerte")+
  labs(x = "Zeigerwerte", y = "Zahl")


