
a <- read.csv("./datafiles/orchard_temps.csv")
a$date <- as.POSIXlt(as.character(a$date), format = "%m/%d/%y %H:%M")

a$CP <- Dynamic_Model(a$temp)/14.4 # Scaling 
a$GDH <- 0
a$GDH[2227:length(a$GDH)] <- GDH(a$temp[2227:length(a$GDH)])
a$GDH <- a$GDH / 4648 # Scaling

a$Bing <- generate_datesims(a, "Bing")
a$Sweetheart <- generate_datesims(a, "Sweetheart")
a$Rainier <- generate_datesims(a, "Rainier")
a$Regina <- generate_datesims(a, "Regina")
a$Santina <- generate_datesims(a, "Santina")
a$KordiaC <- generate_datesims(a, "Kordia (C )")
a$KordiaM <- generate_datesims(a, "Kordia (M)")
a$Skeena <- generate_datesims(a, "Skeena")
a$Lapins <- generate_datesims(a, "Lapins")

a$GDH[which(a$date == "2017-09-17 00:00:00 PDT")] * 4848
a$Lapins[which(a$date == "2017-09-09 00:00:00 PDT")]


a$date[which(a$Bing > .5) %>% min]
a$date[which(a$Sweetheart > .5) %>% min]
a$date[which(a$Rainier > .5) %>% min]
a$date[which(a$Regina > .5) %>% min]
a$date[which(a$Santina > .5) %>% min]
a$date[which(a$KordiaC > .5) %>% min]
a$date[which(a$KordiaM > .5) %>% min]
a$date[which(a$Skeena > .5) %>% min]
a$date[which(a$Lapins > .5) %>% min]





