library(data.table)
library(ggplot2)

# DATEN IMPFUNGEN
impfungen <- fread("https://info.gesundheitsministerium.gv.at/data/timeline-eimpfpass.csv")
impfungen [,Datum:=as.character(Datum)]
dates <- do.call(rbind, strsplit(sapply(strsplit(impfungen$Datum, " "), `[`, 1), "-"))
impfungen [, `:=` (jahr=dates[,1], monat=dates[,2], tag=dates[,3],
                   dateID=as.Date(sapply(strsplit(impfungen$Datum, " "), `[`, 1)))]
                   #dateID=apply(dates, 1, function(x) {paste0(x[1:3], collapse = "")}))]

# DATEN COVID 19 FAELLE
faelle <- fread("https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline.csv")
dates <- do.call(rbind, strsplit(sapply(strsplit(faelle$Time, " "), `[`, 1), "\\."))
faelle [, `:=` (jahr=dates[,3], monat=dates[,2], tag=dates[,1],
                dateID=as.Date(apply(dates, 1, function(x) {paste0(x[c(3,2,1)], collapse = "-")})))]

# MERGE DATASETS 
oe <- merge(faelle[Bundesland=="Österreich", .(faelle=AnzahlFaelle, 
                                               tote=AnzahlTotTaeglich, 
                                               geheilte=AnzahlGeheiltTaeglich, 
                                               dateID)],
            impfungen [Name=="Österreich", .(teilgeimpfte=Teilgeimpfte/Bevölkerung, 
                                             vollgeimpfte=Vollimmunisierte/Bevölkerung,
                                             geimpfte=(Teilgeimpfte+Vollimmunisierte)/Bevölkerung,
                                             dateID)],
            by="dateID", all = T)


#first.vacc <- min(which(!is.na(oe$EingetrageneImpfungen)))
#oe[first.vacc:nrow(oe), add.rel.impfungen:=sapply(first.vacc:nrow(oe), 
#                             function (i) {
#                               return(oe[i,EingetrageneImpfungen]-oe[i-1, EingetrageneImpfungen])
#                              })]

differ.by.factor <- 10000
ggplot() + 
  geom_bar(aes(x=oe$dateID, y=oe$faelle), stat="identity") +
  geom_line(aes(x = oe$dateID, y = oe$geimpfte*differ.by.factor, group=1)) + 
  scale_y_continuous(name = "faelle pro tag", 
                     expand = c(0, 0),
                     sec.axis = sec_axis(~./differ.by.factor, name = "% der bevoelkerung geimpft", 
                                         labels = function(b) { paste0(round(b * 100, 0), "%")})) + 
  theme_classic() +
  theme(axis.line.y.right = element_blank())

ggplot(oe) + 
  geom_bar(aes(x=dateID, y=faelle), stat="identity") +
  geom_line(aes(x = dateID, y = geimpfte*differ.by.factor, group=1)) + 
  scale_y_continuous(name = "faelle pro tag", 
                     expand = c(0, 0),
                     sec.axis = sec_axis(~./differ.by.factor, name = "% der bevoelkerung geimpft", 
                                         labels = function(b) { paste0(round(b * 100, 0), "%")})) + 
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%m-%y",
               expand = c(0,0)) +
  theme_minimal() +
  theme(axis.line.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))


