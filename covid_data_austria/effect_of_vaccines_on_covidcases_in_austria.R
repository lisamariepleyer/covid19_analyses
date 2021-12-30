library(data.table)
library(ggplot2)
library(reshape2)
#library(patchwork)

population <- 7901417 #impfbare bevoelkerung, 12+ yrs, from https://info.gesundheitsministerium.gv.at/?re=opendata

waves <- list(wave.1=c("2020-10-18", "2021-01-31"),
              wave.2=c("2021-02-01", "2021-05-09"),
              wave.3=c("2021-10-17", "2021-12-26"))

### READ IN DATA

cases <- fread("https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline.csv")
cases[, `:=` (dateID=as.Date(sapply(strsplit(as.character(Time), " "), `[`, 1), format="%d.%m.%Y"),
                        Time=NULL)]

hospitalization <- fread("https://covid19-dashboard.ages.at/data/CovidFallzahlen.csv")
hospitalization[, `:=` (dateID=as.Date(sapply(strsplit(as.character(MeldeDatum), " "), `[`, 1), format="%d.%m.%Y"),
                        MeldeDatum=NULL)]

vaccinations <- fread("https://info.gesundheitsministerium.gv.at/data/COVID19_vaccination_doses_timeline.csv")
vaccinations[, `:=` (dateID=as.Date(sapply(strsplit(as.character(date), " "), `[`, 1), format="%Y-%m-%d"),
                        date=NULL)]
vaccinations <- vaccinations[state_name=="Österreich",
                             .(cumulative.doses.per.dose.number=sum(doses_administered_cumulative)),
                             by=c("dose_number", "dateID")]
vaccinations[order(dose_number),
             population.on.dose.x:=c(cumulative.doses.per.dose.number[1]-cumulative.doses.per.dose.number[2],
                                     cumulative.doses.per.dose.number[2]-cumulative.doses.per.dose.number[3],
                                     cumulative.doses.per.dose.number[3]),
             "dateID"]
vaccinations[, dose_number:=factor(dose_number, levels = order(unique(dose_number)))]

#vaccinations[, percentage.population.on.dose.x:=population.on.dose.x/population]

### MERGE ALL DATA TABLES INTO ONE

data.factors <- data.table(factor=c("positive.cases", "covid.normal.beds", "covid.ICU.beds", "deaths", "got.dose.1", "severe.sickness", "got.dose.1", "got.dose.2", "got.dose.3"),
                           factor.name=c("new positive cases", "normal beds", "ICU beds", "deaths", "got vaccinated", "in hospital or death", "got first dose", "got second dose", "got third dose"),
                           colour=c("black", "#fcbba1", "#fb6a4a", "#cb181d", "#2171b5", "#cb181d", "#c6dbef", "#6baed6", "#2171b5"))

# the vaccination rates (600k) will be much higher values than everything else (16k)
# we could instead plot the % of population vaccinated, but then the values would be much smaller (<100)
# we scale the percentage of population vaccinated to the positive cases, then the values are ~ the same range

max.cases <- max(cases$AnzahlFaelle)

oe <- Reduce(function(...) merge(..., all = TRUE,
                                 by = "date"),
             list(cases[Bundesland=="Österreich", .(positive.cases=AnzahlFaelle,
                                                    deaths=AnzahlTotTaeglich,
                                                    date=dateID)],
                  hospitalization [Bundesland=="Alle", .(covid.ICU.beds=FZICU,
                                                         covid.normal.beds=FZHosp,
                                                         date=dateID)],
                  data.table(as.data.table(sapply(paste0("got.dose.", 1:3),
                                                  function (d) {
                                                    vaccinations[dose_number==unlist(strsplit(d, split="\\."))[3],
                                                                 cumulative.doses.per.dose.number/population*max.cases]},
                                                  USE.NAMES = T)),
                             date=unique(vaccinations$dateID))))
if (any(is.na(oe[nrow(oe)]))) oe <- oe[-nrow(oe)] # remove last row if there is data missing
for (j in (2:ncol(oe))) set(oe,which(is.na(oe[[j]])),j,0) # make all NAs zero, e.g. vaccination values before December 2020
oe[,
   severe.sickness := rowSums(.SD),
   .SDcols = c("covid.normal.beds", "covid.ICU.beds", "deaths")]

### GATHER DATA FOR WAVES

oe[,char.date:=as.character(date)]
setkey(oe, "char.date")

data.waves <- as.data.table(do.call(rbind, lapply(names(waves), function(wn) {

  w <- waves[[wn]]
  w <- seq(as.Date(w[1]), as.Date(w[2]), "days")
  return(data.table(oe[as.character(w), .(peak.bed.usage=max(severe.sickness),
                                          sum.positive.cases=sum(positive.cases))],
                    vaccinations[dose_number==1 & dateID==max(w), .(number.vaccinated.people.at.end=cumulative.doses.per.dose.number)],
                    wave=unlist(strsplit(wn, "\\."))[2],
                    start.wave=min(w),
                    end.wave=max(w),
                    median.wave=median(seq(min(w), max(w), "days"))))
})))
data.waves[,`:=` (hospital.case.ratio=peak.bed.usage/sum.positive.cases,
                  wave=factor(wave, levels = c(1:length(waves))))]

### PLOT DATA

dt.days <- nrow(vaccinations)/3 # this will be used to adjust plot parameters

# plot number of people that got vaccinated
pdf(paste0("vaccinations_dose.resolved_",format(Sys.time(), "%Y-%m-%d"), ".pdf"), width = dt.days/365*10, height = 5)
ggplot(data=vaccinations,
       aes(x=dateID,
           group=dose_number)) +
  # add lines
  geom_line(aes(y=cumulative.doses.per.dose.number,
                colour=dose_number)) +
  # add area under lines
  geom_area(aes(y=population.on.dose.x,
                fill=dose_number),
            alpha=0.5) +
  # add text on right side of plot
  geom_label(data=vaccinations[dateID==max(vaccinations$dateID)],
            aes(x=unique(dateID)+dt.days/365*20,
                y=rev(cumulative.doses.per.dose.number),
                label=paste(round(rev(cumulative.doses.per.dose.number/population)*100, 2), "%")),
            fill=rev(data.factors[factor.name %like% "dose", colour]),
            alpha=0.5) +
  # add line for impfbare bevoelkerung
  geom_hline(yintercept = population, linetype="dashed") +
  annotate(geom="text",
           x=median(vaccinations$dateID),
           y=population+population*0.025,
           label="Population >= 12 years") +
  # adjust axes
  scale_y_continuous(labels = scales::comma,
                     expand = c(0,0),
                     limits = c(0,population+population*0.05)) +
  scale_x_date(limits=c(min(vaccinations$dateID),
                        max(vaccinations$dateID)+dt.days/365*25),
               date_labels = "%b %Y") +
  labs(y="population on dose x",
       fill="dose number",
       color="dose number") +
  # beautify
  scale_fill_manual(values = data.factors[factor.name %like% "dose", colour]) +
  scale_color_manual(values = data.factors[factor.name %like% "dose", colour]) +
  theme_classic() +
  theme(axis.title.x = element_blank())
dev.off()

# made just to see how many people are on first dose over time
# idea: show how many people get first dose per day
population.on.dose.1 <-
  ggplot(data=vaccinations[dose_number==1],
         aes(x=dateID,
             y=population.on.dose.x)) +
  geom_line() +
  ylab("population on first dose") +
  scale_y_continuous(labels = scales::comma,
                     expand = c(0,0)) +
  scale_x_date(date_labels = "%b %Y") +
  theme_classic() +
  theme(axis.title.x = element_blank())

# this function will plot variables dependening on the given vars
# it will use the object oe and plot whatever column index is given in vars
plot.these.vars <- function(vars, name, wave=F) {

  # prepping data, we use reshape2 package
  ggoe <- melt(oe, id = "date", measure.vars = data.factors[vars, factor])
  ggoe$variable <- factor(ggoe$variable, levels = data.factors[vars, factor])

  # if we also want to plot wave data in trinagles, wave must be TRUE
  if (wave==T) {
    pl <- ggplot() +
      geom_rect(data=data.waves,
                aes(xmin = start.wave,
                    xmax = end.wave,
                    ymin = 1,
                    ymax = max(oe$positive.cases)),
                fill = "gray90",color="white",
                alpha=0.3)
  } else {
    pl <- ggplot()
  }

  pl <- pl +
    # we plot lines and colour them based on data.factors
    geom_line(data=ggoe, aes(x=date, y=value, group=variable, color=variable)) +
    scale_color_manual (label=data.factors[vars, factor.name],
                        values = data.factors[vars, colour]) +
    # adjust axes
    scale_x_date(date_labels = "%b %Y") +
    ylab ("# of cases") +
    # beautify
    theme_classic() +
    theme(axis.title.x = element_blank(),
          legend.title = element_blank(),
          axis.line.y = element_blank())
          #panel.grid.major.y = element_line(colour = "gray90"),
          #axis.ticks.y = element_blank())

  # if wave==T, we add the ratio of peak of sever cases over the number of new positive cases per wave
  if (wave==T) {
    pl <- pl +
      geom_label(data=data.waves,
                 aes(x=median.wave,
                     y=max.cases-(max.cases*0.03),
                     label=round(hospital.case.ratio,4)))
  }

  # if vaccination data is plotted, then we add a second axis showing the
  # percentage of vaccinated people of impfbare bevoelkerung
  if(sum(data.factors[vars, factor %like% "dose"])>0) {
    pl <- pl + scale_y_continuous(expand = c(0,0),
                                  sec.axis = sec_axis(trans=~./(max.cases/100),#~./164.75,
                                                      name="% of vaccinations",
                                                      breaks = c(25, 50,75,100),
                                                      labels = paste0(c(25, 50,75,100), "%")))
  } else {
    pl <- pl + scale_y_continuous(expand = c(0,0))
  }

  # plot using name option
  pdf(paste0(name, ".pdf"), width = 10, height = 5)
  plot(pl)
  dev.off()
}

#line.plots <- lapply(list(c(1,2,3,4,7,8,9),c(1,6,5),c(1,7,8,9)),plot.these.vars)
#wrap_plots(line.plots)

plot.these.vars(c(1,2,3,4,7,8,9), paste0("vaccinations_newcases.deaths.hospitalizations.overtime_",format(Sys.time(), "%Y-%m-%d")))
plot.these.vars(c(1,2,3,4,7,8,9), paste0("vaccinations_ratioof.severecases.over.newcases.perwave_",format(Sys.time(), "%Y-%m-%d")), w=T)
plot.these.vars(c(1,6,5), paste0("vaccinations_newcases.vs.severecases.overtime_",format(Sys.time(), "%Y-%m-%d")))
plot.these.vars(c(1,7,8,9), paste0("vaccinations_newcases.overtime_",format(Sys.time(), "%Y-%m-%d")))
