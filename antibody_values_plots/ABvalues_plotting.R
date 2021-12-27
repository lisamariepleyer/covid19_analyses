
library(data.table)
library(ggplot2)

dt <- fread("ABvalues_exampleinput.txt", 
            colClasses = c("Date", "character", "numeric", "character"), 
            fill = T)

dt[,`:=` (comment=NULL,
          what=ifelse(type %like% "test", 
                      "test", 
                      "vaccine"))]

#add today's date as data point
dt <- rbind(dt, 
            data.table(date=as.Date(format(Sys.time(), "%Y-%m-%d")),
                       type="today",
                       IgG_RBD=as.numeric(NA),
                       what="today"))

# assume last measures AB value for vaccination dates
for (i in dt[what=='vaccine', which=T]) { dt[i, IgG_RBD:=dt[i-1, IgG_RBD]] }

# as suggested by refernce data
rectangles <- data.table(ystart=c(0, 0.5, 1, 2),
                    yend=c(0.5, 1, 2, 4),
                    col=factor(c("negative", "low", "medium", "high"),
                               levels = c("high", "medium", "low", "negative")))

# set plot height
plot.height <- ceiling(max(dt[,IgG_RBD], na.rm = T))
if (plot.height < 4) {plot.height <- 4}

titerplot <- ggplot(data=dt[!is.na(IgG_RBD)]) +
  # add rectangles
  geom_rect(data=rectangles,
            aes(xmin = as.Date(min(dt$date)),
                xmax = as.Date(max(dt$date)),
                ymin = ystart,
                ymax = yend, 
                fill = col)) +
  scale_fill_manual(values=c("negative" = "#ffffff",
                             "low" = "#f0f0f0",
                             "medium" = "#d9d9d9",
                             "high" = "#bdbdbd")) +
  geom_text (data=rectangles,
             aes(label=col,
                 x=as.Date(min(dt$date))+3,
                 y=yend-0.15),
             hjust=0) +
  # add data
  geom_point(aes(x=date, y=IgG_RBD, colour=what)) +
  scale_color_manual(values=c('black', '00')) +
  geom_line(aes(x=date, y=IgG_RBD), linetype="dashed") +
  # add vaccine lines
  geom_vline(xintercept = dt[what == "vaccine", date], colour="tomato3") +
  geom_text(data = dt[what == "vaccine"],
            aes(x=date-10,
                y=3,
                label=dt[what=="vaccine", type]),
            colour="tomato3", angle=90) +
  # adjust plot form
  xlab("time") + ylab ("IgG RBD") +
  scale_y_continuous(expand = c(0,0), limits = c(0, plot.height)) +
  #coord_cartesian(ylim=c(0,max(rectangles[,yend]))) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%m-%y",
               expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'none')



pdf('ABvalues_output.pdf', height = plot.height, width = 5)
titerplot
dev.off()
