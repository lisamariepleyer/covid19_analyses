
library(data.table)
library(ggplot2)

#dt <- data.table (dates=as.Date(c("2020-06-17", "2021-03-20", "2021-05-05", "2021-06-01", "2021-06-05", "2021-07-07")),
#                  what=factor(c("AB test", "vaccine", "AB test", "AB test", "vaccine", "AB test")),
#                  KW=c(10.5, 11, 18, 21.5, 22, 27),
#                  IgG_RBD=c(0.2, NA, 1.63, 1.34, NA, 2.14),
#                  )

dt <- data.table (dates=as.Date(c("2020-06-17", "2021-03-20", "2021-05-05", "2021-06-01", "2021-06-05", "2021-07-07", "2021-08-09", format(Sys.time(), "%Y-%m-%d"))),
                  what=c("AB test", "vaccine", "AB test", "AB test", "vaccine", "AB test", "AB test", "today"),
                  IgG_RBD=c(0.2, NA, 1.63, 1.34, NA, 2.14, 1.52, NA))

for (i in dt[what=='vaccine', which=T]) { dt[i, IgG_RBD:=dt[i-1, IgG_RBD]] }

rectangles <- data.table(ystart=c(0, 0.5, 1, 2),
                    yend=c(0.5, 1, 2, 4),
                    col=factor(c("negative", "low", "medium", "high"),
                               levels = c("high", "medium", "low", "negative")))

titerplot <- ggplot(data=dt[!is.na(IgG_RBD)]) +
  # add rectangles
  geom_rect(data=rectangles, 
            aes(xmin = as.Date(min(dt$dates)), 
                xmax = as.Date(max(dt$dates)), 
                ymin = ystart, 
                ymax = yend, 
                fill = col)) +
  scale_fill_manual(values=c("negative" = "#ffffff", 
                             "low" = "#f0f0f0", 
                             "medium" = "#d9d9d9", 
                             "high" = "#bdbdbd")) +
  geom_text (data=rectangles,
             aes(label=col,
                 x=as.Date(min(dt$dates))+3,
                 y=yend-0.15),
             hjust=0) +
  # add data
  geom_point(aes(x=dates, y=IgG_RBD, colour=what)) +
  scale_color_manual(values=c('black', '00')) +
  geom_line(aes(x=dates, y=IgG_RBD), linetype="dashed") +
  # add vaccine lines
  geom_vline(xintercept = dt[what %like% "vaccine", dates], colour="tomato") +
  geom_text(data = dt[what %like% "vaccine"],
            aes(x=dates+10, 
                y=3, 
                label=paste(c('1st', '2nd'), what)), 
            colour="tomato", angle=90) +
  # adjust plot form
  xlab("time") + ylab ("IgG RBD") +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%m-%y",
               expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'none')

pdf('ABtiter.pdf', height = 4, width = 5)
titerplot
dev.off()
  
  

