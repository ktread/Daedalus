library(googleVis)

df<- readRDS("police_final.rds")
sapply(df,class)
img.width <- 1000
img.height <- 300
#HISTOGRAM
h <- "test"

df <- df %>% 
  mutate(age.html.tooltip = paste("HI STELLA",age))

gvis.options <- list(hAxis="{title:'Age'},{opacity: 0.2}",
                     width=img.width, height=img.height, "{opacity = 0.2}")

hist <- data.frame(df$age,df$age.html.tooltip)


hist.gvis <- gvisHistogram(hist,option=gvis.options)

plot(hist.gvis)
