
library(haven)
library(tidyverse)
library(ggplot2)
library(plotly)

# read data
adsl <- read_xpt("data/adsl.xpt")
# color 
color_fill <- c("#008080","#35aab2","#83d4da")
# create count table
dt <- adsl  %>% count(ARM,RACE) %>% group_by(ARM) %>% mutate( per = (n/ sum(n))*100) %>% mutate( count = paste0(n, " (", sprintf('%.1f', per), "%)"))
# Plot

gg_plot_theme <- function(p, y_label = "", x_label = "", title = "", legend = "bottom"){
  p <- p + theme_minimal()+
    theme(axis.text.x=element_text(color = "#008080",size = 10),
          axis.title.x=element_text(color = "#008080",size = 11),
          axis.text.y=element_text(color = "#008080",size = 12,  hjust = 0.5),
          axis.title.y=element_text(color = "#008080",size = 11),
          plot.title = element_text(hjust = 0.5,color = "#008080", size = 12, face = "bold.italic"),
          legend.text=element_text(size=10,color = "#008080"),
          legend.position = legend,
          legend.justification = "center",
          panel.border = element_blank(),
          axis.line = element_line(size = 0.2, color = "#008080"),
          plot.margin = margin(1, 0.1, 2, 0.1, "cm"),
          legend.margin=margin(-11,0,0,0)
    )+ labs (x = x_label, y = y_label, title = title)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# The bar width is too fat
p1 <- ggplot(dt, aes(y = n, x = RACE, colour = ARM, fill = ARM)) +
  geom_bar(stat = 'identity',position = position_dodge(0.95), width = 0.9) +
  geom_text(aes(label = count, colour = ARM), size = 4,vjust= -0.5, position = position_dodge(0.95)) +
  scale_color_manual("",values = c(color_fill)) + 
  scale_fill_manual('', values = c(color_fill))
p1 <- gg_plot_theme(p1, y_label = "Count", x_label = "Race", title = "Why the first bar width is so BIG?", legend = "none")
p1
# The bar width is great but the label is not correct
# using preserve = "single"
p2 <- ggplot(dt, aes(y = n, x = RACE, colour = ARM, fill = ARM)) +
  geom_bar(stat = 'identity',position = position_dodge(0.95,preserve = "single"), width = 0.9) +
  geom_text(aes(label = count, colour = ARM), size = 4,vjust= -0.5, position = position_dodge(0.95)) +
  scale_color_manual("",values = c(color_fill)) + 
  scale_fill_manual('', values = c(color_fill))
p2 <- gg_plot_theme(p2, y_label = "Count", x_label = "Race", title = "Adding preserve = 'single'", legend = "none")
p2
# Fill data with NA
dt_fill <- data.frame(dt)
dt_fill  <- complete(dt_fill,ARM,RACE)
p3 <- ggplot(dt_fill, aes(y = n, x = RACE, colour = ARM, fill = ARM)) +
     geom_bar(stat = 'identity',position = position_dodge(0.95,preserve = "single"), width = 0.9) +
     geom_text(aes(label = count, colour = ARM), size = 4,vjust= -0.5, position = position_dodge(0.95)) +
     scale_color_manual("",values = c(color_fill)) + 
     scale_fill_manual('', values = c(color_fill))
p3 <- gg_plot_theme(p3, y_label = "Count", x_label = "Race", title = " Fill data with complete()", legend = "none")
p3

p4 <- ggplot(dt_fill, aes())
p4 <- gg_plot_theme(p4, y_label = "", x_label = "", title = "", legend = "none")
p4

fig <- multiplot(p1,p4,p2,p3,cols=2)
fig

