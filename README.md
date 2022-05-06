Gggplot tricks
--------------

### Bar plot width

Why the bar widths were so *fat* ?????? when the levels of categorical
variable were missing.

Let’s fix it.

``` r
# read data
adsl <- read_xpt("data/adsl.xpt")
color_fill <- c("#008080","#35aab2","#83d4da")
# create count table
dt <- adsl  %>% count(ARM,RACE) %>% group_by(ARM) %>% mutate( per = (n/ sum(n))*100) %>% mutate( count = paste0(n, " (", sprintf('%.1f', per), "%)"))
source("source/trick_20220506.R")
```

![](friday-tricks_files/figure-markdown_github/unnamed-chunk-1-1.png)

The bar width is too big???? What happened?

``` r
p1 <- ggplot(dt, aes(y = n, x = RACE, colour = ARM, fill = ARM)) +
  geom_bar(stat = 'identity',position = position_dodge(0.95), width = 0.9) +
  geom_text(aes(label = count, colour = ARM), size = 4,vjust= -0.5, position = position_dodge(0.95)) +
  scale_color_manual("",values = c(color_fill)) + 
  scale_fill_manual('', values = c(color_fill))
p1 <- gg_plot_theme(p1, y_label = "Count", x_label = "Race", title = "Why the first bar width is so BIG?", legend = "none")
p1
```

![](friday-tricks_files/figure-markdown_github/unnamed-chunk-2-1.png)

![plot](figures/p1.png)
