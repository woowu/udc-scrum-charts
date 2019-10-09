library(ggplot2); library(ggalt); library(RColorBrewer);

font <- 'Roboto'
font_sz <- 16
palette <- brewer.pal(n = 8, name = 'Dark2')

my_theme <- function() {
    theme_minimal(base_size = font_sz, base_family = font) +
        theme(plot.title = element_text(vjust = 1.25, face = 'bold')
              )
}

dat <- read.csv('feature-list.csv')
dat$remaining <- dat$efforts * ifelse(dat$completed < 1, 1 - dat$completed, 0)

dat <- dat[order(dat$efforts),]
dat$feature <- factor(dat$feature, levels = as.character(dat$feature))

max_efforts <- max(dat$efforts)

plot <- ggplot(dat, aes(x = remaining, xend = efforts, y = feature
                        , group = efforts)) +
    geom_dumbbell(colour = palette[1]
                  , colour_xend = palette[2]
                  , size_xend = 3) +
        scale_x_continuous(limits = c(0, max_efforts)
                             , breaks = seq(0, max_efforts, by = 20)
                             , name = 'effort (points)') +
        labs(y = NULL
               , title = 'PBI Estimation'
               , subtitle = 'Day 0 vs Remainning') +
    my_theme()

ggsave(file = 'features-dumbbell.svg', width = 8, height = 6, units = 'in')
ggsave(file = 'features-dumbbell.png', width = 8, height = 6, units = 'in'
       , dpi = 300)

