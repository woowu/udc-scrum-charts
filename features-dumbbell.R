library(ggplot2); library(ggalt); library(RColorBrewer);

font <- 'Roboto'
font_sz <- 16
#palette <- brewer.pal(n = 11, name = 'RdBu')
palette <- brewer.pal(n = 11, name = 'Dark2')

my_theme <- function() {
    theme_minimal(base_size = font_sz, base_family = font) +
        theme(plot.title = element_text(vjust = 1.25, face = 'bold')
              )
}

dat <- read.csv('feature-list.csv')
dat$Remaining <- dat$Efforts * (1 - dat$Completed)
dat <- dat[order(dat$Efforts),]
dat$Feature <- factor(dat$Feature, levels = as.character(dat$Feature))

max_efforts <- max(dat$Efforts)

plot <- ggplot(dat, aes(x = Remaining, xend = Efforts, y = Feature
                        , group = Efforts)) +
    geom_dumbbell(colour = palette[1]
                  , colour_xend = palette[2]
                  , size_xend = 3) +
        scale_x_continuous(limits = c(0, max_efforts)
                             , breaks = seq(0, max_efforts, by = 20)
                             , name = 'effort (points)') +
        labs(y = NULL
               , title = 'Feature Efforts'
               , subtitle = 'Day 0 vs Remainning') +
    my_theme()

ggsave(file = 'features-dumbbell.svg', width = 8, height = 5, units = 'in')
ggsave(file = 'features-dumbbell.png', width = 8, height = 5, units = 'in'
       , dpi = 96)

