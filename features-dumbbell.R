library(ggplot2); library(ggalt)

dat <- read.csv('feature-list.csv')
dat$Remaining <- dat$Efforts * (1 - dat$Completed)
dat <- dat[order(dat$Efforts),]
dat$Feature <- factor(dat$Feature, levels = as.character(dat$Feature))

max_efforts <- max(dat$Efforts)

plot <- ggplot(dat, aes(x = Remaining, xend = Efforts, y = Feature,
                        group = Efforts)) +
    geom_dumbbell(colour = '#a3c4dc', colour_xend = '#0e668b', size_xend = 3) +
    scale_x_continuous(limits = c(0, max_efforts),
        breaks = seq(0, max_efforts, by = 20), name = 'effort (points)') +
    labs(y = NULL,
         title = 'Efforts Comes From Features',
         subtitle = 'Day 0 vs Remainning') +
    theme(plot.title = element_text(vjust = 1.25, face = 'bold'),
          axis.ticks = element_blank(),
          plot.margin = unit(c(1, 0.5, .75, .5), "cm"))

svg('features-dumbbell.svg')
plot
dev.off()
