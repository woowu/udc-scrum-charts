library(ggplot2);

ystep <- 50
guide_line_color <- '#c0c0c0'
guide_line_sz <- .2
curr_sprint <- 11
annotation_sz <- 3

my_theme <- function() {
    theme_bw(base_size=12, base_family = 'SF Pro Display') +
        theme(legend.title = element_blank()) +
        theme(plot.title = element_text(vjust = 1.25, face = 'bold')) +
        theme(plot.margin = unit(c(1, .5, .75, 1), "cm"))
}

dat <- read.csv('burndown.csv')
baseline <- data.frame(dat$Sprint, dat$Baseline, rep('baseline', dim(dat)[1]))
actual <- data.frame(dat$Sprint, dat$Actual, rep('actual', dim(dat)[1]))
actual <- subset(actual, ! is.na(dat.Actual))
head <- c('sprint', 'efforts', 'type')
names(baseline) <- head
names(actual) <- head
burndown <- rbind(baseline, actual)

max_efforts <- max(burndown$efforts)
min_sprint <- min(burndown$sprint)
max_sprint <- max(burndown$sprint)
velocity <- max_efforts / (max_sprint - min_sprint)

min(actual$efforts)
plot <- ggplot(burndown, aes(sprint, efforts)) +
    geom_line(aes(color = type, size = type != 'baseline')) +
    scale_x_continuous(limits = c(min_sprint, max_sprint),
        breaks = seq(min_sprint, max_sprint, by=1), name = 'sprint (2w)') +
    scale_y_continuous(limits = c(0, max_efforts),
        breaks = seq(0, max_efforts, by=ystep), name = 'remaining effort (points)') +
    scale_size_manual(values = c(0.5, 1.2), guide = FALSE) +
    geom_segment(aes(x = curr_sprint, xend = curr_sprint, y = 0, yend = 554.40 - 10),
                 color = guide_line_color, size = guide_line_sz) +
    geom_segment(aes(x = 17, xend = 17, y = 0, yend = 373.2 - 10),
                 color = guide_line_color, size = guide_line_sz) +
    geom_segment(aes(x = 21, xend = 21, y = 0, yend = 252.4 - 10),
                 color = guide_line_color, size = guide_line_sz) +
    geom_segment(aes(x = 26, xend = 26, y = 0, yend = 101.4 - 10),
                 color = guide_line_color, size = guide_line_sz) +
    annotate('text', x = curr_sprint, y = 554.40 + 10, hjust = 0,
             size = annotation_sz, label = 'Today') +
    annotate('text', x = 17, y = 373.2 + 10, hjust = 0,
             size = annotation_sz, label = 'Arch. Matured') +
    annotate('text', x = 21, y = 252.4 + 10, hjust = 0,
             size = annotation_sz, label = 'Full Functional') +
    annotate('text', x = 26, y = 101.4 + 10, hjust = 0,
             size = annotation_sz, label = 'Feature Rich') +
    labs(title = 'Efforts Burndown',
         subtitle = 'Planned vs Actual') +
    my_theme()

svg('burndown.svg', width = 8, height = 5)
plot
dev.off()
