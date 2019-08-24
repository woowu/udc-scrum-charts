library(ggplot2); library(scales); library(RColorBrewer);

# fte_theme taken from http://minimaxir.com/2015/02/ggplot-tutorial/
fte_theme <- function() {
    palette <- brewer.pal("Greys", n=9)
    color.background = palette[2]
    color.grid.major = palette[3]
    color.axis.text = palette[6]
    color.axis.title = palette[7]
    color.title = palette[9]

    theme_grey(base_size=11) +
        theme(legend.position="right") +
        theme(legend.text = element_text(size=11,color=color.axis.title)) +
        theme(legend.title = element_blank()) +

        # Set title and axis labels, and format these and tick marks
        theme(plot.title = element_text(color = color.title, size = 12,
                                        vjust = 1.25)) +
        #theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
        #theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
        theme(axis.title.x=element_text(size = 13, color = color.axis.title,
                                        vjust = -2)) +
        theme(axis.title.y=element_text(size = 13, color = color.axis.title,
                                        vjust = 5)) +

        theme(plot.margin = unit(c(0.5, 0.5, 0.35, 1), "cm"))
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
ystep <- 50

min(actual$efforts)
plot <- ggplot(burndown, aes(sprint, efforts)) +
    geom_line(aes(color = type, size = type != 'baseline')) +
    geom_segment(aes(x = 11, xend = 11, y = 0, yend = max_efforts),
                 color = '#c0c0c0', size = 0.3, linetype = 'dashed') +
    scale_x_continuous(limits = c(min_sprint, max_sprint),
        breaks = seq(min_sprint, max_sprint, by=1), name = 'sprint (2w)') +
    scale_y_continuous(limits = c(0, max_efforts),
        breaks = seq(0, max_efforts, by=ystep), name = 'remaining points') +
    scale_size_manual(values = c(0.5, 1.2), guide = FALSE) +
    fte_theme()

svg('burndown.svg')
plot
dev.off()
