library(ggplot2); library(purrr); library(RColorBrewer);

ystep <- 100
guide_line_color <- '#c0c0c0'
guide_line_sz <- .2
curr_sprint <- 11
annotation_point_sz <- 3
tick_label_color <- brewer.pal(n = 8, name = 'Dark2')[8]
highlight_color <- brewer.pal(n = 8, name = 'Dark2')[4]
annotation_color <- brewer.pal(n = 8, name = 'Dark2')[4]
font <- 'Roboto'
font_sz <- 16

mid <- function(min, max, dist) {
    ceiling(min + (max - min) * dist)
}

dat <- read.csv('burndown.csv')
forecast <- data.frame(dat$sprint, dat$forecast, rep('forecast', dim(dat)[1]))
actual <- data.frame(dat$sprint, dat$actual, rep('actual', dim(dat)[1]))
actual <- subset(actual, ! is.na(dat.actual))
head <- c('sprint', 'efforts', 'type')
names(forecast) <- head
names(actual) <- head
burndown <- rbind(forecast, actual)
forecast <- subset(burndown, burndown$type == 'forecast')

max_efforts <- max(burndown$efforts)
min_sprint <- min(burndown$sprint)
max_sprint <- max(burndown$sprint)
sprints_num <- nrow(forecast)
velocity <- max_efforts / (max_sprint - min_sprint)

key_sprints <- forecast[c(mid(1, sprints_num, .2)
                          , mid(1, sprints_num, .4)
                          , mid(1, sprints_num, .6)
                          , mid(1, sprints_num, .8)),]
key_sprints$name <- c('MVP'
                      , 'Arch. Mature'
                      , 'Full Functional'
                      , 'Feature Rich');
sprint_colors <- rep(tick_label_color, sprints_num)
sprint_colors[curr_sprint - min_sprint + 1] <- highlight_color

my_theme <- function() {
    theme_minimal(base_size = font_sz, base_family = font) +
        theme(legend.title = element_blank()) +
        theme(plot.title = element_text(vjust = 1.25, face = 'bold')) +
        theme(axis.text.x = element_text(color = sprint_colors)) +
        theme(axis.text.y = element_text(color = tick_label_color))
}

plot <- ggplot(burndown, aes(sprint, efforts)) +
    geom_line(aes(color = type, size = type != 'forecast')) +
    scale_color_brewer(palette = 'Dark2') +
    scale_x_continuous(limits = c(min_sprint, max_sprint),
        breaks = seq(min_sprint, max_sprint, by=1), name = 'sprint (2w)') +
    scale_y_continuous(limits = c(0, max_efforts)
        , breaks = seq(0, max_efforts, by=ystep)
        , name = 'remaining effort (points)') +
    scale_size_manual(values = c(0.5, 1.2), guide = FALSE) +
    geom_point(data = key_sprints, aes(sprint, efforts), shape = 1
               , size = annotation_point_sz
               , color = tick_label_color) +
    geom_text(data = key_sprints, aes(sprint, efforts, label = name)
              , color = annotation_color
              , hjust = -.1, vjust = -1) +
    labs(title = 'Efforts Burndown w/ Release Milestones'
         , subtitle = 'Forecast vs Actual') +
    my_theme()

ggsave(file = 'burndown.svg', width = 8, height = 6, units = 'in')
ggsave(file = 'burndown.png', width = 8, height = 6, units = 'in'
       , dpi = 300)

