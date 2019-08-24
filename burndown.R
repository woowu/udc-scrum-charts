library(ggplot2); library(scales); library(RColorBrewer);

# fte_theme taken from http://minimaxir.com/2015/02/ggplot-tutorial/
fte_theme <- function() {

    # Generate the colors for the chart procedurally with RColorBrewer
    palette <- brewer.pal("Greys", n=9)
    color.background = palette[2]
    color.grid.major = palette[3]
    color.axis.text = palette[6]
    color.axis.title = palette[7]
    color.title = palette[9]

    # Begin construction of chart
    theme_grey(base_size=11) +

        # Set the entire chart region to a light gray color
        #theme(panel.background=element_rect(fill=color.background, color=color.background)) +
        #theme(plot.background=element_rect(fill=color.background, color=color.background)) +
        #theme(panel.border=element_rect(color=color.background)) +

        # Format the grid
        #theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
        #theme(panel.grid.minor=element_blank()) +
        #theme(axis.ticks=element_blank()) +

        # Format the legend, but hide by default
        theme(legend.position="right") +
        #theme(legend.background = element_rect(fill=color.background)) +
        theme(legend.text = element_text(size=11,color=color.axis.title)) +
        theme(legend.title = element_text(size=12,color=color.axis.title)) +

        # Set title and axis labels, and format these and tick marks
        theme(plot.title = element_text(color = color.title, size = 12, vjust = 1.25)) +
        #theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
        #theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
        theme(axis.title.x=element_text(size = 13, color = color.axis.title, vjust = -2)) +
        theme(axis.title.y=element_text(size = 13, color = color.axis.title, vjust = 5)) +

        # Plot margins
        theme(plot.margin = unit(c(0.5, 0.5, 0.35, 1), "cm"))
}

#pdf("plots.pdf")
svg('burndown.svg')
burndown <- read.csv("burndown.csv")
max_points <- max(burndown[[1]])
#max_sprint <- max(burndown[[2]])
max_sprint <- 24
min_sprint <- min(burndown[[2]])
velocity <- max_points / (max_sprint - min_sprint)
ystep <- max_points / 20

plot <- ggplot(burndown, aes(Sprint, Points)) +
    geom_segment(aes(x = min_sprint, y = max_points, xend = max_sprint, yend = 0), color='#878787') +
    geom_line(aes(color = burndown[[1]] > max_points - (burndown[[2]] - min_sprint) * velocity), size=1.5) +
    scale_x_continuous(limits = c(min_sprint, max_sprint), breaks = seq(min_sprint, max_sprint, by=1)) +
    scale_y_continuous(limits = c(0, max_points), breaks = seq(0, max_points, by=ystep)) +
    scale_color_manual(values = c('green', 'red'), name = 'Progress', labels = c('ahead', 'behind')) +
    fte_theme()
plot
dev.off()
