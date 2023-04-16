devtools::load_all()
temp <- format_events(events)
temp
ggtimeline_core(data = temp,
           group = group,
           label= label,
           start = start,
           end = end,
           y_position = y_position) +
  ggtimeline_format(data = temp)



# x <- ggplot(mtcars, aes(x = mpg, y = cyl)) + geom_point()
# x + geom_smooth()
# y <- list(geom_smooth(), theme_classic())
# x + y
