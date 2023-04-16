#' ggtimeline
#'
#' @param data a data frame
#' @param group data to be displayed along a single track of the timeline
#' @param label text label for the starting point of each segment in the track
#' @param start starting position of a segment on the timeline
#' @param end end position of a segment on the timeline
#' @param y_position track position
#'
#' @import ggplot2 dplyr
#' @return fdsa
#' @export
#'
ggtimeline <- function(data, group, label, start, end, y_position, layout = "mirrored"){
  group <- enquo(group)
  label <- enquo(label)
  start <- enquo(start)
  end <- enquo(end)
  y_position <- enquo(y_position)

  numberLine_df <- tibble::tibble(x = -10:15, ymin = -0.05, ymax = 0.05)
  label_size <- 6
  axis_size <- 1.5
  xtext_size <- 16
  xlabel_size <- 18
  legend_size <- 18
  element_thickness <- 5
  timeline_size <- 1.5
  min_date <- min(pull(data, start))
  max_date <- max(pull(data, end))
  max_track <- max(abs(pull(data, y_position)))

  ggplot(data = data, aes(x=!!start, xmin = !!start, xmax = !!end, y = !!y_position, color=!!group, fill=!!group)) +
    geom_hline(yintercept=0, color = "black", size=timeline_size) + # Base strip of timeline
    geom_segment(inherit.aes = F, data = numberLine_df, mapping = aes(x=x,y=ymin,xend=x,yend=ymax), size = timeline_size)+ # Timeline ticks
    theme_classic()+
    scale_x_continuous(breaks = seq(min_date,max_date,1)) +
    scale_y_continuous(expand = expansion(mult = 0.5), limits = c(-max_track, max_track))+
    coord_cartesian(ylim = c(-max_track,max_track), xlim = c(min_date,max_date))+
    geom_errorbarh(height = 0, size = element_thickness)+ # Size = thickness of timeline element duration
    geom_point(size=element_thickness, color = "black") +
    geom_point(size=element_thickness*(3/5))+
    theme(axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line(size = axis_size),
          axis.ticks.x = element_line(size = axis_size),
          axis.text.x = element_text(size = xtext_size),
          axis.title.x = element_text(size = xlabel_size),
          legend.text = element_text(size = legend_size),
          axis.ticks.length=unit(0.1*axis_size,"cm"),
          legend.position = "bottom") +
    guides(fill = "none", color = guide_legend(nrow = 1)) +
    labs(y="",x="Days", color = "") +
    ggrepel::geom_label_repel(data = temp %>% filter(group == "Tacrolimus"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) +
    ggrepel::geom_label_repel(data = temp %>% filter(group == "ATG"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) +
    ggrepel::geom_label_repel(data = temp %>% filter(group == "Fludarabine"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) +
    ggrepel::geom_label_repel(data = temp %>% filter(group == "Busulfan"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) +
    ggrepel::geom_label_repel(data = temp %>% filter(group == "Rituximab"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) +
    ggrepel::geom_label_repel(data = temp %>% filter(group == "Methotrexate"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) +
    ggrepel::geom_label_repel(data = temp %>% filter(group == "Transplant"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) +
    guides(fill = guide_legend(nrow = 1, override.aes = aes(color = NA), title = ""), color = FALSE) +
    scale_color_brewer(palette = "Set2") +
    scale_fill_brewer(palette = "Set2")
}


#' Format
#'
#' @param df  fdsa
#'
#' @return fds
#' @export
#'
format_events <- function(df){
  temp <- tibble::tibble(data=list(
    # y_position = offset of element from timeline
    # nudge_position = distance of label from element
    list(group = "Tacrolimus", y_position = -3, nudge_position = -1.5),
    list(group = "ATG", y_position = 1, nudge_position = 0.75),
    list(group = "Fludarabine", y_position = -2, nudge_position = -1.25),
    list(group = "Busulfan", y_position = -1, nudge_position = -0.75),
    list(group = "Rituximab", y_position = 2, nudge_position = 1.25),
    list(group = "Methotrexate", y_position = 3, nudge_position = 1.55),
    list(group = "Transplant", y_position = 0, nudge_position = -0.25))) %>%
    tidyr::unnest_wider(data) %>%
    mutate(nudge_position = ifelse(y_position==0,-1,y_position*1.25))
  df %>% left_join(temp)
}


#' ggtimeline_core
#'
#' @param data s
#' @param group s
#' @param label s
#' @param start s
#' @param end s
#' @param y_position s
#' @param layout s
#'
#' @return s
#' @export
#'
ggtimeline_core <- function(data, group, label, start, end, y_position, layout = "mirrored"){
  group <- enquo(group)
  label <- enquo(label)
  start <- enquo(start)
  end <- enquo(end)
  y_position <- enquo(y_position)

  numberLine_df <- tibble::tibble(x = -10:15, ymin = -0.05, ymax = 0.05)

  element_thickness <- 5
  timeline_size <- 1.5


  ggplot(data = data, aes(x=!!start, xmin = !!start, xmax = !!end, y = !!y_position, color=!!group, fill=!!group)) +
    theme_classic()+
    geom_hline(yintercept=0, color = "black", size=timeline_size) + # Central strip of timeline for mirrored format
    geom_segment(inherit.aes = F, data = numberLine_df, mapping = aes(x=x,y=ymin,xend=x,yend=ymax), size = timeline_size)+ # Timeline ticks
    geom_errorbarh(height = 0, size = element_thickness)+ # Size = thickness of a timeline segment
    geom_point(size=element_thickness, color = "black") + # Outline of a point that starts a segment
    geom_point(size=element_thickness*(3/5)) # Core of a point that starts a segment

}




#' ggtimeline_format
#'
#' @return fd
#' @export
#'
ggtimeline_format <- function(data) {
  min_date <- min(pull(data, start))
  max_date <- max(pull(data, end))
  max_track <- max(abs(pull(data, y_position)))    # +
  label_size <- 6
  axis_size <- 1.5
  xtext_size <- 16
  xlabel_size <- 18
  legend_size <- 18

  list(

  scale_x_continuous(breaks = seq(min_date,max_date,1)),
  scale_y_continuous(expand = expansion(mult = 0.5), limits = c(-max_track, max_track)),
  coord_cartesian(ylim = c(-max_track,max_track), xlim = c(min_date,max_date)),
    theme(axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.x = element_line(size = axis_size),
          axis.ticks.x = element_line(size = axis_size),
          axis.text.x = element_text(size = xtext_size),
          axis.title.x = element_text(size = xlabel_size),
          legend.text = element_text(size = legend_size),
          axis.ticks.length=unit(0.1*axis_size,"cm"),
          legend.position = "bottom") ,
    guides(fill = "none", color = guide_legend(nrow = 1)) ,
    labs(y="",x="Days", color = "") ,
    ggrepel::geom_label_repel(data = temp %>% filter(group == "Tacrolimus"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) ,
    ggrepel::geom_label_repel(data = temp %>% filter(group == "ATG"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) ,
    ggrepel::geom_label_repel(data = temp %>% filter(group == "Fludarabine"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) ,
    ggrepel::geom_label_repel(data = temp %>% filter(group == "Busulfan"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) ,
    ggrepel::geom_label_repel(data = temp %>% filter(group == "Rituximab"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) ,
    ggrepel::geom_label_repel(data = temp %>% filter(group == "Methotrexate"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) ,
    ggrepel::geom_label_repel(data = temp %>% filter(group == "Transplant"), aes(nudge_y = nudge_position, label = label), box.padding = 0.05, max.overlaps = Inf, color = "black", size = label_size) ,
    guides(fill = guide_legend(nrow = 1, override.aes = aes(color = NA), title = ""), color = FALSE) ,
    scale_color_brewer(palette = "Set2") ,
    scale_fill_brewer(palette = "Set2")
  )
}
