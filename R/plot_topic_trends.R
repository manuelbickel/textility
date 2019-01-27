

#' Generate trend plots for topics modelled with text2vec and processed with textility
#'
#' Documentation to be updated...
#'
#' @param topic_trends ...
#' @param topic_selection Named list of character vectors with the topic numbers to be plotted (and stored) under the list name.
#' @param n Number of top topics to plot per list in \code{topic_selection}.
#' @param exclude_topics A character vector giving the topic numbers to be excluded from all lists in \code{topic_selection}.
#' @param out_dir Directory where plots are saved.
#' @param filename_suffix A string to specify the end of the filename inserted before \code{.filetype}.
#' @param ylim The y limits, a numeric vector with two entries. By default \code{c(0, max(prob_mean))}.
#' @param facet_columns The number of columns to be used for the facet plot.
#' @param text_size By default \code{12}.
#' @param device The plotting device (output filetype), by default \code{"pdf"}.
#'
#' @return
#' @export
#'
#' @examples


plot_topic_trends = function(topic_trends, topic_selection, n = NULL, exclude_topics = NULL, out_dir = NULL, filename_suffix = ""
                             , ylim = NULL, facet_columns = 3, text_size = 12, device = "pdf", order_by_decreasing_trend = TRUE) {


  if (is.null(ylim)) {
    ylim = topic_trends[topic %in% unique(unlist(topic_selection)), c(0, max(prob_mean))]
  }

  if (!is.null(exclude_topics)) {
    for (i in names(topic_selection)) {
      topic_selection[[i]] = setdiff(topic_selection[[i]], exclude_topics[[i]])
    }
  }

  if (is.null(n)) {
    n = max(sapply(topic_selection, length))
  }

  if (!(substr(out_dir, nchar(out_dir), nchar(out_dir)) %in% c("\\", "/"))) {
    out_dir = paste0(out_dir, "/")
  }

  # https://stackoverflow.com/questions/15116081/controlling-order-of-facet-grid-facet-wrap-in-ggplot2
  topic_trends[, facet_group := factor(label2, levels = unique(label2))]

  if (order_by_decreasing_trend == TRUE) {
    topic_selection = lapply(topic_selection, function(x) {
      unique(topic_trends[topic %in% x, .(topic, lm_slope)])[
      order(lm_slope, decreasing = TRUE), as.character(topic)]
    })
  }

  for (i in seq_len(length(topic_selection))) {
    n_plot = ifelse(n >= length(topic_selection[[i]]), length(topic_selection[[i]]), n)
    p = ggplot(topic_trends[topic %in% topic_selection[[i]][1:n_plot], ]) +
      geom_point(aes(year, prob_mean, group = label), color = "black", size = 1, shape = 1) +
      geom_line(aes(year, loess_aicc_fit, group = label), color = "black", size = 0.7) +
      geom_line(aes(year, lm_fit , group = label), color = "black", linetype = "dashed", size = 0.7) +
      #geom_line(aes(year, prob_cumsum/(max(prob_cumsum*(1/max(prob_mean)))), group = label), color = "grey70", size = 0.7) +
      ylim(ylim) +
      ylab("expected topic probability") +
      scale_x_continuous(breaks = c(seq(min(topic_trends$year),max(topic_trends$year), 10) ,max(topic_trends$year))
                         ,limits = c(min(topic_trends$year),max(topic_trends$year))
      ) +
      #xlim(min(topic_trends_aggr$year)[1], max(topic_trends_aggr$year)[1]) +
      #geom_text(data = unique(topic_trends_aggr[topic %in% topic_selection[[i]], .(topic, label)]), aes(x = max(topic_trends_aggr$year), y = -max(topic_trends_aggr$prob_mean)/6, label = topic, group = topic)) +
      theme_bw(base_size = text_size) +
      facet_wrap( ~ facet_group, ncol = facet_columns) +
      theme(strip.text.x = element_text(size = text_size)) #+
    # theme(aspect.ratio = .5) +
    # ggtitle(
    #   paste0("topics with ", gsub("_"," ", names(topic_selection[i])), "\n"
    #          ,"GAM smoothed trend of topic probability: black", "\n"
    #          ,"Linear trend of topic probability: black dashed", "\n"
    #          ,"Cumulative sum of topic probability: grey", "\n"
    #          ,"Mean of topic probability: black dots")
    # )

    ggsave(filename = paste0(out_dir, n_topics, "_topics_", names(topic_selection)[i], filename_suffix, ".", device)
           , plot = p
           , device = device
           , width = 12
           , height = 5*ceiling(length(topic_selection[[i]][1:n_plot])/3)
           , units = "in", limitsize = FALSE)
    #ggsave(filename = paste0(dirres, n_topics, "_topics_", names(topic_selection)[i], "_final.jpg"),plot = p, device = "jpg", width = 4, height = ceiling(length(topic_selection[[i]])/4), units = "cm")
    # ggsave(filename = paste0(dirres, n_topics, "_topics_", names(topic_selection)[i], "_final.svg"), plot = p,  device = "svg", width = 4, height = ceiling(length(topic_selection[[i]])/4), units = "cm")
  } # loop over topic selection
}
