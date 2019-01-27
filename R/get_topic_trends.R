

#' Model and summarize topic trends based on text2vec LDA
#'
#' @param doc_topic_distr
#' @param top_terms
#' @param doc_ids_years
#' @param loess_aicc_span
#' @param sep_labels
#' @param topic_selection_qt
#' @param topic_selection_top_n
#' @param topic_selection_pattern
#'
#' @return
#' @export
#'
#' @examples
#'

get_topic_trends = function(doc_topic_distr
                            , top_terms
                            , doc_ids_years
                            , loess_aicc_span = c(.1, .9)
                            , sep_labels = "#"
                            , topic_selection_qt = .95
                            , topic_selection_top_n = 10
                            , topic_selection_pattern) {
  #, models = list(...) # user definition of models to be implemented




  # years to be predicted
  # stored tabular instead of as vector because predict requires this format for newdata argument
  year_range = setnames(doc_ids_years[,.(unique(year))], "V1", "year")
  setorder(year_range, year)
  # create some names for topics and labels
  colnames(doc_topic_distr) = seq_len(ncol(doc_topic_distr))
  n_topics = ncol(doc_topic_distr)
  topic_labels = data.table(topic = as.character(seq_len(n_topics)),
                            label = stri_join_list(split(t(top_terms), seq_len(n_topics)), sep = sep_labels)
  )
  # join topic distribution with information on time
  # (since length of colnames is limited in data.table do NOT YET use full topic labels)
  # force ids to character (this is the most general case, although only integers might be present)
  doc_ids_years[,id:=as.character(id)]
  topic_trends = doc_ids_years[as.data.table(doc_topic_distr, keep.rownames="id"), on = "id"]
  # order important for calcualting cumulative sum
  setorder(topic_trends, year)
  topic_trends =  melt(topic_trends
                       ,value.name = "prob"
                       ,variable.name = "topic"
                       ,variable.factor = FALSE
                       ,id.vars = c("year", "id"))
  # statistics on aggregated values by year =================================================
  # cumsum as aggregate summary statistic of "strength/niveau" of topic prevalence
  topic_trends_aggr = topic_trends[, .(prob_sum = sum(prob)), by =  .(topic, year)][,
                                                                                    prob_cumsum:= cumsum(prob_sum), by = .(topic)]
  # add mean values per year
  topic_trends_aggr = topic_trends[, .(prob_mean = mean(prob)),
                                   by =  .(topic, year)][topic_trends_aggr , on = .(topic, year)]
  # fit and store models =================================================
  # linear model #################################################
  # NOTE the resulting object will be quite large
  #TODO
  # for 300 topics and 20 year range all 300 linear models had about 2 GB
  # if RAM is an issue, fitting lm and extracting information might be performed sequentially
  # while discarding model after extraction...
  models_linear = setnames(topic_trends[, data.table(list(lm(prob ~ year))), by = "topic"], "V1", "model")
  # generalized additive model #################################################
  #TODO currently DEACTIVATED since under discussion, see. e.g.,
  # https://stats.stackexchange.com/questions/354003/generalized-additive-model-for-timely-trends-of-topics-generated-via-latent-diri
  # maybe use betar link or set up dirichlet regression, could not produce reasonable results with these approaches, yet
  # fitting a "normal" gam still produces acceptable results but builds on an incomplete logic
  # therefore, for smoohting, only scatterplot smoohting via AICC loess is applied
  # admittedly the latter is also not perfect, resulting curves are similar to gam ouptut but a bit wigglier

  #cubic spline as basis for fitting
  #smoothing parameter is estimated via the "magic" optimizer of mgcv / also degrees of freedom k are automatically optimized
  # models_gam = topic_trends[, data.table(list(mgcv::gam(prob ~ s(year, bs = "cs", k = -1)
  #                                                       , method="GCV.Cp"))), by = "topic"]

  # hist(topics_over_time[topic == 200, log(prob)])
  # hist(rnorm(50))
  # ks.test(x=topics_over_time[topic == 1, log(prob)],y='pnorm',alternative='two.sided')
  #
  # ks.test(x=rnorm(100),y='pnorm',alternative='two.sided')
  #   int = topic_trends[year == 2016, mean(prob), by = "topic"]
  # int[, topic:= as.integer(topic)]
  # # NOTE RECENT CHANGES TOPICLABELS TOPIC IS NOW CHARACTET
  # topic_labels[int[V1 > 0.005, ] , on = "topic"]
  # MannKendall(int$V1)
  #
  #
  # require(graphics)
  #
  # ## example taken from Kendall/Stuart
  # x <- c(-50, 175, 149, 214, 247, 237, 225, 329, 729, 809,
  #        530, 489, 540, 457, 195, 176, 337, 239, 128, 102, 232, 429, 3,
  #        98, 43, -141, -77, -13, 125, 361, -45, 184)
  # x <- ts(x, start = c(1951, 1), end = c(1958, 4), frequency = 4)
  # m <- stats::decompose(x)
  # ## seasonal figure: 6.25, 8.62, -8.84, -6.03
  # round(stats::decompose(x)$figure / 10, 2)
  #
  #
  # m = topic_trends[,.SD, .SDcols = c("year", "200")]
  # setnames(m, "200", "prob")
  # m = m[, mean(prob), by = "year"]
  # MannKendall(m$V1)
  # plot(m$year, m$V1)

  # extract information from models =================================================
  # lm #################################################
  # to get topics with stongest increase/decrease
  # get slope of the model and subset topics within certain quantile of slope, e.g., 5% / 95%,
  models_linear[,  slope:= lapply(model, function(x) {x$coefficients[2]}), by = topic][, slope_sign:= sign(slope)]
  # get p_values
  models_linear[, c("p_model","p_intercept","p_slope"):=
                  models_linear[, sapply(model, function(x) as.data.frame(as.matrix(t(p_lm(x))))), by = topic][,-1]
                ]
  # add predicted values to aggregated data (aggregated because one value per year now not multiple ones as before)
  # get predicted values and 95% confidence interval
  # for (i in unique(topic_trends_aggr$topic)) { # unique because topic number exists for each year
  #   topic_trends_aggr[topic == i,  (lm_cols):=
  #                     as.data.frame(predict(models_linear[topic == i, model][[1]], newdata = year_range, interval = "confidence", level = 0.95))]
  # }
  #
  topic_trends_aggr[, c("lm_fit", "lm_lwr", "lm_upr"):=
                      as.data.frame(predict(models_linear[topic == unlist(.BY), model][[1]]
                                            , newdata = year_range, interval = "confidence", level = 0.95))
                    , by = topic]
  # gam #################################################
  # TODO DEACTIVATED see above model fitting section
  # gam_cols <- c("gam_fit", "gam_lwr", "gam_upr")
  # topic_trends_aggr[, (gam_cols) := numeric()]
  # for (i in unique(topic_trends_aggr$topic)) {
  #   pred <- predict.gam(attr_DT(topics_over_time, "gam")[topic == i, V1][[1]],  newdata = year_range, se.fit = TRUE)
  #   # https://stats.stackexchange.com/questions/33327/confidence-interval-for-gam-model
  #   # since the link function is identity no further transformation of se is necessary, otherwise see above hyperlink
  #   pred <- data.table(gam_fit = pred$fit
  #                      ,gam_lwr = pred$fit - pred$se
  #                      ,gam_upr =  pred$fit + pred$se
  #   )
  #   topic_trends_aggr[topic == i,  (gam_cols):= pred]
  # }

  # loess AICC optimized #################################################
  # NOTE: for loess fitting the AGGREGATED mean topic trends are used
  models_loess_aicc = loess_aicc_optimized(topic_trends_aggr            #<<<<
                                           ,independent_var = "year"
                                           ,dependent_var = "prob_mean" #<<<<
                                           ,groups = "topic"
                                           ,span_interval = loess_aicc_span
  )
  # add predicted values to aggregated data
  # helper function to predict loess including errors based on standard errors and 0.95 confidence interval
  predict_loess_w_err = function(m) {
    fit = predict(m, se=T)
    # confidence_interval = 0.95
    # confidence_quantile = 1-((1-confidence_interval)/2)
    # -> 0.975
    data.frame(loess_aicc_fit = fit$fit
               ,loess_aicc_lwr = fit$fit - qt(0.975,fit$df)*fit$se
               ,loess_aicc_upr= fit$fit + qt(0.975,fit$df)*fit$se
    )
  }

  topic_trends_aggr[, c("loess_aicc_fit", "loess_aicc_lwr", "loess_aicc_upr"):=
                      predict_loess_w_err(models_loess_aicc[group == unlist(.BY), model_aicc_loess][[1]])
                    , by = topic]

  # ------------------------
  # TODO adapt code (merge with non aggregated topics over time)
  # # NOTE the following may or may not be informative
  # # in the present study it was found that using different scaling/weighting applied on the ranks may result in interesting plots
  # # although the major trends can still be seen in the plots simply using prob
  # # calculation of ranks is just kept for purpose of demonstration but results are not used
  # # count ranks achieved by topic per year
  # #get topic ranks per document and count achieved ranks per year (rank 1 is best) / maximum of top 5 ranks are considered per doc
  # top_n_topics = c(1,3,5)
  # #not very elegant solution, yet...
  # for (n in top_n_topics) {
  #   # note that the number of rows in the top feature matrix equals the number of topics,
  #   # however, the rows include rank information, i.e., does topic beling to n highest ranked topics
  #   # due to ties, row numbers are not necessarily equal to rank number
  #   # rows do not necessarily provide information on the same rank number for each column
  #   # entries in the matrix are topic numbers
  #   topic_trends_ranked = top_feature_matrix(entity_feature_matrix = topics, n = n, terms = colnames(topics), include_all_ties = TRUE)
  #   # topic_trends_ranked[1:7, 1:7]
  #   #        1     2     3     4     5     6     7
  #   # [1,] "253" "93"  "226" "99"  "146" "253" "295"
  #   # [2,] "78"  "291" "92"  "100" "9"   "120" "66"
  #   # [3,] "164" "183" "101" "175" "295" "78"  "16"
  #   # [4,] "4"   "193" "153" "263" "234" "299" "221"
  #   # [5,] "276" "259" "12"  "26"  "264" "223" "272"
  #   # [6,] NA    NA    NA    NA    NA    "11"  NA
  #   # [7,] NA    NA    NA    NA    NA    NA    NA
  #   #  nrow(topics_over_time_ranked)
  #   # 300 (same as n_topics, hence, lowest rank may be 300)
  #   # ncol(topics_over_time_ranked )
  #   # 26533 (n_docs)
  #
  #   # format top feature matrix to add year information from dtm and count occurences of topics per year
  #   topic_trends_ranked = t(topics_over_time_ranked)
  #   topic_trends_ranked = as.data.table(topics_over_time_ranked, keep.rownames = "id")
  #   topic_trends_ranked[, id:= as.integer(id)][, year:=  data[topics_over_time_ranked[,"id"],  Year, on = "id"]]
  #   setorder(topics_over_time_ranked, year)
  #   topic_trends_ranked =  melt(topics_over_time_ranked
  #                                    ,value.name = "topic"
  #                                    , variable.factor = FALSE
  #                                    ,id.vars = c("year", "id"))
  #   topic_trends_ranked = topic_trends_ranked[ !is.na(topic), ]
  #   topic_trends_ranked = topic_trends_ranked[ !is.na(topic), .N, by =  c("topic", "year")]
  #   setnames(topics_over_time_ranked, "N", paste0("top", n))
  #   # merge rank information into aggregated data (adapt variable names for merge accordingly)
  #   topic_trends_ranked[, topic:= paste0("topic.", topic)]
  #   topic_trends_aggregated = topic_trends_ranked[topic_trends_aggregated , on =  c("topic", "year")]
  #   #replace NA values by zero
  #   #for full DT see https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
  #   top_n_colname = paste0("top", n)
  #   topic_trends_aggregated[is.na(eval(parse(text = top_n_colname))),(top_n_colname):= 0]
  #   #relative counts of achieved ranks per docs per year
  #   topic_trends_aggregated[,  (paste0("rel_top", n)) :=  eval(parse(text = top_n_colname))/docs_per_year]
  #   #prob_cumsum for achieved ranks
  #   topic_trends_aggregated[, (paste0("top", n, "_prob_cumsum")) := lapply(.SD, prob_cumsum), .SDcols = top_n_colname, by = topic]
  #   #get topics per year to calculate additional relative values in this respect
  #   topics_per_year = topic_trends_aggregated[eval(parse(text = top_n_colname)) > 0 , length(unique(topic)), by = year]
  #   setnames(topics_per_year, "V1",  paste0("topics_per_year_top", n))
  #   topic_trends_aggregated = topics_per_year[topic_trends_aggregated , on = "year"]
  #   #calculate additional relative values and scaled values
  #   topic_trends_aggregated[,  (paste0(top_n_colname, "_per_tps_year")) :=  eval(parse(text = top_n_colname))/eval(parse(text =  paste0("topics_per_year_top", n)))]
  # }
  #
  # ------------------------------

  # select topics =================================================
  # initialize list to store selection of topics
  # order for selecting topic_selection_top_n by slope
  setorder(models_linear, slope)
  # get local extrema to identify topics with strong fluctuation
  topic_trends_aggr[, c("prob_mean_minimum","prob_mean_maximum"):=
                      get_extrema(prob_mean, format_output = "boolean"), by = topic]

  setorder(topic_trends_aggr[,.SD[(prob_mean_minimum | prob_mean_maximum == TRUE), ][,
                                                                                     sum(abs((prob_mean-mean(prob_mean))))]
                             , by = topic], V1)[V1 >= quantile(V1, probs = topic_selection_qt), topic]


  topic_selection = list(positive_trend_all = models_linear[slope_sign == 1, topic]
                         ,negative_trend_all = models_linear[slope_sign == -1, topic]
                         ,strong_positive_trend_qt =   models_linear[slope_sign == 1 & slope >= quantile(
                           slope, probs = topic_selection_qt), topic]

                         # for negative values the quantile needs to be modified
                         # and comparison operator needs to be switched
                         ,strong_negative_trend_qt = models_linear[slope_sign == -1 & slope < quantile(slope
                                                                                                       , probs = c(1-topic_selection_qt)), topic]

                         ,strong_positive_trend_topx = models_linear[(.N-topic_selection_top_n+1):.N,topic]

                         ,strong_negative_trend_topx =  models_linear[1:topic_selection_top_n,topic]

                         ,weak_slope_qt = models_linear[abs(slope) < quantile(abs(slope)
                                                                              , probs = c(1-topic_selection_qt)),topic]

                         ,weak_slope_topx =  models_linear[order(abs(slope), decreasing = FALSE),][
                           1:topic_selection_top_n,topic]

                         ,strong_fluctuation_qt =   setorder(topic_trends_aggr[,
                                                                               .SD[(prob_mean_minimum |prob_mean_maximum == TRUE), ][,
                                                                                                                                     sum(abs(prob_mean-lm_fit))]
                                                                               , by = topic],
                                                             V1)[V1 >= quantile(V1, probs = topic_selection_qt), topic]

                         ,strong_fluctuation_topx =   setorder(topic_trends_aggr[,
                                                                                 .SD[(prob_mean_minimum |prob_mean_maximum == TRUE), ][,
                                                                                                                                       sum(abs(prob_mean-lm_fit))]
                                                                                 , by = topic],
                                                               V1)[(.N-topic_selection_top_n+1):.N, topic]

                         ,max_cumsum_qt = topic_trends_aggr[prob_cumsum >= quantile(
                           topic_trends_aggr[,max(prob_cumsum), by = topic]$V1
                           , probs = topic_selection_qt), topic]

                         ,max_cumsum_topx = setorder(topic_trends_aggr[,max(prob_cumsum), by = topic], V1)[
                           (.N-topic_selection_top_n+1):.N,topic]

                         ,my_selection = topic_labels[label %like% topic_selection_pattern, unique(topic)]
  )

  # add labels #################################################
  # labels have not been assigned as column names earlier due restriction of length of colnames in data.table
  # add them now in rows
  topic_trends_aggr = topic_labels[topic_trends_aggr , on = "topic"]

  # output list of result objects
  list(topic_trends = topic_trends
       ,topic_trends_aggr = topic_trends_aggr
       ,topic_selection = topic_selection
       ,models_loess_aicc = models_loess_aicc
       ,models_linear = models_linear
  )
}
