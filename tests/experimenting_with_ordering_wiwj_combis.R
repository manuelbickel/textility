
library(data.table)

beta <- rbind(c(0.01, 0.4, 0.25, 0.1, 0.19, 0.05),
               c(0.05, 0.19, 0.10, 0.25, 0.40, 0.01),
               c(0.4, 0.1, 0.05, 0.19, 0.25, 0.01),
               c(0.01, 0.1, 0.25, 0.05, 0.19, 0.4))

colnames(beta) <- c(letters[1:6])
rowSums(beta)

dtm <- rbind(c(0,2,1,0,3,1),
             c(1,1,0,1,2,0),
             c(0,0,1,2,1,2),
             c(1,1,0,1,0,1),
             c(2,2,0,0,2,1))
colnames(dtm) <- c(letters[1:6])


n <- 4

topic_coherence <- data.table(Topic = paste0("T", 1:nrow(beta)))
idxs_topwords_topic <- apply(beta, 1, order, decreasing=TRUE, method = "radix")[1:n,]
t(idxs_topwords_topic)
# [1,]    2    3    5    4
# [2,]    5    4    2    3
# [3,]    1    5    4    2
# [4,]    6    3    5    2
idxs_topwords_unique <- unique(as.vector(idxs_topwords_topic))
#[1] 2 3 5 4 1 6
dtm_topwords <- dtm[,idxs_topwords_unique]
dtm_topwords[dtm_topwords>0] <- 1
tcm <- t(dtm_topwords) %*% dtm_topwords
tcm
#   b c e d a f
# b 4 1 3 2 3 3
# c 1 2 2 1 0 2
# e 3 2 4 2 2 3
# d 2 1 2 3 2 2
# a 3 0 2 2 3 2
# f 3 2 3 2 2 4

#TODO
#order columns of tcm from high to low entries for [wi,wi]
# reorder_decr <- order(diag(tcm), decreasing = TRUE, method = "radix")
# tcm <- tcm[reorder_decr, reorder_decr]
# idxs_topwords_unique <- idxs_topwords_unique[reorder_decr]

as.vector(idxs_topwords_topic)
#[1] 2 3 5 4 5 4 2 3 1 5 4 2 6 3 5 2
idxs_topwords_unique
#[1] 2 3 5 4 1 6


match <- split(match(as.vector(idxs_topwords_topic), idxs_topwords_unique)
               , rep(1:nrow(beta), each=n))
# $`1`
# [1] 1 2 3 4
# $`2`
# [1] 3 4 1 2
# $`3`
# [1] 5 3 4 1
# $`4`
# [1] 6 2 3 1

reorder <- order(diag(tcm),  decreasing = TRUE)
#tcm_ord <- tcm[reorder, reorder]
idxs_topwords_unique_ord <- idxs_topwords_unique[reorder]
#[1] 2 5 6 4 1 3

idxs_topwords_unique_ord <- match(idxs_topwords_unique_ord, idxs_topwords_unique)
#[1] 1 3 6 4 5 2


match_order <- lapply(match, function(x) {
  matchorder <- match(x, idxs_topwords_unique_ord)
  x[order(matchorder, decreasing = TRUE)]
})
# $`1`
# [1] 2 4 3 1
# $`2`
# [1] 2 4 3 1
# $`3`
# [1] 5 4 3 1
# $`4`
# [1] 2 6 3 1

#check if both sets contain the same indices for each each row
for (i in 1:length(match)) {
  print(all(sort(match_order[[i]]) ==  sort(match[[i]])))
}
# [1] TRUE
# [1] TRUE
# [1] TRUE
# [1] TRUE

lapply(match, function(x) {
  tcm[x,x]
})
# $`1`
# b c e d
# b 4 1 3 2
# c 1 2 2 1
# e 3 2 4 2
# d 2 1 2 3
#
# $`2`
# e d b c
# e 4 2 3 2
# d 2 3 2 1
# b 3 2 4 1
# c 2 1 1 2
#
# $`3`
# a e d b
# a 3 2 2 3
# e 2 4 2 3
# d 2 2 3 2
# b 3 3 2 4
#
# $`4`
# f c e b
# f 4 2 3 3
# c 2 2 2 1
# e 3 2 4 3
# b 3 1 3 4

lapply(match_order, function(x) {
  tcm[x,x]
})
# $`1`
# c d e b
# c 2 1 2 1
# d 1 3 2 2
# e 2 2 4 3
# b 1 2 3 4
#
# $`2`
# c d e b
# c 2 1 2 1
# d 1 3 2 2
# e 2 2 4 3
# b 1 2 3 4
#
# $`3`
# a d e b
# a 3 2 2 3
# d 2 3 2 2
# e 2 2 4 3
# b 3 2 3 4
#
# $`4`
# c f e b
# c 2 2 2 1
# f 2 4 3 3
# e 2 3 4 3
# b 1 3 3 4




###comparison of different approaches to create wi wij combis

#CREATE SETS OF wi/wj (functions)
#following approach was taken from textmineR package and turned into generalized function
#to create indices of token combinations to extract their probabilities from tcm to calcualte, e.g, P(wi|wj))
#resembles utils::combn(idxs, 2) but is slightly faster for higher number of idxs
#another base R implementation via grid() is used in stm package
#S_one_one - all word pair combinations

idxs <- 11:99
choose(3,2)


create_wiwj_sym(idxs)
t(combn(idxs,2))
t(combn(idxs,2, FUN = sort))


a <- create_wiwj_asym(idxs)
b <- t(combn(idxs,2, FUN = function(x) sort(x, decreasing = T)))
all(a[order(a[1,]), ] == b[order(b[1,]), ])

create_wiwj_sym <- function(idxs) {
  #from high to low prob, so that p(wi) > p(wj) starting from the left in the tcm
  #idxs <- sort(idxs, decreasing = FALSE)

  do.call(rbind,
          sapply(1:(length(idxs)-1), function(x) {
            cbind(wi = rep(idxs[x], length(idxs[(x + 1):length(idxs)]))
                  ,wj = idxs[(x + 1):length(idxs)])
          } , USE.NAMES = F))
}



create_wiwj_sym <- function(idxs) {
  #from high to low prob, so that p(wi) > p(wj) starting from the left in the tcm
  #idxs <- sort(idxs, decreasing = FALSE)
  do.call(rbind,
          sapply(1:(length(idxs)-1), function(x) {
            cbind(wi = rep(idxs[x], length(idxs[(x + 1):length(idxs)]))
                  ,wj = idxs[(x + 1):length(idxs)])
          } , USE.NAMES = F))
}

#S_one_pre - required for asymmetric UMass measure
create_wiwj_asym <- function(idxs) {
  #to comply with stm results idxs have to be reorderd
  idxs <- idxs[order(idxs, decreasing = F, method = "radix")]
  do.call(rbind,
          sapply(2:length(idxs), function(x) {
            cbind(wi = rep(idxs[x], length(idxs[1:length(idxs[1:(x-1)])]))
                  ,wj = idxs[1:length(idxs[1:(x-1)])])
          }, USE.NAMES = FALSE))
}





# top <- c("C", "B", "A", "B", "A", "C", "A", "B", "C")
# top_unq <- unique(top) #the original unique order to subset tcm
# reorder <- c(2,1,3) #assumed reorder
# top_unq_ord <- top_unq[reorder]
# idxs <- c(1,2)
# idxs <- idxs[match(idxs, reorder)]
# top_unq_idxs <-  top_unq[c(1,2)] #the selected elements from this order
# top_unq_reorder <-  top_unq[prob_order] #the current order in tcm


#       top_unq_re <-   top_unq[reorder]
#       idx_names <- rbind(idxs, colnames(tcm)[idxs])
#       A B C D E
#       1 2 3 4 5
#
#       C B A E D
#       3 2 1 5 4
#
#       A B D
#       3 2 5
#
#       restore_order <- match(1:5, c(3,2,1,5,4))
#
#       match(c(3,2,5), restore_order)
#
