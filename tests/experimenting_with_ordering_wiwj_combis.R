
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

