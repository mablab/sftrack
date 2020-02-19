#' Methods for plot an sftraj
#'
#' @export plot.sftrack
plot.sftrack <- function(x,...){
plot(x$geometry)
burst_srt <- burst_sort(x$burst)

new_clrs <- rainbow(length(unique(burst_srt)))
  for(i in seq_along(new_clrs)){
    #i=1
    lvl <- levels(burst_srt)[i]
  plot(x$geometry[burst_lbl==lvl], type='l',add = T ,col=new_clrs[i])
  }
}

# plot(my_track)
#' @export geom_sftraj
geom_sftraj <- function(data,...){
  sub <- data[!sapply(data$geometry, function(x) all(is.na(x[[1]]))),]
  geom_sf(data = sub, aes(color=burst_sort(sub$burst)))
}
#library(ggplot2)
#ggplot(data=my_track) +geom_sf(aes(color=burst_sort(my_track$burst)))
#ggplot() + geom_sftraj(my_step)
