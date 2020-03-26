###################
# Misc utilitie functions
###################
#'@export
pts_traj <- function(traj){
  if(inherits(traj,'sftraj')){pts <- traj[,attr(traj,'sf_column')]}
  if(inherits(traj, 'sfc')){pts <- traj}
  if('XY'%in%class(pts[[1]])){dim=c('X','Y')}else{dim=c('X','Y','Z')}
  lapply(pts, function(x){
    if(inherits(x,'GEOMETRYCOLLECTION')) { x[1][[1]]} else{ st_point(st_coordinates(x)[1,dim])}
  })
}

#'@export
coord_traj <- function(traj){
  if(inherits(traj,'traj')){pts <- traj[,attr(traj,'sf_column')]}
  if(inherits(traj, 'sfc')){pts <- traj}
  if('XY'%in%class(pts[[1]])){dim=c('X','Y')}else{dim=c('X','Y','Z')}
  ret <- lapply(pts, function(x){
    if(inherits(x,'GEOMETRYCOLLECTION')) { st_coordinates(x[1][[1]])} else{ st_coordinates(x)[1,dim]}
  })
  do.call(rbind,ret)
}

#'@export
is_linestring <- function(traj){
  if(inherits(traj,'sftraj')){pts <- traj[,attr(traj,'sf_column')]}
  if(inherits(traj, 'sfc')){pts <- traj}
  if('XY'%in%class(pts[[1]])){dim=c('X','Y')}else{dim=c('X','Y','Z')}
  sapply(traj, function(x) inherits(x, 'LINESTRING'))
}

#'@export
summary_sftrack <- function(x){
  track_class <- class(x)[1]
  #x = my_sftrack
  time_col <- attr(x,'time')
  error_col <- attr(x, 'error')
  sf_col <- attr(x,'sf_column')
  sub <- x[,colnames(x)%in%c(time_col,error_col,'burst',sf_col)]
  levelz <- attr(x$burst,'sort_index')
  statz <- tapply(sub[,time_col],levelz, function(x)list('begin'=min(x),'end'=max(x),'points'=length(x)))
  statz

  if(track_class=='sftrack'){
    lenz <- tapply(sub[,sf_col], levelz, function(pts) {
      new_pts <- pts[!sapply(pts, st_is_empty)]
      st_length(st_linestring(st_coordinates(new_pts)))
    })
  }
  if(track_class=='sftraj'){
    lenz <- tapply(sub[,sf_col], levelz, function(pts) {
      mat <- coord_traj(pts)
      st_length(st_linestring(na.omit(mat)))
    })
  }
  points = vapply(statz, FUN = function(x)x$points, numeric(1))
  begin_time = lapply(statz, function(x)x$begin)
  end_time = lapply(statz, function(x)x$end)
  class(begin_time) <- class(end_time) <- c("POSIXct", "POSIXt")
  attr(begin_time, "tzone") <- attr(end_time, "tzone") <- attr(x[,attr(x,'time')], "tzone")
  data.frame(points, begin_time,end_time,length=lenz,row.names=levels(levelz))

}
