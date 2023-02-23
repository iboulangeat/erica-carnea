#--- FUNCTIONS ---
require(ade4)
#--- FUNCTIONS ---

ascgen <- function(xy = NULL, cellsize = NULL,
                     nrcol = 10, count = TRUE)
{
  ## Verifications
  if (ncol(xy)!=2)
    stop("xy should have two columns")
  
  ## Remove the possible missing values
  xy <- xy[!is.na(xy[,1]),]
  xy <- xy[!is.na(xy[,2]),]
  
  
  ## Identifies the axis on which the points cover the maximum range
  xl<-c(min(xy[,1]), max(xy[,1]))
  yl<-c(min(xy[,2]), max(xy[,2]))
  rx<-xl[2]-xl[1]
  ry<-yl[2]-yl[1]
  u<-rx
  ref<-"x"
  if (ry>rx) {
    u<-ry
    ref<-"y"
  }
  
  ## xll and yll attributes
  xll<-xl[1]
  yll<-yl[1]
  
  if (!is.null(cellsize)) {
    cx<-ceiling(rx/cellsize)+1
    cy<-ceiling(ry/cellsize)+1
    asc<-matrix(0, nrow=cx, ncol=cy)
    attr(asc, "xll")<-xll
    attr(asc, "yll")<-yll
    attr(asc, "cellsize")<-cellsize
    attr(asc, "type")<-"numeric"
    class(asc)<-"asc"
  } else {
    asc<-matrix(0, nrow=nrcol, ncol=nrcol)
    cellsize<-u/(nrcol-1)
    attr(asc, "xll")<-xll
    attr(asc, "yll")<-yll
    attr(asc, "cellsize")<-cellsize
    attr(asc, "type")<-"numeric"
    class(asc)<-"asc"
  }
  
  ## If count TRUE, the number of points is added for each pixel
  if (count) {
    kasc<-as.kasc(list(a=asc))
    asc<-count.points(xy, kasc)
  }
  
  ## Output
  return(asc)
}
#--- FUNCTIONS ---

densitePts2 <- function(focalPts,backgroundPts=NULL, density.bg=NULL, R=100, scale=TRUE) {
  
  try(lib <- require(ade4))
  if( !lib ) stop("missing library 'ade4' ")
  
  
  if(is.null(backgroundPts)) backgroundPts <- focalPts
  
  if (dim(focalPts)[1]>=5) { # verif si 5 presences minimum
    
    xmin <- min(backgroundPts[,1]) ; xmax<-max(backgroundPts[,1])
    ymin<-min(backgroundPts[,2]) ; ymax<-max(backgroundPts[,2])
    mask <- ascgen(cbind((1:R)/R,(1:R)/R),nrcol=R,count=F)
    
    if (is.null(density.bg)) 	density.bg <- matrix(1,nrow=R,ncol=R,byrow=F) 	
    # translation
    spr <- data.frame(cbind((focalPts[,1]-xmin)/abs(xmax-xmin),(focalPts[,2]-ymin)/abs(ymax-ymin)))
    
    ras = raster(mask)
    ras[] = 0
    values = unlist(lapply(split(spr, cellFromXY(ras, spr)), nrow))
    ras[as.numeric(names(values))] = as.integer(values)
    mat = rasterToPoints(ras)
    mat = mat[order(mat[,2]),]
    density.sp = matrix(0,nrow=R,ncol=R)
    density.sp[] = mat[,3]
    density.sp = density.sp/density.bg
 
    
    density.sp[which(density.sp=="Inf")]=0
    #density.sp[which(is.nan(density.sp))]=-1
    
    if(scale) 	density.sp <- density.sp/max(density.sp, na.rm = TRUE)
    
  }else{
    density.sp <- matrix(0,nrow=R,ncol=R)
  }
  
  return(density.sp)
  
}

#--- FUNCTIONS ---
# 
# densiteAb <- function(focalPts, z, backgroundPts=NULL, pts.bg=NULL, R=100, raster=FALSE) {
#   
#   try(lib <- require(ade4, raster))
#   if( !lib ) stop("missing library 'ade4' or raster")
#   
#   if(is.null(backgroundPts)) backgroundPts <- focalPts
#   
#   
#   xmin <- min(backgroundPts[,1]) ; xmax<-max(backgroundPts[,1])
#   ymin<-min(backgroundPts[,2]) ; ymax<-max(backgroundPts[,2])
#   mask <- ascgen(cbind((1:R)/R,(1:R)/R),nrcol=R,count=F)
#   
#   
#   if (is.null(pts.bg)) 	pts.bg <- rep(1,nrow(focalPts)) 	
#   if( length(z)!=length(pts.bg) ) stop("pts.bg must have the same length as z")
#   
#   # sampling
#   spr <- data.frame(cbind((focalPts[,1]-xmin)/abs(xmax-xmin),(focalPts[,2]-ymin)/abs(ymax-ymin)))
#   rasSampling = raster(mask)
#   rasSampling[] = 0
#   values = unlist(lapply(split(spr, cellFromXY(rasSampling, spr)), nrow))
#   rasSampling[as.numeric(names(values))] = as.integer(values)
#   rasSampling[] = ifelse(rasSampling[] == 0, -2, -1)
#   
#   # predicted 1
#   focalPts = focalPts[which(pts.bg==1),]
#   z = z[which(pts.bg==1)]
#   
#   spr <- data.frame(cbind((focalPts[,1]-xmin)/abs(xmax-xmin),(focalPts[,2]-ymin)/abs(ymax-ymin)))
#   
#   ras = rasSampling
#   
#   vals = lapply(split(z, cellFromXY(ras, spr)), function(x){
#     if(sum(x[x!=0])!=0){
#       res=quantile(x, probs=0.75)
#     } else { res = 0 }
#     return(res)
#   })    
#   values = as.numeric(vals)
#   names(values) = names(vals)
#   ras[as.numeric(names(values))] = as.integer(values)
#   mat = rasterToPoints(ras)
#   mat = mat[order(mat[,2]),]
#   density.sp = matrix(0,nrow=R,ncol=R)
#   density.sp[] = mat[,3]
#   
#   if(raster) return(ras)
#   else return(density.sp)
#   
# }

#--- FUNCTIONS ---

niche_space <- function(rast_env, com = NULL){
  
  require(raster)
  require(sp)
  require(lattice)
  require(ade4)
  
  ### dataset environnement et ACP
  pts = as.data.frame(rast_env)
  pts = na.omit(pts)
  pts_scaled = scale(pts)
  pca = dudi.pca(pts_scaled, scannf=F, nf=3, scale = FALSE)
  
  return(list(pca=pca, pts = pts_scaled))
}

#--- FUNCTIONS ---
# 
# sampling_density <- function (sampling_env, nspace, R=100){
#   senv = sampling_env[, which(colnames(sampling_env)%in%colnames(nspace$pca$tab))]
#   # scale env variables 
#   sampling_env_s = scale(senv, center = attr(nspace$pts,"scaled:center"), scale = attr(nspace$pts,"scaled:scale"))
#   
#   rowSampl = suprow(nspace$pca, sampling_env_s)
# 
#   dens_sampling = densitePts2(focalPts=rowSampl$lisup[,1:2], backgroundPts=nspace$pca$li[,1:2],  R=R)
#   
#   return(dens_sampling)
# }

#--- FUNCTIONS ---

# function graphiques -----
plot.pca <- function(pcaTot, col.select, w.arrows, clab.arrows){
  rangeX = abs(max(pcaTot$li[,1])-min(pcaTot$li[,1]))
  rangeY = abs(max(pcaTot$li[,2])-min(pcaTot$li[,2]))
  
  newOri = c(abs(min(pcaTot$li[,1])/rangeX),abs(min(pcaTot$li[,2])/rangeY))
  
  abline(v=newOri[1], lty=2, col=1) ; abline(h=newOri[2], lty=2, col=1)
  
  coX = (w.arrows*pcaTot$co[,1]- min(pcaTot$li[,1]))/rangeX
  coY = (w.arrows*pcaTot$co[,2] - min(pcaTot$li[,2]))/rangeY
  
  par(xpd=TRUE)
  s.arrow(cbind(coX,coY)[col.select,], label = rownames(pcaTot$co)[col.select],add.plot=T, origin = newOri, clab=clab.arrows, boxes=FALSE,  addaxes=FALSE, grid=FALSE)
  par(xpd=FALSE)
  
}
##--

plot.omi <- function(omi, col.select, w.arrows, clab.arrows){
  rangeX = abs(max(omi$ls[,1])-min(omi$ls[,1]))
  rangeY = abs(max(omi$ls[,2])-min(omi$ls[,2]))
  
  newOri = c(abs(min(omi$ls[,1])/rangeX),abs(min(omi$ls[,2])/rangeY))
  
  abline(v=newOri[1], lty=2, col=1) ; abline(h=newOri[2], lty=2, col=1)
  
  coX = (w.arrows*omi$co[,1]- min(omi$ls[,1]))/rangeX
  coY = (w.arrows*omi$co[,2] - min(omi$ls[,2]))/rangeY
  
  par(xpd=TRUE)
  s.arrow(cbind(coX,coY)[col.select,], label = rownames(omi$co)[col.select],add.plot=T, origin = newOri, clab=clab.arrows, boxes=FALSE,  addaxes=FALSE, grid=FALSE)
  par(xpd=FALSE)
  
}
##--

s.arrow <- function (dfxy, xax = 1, yax = 2, label = row.names(dfxy), clabel = 1, 
                     pch = 20, cpoint = 0, boxes = TRUE, edge = TRUE, origin = c(0, 
                                                                                 0), xlim = NULL, ylim = NULL, grid = TRUE, addaxes = TRUE, 
                     cgrid = 1, sub = "", csub = 1.25, possub = "bottomleft", 
                     pixmap = NULL, contour = NULL, area = NULL, add.plot = FALSE) 
{
  arrow1 <- function(x0, y0, x1, y1, len = 0.1, ang = 15, lty = 1, 
                     edge) {
    d0 <- sqrt((x0 - x1)^2 + (y0 - y1)^2)
    if (d0 < 1e-07) 
      return(invisible())
    segments(x0, y0, x1, y1, lty = lty)
    h <- strheight("A", cex = par("cex"))
    if (d0 > 2 * h) {
      x0 <- x1 - h * (x1 - x0)/d0
      y0 <- y1 - h * (y1 - y0)/d0
      if (edge) 
        arrows(x0, y0, x1, y1, ang = ang, len = len, 
               lty = 1)
    }
  }
  dfxy <- data.frame(dfxy)
  opar <- par(mar = par("mar"))
  on.exit(par(opar))
  par(mar = c(0.1, 0.1, 0.1, 0.1))
  coo <- scatterutil.base(dfxy = dfxy, xax = xax, yax = yax, 
                          xlim = xlim, ylim = ylim, grid = grid, addaxes = addaxes, 
                          cgrid = cgrid, include.origin = TRUE, origin = origin, 
                          sub = sub, csub = csub, possub = possub, pixmap = pixmap, 
                          contour = contour, area = area, add.plot = add.plot)
  if (grid & !add.plot) 
    scatterutil.grid(cgrid)
  if (addaxes & !add.plot) 
    abline(h = 0, v = 0, lty = 1)
  if (cpoint > 0) 
    points(coo$x, coo$y, pch = pch, cex = par("cex") * cpoint)
  for (i in 1:(length(coo$x))) arrow1(origin[1], origin[2], 
                                      coo$x[i], coo$y[i], edge = edge)
  if (clabel > 0) 
    scatterutil.eti.circ(coo$x, coo$y, label, clabel, origin, 
                         boxes)
  if (csub > 0) 
    scatterutil.sub(sub, csub, possub)
  # box()
  invisible(match.call())
}



plot_niche <- function(pca, density.pts,
                       colo.pts = c("lightyellow", colorRampPalette(c("lightblue", "violet", "darkred"), space="Lab")(5)),
                       tit.legend = "probability \n density",
                       at.pts = c(-0.1, 0.001, 0.2, 0.4, 0.6, 0.8, 1 ),
                       at.scaleLab = c(0,0, 0.2, 0.4, 0.6, 0.8, 1), 
                       at.scalePts= 0:6,
                       w.arrows=5,
                       clab.arrows=2, 
                       col.select = 1:ncol(pca$tab), 
                       omi=FALSE
){
  
  #-- layout
  # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  
  layout(matrix(c(1, 2), ncol = 2, byrow=T), widths = c(lcm(5), 1), heights=1)
  
  #-- legend density 
  par(mar = c(1,1,4,5))
  plot.new()
  plot.window(xlim = c(0.5, 1), ylim = range(at.scalePts), xaxs = "i",yaxs = "i")
  rect(0.7, at.scalePts[-length(at.scalePts)], 1, at.scalePts[-1], col = colo.pts)
  axis(4, las=1, cex.axis=1.2, at=at.scalePts, labels=at.scaleLab )
  title(paste0("     ",tit.legend))
  
  #-- 
  par(mar = c(0.1, 1, 1, 0.1))
  plot.new()
  plot.window(c(0,1), c(0,1), "", xaxs = "i", yaxs = "i", asp = 1)
  z= density.pts
  x = seq(0, 1, length.out = nrow(z))
  y = seq(0, 1, length.out = ncol(z))
  .filled.contour(as.double(x), as.double(y), z, as.double(at.pts), col = colo.pts)
  if(omi){
    plot.omi(pca, w.arrows = w.arrows, clab.arrows=clab.arrows, col.select = col.select)
  }else plot.pca(pca, w.arrows = w.arrows, clab.arrows=clab.arrows, col.select = col.select)
  
  
}

