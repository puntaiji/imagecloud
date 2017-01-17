require("wordcloud")
require("jpeg")

#edit from wordcloud library

imagecloud <- function (words, freq, max.size = 4,min.size = 0.5 , min.freq = 1, max.words = Inf, 
  random.order = TRUE, random.color = FALSE, rot.per = 0.1, 
  colors = "black", ordered.colors = FALSE, use.r.layout = FALSE, 
  fixed.asp = TRUE,save.png = TRUE,png.w = 300,png.h = 300,png.unit = "mm", filename="def.png" ,...) 
{
  scale <- c(max.size,min.size)
  #set output to png
  if(save.png){
    png(filename = filename,width = 300,height = 300, units = "mm" ,res = 600,bg='white')
  }
  if (!fixed.asp && rot.per > 0) 
    stop("Variable aspect ratio not supported for rotated words. Set rot.per=0.")
  tails <- "g|j|p|q|y"
  last <- 1
  nc <- length(colors)
  if (missing(freq)) {
    if (!require("tm")) 
      stop("freq must either be non-missing, or the tm package must be available")
    if (is.character(words) || is.factor(words)) {
      corpus <- Corpus(VectorSource(words))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, function(x) removeWords(x, 
        stopwords()))
    }
    else corpus <- words
    tdm <- TermDocumentMatrix(corpus)
    freq <- slam::row_sums(tdm)
    words <- names(freq)
  }
  if (ordered.colors) {
    if (length(colors) != 1 && length(colors) != length(words)) {
      stop(paste("Length of colors does not match length of words", 
        "vector"))
    }
  }
  if (min.freq > max(freq)) 
    min.freq <- 0
  overlap <- function(x1, y1, sw1, sh1) {
    #if (!use.r.layout) 
    #  return(.overlap(x1, y1, sw1, sh1, boxes))
    s <- 0
    if (length(boxes) == 0) 
      return(FALSE)
    for (i in c(last, 1:length(boxes))) {
      bnds <- boxes[[i]]
      x2 <- bnds[1]
      y2 <- bnds[2]
      sw2 <- bnds[3]
      sh2 <- bnds[4]
      if (x1 < x2) 
        overlap <- x1 + sw1 > x2 - s
      else overlap <- x2 + sw2 > x1 - s
      if (y1 < y2) 
        overlap <- overlap && (y1 + sh1 > y2 - s)
      else overlap <- overlap && (y2 + sh2 > y1 - s)
      if (overlap) {
        last <<- i
        return(TRUE)
      }
    }
    FALSE
  }
  ord <- rank(-freq, ties.method = "random")
  words <- words[ord <= max.words]
  freq <- freq[ord <= max.words]
  if (ordered.colors) {
    colors <- colors[ord <= max.words]
  }
  if (random.order) 
    ord <- sample.int(length(words))
  else ord <- order(freq, decreasing = TRUE)
  words <- words[ord]
  freq <- freq[ord]
  words <- words[freq >= min.freq]
  freq <- freq[freq >= min.freq]
  if (ordered.colors) {
    colors <- colors[ord][freq >= min.freq]
  }
  thetaStep <- 0.1
  rStep <- 0.05
  plot.new()
  op <- par("mar")
  par(mar = c(0, 0, 0, 0))
  if (fixed.asp) 
    plot.window(c(0, 1), c(0, 1), asp = 1)
  else plot.window(c(0, 1), c(0, 1))
  normedFreq <- freq/max(freq)
  size <- (scale[1] - scale[2]) * normedFreq + scale[2]
  boxes <- list()
  for (i in 1:length(words)) {
    rotWord <- runif(1) < rot.per
    rotWord <- FALSE
    r <- 0
    theta <- runif(1, 0, 2 * pi)
    x1 <- 0.5
    y1 <- 0.5
    wid <- strwidth(words[i], cex = size[i], ...)
    ht <- strheight(words[i], cex = size[i], ...)
    if (grepl(tails, words[i])) 
      ht <- ht + ht * 0.2


    isOverlaped <- TRUE
    print(paste0(i,"/",length(words)," ",words[i]))
    #read jpeg image
    img <- readJPEG(words[i])
   
    while (isOverlaped) {
      
      #manage size and ratio
      img_size <- dim(img)
      img_resize <- 0.2
      wid <- size[i] * img_resize * 0.4
      ht <- size[i] * img_resize * 0.4 * img_size[1]/img_size[2]
      x1 <- x1 - wid/2
      y1 <- y1 - ht/2
      xc <- x1
      yc <- y1
      widc <- wid
      htc <- ht
      if (rotWord) {
        xc <- x1+ (wid/2)-(ht/2)
        yc <- y1+(ht/2)-(wid/2)
        widc <- ht
        htc <- wid
      }
      
      if (!overlap(xc, yc, widc, htc) && xc > 0 && yc > 
        0 && xc + widc < 1 && yc + htc < 1) {
        if (!random.color) {
          if (ordered.colors) {
            cc <- colors[i]
          }
          else {
            cc <- ceiling(nc * normedFreq[i])
            cc <- colors[cc]
          }
        }
        else {
          cc <- colors[sample(1:nc, 1)]
        }
        #text(x1, y1, words[i], cex = size[i], offset = 0, 
        #  srt = rotWord * 90, col = cc, ...)
        #abline(v=0.5,h=0.5)
        
        if(rotWord){
          rasterImage(img, x1+wid/2+ht/2, y1+wid/2, x1 + wid+wid/2+ht/2, y1 + ht+wid/2,angle = rotWord*90)
          boxes[[length(boxes) + 1]] <- c(xc, yc, widc, htc)
        }else{
          rasterImage(img, x1, y1, x1 + wid, y1 + ht,angle = rotWord*90)
          boxes[[length(boxes) + 1]] <- c(x1, y1, wid, ht)
        }
        
        
        isOverlaped <- FALSE
      }
      else {
        
        if (r > sqrt(0.5)) {
          warning(paste(words[i], "could not be fit on page. It will not be plotted."))
          isOverlaped <- FALSE
        }
        theta <- theta + thetaStep
        r <- r + rStep * thetaStep/(2 * pi)
        x1 <- 0.5 + r * cos(theta)
        y1 <- 0.5 + r * sin(theta)
      }
    }
  }
  par(mar = op)
  if(save.png){
    dev.off()
  }
  invisible()
}
