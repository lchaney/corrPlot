THIS IS THE MAIN FUNCTION FOR PLOTTING THE CORRELATION VALUES & ELLIPSE PLOT
# READ THE NOTE BELOW REGARDING ITS FUNCTIONALITY
##########################################
#Edited by Lindsay and Robbie Chaney on 04/22/2013
##########################################
corrPlot <- function( data, varnames=colnames(data), digits=NULL, fontsize=NULL,title=NULL, colors=TRUE, sig=NULL, alpha=0.05 ){

# *Note: This is a secondary function that uses the my.plotcorr() function. 
# This function allows the user to generate a plot in one step, while specifying readable column names. The defaults are indicated below.

# digits specifies how many digits to print. The default is 3. Change by using digits=2 for 2 digits for example.
# fontsize specifies the size of font on the upper triangle of the plot. This is a scale value. 1 is 100% of the normal scale. To increase, do 1.1 for 110%, or 1.5 for 150% larger. To decrease do 0.75 for 75% of normal scale, and so on. 
# First the general correlation matrix needs to be generated
library(Hmisc)

	corr.data <- cor(data, use="complete")
	corr.p <- rcorr(as.matrix(data))$P

# To make the corrPlot() look more readable, we can specify the labels
# Create an object with the specified label names 
# Ex: mylabels <- c( 'Day of first flower', 'Grabbing on', 'Leaf Length') and so on...
# Then call corrPlot() with cnames=mylabels
			colnames(corr.data) = varnames
			rownames(corr.data) = colnames(corr.data)
#Here I am defining the colors to use in the plot.
if(colors != TRUE){
	colors<- c("#252525", "#636363", "#969696", "#CCCCCC", "#F7F7F7",  "white", "#F7F7F7", "#CCCCCC", "#969696", "#636363", "#252525") #Generated using brewer.pal() in the RColorBrewer package. brewer.pal(length(1:5), "Greys")
						}
						else {
							colors <-  c( "#08519C", "#3182BD", "#6BAED6", "#BDD7E7", "#EFF3FF",  "white", "#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26",  "#A50F15")
							}			
# An if-else that deals with user-specified alpha level for bolding significant correlation values 
# This is only useful when sig=1
if(alpha != 0.05){ 
	alpha=alpha}
	else{ alpha=0.05}
# An if-else that deals with the user-specified signficance reporting style
if(!is.null(sig)){
	sig = sig}
	else { sig = 0}
	# sig = 0 is for nothing
	# sig = 1 is for Bold significance values
	# sig = 2 is for STARS 
	# sig = 3 is for STARS AND BOLD

# An if-else that deals with the user-specified font size
if(!is.null(fontsize)){
	fontsize = fontsize}
	else { fontsize =1}

# An if-else that deals with the user-specified digits
if(!is.null(digits)){
	digits=digits}
	else { digits=3}

# An if-else that deals with the user-specified plot title
if(!is.null(title)){
	title =title}
	else { title ='Variable Correlations'}

# plotting the data
# The defaults are set to have the upper panel be correlation values and the lower panel be the colors with the diagonal be blank. These can be changed by chaning the below code.
my.plotcorr(corr.data, diag='none', col=colors[5*corr.data + 6], upper.panel="number", main= title, digits=digits, cex = fontsize * par("cex"), significance=sig, alpha=alpha, corr.p=corr.p )	
	}
my.plotcorr <- function (corr, outline = FALSE, col = "grey", upper.panel = c("ellipse", "number", "none"), lower.panel = c("ellipse", "number", "none"), diag = c("none", "ellipse", "number"), digits = 1 # 2 Changed 
, bty = "n", axes = FALSE, xlab = "", ylab = "", asp = 1, cex.lab = par("cex.lab"), cex = fontsize * par("cex"), mar = 0.1 + c(2, 2, 4, 2), significance=sig, alpha=alpha,corr.p=corr.p,...)
{
# this is a modified version of the plotcorr function from the ellipse package
# this prints numbers and ellipses on the same plot but upper.panel and lower.panel changes what is displayed
# diag now specifies what to put in the diagonal (numbers, ellipses, nothing)
# digits specifies the number of digits after the . to round to
# unlike the original, this function will always print x_i by x_i correlation rather than being able to drop it
# modified by Esteban Buz

  if (!require('ellipse', quietly = TRUE, character = TRUE)) {
    stop("Need the ellipse library")
  }
  savepar <- par(pty = "s", mar = mar)
  on.exit(par(savepar))
  if (is.null(corr))
    return(invisible())
  if ((!is.matrix(corr)) || (round(min(corr, na.rm = TRUE), 6) < -1) || (round(max(corr, na.rm = TRUE), 6) > 1))
    stop("Need a correlation matrix")
  plot.new()
  par(new = TRUE)
  rowdim <- dim(corr)[1]
  coldim <- dim(corr)[2]
  rowlabs <- dimnames(corr)[[1]]
  collabs <- dimnames(corr)[[2]]
  if (is.null(rowlabs))
    rowlabs <- 1:rowdim
  if (is.null(collabs))
    collabs <- 1:coldim
  rowlabs <- as.character(rowlabs)
  collabs <- as.character(collabs)
  col <- rep(col, length = length(corr))
  dim(col) <- dim(corr)
  upper.panel <- match.arg(upper.panel)
  lower.panel <- match.arg(lower.panel)
  diag <- match.arg(diag)
  cols <- 1:coldim
  rows <- 1:rowdim
  maxdim <- max(length(rows), length(cols))
  plt <- par("plt")
  xlabwidth <- max(strwidth(rowlabs[rows], units = "figure", cex = cex.lab))/(plt[2] - plt[1])
  xlabwidth <- xlabwidth * maxdim/(1 - xlabwidth)
  ylabwidth <- max(strwidth(collabs[cols], units = "figure", cex = cex.lab))/(plt[4] - plt[3])
  ylabwidth <- ylabwidth * maxdim/(1 - ylabwidth)
  plot(c(-xlabwidth - 0.5, maxdim + 0.5), c(0.5, maxdim + 1 + ylabwidth), type = "n", bty = bty, axes = axes, xlab = "", ylab = "", asp = asp, cex.lab = cex.lab, ...)
  text(rep(0, length(rows)), length(rows):1, labels = rowlabs[rows], adj = 1, cex = cex.lab)
  text(cols, rep(length(rows) + 1, length(cols)), labels = collabs[cols], srt = 90, adj = 0, cex = cex.lab)
  mtext(xlab, 1, 0)
  mtext(ylab, 2, 0)
  mat <- diag(c(1, 1))
  plotcorrInternal <- function() {
    if (i == j){ #diag behavior --------- DIAGONAL 
      if (diag == 'none'){
        return()
      } else if (diag == 'number'){
        text(j + 0.3, length(rows) + 1 - i, sprintf("%.3f", round(corr[i, j], digits=digits)), adj = 1, cex = cex)
      } else if (diag == 'ellipse') {
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      }
    } else if (i >= j){ #lower half of plot -------- LOWER TRIANGLE 
      if (lower.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      } else if (lower.panel == 'number') { #check if ellipses should go here
        text(j + 0.3, length(rows) + 1 - i, sprintf("%.3f", round(corr[i, j], digits=digits)), adj = 1, cex = cex)
      } else {
        return()
      }
    } else { #upper half of plot THIS SECTION IS FOR THE UPPER TRIANGLE OF THE PLOT -------- UPPER TRIANGLE
      if (upper.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      } else if (upper.panel == 'number') { #check if ellipses should go here
      	digs <- paste("%.",digits,"f",sep="" )      	      	
    # ADDING OPTION FOR STARS OR BOLDING OF TEXT  
	if(significance==0){
		text(j + 0.3, length(rows) + 1 - i, sprintf(digs, round(corr[i, j], digits=digits)), adj = 1, cex = cex)
	}	
	else if(significance==1){
			# ADDING SIGNIFICANCE TO TEXT OUTPUT IN UPPER TRIANGLE
      		if (corr.p[i,j] <= alpha){
		text(j + 0.3, length(rows) + 1 - i, sprintf(digs, round(corr[i, j], digits=digits)), adj = 1, cex = cex, font=2)
      		}
      		else {
      	text(j + 0.3, length(rows) + 1 - i, sprintf(digs, round(corr[i, j], digits=digits)), adj = 1, cex = cex)
      			}
      			}
	else if (significance==2) { 
			if(corr.p[i,j] <= 0.001){
					text(j + 0.3, length(rows) + 1 - i, paste(sprintf(digs, round(corr[i, j], digits=digits)),"***",sep=""), adj = 1, cex = cex)}
					else if( corr.p[i,j] <= 0.01){
						text(j + 0.3, length(rows) + 1 - i, paste(sprintf(digs, round(corr[i, j], digits=digits)),"**",sep=""), adj = 1, cex = cex)}
					else if( corr.p[i,j] <= 0.05){
						text(j + 0.3, length(rows) + 1 - i, paste(sprintf(digs, round(corr[i, j], digits=digits)),"*",sep=""), adj = 1, cex = cex)}
					else if( corr.p[i,j] <= 0.10){
						text(j + 0.3, length(rows) + 1 - i, paste(sprintf(digs, round(corr[i, j], digits=digits)),"^",sep=""), adj = 1, cex = cex)}	
			else {
      			text(j + 0.3, length(rows) + 1 - i, sprintf(digs, round(corr[i, j], digits=digits)), adj = 1, cex = cex)
      			}
								}
		else if (significance==3) { 
			if(corr.p[i,j] <= 0.001){
					text(j + 0.3, length(rows) + 1 - i, paste(sprintf(digs, round(corr[i, j], digits=digits)),"***",sep=""), adj = 1, cex = cex, font=2)}
					else if( corr.p[i,j] <= 0.01){
						text(j + 0.3, length(rows) + 1 - i, paste(sprintf(digs, round(corr[i, j], digits=digits)),"**",sep=""), adj = 1, cex = cex, font=2)}
					else if( corr.p[i,j] <= 0.05){
						text(j + 0.3, length(rows) + 1 - i, paste(sprintf(digs, round(corr[i, j], digits=digits)),"*",sep=""), adj = 1, cex = cex, font=2)}
					else if( corr.p[i,j] <= 0.10){
						text(j + 0.3, length(rows) + 1 - i, paste(sprintf(digs, round(corr[i, j], digits=digits)),"^",sep=""), adj = 1, cex = cex, font=2)}	
			else {
      			text(j + 0.3, length(rows) + 1 - i, sprintf(digs, round(corr[i, j], digits=digits)), adj = 1, cex = cex)
      			}
								}						
      }
       else {
        return()
      }
    }
  }
  for (i in 1:dim(corr)[1]) {
    for (j in 1:dim(corr)[2]) {
      plotcorrInternal()
    }
  }
  invisible()
}