library(gitBasedProjects)
sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/')
graphics.off()

paramFile = 'outputs/params'
datFile   = 'data/frpVariables'

mnth = 8

month_names = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

logistic <- function(x, x0, k) 
	1/(1+exp(-k * (x - x0)))
	
openParams <- function(mnth) {
	paramFile_mn = paste(paramFile, mnth-1, '.csv', sep = '')
	params = read.csv(paramFile_mn, stringsAsFactors = FALSE)
	nit =  nrow(params)/2
	start = ((nit/2)+1)
	index = c(start:nit, (nit + start):(2* nit))
	params = params[index,]
	return(params)
}


addHistAxis <- function(x, breaks = seq(0, 1, 0.01), switch = FALSE) {
	mar = par("mar")
	
	yhist = hist(x, breaks = breaks, plot = FALSE)
	y = yhist$density
	x = yhist$mids
	ylim = c( max(y), 0)
	xlim = NULL
	xp = c(x, rev(x))
	yp = c(y, rep(0, length(y)))
	if (switch) {
		xi = y; y = x; x = xi
		xlim = ylim; ylim = NULL
		xpi = yp; yp = xp; xp = xpi
		par(mar = c(mar[1], 0, mar[3], 0))
	} else par(mar = c(0, mar[2], 0, mar[4]))

	plot(x, y, type = 'l', xlim = xlim, ylim = ylim, axes = FALSE, xlab = '', ylab = '')
	polygon(xp, yp, col = 'black')
	par(mar = mar)
}

lmat = rbind(c(3, 1),
			 c(NaN, 2))
lmat = cbind(lmat, lmat + 3, lmat + 6, lmat + 9)
lmat = rbind(lmat, lmat + 12, lmat + 24)
#lmat = rbind(lmat, lmat + 12, 0, lmat + 24, lmat + 36)
lmat[is.na(lmat)] = 0

widths = rep(c(0.2, 1.0), 4)
heights = rep(c(1.0, 0.2), 3)

png('figs/hypoth_test.png', height = 16, width = 20, unit = 'in', res = 300)
layout(lmat, widths = widths, heights = heights)


plotMnth <- function(mnth) {
	params = openParams(mnth)

	##wood tree cover = 100
	wood_100   = logistic( 100, params[,    'wood_0'], -params[,    'wood_k'])
	moisture_0 = logistic(  0, params[,'moisture_0'], params[,'moisture_k'])
	
	lims = range(c(wood_100, moisture_0))
	lims = c(1, 0)	
	plot(wood_100, moisture_0, xlim = lims, ylim = lims, pch = 19, cex =0.2)

	pcSide <- function(test, x, y) {
		pc = round(sum(test) /length(wood_100) * 100, 2)
		pc = paste(pc, '%', sep = '')	
		text(pc, x = x, y = y)
	}
	pcSide(wood_100 > moisture_0, x = 0.9, y = -0.02)
	pcSide(wood_100 < moisture_0, x = 0.1, y =1.02)
	

	
	lines(c(0, 1), c(0, 1), col = 'red')
	mtext(month_names[mnth])

	addHistAxis(wood_100)
	addHistAxis(moisture_0, switch = TRUE)
}
mapply(plotMnth, 1:12)
dev.off()
