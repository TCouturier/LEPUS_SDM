# fonction corrplot
corrplot::corrplot(CorTable, type="upper", order="hclust", tl.col="black", tl.srt=45)

# création masques
maskzone<-rasterize(pnm, zone, background=NA)
alt_PNM <- mask(alt_PNM, maskzone)