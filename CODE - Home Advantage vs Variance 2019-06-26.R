library(ggplot2)

dir_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data/Figures"
file_in = "PLOT DATA - Home Advantage vs Variance 2019-06-26.csv"
pltdf = read.csv(file.path(dir_in, file_in), header=TRUE, stringsAsFactors=FALSE)

expression(atop("Observed Home Field", 
                paste("Advantage [Mg ", "ha"^"-1", "]")))

plt = ggplot(pltdf) +
  xlab(expression(atop("Variation Across Environments",
                  paste("[Mg ", "ha"^"-1", "]")))) +
  ylab(expression(paste("Home Advantage [Mg ", "ha"^"-1", "]"))) +
  geom_smooth(aes(x=sqrt(VARIANCE),
                  y=HOME_ADVANTAGE),
              color='tomato4',
              method='lm',
              weight=0.1,
              linetype="dashed") +
  geom_point(aes(x=sqrt(VARIANCE),
                 y=HOME_ADVANTAGE),
             color='steelblue3',
             size=2,
             alpha=0.5) +
  theme_light()
plt


#### Write to PDF ####
ww = FALSE
# ww = TRUE
if (ww) {
  fout = paste("Fig S2 - Home Advantage vs Variation ",
               Sys.Date(),
               ".pdf",
               sep="")
  fout = file.path(dir_in, fout)
  pdf(file=fout, height=3.25, width=3)#, res=300)
  plt
  dev.off()
}
