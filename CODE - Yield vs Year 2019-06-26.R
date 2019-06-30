library(ggplot2)

dir_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data/Figures"
file_in = "PLOT DATA - Yield vs Year 2019-06-26.csv"
pltdf = read.csv(file.path(dir_in, file_in), header=TRUE, stringsAsFactors=FALSE)

expression(atop("Observed Home Field", 
                paste("Advantage [Mg ", "ha"^"-1", "]")))

plt = ggplot(pltdf) +
  xlab("Year") +
  ylab(expression(paste("Overall Yield [Mg ", "ha"^"-1", "]"))) +
  # ylim(c(0,16.2)) +
  geom_errorbar(aes(x=YEAR,
                    ymin=YIELD-ERROR,
                    ymax=YIELD+ERROR),
                width=0,
                color="#666666") +
  geom_smooth(aes(x=YEAR,
                  y=YIELD),
              color='tomato4',
              method='lm',
              weight=0.1,
              linetype="dashed") +
  geom_point(aes(x=YEAR,
                 y=YIELD),
             color='steelblue3',
             alpha=0.8,
             size=3) +
  theme_light()
plt


#### Write to PDF ####
ww = FALSE
# ww = TRUE
if (ww) {
  fout = paste("Fig S3 - Yield vs Year ",
               Sys.Date(),
               ".pdf",
               sep="")
  fout = file.path(dir_in, fout)
  pdf(file=fout, height=3, width=3.5)#, res=300)
  plt
  dev.off()
}
