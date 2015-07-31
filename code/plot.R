# Plot ROC
plot(perf1,lwd = 2)
plot(perf2, add = TRUE, col="red",lwd = 2)
plot(perf3, add = TRUE, col="blue",lwd = 2)
plot(perf4, add = TRUE, col="green",lwd = 2)
plot(perf5, add = TRUE, col="purple",lwd = 2)
plot(perf6, add = TRUE, col="orange",lwd = 2)
plot(perf7, add = TRUE, col="red3",lwd = 2)
plot(perf8, add = TRUE, col="grey",lwd = 2)
plot(perf9, add = TRUE, col="green3",lwd = 2)
plot(perf10, add = TRUE, col="brown",lwd = 2)
plot(perf11, add = TRUE, col="yellow",lwd = 2)
plot(perf12, add = TRUE, col="blue3",lwd = 2)


legend(.84, .84, 
       legend = c("NB-Txt", "L1LOG-Txt", 
                  "L2LOG-Txt","NB-Soc", "L1LOG-Soc", "L2LOG-Soc","NB-Sent", 
                  "L1LOG-Sent", "L2LOG-Sent","NB-Comb", "L1LOG-Comb", "L2LOG-Comb"),
       lwd = 2, cex=0.7,
       col = c("black","red","blue", "green","purple","orange","red3",
               "grey","green3","brown","yellow","blue3"))
       
