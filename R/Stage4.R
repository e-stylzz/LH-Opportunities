##### File 4: Visuals (Plots & Graphs) #####

# Plotting distribution of prediction score grouped by known outcome. The False line should be focused on the Left, and the 
# True density should be focused on the Right for a good distribution.
ggplot(train, aes(x=LogRePred, color=WonLost, linetype=WonLost)) +
  geom_density()

## This plot is pretty good


#Graph the Decision Tree
fancyRpartPlot(dTree)