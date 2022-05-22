# Shiny_App_coursework

This repo is what it's name suggests: A place to keep my shiny app coursework


App development final project status

First version of app is not functional: cannot plot ordination, cannot verify that ordination function is running properly

Current biggest issue: Ordi_app functions the_ord() and measure_distance() and plot_df() work together to plot the ordination outside of the app but not inside of it in the reactive context. 

Stack Trace:


Warning: Error in $: $ operator is invalid for atomic vectors
  - 142: as.matrix
  - 141: ape::pcoa
  - 140: ordinate [C:/Users/enebe/OneDrive/Documents/Shiny_App_coursework/Ordi_App.R#77]
  - 139: <reactive:the_ord> [C:/Users/enebe/OneDrive/Documents/Shiny_App_coursework/Ordi_App.R#151]
  - 123: the_ord
  - 121: <reactive:plot_df> [C:/Users/enebe/OneDrive/Documents/Shiny_App_coursework/Ordi_App.R#165]
  - 105: plot_df
  - 104: exprFunc
  - 103: widgetFunc
  - 102: ::


