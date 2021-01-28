###---------------------------------------------------------------###
###   Building a Platform for Data-Driven Pandemic Prediction:    ###
###   From Data Modelling to Visualization - the CovidLP Project  ###
###                                                               ###
###   Chapter 14: Building an interactive app withShiny           ###
###   Sections 14.6                                               ###
###   Subsection 14.6.1 plotlybasics                              ###
###                                                               ###
###   Author: the CovidLP Team                                    ###
###---------------------------------------------------------------###

## Chapter 14 - Section 14.6.1
## Code to create 'plotly' graphs for example


# load packages
require(plotly)
require(PandemicLP)
require(RColorBrewer)
require(rsvg)


# load data
Argentina = load_covid("Argentina", last_date = "2020/12/15")


# Listing 14.13:
# initialising the plot
plt = Argentina$data %>%
  plot_ly()


# Listing 14.14:
# add trace
plt = plt %>%
  add_trace(x = Argentina$data$date, y = Argentina$data$new_cases,
            type = 'scatter', mode='lines+markers', name="new daily cases",
            marker = list(color="black"),
            line = list(color="grey"))

# show plot - Figure 14.13
plt


# Listing 14.15:
# add title, axes labels and legend
plt = plt %>%
  layout(title=list(text="Argentina"),
         xaxis=list(title="Date"), yaxis=list(title="number of cases"),
         legend=list(x=0.1,y=0.9, bgcolor=gray(0.9)), showlegend=T
         )

# show plot - Figure 14.14
plt


