# from https://plotly.com/r/multiple-axes/
library(plotly)

fig <- plot_ly()
# Add traces
fig <- fig %>% add_trace(x = ~1:3, y = ~10*(4:6), name = "yaxis data", mode = "lines+markers", type = "scatter")

ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "<b>secondary</b> yaxis title")

fig <- fig %>% add_trace(x = ~2:4, y = ~4:6, name = "yaxis 2 data", yaxis = "y2", mode = "lines+markers", type = "scatter")

# Set figure title, x and y-axes titles
fig <- fig %>% layout(
  title = "Double Y Axis Example", yaxis2 = ay,
  xaxis = list(title="xaxis title "),
  yaxis = list(title="<b>primary</b> yaxis title")
)%>%
  layout(plot_bgcolor='#e5ecf6',
          xaxis = list(
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff')
          )

fig
