### Global variables shared across ui.R and server.R

require(shinyIncubator)
require(rCharts) # @dev

options(RCHART_LIB = c('polycharts', 'morris', "nvd3", "timeline", "dimple"))