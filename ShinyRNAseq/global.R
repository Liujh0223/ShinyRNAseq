##################################################################
#
# Global script 
# Author: Jianhong Liu
# Date: 2023.1.26
#
##################################################################

# contains:-

# 1. required packages
# 2. required funtions
# 3. source ui/server scripts
# 4. running shiny app

##################################################################

# 1. required packages -------------------------------------------
library(shiny)
library(shinyjs)
# library(slickR)(弃用)
library(ggplot2)
library(dplyr)
library(bsplus)
library(shinycssloaders)

library(shinythemes)
library(shinyWidgets)

# openxlsx, reshape2, aplot, colourpicker, stringr, R.utils, DT, edgeR, DESeq2, ggrepel, factoextra, ggforce, clusterProfiler, data.table, statmod, VennDiagram, shinydashboard, Mfuzz, ComplexHeatmap, WGCNA, gplots,zip

# 2. required funtions
source('funtions/funtion_tab_voronoys.R') #tab_voronoys
source('funtions/funtion_upload_file.R') #tab_voronoys

# 3. source ui/server scripts ------------------------------------
source('ui.R')
options(shiny.maxRequestSize = 20 * 1024^2)#文件上传最大为15M
source('server.R')

# 4.running shiny app --------------------------------------------
shinyApp(ui = ui, server = server)