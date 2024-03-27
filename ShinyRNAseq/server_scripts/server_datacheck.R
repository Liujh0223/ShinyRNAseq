datacheck_process <- reactiveValues()
datacheck_process$value <- "start"

# render UI
observeEvent(datacheck_process$value, {
  if (datacheck_process$value == "start") {
    output$ui_datacheck <- renderUI({
      fluidRow(style = "height:700px;",
        br(),br(),br(),br(),br(),
        column(width = 12, style = "left:00px;",
               fluidRow(
                 column(width = 7, align="left", offset = 5,
                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step1: Upload you Matrix</div>")),
                column(width = 1, align = "right", offset = 4,
                    shinyWidgets::dropdownButton(
                      tags$h4(p(icon("check",lib = "font-awesome"),em("上传基因表达矩阵数据（值必须为整数的reads值），文件格式仅支持 .csv .xlsx"),style="color:#337ab7;text-align:center;font-size:15px;")),
                      column(width = 12, align = "center", downloadButton("download_deg_matrix_template2", label = "Template matrix", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                      circle = TRUE, status = "default",
                      icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                    )
                  ),
                 column(width = 7, align="left", offset = 0,
                        fileInput(inputId = "datacheck_upload_matrix", label = NULL, accept = ".csv", width = "340px"))
               ),# close fluidRow
               br(),
               fluidRow(
                 column(width = 7, align="left", offset = 5,
                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step2: Download you Group info</div>")),
                column(width = 1, align = "right", offset = 4,
                    shinyWidgets::dropdownButton(
                      tags$h4(p(icon("check",lib = "font-awesome"),em("下载您的分组信息表，并且修改group列（sample列不需要修改），修改后于Step3上传。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                      circle = TRUE, status = "default",
                      icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                    )
                  ),
                 column(width = 7, align="left", offset = 0,
                        downloadButton(outputId = "datacheck_download_groupinfo", label = "Download", icon = icon("download"), style = "width: 340px;margin-bottom: -10px;"))
               ),# close fluidRow
               br(),br(),br(),
               fluidRow(
                column(width = 7, align="left", offset = 5,
                        HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step3: Upload you Group info</div>")),
                column(width = 1, align = "right", offset = 4,
                    shinyWidgets::dropdownButton(
                      tags$h4(p(icon("check",lib = "font-awesome"),em("上传您的样品分组文件（下载step2文件，并修改group列，相同组别的样品设置同一组名）。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                      column(width = 12, align = "center", downloadButton("download_deg_group_template2", label = "Template groupinfo", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                      circle = TRUE, status = "default",
                      icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                    )
                  ),
                 column(width = 7, align="left", offset = 0,
                        fileInput(inputId = "datacheck_upload_groupinfo", label = NULL, accept = ".csv", width = "340px"))
               ),# close fluidRow
               br(),br(),br(),
               fluidRow(
                 column(width = 2, align="center", offset = 4,
                        actionButton(inputId = "reset_datacheck", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
                 column(width = 3, align="center", offset = 0,
                        actionButton(inputId = "enter_datacheck", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
               )# close fluidRow
        )#, #close cloumn
        #column(width = 6, slickROutput("slickr"), style = "right:200px;"
        #) #close cloumn
      )# close fluidRow
    })
  } else if (datacheck_process$value == "processing") {
    output$ui_datacheck <- renderUI({
      fluidRow(
        br(),br(),
        tags$head(tags$style(HTML('.form-control {height: 37.5px;border: 1px solid #000;}'))),
        tags$head(tags$style(HTML('.selectize-input {border-color: black;line-height:23px; font-size:14px;height:38px}'))),
        HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
        column(width = 12, align = "left", style = "width:80%;margin-left:10%;",
               column(width = 3, align = "left", h3("Correlation heatmap")),
               column(width = 1, align = "left", offset = 0, style = "bottom: -15px;left: -140px;",
                  shinyWidgets::dropdownButton(
                    tags$h4(p(icon("check",lib = "font-awesome"),em("Genes of variance top 500: 在所有样品中，方差最大的前500个基因的表达热图。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                    tags$h4(p(icon("check",lib = "font-awesome"),em("Correlation heatmap of each sample: 样品和样品间的相关性，相关性系数越大则越相关，最大为1。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                    circle = TRUE, status = "default",
                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T)
                 ),
               HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),
               column(width = 4, align = "left", h6("Genes of variance top 500"), style = "left:60px;"),
               column(width = 5, align = "left", h6("Correlation heatmap of each sample"), style = "left:120px;"),
               column(width = 5, align = "center", plotOutput(outputId = "plottop500", height = "470px", width = "500px")),
               column(width = 5, align = "left", plotOutput(outputId = "corheatmap", width = "550px", height = "550px"), style = "top:-40px;left:-30px;z-index: -1;"),# corheatmap
               column(width = 2, align = "left", style = "left:-30px;margin-top:-10px;",
                      column(width = 12, HTML("<p style = 'font-size:15px;font-weight: 600;margin-bottom: 5px;'>Color of Variance top 500</p>")),
                      column(width = 12, colourpicker::colourInput("color_up_top500", label = NULL, "#ff0000"),style = "margin-bottom:-13px;"),
                      column(width = 12, colourpicker::colourInput("color_mid_top500", label = NULL, "#ffffff"),style = "margin-bottom:-13px;"),
                      column(width = 12, colourpicker::colourInput("color_down_top500", label = NULL, "#0000ff")),
                      column(width = 12, HTML("<p style = 'font-size:15px;font-weight: 600;margin-bottom: 5px;'>Color of Cor heatmap</p>")),
                      column(width = 12, colourpicker::colourInput("color_up_corheatmap", label = NULL, "#DD3E2C"),style = "margin-bottom:-13px;"),
                      column(width = 12, colourpicker::colourInput("color_mid_corheatmap", label = NULL, "#FEFEC5"),style = "margin-bottom:-13px;"),
                      column(width = 12, colourpicker::colourInput("color_down_corheatmap", label = NULL, "#4878B6")),
                      column(width = 5, numericInput("corheatmapwidth", label = "Width", value = 8, width = "90%")),
                      column(width = 5, numericInput("corheatmapheight", label = "Height", value = 8, width = "90%"), style = "margin-left:-35px"),
                      column(width = 6, selectInput(inputId = "corheatmapformat", label = "Format", choices = c("PNG" = "png", "PDF" = "pdf"),width = "80%"),style = "position: absolute;top: 315px;right: -3px;"),
                      column(width = 12, align = "center", downloadButton("downloadtoop500", label = "Download Variance top 500",style = "width: 188px;border-color: #ccc;font-size:10px;text-align: center;")),
                      column(width = 12, align = "center", downloadButton("downloadcorheatmap", label = "Download Cor heatmap",style = "width: 188px;border-color: #ccc;font-size:10px;text-align: center;margin-top: 10px;")))
          ),
        HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
        column(width = 12, align = "left", style = "width:80%;margin-left:10%;margin-bottom: 70px;",
               column(width = 4, align = "left", h3("Principal component analysis")),
               column(width = 1, align = "left", offset = 0, style = "bottom: -15px;left: -170px;",
                  shinyWidgets::dropdownButton(
                    tags$h4(p(icon("check",lib = "font-awesome"),em("主成分分析（PCA）是一种常用的数据降维和特征提取方法，用于将高维数据集转化为低维空间，PCA的目标是找到原始数据中最重要的方向（主成分）。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                    tags$h4(p(icon("check",lib = "font-awesome"),em("建议两个主成分累计方差百分比在70%以上，这个范围可以在降低维度的同时保留相当多的原始信息。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                    circle = TRUE, status = "default",
                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T)
                 ),
               HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'), br(),br(),
               column(width = 5, column(width = 12, div(tableOutput(outputId = "pcaproportiontable"), style ="font-size:70%")), column(width = 12, plotOutput(outputId = "screeplot", height = "350px",width = "500px"))),
               column(width = 5, align = "left", plotOutput(outputId = "pcaplot", width = "100%", height = "475px"), style = "left:-70px;"),# corheatmap
               column(width = 2, align = "left", style = "left:-30px;margin-top:-10px;",
                      column(width = 5, numericInput("pcapx", label = "Axis 1", value = 1, min = "1", max = "5", width = "90%")),
                      column(width = 5, numericInput("pcapy", label = "Axis 2", value = 2, min = "1", max = "5", width = "90%"), style = "margin-left:-34px"),
                      column(width = 5, numericInput("pcapaxis", label = "Zoom", value = 0,min = "0", width = "100%"),style = "position: absolute;right: 16px;top: 0px;"),
                      column(width = 6, checkboxInput("pcaellipse", label = "Ellipse", value = FALSE)),
                      column(width = 6, checkboxInput("pcaellipselable", label = "Tags", value = FALSE)),
                      column(width = 6, checkboxInput("pcasamplename", label = "Sample", value = TRUE)),
                      column(width = 6, checkboxInput("pcagrid", label = "Grid", value = TRUE)),
                      column(width = 6, checkboxInput("pcaguidelines", label = "Guidelines", value = TRUE), style = "margin-bottom:140px"),
                      column(width = 6, checkboxInput("pcaborders", label = "Borders", value = TRUE), style = "margin-bottom:140px"),
                      column(width = 5, numericInput("pcawidth", label = "Width", value = 8, width = "90%")),
                      column(width = 5, numericInput("pcaheight", label = "Height", value = 6, width = "90%"), style = "margin-left:-34px"),
                      column(width = 5, selectInput(inputId = "pcaformat", label = "Format", choices = c("PNG" = "png", "PDF" = "pdf"),width = "100%"),style = "position: absolute;top: 352px;right: 16px;"),
                      column(width = 12, align = "center", downloadButton("downloadpca", label = "Download PCA plot",style = "width: 188px;border-color: #ccc;font-size:10px;text-align: center;")))
                      
          ),
        br(),
        HTML('<hr style="width: 90%;border-top: 2px solid #000;" />'),
        column(width = 12, align = "center",
          actionButton(inputId = "reste_datacheck_2", label = "BACK", icon = icon("angle-left"),
                       style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;margin-bottom: 10px")
        )#close cloumn
      )# close fluidRow
    })# close output$ui_datacheck
  }
})

output$download_deg_matrix_template2 <- downloadHandler(#下载deg matrix 模板
    filename = function() {"gene_count_matrix_template.csv"},
    content = function(file) {
      data <- read.csv("template/gene_count_matrix_template.csv")
      write.csv(data, file, row.names = F, fileEncoding="GBK", quote = FALSE)
    }
)
output$download_deg_group_template2 <- downloadHandler(#下载deg matrix 模板
    filename = function() {"groupinfo_template.csv"},
    content = function(file) {
      data <- read.csv("template/groupinfo_template.csv")
      colnames(data) <- c("sample", "group")
      write.csv(data, file, row.names = F, fileEncoding="GBK", quote = FALSE)
    }
)

# slickR
# output$slickr <- renderSlickR({
#   imgs <- list.files("data/data_check/", pattern=".png", full.names = TRUE)
#   slick <- slickR(imgs, height = "500px")
#   slick + settings(autoplay = TRUE, autoplaySpeed = 1500, dots = TRUE, fade = FALSE)
# })

# ENTER
observeEvent(input$enter_datacheck,{
  if (! is.null(countdata())) {
    datacheck_process$value <- "processing"
  } else {
     shinyjs::runjs('alert("ERROR: Please upload matrix and group info.")')
     datacheck_process$value <- "start"
  }
  
})

# RESET
observeEvent(input$reset_datacheck,{
  shinyjs::reset("datacheck_upload_matrix")
  shinyjs::reset("datacheck_upload_groupinfo")
})
observeEvent(input$reste_datacheck_2,{
  datacheck_process$value <- "start"
})

# read matrix
countdata <- reactive({#读取count矩阵，返回datafarme，header=T， rowname=F
  countdata <- input$datacheck_upload_matrix
  if (is.null(countdata)) {
    return(NULL)
  }else {
    shinyjs::reset("datacheck_upload_groupinfo")
    fileformat <- strsplit(countdata$datapath, "\\.")[[1]][-1]
    if (fileformat == "csv") {
      countdata <- read.csv(countdata$datapath, header = TRUE)
    }else if (fileformat == "xlsx") {
        countdata <- openxlsx::read.xlsx(countdata$datapath, colNames = TRUE, rowNames = F)
    }else {
          shinyjs::reset("datacheck_upload_matrix")
          shinyjs::reset("datacheck_upload_groupinfo")
          shinyjs::runjs('alert("ERROR: Format of matrix file must be <.csv> or <.xlsx>.")')
          return(NULL)
    }
  }
  data <- unlist(matrix(countdata[, -1]))
  if (typeof(data) == "character") {
        shinyjs::reset("datacheck_upload_matrix")
        shinyjs::reset("datacheck_upload_groupinfo")
        shinyjs::runjs('alert("ERROR: The matrix must be numeric.")')
        return(NULL)
  }
  # if (all(data == floor(data)) == FALSE) {#判断整数
  #       sendSweetAlert(
  #       session = session,
  #       title = "ERROR",
  #       text = "基因Count值必须为整数",
  #       type = "error")
  #       shinyjs::reset("uploadcountdata")
  #       shinyjs::reset("uploadgroupinfo")
  #       return(NULL)
  # }
  countdata
})

# CPM
cpm <- reactive({#将conutdata标准化为CPM，返回datafarme， header=T， rowname=1
  countdata <- countdata()
  if (is.null(countdata)) {
    return(NULL)
  }else {
    rownames(countdata) <- countdata[, 1]
    countdata <- countdata[, -1]
    cpm <- t(t(countdata) / colSums(countdata) * 1000000)
    cpm <- data.frame(cpm)
  }
  cpm
})

# read group_info
groupinfotemplate <- reactive({#deggroupinfo模板(生物学重复分组), 返回datafarme header=T，rowname=1
  if (is.null(countdata())) {
    return(NULL)
  }else {
      readscountdata <- countdata()
      readscountdata <- readscountdata[, -1]
      group <- colnames(readscountdata)
      group <- data.frame(group)
      rownames(group) <- group$group
      group
  }
})

# download group_info
output$datacheck_download_groupinfo <- downloadHandler(#下载groupinfo模板
  filename = function() {"groupinfo.csv"},
  content = function(file) {
    if(is.null(groupinfotemplate())) {return(NULL)}else {
        write.csv(groupinfotemplate(), file, row.names = TRUE, fileEncoding="GBK", quote = FALSE)
    }
  }
)

# completion group info
groupinfo <- reactive({#pcadeggroupinfo,返回datafarme,headerT, rownames=F
  groupinfo <- input$datacheck_upload_groupinfo
  countdata <- countdata()

  if (is.null(groupinfo)) {
    return(NULL)
  } else {
      if (is.null(countdata)) {
        shinyjs::runjs('alert("ERROR: Please upload matrix before upload group info.")')
        shinyjs::reset("datacheck_upload_groupinfo")
        return(NULL)
      }
  }

  fileformat <- strsplit(groupinfo$datapath, "\\.")[[1]][-1]
  if (fileformat == "csv") {
    groupinfo <- read.csv(groupinfo$datapath, header = TRUE)
  }else if (fileformat == "xlsx") {
    groupinfo <- openxlsx::read.xlsx(groupinfo$datapath, colNames = TRUE, rowNames = F)
  }else {
        shinyjs::runjs('alert("ERROR: Format of group info file must be <.csv> or <.xlsx>.")')
        shinyjs::reset("datacheck_upload_groupinfo")
        return(NULL)
  }
  allsample <- groupinfo[, 1]
  cpmsamplename <- colnames(countdata())
  for (i in allsample) {
    if (i %in% cpmsamplename == FALSE) {
      shinyjs::runjs('alert("ERROR: The sample name in group info does not correspond to the sample name in matrix.")')
      shinyjs::reset("datacheck_upload_groupinfo")
      return(NULL)
    }
  }
  colnames(groupinfo) <- c("sample", "group")
  groupinfo
})

# corheatmap
output$corheatmap <- renderPlot(return(plotcorheat()))

#PCA
output$screeplot <- renderPlot(screeplot())
output$pcaproportiontable <- renderTable(rownames = TRUE, pcaproportiontable())
output$pcaplot <- renderPlot(pcaplot())

# plottop500
output$plottop500 <- renderPlot({plottop500()})