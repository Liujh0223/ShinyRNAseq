timeseriesanalysis_process <- reactiveValues()
timeseriesanalysis_process$value <- "start"
library(Mfuzz)

# render UI
observeEvent(timeseriesanalysis_process$value, {
    if (timeseriesanalysis_process$value == "start") {
        output$ui_timeseriesanalysis <- renderUI({
            fluidRow(style = ";",
                    br(), br(), br(), br(), br(), br(), br(),
                    column(width = 6, align = "center", style = "left: 450px;margin-top: -70px;",
                        fluidRow(
                            column(width = 8, align="left", offset = 4,
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step1: Upload you Matrix</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("上传基因表达矩阵数据，文件格式仅支持 .csv .xlsx"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    column(width = 12, align = "center", downloadButton("download_deg_matrix_template3", label = "Template matrix", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 8, align="left", offset = 0,
                                fileInput(inputId = "tsa_upload_matrix", label = NULL, accept = ".csv", width = "340px"))
                        ),# close fluidRow
                        br(),
                        fluidRow(
                            column(width = 8, align="left", offset = 4, style = "margin-top: -35px;",
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step2: Download you Group info</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("下载您的分组信息表，并且修改group列（sample列不需要修改），修改后于Step3上传。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 8, align="left", offset = 0,
                                downloadButton(outputId = "tsa_download_groupinfo", label = "Download", icon = icon("download"), style = "width: 340px;margin-bottom: -10px;"))
                        ),# close fluidRow
                        br(),
                        fluidRow(
                            column(width = 8, align="left", offset = 4,
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step3: Upload you Group info</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("上传您的样品分组文件（下载step2文件，并修改group列，相同组别的样品设置同一组名）。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    column(width = 12, align = "center", downloadButton("download_deg_group_template3", label = "Template groupinfo", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 8, align="left", offset = 0,
                                fileInput(inputId = "tsa_upload_groupinfo", label = NULL, accept = ".csv", width = "340px"))
                        ),# close fluidRow
                        br(),
                        fluidRow(
                            column(width = 8, align="left", offset = 4, style = "margin-top: -35px;",
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step4: Homogenization or not</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("是否需要均一化，本方法默认使用均一化方式为CPM。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("计算公式：CPM=比对到 gene A 的 reads 数 / 比对到所有 gene 的总reads 数 * 1000000"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("如果您的基因表达矩阵是reads值矩阵则需要均一化，如果已经是均一化处理过后的则不需要，如TPM,FPKM,CPM,RPKM等。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 8, align="left", offset = 0, 
                                    shinyWidgets::radioGroupButtons(inputId = "tsa_cpm", label = NULL, choices = c("YES", "NO"), individual = TRUE, width = "100%",
                                    checkIcon = list(yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"), no = tags$i(class = "fa fa-square-o",style = "color: steelblue"))))
                        ),# close fluidRow
                        br()
                    ), #close cloumn
                    br(),  br(), br(),
                    column(width = 4, offset = 4,
                            column(width = 6, align="left", offset = 0, style="margin-bottom: 80px;margin-top: 80px;",
                                    actionButton(inputId = "reset_tsa", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
                            column(width = 6, align="right", offset = 0, style="margin-bottom: 80px;margin-top: 80px;",
                                    actionButton(inputId = "enter_tsa", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
                    )
            )# close fluidRow
        })
    } else if (timeseriesanalysis_process$value == "process") {
       output$ui_timeseriesanalysis <- renderUI({
            fluidRow(
                br(),
                column(width = 12, 
                    column(width = 3, align = "left", h2("Time series analysis")),
                    column(width = 2, align = "left", offset = 0, style = "bottom: -20px;left: -160px;",
                    shinyWidgets::dropdownButton(
                        tags$h4(p(icon("check",lib = "font-awesome"),em("时间序列分析基于R包：Mfuzz。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                        tags$h4(p(icon("check",lib = "font-awesome"),em("用户上传的基因表达矩阵中将会对每个生物学重复取均值合并，以进行后续分析"),style="color:#337ab7;text-align:center;font-size:15px;")),

                        circle = TRUE, status = "default",
                        icon = icon("question"), width = "300px", size = "sm", right = F, up = T)
                    )
                ),
                HTML('<hr style="width: 100%;border-top: 2px solid #000;" />'),
                br(),
                mainPanel(width = 9, style = "border-right: 1px solid #ddd; margin-bottom: 40px;", 
                    column(width = 12, align = "center", htmlOutput(outputId = "tsa_base_info", style = "font-size:15px; margin-bottom: 30px;margin-top: -25px;")),
                    HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                    column(width = 12, 
                        column(width = 2, align = "left", style="background-color: #dddddd6b;height: 50px;width: 200px;font-weight: 900;line-height: 2.5em;font-size: 20px;", HTML("After filtration")),
                        column(width = 2, downloadButton(outputId = "download_tsa_after_filter", label = "Download", style = "height:30px;line-height: 0em;"), style="background-color: #dddddd6b;height: 50px;line-height: 3.5em;width: 150px"),
                        column(width = 12, align = "center", DT::dataTableOutput(outputId = "tsa_test2"), style="background-color: #dddddd6b;margin-bottom: 20px;"), 
                    ),
                    br(),
                    HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                    column(width = 12,
                        column(width = 12, align = "center", imageOutput("tsa_plot") %>% withSpinner()),
                        column(width = 12, align = "center", div(tableOutput(outputId = "tsa_mufzz_stat"), style ="width:100%;"))
                    ),
                    br(),
                    HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                    column(width = 12,
                        column(width = 2, align = "left", style="background-color: #dddddd6b;height: 50px;width: 200px;font-weight: 900;line-height: 2.5em;font-size: 20px;", HTML("Result")),
                        column(width = 2, downloadButton(outputId = "download_tsa_mfuzz_result", label = "Download", style = "height:30px;line-height: 0em;"), style="background-color: #dddddd6b;height: 50px;line-height: 3.5em;width: 150px"),
                        column(width = 12, align = "center", DT::dataTableOutput(outputId = "tsa_test1"), style="background-color: #dddddd6b;margin-bottom: 20px;")
                    ),
                    
                ),
                sidebarPanel(width = 3,
                        br(),br(),br(),
                        column(width = 12, align = "left", h4("Base setting"), style = "margin-bottom: -10px;margin-left: -10px;"),
                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                        fluidRow(
                            column(width = 11, align="left", offset = 2, style = "margin-top: -0px;",
                                HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Gene filter</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("基因过滤，提供3种统计值可供过滤选择，Var为方差，Mean为均值，Mad为绝对中为差。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("0.25：前25%，0.5：前50%，0.75：前75%， 1：全部（不进行过滤）"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 3, align="left", offset = 0,
                                    shinyWidgets::pickerInput(inputId = "tsa_filter_method",label = NULL, choices = c("Var", "Mean", "Mad"), selected = "Var", width = "100%")
                                ),
                            column(width = 7, align="left", offset = 0,
                                    shinyWidgets::pickerInput(inputId = "tsa_filter_value",label = NULL, choices = c(0.25, 0.5, 0.75, 1), selected = 0.25, width = "100%")
                                )
                        ),# close fluidRow
                        fluidRow(
                            column(width = 11, align="left", offset = 2, style = "margin-top: -0px;",
                                HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Number of clusters</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("分类数目"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 10, align="left", offset = 0,
                                    shinyWidgets::pickerInput(inputId = "tsa_cluster_num",label = NULL, choices = c(1:10), selected = 10, width = "100%")
                                )
                        ),# close fluidRow
                        fluidRow(
                            column(width = 11, align="left", offset = 2, style = "margin-top: -0px;",
                                HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Set seed </div>")),
                            column(width = 2, align = "center", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("Set seed 作用是设定生成随机数的种子，目的是为了让结果具有重复性，重现结果。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("设置不同的seed将会出现不同的结果。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 10, align="left", offset = 0,
                                    shinyWidgets::pickerInput(inputId = "tsa_seed",label = NULL, choices = c(1:1000), selected = 123, width = "100%"))
                        ),# close fluidRow
                        br(),br(),br(),
                        column(width = 12, align = "left", h4("Graph setting"), style = "margin-bottom: -10px;margin-left: -10px;"),
                        HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                        fluidRow(
                            column(width = 5, align="left", offset = 2, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Axis Y min</div>")),
                            column(width = 5, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Axis Y max</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("设置Y轴范围， 当设置为0，0时，将变成自动模式"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )
                            ),
                            column(width = 5, offset = 0, numericInput("tsa_ylim_min", label = NULL, value = -0, width = "100%")),
                            column(width = 5, offset = 0, numericInput("tsa_ylim_max", label = NULL, value = 0, width = "100%"))
                        ),# close fluidRow
                        fluidRow(
                            column(width = 5, align="left", offset = 2, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Images per row</div>")),
                            column(width = 5, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Images per col</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("设置每行每列的图像数量"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 5, offset = 0, shinyWidgets::sliderTextInput(inputId = "tsa_line_image",label = NULL, choices = seq(from = 2,to = 5,by = 1), selected = 5, grid = TRUE)),
                            column(width = 5, offset = 0, shinyWidgets::sliderTextInput(inputId = "tsa_col_image",label = NULL, choices = seq(from = 2,to = 3,by = 1), selected = 2, grid = TRUE)),
                        ),# close fluidRow
                        fluidRow(
                            column(width = 5, align="left", offset = 2, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>X title</div>")),
                            column(width = 5, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Y title</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("图像的X轴与Y轴坐标"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 5, offset = 0, textInput("tsa_x_title", value = "", label = NULL)),
                            column(width = 5, offset = 0, textInput("tsa_y_title", value = "", label = NULL)),
                        ),# close fluidRow
                        fluidRow(
                            column(width = 10, align="left", offset = 2, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Graph colour</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("设置图像颜色，原理是将这5个颜色均匀地设置成渐变色"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 3, colourpicker::colourInput("tsa_col1", label = NULL, "yellow")),
                            column(width = 3, colourpicker::colourInput("tsa_col2", label = NULL, "green")),
                            column(width = 3, colourpicker::colourInput("tsa_col3", label = NULL, "blue")),
                            column(width = 3, offset = 2, colourpicker::colourInput("tsa_col4", label = NULL, "red")),
                            column(width = 3, colourpicker::colourInput("tsa_col5", label = NULL, "black")),
                            column(width = 4, checkboxInput("tsa_fancy", label = "Default colour", value = TRUE))
                        ),# close fluidRow
                        fluidRow(
                            column(width = 5, align="left", offset = 2, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Width</div>")),
                            column(width = 5, align="left", offset = 0, style = "margin-top: -0px;",HTML("<div style='font-size:16px;font-weight:600;line-height: 2em;'>Height</div>")),
                            column(width = 2, align = "left", offset = 0,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("设置下载图片的宽高和格式，注意：宽高改变在网页端是不会有变化，需要下载后在本地查看才有变化。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 5, numericInput("tsa_width", label = NULL, value = 8)),
                            column(width = 5, numericInput("tsa_height", label = NULL, value = 4)),
                            column(width = 10, offset = 2, align = "center", downloadButton("download_tsa_plot", label = "Download", style = "width:100%; margin-top: 50px;"))
                        ),# close fluidRow
                ),
                HTML('<hr style="width: 100%;border-top: 2px solid #000;" />')
            )
       })
    }
})


#file upload stat
values_tsa <- reactiveValues(upload_tsa_matrix = "NULL", upload_tsa_groupinfo = "NULL")
observeEvent(input$tsa_upload_matrix, {values_tsa$upload_tsa_matrix <- 'uploaded'})
observeEvent(input$tsa_upload_groupinfo, {values_tsa$upload_tsa_groupinfo <- 'uploaded'})

#enter
observeEvent(input$enter_tsa, {
    if(values_tsa$upload_tsa_matrix == "uploaded" & values_tsa$upload_tsa_groupinfo == "uploaded"){
        countdata <- input$tsa_upload_matrix
        groupinfo <- input$tsa_upload_groupinfo
        
        #判断matrix格式
        fileformat <- strsplit(countdata$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
        countdata <- read.csv(countdata$datapath, header = TRUE, row.names = 1)
        }else if (fileformat == "xlsx") {
            countdata <- openxlsx::read.xlsx(countdata$datapath, colNames = TRUE, rowNames = T)
        }else {
            shinyjs::runjs('alert("ERROR: Format of matrix file must be <.csv> or <.xlsx>.")')
            shinyjs::reset("tsa_upload_matrix")
            shinyjs::reset("tsa_upload_groupinfo")
            values_tsa$upload_tsa_matrix = "NULL"
            values_tsa$upload_tsa_groupinfo = "NULL"
            return(NULL)
        }
        #判断matrix是否数值
        data <- unlist(matrix(countdata))
        if (typeof(countdata) == "character") {
                shinyjs::runjs('alert("ERROR: The matrix must be numeric.")')
                shinyjs::reset("tsa_upload_matrix")
                shinyjs::reset("tsa_upload_groupinfo")
                values_tsa$upload_tsa_matrix = "NULL"
                values_tsa$upload_tsa_groupinfo = "NULL"
                return(NULL)
        }
        #判断groupinfo格式
        fileformat <- strsplit(groupinfo$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            groupinfo <- read.csv(groupinfo$datapath, header = TRUE)
        }else if (fileformat == "xlsx") {
            groupinfo <- openxlsx::read.xlsx(groupinfo$datapath, colNames = TRUE, rowNames = F)
        }else {
                shinyjs::runjs('alert("ERROR: Format of group info file must be <.csv> or <.xlsx>.")')
                shinyjs::reset("tsa_upload_groupinfo")
                shinyjs::reset("tsa_upload_matrix")
                values_tsa$upload_tsa_matrix = "NULL"
                values_tsa$upload_tsa_groupinfo = "NULL"
                return(NULL)
        }
        #判断groupinfo的名字和matrix是否符合
        allsample <- groupinfo[, 1]
        cpmsamplename <- colnames(countdata)
        for (i in allsample) {
            if (i %in% cpmsamplename == FALSE) {
            shinyjs::runjs('alert("ERROR: The sample name in group info does not correspond to the sample name in matrix.")')
            shinyjs::reset("tsa_upload_matrix")
            shinyjs::reset("tsa_upload_groupinfo")
            values_tsa$upload_tsa_matrix = "NULL"
            values_tsa$upload_tsa_groupinfo = "NULL"
            return(NULL)
            }
        }
        
        timeseriesanalysis_process$value <- "process"

    }else if (values_tsa$upload_tsa_matrix == "uploaded" & values_tsa$upload_tsa_groupinfo != "uploaded") {
            shinyjs::runjs('alert("Please upload groupinfo file")')
            shinyjs::reset("tsa_upload_matrix")
            shinyjs::reset("tsa_upload_groupinfo")
            values_tsa$upload_tsa_matrix = "NULL"
            values_tsa$upload_tsa_groupinfo = "NULL"
            return(NULL)
    }else if (values_tsa$upload_tsa_matrix != "uploaded" & values_tsa$upload_tsa_groupinfo == "uploaded") {
            shinyjs::runjs('alert("Please upload matrix file")')
            shinyjs::reset("tsa_upload_matrix")
            shinyjs::reset("tsa_upload_groupinfo")
            values_tsa$upload_tsa_matrix = "NULL"
            values_tsa$upload_tsa_groupinfo = "NULL"
            return(NULL)
    }else {
       return(NULL)
    }
})

#读取矩阵，均一化，取均值，输出矩阵
tsa_matrix <- reactive({
    if(timeseriesanalysis_process$value == "process"){
        countdata <- input$tsa_upload_matrix
        groupinfo <- input$tsa_upload_groupinfo
        #读取matrix
        fileformat <- strsplit(countdata$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            countdata <- read.csv(countdata$datapath, header = TRUE, row.names = 1)
        }else if (fileformat == "xlsx") {
            countdata <- openxlsx::read.xlsx(countdata$datapath, colNames = TRUE, rowNames = T)
        }
        #均一化（自定义步骤）
        if(input$tsa_cpm == "YES"){
            countdata <- t(t(countdata) / colSums(countdata) * 1000000)
            countdata <- data.frame(countdata)
        }
        #读取groupinfo
        fileformat <- strsplit(groupinfo$datapath, "\\.")[[1]][-1]
        if (fileformat == "csv") {
            groupinfo <- read.csv(groupinfo$datapath, header = TRUE, row.names = 1)
        }else if (fileformat == "xlsx") {
            groupinfo <- openxlsx::read.xlsx(groupinfo$datapath, colNames = TRUE, rowNames = T)
        }
        #取每个样品的均值
        countdata <- countdata %>% select(rownames(groupinfo))
        colnames(countdata) <- groupinfo$group
        countdata <- as.data.frame( # sapply returns a list here, so we convert it to a data.frame
            sapply(unique(names(countdata)), # for each unique column name
                    function(col) rowMeans(countdata[names(countdata) == col]) # calculate row means
            )
        )
        return(countdata)
    }else {
       return(NULL)
    }
})

#基因过滤
tsa_matrix_filter <- reactive({
    if(!is.null(tsa_matrix())) {
        countdata <- tsa_matrix()
        if(input$tsa_filter_method == "Var"){
            countdata$var <- apply(countdata, 1,var)
            filter_vlaue <- ifelse(is.null(input$tsa_filter_value), 0.75, 1-as.numeric(input$tsa_filter_value))
            countdata <- countdata[countdata$var >= quantile(countdata$var, filter_vlaue),][1:(ncol(countdata)-1)]

        }else if (input$tsa_filter_method == "Mean") {
            countdata$var <- apply(countdata, 1,mean)
            filter_vlaue <- ifelse(is.null(input$tsa_filter_value), 0.75, 1-as.numeric(input$tsa_filter_value))
            countdata <- countdata[countdata$var >= quantile(countdata$var, filter_vlaue),][1:(ncol(countdata)-1)]

        }else if (input$tsa_filter_method == "Mad") {
            countdata$var <- apply(countdata, 1,mad)
            filter_vlaue <- ifelse(is.null(input$tsa_filter_value), 0.75, 1-as.numeric(input$tsa_filter_value))
            countdata <- countdata[countdata$var >= quantile(countdata$var, filter_vlaue),][1:(ncol(countdata)-1)]

        }
        return(countdata)
    }else {
       return(NULL)
    }
})

#矩阵基础信息
output$tsa_base_info <- renderText({
    if(!is.null(tsa_matrix()) & !is.null(tsa_matrix_filter())){
        before_filter <- tsa_matrix()
        after_filter <- tsa_matrix_filter()
        
        geneset_filename <- paste0("File name: ", input$tsa_upload_matrix[1])
        filter_method <- ifelse(is.null(input$tsa_filter_method), "Filtration method: Var", paste0("Filtration method: ", input$tsa_filter_method))
        filter_value <- ifelse(is.null(input$tsa_filter_value), "Filtering threshold: 0.25", paste0("Filtering threshold: ", as.numeric(input$tsa_filter_value)))
        before_filter_num <- paste0("Number of genes before filtration: ", nrow(before_filter))
        after_filter_num <- paste0("Number of genes after filtration: ", nrow(after_filter))
        filter_num <- paste0("Number of genes filtered: ", nrow(before_filter) - nrow(after_filter))

        a <- HTML(paste0("<div style='font-size:25px;font-weight:600;line-height: 1em;'><br/>Basic information</div>",
                        "<br/>", geneset_filename, "<br/>", filter_method, "<br/>", filter_value, "<br/>", before_filter_num, "<br/>", after_filter_num, "<br/>", filter_num,"<br/><br/>"))

    }else {
       return(NULL)
    }
})

#过滤后矩阵输出并下载
output$tsa_test2 <- DT::renderDataTable(tsa_matrix_filter(), rownames = T, selection = list(mode = 'multiple'), options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T, 
                                                                                                                                                      rowCallback = DT::JS(
                                                                                                                                                                          "function(row, data) {",
                                                                                                                                                                            "for (i = 1; i < data.length; i++) {",
                                                                                                                                                                              "if (data[i] != null){",
                                                                                                                                                                                "if (data[i]>1 | data[i]<1){",
                                                                                                                                                                                  "$('td:eq('+i+')', row).html(data[i].toFixed(5));", # vlaue.toExponential(3)科学计数法
                                                                                                                                                                                "}",
                                                                                                                                                                              "}else{$('td:eq('+i+')', row).html('');}",
                                                                                                                                                                            "}",
                                                                                                                                                                          "}"),
                                                                                                                                                      columnDefs = list(
                                                                                                                                                                        list(className = 'dt-center', 
                                                                                                                                                                              targets = '_all')
                                                                                                                                                                        )
                                                                                                                                                      )
)
output$download_tsa_after_filter <- downloadHandler(#下载enrich_geneset模板
    filename = function() {"After_filtration.csv"},
    content = function(file) {
      data <- tsa_matrix_filter()
      write.csv(data, file, fileEncoding="GBK", quote = FALSE)
    }
)

#新建tsa的tmp文件夹
tsa_tmp_filepath <- reactive({
    if(!is.null(tsa_matrix_filter())){
        prjpath <- prjpath("timeseriesanalysis")
        return(prjpath)
    }else {
       return(NULL)
    }
})

#Mfuzz
mfuzz <- reactive({
    if(!is.null(tsa_matrix_filter()) & !is.null(tsa_tmp_filepath())){
        data <- tsa_matrix_filter()
        data <- as.matrix(data)
        mfuzz_class <- new('ExpressionSet', exprs = data)

        mfuzz_class <- Mfuzz::filter.NA(mfuzz_class, thres = 0.25)
        mfuzz_class <- Mfuzz::fill.NA(mfuzz_class, mode = 'mean')
        mfuzz_class <- Mfuzz::filter.std(mfuzz_class, min.std = 0, visu = F)
        mfuzz_class <- Mfuzz::standardise(mfuzz_class)
        set.seed(as.numeric(input$tsa_seed))
        cluster_num <- as.numeric(input$tsa_cluster_num)
        mfuzz_cluster <- Mfuzz::mfuzz(mfuzz_class, c = cluster_num, m = mestimate(mfuzz_class))

        plot_path <- paste0(tsa_tmp_filepath(), "/mfuzz_plot.png")
        height <- ifelse(is.null(input$tsa_height), 4, input$tsa_height)
        width <- ifelse(is.null(input$tsa_width), 8, input$tsa_width)


        png(file = plot_path, height = height, width = width, units = "in", res = 200)

        if(input$tsa_fancy == T){#需在R4.1上运行，4.2会报错
            Mfuzz::mfuzz.plot2(mfuzz_class, cl = mfuzz_cluster, time.labels = colnames(data), colo = "fancy", centre = T, x11 = F,
                                mfrow = c(as.numeric(input$tsa_col_image), as.numeric(input$tsa_line_image)),
                                ylim.set=c(as.numeric(input$tsa_ylim_min), as.numeric(input$tsa_ylim_max)), 
                                xlab = input$tsa_x_title, ylab = input$tsa_y_title)
        }else {
            my_colors <- colorRampPalette(c(input$tsa_col1, input$tsa_col2, input$tsa_col3, input$tsa_col4, input$tsa_col5))
            my_colors <- my_colors(20)
            Mfuzz::mfuzz.plot2(mfuzz_class, cl = mfuzz_cluster, time.labels = colnames(data), colo = my_colors, centre = T, x11 = F,
                                mfrow = c(as.numeric(input$tsa_col_image), as.numeric(input$tsa_line_image)),
                                ylim.set=c(as.numeric(input$tsa_ylim_min), as.numeric(input$tsa_ylim_max)), 
                                xlab = input$tsa_x_title, ylab = input$tsa_y_title)
        }

        dev.off()

        cluster <- mfuzz_cluster$cluster
        cluster <- cbind(data[names(cluster), ], cluster)
        cluster <- as.data.frame(cluster)
        cluster$gene_id <- rownames(cluster)
        cluster <- select(cluster, c(ncol(cluster), ncol(cluster)-1), everything())#把gene_id列和cluster列排前面
        cluster$cluster <- paste0("Cluster_", cluster$cluster)
        # file_name <- paste0(tsa_tmp_filepath(), "/mfuzz_result.csv")
        # write.csv(cluster, file_name, row.names = F, quote = F)

        return(cluster)
    }else {
       return(NULL)
    }
})
output$tsa_plot <- renderImage({
    if(!is.null(mfuzz()) & !is.null(tsa_tmp_filepath())){
        list(src = paste0(tsa_tmp_filepath(), "/mfuzz_plot.png"), contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})
output$download_tsa_plot <- downloadHandler(#下载
    filename = function() { "tsa_plot.png"},
    content = function(file) {
    file.copy(paste0(tsa_tmp_filepath(), "/mfuzz_plot.png"), file)
})

#Mfuzz result table
output$tsa_test1 <- DT::renderDataTable(mfuzz(), rownames = F, selection = list(mode = 'multiple'), options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T, 
                                                                                                                                                      rowCallback = DT::JS(
                                                                                                                                                                          "function(row, data) {",
                                                                                                                                                                            "for (i = 3; i < data.length; i++) {",
                                                                                                                                                                              "if (data[i] != null){",
                                                                                                                                                                                "if (data[i]>1 | data[i]<1){",
                                                                                                                                                                                  "$('td:eq('+i+')', row).html(data[i].toFixed(5));", # vlaue.toExponential(3)科学计数法
                                                                                                                                                                                "}",
                                                                                                                                                                              "}else{$('td:eq('+i+')', row).html('');}",
                                                                                                                                                                            "}",
                                                                                                                                                                          "}"),
                                                                                                                                                      columnDefs = list(
                                                                                                                                                                        list(className = 'dt-center', 
                                                                                                                                                                              targets = '_all')
                                                                                                                                                                        )
                                                                                                                                                      )
)
Mfuzz_cluster_stat <- reactive({
    if(!is.null(mfuzz())){
        data <- mfuzz()
        stat <- data.frame(table(data$cluster))
        stat[, 1] <- as.character(stat[, 1])
        stat[, 2] <- as.numeric(stat[, 2])
        stat <- rbind(stat, c("All clusters", sum(stat[,2])))
        stat <- t(stat)
        return(stat)
    }else {
       return(NULL)
    }
})
output$tsa_mufzz_stat <- renderTable(rownames = F, colnames = F, Mfuzz_cluster_stat())
output$download_tsa_mfuzz_result <- downloadHandler(#下载enrich_geneset模板
    filename = function() {"TSA_result.csv"},
    content = function(file) {
      data <- mfuzz()
      write.csv(data, file, fileEncoding="GBK", quote = FALSE, row.names=F)
    }
)