wgcna_process <- reactiveValues(value = "start", step2 ="stop", step3 ="stop", step4 ="stop", step5 ="stop")
# library(WGCNA)

# render UI
observeEvent(wgcna_process$value, {
    if (wgcna_process$value == "start") {
        output$ui_wgcna <- renderUI({
            fluidRow(style = "",
                    column(width = 12, align = "center",
                    HTML("<p style='font-size:20px;color:black' class='title'>WARNNING:  <br/>"),
                    HTML("<p style='font-size:20px;color:black' class='title'>WGCNA is an advanced analytics methods of RNAseq, we only provides an implementation method to make this happen, the content of the analysis is not explained <br/>"),
                    HTML("<p style='font-size:20px;color:black' class='title'>So users must have a full understanding of WGCNA and a complete experimental design before using this function. <br/>"),
                    ),
                    br(), br(), br(), br(), br(),
                    column(width = 6, align = "center", style = "left: 450px;margin-top: 30px;",
                        fluidRow(
                            column(width = 8, align="left", offset = 4,
                                HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step1: Upload you Matrix</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                    tags$h4(p(icon("check",lib = "font-awesome"),em("上传基因表达矩阵数据，文件格式仅支持 .csv .xlsx"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                    column(width = 12, align = "center", downloadButton("download_deg_matrix_template_wgcna", label = "Template matrix", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                                    circle = TRUE, status = "default",
                                    icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )),
                            column(width = 8, align="left", offset = 0,
                                fileInput(inputId = "wgcna_upload_matrix", label = NULL, accept = ".csv", width = "340px"))
                        ),# close fluidRow
                        br(),
                        fluidRow(
                            column(width = 8, align="left", offset = 4,
                                    HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step2: Download you Group info</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                tags$h4(p(icon("check",lib = "font-awesome"),em("下载您的分组信息表，并且修改group列（sample列不需要修改），修改后于Step3上传。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                circle = TRUE, status = "default",
                                icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )
                            ),
                            column(width = 8, align="left", offset = 0,
                                    downloadButton(outputId = "wgcna_download_groupinfo", label = "Download", icon = icon("download"), style = "width: 340px;margin-bottom: -10px;"))
                        ),# close fluidRow
                        br(),br(),br(),
                        fluidRow(
                            column(width = 8, align="left", offset = 4,
                                    HTML("<div style='font-size:20px;font-weight:600;line-height: 2em;'>Step3: Upload you Group info</div>")),
                            column(width = 1, align = "right", offset = 3,
                                shinyWidgets::dropdownButton(
                                tags$h4(p(icon("check",lib = "font-awesome"),em("上传您的样品分组文件（下载step2文件，并修改group列，相同组别的样品设置同一组名）。"),style="color:#337ab7;text-align:center;font-size:15px;")),
                                column(width = 12, align = "center", downloadButton("download_deg_group_template2", label = "Template groupinfo", style = "border-color: #ccc;font-size:12px;text-align: center;")),
                                circle = TRUE, status = "default",
                                icon = icon("question"), width = "300px", size = "sm", right = F, up = T
                                )
                            ),
                            column(width = 8, align="left", offset = 0,
                                    fileInput(inputId = "wgcna_upload_groupinfo", label = NULL, accept = ".csv", width = "340px"))
                        ),# close fluidRow
                    ), #close cloumn
                    br(), 
                    column(width = 4, offset = 4,
                            column(width = 6, align="left", offset = 0, style="margin-bottom: 80px;margin-top: 40px;",
                                    actionButton(inputId = "reset_wgcna", label = "RESET", icon = icon("rotate-left"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;")),
                            column(width = 6, align="right", offset = 0, style="margin-bottom: 80px;margin-top: 40px;",
                                    actionButton(inputId = "enter_wgcna", label = "ENTER", icon = icon("check"), style = "width:150px;height:50px;font-size: 20px;font-weight: 1000;background-color: #639fbe;color: white;"))
                    )
            )# close fluidRow
        })
    } else if (wgcna_process$value == "running") {
       output$ui_wgcna <- renderUI({
            fluidRow(
                br(),
                column(width = 12, h3("First step : Data preprocessing")),
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                column(width = 12,
                        column(width = 7, 
                        column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"),"Before", "</p>"))),
                        column(width = 12, DT::dataTableOutput(outputId = "wgcna_matrix_before", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;"),

                        column(width = 2, checkboxInput("wgcna_cpm", "CPM Normalization", value = TRUE), style="margin-bottom: 0px;font-size: 15px;margin-top: 15px;"),
                        column(width = 2, numericInput("wgcna_filterlow_cutoff", label = "Cutoff low expression", value = 2, width = "100%"), style="margin-bottom: 0px;border-left: 3px solid #ddd"),
                        column(width = 2, numericInput("wgcna_filterlow_sample_cutoff", label = "Low expression sample", value = 3, width = "100%"), style="margin-bottom: ;border-right: 3px solid #ddd;"),
                        column(width = 2, selectInput("wgcna_select_filter_mode", "Select a filtering method", choices = c("mad", "var", "mean"), width = "100%"), style="margin-bottom: 0px"),
                        column(width = 3, numericInput("wgcna_select_filter_value", label = "A few percent smaller than the method will be filtered out", value = 98, min = 1, max = 100, step = 5, width = "100%"), style="margin-bottom: 0px;border-right: 3px solid #ddd"),
                        column(width = 1, numericInput("wgcna_filterlow_setseed_color", label = "Random colors", value = 1, width = "100%"), style="margin-bottom: 0px"),

                        column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                                        HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"),"After", "</p>"))),
                        column(width = 12, DT::dataTableOutput(outputId = "wgcna_matrix_after", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;")
                    ),
                    column(width = 5, imageOutput(outputId = "sample_dendrogram", height = "600px",width = "100%") %>% withSpinner()),
                    
                ),

                uiOutput("wgcna_step2")
            )
       })
    }
})

#upload stat
wgcna_upload <- reactiveValues(file = NULL, group = NULL)
observeEvent(input$wgcna_upload_matrix, {wgcna_upload$file = "uploaded"})
observeEvent(input$wgcna_upload_groupinfo, {wgcna_upload$group = "uploaded"})

# enter_wgcna
observeEvent(input$enter_wgcna, {
    req(wgcna_upload$file, wgcna_upload$group)
    if(wgcna_upload$file == "uploaded" & wgcna_upload$group == "uploaded"){
        file <- read.csv(input$wgcna_upload_matrix$datapath, header = T, row.names = 1)
        group <- read.csv(input$wgcna_upload_groupinfo$datapath, header = T, row.names = 1)
        #判断groupname样品是否在data里
        for (i in rownames(group)) {
            if (i %in% colnames(file) == FALSE) {
                shinyjs::runjs('alert("ERROR: The sample name in group info does not correspond to the sample name in matrix.")')
                shinyjs::reset("wgcna_upload_matrix")
                shinyjs::reset("wgcna_upload_groupinfo")
                wgcna_upload$file = NULL
                wgcna_upload$group = NULL
                return(NULL)
            }
        }
        wgcna_process$value = "running"
    }else {
        shinyjs::runjs('alert("ERROR: Please upload matrix and group info.")')
        shinyjs::reset("wgcna_upload_matrix")
        shinyjs::reset("wgcna_upload_groupinfo")
        wgcna_upload$file = NULL
        wgcna_upload$group = NULL
    }
})

# reset_wgcna
observeEvent(input$reset_wgcna, {
    shinyjs::reset("wgcna_upload_matrix")
    shinyjs::reset("wgcna_upload_groupinfo")
    wgcna_upload$file = NULL
    wgcna_upload$group = NULL
})

# download group_info
output$wgcna_download_groupinfo <- downloadHandler(#下载groupinfo模板
  filename = function() {"groupinfo.csv"},
  content = function(file) {
    if(wgcna_upload$file == "uploaded") {

        group <- read.csv(input$wgcna_upload_matrix$datapath, header = T, row.names = 1)
        
        group <- colnames(group)

        group <- data.frame(group)
        rownames(group) <- group$group
        write.csv(group, file, row.names = TRUE, fileEncoding="GBK", quote = FALSE)
    }else {
        shinyjs::runjs('alert("ERROR: Please upload matrix and group info.")')
        return(NULL)
    }
  }
)

# wgcna_matrix
wgcna_matrix_before <- reactive({
    req(input$wgcna_upload_matrix, input$wgcna_upload_groupinfo)
    if(wgcna_process$value == "running"){
        wgcna_matrix_before <- read.csv(input$wgcna_upload_matrix$datapath, header = T, row.names = 1)
        return(wgcna_matrix_before)
    }else {
       return(NULL)
    }
})
output$wgcna_matrix_before <- DT::renderDataTable(wgcna_matrix_before(), rownames = T, options = list(dom = 'fltipr', pageLength = 2, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
#

wgcna_matrix_after <- reactive({
    req(input$wgcna_upload_matrix, input$wgcna_upload_groupinfo)
    if(wgcna_process$value == "running"){
        wgcna_matrix_before <- wgcna_matrix_before()
        # wgcna_matrix_before <- wgcna_matrix_before[!apply(wgcna_matrix_before, 1, function(x){sum(floor(x)==0)>0}),]
        if(input$wgcna_cpm == T){
            group_list <- wgcna_group()[,1]
            wgcna_matrix_before <- edgeR::DGEList(wgcna_matrix_before, group = group_list)
            wgcna_matrix_before <- edgeR::cpm(wgcna_matrix_before, group = group_list)
            wgcna_matrix_before <- data.frame(wgcna_matrix_before)
        }
        wgcna_matrix_before <- wgcna_matrix_before[apply(wgcna_matrix_before, 1, function(x){sum(x>input$wgcna_filterlow_cutoff)>=input$wgcna_filterlow_sample_cutoff}),] 

        wgcna_matrix_before$mad <- apply(wgcna_matrix_before, 1, input$wgcna_select_filter_mode)
        wgcna_matrix_before <- wgcna_matrix_before[order(wgcna_matrix_before$mad, decreasing = T),]

        wgcna_matrix_after <- wgcna_matrix_before[wgcna_matrix_before$mad >= quantile(wgcna_matrix_before$mad, (input$wgcna_select_filter_value/100)),][1:(ncol(wgcna_matrix_before)-1)]
        return(wgcna_matrix_after)
    }else {
       return(NULL)
    }
})
output$wgcna_matrix_after <- DT::renderDataTable(wgcna_matrix_after(), rownames = T, options = list(dom = 'fltipr', pageLength = 2, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
#

# wgcna_group
wgcna_group <- reactive({
    req(input$wgcna_upload_matrix, input$wgcna_upload_groupinfo)
    if(wgcna_process$value == "running"){
        wgcna_group <- read.csv(input$wgcna_upload_groupinfo$datapath, header = T, row.names = 1)
        return(wgcna_group)
    }else {
       return(NULL)
    }
})

# 新建wgcna的tmp文件夹
wgcna_tmp_filepath <- reactive({
    req(input$enter_wgcna)
    if(wgcna_process$value == "running"){
        prjpath <- prjpath("wgcna")
        return(prjpath)
    }else {
       return(NULL)
    }
})

## ==================step 1
#sample dendrogram
sample_dendrogram <- reactive({
    req(wgcna_matrix_after(), wgcna_group(), wgcna_tmp_filepath())
    data_filter <- wgcna_matrix_after()
    group_list <- wgcna_group()[,1]

    data_Expr <- log2(t(data_filter)+1)
    datTraits <- group_list
    datExpr_tree<-hclust(dist(data_Expr), method = "average")

    set.seed(input$wgcna_filterlow_setseed_color)
    random_colors <- sample(colors(), length(unique(group_list)))
    sample_colors1 <- WGCNA::numbers2colors(as.numeric(factor(datTraits)), 
                                 colors = random_colors, signed = FALSE)
    col=as.matrix(data.frame(group_list=sample_colors1))
    par(mar = c(1,4,3,1),cex=0.8)
    png(paste0(wgcna_tmp_filepath(),"/step1-sample-subtype-cluster.png"),width = 700,height = 600)
    WGCNA::plotDendroAndColors(datExpr_tree, col,
                    groupLabels = colnames(sample),
                    cex.dendroLabels = 0.8,
                    marAll = c(2, 4, 6, 1),
                    cex.rowText = 0.01,
                    main = "Sample dendrogram and trait heatmap")
    dev.off()
    return(paste0(wgcna_tmp_filepath(),"step1-sample-subtype-cluster.png"))
})
output$sample_dendrogram <- renderImage({
    if(!is.null(sample_dendrogram())){
        list(src = sample_dendrogram(), contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})




## ==================step 2
observeEvent(input$wgcna_step1_ok, {wgcna_process$step2 = "start"})
output$wgcna_step2 <- renderUI({
    if(wgcna_process$step2 == "start"){
        column(width = 12,
            br(),
            column(width = 12, h3("Second step : Power estimate")),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 4, offset = 1, align = "center", tableOutput(outputId = "wgcna_sft_fitIndices"), style = "font-size: 11px;"),
            column(width = 6, offset = 0, align = "center", imageOutput(outputId = "wgcna_powerEstimate", height = "600px",width = "100%") %>% withSpinner(), style = "margin-top: -20px;"),
            column(width = 6, offset = 3, align = "center", selectInput("wgcna_power_input", "Power(Soft thresholds) selection", choices = wgcna_powerEstimate()$sft, width = "100%")),
            uiOutput("wgcna_warnning_power_ui"),
            uiOutput("wgcna_step3")
        )
    }else {
       column(width = 12, 
            column(width = 12, actionButton("wgcna_step1_ok", "!!!! Click to confirm that the data cleansing is complete and proceed to the second step !!!!", width = "100%", style = "background-color: pink;"), style="margin-bottom: 50px;")
       )
    }
})

output$wgcna_warnning_power_ui <- renderUI({
    if(length(wgcna_powerEstimate()$sft) == 30){
        column(width = 12, align = "center", style = "margin-bottom: 30px;",
            HTML("<p style='font-size:20px;color:red' class='title'>WARNNING: No suitable soft threshold to choose from, and it is recommended to adjust the data.<br/>"),
            HTML("<p style='font-size:20px;color:red' class='title'>You can use empirical soft thresholds: Samples<20 Power = 9; Samples<30 Power = 8; Samples<40 Power = 7;"))
    }else {
       column(width = 12, align = "center", style = "margin-bottom: 30px;",
            HTML("<p style='font-size:20px;color:black' class='title'>Recommendation of Power: SFT.R.sq ≥ 0.8; slope ≥ 0.5; mean.k ≤ 150<br/>"))
    }
})

wgcna_powerEstimate <- reactive({
    req(input$wgcna_step1_ok)
    WGCNA::allowWGCNAThreads()
    withProgress(message = 'Running step 2 : Power estimate',
      detail = 'This may take a few minutes...', value = 1, {
        for (i in 1: 1) {

            data_filter <- wgcna_matrix_after()
            group_list <- wgcna_group()[,1]
            data_Expr <- log2(t(data_filter)+1)
            datTraits <- group_list

            powers = c(c(1:10), seq(from = 12, to=30, by=2))
            sft = WGCNA::pickSoftThreshold(data_Expr, powerVector = powers, verbose = 5)

            png(paste0(wgcna_tmp_filepath(), "step2-beta-value.png"),width = 800,height = 600)
                par(mfrow = c(1,2));
                cex1 = 0.7;
                plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
                xlab=("Soft Threshold (power)"),ylab="Scale Free Topology Model Fit,signed R^2",type="n",
                main = paste("Scale independence"));
                text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
                labels=powers,cex=cex1,col="red");
                # abline(h=0.85,col="red")
                plot(sft$fitIndices[,1], sft$fitIndices[,5],
                    xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
                    main = paste("Mean connectivity"))
                text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")
            dev.off()
            my_list <- list()
            a <- data.frame(t(sft$fitIndices))
            for (i in a) {
                print(i)
                if(i[2]>=0.8 & i[3]<0 & i[5]<=150){
                    my_list[[length(my_list) + 1]] <- i[1]
                }
            }

            if(length(my_list) == 0){
                my_list <- c(c(1:30))
            }
        }
      }
    )
    return(list(sft = unlist(my_list), imgfile = paste0(wgcna_tmp_filepath(), "step2-beta-value.png"), fitIndices = sft$fitIndices))
})

output$wgcna_sft_fitIndices <- renderTable(wgcna_powerEstimate()$fitIndices , colnames = T)

output$wgcna_powerEstimate <- renderImage({
    if(!is.null(wgcna_powerEstimate())){
        list(src = wgcna_powerEstimate()$imgfile, contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})




##  ==================step 3
observeEvent(input$wgcna_step2_ok, {wgcna_process$step3 = "start"})
output$wgcna_step3 <- renderUI({
    if(wgcna_process$step3 == "start"){
        column(width = 12,
            br(),
            column(width = 12, h3("Third step : Build the co-expression network"), style = "margin-top: 20px;"),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 3, offset = 0, align = "center", numericInput("wgcna_minModuleSize", label = "minModuleSize", value = 30, min = 1, max = 1000, step = 1, width = "100%")),
            column(width = 3, offset = 0, align = "center", numericInput("wgcna_maxBlockSize", label = "maxBlockSize", value = 10000, min = 5000, max = 30000, step = 1, width = "100%")),
            column(width = 3, offset = 0, align = "center", numericInput("wgcna_mergeCutHeight", label = "mergeCutHeight", value = 0.1, min = 0, max = 1, step = 0.01, width = "100%")),
            column(width = 3, offset = 0, actionButton("wgcna_step3_network", "Click to start building the co-expression network", width = "100%", style = "height: 60px ;font-size: 18px;background-color: lightgreen;")),
            uiOutput("wgcna_step3_network")
        )
    }else {
       column(width = 12, 
            column(width = 12, actionButton("wgcna_step2_ok", "!!!! Click to confirm the power and proceed to the third step !!!!", width = "100%", style = "background-color: pink;"), style="margin-bottom: 50px;")
        )
    }
})

wgcna_step3_network <- reactiveValues(process = "stop")
observeEvent(input$wgcna_step3_network, {wgcna_step3_network$process = "start"})
output$wgcna_step3_network <- renderUI({
    if(wgcna_step3_network$process == "stop"){
        column(width = 12,
            column(width = 12, align = "center", HTML("<p style='font-size:30px;color:black' class='title'>Note: <br/> Building a co-expression network will consume a lot of computing resources for computing, please do not operate during the calculation, please wait patiently and press the green button to start building. <br/>"),)
        )
    }else {
       column(width = 12, style = "margin-bottom: 50px;margin-top: 30px;",
            column(width = 4, offset = 0, align = "center", imageOutput(outputId = "wgcna_module_dendrogram", height = "500px",width = "100%") %>% withSpinner(), style = ""),
            column(width = 4, offset = 0, align = "center", imageOutput(outputId = "wgcna_mes_col", height = "500px",width = "100%") %>% withSpinner(), style = ""),
            column(width = 4, offset = 0, align = "center", imageOutput(outputId = "wgcna_TOMplot", height = "500px",width = "100%") %>% withSpinner(), style = ""),

            uiOutput("wgcna_step4")

       )
    }
})

# bulid co-network
wgcna_blockwiseModules <- reactive({

    WGCNA::allowWGCNAThreads()
    cor <- WGCNA::cor
    withProgress(message = 'Running step 3 : Build the co-expression network',
      detail = 'This will take a lot of time, be patient !', value = 1, {
        for (i in 1: 1) {
            
            WGCNA::allowWGCNAThreads()
            data_filter <- wgcna_matrix_after()
            group_list <- wgcna_group()[,1]
            data_Expr <- log2(t(data_filter)+1)
            datTraits <- group_list

            net = WGCNA::blockwiseModules(
                data_Expr,
                power = input$wgcna_power_input,
                TOMType = "unsigned",
                minModuleSize = input$wgcna_minModuleSize,
                reassignThreshold = 0,
                maxBlockSize = input$wgcna_maxBlockSize,
                mergeCutHeight = input$wgcna_mergeCutHeight,#需要合并模块的阈值
                numericLabels = TRUE, #以数字作为模块的名字
                pamRespectsDendro = FALSE,
                saveTOMs = T,
                saveTOMFileBase = paste0(wgcna_tmp_filepath(), "WGCNA_TOM"),
                verbose = 3
            )
            # print(table(net$colors))

            #模块可视化
            moduleColors <- WGCNA::labels2colors(net$colors)
            png(paste0(wgcna_tmp_filepath(), "step3-Cluster Dendrogram.png"),width = 500,height = 500)
                WGCNA::plotDendroAndColors(
                    net$dendrograms[[1]],
                    moduleColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE,
                    hang = 0.03,
                    addGuide = TRUE,
                    guideHang = 0.05
                )
            dev.off()

            #TOM可视化
            load(net$TOMFiles[1], verbose = TRUE)
            TOM <- as.matrix(TOM)
            dissTOM <- 1-TOM
            plotTOM <- dissTOM^7
            diag(plotTOM) <- NA
            mycolor <- gplots::colorpanel(250, 'red', "orange", 'lemonchiffon')
            png(paste0(wgcna_tmp_filepath(), "step3-Network heatmap plot all genes.png"),width = 500,height = 500)
                WGCNA::TOMplot(
                    plotTOM,
                    # 层次聚类的第一个
                    net$dendrograms[[1]],
                    # 取对应块基因的颜色
                    moduleColors[net$blockGenes[[1]]],
                    main = "Network heatmap plot_all genes",
                    col = mycolor
                )
            dev.off()

            #各模块间相关性
            MEs_col <- net$MEs
            colnames(MEs_col) <- paste0("ME", WGCNA::labels2colors(as.numeric(stringr::str_replace_all(colnames(MEs_col),"ME",""))))
            MEs_col <- WGCNA::orderMEs(MEs_col)
            png(paste0(wgcna_tmp_filepath(), "step3-Eigengene adjacency heatmap.png"),width = 500,height = 500)
                WGCNA::plotEigengeneNetworks(
                    MEs_col,
                    "Eigengene adjacency heatmap",
                    marDendro = c(0.5, 3, 2, 4),
                    marHeatmap = c(2, 3, 0, 2),
                    plotDendrograms = T,
                    xLabelsAngle = 90)#输出6x6的pdf
            dev.off()

            # 写入个模块基因

            modulegenes <- data.frame(gene_id = names(net$colors), moduleColors = moduleColors)
            write.table(modulegenes, paste0(wgcna_tmp_filepath(), "modlegenes.txt"), sep = "\t", quote = F, row.names = F)

            # 写入catoscape
            cytoscape_file <- paste0(wgcna_tmp_filepath(), "cytoscape/")
            dir.create(cytoscape_file)
            TOM <- WGCNA::TOMsimilarityFromExpr(data_Expr, power = input$wgcna_power_input)
            module <- unique(moduleColors)
            for(i in module) {
                probes = colnames(data_Expr)
                inModule = (moduleColors==i)
                modProbes = probes[inModule]

                modTOM = TOM[inModule, inModule]
                dimnames(modTOM) = list(modProbes, modProbes)
                cyt <- WGCNA::exportNetworkToCytoscape(
                    modTOM,
                    edgeFile = paste(cytoscape_file, i, ".edges.txt", sep=""),
                    nodeFile = paste(cytoscape_file, i, ".nodes.txt", sep=""),
                    weighted = TRUE,
                    threshold = 0.02,
                    nodeNames = modProbes, 
                    nodeAttr = moduleColors[inModule]
                )
            }

            # 计算kME
            datKME <- WGCNA::signedKME(data_Expr, MEs_col, outputColumnName="kME_MM.")
            write.csv(datKME, paste0(wgcna_tmp_filepath(), "kME_MM.csv"))  

            return(list(net = net, 
                dendrogram = paste0(wgcna_tmp_filepath(),"step3-Cluster Dendrogram.png"),
                tomplot = paste0(wgcna_tmp_filepath(), "step3-Network heatmap plot all genes.png"),
                mes_col = paste0(wgcna_tmp_filepath(), "step3-Eigengene adjacency heatmap.png"),
                cytoscape = paste0(wgcna_tmp_filepath(), "cytoscape/"),
                group = unique(moduleColors),
                datKME = paste0(wgcna_tmp_filepath(), "kME_MM.csv"),
                data_Expr = data_Expr,
                datTraits = datTraits
            ))
        }
      }
    )

})
output$wgcna_module_dendrogram <- renderImage({
    if(!is.null(wgcna_blockwiseModules())){
        list(src = wgcna_blockwiseModules()$dendrogram, contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})
output$wgcna_TOMplot <- renderImage({
    if(!is.null(wgcna_blockwiseModules())){
        list(src = wgcna_blockwiseModules()$tomplot, contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})
output$wgcna_mes_col <- renderImage({
    if(!is.null(wgcna_blockwiseModules())){
        list(src = wgcna_blockwiseModules()$mes_col, contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})



##  ==================step 4
observeEvent(input$wgcna_step3_ok, {wgcna_process$step4 = "start"})
output$wgcna_step4 <- renderUI({
    if(wgcna_process$step4 == "start"){
        column(width = 12,
            br(),
            column(width = 12, h3("Fourth step : (1) Export the module gene and the network to cytoscape"), style = "margin-top: 20px;"),
            HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
            column(width = 2, selectInput("wgcna_select_NetworkToCytoscape_input", label = "Module genes selection", choices = wgcna_blockwiseModules()$group)),
            column(width = 10, tableOutput(outputId = "wgcna_group_stat_table"), align = "left", style = "font-size: 20px;"),
            column(width = 6,
                column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                            HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"), paste0("Modulegenes"), "</p>"))),
                column(width = 12, DT::dataTableOutput(outputId = "wgcna_select_modulegene", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;")
            ),
            column(width = 6,
                column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                            HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"), paste0("Network To Cytoscape"), "</p>"))),
                column(width = 12, DT::dataTableOutput(outputId = "wgcna_select_NetworkToCytoscape", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;")
            ),

            br(),
            column(width = 12, h3("Fourth step : (2) Export the Module Membership (eigengene-based connectivity, MM, k_ME)"), style = "margin-top: 20px;"),
            column(width = 12,
                column(width = 12, align = "left", style="background-color: #0000001f;height: 50px;font-weight: 900;", 
                            HTML(paste0("<p style='height: 1px;font-size: 20px;line-height: 2.5em;'>",icon("arrow-right"), paste0("Module Membership for each genes"), "</p>"))),
                column(width = 12, DT::dataTableOutput(outputId = "wgcna_k_me", width = "100%"), style="background-color: #AEEEEE;margin-bottom: 20px;")
            ),


            uiOutput("wgcna_step5")
        )
    }else {
       column(width = 12, style = "margin-top: 50px;",
            column(width = 12, actionButton("wgcna_step3_ok", "!!!! Click for forth step to visualize the co-representation network !!!!", width = "100%", style = "background-color: pink;"), style="margin-bottom: 50px;")
        )
    }
})

wgcna_select_modulegene <- reactive({
    file_module <- read.csv(paste0(wgcna_tmp_filepath(), "modlegenes.txt"), sep = "\t")
    return(file_module)
})
filtered_wgcna_select_modulegene <- reactive({
    req(input$wgcna_select_NetworkToCytoscape_input)

    # 读取选择的文件
    data <- wgcna_select_modulegene()
    subset(data, 
            moduleColors == input$wgcna_select_NetworkToCytoscape_input
            )
})
output$wgcna_select_modulegene <- DT::renderDataTable(filtered_wgcna_select_modulegene(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
#

wgcna_net_num <- reactive({
    req(wgcna_blockwiseModules())
    net <- wgcna_blockwiseModules()$net
    group <- WGCNA::labels2colors(net$colors)
    group <- data.frame(table(group))
    group <- t(group)
    return(group)
})
output$wgcna_group_stat_table <- renderTable(rownames = TRUE, wgcna_net_num(), colnames = F)

wgcna_select_NetworkToCytoscape <- reactive({
    file <- read.csv(paste0(wgcna_tmp_filepath(), "cytoscape/", input$wgcna_select_NetworkToCytoscape_input, ".edges.txt"), sep = "\t")
    file <- file[,1:3]
})

output$wgcna_select_NetworkToCytoscape <- DT::renderDataTable(wgcna_select_NetworkToCytoscape(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                                                        columnDefs = list(
                                                                                        list(className = 'dt-center', 
                                                                                                targets = '_all')
                                                                                        )
                                                                        ), selection = "none")
#

wgcna_k_me <- reactive({
    file_k_me <- read.csv(paste0(wgcna_tmp_filepath(), "kME_MM.csv"), sep = ",")
    colnames(file_k_me) <- c("gene_id", colnames(file_k_me)[2:ncol(file_k_me)])
    return(file_k_me)
})
output$wgcna_k_me <- DT::renderDataTable(wgcna_k_me(), rownames = FALSE, options = list(dom = 'fltipr', pageLength = 5, scrollX=TRUE, orderClasses = T,
                                        columnDefs = list(
                                                        list(className = 'dt-center', 
                                                                targets = '_all')
                                                        )
                                        ), selection = "none")
#

##  ==================step 5
observeEvent(input$wgcna_step4_ok, {wgcna_process$step5 = "start"})
output$wgcna_step5 <- renderUI({
    if(wgcna_process$step5 == "start"){
        column(width = 12,
            br(),
            column(width = 6, 
                column(width = 12, h3("Fifth step : Trait and module correlation analysis"), style = "margin-top: 20px;"),
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                uiOutput("step6_start_genesexp_analysis")
            ),
            column(width = 6, 
                column(width = 12, h3("Sixth step : Genes expression and module correlation analysis"), style = "margin-top: 20px;"),
                HTML('<hr style="width: 100%;border-top: 1px solid #ddd;" />'),
                uiOutput("step5_start_trait_analysis")
            ),
            column(width = 12, align = "center", HTML("<p style='font-size:30px;color:black' class='title'>Note: <br/> Please do not operate during the calculation, please wait patiently and press the green button to start. <br/>")),
            column(width = 12, style = "margin-top: 80px;",
                downloadButton("wgcna_download_result", "If the WGCNA analysis is complete, click to download all the analysis results.", style = "width:100%;background-color: pink;"))
        )
    }else {
       column(width = 12, style = "margin-top: 50px;",
            column(width = 12, actionButton("wgcna_step4_ok", "!!!! Click to proceed to the fifth step of trait and module correlation analysis !!!!", width = "100%", style = "background-color: pink;"), style="margin-bottom: 50px;")
        )
    }
})

output$step5_start_trait_analysis <- renderUI({
    if(wgcna_step5_trait$process == "start"){
        column(width = 12, style = "border-left: 1px solid #ddd;",
            column(width = 6, offset = 0, align = "center", imageOutput(outputId = "wgcna_trait_correlation", height = "800px",width = "100%") %>% withSpinner(), style = "border-right: 1px solid #ddd;"),
            column(width = 6,
                column(width = 6, selectInput("wgana_gs_mm_trait_selection", "Trait selection", choices = wgcna_trait_group())),
                column(width = 6, selectInput("wgana_gs_mm_module_selection", "Module selection", choices = wgcna_blockwiseModules()$group)),
                column(width = 12, align = "center", style = "margin-top: 100px;", imageOutput(outputId = "wgcna_trait_MM_plot", height = "500px",width = "100%") %>% withSpinner()),
            )
        )
    }else {
       fluidRow(style = "border-left: 1px solid #ddd;",
            column(width = 3, 
                column(width = 12, align="left", offset = 0,HTML("<div style='font-size:14px;font-weight:600;line-height: 2em;'>Step 1: Upload expression matrix</div>")),
                column(width = 12, align="left", offset = 0, fileInput(inputId = "wgcna_upload_trait_matrix", label = NULL, accept = ".csv", width = "100%"), style = "font-size: 15px;")
            ),
            column(width = 3,
                column(width = 12, align="left", offset = 0,HTML("<div style='font-size:14px;font-weight:600;line-height: 2em;'>Step2: Download expression group info</div>")),
                column(width = 12, align="left", offset = 0, downloadButton(outputId = "wgcna_download_tarit_groupinfo", label = NULL, icon = icon("download"), style = "width: 100%;margin-bottom: -10px;"))
            ),
            column(width = 3,
                column(width = 12, align="left", offset = 0,HTML("<div style='font-size:14px;font-weight:600;line-height: 2em;'>Step 3: Upload expression groupinfo</div>")),
                column(width = 12, align="left", offset = 0, fileInput(inputId = "wgcna_upload_trait_group", label = NULL, accept = ".csv", width = "100%"), style = "font-size: 15px;"),
            ),
            column(width = 3, 
                column(width = 12, offset = 0, actionButton("wgcna_step5_trait_analysis", "Click to start ", width = "100%", style = "height: 95px; font-size: 20px;background-color: lightgreen;")),
            )
       )
    }
})

output$step6_start_genesexp_analysis <- renderUI({
    if(wgcna_step6$process == "start"){
        fluidRow(
            column(width = 6, offset = 0, align = "center", shinyWidgets::sliderTextInput(inputId = "wgcna_trait_width", label = "Width", choices = c(4,5,6,7,8,9,10), grid = TRUE)),
            column(width = 6, offset = 0, align = "center", shinyWidgets::sliderTextInput(inputId = "wgcna_trait_height", label = "Height", choices = c(4,5,6,7,8,9,10), grid = TRUE)),
            column(width = 12, offset = 0, align = "center", imageOutput(outputId = "wgcna_trait_cor", height = "800px",width = "100%") %>% withSpinner())
        )
    }else {
       fluidRow(style = "border-left: 1px solid #ddd;",
            column(width = 3, align = "left", offset = 0, 
                column(width = 12, align="left", offset = 0,HTML("<div style='font-size:14px;font-weight:600;line-height: 2em;'>Step 1: Upload the traits data</div>")),
                column(width = 12, align="left", offset = 0, fileInput(inputId = "wgcna_upload_trait_data", label = NULL, accept = ".csv", width = "100%"), style = "font-size: 15px;")
            ),
            column(width = 3, 
                column(width = 12, offset = 0, actionButton("wgcna_step6_start", "Click to start ", width = "100%", style = "height: 95px; font-size: 20px;background-color: lightgreen;")),
            )
       )
    }
})

###### trait and mes cor ######
wgcna_step6 <- reactiveValues(process = "stop")
# trait upload stat
wgcna_trait_data_upload <- reactiveValues(file = NULL)
observeEvent(input$wgcna_upload_trait_data, {wgcna_trait_data_upload$file = "uploaded"})

observeEvent(input$wgcna_step6_start, {
    req(wgcna_trait_data_upload$file, wgcna_blockwiseModules())
    if(wgcna_trait_data_upload$file == "uploaded"){
        file <- read.csv(input$wgcna_upload_trait_data$datapath, header = T, row.names = 1)
        data_Expr <- wgcna_blockwiseModules()$data_Expr
        if(identical(sort(colnames(file)), sort(rownames(data_Expr)))==TRUE){
            wgcna_step6$process = "start"
        }else {
            shinyjs::runjs('alert("ERROR: Sample names of traits data and matrix are not the same(column name is sample, row name is tarit).")')
            shinyjs::reset("wgcna_upload_trait_data")
            wgcna_trait_data_upload$file = NULL
        }
    }else {
        shinyjs::runjs('alert("ERROR: Please upload traits data.")')
        shinyjs::reset("wgcna_upload_trait_data")
        wgcna_trait_data_upload$file = NULL
    }
})
wgcna_step6_trait <- reactive({
    req(input$wgcna_upload_trait_data, wgcna_blockwiseModules())

    data_Expr <- wgcna_blockwiseModules()$data_Expr
    datTraits <- wgcna_blockwiseModules()$datTraits
    nGenes = ncol(data_Expr)
    nSamples = nrow(data_Expr)  

    design <- read.csv(input$wgcna_upload_trait_data$datapath, header = T, row.names = 1)
    design <- design[, match(colnames(design), rownames(data_Expr))]
    design <- t(design)

    net <- wgcna_blockwiseModules()$net
    moduleColors <- WGCNA::labels2colors(net$colors)
    # Recalculate MEs with color labels
    MEs0 = WGCNA::moduleEigengenes(data_Expr, moduleColors)$eigengenes
    MEs = WGCNA::orderMEs(MEs0); ##不同颜色的模块的ME值矩 (样本vs模块)
    moduleTraitCor = cor(MEs, design , use = "p");
    moduleTraitPvalue = WGCNA::corPvalueStudent(moduleTraitCor, nSamples)

    # sizeGrWindow(10,6)
    # Will display correlations and their p-values
    textMatrix = paste(signif(moduleTraitCor, 2), "\n(",
                    signif(moduleTraitPvalue, 1), ")", sep = "");
    dim(textMatrix) = dim(moduleTraitCor)
    png(paste0(wgcna_tmp_filepath(),"step5-Module-trait-relationships.png"),width = (input$wgcna_trait_width * 100) ,height = (input$wgcna_trait_height * 100), res = 100)
    par(mar = c(6, 9, 3, 6));#下、左、上、右的边距
    # Display the correlation values within a heatmap plot
    print("22")
    WGCNA::labeledHeatmap(Matrix = moduleTraitCor,
                xLabels = colnames(design),
                yLabels = names(MEs),
                ySymbols = names(MEs),
                colorLabels = FALSE,
                colors = WGCNA::blueWhiteRed(50),
                textMatrix = textMatrix,
                setStdMargins = FALSE,
                cex.text = 0.5,
                zlim = c(-1,1),
                main = paste("Module-trait relationships"))
    dev.off()
    print("33")
    # table( labels2colors(net$colors))
    return(paste0(wgcna_tmp_filepath(),"step5-Module-trait-relationships.png"))
})
output$wgcna_trait_cor <- renderImage({
    if(!is.null(wgcna_step6_trait())){
        list(src = wgcna_step6_trait(), contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
    }
})


# ###### geneexpression and mes cor ######
wgcna_step5_trait <- reactiveValues(process = "stop")

# # geneexpression upload stat
# wgcna_trait_upload <- reactiveValues(file = NULL, group = NULL)
# observeEvent(input$wgcna_upload_trait_matrix, {wgcna_trait_upload$file = "uploaded"})
# observeEvent(input$wgcna_upload_trait_group, {wgcna_trait_upload$group = "uploaded"})

# observeEvent(input$wgcna_step5_trait_analysis, {
#     req(wgcna_trait_upload$file, wgcna_trait_upload$group)
#     if(wgcna_trait_upload$file == "uploaded" & wgcna_trait_upload$group == "uploaded"){
#         file <- read.csv(input$wgcna_upload_trait_matrix$datapath, header = T, row.names = 1)
#         group <- read.csv(input$wgcna_upload_trait_group$datapath, header = T, row.names = 1)
#         #判断groupname样品是否在data里
#         if (identical(rownames(group), colnames(file))==FALSE) {
#             shinyjs::runjs('alert("ERROR: The sample name in group info does not correspond to the sample name in matrix.")')
#             shinyjs::reset("wgcna_upload_trait_matrix")
#             shinyjs::reset("wgcna_upload_trait_group")
#             wgcna_trait_upload$file = NULL
#             wgcna_trait_upload$group = NULL
#             return(NULL)
#         }else {
#            wgcna_step5_trait$process = "start"
#         }
#     }else {
#         shinyjs::runjs('alert("ERROR: Please upload matrix and group info.")')
#         shinyjs::reset("wgcna_upload_trait_matrix")
#         shinyjs::reset("wgcna_upload_trait_group")
#         wgcna_trait_upload$file = NULL
#         wgcna_trait_upload$group = NULL
#     }
# })

# wgcna_trait <- reactive({
#     req(input$wgcna_upload_trait_matrix, input$wgcna_upload_trait_group, wgcna_blockwiseModules())

#     trait_file <- read.csv(input$wgcna_upload_trait_matrix$datapath, header = T, row.names = 1)
#     trait_group <- read.csv(input$wgcna_upload_trait_group$datapath, header = T, row.names = 1)

#     net <- wgcna_blockwiseModules()$net
#     wgcna_gene <- names(net$colors)

#     data_Expr <- trait_file[rownames(trait_file) %in% wgcna_gene, ]
#     data_Expr <- data_Expr[match(wgcna_gene, rownames(data_Expr)), ]
#     data_Expr <- log2(t(data_Expr)+1)


#     datTraits <- trait_group[,1]


#     nGenes = ncol(data_Expr)
#     nSamples = nrow(data_Expr)
#     design1=model.matrix(~0+as.factor(datTraits))
#     design=design1
#     colnames(design) 
#     colnames(design)=c(levels(as.factor(datTraits)) ) 
#     design <- design[, match(colnames(design), unique(datTraits))]
    
#     moduleColors <- WGCNA::labels2colors(net$colors)

#     MEs0 = WGCNA::moduleEigengenes(data_Expr, moduleColors)$eigengenes
#     MEs = WGCNA::orderMEs(MEs0); ##不同颜色的模块的ME值矩 (样本vs模块)
#     moduleTraitCor = cor(MEs, design , use = "p");
#     moduleTraitPvalue = WGCNA::corPvalueStudent(moduleTraitCor, nSamples)

#     # sizeGrWindow(10,6)
#     # Will display correlations and their p-values
#     textMatrix = paste(signif(moduleTraitCor, 2), "\n(",
#                         signif(moduleTraitPvalue, 1), ")", sep = "");
#     dim(textMatrix) = dim(moduleTraitCor)

#     png(paste0(wgcna_tmp_filepath(),"step5-Module-trait-relationships.png"),width = 700,height = 1000,res = 100)
#         par(mar = c(6, 9, 3, 6));#下、左、上、右的边距
#         # Display the correlation values within a heatmap plot
#         WGCNA::labeledHeatmap(Matrix = moduleTraitCor,
#                         xLabels = colnames(design),
#                         yLabels = names(MEs),
#                         ySymbols = names(MEs),
#                         colorLabels = FALSE,
#                         colors = WGCNA::blueWhiteRed(50),
#                         textMatrix = textMatrix,
#                         setStdMargins = FALSE,
#                         cex.text = 0.8,
#                         zlim = c(-1,1),
#                         main = paste("Module-trait relationships"))
#     dev.off()

#     return(list(plop_cor = paste0(wgcna_tmp_filepath(),"step5-Module-trait-relationships.png"),
#                 MEs = MEs,
#                 data_Expr = data_Expr,
#                 moduleColors = moduleColors,
#                 design = design
#                 )
#             )
# })

# output$wgcna_trait_correlation <- renderImage({
#     if(!is.null(wgcna_trait())){
#         list(src = wgcna_trait()$plop_cor, contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
#     }
# })

# wgcna_trait_group <- reactive({
#     req(input$wgcna_upload_trait_group)
#     data <- read.csv(input$wgcna_upload_trait_group$datapath, header = T, row.names=1)
#     data <- unique(data[,1])
#     return(data)
# })

# wgcna_gs_mm <- reactive({
#     req(input$wgana_gs_mm_trait_selection, input$wgana_gs_mm_trait_selection, wgcna_blockwiseModules(), wgcna_trait())

#     design = wgcna_trait()$design
#     Acute = as.data.frame(design[, input$wgana_gs_mm_trait_selection]);

#     names(Acute) = input$wgana_gs_mm_trait_selection

#     MEs <- wgcna_trait()$MEs
#     data_Expr <- wgcna_trait()$data_Expr
#     nGenes = ncol(data_Expr)
#     nSamples = nrow(data_Expr)
#     moduleColors = wgcna_trait()$moduleColors

#     # names (colors) of the modules
#     modNames = substring(names(MEs), 3)

#     geneModuleMembership = as.data.frame(cor(data_Expr, MEs, use = "p"));
#     MMPvalue = as.data.frame(WGCNA::corPvalueStudent(as.matrix(geneModuleMembership), nSamples));

#     names(geneModuleMembership) = paste("MM", modNames, sep="")
#     names(MMPvalue) = paste("p.MM", modNames, sep="")

#     geneTraitSignificance = as.data.frame(cor(data_Expr, Acute, use = "p"))
#     GSPvalue = as.data.frame(WGCNA::corPvalueStudent(as.matrix(geneTraitSignificance), nSamples))

#     names(geneTraitSignificance) = paste("GS.", names(Acute), sep="")
#     names(GSPvalue) = paste("p.GS.", names(Acute), sep="")

#     module = input$wgana_gs_mm_module_selection
#     column = match(module, modNames);
#     moduleGenes = moduleColors==module;

#     dir.create(paste0(wgcna_tmp_filepath(),"GS_MM/"))
#     write.csv(geneModuleMembership, paste0(wgcna_tmp_filepath(), "GS_MM/",input$wgana_gs_mm_trait_selection,"_", input$wgana_gs_mm_module_selection,"_MM.csv"))
#     write.csv(geneTraitSignificance, paste0(wgcna_tmp_filepath(),"GS_MM/",input$wgana_gs_mm_trait_selection,"_", input$wgana_gs_mm_module_selection,"_GS.csv"))
#     write.csv(GSPvalue, paste0(wgcna_tmp_filepath(),"GS_MM/",input$wgana_gs_mm_trait_selection,"_", input$wgana_gs_mm_module_selection,"GS_pvalue.csv"))
#     write.csv(MMPvalue, paste0(wgcna_tmp_filepath(),"GS_MM/",input$wgana_gs_mm_trait_selection,"_", input$wgana_gs_mm_module_selection,"_MM_pvalue.csv"))

#     #sizeGrWindow(7, 7);
#     png_file <- paste0(wgcna_tmp_filepath(),"GS_MM/","Module Membership in", input$wgana_gs_mm_module_selection, "module with ",input$wgana_gs_mm_trait_selection,".png")
#     png(png_file, width = 500, height = 550, res = 100)
#         par(mfrow = c(1,1));
#         WGCNA::verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
#                         abs(geneTraitSignificance[moduleGenes, 1]),
#                         xlab = paste("Module Membership in", module, "module"),
#                         ylab = "Gene significance for body weight",
#                         main = paste("Module membership vs. gene significance\n"),
#                         cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module, abline = TRUE) #5.5高*5宽
#     dev.off()
#     return(png_file)
# })

# output$wgcna_trait_MM_plot <- renderImage({
#     if(!is.null(wgcna_gs_mm())){
#         list(src = wgcna_gs_mm(), contentType = 'image/png', style = "max-width:100%; max-height:100%;", deleteFile = FALSE)
#     }
# })

# #download result wgcna_download_result
# output$wgcna_download_result <- downloadHandler(
#   filename = function() {"WGCNA.zip"},
#   content = function(file) {
#     directory_to_compress <- wgcna_tmp_filepath()
#     files_and_folders <- list.files(directory_to_compress, full.names = TRUE, recursive = TRUE)
#     zip::zip(file, files_and_folders)
#   }
# )