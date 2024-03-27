# Volcano map
vol_plot <- reactive({
    if(!is.null(deg())){
        log2fccutoff1 <- input$log2fccutoff1
        pvaluecutoff1 <- input$pvaluecutoff1
        a <- getdegtable()# Gene, CGSNL_symbol, Change, DESeq2_log2FC, DESeq2_pvalue, DESeq2_padj, X1600.1_CPM, X1600.2_CPM, X1600.3_CPM, ZH11.1_CPM, ZH11.2_CPM, ZH11.3_CPM

        if(colnames(a)[2]=="Symbol"){
           colnames(a)[1:6] <- c("gene", "symbol", "change", "log2fc", "pvalue", "padj")
        }else {
           colnames(a)[1:5] <- c("gene", "change", "log2fc", "pvalue", "padj")
        }

        a$up <- NA
        a$down <- NA
        a$not <- NA

        if(!is.null(input$degresulttable_rows_selected)){
            if(length(a[input$degresulttable_rows_selected, 1]) >=6 ) {
                for(i in a[input$degresulttable_rows_selected, 1][1:6]){
                    if(a[a$gene == i, "change"] == "UP"){a[a$gene == i, "up"] <- i }
                    if(a[a$gene == i, "change"] == "DOWN"){a[a$gene == i, "down"] <- i }
                    if(a[a$gene == i, "change"] == "NOT"){a[a$gene == i, "not"] <- i }
                    }
            }else {
                for(i in a[input$degresulttable_rows_selected, 1]){
                    if(a[a$gene == i, "change"] == "UP"){a[a$gene == i, "up"] <- i }
                    if(a[a$gene == i, "change"] == "DOWN"){a[a$gene == i, "down"] <- i }
                    if(a[a$gene == i, "change"] == "NOT"){a[a$gene == i, "not"] <- i }
                    }
            }
        }

        xmin <- min(a$log2fc)
        xmax <- max(a$log2fc)
        if(abs(xmin) > abs(xmax)) {xlim = abs(xmin)} else {xlim = abs(xmax)}
        vol_title <- paste0("Analyze by ",input$differenceanalysis_software, " :  ","|log2FC| >= ", log2fccutoff1," & pvalue <= ", pvaluecutoff1,
                        "\n", nrow(a[a$change == "UP", ]), "  genes up regulation ",
                        "\n", nrow(a[a$change == "DOWN", ]),"  genes down regulation ")
        
        vol <- ggplot(data = a, aes(x = log2fc, y = -log10(pvalue), color = change)) +
            geom_point(size = 1) + scale_color_manual(values = c(input$color_vol_plot_up, input$color_vol_plot_not, input$color_vol_plot_down), limits = c('UP', 'NOT', 'DOWN')) +
            theme(panel.background = element_blank(), legend.key = element_rect(fill = 'transparent') )+ theme_bw()+
            geom_vline(xintercept = c(-log2fccutoff1, log2fccutoff1), lty = 2, color = "black") + 
            labs(title = vol_title, color = '') + xlab(expression("log"["2"]*" (Fold Change)")) + ylab(expression("-log"["10"]*"(P Value)")) +
            geom_hline(yintercept = -log10(pvaluecutoff1), lty = 2, color = "black") + xlim(-xlim,xlim) +
            theme(axis.line.y.left = element_line(color = "black"),axis.line.x.bottom = element_line(color = "black")) + 
            theme(plot.title = element_text(size = 12, family = "sans")) +
            theme(axis.text = element_text(size = 12, face = 'bold'), axis.title = element_text(size = 15, face = 'bold'))  

        vol_label <- vol + ggrepel::geom_text_repel(aes(label = up),size = 4, fontface = "italic", segment.size = 0.5, direction = "x", hjust = 0, color = input$color_vol_plot_up)+
                     ggrepel::geom_text_repel(aes(label = down),size = 4, fontface = "italic", segment.size = 0.5, direction = "x", hjust = 0, color = input$color_vol_plot_down)+
                     ggrepel::geom_text_repel(aes(label = not),size = 4, fontface = "italic", segment.size = 0.5, direction = "x", hjust = 0, color = input$color_vol_plot_not)       

        ifelse(input$label_vol_plot, vol <- vol_label, vol)
            

    }else {
       return(NULL)
    }
    vol
})
output$download_vol_plot <- downloadHandler(#下载
    filename = function() { paste(input$selectgroupname, "Volcano", input$vol_plot_format, sep = ".")},
    content = function(file) {
    p <- vol_plot()
    ggsave(file, p, width = input$vol_plot_width, height = input$vol_plot_height)
})

# deg bar plot
deg_bar_1 <- reactive({
    gene <- input$degresulttable_rows_selected[1]
    a <- read.csv(paste0(deg(), input$selectgroupname))
    gene_id <- a[gene,1]
    group <- a[nrow(a),][,6:ncol(a)]
    rownames(group) <- "group"
    a <- a[gene,][,6:ncol(a)]
    rownames(a) <- gene_id
    a <- rbind(group,a)

    TR_name <- unique(unlist(a["group", grep("^TR:", a["group",])]))
    TR <- as.numeric(unlist(a[gene_id, grep("^TR:", a["group",])]))
    TR_mean <- mean(TR)
    TR_sd <- sd(TR)

    CO_name <- unique(unlist(a["group", grep("^CO:", a["group",])]))
    CO <- as.numeric(unlist(a[gene_id, grep("^CO:", a["group",])]))
    CO_mean <- mean(CO)
    CO_sd <- sd(CO)

    data <- data.frame(name = c(TR_name, CO_name), mean = c(TR_mean, CO_mean), sd = c(TR_sd, CO_sd))
    ymax <- max(data$mean) + max(data$sd)
    ymax <- ifelse(is.na(ymax), max(data$mean)+(max(data$mean)/3), ymax+(ymax/3))
    
    dd <- ggplot(data, aes(x=name, y=mean))+
        geom_bar(position = position_dodge(), stat = "identity",colour = 'black',fill=input$color_deg_bar)+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = position_dodge(0.9))+
        ylab('CPM') + xlab('') + ggtitle(paste(gene_id))+
        theme(plot.title = element_text(size=15), axis.title.y = element_text(size = 15),
            axis.text.x=element_text(vjust=1,size=13, angle = 45, hjust = 1),axis.text.y=element_text(vjust=0.3,size=10, color = 'black'))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(color="black"),legend.position="none")+
        scale_y_continuous(expand = c(0,0), limits = c(0,ymax))

    dd
})
output$deg_bar_1 <- renderPlot(deg_bar_1())

deg_bar_2 <- reactive({
    gene <- input$degresulttable_rows_selected[2]
    a <- read.csv(paste0(deg(), input$selectgroupname))
    gene_id <- a[gene,1]
    group <- a[nrow(a),][,6:ncol(a)]
    rownames(group) <- "group"
    a <- a[gene,][,6:ncol(a)]
    rownames(a) <- gene_id
    a <- rbind(group,a)

    TR_name <- unique(unlist(a["group", grep("^TR:", a["group",])]))
    TR <- as.numeric(unlist(a[gene_id, grep("^TR:", a["group",])]))
    TR_mean <- mean(TR)
    TR_sd <- sd(TR)

    CO_name <- unique(unlist(a["group", grep("^CO:", a["group",])]))
    CO <- as.numeric(unlist(a[gene_id, grep("^CO:", a["group",])]))
    CO_mean <- mean(CO)
    CO_sd <- sd(CO)

    data <- data.frame(name = c(TR_name, CO_name), mean = c(TR_mean, CO_mean), sd = c(TR_sd, CO_sd))
    ymax <- max(data$mean) + max(data$sd)
    ymax <- ifelse(is.na(ymax), max(data$mean)+(max(data$mean)/3), ymax+(ymax/3))
    dd <- ggplot(data, aes(x=name, y=mean))+
        geom_bar(position = position_dodge(), stat = "identity",colour = 'black',fill=input$color_deg_bar)+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = position_dodge(0.9))+
        ylab('CPM') + xlab('') + ggtitle(paste(gene_id))+
        theme(plot.title = element_text(size=15), axis.title.y = element_text(size = 15),
            axis.text.x=element_text(vjust=1,size=13, angle = 45, hjust = 1),axis.text.y=element_text(vjust=0.3,size=10, color = 'black'))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(color="black"),legend.position="none")+
        scale_y_continuous(expand = c(0,0), limits = c(0,ymax))

    dd
})
output$deg_bar_2 <- renderPlot(deg_bar_2())

deg_bar_3 <- reactive({
    gene <- input$degresulttable_rows_selected[3]
    a <- read.csv(paste0(deg(), input$selectgroupname))
    gene_id <- a[gene,1]
    group <- a[nrow(a),][,6:ncol(a)]
    rownames(group) <- "group"
    a <- a[gene,][,6:ncol(a)]
    rownames(a) <- gene_id
    a <- rbind(group,a)

    TR_name <- unique(unlist(a["group", grep("^TR:", a["group",])]))
    TR <- as.numeric(unlist(a[gene_id, grep("^TR:", a["group",])]))
    TR_mean <- mean(TR)
    TR_sd <- sd(TR)

    CO_name <- unique(unlist(a["group", grep("^CO:", a["group",])]))
    CO <- as.numeric(unlist(a[gene_id, grep("^CO:", a["group",])]))
    CO_mean <- mean(CO)
    CO_sd <- sd(CO)

    data <- data.frame(name = c(TR_name, CO_name), mean = c(TR_mean, CO_mean), sd = c(TR_sd, CO_sd))
    ymax <- max(data$mean) + max(data$sd)
    ymax <- ifelse(is.na(ymax), max(data$mean)+(max(data$mean)/3), ymax+(ymax/3))
    dd <- ggplot(data, aes(x=name, y=mean))+
        geom_bar(position = position_dodge(), stat = "identity",colour = 'black',fill=input$color_deg_bar)+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = position_dodge(0.9))+
        ylab('CPM') + xlab('') + ggtitle(paste(gene_id))+
        theme(plot.title = element_text(size=15), axis.title.y = element_text(size = 15),
            axis.text.x=element_text(vjust=1,size=13, angle = 45, hjust = 1),axis.text.y=element_text(vjust=0.3,size=10, color = 'black'))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(color="black"),legend.position="none")+
        scale_y_continuous(expand = c(0,0), limits = c(0,ymax))

    dd
})
output$deg_bar_3 <- renderPlot(deg_bar_3())

deg_bar_4 <- reactive({
    gene <- input$degresulttable_rows_selected[4]
    a <- read.csv(paste0(deg(), input$selectgroupname))
    gene_id <- a[gene,1]
    group <- a[nrow(a),][,6:ncol(a)]
    rownames(group) <- "group"
    a <- a[gene,][,6:ncol(a)]
    rownames(a) <- gene_id
    a <- rbind(group,a)

    TR_name <- unique(unlist(a["group", grep("^TR:", a["group",])]))
    TR <- as.numeric(unlist(a[gene_id, grep("^TR:", a["group",])]))
    TR_mean <- mean(TR)
    TR_sd <- sd(TR)

    CO_name <- unique(unlist(a["group", grep("^CO:", a["group",])]))
    CO <- as.numeric(unlist(a[gene_id, grep("^CO:", a["group",])]))
    CO_mean <- mean(CO)
    CO_sd <- sd(CO)

    data <- data.frame(name = c(TR_name, CO_name), mean = c(TR_mean, CO_mean), sd = c(TR_sd, CO_sd))
    ymax <- max(data$mean) + max(data$sd)
    ymax <- ifelse(is.na(ymax), max(data$mean)+(max(data$mean)/3), ymax+(ymax/3))
    dd <- ggplot(data, aes(x=name, y=mean))+
        geom_bar(position = position_dodge(), stat = "identity",colour = 'black',fill=input$color_deg_bar)+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = position_dodge(0.9))+
        ylab('CPM') + xlab('') + ggtitle(paste(gene_id))+
        theme(plot.title = element_text(size=15), axis.title.y = element_text(size = 15),
            axis.text.x=element_text(vjust=1,size=13, angle = 45, hjust = 1),axis.text.y=element_text(vjust=0.3,size=10, color = 'black'))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(color="black"),legend.position="none")+
        scale_y_continuous(expand = c(0,0), limits = c(0,ymax))

    dd
})
output$deg_bar_4 <- renderPlot(deg_bar_4())

deg_bar_5 <- reactive({
    gene <- input$degresulttable_rows_selected[5]
    a <- read.csv(paste0(deg(), input$selectgroupname))
    gene_id <- a[gene,1]
    group <- a[nrow(a),][,6:ncol(a)]
    rownames(group) <- "group"
    a <- a[gene,][,6:ncol(a)]
    rownames(a) <- gene_id
    a <- rbind(group,a)

    TR_name <- unique(unlist(a["group", grep("^TR:", a["group",])]))
    TR <- as.numeric(unlist(a[gene_id, grep("^TR:", a["group",])]))
    TR_mean <- mean(TR)
    TR_sd <- sd(TR)

    CO_name <- unique(unlist(a["group", grep("^CO:", a["group",])]))
    CO <- as.numeric(unlist(a[gene_id, grep("^CO:", a["group",])]))
    CO_mean <- mean(CO)
    CO_sd <- sd(CO)

    data <- data.frame(name = c(TR_name, CO_name), mean = c(TR_mean, CO_mean), sd = c(TR_sd, CO_sd))
    ymax <- max(data$mean) + max(data$sd)
    ymax <- ifelse(is.na(ymax), max(data$mean)+(max(data$mean)/3), ymax+(ymax/3))
    dd <- ggplot(data, aes(x=name, y=mean))+
        geom_bar(position = position_dodge(), stat = "identity",colour = 'black',fill=input$color_deg_bar)+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = position_dodge(0.9))+
        ylab('CPM') + xlab('') + ggtitle(paste(gene_id))+
        theme(plot.title = element_text(size=15), axis.title.y = element_text(size = 15),
            axis.text.x=element_text(vjust=1,size=13, angle = 45, hjust = 1),axis.text.y=element_text(vjust=0.3,size=10, color = 'black'))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(color="black"),legend.position="none")+
        scale_y_continuous(expand = c(0,0), limits = c(0,ymax))

    dd
})
output$deg_bar_5 <- renderPlot(deg_bar_5())

deg_bar_6 <- reactive({
    gene <- input$degresulttable_rows_selected[6]
    a <- read.csv(paste0(deg(), input$selectgroupname))
    gene_id <- a[gene,1]
    group <- a[nrow(a),][,6:ncol(a)]
    rownames(group) <- "group"
    a <- a[gene,][,6:ncol(a)]
    rownames(a) <- gene_id
    a <- rbind(group,a)

    TR_name <- unique(unlist(a["group", grep("^TR:", a["group",])]))
    TR <- as.numeric(unlist(a[gene_id, grep("^TR:", a["group",])]))
    TR_mean <- mean(TR)
    TR_sd <- sd(TR)

    CO_name <- unique(unlist(a["group", grep("^CO:", a["group",])]))
    CO <- as.numeric(unlist(a[gene_id, grep("^CO:", a["group",])]))
    CO_mean <- mean(CO)
    CO_sd <- sd(CO)

    data <- data.frame(name = c(TR_name, CO_name), mean = c(TR_mean, CO_mean), sd = c(TR_sd, CO_sd))
    ymax <- max(data$mean) + max(data$sd)
    ymax <- ifelse(is.na(ymax), max(data$mean)+(max(data$mean)/3), ymax+(ymax/3))
    dd <- ggplot(data, aes(x=name, y=mean))+
        geom_bar(position = position_dodge(), stat = "identity",colour = 'black',fill=input$color_deg_bar)+
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2, position = position_dodge(0.9))+
        ylab('CPM') + xlab('') + ggtitle(paste(gene_id))+
        theme(plot.title = element_text(size=15), axis.title.y = element_text(size = 15),
            axis.text.x=element_text(vjust=1,size=13, angle = 45, hjust = 1),axis.text.y=element_text(vjust=0.3,size=10, color = 'black'))+
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(color="black"),legend.position="none")+
        scale_y_continuous(expand = c(0,0), limits = c(0,ymax))

    dd
})
output$deg_bar_6 <- renderPlot(deg_bar_6())

output$download_deg_bar <- downloadHandler(#下载
    filename = function() { paste("DEGs_bar_plot", input$deg_bar_format, sep = ".")},
    content = function(file) {

    if(length(input$degresulttable_rows_selected)>=1){
        width = input$deg_bar_width
        height = input$deg_bar_height
        p <- gridExtra::grid.arrange(deg_bar_1(), nrow=1)
        ggsave(file, p, width = width, height = height)
    }
    if(length(input$degresulttable_rows_selected)>=2){
        width = 2 * input$deg_bar_width
        height = input$deg_bar_height
        p <- gridExtra::grid.arrange(deg_bar_1(), deg_bar_2(), nrow=1)
        ggsave(file, p, width = width, height = height)
    }
    if(length(input$degresulttable_rows_selected)>=3){
        width = 3 * input$deg_bar_width
        height = input$deg_bar_height
        p <- gridExtra::grid.arrange(deg_bar_1(), deg_bar_2(), deg_bar_3(), nrow=1)
        ggsave(file, p, width = width, height = height)
    }
    if(length(input$degresulttable_rows_selected)>=4){
        width = 4 * input$deg_bar_width
        height = input$deg_bar_height
        p <- gridExtra::grid.arrange(deg_bar_1(), deg_bar_2(), deg_bar_3(), deg_bar_4(), nrow=1)
        ggsave(file, p, width = width, height = height)
    }
    if(length(input$degresulttable_rows_selected)>=5){
        width = 5 * input$deg_bar_width
        height = input$deg_bar_height
        p <- gridExtra::grid.arrange(deg_bar_1(), deg_bar_2(), deg_bar_3(), deg_bar_4(), deg_bar_5(), nrow=1)
        ggsave(file, p, width = width, height = height)
    }
    if(length(input$degresulttable_rows_selected)>=6){
        width = 6 * input$deg_bar_width
        height = input$deg_bar_height
        p <- gridExtra::grid.arrange(deg_bar_1(), deg_bar_2(), deg_bar_3(), deg_bar_4(), deg_bar_5(), deg_bar_6(),nrow=1)
        ggsave(file, p, width = width, height = height)
    }
})