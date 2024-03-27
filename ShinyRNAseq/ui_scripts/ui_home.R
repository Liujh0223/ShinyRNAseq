 home <- tabPanel(title = HTML('<i class="fas fa-house"></i>',"HOME"), 
                 value = "home",
                 fluidRow(style = "height:700px;width:initial",
                  #  column(width = 12, align="center", 
                  #           HTML("<p class='title'>温馨提示: 由于该网页目前处于开发测试阶段，仅在华农校内局域网开放仍未正式上线互联网，故只能使用华农校园网访问供大家学习使用。<br/> 
                  #           如使用过程中出现任何问题（BUG，疑问，建议，甚至希望开发其他功能等），可以直接联系开发者或邮件咨询364832885@qq.com。<br/> 
                  #           感谢您的鼓励与支持！！！</p>")),
                   
                   br(), br(), br(), br(),br(), br(),
                   HTML("<h1><center>WELCOME TO <b>RNA-seq Analysis Visualization Platform</b></center></h1>"),
                   br(), br(), br(), br(),
                   fluidRow(style = "width:80%;margin-left:10%",
                     column(width = 4, align = "center",
                            tab_voronoys(texto = "DATA CHECK", 
                                          texto2 = "Including Correlation analysis and PCA.",
                                          texto3 = "Based on Pearson correlation and ANOVA.",
                                          cor = "#337ab7", icon = icon("right-to-bracket"), id = "ui_datacheck")
                     ),
                     column(width = 4, align = "center",
                            tab_voronoys(texto = "DIFFERENCE ANALYSIS", 
                                          texto2 = "Differentiallly expressed genes analysis.",
                                          texto3 = "Based on R packages of DEseq2, edgeR, limma.",
                                          cor = "#337ab7", icon = icon("right-to-bracket"), id = "ui_differenceanalysis")
                     ),
                     column(width = 4, align = "center",
                            tab_voronoys(texto = "Time series analysis", 
                                          texto2 = "Based on Mfuzz",
                                          texto3 = "",
                                          cor = "#337ab7", icon = icon("right-to-bracket"), id = "ui_timeseriesanalysis")
                     ),
                   ),
                   fluidRow(style = "width:80%;margin-left:10%;margin-top: 2%;",
                     column(width = 4, align = "center",
                            tab_voronoys(texto = "Enrichment analysis", 
                                          texto2 = "Funtion and pathway enrichment analysis.",
                                          texto3 = "Based on KEGG and GO databases, Hypergeometric Distribution menthod.",
                                          cor = "#337ab7", icon = icon("right-to-bracket"), id = "ui_enrichmentanallysis")
                     ),
                     column(width = 4, align = "center",
                            tab_voronoys(texto = "WGCNA", 
                                          texto2 = "Weighted correlation network analysis.",
                                          texto3 = "Based on R packages of WGCNA.",
                                          cor = "#337ab7", icon = icon("right-to-bracket"), id = "ui_wgcna")
                     ),
                     column(width = 4, align = "center",
                            tab_voronoys(texto = "TOOLS", 
                                          texto2 = "A collection of utilities.",
                                          texto3 = "",
                                          cor = "#337ab7", icon = icon("right-to-bracket"), id = "ui_tools")
                     )
                   )

                 )
        )