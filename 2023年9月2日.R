source("impute-function.R")
library(shinydashboard)
library(readxl)
library(openxlsx)
library(DT)
library(shiny)
library(tableone)
library(VIM)
library(DMwR2)
library(missForest)
library(rsconnect)
library(shinycustomloader)
library(shinyalert)
library(esquisse)
library(showtext)
library(rvg)
library(mice)
library(rhandsontable)
library(rms)
library(colourpicker)
library(ggplot2)
library(ggrcs)
library(plotly)
library(CBCgrps)
set_i18n("cn")
showtext_auto()
font_add("simsun", "SIMSUN.ttf")
ui=dashboardPage(skin="purple",
                 dashboardHeader(title=tags$b("易统计 1.0版")),
                 dashboardSidebar(
                   sidebarMenu(
                     menuItem("上传数据",tabName="上传数据",icon = icon("home")),
                     menuItem("缺失值处理", tabName = "缺失值分析", icon = icon("ambulance"),badgeLabel="hot"),
                     menuItem("一键基线分析",tabName = "一键基线分析",icon=icon("book"),badgeLabel="hot"),
                     menuItem("GGp1ot2基础绘图",tabName = "ggplot绘图",icon=icon("dashboard")),
                     menuItem("限制性立方样条",tabName = "限制性立方样条",icon=icon("chart-line",lib="font-awesome")),
                     tags$div(style = "margin-top:230px;"),
                     menuItem(HTML("<b style='font-size:16px;'>平台说明</b>"),tabName = "平台说明",icon=icon("book")),
                     menuItem(HTML("<b style='font-size:16px;'>平台作者:&nbsp;&nbsp;21级张新宇</b>"),icon=icon("user")),
                     menuItem(HTML("<b style='font-size:16px;'>WEI XIN:&nbsp;&nbsp;1251717762</b>"),icon=icon("weixin")),
                     HTML("<br>"),HTML("<br>"),HTML("<br>"),HTML("<br>"),tags$div(style = "margin-top:10px;"),
                     menuItem(HTML("<b style='font-size:17px;'>本平台基于R&nbsp;4.2.0开发</b>"))
                   )
                 ),
                 dashboardBody(
                   tabItems(
                     tabItem(tabName="上传数据",fluidPage(
                       fluidRow(
                         column(8,box(title = "点击上传数据",solidHeader = TRUE,collapsible = T,status="success",
                                      fileInput("file1","选择后缀名为.xlsx文件",accept=c(".xlsx"))
                         ))
                         ,tags$br(),tags$br(),
                         column(12,dataTableOutput("Data"))
                       ))
                       
                     ),
                     tabItem(tabName="一键基线分析",fluidRow(column(12,box(title=HTML("<b style='font-size:18px;'>设置变量</b>"),width=10,status = "success", solidHeader = TRUE,collapsible = T,
                                                                     selectInput("column_select1","定义分类变量",choices=NULL,multiple = TRUE),
                                                                     selectInput("column_select2","定义分组变量",choices=NULL,multiple = T),
                                                                     actionButton("jixian_button","开始分析",icon=icon('play-circle'))
                     ))),br(),fluidRow(column(12,offset=10,downloadButton("downloadData", "下载基线结果",icon=icon("download")))),
                     fluidRow(column(12,box(title=HTML("<b style='font-size:18px;'>单因素分析结果</b>"),status = "success",solidHeader = TRUE,collapsible = T,HTML("<br>"),
                                            dataTableOutput("jixian_an"),width = 12)))
                     
                     ),tabItem(tabName="缺失值分析",tabsetPanel(type="pills",
                                                           tabPanel("缺失值统计",HTML("<br>"),
                                                                    HTML("<br>"),fluidPage(fluidRow(plotOutput("plot_queshi", width = "1300px",height = "500px")),HTML("<br>"),HTML("<br>"),
                                                                                           fluidRow(column(3,box(title=HTML("<b style='font-size:18px;'>缺失值统计</b>"),withLoader(dataTableOutput("queshi_count"),type="html",loader="dnaspin"),status="success",solidHeader=TRUE,width=12)),
                                                                                                    column(9,box(title=HTML("<b style='font-size:18px;'>缺失值检出(支持手动填补,适合缺失值较少的数据)</b>"),withLoader(dataTableOutput("queshi_data"),type="html",loader="dnaspin"),status="success",solidHeader=TRUE,width=12,
                                                                                                                 HTML("<br>"),downloadButton("querenchaibu",HTML("下载数据<br>(包含未缺失在内的所有数据)"),icon=icon("hourglass-start"))))))),
                                                           tabPanel("缺失值填补",HTML("<br>"),HTML("<br>"),fluidPage(                                                                  
                                                             fluidRow(
                                                               column(4,box(title=HTML("<span style='font-size: 18px;'>单变量简单填补</span><br><span style='font-size:9px;color:black'>(注:数据中所有变量按一种方法进行填补)</span>"),
                                                                            checkboxGroupInput("easy",label=NULL, choices=list("删除法","均数填补","中位数填补","众数填补")),width=10,solidHeader=TRUE,status="success",collapsed=T
                                                               ),box(title=HTML("<span style='font-size: 18px;'>多变量简单填补</span><br><span style='font-size:9px;color:black'>(注:不同的变量按不同的方法进行填补)</span>"),
                                                                     selectInput("column_select3","请选择均数填补变量",choices=NULL,multiple = TRUE),
                                                                     selectInput("column_select4","请选择中位数填补变量",choices=NULL,multiple = TRUE),
                                                                     selectInput("column_select5","请选择众数填补变量",choices=NULL,multiple = TRUE),width=10,solidHeader=TRUE,status="success",collapsed=T),
                                                               box(title=HTML("<b style='font-size:18px;'>机器学习填补法</b>"),checkboxGroupInput("machine",label=NULL, choices=list("随机森林填补(NTREE=100)","K-近邻算法填补")),width=10,solidHeader=TRUE,status="success",collapsed=T),
                                                               box(
                                                                 actionButton("tianbu_button","开始填补",icon=icon("hourglass-start")),tags$div(style = "margin-top:10px;"),
                                                                 downloadButton("download_compute", "下载插补数据",icon=icon("download")),width=10
                                                               )),
                                                               column(8,box(title=HTML("<b style='font-size:18px;'>插补结果</b>"),withLoader(dataTableOutput("data_compute"),type="html", loader="dnaspin"),width=12,solidHeader=TRUE,status="success")
                                                               )
                                                             ))),
                                                           tabPanel("多重插补",fluidPage(fluidRow(br(),br(),column(3,box(title=HTML("<b style='font-size:18px;'>设置参数</b>"),solidHeader=TRUE,status="success",width=12,
                                                                                                                     numericInput("number_compute1","请输入要插补数据集的个数",value=5, min = 1, max =100),
                                                                                                                     numericInput("columnselect7","请选择输出填补数据的序号",value=5, min = 1, max =100),
                                                                                                                     checkboxGroupInput("queren_premat",label="是否更改预测矩阵",choices=list("否","是"),selected="否")),
                                                                                                               box(actionButton("tianbu_button1","开始填补",icon=icon("hourglass-start")),tags$div(style = "margin-top:10px;"),
                                                                                                                   downloadButton("download_compute1", "下载填补数据",icon=icon("download")),tags$div(style = "margin-top:10px;"),
                                                                                                                   downloadButton("download_compute2", "下载全部数据",icon=icon("download")),
                                                                                                                   width=12)),
                                                                                              column(9,box(title="构建预测矩阵及插补方法(非必须参数:默认插补法为PMM,默认预测矩阵为对角矩阵,可在此更改)",rHandsontableOutput("table",width = "180%",height = "300%"),status="warning",width=12,solidHeader=TRUE,color="yellow"))))
                                                           ),
                                                           tabPanel("Rubin统计值合并")
                                                           
                     )),
                     tabItem(tabName="ggplot绘图",fluidRow(
                       column(4,box(title=HTML("<b style='font-size:18px;'>设置变量</b>"),solidHeader=TRUE,status="success",width=12,
                                    fileInput("file2","上传后缀名为.xlsx文件",accept=c(".xlsx")),
                                    selectInput("column_select6","定义分类变量",choices=NULL,multiple = TRUE),
                                    actionButton("querenggplot","确认",icon=icon("hourglass-start")))),
                       column(8,box(title=HTML("<b style='font-size:18px;'>GGplot2绘图区(注:框内为示例数据,请上传你的数据)</b>"),esquisse_ui(id = "esquisse",header = FALSE),solidHeader=TRUE,status="success",width=12))
                     )),
                     tabItem(tabName="限制性立方样条",fluidPage(
                       titlePanel(strong("目前只支持logistic回归和线性回归,图形数据可通过下载链接进行下载(寻找截断值)")),
                       br(),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("model", label = "分析模型",choices=c("线性回归","logistic回归"),selected="logistic回归"),
                           selectInput("factor","定义因子变量",choices=NULL,multiple = T),
                           selectInput("y","请选择因变量",choices=NULL,multiple = F),
                           selectInput("x","请选择自变量",choices=NULL,multiple =F),
                           selectInput("cov","请选择协变量",choices=NULL,multiple = TRUE),
                           sliderInput("alpa",label="请选置信区间择透明度",min=0,max=1,value=0.3,step=0.1),
                           textInput("xlab", "x轴标签", value = ""),
                           textInput("ylab", "y轴标签", value = ""),
                           textInput("title", "标题", value = ""),
                           colourInput("col_ribbon", "选择置信区间颜色","red"),
                           selectInput("p_appear","是否显示P值",choices=c("是","否"),multiple = F),
                           numericInput("width", "图像宽度：", value=800, min=200, max=1000, step =100),
                           numericInput("height", "图像高度：", value=500, min=200, max=1000, step =100),
                           width=3,style = "background-color:plum;"
                         ),
                         mainPanel(
                           box(title="限制性立方样条图",plotlyOutput("distPlot"),status = "warning",width=12,solidHeader=TRUE,height=600),
                           box(actionButton("rcs_button","开始分析",icon=icon('play-circle')),tags$div(style = "margin-top:10px;"),
                               downloadButton("rcs_data", "下载结果",icon=icon("download")),width=3),width=9
                         )
                       )
                       
                       
                     )),tabItem(tabName="平台说明",tags$img(src="https://s3.bhimgs.com/i/2023/09/02/ipg8ga.jpg
", width = "1000px", height = "600px"))
                   )))

server=function(input,output,session){
  options(shiny.maxRequestSize=30*1024^2)
  dataset=reactive({
    inFile=input$file1
    if(is.null(inFile))
      return(NULL)
    data= read.xlsx(inFile$datapath)
    if(any(sapply(data, is.character))){
      shinyalert("错误", "上传的数据中包含字符变量,请重新上传！！", type = "error")
      return(NULL)
    } 
    return(data)
  })
  
  dataset2=reactive({
    inFile2=input$file2
    if(!is.null(inFile2)){
      data2=read.xlsx(inFile2$datapath)
      return(data2)
    }
  })
  
  
  
  output$Data=renderDataTable({
    datatable(dataset(),options = list(pageLength =20,scrollX = TRUE)) 
  })
  
  data_all=reactive(dataset())
  data_r=reactiveValues(data=iris,name = "iris")
  
  observe({
    columns1=colnames(dataset())
    columns2=colnames(dataset2())
    updateSelectInput(session,"column_select1", choices = columns1)
    updateSelectInput(session,"column_select2", choices = c(columns1,"不分组"))
    updateSelectInput(session,"column_select3", choices = columns1)
    updateSelectInput(session,"column_select4", choices = columns1)
    updateSelectInput(session,"column_select5", choices = columns1)
    updateSelectInput(session,"column_select6", choices = c(columns2))
    updateSelectInput(session,"x", choices = columns1)
    updateSelectInput(session,"y", choices = columns1)
    updateSelectInput(session,"cov", choices = columns1)
    updateSelectInput(session,"factor", choices = columns1)
  })#更新下拉菜单框
  
  
  
  
  
  
  output$plot_queshi=renderPlot(aggr(dataset(), numbers = TRUE, prop = FALSE,cex.axis=0.7))
  output$queshi_count=renderDataTable({
    datatable(aggr(dataset(),plot=F)$missings,rownames=F)
  })
  
  output$queshi_data=renderDataTable({
    datatable(dataset()[find_missing(dataset()),],editable=T,selection = 'none', extensions = 'Buttons',options=list(dom = 'Bfrtip',buttons = c('csv', 'excel'),pageLength =20,ordering = F,scrollX = TRUE))
  })
  
  
  
  observeEvent(input$queshi_data_cell_edit, {
    queshi_data_linshi=dataset()
    queshi_data_linshi[input$queshi_data_cell_edit$row,input$queshi_data_cell_edit$col]=input$queshi_data_cell_edit$value
    output$querenchaibu=downloadHandler(
      filename=function(){
        "手动插补结果.xlsx"
      },
      content=function(file){
        queshi_total=rbind(dataset()[-find_missing(dataset()),],queshi_data_linshi)
        write.xlsx(queshi_total,file,rowNames=F)
      })})
  
  observeEvent(input$jixian_button,{
    describle=to_factor_data(input$column_select1,dataset())
    if(length(input$column_select2)!=1){
      shinyalert("错误", "分类变量只有一种或选择不分组！", type = "error")
      jixian_anl1=data.frame()
    } else if(input$column_select2=="不分组"){
      jixian_anl1=CreateTableOne(factorVars=input$column_select1,addOverall =T,data = as.data.frame(dataset()))
      
      } else if(input$column_select2!="不分组"&length(unique(describle[,which(names(describle)%in%input$column_select2)]))==2){
      jixian_anl1=twogrps(describle, input$column_select2,ShowStatistic =T)
      jixian_anl1=as.data.frame(jixian_anl1[1])
    } else if(input$column_select2!="不分组"&length(unique(describle[,which(names(describle)%in%input$column_select2)]))>2){
      jixian_anl1=multigrps(describle, input$column_select2,ShowStatistic =T)
      jixian_anl1=as.data.frame(jixian_anl1[1])
    } else{
      shinyalert("错误", "分组变量只有一个类,请检查！", type = "error")
    }
    output$jixian_an=renderDataTable({
      datatable(as.data.frame(print(jixian_anl1)),options = list(pageLength =20))
    })
    jixian_csv=as.data.frame(print(jixian_anl1))
      output$downloadData=downloadHandler(
        filename=function(){
          "单因素分析结果.csv"
        },
        content=function(file){
          write.csv(jixian_csv,file,row.names=F)
        })
  })
  
  
  
  
  observeEvent(input$tianbu_button,{
    impute_easy=input$easy
    impute_machine=input$machine
    withProgress(message = "正在插补,请耐心等待",if(length(impute_easy)==1 & length(impute_machine)==0&length(input$column_select3)==0&length(input$column_select4)==0&length(input$column_select5)==0){
      data_compute=switch(impute_easy,
                          "删除法"=delete(dataset()),
                          "均数填补"=junshu(dataset()),
                          "中位数填补"=zhongweishu(dataset()),
                          "众数填补"=getmode(dataset())
      )} else if(length(impute_machine)==1 & length(impute_easy)==0&length(input$column_select3)==0&length(input$column_select4)==0&length(input$column_select5)==0){
        data_compute=switch(impute_machine,
                            "随机森林填补(NTREE=100)"=suijisenlin(dataset()),
                            "k-近邻算法填补"=knn(dataset())
        )} else if(length(impute_machine)==0 & length(impute_easy)==0&(length(input$column_select3)!=0|length(input$column_select4)!=0|length(input$column_select5)!=0)&
                   length(unique(c(input$column_select3,input$column_select4,input$column_select5)))==length(c(input$column_select3,input$column_select4,input$column_select5))){
          data_compute=m_impute(dataset(),id_junshu=input$column_select3,id_zhongweishu=input$column_select4,id_zhongshu=input$column_select5)
        } else if(length(unique(c(input$column_select3,input$column_select4,input$column_select5)))!=length(c(input$column_select3,input$column_select4,input$column_select5))){
          showModal(modalDialog(
            title = "错误",
            "请误重复选择变量,请检查！！",
            easyClose = TRUE
          ))} else
          {
            showModal(modalDialog(
              title = "错误",
              "请勿重复选择插补方法,请重新选择！！",
              easyClose = TRUE
            ))
          })
    
    output$data_compute=renderDataTable({
      datatable(data_compute,options = list(pageLength =25,scrollX = TRUE))
    })
    output$download_compute=downloadHandler(
      filename=function(){
        "插补后数据.xlsx"
      },
      content=function(file){
        write.xlsx(data_compute,file)
      })
  })
  
  
  observe({
    output$table=renderRHandsontable({rhandsontable(pre_data(dataset()),width=840,height=550,rowHeaderWidth=150)%>% 
        hot_col(col ="插入方法", type = "dropdown", source = c("pmm", "logreg", "polyreg", "polr"),colWidths = 100)%>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)})
  }) 
  
  observe({
    observeEvent(input$tianbu_button1,{
      pre_matrw=hot_to_r(input$table)
      if(input$columnselect7==0|input$number_compute1==0){
        showModal(modalDialog(
          title = "错误",
          "参数不能为0,请检查!!",
          easyClose = TRUE
        ))
      } else if(length(input$queren_premat)==2){
        showModal(modalDialog(
          title = "错误",
          "请确认是否使用预测矩阵!!",
          easyClose = TRUE
        ))
      } else if(input$queren_premat=="否"){
        newdata=dataset()
        withProgress(message = "正在插补,请耐心等待",(mice1=mice(newdata,input$number_compute1)))} else{
          newdata=dataset()
          withProgress(message = "正在插补,请耐心等待",(mice1=mice(newdata,method=pre_matrw[,1],predictorMatrix=as.matrix(ifelse(pre_matrw[,-1]==TRUE,1,0)))))
        }
      if(!exists("mice1")){
        output$mimpute_data=renderDataTable({
          datatable(colnames(dataset()),options = list(pageLength=25,scrollX = TRUE))
        })
      } else{
        if(input$columnselect7<=input$number_compute1){
          mimpute_data=complete(mice1,action=input$columnselect7)
        } else{
          showModal(modalDialog(
            title = "错误",
            "选择填补的数据集数应小于等于插补的个数,请检查！！",
            easyClose = TRUE
          ))
        }
        output$mimpute_data=renderDataTable({
          datatable(mimpute_data,options = list(pageLength=25,scrollX = TRUE))
        })
        mimpute_data1=complete(mice1,action="long")
        output$download_compute1=downloadHandler(
          filename=function(){
            paste("第",input$columnselect7,"套多重插补数据.xlsx")
          },
          content=function(file){
            write.xlsx(mimpute_data,file)
          })
        output$download_compute2=downloadHandler(
          filename=function(){
            "全部多重插补数据.xlsx"
          },
          content=function(file){
            write.xlsx(mimpute_data1,file)
          })
      }})})
  
  
  observeEvent(input$querenggplot,{
    data_r$data=dataset2()
    data_r$data[,which(colnames(data_r$data) %in% input$column_select6)]=sapply(data_r$data[,which(colnames(data_r$data) %in% input$column_select6)],function(x){x=as.character(x)})
  }) #ggplot2绘图区定义因子变量
  
  results=esquisse_server(
    id = "esquisse",
    data_rv=data_r,
    import_from="env"
  )
  
  
  observeEvent(input$rcs_button,{
    withProgress(message = "正在计算,请稍等!",{
      factor_data=to_factor_data(input$factor,dataset())
      if(length(unique(factor_data[,which(colnames(factor_data)%in%input$x)]))<=5|length(input$x)==0|length(input$y)==0|is.character(factor_data[,which(colnames(factor_data)%in%input$x)|unique(c(input$x,input$y,input$cov))!=c(input$x,input$y,input$cov)])){
        showModal(modalDialog(
          title = "错误",
          "X可能不是连续性变量、不存在或者Y不存在或者X、Y、协变量有重复,请检查！！",
          easyClose = TRUE
        ))
      } else{
        if(input$model=="线性回归"&length(unique(factor_data[,which(colnames(factor_data)%in%input$y)]))>=6&is.numeric(factor_data[,which(colnames(factor_data)%in%input$y)])){
          model_reg=regression(response=input$y,x=input$x,predictors=input$cov,data=factor_data)
          if(input$p_appear=="是"){
            position=singlercs(data=factor_data,fit=model_reg,x=input$x)
            position=max(position$data$yhat,na.rm=T)
            output$distPlot=renderPlotly({
              ggplotly(singlercs(data=factor_data,fit=model_reg,x=input$x,ribcol=input$col_ribbon,ribalpha=input$alpa,
                                 xlab =input$xlab,ylab=input$ylab,title=input$title,P.Nonlinear=T,xP.Nonlinear=max(factor_data[,input$x],na.rm=T)/1.5,yP.Nonlinear=max(factor_data[,input$y],na.rm=T)),
                       width = input$width, height = input$height)
            })} else{
              output$distPlot= renderPlotly({
                ggplotly(singlercs(data=factor_data,fit=model_reg,x=input$x,ribcol=input$col_ribbon,ribalpha=input$alpa,
                                   xlab =input$xlab,ylab=input$ylab,title=input$title),
                         width = input$width, height = input$height)
              })}
        } else if(input$model=="线性回归"&(length(unique(factor_data[,which(colnames(factor_data)%in%input$y)]))<6|is.character(factor_data[,which(colnames(factor_data)%in%input$y)]))){
          showModal(modalDialog(
            title = "错误",
            "Y可能是分类变量请检查,请检查！！",
            easyClose = TRUE
          ))
        } else if(input$model=="logistic回归"&length(unique(factor_data[,which(colnames(factor_data)%in%input$y)]))!=2){
          showModal(modalDialog(
            title = "错误",
            "Y可能不是分类变量,请检查！！",
            easyClose = TRUE
          ))} else{
            model_reg=logistic_regression(response=input$y,x=input$x,predictors=input$cov,data=factor_data)
            if(input$p_appear=="是"){
              position=singlercs(data=factor_data,fit=model_reg,x=input$x)
              position=max(position$data$yhat,na.rm=T)
              output$distPlot=renderPlotly({
                ggplotly(singlercs(data=factor_data,fit=model_reg,x=input$x,ribcol=input$col_ribbon,ribalpha=input$alpa,
                                   xlab =input$xlab,ylab=input$ylab,title=input$title,P.Nonlinear=T,xP.Nonlinear=max(factor_data[,input$x],na.rm=T)/1.5,yP.Nonlinear=position+0.5)+
                           geom_hline(yintercept=1,linetype=2,linewidth=0.7),width = input$width, height = input$height)
              })} else{
                output$distPlot= renderPlotly({
                  ggplotly(singlercs(data=factor_data,fit=model_reg,x=input$x,ribcol=input$col_ribbon,ribalpha=input$alpa,
                                     xlab =input$xlab,ylab=input$ylab,title=input$title,xP.Nonlinear=max(factor_data[,input$x],na.rm=T)/2.5,yP.Nonlinear=position+0.5)+geom_hline(yintercept=1,linetype=2,linewidth=0.7),
                           width = input$width, height = input$height)
                  
                })}
          }
        model_reg=logistic_regression(response=input$y,x=input$x,predictors=input$cov,data=factor_data)
        rcs=singlercs(data=factor_data,fit=model_reg,x=input$x)
        rcs_data=rcs$data
        output$rcs_data=downloadHandler(
          filename=function(){
            "限制性立方样条.xlsx"
          },
          content=function(file){
            write.xlsx(rcs_data,file)
          })
      }})
  }
  )
}

shinyApp(ui,server)