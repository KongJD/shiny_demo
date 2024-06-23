      
library(shiny)
library(shinydashboard)

vars <- setdiff(names(iris), "Species")

ui <- pageWithSidebar(
  ## Header
  headerPanel('Iris k-means clustering'),
  # actionButton("add_item", "Add item"),
  ## siderbar 侧边栏
  sidebarPanel(
    selectInput('xcol', 'X Variable', vars),
    selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
    numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
  ),
  ## 旁边的主区域部分
  mainPanel(
    plotOutput('plot1'),
  )
)

server <- function(input, output, session) {

  ## 这里获取到上边侧边栏的 xcol, ycol 这两个参数
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })

  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

    # 设置绘图参数，调整边距
    par(mar = c(5.1, 4.1, 0, 1))

    # 绘制散点图
    plot(selectedData(),             # 使用 selectedData() 函数获取的数据进行绘图
         col = clusters()$cluster,   # 根据 clusters() 函数返回的簇信息设置颜色
         pch = 20,                   # 设置点的形状为实心圆点
         cex = 3)                    # 设置点的大小

    # 在散点图上添加簇中心点
    points(clusters()$centers,       # 使用 clusters() 函数返回的簇中心数据进行绘图
           pch = 4,                  # 设置中心点的形状为叉号
           cex = 4,                  # 设置中心点的大小
           lwd = 4)                  # 设置中心点线条的宽度
  })

}

shinyApp(ui = ui, server = server)