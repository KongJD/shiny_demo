
# 使用 Shiny 和 SQLite 实现表管理的仪表板应用程序

library(shiny)
library(shinydashboard)
library(DBI)
library(DT)
library(RSQLite)

# 辅助函数：连接数据库
connect_db <- function() {
  dbConnect(RSQLite::SQLite(), "mydatabase.db")
}

# 辅助函数：断开数据库连接
disconnect_db <- function(conn) {
  dbDisconnect(conn)
}

# 辅助函数：安全格式化 SQL 标识符
sql_identifier <- function(name) {
  DBI::SQL(paste0("`", gsub("`", "``", name), "`"))
}

# 辅助函数：带有错误处理的 SQL 查询执行函数
execute_query <- function(conn, query) {
  tryCatch({
    dbExecute(conn, query)
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error",
      paste("发生错误：", e$message)
    ))
  })
}

# 辅助函数：带有错误处理的数据获取函数
get_data <- function(conn, query) {
  tryCatch({
    dbGetQuery(conn, query)
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error",
      paste("发生错误：", e$message)
    ))
    return(data.frame())
  })
}

# 创建一个演示用的表
initialize_db <- function(conn) {
  my_dat <- iris
  cols <- paste0("`", names(my_dat), "` ", vapply(my_dat, function(x) class(x), character(1)), collapse = ", ")
  create_table_sql <- paste0("CREATE TABLE IF NOT EXISTS my_dat_table (id INTEGER PRIMARY KEY, ", cols, ")")
  execute_query(conn, create_table_sql)
  dbWriteTable(conn, "my_dat_table", my_dat, append = TRUE, row.names = FALSE)
}

# UI 部分
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    collapsed = TRUE, 
    div(htmlOutput("welcome"), style = "padding: 20px"),
    sidebarMenu(
      menuItem("查看表", tabName = "view_table", icon = icon("search")),
      menuItem("创建表", tabName = "create_table", icon = icon("plus-square")),
      menuItem("更新表", tabName = "update_table", icon = icon("exchange-alt")),
      menuItem("插入条目", tabName = "insert_value", icon = icon("edit")),
      menuItem("删除表", tabName = "del_table", icon = icon("trash-alt")),
      menuItem("关于", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "view_table", uiOutput("tab1UI")),
      tabItem(tabName = "create_table", uiOutput("tab2UI")),
      tabItem(tabName = "update_table", uiOutput("tab3UI")),
      tabItem(tabName = "insert_value", uiOutput("tab4UI")),
      tabItem(tabName = "del_table", uiOutput("tab5UI")),
      tabItem(tabName = "about", uiOutput("tab6UI"))
    )
  ),
  
  # 捕获点击事件
  tags$script(HTML("
  $(document).on('click', '.edit', function() {
    var id = $(this).data('id');
    Shiny.setInputValue('edit_row_id', id, {priority: 'event'});
  });
  $(document).on('click', '.delete', function() {
    var id = $(this).data('id');
    Shiny.setInputValue('delete_row_id', id, {priority: 'event'});
  });
"))
)

# 服务器逻辑
server <- function(input, output, session) {
  # 连接数据库
  conn <- connect_db()
  initialize_db(conn)
  
  # 确保在退出时断开数据库连接
  onStop(function() {
    disconnect_db(conn)
  })
  
  # 欢迎信息
  output$welcome <- renderText({
    "欢迎使用 Shiny 仪表板！"
  })
  
  # 查看表的选项卡
  output$tab1UI <- renderUI({
    fluidPage(
      titlePanel("查看表"),
      selectInput("viewTableName", "选择表", choices = dbListTables(conn)),
      DTOutput("viewTable")
    )
  })
  
  output$viewTable <- renderDT({
    tableName <- input$viewTableName
    if (!is.null(tableName)) {
      data <- get_data(conn, paste0("SELECT * FROM ", sql_identifier(tableName)))
      datatable(data, options = list(pageLength = 5))
    }
  })
  
  # 创建表的选项卡
  output$tab2UI <- renderUI({
    fluidPage(
      titlePanel("创建表"),
      textInput("newTableName", "表名"),
      textInput("newColName", "列名"),
      selectInput("newColType", "列类型", choices = c("INTEGER", "TEXT", "REAL")),
      actionButton("addColumnBtn", "添加列"),
      verbatimTextOutput("columnsList"),
      actionButton("createTableBtn", "创建表")
    )
  })
  
  newCols <- reactiveVal(list())
  
  observeEvent(input$addColumnBtn, {
    newCol <- paste(sql_identifier(input$newColName), input$newColType)
    currentCols <- newCols()
    currentCols <- c(currentCols, newCol)
    newCols(currentCols)
    updateTextInput(session, "newColName", value = "")
  })
  
  output$columnsList <- renderPrint({
    cols <- newCols()
    if (length(cols) > 0) {
      cat(paste(cols, collapse = "\n"))
    } else {
      cat("尚未添加列。")
    }
  })
  
  observeEvent(input$createTableBtn, {
    tableName <- sql_identifier(input$newTableName)
    columns <- newCols()
    if (tableName != "" && length(columns) > 0) {
      colsQuery <- paste(columns, collapse = ", ")
      query <- paste0("CREATE TABLE IF NOT EXISTS ", tableName, " (id INTEGER PRIMARY KEY, ", colsQuery, ")")
      execute_query(conn, query)
      showModal(modalDialog(
        title = "表已创建",
        paste("表", input$newTableName, "已创建。")
      ))
      newCols(list())
    }
  })
  
  # 更新表的选项卡
  output$tab3UI <- renderUI({
    fluidPage(
      titlePanel("更新表"),
      selectInput("updateTableName", "选择表", choices = dbListTables(conn)),
      DTOutput("updateTable")
    )
  })
  
  output$updateTable <- renderDT({
    tableName <- input$updateTableName
    if (!is.null(tableName) && tableName != "") {
      data <- get_data(conn, paste0("SELECT * FROM ", sql_identifier(tableName)))
      if (nrow(data) > 0) {
        data$Actions <- paste0(
          '<button class="btn btn-primary btn-sm edit" data-id="', data$id, '">编辑</button> ',
          '<button class="btn btn-danger btn-sm delete" data-id="', data$id, '">删除</button>'
        )
        datatable(data, escape = FALSE, rownames = FALSE, options = list(pageLength = 5))
      } else {
        datatable(data)
      }
    }
  })
  
  # 捕获编辑单元格事件
  observeEvent(input$updateTable_cell_edit, {
    info <- input$updateTable_cell_edit
    tableName <- sql_identifier(input$updateTableName)
    columnName <- sql_identifier(n0ames(info)[2])
    query <- paste0("UPDATE ", tableName, " SET ", columnName, " = '", info$value, "' WHERE id = ", info$row)
    execute_query(conn, query)
  })
  
  # 捕获删除按钮事件
  observeEvent(input$delete_row_id, {
    rowID <- input$delete_row_id
    tableName <- sql_identifier(input$updateTableName)
    query <- paste0("DELETE FROM ", tableName, " WHERE id = ", rowID)
    execute_query(conn, query)
    
  })
  
  # 捕获编辑按钮事件
  observeEvent(input$edit_row_id, {
    rowID <- input$edit_row_id
    
    # 获取当前行的数据以预填充表单字段
    current_data <- get_data(conn, paste0("SELECT * FROM ", sql_identifier(input$updateTableName), " WHERE id = ", rowID))
    
    # 打开编辑模态窗口并预填充数据
    showModal(modalDialog(
      title = "编辑行",
      textInput("editField1", "字段1", value = current_data$field1),
      textInput("editField2", "字段2", value = current_data$field2),
      actionButton("saveEditBtn", "保存"),
      easyClose = TRUE
    ))
    
    # 保存时更新数据库并刷新表格数据 
    observeEvent(input$saveEditBtn,{
      update_query<-paste0(
        "UPDATE ",
        sql_identifier(input$updateTableName),
        "SET field1='",
        input.editField1,
        "',field2='",
        input.editField2,
        "'WHERE id =",rowID)
      
      execute_query(conn , update_query);
      removeModal()
      
    })
  })
  
  # 插入条目的选项卡
  output$tab4UI <- renderUI({
    fluidPage(
      titlePanel("插入条目"),
      selectInput("insertTableName", "选择表", choices = dbListTables(conn)),
      textInput("insertName", "名称"),
      numericInput("insertValue", "值", value = 0),
      actionButton("insertEntryBtn", "插入条目")
    )
  })
  
  observeEvent(input$insertEntryBtn, {
    tableName <- sql_identifier(input$insertTableName)
    insertName <- sql_identifier(input$insertName)
    insertValue <- input$insertValue
    if (tableName != "" && insertName != "") {
      query <- paste0("INSERT INTO ", tableName, " (name, value) VALUES ('", insertName, "', ", insertValue, ")")
      execute_query(conn, query)
      showModal(modalDialog(
        title = "条目已插入",
        paste("条目已插入表", input$insertTableName)
      ))
    }
  })
  
  # 删除表的选项卡
  output$tab5UI <- renderUI({
    fluidPage(
      titlePanel("删除表"),
      selectInput("deleteTableName", "选择表", choices = dbListTables(conn)),
      actionButton("deleteTableBtn", "删除表")
    )
  })
  
  observeEvent(input$deleteTableBtn, {
    tableName <- sql_identifier(input$deleteTableName)
    if (tableName != "") {
      query <- paste0("DROP TABLE IF EXISTS ", tableName)
      execute_query(conn, query)
      showModal(modalDialog(
        title = "表已删除",
        paste("表", input$deleteTableName, "已删除。")
      ))
    }
  })
  
  # 关于选项卡
  output$tab6UI <- renderUI({
    fluidPage(
      titlePanel("关于"),
      p("这是一个用于管理 SQLite 数据库表的 Shiny 应用程序。")
    )
  })
}

# 启动 Shiny 应用
shinyApp(ui, server)
