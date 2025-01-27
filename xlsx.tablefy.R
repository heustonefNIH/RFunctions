# Simplify making xlsx.workbooks

xlsx.tablefy <- function(workbook.table = workbook.table, 
                         sheet.name = sheet.name, 
                         workbook.name = workbook.name, 
                         sort.it = TRUE,
                         style = TRUE, 
                         style.cols = 1, #numeric list or character to pass to grep(pattern, x= colnames(workbook.table))
                         veryhigh.rule = ">=2", 
                         high.rule = ">=1.5", 
                         low.rule = "<=-1.5", 
                         verylow.rule = "<=-2", 
                         type = "expression",
                         start.Col = 1, 
                         start.Row = 1, 
                         col.Names = TRUE){
  
  ##write all markers to table
  if(sort.it == TRUE){
    workbook.table <- workbook.table %>% 
      group_by(cluster) %>%
      mutate(
        sort_group = case_when(
          avg_log2FC >= 2 ~ 1,          # Group 1: values >= 2
          avg_log2FC <= -2 ~ 2,         # Group 2: values <= -2
          avg_log2FC >= 1.5 ~ 3,          # Group 3: values >= 1
          TRUE ~ 4                  # Default group for remaining values
        )
      ) %>%
      arrange(
        sort_group,                 # Sort by groups
        desc(avg_log2FC) * (sort_group == 1) + # Descending for group 1
          avg_log2FC * (sort_group == 2) +    # Ascending for group 2
          desc(avg_log2FC) * (sort_group == 3) # Descending for group 3
      ) %>% 
      select(-sort_group) %>% 
      mutate(across(.fns = as.character)) %>%
      group_split()
    
  }else{
    workbook.table <- workbook.table %>% 
      group_by(cluster) %>% 
      mutate(across(.fns = as.character)) %>%
      group_split()
  }
    row.max <- max(
      sapply(1:length(workbook.table), 
             function(x){
               dim(workbook.table[[x]])[1]
             }
      )
    )
    for(i in 1:length(workbook.table)){
      gene.count <- dim(workbook.table[[i]])[1]
      spacer <- row.max - gene.count
      if(spacer != 0){
        spacer <- rep(NA, spacer)
        workbook.table[[i]] <- workbook.table[[i]] %>%
          tibble::add_row(avg_log2FC = spacer, p_val_adj = spacer, cluster = spacer, gene = spacer, .after = gene.count)
      }
    }
    workbook.table <- workbook.table %>% 
      bind_cols() %>% 
      mutate_at(vars(grep("avg_log2FC|p_val_adj", colnames(xlsx.all))), as.numeric)
    
  openxlsx::addWorksheet(workbook.name, sheetName = sheet.name)
  openxlsx::writeData(workbook.name, sheet = sheet.name, x = workbook.table, startCol = start.Col, startRow = start.Row, colNames = col.Names)

  if(style == TRUE){
    style.veryhigh <- openxlsx::createStyle(fontColour = "#FFFFFF", bgFill = "#377D43", textDecoration = "bold")
    style.high <- openxlsx::createStyle(fontColour = "#377D43", bgFill = "#CEEED0")
    style.low <- openxlsx::createStyle(fontColour = "#8E1C12", bgFill = "#F6C9CE")
    style.verylow <- openxlsx::createStyle(fontColour = "#FFFFFF", bgFill = "#8E1C12", textDecoration = "bold")
    
    if(is.character(style.cols)){
      style.cols <- grep(style.cols, colnames(workbook.table))
    }
    for(col.to.format in style.cols){
      openxlsx::conditionalFormatting(workbook.name, sheet = sheet.name, 
                                      cols = col.to.format, 
                                      rows = 1:nrow(workbook.table) +1,
                                      rule = high.rule, 
                                      style = style.high, 
                                      type = "expression", stack = TRUE)
      openxlsx::conditionalFormatting(workbook.name, sheet = sheet.name, 
                                      cols = col.to.format, 
                                      rows = 1:nrow(workbook.table) +1,
                                      rule = veryhigh.rule, 
                                      style = style.veryhigh, 
                                      type = "expression", stack = TRUE)
      openxlsx::conditionalFormatting(workbook.name, sheet = sheet.name, 
                                      cols = col.to.format, 
                                      rows = 1:nrow(workbook.table) +1,
                                      rule = low.rule, 
                                      style = style.low, 
                                      type = "expression", stack = TRUE)
      openxlsx::conditionalFormatting(workbook.name, sheet = sheet.name, 
                                      cols = col.to.format, 
                                      rows = 1:nrow(workbook.table) +1,
                                      rule = verylow.rule, 
                                      style = style.verylow, 
                                      type = "expression", stack = TRUE)
    }
    
  } 
}
