jscode <- "
  shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"

app_builder_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c('disableTab','enableTab')),
    inlineCSS(css),
    wellPanel(
      style = "background: lightblue",
      textOutput(ns('instructions'))
    ),
    actionButton(
      ns('start'),
      'Start New App',
      onclick = "var $btn=$(this); setTimeout(function(){$btn.remove();},0);"
    ),
    uiOutput(ns('builderUI'))
  )
}

app_builder_server <- function(id, shinyMgrPath) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    needs_reset <- reactiveValues(reset = reactive(FALSE))
    
    output$instructions <- renderText(
      "This is where you use modules from the database to build an app. It is 
      essential that an App be fully planned before being built. For more on 
      planning your App, see the walkthrough in the wiki. Once your app is planned,
      this builder allows you to write all your App's metadata, define the tabs,
      mods, and how the mods will be stitched together. The builder than writes 
      a script for your App, and adds it to the database. Your App can then be 
      run using 'New Analysis'. Press \"Start New App\" to begin."
    )
    
    observeEvent(input$start, {
      # INSERT MAIN UI
      output$builderUI <- renderUI({
        
        tagList(
          sidebarLayout(
            sidebarPanel(
              width = 6,
              h1("Your App"),
              reactableOutput(ns("app_summary")),
              h6("")
            ),
            mainPanel(
              width = 6,
              tabsetPanel(
                id = ns("mainTabSet"),
                tabPanel(
                  title = "App Metadata",
                  value = "tab1",
                  add_app_metadata_UI(ns("add_app")),
                  actionButton(
                    ns("next_mod_1"),
                    "Begin 1st Tab"
                  )
                ),
                tabPanel(
                  title = "Begin Tab",
                  value = "tab2",
                  add_tab_metadata_UI(ns("add_tab")),
                  actionButton(
                    ns("next_mod_2"),
                    "Add Modules"
                  ),
                  HTML('&emsp;&emsp;'),
                  actionButton(ns('reset_button1'), 'Reset the builder')
                ),
                tabPanel(
                  title = "Add Module",
                  value = "tab3",
                  uiOutput(ns("mod_resettable")),
                  tags$br(),
                  tags$br(),
                  fluidRow(
                    disabled(actionButton(ns("new_tab"), "New Tab")),
                    disabled(actionButton(ns("new_mod"), "New Mod")),
                    disabled(actionButton(ns("finish_app"), "Finish Framework")),
                    HTML('&emsp;&emsp;'),
                    actionButton(ns('reset_button2'), 'Reset the builder')
                  )
                ),
                tabPanel(
                  title = "Match Arguments/Returns",
                  value = "tab4",
                  h5(""),
                  actionButton(ns("complete_app"), "Lock in stitch"),
                  HTML('&emsp;&emsp;'),
                  actionButton(ns('reset_button3'), 'Reset the builder')
                )
              )
            )
          )
        )
      })
    
    delay(50, {
      js$disableTab("tab2")
      js$disableTab("tab3")
      js$disableTab("tab4")
    })
    
    appNames <- qry_row(
      tableName = 'apps',
      colConditions = 'pkAppName',
      shinyMgrPath = shinyMgrPath
    )[,]
    tabNames <- qry_row(
      tableName = 'tabs',
      colConditions = 'pkTabName',
      shinyMgrPath = shinyMgrPath
    )[,]
    mod_info <- qry_row(
      tableName = 'modules',
      colConditions = c('pkModuleName', 'modDisplayName', 'modDescription'),
      shinyMgrPath = shinyMgrPath
    )
    qryModReturns <- qry_row('modFunctionReturns', shinyMgrPath = shinyMgrPath)
    qryModArguments <- qry_row('modFunctionArguments', shinyMgrPath = shinyMgrPath)

    # Set up table with mod argument info for display on "stitching" 
    mod_return_table <- qryModReturns[
      c("fkModuleName", "functionReturnName", "functionReturnClass", "description")
    ]
    names(mod_return_table) <- c('Module Name', 'Return Name', 'class', 'description')
    
    # Set up table with mod argument info for display on "stitching" 
    mod_arg_table <- qryModArguments[
      c("fkModuleName", "functionArgName", "functionArgClass", "description")
    ]
    names(mod_arg_table) <- c('Module Name', 'Argument Name', 'class', 'description')
    
    # Number of arguments that each module requires
    conx <- DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = paste0(shinyMgrPath, "/database/shinymgr.sqlite")
    )
    n_args <- rs <- DBI::dbGetQuery(
      conx,
      statement = 
        'SELECT modules.pkModuleName AS [mod_name], 
       Count(modFunctionArguments.pkModArgID) AS [ct]
       FROM modules LEFT JOIN modFunctionArguments 
       ON modules.pkModuleName = modFunctionArguments.fkModuleName
       GROUP BY modules.pkModuleName;'
    )
    DBI::dbDisconnect(conx)
    
    # Set up table with mod info to display
    names(mod_info) <- c('Module Name', 'Display Name', 'Description')
    for (i in 1:nrow(mod_info)) {
      mod_info$arg_ct[i] <- sum(qryModArguments$fkModuleName == mod_info$`Module Name`[i])
      mod_info$return_ct[i] <- sum(qryModReturns$fkModuleName == mod_info$`Module Name`[i])
    }
  
    shinyjs::disable('new_tab')
    shinyjs::disable('new_mod')
    shinyjs::disable('finish_app')
    
    # Store App, Tab, and Mod names (and order) for display
    app_meta <- reactiveVal(
      data.frame(
        elem_type = character(0),
        num = character(0),
        elem_name = character(0),
        stringsAsFactors = FALSE
      )
    ) 
    
    # Store mod order
    app_meta2 <- reactiveVal(
      data.frame(
        tab_num = integer(0),
        tab_name = character(0),
        mod_num = integer(0),
        mod_name = character(0),
        stringsAsFactors = FALSE
      )
    ) 
    
    # Which mods are available (based on how many returns have happened)
    available_mods <- reactiveVal(
      data.frame(
        mod_name = n_args$mod_name[n_args$ct==0]
      )
    )
    
    # Store available arguments
    available_args <- reactiveVal(
      data.frame(
        arg_name = character(0),
        arg_tab_name = character(0),
        arg_mod_order = character(0),
        stringsAsFactors = FALSE
      )
    )
    
    # Initialize counters
    mod_ct <- reactiveVal(0)
    tab_ct <- reactiveVal(0)
    ret_ct <- reactiveVal(0) # Number of returns encountered
    
    # Keep a running tally of tabs, for adding to the database
    tabs_2_add <- reactiveVal(
      data.frame(
        pkTabName = character(0),
        tabDisplayName = character(0),
        tabInstructions = character(0),
        tabNotes = character(0),
        stringsAsFactors = FALSE
      )
    )
    
    # Keep a running tally of mods, for adding to the database
    mods_2_add <- reactiveVal(
      data.frame(
        pkInstanceID = integer(0),
        fkTabName = character(0),
        fkModuleName = character(0),
        modOrder = integer(0),
        stringsAsFactors = FALSE
      )
    )
    
    output$mod_resettable <- renderUI({
      times <- input$new_mod
      times_tab <- input$new_tab
      add_mod_ui(ns('add_mod'))
    })
    
    data1 <- add_app_metadata_Server('add_app')
    data2 <- add_tab_metadata_Server('add_tab')
    data3 <- add_mod_server('add_mod', available_mods, mod_info)
    
    # BUTTON-CLICK (Begin 1st Tab)
    observeEvent(input$next_mod_1, {
      if (data1$app_name() %in% appNames |
          data1$app_name() %in% mod_info$pkModuleName) {
        showModal(modalDialog(
          title = "AppName Already in Use",
          "Select a different AppName"
        ))
      } else {
        # ADD TO VERBOSE OUTPUT
        # See for approach: https://stackoverflow.com/questions/54495321
        new_dat <- data.frame(
          elem_type = 'App Name:',
          num = as.character(''),
          elem_name = data1$app_display_name(),
          stringsAsFactors = FALSE
        )
        app_meta( rbind(app_meta(), new_dat, stringsAsFactors = FALSE) )
        
        # Move to next tab
        js$disableTab("tab1")
        js$enableTab("tab2")
        updateTabsetPanel(
          session, 
          'mainTabSet',
          selected = 'tab2'
        )
      }
    })
    
    output$mod_info_text <- renderUI({
      HTML(
        paste0(
          tags$hr(),
          tags$h3('Module Information'),
          tags$em('Use the information below to help choose which mods to use in your App:'),
          tags$br(), tags$br()
        )
      )
    })
    
    output$mod_info <- renderReactable({
      reactable(
        data = mod_info,
        filterable = TRUE,
        searchable = TRUE
        )
    })
    
    # Add the mod, when the button is clicked
    observeEvent(input$next_mod_2, {
      if (data2$tab_name() %in% c(tabNames, tabs_2_add()$pkTabName)) {
        showModal(modalDialog(
          title = "TabName Already in Use",
          "Select a different TabName"
        ))
      } else {
        
        js$disableTab("tab2")
        js$enableTab("tab3")
        
        tab_ct(tab_ct() + 1) # Increment tab counter
        
        app_meta( # Update app metadata (for display)
          rbind(
            app_meta(), 
            data.frame(
              elem_type = 'Tab Name:',
              num = paste0('(', tab_ct(),')'),
              elem_name = data2$tab_display_name(),
              stringsAsFactors = FALSE
            ), 
            stringsAsFactors = FALSE
          )
        )
        
        tabs_2_add( # keep a running list of new tabs to be added
          rbind(
            tabs_2_add(),
            data.frame(
              pkTabName = data2$tab_name(),
              tabDisplayName = data2$tab_display_name(),
              tabInstructions = data2$tab_inst(),
              tabNotes = data2$tab_notes(),
              stringsAsFactors = FALSE
            ),
            stringsAsFactors = FALSE
          )
        )
        
        # Add mod table to sidebar
        insertUI(
          selector = "h6",
          where = "beforeEnd",
          ui = tagList(
            htmlOutput(ns('mod_info_text')),
            reactableOutput(ns('mod_info'))
          )
        )
        
        updateTabsetPanel(
          session,
          'mainTabSet',
          selected = 'tab3'
        )

      }
    })
  
    output$app_summary <- renderReactable({
      reactable(data = app_meta())
    })
    
    observeEvent(data3(), { 
      print(data3()$mod_name())
      
      shinyjs::enable('new_tab')
      shinyjs::enable('new_mod')
      shinyjs::enable('finish_app')
      
      mod_ct(mod_ct() + 1) # Increment mod counter
      
      app_meta(
        rbind(
          app_meta(), 
          data.frame(
            elem_type = 'Mod Name:',
            num = as.character(paste0('(', mod_ct(),')')),
            elem_name = data3()$mod_name(),
            stringsAsFactors = FALSE
          ), 
          stringsAsFactors = FALSE
        ) 
      )
      
      app_meta2(
        rbind(
          app_meta2(), 
          data.frame(
            tab_num = tab_ct(),
            tab_name = data2$tab_name(),
            mod_num = mod_ct(),
            mod_name = data3()$mod_name(),
            stringsAsFactors = FALSE
          ), 
          stringsAsFactors = FALSE
        )
      )
      
      # Get names of returns (to be passed into available_args)
      new_args <- qryModReturns$functionReturnName[qryModReturns$fkModuleName == data3()$mod_name()]
      
      available_args(
        rbind(
          available_args(), 
          data.frame(
            arg_name = new_args,
            arg_tab_name = rep(data2$tab_display_name(),length(new_args)),
            arg_mod_name = rep(data3()$mod_name(),length(new_args)),
            arg_mod_order = rep(mod_ct(),length(new_args)),
            stringsAsFactors = FALSE
          ), 
          stringsAsFactors = FALSE
        )
      )
    })
    
    observeEvent(input$new_tab, {
      # Remove "mod_info" since not selecting a mod here
      removeUI(
        selector = paste0('#',ns("mod_info")),
        session = session
        )
      removeUI(
        selector = paste0('#',ns("mod_info_text")),
        session = session
      )
      
      # BELOW HERE attempt to reset mod on new tab
      # This attempt is successful, but lots of repeated code with "new_mod" button
      # Tried finding a way to reduce duplication, but removing it from "new_mod" 
      # messes up the "updatePanel" to stay on the right tab.
      times <- input$new_mod
      removeTab(
        inputId = 'mainTabSet',
        target = 'tab3'
      )
      insertTab(
        inputId = "mainTabSet",
        tabPanel(
          title = 'Add Module',
          value = paste('tab3'),
          add_mod_ui(ns('add_mod')),
          tags$br(),
          tags$br(),
          fluidRow(
            shinyjs::disabled(actionButton(ns('new_tab'),'New Tab')),
            shinyjs::disabled(actionButton(ns('new_mod'),'New Mod')),
            shinyjs::disabled(actionButton(ns('finish_app'),'Finish Framework')),
            HTML('&emsp;&emsp;'),
            actionButton(ns('reset_button2'), 'Reset the builder')
          )
        ),
        target = 'tab2',
        position = 'after'
      )
      # ABOVE HERE reset mod on new tab
      
      # Update available_mods
      selected_mod <- data3()$mod_name
      ret_ct(ret_ct() + sum(qryModReturns$fkModuleName == selected_mod())) # Update return count
      new_dat3 <- data.frame(
        mod_name = n_args$mod_name[n_args$ct <= ret_ct()],
        stringsAsFactors = FALSE
      )
      available_mods(new_dat3)
      # END update avail mods
      
      mod_ct(0) # reset mod_ct
      times <- input$new_tab
      
      removeTab(
        inputId = 'mainTabSet',
        target = 'tab2'
      )
      insertTab(
        inputId = 'mainTabSet',
        tabPanel(
          title = 'Begin Tab',
          value = 'tab2',
          add_tab_metadata_UI(ns('add_tab')),
          tags$br(),
          actionButton(
            ns('next_mod_2'),
            'Add Modules'
            ),
          HTML('&emsp;&emsp;'),
          actionButton(ns('reset_button1'), 'Reset the builder')
          ),
        target = 'tab1',
        position = 'after'
      )
      updateTabsetPanel(
        session,
        'mainTabSet',
        selected = 'tab2'
      )
      
      delay(50, {
        js$disableTab("tab3")
        js$enableTab("tab2")
      })
    })
    
    # Add some logic for the "new mod" button
    observeEvent(input$new_mod, {

      # Update available_mods
      selected_mod <- data3()$mod_name
      ret_ct(ret_ct() + sum(qryModReturns$fkModuleName == selected_mod())) # Update return count
      new_dat3 <- data.frame(
        mod_name = n_args$mod_name[n_args$ct <= ret_ct()],
        stringsAsFactors = FALSE
      )
      available_mods(new_dat3)
      
      times <- input$new_mod
      removeTab(
        inputId = 'mainTabSet',
        target = 'tab3'
      )
      insertTab(
        inputId = "mainTabSet",
        tabPanel(
          title = 'Add Module',
          value = paste('tab3'),
          add_mod_ui(ns('add_mod')),
          tags$br(),
          fluidRow(
            shinyjs::disabled(actionButton(ns('new_tab'),'New Tab')),
            shinyjs::disabled(actionButton(ns('new_mod'),'New Mod')),
            shinyjs::disabled(actionButton(ns('finish_app'),'Finish Framework')),
            HTML('&emsp;&emsp;'),
            actionButton(ns('reset_button2'), 'Reset the builder')
          )
        ),
        target = 'tab2',
        position = 'after'
      )
      updateTabsetPanel(
        session, 
        'mainTabSet',
        selected = 'tab3'
      )
    })
    
    observeEvent(input$finish_app, {
      
      # Render the tables, based on mods/arguments/returns that have appeared in the app
      used_mods <- unique(app_meta()[app_meta()$elem_type == 'Mod Name:','elem_name'])
      i_used_arg <- mod_arg_table$`Module Name` %in% used_mods
      i_used_return <- mod_return_table$`Module Name` %in% used_mods
      
      output$mod_arg_table <- renderReactable({
        reactable(
          mod_arg_table[i_used_arg,],
          defaultPageSize = 5,
          filterable = TRUE,
          searchable = TRUE
        )
      })
      
      output$mod_arg_table_text <- renderUI({
        HTML(
          paste0(
            tags$hr(),
            tags$h3('Module ARGUMENT Information'),
            tags$em('Use the information below to inform which arguments must be defined in your App:'),
            tags$br(), tags$br()
          )
        )
      })
      output$mod_return_table <- renderReactable({
        reactable(
          mod_return_table[i_used_return,],
          defaultPageSize = 5,
          filterable = TRUE,
          searchable = TRUE
        )
      })
      output$mod_return_table_text <- renderUI({
        HTML(
          paste0(
            tags$hr(),
            tags$h3('Module RETURN Information'),
            tags$em('Use the information below to inform what has been returned by modules in your App:'),
            tags$br(), tags$br()
          )
        )
      })
      
      # Remove "mod_info" since not selecting a mod here
      removeUI(
        selector = paste0('#',ns("mod_info")),
        session = session
      )
      removeUI(
        selector = paste0('#',ns("mod_info_text")),
        session = session
      )
      
      # Add mod table to sidebar
      insertUI(
        selector = "h6",
        where = "beforeEnd",
        ui = tagList(
          htmlOutput(ns('mod_arg_table_text')),
          reactableOutput(ns('mod_arg_table')),
          htmlOutput(ns('mod_return_table_text')),
          reactableOutput(ns('mod_return_table'))
        )
      )
      
      t_ct <- 0 # count of tabs (for stitching options)
      m_ct <- 0 # count of mods (for stitching options)
      avail_returns <- data.frame(
        return_tab_num = numeric(0),
        return_mod_num = numeric(0),
        return_name = character(0)
      ) # Keep track of which returns will be available
      
      #Loop through each tab/mod, updating stitching selection process sequentially
      for (i in 2:nrow(app_meta())) {

        # Increment if tab
        if (app_meta()$elem_type[i] == 'Tab Name:') {
          t_ct <- t_ct + 1
          m_ct <- 0
          
        # If not tab (ie., it's a mod), do stuff
        } else {
          m_ct <- m_ct + 1
          mod_name <- app_meta()$elem_name[i] # Mod Name
          # Create list of choices for drop-down
          mod_arg_choices <- character(nrow(avail_returns))
          for (j in 1:nrow(avail_returns)) {
            mod_arg_choices[j] <- paste0(
              'Tab #',
              avail_returns$return_tab_num[j],
              ' Mod #',
              avail_returns$return_mod_num[j],
              '; ',
              avail_returns$return_name[j]
            )
          }

          # Add dropdown for each argument that must be defined for the given mod
          
          # Arguments for that mod, add UI element for each
          the_args <- qryModArguments$functionArgName[qryModArguments$fkModuleName == mod_name] 
          for (the_arg in the_args) {
            insertUI(
              selector = "h5",
              where = "beforeEnd",
              ui = tagList(
                selectInput(
                  ns(paste('arg_select', t_ct, m_ct, the_arg, sep = '_')),
                  HTML(
                    paste0(
                      'Tab: ', t_ct, '; ',
                      'Mod: ', m_ct, '; ',
                      'Argument: ', the_arg, '<br/>',
                      'Assign Argument:' 
                    )
                  ),
                  choices = mod_arg_choices,
                  width = 500
                )
              )
            )
          }
          
          # Add any returns generated by the current mod
          the_returns <- qryModReturns$functionReturnName[qryModReturns$fkModuleName == mod_name]
          for (the_return in the_returns) {
            avail_returns <- rbind(
              avail_returns,
              data.frame(
                return_tab_num = t_ct,
                return_mod_num = m_ct,
                return_name = the_return
              )
            )
          }
        }
      }
      
      updateTabsetPanel(
        session, 
        'mainTabSet',
        selected = 'tab4'
      )
      
      js$disableTab("tab3")
      js$enableTab("tab4")
    })
    
    observeEvent(input$complete_app, {
      # This will be where all the writing to the database happens
      # Add new app to "App" Table
      rslt <- qry_insert(
        'apps',
        data.frame(
          pkAppName = data1$app_name(),
          appDisplayName = data1$app_display_name(),
          appDescription = data1$app_descrip(),
          appVideoURL = data1$app_videoURL(),
          appCSS = data1$app_CSS(),
          appNotes = data1$app_notes(),
          appActive = data1$app_active(),
          dateCreated = as.character(Sys.time()),
          fkParentAppName = NA
        ),
        shinyMgrPath
      )

      # Add each new tab to the "Tabs" Table
      for (i in seq_len(nrow(tabs_2_add()))) {
        rslt <- qry_insert(
          'tabs',
          data.frame(
            pkTabName = tabs_2_add()$pkTabName[i],
            tabDisplayName = tabs_2_add()$tabDisplayName[i],
            tabInstructions = ifelse(
              test = nchar(tabs_2_add()$tabInstructions[i]) != 0,
              yes = tabs_2_add()$tabInstructions[i],
              no = NA
            ),
            tabNotes = tabs_2_add()$tabNotes[i]
          ),
          shinyMgrPath
        )
        
        rslt <- qry_insert(
          'appTabs',
          data.frame(
            fkTabName = tabs_2_add()$pkTabName[i],
            fkAppName = data1$app_name(),
            tabOrder = i
          ),
          shinyMgrPath
        )
      }
      
      # 
      for (i in seq_len(nrow(app_meta2()))) {
        # Correction to accommodate scenario where tabModules is empty
        instanceIDs <- qry_row('tabModules', shinyMgrPath = shinyMgrPath)$pkInstanceID
        instanceID <- ifelse(length(instanceIDs), max(instanceIDs)+1, 1)

        rslt <- qry_insert(
          'tabModules',
          data.frame(
            fkTabName = app_meta2()$tab_name[i],
            fkModuleName = app_meta2()$mod_name[i],
            modOrder = app_meta2()$mod_num[i]
          ),
          shinyMgrPath
        )
        
        # Add return part of the stitching instructions
        i_mod_returns <- which(qryModReturns$fkModuleName == app_meta2()$mod_name[i])
        for (i_mod_return in i_mod_returns) {
          rslt <- qry_insert(
            'appStitching',
            data.frame(
              fkAppName = data1$app_name(),
              fkInstanceID = instanceID,
              fkModArgID = NA,
              fkModReturnID = qryModReturns$pkModReturnID[i_mod_return],
              fkStitchID = NA
            ),
            shinyMgrPath
          )
        }
        
        # Add argument part of the stitching instructions
        i_mod_args <- which(qryModArguments$fkModuleName == app_meta2()$mod_name[i])
        for (i_mod_arg in i_mod_args) {
          # Find the matching stitchID
          input_name <- paste(
            'arg_select', # reserved prefix
            app_meta2()$tab_num[i], # tab number
            app_meta2()$mod_num[i], # mod number
            qryModArguments$functionArgName[i_mod_arg], # Arg name
            sep = '_'
          )
          
          # Now, parse the button to directly fetch stitching info
          return_button_val <- input[[input_name]]
          delims <- unlist(gregexpr('#', return_button_val))
          delim2 <- unlist(gregexpr(' ', return_button_val))
          delim3 <- unlist(gregexpr(';', return_button_val))[1]
          ret_tab_num <- as.integer(substr(return_button_val, delims[1]+1, delim2[2]-1))
          ret_mod_num <- as.integer(substr(return_button_val, delims[2]+1, delim3-1))
          ret_mod_name <-  app_meta2()$mod_name[app_meta2()$tab_num == ret_tab_num][ret_mod_num]
          ret_name <- substr(return_button_val, delim2[length(delim2)]+1, nchar(return_button_val))
          
          # Get the pkModReturnID for the above return (which will become an argument)
          # Find tabModules row with...
          ret_instance_ID <- qry_row(
            'tabModules', 
            rowConditions = list(
              fkTabName = tabs_2_add()$pkTabName[ret_tab_num], #... matching tab name
              fkModuleName = ret_mod_name, #... mathcing mod name
              modOrder = ret_mod_num #... and matching mod order
            ),
            colConditions = 'pkInstanceID',
            shinyMgrPath = shinyMgrPath
          )[,]
  
          # Now, get the matching fkModArgID
          mod_return_ID <- qryModReturns$pkModReturnID[
            (qryModReturns$functionReturnName == ret_name) &
            (qryModReturns$fkModuleName == ret_mod_name)
          ]

          # Get stitch ID with matching return instance ID
          ret_stitch_id <- qry_row(
            'appStitching',
            rowConditions = list(
              fkInstanceID = ret_instance_ID,
              fkModReturnID = mod_return_ID
            ),
            colConditions = 'pkStitchID',
            shinyMgrPath = shinyMgrPath
          )[,]

          rslt <- qry_insert(
            'appStitching',
            data.frame(
              fkAppName = data1$app_name(),
              fkInstanceID = instanceID,
              fkModArgID = qryModArguments$pkModArgID[i_mod_arg],
              fkModReturnID = NA,
              fkStitchID = ret_stitch_id
            ),
            shinyMgrPath
          )
        }
      }

      # Run stitching script
      stitch_script(data1$app_name(), shinyMgrPath)
      
      # Show that the App has been Added
      showModal(modalDialog(
        title = "Update Success",
        "Your App has been written and added to the database. You will need to 
          reset shinymgr (log-out and log back in) to load your new app before 
          using it to run a new analysis.",
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
      
      # Trigger builder reset
      needs_reset$reset <- reactive(TRUE)
    })
    
    observeEvent(input$reset_button1, {
      needs_reset$reset <- reactive(TRUE)
    })
    
    observeEvent(input$reset_button2, {
      needs_reset$reset <- reactive(TRUE)
    })
    
    observeEvent(input$reset_button3, {
      needs_reset$reset <- reactive(TRUE)
    })

    observeEvent(needs_reset$reset, { 
      
      if (needs_reset$reset()) {
    
        # Reset tab enabling/disabling
        delay(50, {
          js$disableTab("tab2")
          js$disableTab("tab3")
          js$disableTab("tab4")
        })
  
        # Reset all internal variables
        app_meta(app_meta()[0,])
        app_meta2(app_meta2()[0,])
        available_mods(rbind(
          available_mods()[0,],
          data.frame(
            mod_name = n_args$mod_name[n_args$ct==0]
          )
        ))
        # Reset running list of tabs
        tabs_2_add(rbind(
          tabs_2_add()[0,],
          data.frame(
            pkTabName = character(0),
            tabDisplayName = character(0),
            tabInstructions = character(0),
            tabNotes = character(0),
            stringsAsFactors = FALSE
          )
        ))
  
        available_args(available_args()[0,])
  
        # Reset counters
        mod_ct(mod_ct()*0)
        tab_ct(tab_ct()*0)
        ret_ct(ret_ct()*0) # Number of returns encountered
        
        delay(50, {shinyjs::hide('start')})
      }
    })
    
  })
  return(needs_reset)
  })
}
