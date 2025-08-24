#' Run SASPACer Shiny App
#'
#' This function launches SASPACer shiny application. SASPACer is a tool originally made in SAS.
#' The SAS package SASPACer is excel-based support tool for creating SAS packages, while
#' SASPACer shiny app is atool to support creating SAS packages via user-friendly GUI.
#'
#' @description
#' This function starts a Shiny app that allows users to create SAS packages source
#' folders/files.
#'
#' @usage run_app()
#' @return No return value, launches a Shiny app.
#' @examples
#' # run_app()
#'
#' @import shiny bslib
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom bslib page_sidebar sidebar
#' @importFrom zip zip
#' @importFrom shiny shinyApp
#'
#' @export
run_app <- function() {

  ui <- page_sidebar(
    title   = "SASPACer shiny",
    sidebar = sidebar(width=500,
                      p("Set all in the main(right) panel and run:"),
                      downloadButton("button", "Run"),

                      textInput("newTabName", "New Tab Name:", value = "myTab"),
                      actionButton("add_tab", "Add Tab"),

                      HTML("
  <h4>Instructions(3 steps)</h4>
  <p>
  <b>step1:</b> Set all fields in the right panel. You can add tabs and remove tabs and edit contents(by double click) in each tab.<br>
  <b>step2:</b> Run button creates and downloads <i>zipped source folders/files</i> of SAS package.<br>
  <b>step3:</b> You can unzip and run <code>%generatePackage()</code> in SAS Packages Framework(SPF) using SAS
  to generate SAS package file.
  </p>
  <p>
  Example SAS codes to generate package after activating SPF is below. FilesLocation is for the folder path of
  unzipped source files.
  </p>
  <pre>
  %generatePackage(
    filesLocation=your\\location\\of\\package,
    markdownDoc=1,
    easyArch=1
  );
  </pre>
<p>
  ## References ##<br>
  Bartosz Jablonski, \"My First SAS Package - a How To\", SGF Proceedings, Paper 1079-2021,<br>
  <a href=\"https://communities.sas.com/t5/SAS-Global-Forum-Proceedings/My-First-SAS-Package-A-How-To/ta-p/726319\" target=\"_blank\">
    SAS Communities Article
  </a><br>
  <a href=\"https://communities.sas.com/kntur85557/attachments/kntur85557/proceedings-2021/59/1/Paper_1079-2021.pdf\" target=\"_blank\">
    PDF Paper
  </a>
</p>
  ")
    ),
    mainPanel(width=500,
              tabsetPanel(id="tabs", type = "tabs", selected = "description",
                          tabPanel("description", verbatimTextOutput("description"),
                                   mainPanel(
                                     selectInput("Type", "Type:", choices=list("Package"),
                                                 selected="Package", width=150),
                                     textInput("Package", label="Package:", value="myPackage", width=150),
                                     textInput("Title", label="Title:", value="My Package", width=150),
                                     textInput("Version", label="Version:", value="0.0.1", width=150),
                                     textInput("Author", label="Author:", value="Your Name(yourname@mail.com)", width=500),
                                     textInput("Maintainer",label="Maintainer:", value="Your Name(yourname@mail.com)", width=500),
                                     textInput("License", label="License:", value="MIT", width=150),
                                     textInput("Encoding", label="Encoding:", value="UTF8", width=150),
                                     textInput("Required", label="Required:", value="\"Base SAS Software\"", width=500),
                                     textInput("ReqPackage",label=HTML("ReqPackage:&nbsp;&nbsp;(set blank if no dependency with other SAS Packages)"), value="", width=500),
                                     textAreaInput("Description", label="Description:",
                                                   value="
## The myPackage ##
The `myPackage` is my first SAS package.
### References ###
Bartosz Jablonski, \"My First SAS Package - a How To\", SGF Proceedings, Paper 1079-2021,
https://communities.sas.com/t5/SAS-Global-Forum-Proceedings/My-First-SAS-Package-A-How-To/ta-p/726319
https://communities.sas.com/kntur85557/attachments/kntur85557/proceedings-2021/59/1/Paper_1079-2021.pdf
                  ", width=1000, height=200)
                                   )),
                          tabPanel("license",
                                   mainPanel(
                                     textAreaInput("License_text", label="License:",
                                                   value="
\tCopyright (c) [YEAR] [Owner Name]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
                  ", width=1000, height=500)
                                   )),
                          tabPanel("01_libname", verbatimTextOutput("01_libname"),
                                   mainPanel(
                                     DTOutput("code_table"),
                                     actionButton("add_row", "Add Row"),
                                     actionButton("del_row", "Delete Row"),
                                     actionButton("rm_lib", "Remove Tab", class = "btn-danger")
                                   ))

              )
    ),
    tags$style(HTML("
  table.dataTable {
    table-layout: fixed;
    width: 100% !important;
  }
  table.dataTable td {
    white-space: pre-wrap;      /* wrap */
    word-wrap: break-word;      /* force-break long words */
  }
  table.dataTable th:nth-child(1),
  table.dataTable td:nth-child(1) { width: 30px !important; }
  table.dataTable th:nth-child(2),
  table.dataTable td:nth-child(2) { width: 100px !important; }
  table.dataTable th:nth-child(3),
  table.dataTable td:nth-child(3) { width: 350px !important; }
  table.dataTable th:nth-child(4),
  table.dataTable td:nth-child(4) { width: 350px !important; }
"))
  )

  shiny::shinyApp(
    ui = ui,
    server = function(input, output, session) {
      # helper to make a 1-row df
      mk <- function(name="", help="", body="") {
        data.frame(name=name, help=help, body=body, stringsAsFactors=FALSE)
      }

      # --- initial data per tab ---
      initial_tables <- list(
        "01_libname"  = mk("mylib", "Create mylib under work.",
                           "
data _null_;
length rc0 $ 32767 rc1 rc2 8;
lib = \"myLib\";
rc0 = DCREATE(lib, \"%sysfunc(pathname(work))/\");
put rc0 = ;
rc1 = LIBNAME(lib, \"%sysfunc(pathname(work))/\" !! lib, \"BASE\");
rc2 = LIBREF (lib);
if rc2 NE 0 then rc1 = LIBNAME(lib, \"%sysfunc(pathname(work))\", \"BASE\");
run;
libname myLib LIST;"),
        "02_formats"  = rbind(
          mk("fmtnum",  "This is a format",
             "value fmtNum
         low -< 0 = \"negative\"
         0 = \"zero\"
         0 <- high = \"positive\"
         other = \"missing\" ;"),
          mk("infmtnum", "Yes/No format helpers",
             "\tinvalue infNum
         \"negative\" = -1
         \"zero\" = 0
         \"positive\" = 1
         \"missing\" = .
         other = 42;")
        ),
        "03_functions" = mk("f1", "F1 is a user-defined function to output +1 value.\noptions cmplib=work.f ; is required prior to f1 is used.", "function F1(n); return (n+1); endsub;"),
        "04_macro"    = mk("mymacro", "Macro to say hello", "%macro mymacro(); %put hello ; %mend;"),
        "05_test"     = mk("test_mymacro", "Test examples", "%mymacro()")
      )

      rv <- reactiveValues(tables = initial_tables)

      # ---- existing tab 01_libname ----
      output$code_table <- renderDT({
        datatable(rv$tables[["01_libname"]],
                  editable = TRUE,
                  selection = "single",
                  options = list(dom = 't', paging = FALSE, autoWidth = FALSE))
      })

      observeEvent(input$code_table_cell_edit, {
        info <- input$code_table_cell_edit
        rv$tables[["01_libname"]][info$row, info$col] <- info$value
      })
      observeEvent(input$add_row, {
        rv$tables[["01_libname"]] <- rbind(
          rv$tables[["01_libname"]],
          data.frame(name = "", help = "", body = "", stringsAsFactors = FALSE)
        )
      })
      observeEvent(input$del_row, {
        s <- input$code_table_rows_selected
        if (length(s)) {
          rv$tables[["01_libname"]] <- rv$tables[["01_libname"]][-s, , drop = FALSE]
        }
      })

      observeEvent(input$rm_lib, {
        removeTab("tabs", "01_libname")
        rv$tables[["01_libname"]] <- NULL
      })

      # ---- function to add editable tabs ----
      addEditableTab <- function(tab_id) {
        if (is.null(rv$tables[[tab_id]])) {
          rv$tables[[tab_id]] <- data.frame(
            name = "", help = "", body = "", stringsAsFactors = FALSE
          )
        }

        appendTab("tabs",
                  tabPanel(title = tab_id, value = tab_id,
                           mainPanel(
                             DTOutput(paste0("code_", tab_id)),
                             actionButton(paste0("add_row_", tab_id), "Add Row"),
                             actionButton(paste0("del_row_", tab_id), "Delete Row"),
                             actionButton(paste0("rm_tab_", tab_id), "Remove Tab", class = "btn-danger")
                           )
                  ),
                  select = FALSE)

        output[[paste0("code_", tab_id)]] <- renderDT({
          datatable(rv$tables[[tab_id]],
                    editable = TRUE,
                    selection = "single",
                    options = list(dom = 't', paging = FALSE, autoWidth = FALSE))
        })

        observeEvent(input[[paste0("code_", tab_id, "_cell_edit")]], {
          info <- input[[paste0("code_", tab_id, "_cell_edit")]]
          rv$tables[[tab_id]][info$row, info$col] <- info$value
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0("add_row_", tab_id)]], {
          rv$tables[[tab_id]] <- rbind(
            rv$tables[[tab_id]],
            data.frame(name = "", help = "", body = "", stringsAsFactors = FALSE)
          )
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0("del_row_", tab_id)]], {
          s <- input[[paste0("code_", tab_id, "_rows_selected")]]
          if (length(s)) {
            rv$tables[[tab_id]] <- rv$tables[[tab_id]][-s, , drop = FALSE]
          }
        }, ignoreInit = TRUE)

        observeEvent(input[[paste0("rm_tab_", tab_id)]], {
          removeTab("tabs", tab_id)
          rv$tables[[tab_id]] <- NULL
        }, ignoreInit = TRUE)
      }

      # create tabs 02–05 at startup
      isolate({
        for (id in c("02_formats","03_functions","04_macro","05_test")) {
          addEditableTab(id)
        }
      })

      # add tab by button
      observeEvent(input$add_tab, {
        tab_id <- trimws(input$newTabName)
        if (nchar(tab_id) == 0) return(NULL)

        if (!is.null(rv$tables[[tab_id]])) {
          updateTabsetPanel(session, "tabs", selected = tab_id)
          return(NULL)
        }
        addEditableTab(tab_id)
        updateTabsetPanel(session, "tabs", selected = tab_id)
      })

      # ---- ZIP download ----
      output$button <- downloadHandler(
        filename = function() {
          paste0(input$Package, ".zip")
        },
        content = function(file) {
          tmpdir <- file.path(tempdir(), "saspacer_build")
          if (dir.exists(tmpdir)) unlink(tmpdir, recursive=TRUE)
          dir.create(tmpdir)

          ## description.sas
          desc_text <- paste0(
            "Type: ", input$Type, "\n",
            "Package: ", input$Package, "\n",
            "Title: ", input$Title, "\n",
            "Version: ", input$Version, "\n",
            "Author: ", input$Author, "\n",
            "Maintainer: ", input$Maintainer, "\n",
            "License: ", input$License, "\n",
            "Encoding: ", input$Encoding, "\n",
            "Required: ", input$Required, "\n",
            "ReqPackage: ", input$ReqPackage, "\n\n",
            "DESCRIPTION START:\n\n",
            input$Description, "\n\n",
            "DESCRIPTION END:\n"
          )
          writeLines(desc_text, file.path(tmpdir, "description.sas"))

          ## license.sas
          writeLines(input$License_text, file.path(tmpdir, "license.sas"))

          ## per-tab folders/files
          for (tab_id in names(rv$tables)) {
            df <- rv$tables[[tab_id]]
            if (is.null(df) || nrow(df)==0) next
            subdir <- file.path(tmpdir, tab_id)
            dir.create(subdir)
            for (i in seq_len(nrow(df))) {
              nm <- trimws(df$name[i])
              if (nm=="") next
              fpath <- file.path(subdir, paste0(nm, ".sas"))
              fcontent <- paste0(
                "/*** HELP START ***/\n",
                "/* ", df$help[i], " */\n",
                "/*** HELP END ***/\n\n",
                df$body[i], "\n"
              )
              writeLines(fcontent, fpath)
            }
          }

          ## zip
          oldwd <- setwd(tmpdir)
          on.exit(setwd(oldwd), add=TRUE)
          zip::zip(zipfile=file, files=list.files(tmpdir, recursive=TRUE))
        }
      )
    }
  )
}
