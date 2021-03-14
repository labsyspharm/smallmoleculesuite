library(glue)

mod_ui_modal_column <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "modal-dialog modal-lg",
    tags$div(
      class = "modal-content",
      tags$div(
        class = "modal-header",
        tags$h5(
          id = ns("modal-title"),
          class = "modal-title",
          "Title"
        ),
        tags$button(
          type = "button",
          class = "close",
          `data-dismiss` = "modal",
          `aria-label` = "Close",
          tags$span(
            `aria-hidden` = "true",
            icon("window-close")
          )
        )
      ),
      tags$div(
        id = ns("modal-body"),
        class = "modal-body",
        "Body"
      ),
      tags$div(
        class = "modal-footer",
        tags$button(
          class = "btn btn-primary",
          type = "button",
          `data-dismiss` = "modal",
          "Close"
        )
      )
    )
  ) %>%
    tags$div(
      id = ns("modal"),
      `aria-labelledby` = ns("modal-title"),
      `aria-hidden` = "true",
      class = "modal",
      role = "dialog"
    ) %>%
    tagList(
      htmltools::htmlDependency(
        "modal_column_js", "1.0",
        c(href = "sms/js"),
        script = "modal_column.js"
      )
    )
}

DISPLAY_COLUMN_JS <- r"--{
function(cell, cellData, rowData, rowIndex, colIndex) {
  if (cellData === "") {
    return;
  }
  const row_split = $(cell).html().split(";");
  const button = $("<button type=\"button\" class=\"btn btn-link p-0\" \
    data-toggle=\"modal\" data-target=\"#`modal_id`\">`button_text`</button>"
  ).on("click", {
      modal_id: "`modal_id`",
      header_content: row_split[0],
      body_content: row_split[1]
    }, column_modal_click_callback
  );
  $(cell).empty().append(button);
}
}--"

mod_server_modal_column <- function(
  input, output, session, button_text
) {
  ns <- session$ns

  glue(
    DISPLAY_COLUMN_JS,
    modal_id = ns("modal"),
    button_text = button_text %>%
      str_replace_all(fixed('"'), '\\"') %>%
      str_replace_all(fixed("\n"), "\\\n"),
    .open = "`", .close = "`"
  ) %>%
    JS()
}
