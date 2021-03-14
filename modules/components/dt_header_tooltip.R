DT_HEADER_FORMAT_JS = r'--{
  function(thead, data, start, end, display) {
    const tooltip_map = {`tooltip_map`};
    const new_th = $(thead).find("th:not(:has(span))");
    new_th.each(
      function(i) {
        const title = $(this).text();
        const tooltip = tooltip_map[title];
        if (tooltip == "NA")
          return;
        $(this).append(
          '`icon`'
        ).wrapInner(
          "<span class='contains-tooltip', data-toggle='tooltip'></span>"
        );
        const span = $(this).find("span");
        $(span).attr("title", tooltip);
        $(span).tooltip();
      }
    );
  }
}--'

DT_COMPOUND_NAME_JS <- r'--{
function(data, type, row, meta) {
    if (
      type === "display" && data !== null && data.length > 20
    )
      return `<span title=\"${data}\">${data.substr(0, 20)}...</span>`;
    return data;
  }
}--'

get_header_tooltip_js <- function(column_specs) {
  glue(
    DT_HEADER_FORMAT_JS, .open = "`", .close = "`",
    tooltip_map = with(
      column_specs,
      paste('"', column_name, '" : "', column_description, '"', sep = "", collapse = ",")
    ),
    icon = icon("info-circle") %>%
      margin(l = 1)
  ) %>%
    DT::JS()
}

datatable_tooltip <- function(..., column_specs = NULL, reorder_cols = FALSE) {
  dots <- rlang::list2(...)
  if (!("data" %in% names(dots))) {
    dots[["data"]] <- dots[[1]]
    dots[1] <- NULL
  }
  if (reorder_cols)
    dots[["data"]] <- dots[["data"]][, intersect(column_specs[["column_id"]], names(data))]
  if (is.null(column_specs))
    return(DT::datatable(...))
  if (all(c("column_id", "column_name") %in% names(column_specs))) {
    dots[["options"]][["columnDefs"]] <- dots[["options"]][["columnDefs"]] %||% list() %>%
      add_column_title_defs(
        names(dots[["data"]]),
        with(
          column_specs,
          set_names(column_name, column_id)
        )
      )
  }
  if (all(c("column_id", "column_description") %in% names(column_specs))) {
    dots[["options"]][["headerCallback"]] <- get_header_tooltip_js(column_specs)
  }
  rlang::exec(DT::datatable, !!!dots)
}

# Add DT column title defs to existing list of defs
add_column_title_defs <- function(defs, cols, col_map = COLUMN_TITLE_MAP) {
  name_shorten_def <- if ("name" %in% cols)
    list(
      targets = match("name", cols) - 1L,
      render = JS(DT_COMPOUND_NAME_JS)
    )
  match(names(col_map), cols) %>%
    set_names(col_map) %>%
    na.omit() %>%
    imap(~list(targets = .x - 1L, title = .y)) %>%
    unname() %>%
    c(
      defs,
      list(name_shorten_def)
    )
}
