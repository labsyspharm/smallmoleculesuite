function update_column_modal(modal_id, header_content, body_content) {
  const modal = $("#" + modal_id);
  const header = modal.find(".modal-title").first();
  const body = modal.find(".modal-body").first();
  header.html(header_content);
  body.html(body_content);
}


function column_modal_click_callback(event) {
  const data = event.data;
  update_column_modal(data.modal_id, data.header_content, data.body_content);
  //$("#" + modal_id).modal("show");
}
