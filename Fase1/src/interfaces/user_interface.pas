unit user_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, glib2, gtk2, UStructures, UUser, ULogin;

type
  TUserInterface = class
  private
    window: PGtkWidget;
    notebook: PGtkWidget;
    current_user: PUser;

    // Variables para diferentes pestañas
    inbox_tree: PGtkWidget;
    inbox_model: PGtkListStore;

    // Variables para enviar correo
    send_to_entry: PGtkWidget;
    send_subject_entry: PGtkWidget;
    send_message_text: PGtkWidget;

    // Variables para papelera
    trash_tree: PGtkWidget;
    trash_search_entry: PGtkWidget;

    // Variables para correos programados
    schedule_tree: PGtkWidget;
    schedule_to_entry: PGtkWidget;
    schedule_subject_entry: PGtkWidget;
    schedule_message_text: PGtkWidget;
    schedule_date_entry: PGtkWidget;

    // Variables para contactos
    contacts_tree: PGtkWidget;
    add_contact_entry: PGtkWidget;
    current_contact_label: PGtkWidget;

    // Variables para perfil
    profile_username_entry: PGtkWidget;
    profile_phone_entry: PGtkWidget;

    procedure CreateMainWindow;
    procedure CreateInboxTab;
    procedure CreateSendEmailTab;
    procedure CreateTrashTab;
    procedure CreateScheduleTab;
    procedure CreateContactsTab;
    procedure CreateProfileTab;
    procedure CreateReportsTab;

    // Callbacks para bandeja de entrada
    procedure OnSortEmails(widget: PGtkWidget; data: gpointer); cdecl;
    procedure OnEmailSelected(selection: PGtkTreeSelection; data: gpointer); cdecl;
    procedure OnDeleteEmail(widget: PGtkWidget; data: gpointer); cdecl;

    // Callbacks para enviar correo
    procedure OnSendEmail(widget: PGtkWidget; data: gpointer); cdecl;

    // Callbacks para papelera
    procedure OnSearchTrash(widget: PGtkWidget; data: gpointer); cdecl;
    procedure OnDeleteFromTrash(widget: PGtkWidget; data: gpointer); cdecl;

    // Callbacks para correos programados
    procedure OnScheduleEmail(widget: PGtkWidget; data: gpointer); cdecl;
    procedure OnProcessScheduled(widget: PGtkWidget; data: gpointer); cdecl;

    // Callbacks para contactos
    procedure OnAddContact(widget: PGtkWidget; data: gpointer); cdecl;
    procedure OnPrevContact(widget: PGtkWidget; data: gpointer); cdecl;
    procedure OnNextContact(widget: PGtkWidget; data: gpointer); cdecl;

    // Callbacks para perfil
    procedure OnUpdateProfile(widget: PGtkWidget; data: gpointer); cdecl;

    // Callbacks para reportes
    procedure OnGenerateReports(widget: PGtkWidget; data: gpointer); cdecl;

    // Métodos auxiliares
    procedure RefreshInbox;
    procedure RefreshTrash;
    procedure RefreshScheduled;
    procedure RefreshContacts;
    procedure ShowMessage(const msg: String);

  public
    constructor Create(user: PUser);
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
  end;

implementation

constructor TUserInterface.Create(user: PUser);
begin
  inherited Create;
  current_user := user;
  UserManager.SetCurrentUser(user);
  CreateMainWindow;
end;

destructor TUserInterface.Destroy;
begin
  if window <> nil then
    gtk_widget_destroy(window);
  inherited Destroy;
end;

procedure TUserInterface.CreateMainWindow;
var
  vbox: PGtkWidget;
  welcome_label: PGtkWidget;
  welcome_text: String;
begin
  // Crear ventana principal
  window := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  welcome_text := 'EDDMail - ' + current_user^.nombre;
  gtk_window_set_title(GTK_WINDOW(window), PChar(welcome_text));
  gtk_window_set_default_size(GTK_WINDOW(window), 800, 600);
  gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);

  vbox := gtk_vbox_new(FALSE, 5);
  gtk_container_add(GTK_CONTAINER(window), vbox);

  // Etiqueta de bienvenida
  welcome_text := 'Bienvenido: ' + current_user^.nombre + ' (' + current_user^.email + ')';
  welcome_label := gtk_label_new(PChar(welcome_text));
  gtk_box_pack_start(GTK_BOX(vbox), welcome_label, FALSE, FALSE, 10);

  // Crear notebook (pestañas)
  notebook := gtk_notebook_new();
  gtk_box_pack_start(GTK_BOX(vbox), notebook, TRUE, TRUE, 0);

  // Crear todas las pestañas
  CreateInboxTab;
  CreateSendEmailTab;
  CreateTrashTab;
  CreateScheduleTab;
  CreateContactsTab;
  CreateProfileTab;
  CreateReportsTab;

  g_signal_connect(G_OBJECT(window), 'destroy', G_CALLBACK(@gtk_main_quit), nil);
end;

procedure TUserInterface.CreateInboxTab;
var
  vbox, hbox: PGtkWidget;
  label, sort_button, delete_button: PGtkWidget;
  scrolled: PGtkWidget;
  renderer: PGtkCellRenderer;
  column: PGtkTreeViewColumn;
  selection: PGtkTreeSelection;
begin
  vbox := gtk_vbox_new(FALSE, 5);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Botones de acción
  hbox := gtk_hbox_new(FALSE, 5);
  sort_button := gtk_button_new_with_label('Ordenar por Asunto (A-Z)');
  delete_button := gtk_button_new_with_label('Eliminar Correo');

  gtk_box_pack_start(GTK_BOX(hbox), sort_button, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), delete_button, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

  // Crear modelo y vista
  inbox_model := gtk_list_store_new(5, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
  inbox_tree := gtk_tree_view_new_with_model(GTK_TREE_MODEL(inbox_model));

  // Configurar columnas
  renderer := gtk_cell_renderer_text_new();
  column := gtk_tree_view_column_new_with_attributes('ID', renderer, 'text', 0, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(inbox_tree), column);

  column := gtk_tree_view_column_new_with_attributes('Estado', renderer, 'text', 1, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(inbox_tree), column);

  column := gtk_tree_view_column_new_with_attributes('Asunto', renderer, 'text', 2, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(inbox_tree), column);

  column := gtk_tree_view_column_new_with_attributes('Remitente', renderer, 'text', 3, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(inbox_tree), column);

  column := gtk_tree_view_column_new_with_attributes('Fecha', renderer, 'text', 4, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(inbox_tree), column);

  // Scroll para la lista
  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add(GTK_CONTAINER(scrolled), inbox_tree);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled, TRUE, TRUE, 0);

  // Conectar señales
  g_signal_connect(G_OBJECT(sort_button), 'clicked', G_CALLBACK(@OnSortEmails), nil);
  g_signal_connect(G_OBJECT(delete_button), 'clicked', G_CALLBACK(@OnDeleteEmail), nil);

  selection := gtk_tree_view_get_selection(GTK_TREE_VIEW(inbox_tree));
  g_signal_connect(G_OBJECT(selection), 'changed', G_CALLBACK(@OnEmailSelected), nil);

  label := gtk_label_new('Bandeja de Entrada');
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox, label);

  RefreshInbox;
end;

procedure TUserInterface.CreateSendEmailTab;
var
  vbox: PGtkWidget;
  label: PGtkWidget;
  table: PGtkWidget;
  to_label, subject_label, message_label: PGtkWidget;
  send_button: PGtkWidget;
  scrolled: PGtkWidget;
begin
  vbox := gtk_vbox_new(FALSE, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Crear tabla para los campos
  table := gtk_table_new(3, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), 5);
  gtk_table_set_col_spacings(GTK_TABLE(table), 5);

  // Campo destinatario
  to_label := gtk_label_new('Para:');
  send_to_entry := gtk_entry_new();
  gtk_table_attach(GTK_TABLE(table), to_label, 0, 1, 0, 1, GTK_FILL, GTK_FILL, 0, 0);
  gtk_table_attach(GTK_TABLE(table), send_to_entry, 1, 2, 0, 1, GTK_EXPAND or GTK_FILL, GTK_FILL, 0, 0);

  // Campo asunto
  subject_label := gtk_label_new('Asunto:');
  send_subject_entry := gtk_entry_new();
  gtk_table_attach(GTK_TABLE(table), subject_label, 0, 1, 1, 2, GTK_FILL, GTK_FILL, 0, 0);
  gtk_table_attach(GTK_TABLE(table), send_subject_entry, 1, 2, 1, 2, GTK_EXPAND or GTK_FILL, GTK_FILL, 0, 0);

  gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);

  // Campo mensaje
  message_label := gtk_label_new('Mensaje:');
  gtk_misc_set_alignment(GTK_MISC(message_label), 0, 0);
  gtk_box_pack_start(GTK_BOX(vbox), message_label, FALSE, FALSE, 0);

  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  send_message_text := gtk_text_view_new();
  gtk_container_add(GTK_CONTAINER(scrolled), send_message_text);
  gtk_widget_set_size_request(scrolled, -1, 200);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled, TRUE, TRUE, 0);

  // Botón enviar
  send_button := gtk_button_new_with_label('Enviar Correo');
  gtk_box_pack_start(GTK_BOX(vbox), send_button, FALSE, FALSE, 0);

  g_signal_connect(G_OBJECT(send_button), 'clicked', G_CALLBACK(@OnSendEmail), nil);

  label := gtk_label_new('Enviar Correo');
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox, label);
end;

procedure TUserInterface.CreateTrashTab;
var
  vbox, hbox: PGtkWidget;
  label: PGtkWidget;
  search_button, delete_button: PGtkWidget;
  scrolled: PGtkWidget;
  renderer: PGtkCellRenderer;
  column: PGtkTreeViewColumn;
begin
  vbox := gtk_vbox_new(FALSE, 5);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Barra de búsqueda
  hbox := gtk_hbox_new(FALSE, 5);
  trash_search_entry := gtk_entry_new();
  gtk_entry_set_placeholder_text(GTK_ENTRY(trash_search_entry), 'Buscar por palabra clave...');
  search_button := gtk_button_new_with_label('Buscar');
  delete_button := gtk_button_new_with_label('Eliminar de Papelera');

  gtk_box_pack_start(GTK_BOX(hbox), trash_search_entry, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), search_button, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), delete_button, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

  // Lista de correos eliminados
  trash_tree := gtk_tree_view_new();

  // Configurar columnas
  renderer := gtk_cell_renderer_text_new();
  column := gtk_tree_view_column_new_with_attributes('ID', renderer, 'text', 0, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(trash_tree), column);

  column := gtk_tree_view_column_new_with_attributes('Asunto', renderer, 'text', 1, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(trash_tree), column);

  column := gtk_tree_view_column_new_with_attributes('Remitente', renderer, 'text', 2, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(trash_tree), column);

  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add(GTK_CONTAINER(scrolled), trash_tree);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled, TRUE, TRUE, 0);

  g_signal_connect(G_OBJECT(search_button), 'clicked', G_CALLBACK(@OnSearchTrash), nil);
  g_signal_connect(G_OBJECT(delete_button), 'clicked', G_CALLBACK(@OnDeleteFromTrash), nil);

  label := gtk_label_new('Papelera');
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox, label);

  RefreshTrash;
end;

procedure TUserInterface.CreateScheduleTab;
var
  vbox, hbox: PGtkWidget;
  label: PGtkWidget;
  table: PGtkWidget;
  to_label, subject_label, message_label, date_label: PGtkWidget;
  schedule_button, process_button: PGtkWidget;
  scrolled, scrolled_list: PGtkWidget;
  renderer: PGtkCellRenderer;
  column: PGtkTreeViewColumn;
begin
  vbox := gtk_vbox_new(FALSE, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Formulario para programar correo
  table := gtk_table_new(4, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), 5);
  gtk_table_set_col_spacings(GTK_TABLE(table), 5);

  to_label := gtk_label_new('Para:');
  schedule_to_entry := gtk_entry_new();
  gtk_table_attach(GTK_TABLE(table), to_label, 0, 1, 0, 1, GTK_FILL, GTK_FILL, 0, 0);
  gtk_table_attach(GTK_TABLE(table), schedule_to_entry, 1, 2, 0, 1, GTK_EXPAND or GTK_FILL, GTK_FILL, 0, 0);

  subject_label := gtk_label_new('Asunto:');
  schedule_subject_entry := gtk_entry_new();
  gtk_table_attach(GTK_TABLE(table), subject_label, 0, 1, 1, 2, GTK_FILL, GTK_FILL, 0, 0);
  gtk_table_attach(GTK_TABLE(table), schedule_subject_entry, 1, 2, 1, 2, GTK_EXPAND or GTK_FILL, GTK_FILL, 0, 0);

  date_label := gtk_label_new('Fecha (dd/mm/yy hh:mm):');
  schedule_date_entry := gtk_entry_new();
  gtk_entry_set_placeholder_text(GTK_ENTRY(schedule_date_entry), '25/12/24 10:30');
  gtk_table_attach(GTK_TABLE(table), date_label, 0, 1, 2, 3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_table_attach(GTK_TABLE(table), schedule_date_entry, 1, 2, 2, 3, GTK_EXPAND or GTK_FILL, GTK_FILL, 0, 0);

  gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);

  // Campo mensaje
  message_label := gtk_label_new('Mensaje:');
  gtk_misc_set_alignment(GTK_MISC(message_label), 0, 0);
  gtk_box_pack_start(GTK_BOX(vbox), message_label, FALSE, FALSE, 0);

  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  schedule_message_text := gtk_text_view_new();
  gtk_container_add(GTK_CONTAINER(scrolled), schedule_message_text);
  gtk_widget_set_size_request(scrolled, -1, 100);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled, FALSE, FALSE, 0);

  // Botones
  hbox := gtk_hbox_new(FALSE, 5);
  schedule_button := gtk_button_new_with_label('Programar Correo');
  process_button := gtk_button_new_with_label('Enviar Correos Programados');
  gtk_box_pack_start(GTK_BOX(hbox), schedule_button, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), process_button, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

  // Lista de correos programados
  schedule_tree := gtk_tree_view_new();
  renderer := gtk_cell_renderer_text_new();

  column := gtk_tree_view_column_new_with_attributes('Para', renderer, 'text', 0, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(schedule_tree), column);

  column := gtk_tree_view_column_new_with_attributes('Asunto', renderer, 'text', 1, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(schedule_tree), column);

  column := gtk_tree_view_column_new_with_attributes('Fecha Envío', renderer, 'text', 2, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(schedule_tree), column);

  scrolled_list := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_list), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add(GTK_CONTAINER(scrolled_list), schedule_tree);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled_list, TRUE, TRUE, 0);

  g_signal_connect(G_OBJECT(schedule_button), 'clicked', G_CALLBACK(@OnScheduleEmail), nil);
  g_signal_connect(G_OBJECT(process_button), 'clicked', G_CALLBACK(@OnProcessScheduled), nil);

  label := gtk_label_new('Programar Correo');
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox, label);

  RefreshScheduled;
end;

procedure TUserInterface.CreateContactsTab;
var
  vbox, hbox, navigation_box: PGtkWidget;
  label: PGtkWidget;
  add_label: PGtkWidget;
  add_button, prev_button, next_button: PGtkWidget;
  info_frame: PGtkWidget;
  info_vbox: PGtkWidget;
begin
  vbox := gtk_vbox_new(FALSE, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Sección para agregar contacto
  hbox := gtk_hbox_new(FALSE, 5);
  add_label := gtk_label_new('Email del contacto:');
  add_contact_entry := gtk_entry_new();
  add_button := gtk_button_new_with_label('Agregar');

  gtk_box_pack_start(GTK_BOX(hbox), add_label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), add_contact_entry, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), add_button, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

  // Navegación de contactos
  navigation_box := gtk_hbox_new(FALSE, 5);
  prev_button := gtk_button_new_with_label('< Anterior');
  next_button := gtk_button_new_with_label('Siguiente >');

  gtk_box_pack_start(GTK_BOX(navigation_box), prev_button, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(navigation_box), next_button, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), navigation_box, FALSE, FALSE, 0);

  // Frame para mostrar información del contacto actual
  info_frame := gtk_frame_new('Contacto Actual');
  info_vbox := gtk_vbox_new(FALSE, 5);
  gtk_container_set_border_width(GTK_CONTAINER(info_vbox), 10);

  current_contact_label := gtk_label_new('No hay contactos');
  gtk_box_pack_start(GTK_BOX(info_vbox), current_contact_label, FALSE, FALSE, 0);

  gtk_container_add(GTK_CONTAINER(info_frame), info_vbox);
  gtk_box_pack_start(GTK_BOX(vbox), info_frame, TRUE, TRUE, 0);

  g_signal_connect(G_OBJECT(add_button), 'clicked', G_CALLBACK(@OnAddContact), nil);
  g_signal_connect(G_OBJECT(prev_button), 'clicked', G_CALLBACK(@OnPrevContact), nil);
  g_signal_connect(G_OBJECT(next_button), 'clicked', G_CALLBACK(@OnNextContact), nil);

  label := gtk_label_new('Contactos');
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox, label);

  RefreshContacts;
end;

procedure TUserInterface.CreateProfileTab;
var
  vbox: PGtkWidget;
  label: PGtkWidget;
  table: PGtkWidget;
  name_label, username_label, email_label, phone_label: PGtkWidget;
  name_entry, email_entry: PGtkWidget;
  update_button: PGtkWidget;
begin
  vbox := gtk_vbox_new(FALSE, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  table := gtk_table_new(4, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), 10);
  gtk_table_set_col_spacings(GTK_TABLE(table), 10);

  // Nombre (solo lectura)
  name_label := gtk_label_new('Nombre:');
  name_entry := gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(name_entry), PChar(current_user^.nombre));
  gtk_widget_set_sensitive(name_entry, FALSE);
  gtk_table_attach(GTK_TABLE(table), name_label, 0, 1, 0, 1, GTK_FILL, GTK_FILL, 0, 0);
  gtk_table_attach(GTK_TABLE(table), name_entry, 1, 2, 0, 1, GTK_EXPAND or GTK_FILL, GTK_FILL, 0, 0);

  // Usuario (editable)
  username_label := gtk_label_new('Usuario:');
  profile_username_entry := gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(profile_username_entry), PChar(current_user^.usuario));
  gtk_table_attach(GTK_TABLE(table), username_label, 0, 1, 1, 2, GTK_FILL, GTK_FILL, 0, 0);
  gtk_table_attach(GTK_TABLE(table), profile_username_entry, 1, 2, 1, 2, GTK_EXPAND or GTK_FILL, GTK_FILL, 0, 0);

  // Email (solo lectura)
  email_label := gtk_label_new('Email:');
  email_entry := gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(email_entry), PChar(current_user^.email));
  gtk_widget_set_sensitive(email_entry, FALSE);
  gtk_table_attach(GTK_TABLE(table), email_label, 0, 1, 2, 3, GTK_FILL, GTK_FILL, 0, 0);
  gtk_table_attach(GTK_TABLE(table), email_entry, 1, 2, 2, 3, GTK_EXPAND or GTK_FILL, GTK_FILL, 0, 0);

  // Teléfono (editable)
  phone_label := gtk_label_new('Teléfono:');
  profile_phone_entry := gtk_entry_new();
  gtk_entry_set_text(GTK_ENTRY(profile_phone_entry), PChar(current_user^.telefono));
  gtk_table_attach(GTK_TABLE(table), phone_label, 0, 1, 3, 4, GTK_FILL, GTK_FILL, 0, 0);
  gtk_table_attach(GTK_TABLE(table), profile_phone_entry, 1, 2, 3, 4, GTK_EXPAND or GTK_FILL, GTK_FILL, 0, 0);

  gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);

  update_button := gtk_button_new_with_label('Actualizar Perfil');
  gtk_box_pack_start(GTK_BOX(vbox), update_button, FALSE, FALSE, 0);

  g_signal_connect(G_OBJECT(update_button), 'clicked', G_CALLBACK(@OnUpdateProfile), nil);

  label := gtk_label_new('Actualizar Perfil');
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox, label);
end;

procedure TUserInterface.CreateReportsTab;
var
  vbox: PGtkWidget;
  label: PGtkWidget;
  generate_button: PGtkWidget;
  info_label: PGtkWidget;
  reports_info: String;
begin
  vbox := gtk_vbox_new(FALSE, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  reports_info := 'Generar todos los reportes de usuario:' + LineEnding +
                  '• Reporte de Correos Recibidos' + LineEnding +
                  '• Reporte de Papelera' + LineEnding +
                  '• Reporte de Correos Programados' + LineEnding +
                  '• Reporte de Contactos' + LineEnding + LineEnding +
                  'Los reportes se guardarán en: reports/' + current_user^.usuario + '-Reportes/';

  info_label := gtk_label_new(PChar(reports_info));
  gtk_label_set_justify(GTK_LABEL(info_label), GTK_JUSTIFY_LEFT);
  gtk_box_pack_start(GTK_BOX(vbox), info_label, TRUE, TRUE, 0);

  generate_button := gtk_button_new_with_label('Generar Todos los Reportes');
  gtk_box_pack_start(GTK_BOX(vbox), generate_button, FALSE, FALSE, 0);

  g_signal_connect(G_OBJECT(generate_button), 'clicked', G_CALLBACK(@OnGenerateReports), nil);

  label := gtk_label_new('Generar Reportes');
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), vbox, label);
end;

// ============================================================================
// CALLBACKS - BANDEJA DE ENTRADA
// ============================================================================

procedure TUserInterface.OnSortEmails(widget: PGtkWidget; data: gpointer); cdecl;
begin
  UserManager.SortEmailsBySubject;
  RefreshInbox;
  ShowMessage('Correos ordenados alfabéticamente por asunto');
end;

procedure TUserInterface.OnEmailSelected(selection: PGtkTreeSelection; data: gpointer); cdecl;
var
  model: PGtkTreeModel;
  iter: TGtkTreeIter;
  email_id: Integer;
  email: PEmail;
  dialog: PGtkWidget;
  vbox: PGtkWidget;
  label: PGtkWidget;
  message_text: String;
begin
  if gtk_tree_selection_get_selected(selection, @model, @iter) then
  begin
    gtk_tree_model_get(model, @iter, 0, @email_id, -1);
    email := EmailList.FindEmailById(email_id);

    if (email <> nil) and (email^.destinatario = current_user^.email) then
    begin
      // Marcar como leído
      UserManager.MarkEmailAsRead(email_id);

      // Mostrar contenido del correo
      dialog := gtk_dialog_new_with_buttons(
        'Leer Correo',
        GTK_WINDOW(window),
        GTK_DIALOG_MODAL,
        'Cerrar', GTK_RESPONSE_CLOSE,
        nil
      );

      gtk_window_set_default_size(GTK_WINDOW(dialog), 500, 400);

      vbox := gtk_vbox_new(FALSE, 10);
      gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

      message_text := 'De: ' + email^.remitente + LineEnding +
                     'Asunto: ' + email^.asunto + LineEnding +
                     'Fecha: ' + email^.fecha + LineEnding + LineEnding +
                     email^.mensaje;

      label := gtk_label_new(PChar(message_text));
      gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
      gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
      gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

      gtk_box_pack_start(GTK_BOX(gtk_dialog_get_content_area(GTK_DIALOG(dialog))), vbox, TRUE, TRUE, 0);

      gtk_widget_show_all(dialog);
      gtk_dialog_run(GTK_DIALOG(dialog));
      gtk_widget_destroy(dialog);

      RefreshInbox;
    end;
  end;
end;

procedure TUserInterface.OnDeleteEmail(widget: PGtkWidget; data: gpointer); cdecl;
var
  selection: PGtkTreeSelection;
  model: PGtkTreeModel;
  iter: TGtkTreeIter;
  email_id: Integer;
begin
  selection := gtk_tree_view_get_selection(GTK_TREE_VIEW(inbox_tree));
  if gtk_tree_selection_get_selected(selection, @model, @iter) then
  begin
    gtk_tree_model_get(model, @iter, 0, @email_id, -1);
    UserManager.DeleteEmail(email_id);
    RefreshInbox;
    RefreshTrash;
    ShowMessage('Correo movido a la papelera');
  end
  else
    ShowMessage('Por favor selecciona un correo para eliminar');
end;

// ============================================================================
// CALLBACKS - ENVIAR CORREO
// ============================================================================

procedure TUserInterface.OnSendEmail(widget: PGtkWidget; data: gpointer); cdecl;
var
  destinatario, asunto, mensaje: String;
  buffer: PGtkTextBuffer;
  start_iter, end_iter: TGtkTextIter;
begin
  destinatario := gtk_entry_get_text(GTK_ENTRY(send_to_entry));
  asunto := gtk_entry_get_text(GTK_ENTRY(send_subject_entry));

  buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(send_message_text));
  gtk_text_buffer_get_start_iter(buffer, @start_iter);
  gtk_text_buffer_get_end_iter(buffer, @end_iter);
  mensaje := gtk_text_buffer_get_text(buffer, @start_iter, @end_iter, FALSE);

  if (destinatario = '') or (asunto = '') or (mensaje = '') then
  begin
    ShowMessage('Por favor completa todos los campos');
    Exit;
  end;

  if UserManager.SendEmail(destinatario, asunto, mensaje) then
  begin
    gtk_entry_set_text(GTK_ENTRY(send_to_entry), '');
    gtk_entry_set_text(GTK_ENTRY(send_subject_entry), '');
    gtk_text_buffer_set_text(buffer, '', 0);
    ShowMessage('Correo enviado exitosamente');
    RefreshInbox;
  end
  else
    ShowMessage('Error al enviar correo. Verifica que el destinatario esté en tus contactos');
end;

// ============================================================================
// CALLBACKS - PAPELERA
// ============================================================================

procedure TUserInterface.OnSearchTrash(widget: PGtkWidget; data: gpointer); cdecl;
var
  keyword: String;
  found_email: PDeletedEmail;
begin
  keyword := gtk_entry_get_text(GTK_ENTRY(trash_search_entry));
  if keyword = '' then
  begin
    ShowMessage('Ingresa una palabra clave para buscar');
    Exit;
  end;

  found_email := UserManager.SearchInTrash(keyword);
  if found_email <> nil then
    ShowMessage('Correo encontrado: ' + found_email^.asunto)
  else
    ShowMessage('No se encontraron correos con la palabra: ' + keyword);
end;

procedure TUserInterface.OnDeleteFromTrash(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if UserManager.DeleteFromTrash(1) then  // Elimina el último correo de la pila
  begin
    RefreshTrash;
    ShowMessage('Correo eliminado permanentemente de la papelera');
  end
  else
    ShowMessage('No hay correos en la papelera');
end;

// ============================================================================
// CALLBACKS - CORREOS PROGRAMADOS
// ============================================================================

procedure TUserInterface.OnScheduleEmail(widget: PGtkWidget; data: gpointer); cdecl;
var
  destinatario, asunto, mensaje, fecha: String;
  buffer: PGtkTextBuffer;
  start_iter, end_iter: TGtkTextIter;
begin
  destinatario := gtk_entry_get_text(GTK_ENTRY(schedule_to_entry));
  asunto := gtk_entry_get_text(GTK_ENTRY(schedule_subject_entry));
  fecha := gtk_entry_get_text(GTK_ENTRY(schedule_date_entry));

  buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(schedule_message_text));
  gtk_text_buffer_get_start_iter(buffer, @start_iter);
  gtk_text_buffer_get_end_iter(buffer, @end_iter);
  mensaje := gtk_text_buffer_get_text(buffer, @start_iter, @end_iter, FALSE);

  if (destinatario = '') or (asunto = '') or (mensaje = '') or (fecha = '') then
  begin
    ShowMessage('Por favor completa todos los campos');
    Exit;
  end;

  if UserManager.ScheduleEmail(destinatario, asunto, mensaje, fecha) then
  begin
    gtk_entry_set_text(GTK_ENTRY(schedule_to_entry), '');
    gtk_entry_set_text(GTK_ENTRY(schedule_subject_entry), '');
    gtk_entry_set_text(GTK_ENTRY(schedule_date_entry), '');
    gtk_text_buffer_set_text(buffer, '', 0);
    RefreshScheduled;
    ShowMessage('Correo programado exitosamente');
  end
  else
    ShowMessage('Error al programar correo');
end;

procedure TUserInterface.OnProcessScheduled(widget: PGtkWidget; data: gpointer); cdecl;
var
  processed: Integer;
begin
  processed := UserManager.ProcessScheduledEmails;
  RefreshScheduled;
  RefreshInbox;
  ShowMessage('Correos enviados automáticamente: ' + IntToStr(processed));
end;

// ============================================================================
// CALLBACKS - CONTACTOS
// ============================================================================

procedure TUserInterface.OnAddContact(widget: PGtkWidget; data: gpointer); cdecl;
var
  email: String;
begin
  email := gtk_entry_get_text(GTK_ENTRY(add_contact_entry));
  if email = '' then
  begin
    ShowMessage('Ingresa el email del contacto');
    Exit;
  end;

  if UserManager.AddContact(email) then
  begin
    gtk_entry_set_text(GTK_ENTRY(add_contact_entry), '');
    RefreshContacts;
    ShowMessage('Contacto agregado exitosamente');
  end
  else
    ShowMessage('Error al agregar contacto');
end;

procedure TUserInterface.OnPrevContact(widget: PGtkWidget; data: gpointer); cdecl;
begin
  UserManager.NavigateContacts(-1);
  RefreshContacts;
end;

procedure TUserInterface.OnNextContact(widget: PGtkWidget; data: gpointer); cdecl;
begin
  UserManager.NavigateContacts(1);
  RefreshContacts;
end;

// ============================================================================
// CALLBACKS - PERFIL
// ============================================================================

procedure TUserInterface.OnUpdateProfile(widget: PGtkWidget; data: gpointer); cdecl;
var
  new_username, new_phone: String;
begin
  new_username := gtk_entry_get_text(GTK_ENTRY(profile_username_entry));
  new_phone := gtk_entry_get_text(GTK_ENTRY(profile_phone_entry));

  if UserManager.UpdateProfile(new_username, new_phone) then
    ShowMessage('Perfil actualizado exitosamente')
  else
    ShowMessage('Error al actualizar perfil');
end;

// ============================================================================
// CALLBACKS - REPORTES
// ============================================================================

procedure TUserInterface.OnGenerateReports(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if UserManager.GenerateAllUserReports then
    ShowMessage('Todos los reportes generados exitosamente')
  else
    ShowMessage('Error al generar algunos reportes');
end;

// ============================================================================
// MÉTODOS AUXILIARES
// ============================================================================

procedure TUserInterface.RefreshInbox;
var
  current: PEmail;
  iter: TGtkTreeIter;
  unread_count: Integer;
  window_title: String;
begin
  gtk_list_store_clear(inbox_model);
  unread_count := 0;

  current := EmailList.head;
  while current <> nil do
  begin
    if current^.destinatario = current_user^.email then
    begin
      gtk_list_store_append(inbox_model, @iter);
      gtk_list_store_set(inbox_model, @iter,
        0, current^.id,
        1, PChar(current^.estado),
        2, PChar(current^.asunto),
        3, PChar(current^.remitente),
        4, PChar(current^.fecha),
        -1);

      if current^.estado = 'NL' then
        Inc(unread_count);
    end;
    current := current^.next;
  end;

  window_title := 'EDDMail - ' + current_user^.nombre + ' (' + IntToStr(unread_count) + ' no leídos)';
  gtk_window_set_title(GTK_WINDOW(window), PChar(window_title));
end;

procedure TUserInterface.RefreshTrash;
var
  current: PDeletedEmail;
  model: PGtkListStore;
  iter: TGtkTreeIter;
begin
  model := gtk_list_store_new(3, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING);

  current := EmailStack.top;
  while current <> nil do
  begin
    gtk_list_store_append(model, @iter);
    gtk_list_store_set(model, @iter,
      0, current^.id,
      1, PChar(current^.asunto),
      2, PChar(current^.remitente),
      -1);
    current := current^.next;
  end;

  gtk_tree_view_set_model(GTK_TREE_VIEW(trash_tree), GTK_TREE_MODEL(model));
end;

procedure TUserInterface.RefreshScheduled;
var
  current: PScheduledEmail;
  model: PGtkListStore;
  iter: TGtkTreeIter;
begin
  model := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);

  current := EmailQueue.front;
  while current <> nil do
  begin
    if current^.remitente = current_user^.email then
    begin
      gtk_list_store_append(model, @iter);
      gtk_list_store_set(model, @iter,
        0, PChar(current^.destinatario),
        1, PChar(current^.asunto),
        2, PChar(current^.fechaEnvio),
        -1);
    end;
    current := current^.next;
  end;

  gtk_tree_view_set_model(GTK_TREE_VIEW(schedule_tree), GTK_TREE_MODEL(model));
end;

procedure TUserInterface.RefreshContacts;
var
  contact: PContact;
  contact_info: String;
begin
  contact := UserManager.GetCurrentContact;
  if contact <> nil then
  begin
    contact_info := 'Nombre: ' + contact^.nombre + LineEnding +
                   'Usuario: ' + contact^.usuario + LineEnding +
                   'Email: ' + contact^.email + LineEnding +
                   'Teléfono: ' + contact^.telefono;
    gtk_label_set_text(GTK_LABEL(current_contact_label), PChar(contact_info));
  end
  else
    gtk_label_set_text(GTK_LABEL(current_contact_label), 'No hay contactos');
end;

procedure TUserInterface.ShowMessage(const msg: String);
var
  dialog: PGtkWidget;
begin
  dialog := gtk_message_dialog_new(
    GTK_WINDOW(window),
    GTK_DIALOG_MODAL,
    GTK_MESSAGE_INFO,
    GTK_BUTTONS_OK,
    PChar(msg)
  );

  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
end;

procedure TUserInterface.Show;
begin
  if window <> nil then
    gtk_widget_show_all(window);
end;

procedure TUserInterface.Hide;
begin
  if window <> nil then
    gtk_widget_hide(window);
end;

end.

