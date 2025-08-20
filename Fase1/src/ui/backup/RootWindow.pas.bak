unit RootWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTK2, GDK2, SystemCore, DataStructures, ReportGenerator;

type
  TRootWindow = class
  private
    FWindow: PGtkWidget;
    FLoadUsersButton: PGtkWidget;
    FUserReportButton: PGtkWidget;
    FRelationsReportButton: PGtkWidget;
    FCommunitiesButton: PGtkWidget;
    FLogoutButton: PGtkWidget;
    FStatusLabel: PGtkWidget;

    // Ventana de comunidades
    FCommunityWindow: PGtkWidget;
    FCommunityNameEntry: PGtkWidget;
    FCommunityCombo: PGtkWidget;
    FUserEmailEntry: PGtkWidget;

    procedure CreateMainWindow;
    procedure CreateCommunityWindow;
    procedure ShowMessage(const Msg: String);
    procedure UpdateCommunityCombo;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
  end;

// Callbacks para ventana principal
procedure OnLoadUsersClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnUserReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnRelationsReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnCommunitiesClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnRootLogoutClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnRootWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;

// Callbacks para ventana de comunidades
procedure OnCreateCommunityClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnAddUserToCommunityClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnCloseCommunityWindowClicked(widget: PGtkWidget; data: gpointer); cdecl;

var
  RootWin: TRootWindow;

implementation

uses
  LoginWindow;

constructor TRootWindow.Create;
begin
  CreateMainWindow;
  CreateCommunityWindow;
end;

destructor TRootWindow.Destroy;
begin
  if FWindow <> nil then
    gtk_widget_destroy(FWindow);
  if FCommunityWindow <> nil then
    gtk_widget_destroy(FCommunityWindow);
  inherited;
end;

procedure TRootWindow.CreateMainWindow;
var
  vbox, hbox: PGtkWidget;
  label_title, label_welcome: PGtkWidget;
  separator: PGtkWidget;
begin
  // Crear ventana principal
  FWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(FWindow), 'EDDMail - Administrador');
  gtk_window_set_default_size(GTK_WINDOW(FWindow), 500, 400);
  gtk_window_set_position(GTK_WINDOW(FWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_resizable(GTK_WINDOW(FWindow), False);

  // Conectar señal de destrucción
  g_signal_connect(G_OBJECT(FWindow), 'destroy', G_CALLBACK(@OnRootWindowDestroy), nil);

  // VBox principal
  vbox := gtk_vbox_new(False, 15);
  gtk_container_add(GTK_CONTAINER(FWindow), vbox);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 20);

  // Título
  label_title := gtk_label_new(nil);
  gtk_label_set_markup(GTK_LABEL(label_title), '<big><b>Panel de Administración</b></big>');
  gtk_misc_set_alignment(GTK_MISC(label_title), 0.5, 0.5);
  gtk_box_pack_start(GTK_BOX(vbox), label_title, False, False, 0);

  // Mensaje de bienvenida
  label_welcome := gtk_label_new('Bienvenido, Root Administrator');
  gtk_misc_set_alignment(GTK_MISC(label_welcome), 0.5, 0.5);
  gtk_box_pack_start(GTK_BOX(vbox), label_welcome, False, False, 0);

  // Separador
  separator := gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(vbox), separator, False, False, 10);

  // Botón Carga Masiva
  FLoadUsersButton := gtk_button_new_with_label('Carga Masiva de Usuarios');
  gtk_widget_set_size_request(FLoadUsersButton, 300, 40);
  gtk_box_pack_start(GTK_BOX(vbox), FLoadUsersButton, False, False, 0);
  g_signal_connect(G_OBJECT(FLoadUsersButton), 'clicked', G_CALLBACK(@OnLoadUsersClicked), Self);

  // Botón Reporte de Usuarios
  FUserReportButton := gtk_button_new_with_label('Generar Reporte de Usuarios');
  gtk_widget_set_size_request(FUserReportButton, 300, 40);
  gtk_box_pack_start(GTK_BOX(vbox), FUserReportButton, False, False, 0);
  g_signal_connect(G_OBJECT(FUserReportButton), 'clicked', G_CALLBACK(@OnUserReportClicked), Self);

  // Botón Reporte de Relaciones
  FRelationsReportButton := gtk_button_new_with_label('Generar Reporte de Relaciones');
  gtk_widget_set_size_request(FRelationsReportButton, 300, 40);
  gtk_box_pack_start(GTK_BOX(vbox), FRelationsReportButton, False, False, 0);
  g_signal_connect(G_OBJECT(FRelationsReportButton), 'clicked', G_CALLBACK(@OnRelationsReportClicked), Self);

  // Botón Comunidades
  FCommunitiesButton := gtk_button_new_with_label('Gestionar Comunidades');
  gtk_widget_set_size_request(FCommunitiesButton, 300, 40);
  gtk_box_pack_start(GTK_BOX(vbox), FCommunitiesButton, False, False, 0);
  g_signal_connect(G_OBJECT(FCommunitiesButton), 'clicked', G_CALLBACK(@OnCommunitiesClicked), Self);

  // Separador
  separator := gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(vbox), separator, False, False, 10);

  // Botón Cerrar Sesión
  FLogoutButton := gtk_button_new_with_label('Cerrar Sesión');
  gtk_widget_set_size_request(FLogoutButton, 150, 30);
  hbox := gtk_hbox_new(False, 0);
  gtk_box_pack_start(GTK_BOX(hbox), FLogoutButton, False, False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);
  g_signal_connect(G_OBJECT(FLogoutButton), 'clicked', G_CALLBACK(@OnRootLogoutClicked), Self);

  // Label de estado
  FStatusLabel := gtk_label_new('');
  gtk_box_pack_start(GTK_BOX(vbox), FStatusLabel, False, False, 0);
end;

procedure TRootWindow.CreateCommunityWindow;
var
  vbox, hbox, table: PGtkWidget;
  label_title, label_name, label_community, label_email: PGtkWidget;
  create_button, add_user_button, close_button: PGtkWidget;
begin
  // Crear ventana de comunidades
  FCommunityWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(FCommunityWindow), 'Gestión de Comunidades');
  gtk_window_set_default_size(GTK_WINDOW(FCommunityWindow), 400, 350);
  gtk_window_set_position(GTK_WINDOW(FCommunityWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_resizable(GTK_WINDOW(FCommunityWindow), False);
  gtk_window_set_modal(GTK_WINDOW(FCommunityWindow), True);

  // VBox principal
  vbox := gtk_vbox_new(False, 15);
  gtk_container_add(GTK_CONTAINER(FCommunityWindow), vbox);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 20);

  // Título
  label_title := gtk_label_new(nil);
  gtk_label_set_markup(GTK_LABEL(label_title), '<big><b>Gestión de Comunidades</b></big>');
  gtk_misc_set_alignment(GTK_MISC(label_title), 0.5, 0.5);
  gtk_box_pack_start(GTK_BOX(vbox), label_title, False, False, 0);

  // Sección: Crear Comunidad
  label_name := gtk_label_new(nil);
  gtk_label_set_markup(GTK_LABEL(label_name), '<b>Crear Nueva Comunidad</b>');
  gtk_misc_set_alignment(GTK_MISC(label_name), 0.0, 0.5);
  gtk_box_pack_start(GTK_BOX(vbox), label_name, False, False, 0);

  hbox := gtk_hbox_new(False, 10);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new('Nombre:'), False, False, 0);
  FCommunityNameEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FCommunityNameEntry), 100);
  gtk_box_pack_start(GTK_BOX(hbox), FCommunityNameEntry, True, True, 0);

  create_button := gtk_button_new_with_label('Crear');
  gtk_box_pack_start(GTK_BOX(hbox), create_button, False, False, 0);
  g_signal_connect(G_OBJECT(create_button), 'clicked', G_CALLBACK(@OnCreateCommunityClicked), Self);

  // Separador
  gtk_box_pack_start(GTK_BOX(vbox), gtk_hseparator_new(), False, False, 10);

  // Sección: Agregar Usuario a Comunidad
  label_community := gtk_label_new(nil);
  gtk_label_set_markup(GTK_LABEL(label_community), '<b>Agregar Usuario a Comunidad</b>');
  gtk_misc_set_alignment(GTK_MISC(label_community), 0.0, 0.5);
  gtk_box_pack_start(GTK_BOX(vbox), label_community, False, False, 0);

  table := gtk_table_new(2, 2, False);
  gtk_table_set_row_spacings(GTK_TABLE(table), 10);
  gtk_table_set_col_spacings(GTK_TABLE(table), 10);
  gtk_box_pack_start(GTK_BOX(vbox), table, False, False, 0);

  // ComboBox de comunidades
  gtk_table_attach(GTK_TABLE(table), gtk_label_new('Comunidad:'), 0, 1, 0, 1,
                   GTK_FILL, GTK_FILL, 0, 0);
  FCommunityCombo := gtk_combo_box_new_text();
  gtk_table_attach(GTK_TABLE(table), FCommunityCombo, 1, 2, 0, 1,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // Entry para email del usuario
  label_email := gtk_label_new('Email Usuario:');
  gtk_table_attach(GTK_TABLE(table), label_email, 0, 1, 1, 2,
                   GTK_FILL, GTK_FILL, 0, 0);
  FUserEmailEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FUserEmailEntry), 100);
  gtk_table_attach(GTK_TABLE(table), FUserEmailEntry, 1, 2, 1, 2,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  add_user_button := gtk_button_new_with_label('Agregar Usuario');
  gtk_box_pack_start(GTK_BOX(vbox), add_user_button, False, False, 0);
  g_signal_connect(G_OBJECT(add_user_button), 'clicked', G_CALLBACK(@OnAddUserToCommunityClicked), Self);

  // Botón Cerrar
  hbox := gtk_hbox_new(False, 0);
  close_button := gtk_button_new_with_label('Cerrar');
  gtk_box_pack_start(GTK_BOX(hbox), close_button, False, False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 20);
  g_signal_connect(G_OBJECT(close_button), 'clicked', G_CALLBACK(@OnCloseCommunityWindowClicked), Self);
end;

procedure TRootWindow.ShowMessage(const Msg: String);
begin
  gtk_label_set_text(GTK_LABEL(FStatusLabel), PChar(Msg));
end;

procedure TRootWindow.UpdateCommunityCombo;
var
  Community: PCommunity;
begin
  // Limpiar combo
  while gtk_combo_box_get_active(GTK_COMBO_BOX(FCommunityCombo)) >= 0 do
    gtk_combo_box_remove_text(GTK_COMBO_BOX(FCommunityCombo), 0);

  // Agregar comunidades
  Community := SystemCore.GetCommunities.GetFirst;
  while Community <> nil do
  begin
    gtk_combo_box_append_text(GTK_COMBO_BOX(FCommunityCombo), PChar(Community^.Nombre));
    Community := Community^.Next;
  end;

  if SystemCore.GetCommunities.GetCount > 0 then
    gtk_combo_box_set_active(GTK_COMBO_BOX(FCommunityCombo), 0);
end;

procedure TRootWindow.Show;
begin
  if FWindow <> nil then
    gtk_widget_show_all(FWindow);
end;

procedure TRootWindow.Hide;
begin
  if FWindow <> nil then
    gtk_widget_hide(FWindow);
end;

// ============================================================================
// Callbacks para ventana principal
// ============================================================================

procedure OnLoadUsersClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
  dialog: PGtkWidget;
  filename: PChar;
begin
  RootWin := TRootWindow(data);

  dialog := gtk_file_chooser_dialog_new('Seleccionar archivo JSON',
                                       GTK_WINDOW(RootWin.FWindow),
                                       GTK_FILE_CHOOSER_ACTION_OPEN,
                                       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                       GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
                                       nil);

  // Filtro para archivos JSON
  var filter: PGtkFileFilter;
  filter := gtk_file_filter_new();
  gtk_file_filter_set_name(filter, 'Archivos JSON (*.json)');
  gtk_file_filter_add_pattern(filter, '*.json');
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), filter);

  if gtk_dialog_run(GTK_DIALOG(dialog)) = GTK_RESPONSE_ACCEPT then
  begin
    filename := gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));

    if SystemCore.LoadUsersFromJSON(StrPas(filename)) then
      RootWin.ShowMessage('Usuarios cargados exitosamente.')
    else
      RootWin.ShowMessage('Error al cargar usuarios del archivo.');

    g_free(filename);
  end;

  gtk_widget_destroy(dialog);
end;

procedure OnUserReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
  dialog: PGtkWidget;
begin
  RootWin := TRootWindow(data);

  if ReportGenerator.GenerateUserReport then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(RootWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    'Reporte de usuarios generado en Root-Reportes/');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    RootWin.ShowMessage('Reporte de usuarios generado exitosamente.');
  end
  else
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(RootWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Error al generar el reporte de usuarios.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    RootWin.ShowMessage('Error al generar reporte.');
  end;
end;

procedure OnRelationsReportClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
  dialog: PGtkWidget;
begin
  RootWin := TRootWindow(data);

  if ReportGenerator.GenerateRelationsReport then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(RootWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    'Reporte de relaciones generado en Root-Reportes/');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    RootWin.ShowMessage('Reporte de relaciones generado exitosamente.');
  end
  else
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(RootWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Error al generar el reporte de relaciones.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    RootWin.ShowMessage('Error al generar reporte.');
  end;
end;

procedure OnCommunitiesClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
begin
  RootWin := TRootWindow(data);
  RootWin.UpdateCommunityCombo;
  gtk_widget_show_all(RootWin.FCommunityWindow);
end;

procedure OnRootLogoutClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
begin
  RootWin := TRootWindow(data);
  SystemCore.LogoutUser;
  RootWin.Hide;

  if LoginWin <> nil then
    LoginWin.Show;
end;

procedure OnRootWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_main_quit;
end;

// ============================================================================
// Callbacks para ventana de comunidades
// ============================================================================

procedure OnCreateCommunityClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
  Nombre: String;
  dialog: PGtkWidget;
begin
  RootWin := TRootWindow(data);

  Nombre := gtk_entry_get_text(GTK_ENTRY(RootWin.FCommunityNameEntry));

  if Nombre = '' then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(RootWin.FCommunityWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Por favor ingrese un nombre para la comunidad.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    Exit;
  end;

  if SystemCore.CreateCommunity(Nombre) then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(RootWin.FCommunityWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    'Comunidad creada exitosamente.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);

    gtk_entry_set_text(GTK_ENTRY(RootWin.FCommunityNameEntry), '');
    RootWin.UpdateCommunityCombo;
  end
  else
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(RootWin.FCommunityWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Error al crear la comunidad.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
  end;
end;

procedure OnAddUserToCommunityClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
  UserEmail: String;
  CommunityIndex: Integer;
  Community: PCommunity;
  CurrentCommunity: PCommunity;
  dialog: PGtkWidget;
  i: Integer;
begin
  RootWin := TRootWindow(data);

  UserEmail := gtk_entry_get_text(GTK_ENTRY(RootWin.FUserEmailEntry));
  CommunityIndex := gtk_combo_box_get_active(GTK_COMBO_BOX(RootWin.FCommunityCombo));

  if UserEmail = '' then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(RootWin.FCommunityWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Por favor ingrese el email del usuario.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    Exit;
  end;

  if CommunityIndex < 0 then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(RootWin.FCommunityWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Por favor seleccione una comunidad.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    Exit;
  end;

  // Obtener la comunidad seleccionada
  CurrentCommunity := SystemCore.GetCommunities.GetFirst;
  for i := 0 to CommunityIndex - 1 do
  begin
    if CurrentCommunity <> nil then
      CurrentCommunity := CurrentCommunity^.Next;
  end;

  if CurrentCommunity <> nil then
  begin
    if SystemCore.AddUserToCommunity(CurrentCommunity^.Id, UserEmail) then
    begin
      dialog := gtk_message_dialog_new(GTK_WINDOW(RootWin.FCommunityWindow),
                                      GTK_DIALOG_MODAL,
                                      GTK_MESSAGE_INFO,
                                      GTK_BUTTONS_OK,
                                      'Usuario agregado a la comunidad exitosamente.');
      gtk_dialog_run(GTK_DIALOG(dialog));
      gtk_widget_destroy(dialog);

      gtk_entry_set_text(GTK_ENTRY(RootWin.FUserEmailEntry), '');
    end
    else
    begin
      dialog := gtk_message_dialog_new(GTK_WINDOW(RootWin.FCommunityWindow),
                                      GTK_DIALOG_MODAL,
                                      GTK_MESSAGE_ERROR,
                                      GTK_BUTTONS_OK,
                                      'Error: El usuario no existe o ya está en la comunidad.');
      gtk_dialog_run(GTK_DIALOG(dialog));
      gtk_widget_destroy(dialog);
    end;
  end;
end;

procedure OnCloseCommunityWindowClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  RootWin: TRootWindow;
begin
  RootWin := TRootWindow(data);
  gtk_widget_hide(RootWin.FCommunityWindow);
end;

end.

