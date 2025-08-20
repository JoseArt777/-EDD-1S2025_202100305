procedure TUserWindow.RefreshAll;
begin
  RefreshInbox;
  RefreshTrash;
  RefreshScheduled;
  RefreshContacts;
  LoadProfileData;
end;

// ============================================================================
// Callbacks
// ============================================================================

procedure OnUserWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_main_quit;
end;

procedure OnUserLogoutClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
begin
  UserWin := TUserWindow(data);
  SystemCore.LogoutUser;
  UserWin.Hide;

  if LoginWin <> nil then
    LoginWin.Show;
end;

// Inbox callbacks
procedure OnSortEmailsClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  user: PUser;
  inbox: TEmailList;
begin
  UserWin := TUserWindow(data);
  user := SystemCore.GetCurrentUser;
  if user = nil then Exit;

  inbox := SystemCore.GetUserInbox(user^.Email);
  if inbox <> nil then
  begin
    inbox.SortBySubject;
    UserWin.RefreshInbox;
    UserWin.ShowMessage('Correos ordenados alfabéticamente.');
  end;
end;

procedure OnDeleteEmailClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  selection: PGtkTreeSelection;
  model: PGtkTreeModel;
  iter: TGtkTreeIter;
  email_id: Integer;
  value: TGValue;
begin
  UserWin := TUserWindow(data);
  selection := gtk_tree_view_get_selection(GTK_TREE_VIEW(UserWin.FInboxTreeView));

  if gtk_tree_selection_get_selected(selection, @model, @iter) then
  begin
    FillChar(value, SizeOf(value), 0);
    gtk_tree_model_get_value(model, @iter, 0, @value);
    email_id := g_value_get_int(@value);
    g_value_unset(@value);

    SystemCore.DeleteEmail(email_id);
    UserWin.RefreshInbox;
    UserWin.RefreshTrash;
    UserWin.ShowMessage('Correo movido a la papelera.');
  end
  else
  begin
    UserWin.ShowMessage('Seleccione un correo para eliminar.');
  end;
end;

procedure OnInboxRowActivated(tree_view: PGtkTreeView; path: PGtkTreePath;
                              column: PGtkTreeViewColumn; user_data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  model: PGtkTreeModel;
  iter: TGtkTreeIter;
  email_id: Integer;
  value: TGValue;
  user: PUser;
  inbox: TEmailList;
  email: PEmail;
  dialog: PGtkWidget;
  message_text: String;
begin
  UserWin := TUserWindow(user_data);
  model := gtk_tree_view_get_model(tree_view);

  if gtk_tree_model_get_iter(model, @iter, path) then
  begin
    FillChar(value, SizeOf(value), 0);
    gtk_tree_model_get_value(model, @iter, 0, @value);
    email_id := g_value_get_int(@value);
    g_value_unset(@value);

    user := SystemCore.GetCurrentUser;
    if user <> nil then
    begin
      inbox := SystemCore.GetUserInbox(user^.Email);
      if inbox <> nil then
      begin
        email := inbox.Find(email_id);
        if email <> nil then
        begin
          // Marcar como leído
          SystemCore.MarkEmailAsRead(email_id);

          // Mostrar contenido del mensaje
          message_text := 'De: ' + email^.Remitente + #10#10 +
                         'Asunto: ' + email^.Asunto + #10#10 +
                         'Mensaje:' + #10 + email^.Mensaje;

          dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                          GTK_DIALOG_MODAL,
                                          GTK_MESSAGE_INFO,
                                          GTK_BUTTONS_OK,
                                          PChar(message_text));
          gtk_dialog_run(GTK_DIALOG(dialog));
          gtk_widget_destroy(dialog);

          UserWin.RefreshInbox;
        end;
      end;
    end;
  end;
end;

// Compose callbacks
procedure OnSendEmailClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  destinatario, asunto, mensaje: String;
  buffer: PGtkTextBuffer;
  start_iter, end_iter: TGtkTextIter;
  dialog: PGtkWidget;
begin
  UserWin := TUserWindow(data);

  destinatario := gtk_entry_get_text(GTK_ENTRY(UserWin.FComposeToEntry));
  asunto := gtk_entry_get_text(GTK_ENTRY(UserWin.FComposeSubjectEntry));

  buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(UserWin.FComposeMessageText));
  gtk_text_buffer_get_start_iter(buffer, @start_iter);
  gtk_text_buffer_get_end_iter(buffer, @end_iter);
  mensaje := gtk_text_buffer_get_text(buffer, @start_iter, @end_iter, False);

  if (destinatario = '') or (asunto = '') or (mensaje = '') then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Por favor complete todos los campos.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    Exit;
  end;

  if SystemCore.SendEmail(destinatario, asunto, mensaje) then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    'Correo enviado exitosamente.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);

    // Limpiar campos
    gtk_entry_set_text(GTK_ENTRY(UserWin.FComposeToEntry), '');
    gtk_entry_set_text(GTK_ENTRY(UserWin.FComposeSubjectEntry), '');
    gtk_text_buffer_set_text(buffer, '', 0);

    UserWin.ShowMessage('Correo enviado exitosamente.');
  end
  else
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Error: El destinatario no existe o no está en sus contactos.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
  end;
end;

// Trash callbacks
procedure OnSearchTrashClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  keyword: String;
  user: PUser;
  trash: TEmailStack;
  email: PEmail;
  dialog: PGtkWidget;
  message_text: String;
begin
  UserWin := TUserWindow(data);
  keyword := gtk_entry_get_text(GTK_ENTRY(UserWin.FSearchEntry));

  if keyword = '' then
  begin
    UserWin.ShowMessage('Ingrese una palabra clave para buscar.');
    Exit;
  end;

  user := SystemCore.GetCurrentUser;
  if user <> nil then
  begin
    trash := SystemCore.GetUserTrash(user^.Email);
    if trash <> nil then
    begin
      email := trash.Search(keyword);
      if email <> nil then
      begin
        message_text := 'Correo encontrado:' + #10#10 +
                       'De: ' + email^.Remitente + #10 +
                       'Asunto: ' + email^.Asunto + #10 +
                       'Mensaje: ' + email^.Mensaje;

        dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                        GTK_DIALOG_MODAL,
                                        GTK_MESSAGE_INFO,
                                        GTK_BUTTONS_OK,
                                        PChar(message_text));
        gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
      end
      else
      begin
        UserWin.ShowMessage('No se encontraron correos con esa palabra clave.');
      end;
    end;
  end;
end;

procedure OnDeletePermanentClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  user: PUser;
  trash: TEmailStack;
  email: PEmail;
  dialog: PGtkWidget;
  response: Integer;
begin
  UserWin := TUserWindow(data);

  dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                  GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_QUESTION,
                                  GTK_BUTTONS_YES_NO,
                                  '¿Está seguro de eliminar permanentemente todos los correos de la papelera?');
  response := gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);

  if response = GTK_RESPONSE_YES then
  begin
    user := SystemCore.GetCurrentUser;
    if user <> nil then
    begin
      trash := SystemCore.GetUserTrash(user^.Email);
      if trash <> nil then
      begin
        while not trash.IsEmpty do
        begin
          email := trash.Pop;
          if email <> nil then
            Dispose(email);
        end;
        UserWin.RefreshTrash;
        UserWin.ShowMessage('Papelera vaciada exitosamente.');
      end;
    end;
  end;
end;

// Schedule callbacks
procedure OnScheduleEmailClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  destinatario, asunto, mensaje, fecha_str, hora_str: String;
  buffer: PGtkTextBuffer;
  start_iter, end_iter: TGtkTextIter;
  fecha_hora: TDateTime;
  dialog: PGtkWidget;
  fecha_parts, hora_parts: TStringList;
  dia, mes, anio, hora, minuto: Integer;
begin
  UserWin := TUserWindow(data);

  destinatario := gtk_entry_get_text(GTK_ENTRY(UserWin.FScheduleToEntry));
  asunto := gtk_entry_get_text(GTK_ENTRY(UserWin.FScheduleSubjectEntry));
  fecha_str := gtk_entry_get_text(GTK_ENTRY(UserWin.FScheduleDateEntry));
  hora_str := gtk_entry_get_text(GTK_ENTRY(UserWin.FScheduleTimeEntry));

  buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(UserWin.FScheduleMessageText));
  gtk_text_buffer_get_start_iter(buffer, @start_iter);
  gtk_text_buffer_get_end_iter(buffer, @end_iter);
  mensaje := gtk_text_buffer_get_text(buffer, @start_iter, @end_iter, False);

  if (destinatario = '') or (asunto = '') or (mensaje = '') or
     (fecha_str = '') or (hora_str = '') then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Por favor complete todos los campos.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    Exit;
  end;

  try
    // Parsear fecha (DD/MM/YYYY)
    fecha_parts := TStringList.Create;
    try
      fecha_parts.Delimiter := '/';
      fecha_parts.DelimitedText := fecha_str;
      if fecha_parts.Count <> 3 then raise Exception.Create('Formato de fecha inválido');

      dia := StrToInt(fecha_parts[0]);
      mes := StrToInt(fecha_parts[1]);
      anio := StrToInt(fecha_parts[2]);
    finally
      fecha_parts.Free;
    end;

    // Parsear hora (HH:MM)
    hora_parts := TStringList.Create;
    try
      hora_parts.Delimiter := ':';
      hora_parts.DelimitedText := hora_str;
      if hora_parts.Count <> 2 then raise Exception.Create('Formato de hora inválido');

      hora := StrToInt(hora_parts[0]);
      minuto := StrToInt(hora_parts[1]);
    finally
      hora_parts.Free;
    end;

    fecha_hora := EncodeDateTime(anio, mes, dia, hora, minuto, 0, 0);

    if fecha_hora <= Now then
    begin
      dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                      GTK_DIALOG_MODAL,
                                      GTK_MESSAGE_ERROR,
                                      GTK_BUTTONS_OK,
                                      'La fecha y hora deben ser futuras.');
      gtk_dialog_run(GTK_DIALOG(dialog));
      gtk_widget_destroy(dialog);
      Exit;
    end;

    if SystemCore.ScheduleEmail(destinatario, asunto, mensaje, fecha_hora) then
    begin
      dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                      GTK_DIALOG_MODAL,
                                      GTK_MESSAGE_INFO,
                                      GTK_BUTTONS_OK,
                                      'Correo programado exitosamente.');
      gtk_dialog_run(GTK_DIALOG(dialog));
      gtk_widget_destroy(dialog);

      // Limpiar campos
      gtk_entry_set_text(GTK_ENTRY(UserWin.FScheduleToEntry), '');
      gtk_entry_set_text(GTK_ENTRY(UserWin.FScheduleSubjectEntry), '');
      gtk_text_buffer_set_text(buffer, '', 0);
      gtk_entry_set_text(GTK_ENTRY(UserWin.FScheduleDateEntry), FormatDateTime('dd/mm/yyyy', Now));
      gtk_entry_set_text(GTK_ENTRY(UserWin.FScheduleTimeEntry), FormatDateTime('hh:nn', Now + 1/24));

      UserWin.RefreshScheduled;
      UserWin.ShowMessage('Correo programado exitosamente.');
    end
    else
    begin
      dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                      GTK_DIALOG_MODAL,
                                      GTK_MESSAGE_ERROR,
                                      GTK_BUTTONS_OK,
                                      'Error: El destinatario no existe o no está en sus contactos.');
      gtk_dialog_run(GTK_DIALOG(dialog));
      gtk_widget_destroy(dialog);
    end;

  except
    on E: Exception do
    begin
      dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                      GTK_DIALOG_MODAL,
                                      GTK_MESSAGE_ERROR,
                                      GTK_BUTTONS_OK,
                                      'Error en formato de fecha/hora. Use DD/MM/YYYY y HH:MM');
      gtk_dialog_run(GTK_DIALOG(dialog));
      gtk_widget_destroy(dialog);
    end;
  end;
end;

// Scheduled callbacks
procedure OnProcessScheduledClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  processed: Integer;
  dialog: PGtkWidget;
  msg: String;
begin
  UserWin := TUserWindow(data);

  processed := SystemCore.ProcessScheduledEmails;

  if processed > 0 then
  begin
    msg := 'Se procesaron ' + IntToStr(processed) + ' correos programados.';
    UserWin.RefreshScheduled;
    UserWin.RefreshInbox; // Actualizar inbox para mostrar nuevos correos
  end
  else
  begin
    msg := 'No hay correos programados listos para enviar.';
  end;

  dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                  GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_INFO,
                                  GTK_BUTTONS_OK,
                                  PChar(msg));
  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);

  UserWin.ShowMessage(msg);
end;

// Contacts callbacks
procedure OnAddContactClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  contact_email: String;
  dialog: PGtkWidget;
begin
  UserWin := TUserWindow(data);

  contact_email := gtk_entry_get_text(GTK_ENTRY(UserWin.FAddContactEntry));

  if contact_email = '' then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Por favor ingrese el email del contacto.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    Exit;
  end;

  if SystemCore.AddContact(contact_email) then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    'Contacto agregado exitosamente.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);

    gtk_entry_set_text(GTK_ENTRY(UserWin.FAddContactEntry), '');
    UserWin.RefreshContacts;
    UserWin.ShowMessage('Contacto agregado exitosamente.');
  end
  else
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Error: El usuario no existe o ya está en sus contactos.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
  end;
end;

procedure OnPrevContactClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  user: PUser;
  contacts: TContactList;
begin
  UserWin := TUserWindow(data);
  user := SystemCore.GetCurrentUser;
  if user = nil then Exit;

  contacts := SystemCore.GetUserContacts(user^.Email);
  if (contacts = nil) or (contacts.GetCount = 0) then Exit;

  Dec(UserWin.FCurrentContactIndex);
  if UserWin.FCurrentContactIndex < 0 then
    UserWin.FCurrentContactIndex := contacts.GetCount - 1;

  UserWin.RefreshContacts;
end;

procedure OnNextContactClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  user: PUser;
  contacts: TContactList;
begin
  UserWin := TUserWindow(data);
  user := SystemCore.GetCurrentUser;
  if user = nil then Exit;

  contacts := SystemCore.GetUserContacts(user^.Email);
  if (contacts = nil) or (contacts.GetCount = 0) then Exit;

  Inc(UserWin.FCurrentContactIndex);
  if UserWin.FCurrentContactIndex >= contacts.GetCount then
    UserWin.FCurrentContactIndex := 0;

  UserWin.RefreshContacts;
end;

// Profile callbacks
procedure OnUpdateProfileClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  nombre, usuario, telefono: String;
  dialog: PGtkWidget;
begin
  UserWin := TUserWindow(data);

  nombre := gtk_entry_get_text(GTK_ENTRY(UserWin.FProfileNombreEntry));
  usuario := gtk_entry_get_text(GTK_ENTRY(UserWin.FProfileUsuarioEntry));
  telefono := gtk_entry_get_text(GTK_ENTRY(UserWin.FProfileTelefonoEntry));

  if (nombre = '') or (usuario = '') or (telefono = '') then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Por favor complete todos los campos.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    Exit;
  end;

  if SystemCore.UpdateProfile(nombre, usuario, telefono) then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    'Perfil actualizado exitosamente.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);

    // Actualizar etiqueta de bienvenida
    var user: PUser;
    var welcome_text: String;
    user := SystemCore.GetCurrentUser;
    if user <> nil then
      welcome_text := 'Hola: ' + user^.Nombre
    else
      welcome_text := 'Hola: Usuario';
    gtk_label_set_text(GTK_LABEL(UserWin.FWelcomeLabel), PChar(welcome_text));

    UserWin.ShowMessage('Perfil actualizado exitosamente.');
  end
  else
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Error al actualizar el perfil.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
  end;
end;

// Reports callbacks
procedure OnGenerateReportsClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UserWin: TUserWindow;
  user: PUser;
  success: Boolean;
  dialog: PGtkWidget;
begin
  UserWin := TUserWindow(data);
  user := SystemCore.GetCurrentUser;
  if user = nil then Exit;

  success := ReportGenerator.GenerateUserReports(user^.Usuario);

  if success then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    PChar('Reportes generados en la carpeta ' + user^.Usuario + '-Reportes/'));
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    UserWin.ShowMessage('Reportes generados exitosamente.');
  end
  else
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(UserWin.FWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Error al generar los reportes.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    UserWin.ShowMessage('Error al generar reportes.');
  end;
end;

end.unit UserWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTK2, GDK2, SystemCore, DataStructures;

type
  TUserWindow = class
  private
    FWindow: PGtkWidget;
    FNotebook: PGtkWidget;
    FStatusLabel: PGtkWidget;
    FWelcomeLabel: PGtkWidget;

    // Páginas del notebook
    FInboxPage: PGtkWidget;
    FComposePage: PGtkWidget;
    FTrashPage: PGtkWidget;
    FSchedulePage: PGtkWidget;
    FScheduledPage: PGtkWidget;
    FContactsPage: PGtkWidget;
    FProfilePage: PGtkWidget;
    FReportsPage: PGtkWidget;

    // Widgets de Inbox
    FInboxTreeView: PGtkWidget;
    FInboxListStore: PGtkListStore;
    FSortButton: PGtkWidget;
    FUnreadLabel: PGtkWidget;
    FDeleteEmailButton: PGtkWidget;

    // Widgets de Compose
    FComposeToEntry: PGtkWidget;
    FComposeSubjectEntry: PGtkWidget;
    FComposeMessageText: PGtkWidget;
    FSendButton: PGtkWidget;

    // Widgets de Trash
    FTrashTreeView: PGtkWidget;
    FTrashListStore: PGtkListStore;
    FSearchEntry: PGtkWidget;
    FSearchButton: PGtkWidget;
    FDeletePermanentButton: PGtkWidget;

    // Widgets de Schedule
    FScheduleToEntry: PGtkWidget;
    FScheduleSubjectEntry: PGtkWidget;
    FScheduleMessageText: PGtkWidget;
    FScheduleDateEntry: PGtkWidget;
    FScheduleTimeEntry: PGtkWidget;
    FScheduleButton: PGtkWidget;

    // Widgets de Scheduled Emails
    FScheduledTreeView: PGtkWidget;
    FScheduledListStore: PGtkListStore;
    FProcessScheduledButton: PGtkWidget;

    // Widgets de Contacts
    FContactTreeView: PGtkWidget;
    FContactListStore: PGtkListStore;
    FAddContactEntry: PGtkWidget;
    FAddContactButton: PGtkWidget;
    FPrevContactButton: PGtkWidget;
    FNextContactButton: PGtkWidget;
    FCurrentContactIndex: Integer;

    // Widgets de Profile
    FProfileNombreEntry: PGtkWidget;
    FProfileUsuarioEntry: PGtkWidget;
    FProfileTelefonoEntry: PGtkWidget;
    FUpdateProfileButton: PGtkWidget;

    // Widgets de Reports
    FGenerateReportsButton: PGtkWidget;

    // Logout
    FLogoutButton: PGtkWidget;

    procedure CreateMainWindow;
    procedure CreateInboxPage;
    procedure CreateComposePage;
    procedure CreateTrashPage;
    procedure CreateSchedulePage;
    procedure CreateScheduledPage;
    procedure CreateContactsPage;
    procedure CreateProfilePage;
    procedure CreateReportsPage;

    procedure RefreshInbox;
    procedure RefreshTrash;
    procedure RefreshScheduled;
    procedure RefreshContacts;
    procedure LoadProfileData;
    procedure ShowMessage(const Msg: String);
    procedure UpdateUnreadCount;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
    procedure RefreshAll;
  end;

// Callbacks
procedure OnUserWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnUserLogoutClicked(widget: PGtkWidget; data: gpointer); cdecl;

// Inbox callbacks
procedure OnSortEmailsClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnDeleteEmailClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnInboxRowActivated(tree_view: PGtkTreeView; path: PGtkTreePath;
                              column: PGtkTreeViewColumn; user_data: gpointer); cdecl;

// Compose callbacks
procedure OnSendEmailClicked(widget: PGtkWidget; data: gpointer); cdecl;

// Trash callbacks
procedure OnSearchTrashClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnDeletePermanentClicked(widget: PGtkWidget; data: gpointer); cdecl;

// Schedule callbacks
procedure OnScheduleEmailClicked(widget: PGtkWidget; data: gpointer); cdecl;

// Scheduled callbacks
procedure OnProcessScheduledClicked(widget: PGtkWidget; data: gpointer); cdecl;

// Contacts callbacks
procedure OnAddContactClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnPrevContactClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnNextContactClicked(widget: PGtkWidget; data: gpointer); cdecl;

// Profile callbacks
procedure OnUpdateProfileClicked(widget: PGtkWidget; data: gpointer); cdecl;

// Reports callbacks
procedure OnGenerateReportsClicked(widget: PGtkWidget; data: gpointer); cdecl;

var
  UserWin: TUserWindow;

implementation

uses
  LoginWindow, ReportGenerator, DateUtils;

constructor TUserWindow.Create;
begin
  FCurrentContactIndex := 0;
  CreateMainWindow;
end;

destructor TUserWindow.Destroy;
begin
  if FWindow <> nil then
    gtk_widget_destroy(FWindow);
  inherited;
end;

procedure TUserWindow.CreateMainWindow;
var
  vbox, hbox: PGtkWidget;
  user: PUser;
  welcome_text: String;
begin
  // Crear ventana principal
  FWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(FWindow), 'EDDMail - Usuario');
  gtk_window_set_default_size(GTK_WINDOW(FWindow), 800, 600);
  gtk_window_set_position(GTK_WINDOW(FWindow), GTK_WIN_POS_CENTER);

  // Conectar señal de destrucción
  g_signal_connect(G_OBJECT(FWindow), 'destroy', G_CALLBACK(@OnUserWindowDestroy), nil);

  // VBox principal
  vbox := gtk_vbox_new(False, 10);
  gtk_container_add(GTK_CONTAINER(FWindow), vbox);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Header con bienvenida y logout
  hbox := gtk_hbox_new(False, 10);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  user := SystemCore.GetCurrentUser;
  if user <> nil then
    welcome_text := 'Hola: ' + user^.Nombre
  else
    welcome_text := 'Hola: Usuario';

  FWelcomeLabel := gtk_label_new(PChar(welcome_text));
  gtk_misc_set_alignment(GTK_MISC(FWelcomeLabel), 0.0, 0.5);
  gtk_box_pack_start(GTK_BOX(hbox), FWelcomeLabel, True, True, 0);

  FLogoutButton := gtk_button_new_with_label('Cerrar Sesión');
  gtk_box_pack_start(GTK_BOX(hbox), FLogoutButton, False, False, 0);
  g_signal_connect(G_OBJECT(FLogoutButton), 'clicked', G_CALLBACK(@OnUserLogoutClicked), Self);

  // Notebook para las diferentes secciones
  FNotebook := gtk_notebook_new();
  gtk_box_pack_start(GTK_BOX(vbox), FNotebook, True, True, 0);

  // Crear páginas
  CreateInboxPage;
  CreateComposePage;
  CreateTrashPage;
  CreateSchedulePage;
  CreateScheduledPage;
  CreateContactsPage;
  CreateProfilePage;
  CreateReportsPage;

  // Label de estado
  FStatusLabel := gtk_label_new('');
  gtk_box_pack_start(GTK_BOX(vbox), FStatusLabel, False, False, 0);
end;

procedure TUserWindow.CreateInboxPage;
var
  vbox, hbox: PGtkWidget;
  scrolled: PGtkWidget;
  renderer: PGtkCellRenderer;
  column: PGtkTreeViewColumn;
begin
  vbox := gtk_vbox_new(False, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Header con botones
  hbox := gtk_hbox_new(False, 10);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  FSortButton := gtk_button_new_with_label('Ordenar A-Z');
  gtk_box_pack_start(GTK_BOX(hbox), FSortButton, False, False, 0);
  g_signal_connect(G_OBJECT(FSortButton), 'clicked', G_CALLBACK(@OnSortEmailsClicked), Self);

  FUnreadLabel := gtk_label_new('No leídos: 0');
  gtk_box_pack_start(GTK_BOX(hbox), FUnreadLabel, False, False, 0);

  FDeleteEmailButton := gtk_button_new_with_label('Eliminar');
  gtk_box_pack_start(GTK_BOX(hbox), FDeleteEmailButton, False, False, 0);
  g_signal_connect(G_OBJECT(FDeleteEmailButton), 'clicked', G_CALLBACK(@OnDeleteEmailClicked), Self);

  // TreeView para emails
  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
                                GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled, True, True, 0);

  FInboxListStore := gtk_list_store_new(4, G_TYPE_INT, G_TYPE_STRING,
                                           G_TYPE_STRING, G_TYPE_STRING);
  FInboxTreeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(FInboxListStore));
  gtk_container_add(GTK_CONTAINER(scrolled), FInboxTreeView);

  // Columnas
  renderer := gtk_cell_renderer_text_new();
  column := gtk_tree_view_column_new_with_attributes('Estado', renderer, 'text', 1, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FInboxTreeView), column);

  column := gtk_tree_view_column_new_with_attributes('Asunto', renderer, 'text', 2, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FInboxTreeView), column);

  column := gtk_tree_view_column_new_with_attributes('Remitente', renderer, 'text', 3, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FInboxTreeView), column);

  g_signal_connect(G_OBJECT(FInboxTreeView), 'row-activated',
                  G_CALLBACK(@OnInboxRowActivated), Self);

  FInboxPage := vbox;
  gtk_notebook_append_page(GTK_NOTEBOOK(FNotebook), FInboxPage,
                          gtk_label_new('Bandeja de Entrada'));
end;

procedure TUserWindow.CreateComposePage;
var
  vbox, hbox, table: PGtkWidget;
  scrolled: PGtkWidget;
  label_to, label_subject, label_message: PGtkWidget;
begin
  vbox := gtk_vbox_new(False, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Tabla para campos
  table := gtk_table_new(3, 2, False);
  gtk_table_set_row_spacings(GTK_TABLE(table), 10);
  gtk_table_set_col_spacings(GTK_TABLE(table), 10);
  gtk_box_pack_start(GTK_BOX(vbox), table, False, False, 0);

  // Campo Destinatario
  label_to := gtk_label_new('Destinatario:');
  gtk_misc_set_alignment(GTK_MISC(label_to), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), label_to, 0, 1, 0, 1,
                   GTK_FILL, GTK_FILL, 0, 0);

  FComposeToEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FComposeToEntry), 100);
  gtk_table_attach(GTK_TABLE(table), FComposeToEntry, 1, 2, 0, 1,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // Campo Asunto
  label_subject := gtk_label_new('Asunto:');
  gtk_misc_set_alignment(GTK_MISC(label_subject), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), label_subject, 0, 1, 1, 2,
                   GTK_FILL, GTK_FILL, 0, 0);

  FComposeSubjectEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FComposeSubjectEntry), 200);
  gtk_table_attach(GTK_TABLE(table), FComposeSubjectEntry, 1, 2, 1, 2,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // Campo Mensaje
  label_message := gtk_label_new('Mensaje:');
  gtk_misc_set_alignment(GTK_MISC(label_message), 0.0, 0.0);
  gtk_table_attach(GTK_TABLE(table), label_message, 0, 1, 2, 3,
                   GTK_FILL, GTK_FILL, 0, 0);

  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
                                GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_set_size_request(scrolled, -1, 200);
  FComposeMessageText := gtk_text_view_new();
  gtk_container_add(GTK_CONTAINER(scrolled), FComposeMessageText);
  gtk_table_attach(GTK_TABLE(table), scrolled, 1, 2, 2, 3,
                   GTK_FILL or GTK_EXPAND, GTK_FILL or GTK_EXPAND, 0, 0);

  // Botón Enviar
  hbox := gtk_hbox_new(False, 0);
  FSendButton := gtk_button_new_with_label('Enviar');
  gtk_box_pack_start(GTK_BOX(hbox), FSendButton, False, False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);
  g_signal_connect(G_OBJECT(FSendButton), 'clicked', G_CALLBACK(@OnSendEmailClicked), Self);

  FComposePage := vbox;
  gtk_notebook_append_page(GTK_NOTEBOOK(FNotebook), FComposePage,
                          gtk_label_new('Enviar Correo'));
end;

procedure TUserWindow.CreateTrashPage;
var
  vbox, hbox: PGtkWidget;
  scrolled: PGtkWidget;
  renderer: PGtkCellRenderer;
  column: PGtkTreeViewColumn;
begin
  vbox := gtk_vbox_new(False, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Header con búsqueda
  hbox := gtk_hbox_new(False, 10);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new('Buscar:'), False, False, 0);
  FSearchEntry := gtk_entry_new();
  gtk_box_pack_start(GTK_BOX(hbox), FSearchEntry, True, True, 0);

  FSearchButton := gtk_button_new_with_label('Buscar');
  gtk_box_pack_start(GTK_BOX(hbox), FSearchButton, False, False, 0);
  g_signal_connect(G_OBJECT(FSearchButton), 'clicked', G_CALLBACK(@OnSearchTrashClicked), Self);

  FDeletePermanentButton := gtk_button_new_with_label('Eliminar Definitivamente');
  gtk_box_pack_start(GTK_BOX(hbox), FDeletePermanentButton, False, False, 0);
  g_signal_connect(G_OBJECT(FDeletePermanentButton), 'clicked',
                  G_CALLBACK(@OnDeletePermanentClicked), Self);

  // TreeView para papelera
  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
                                GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled, True, True, 0);

  FTrashListStore := gtk_list_store_new(4, G_TYPE_INT, G_TYPE_STRING,
                                           G_TYPE_STRING, G_TYPE_STRING);
  FTrashTreeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(FTrashListStore));
  gtk_container_add(GTK_CONTAINER(scrolled), FTrashTreeView);

  // Columnas
  renderer := gtk_cell_renderer_text_new();
  column := gtk_tree_view_column_new_with_attributes('Asunto', renderer, 'text', 1, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FTrashTreeView), column);

  column := gtk_tree_view_column_new_with_attributes('Remitente', renderer, 'text', 2, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FTrashTreeView), column);

  column := gtk_tree_view_column_new_with_attributes('Mensaje', renderer, 'text', 3, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FTrashTreeView), column);

  FTrashPage := vbox;
  gtk_notebook_append_page(GTK_NOTEBOOK(FNotebook), FTrashPage,
                          gtk_label_new('Papelera'));
end;

procedure TUserWindow.CreateSchedulePage;
var
  vbox, hbox, table: PGtkWidget;
  scrolled: PGtkWidget;
  label_to, label_subject, label_message, label_date, label_time: PGtkWidget;
begin
  vbox := gtk_vbox_new(False, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Tabla para campos
  table := gtk_table_new(5, 2, False);
  gtk_table_set_row_spacings(GTK_TABLE(table), 10);
  gtk_table_set_col_spacings(GTK_TABLE(table), 10);
  gtk_box_pack_start(GTK_BOX(vbox), table, False, False, 0);

  // Campo Destinatario
  label_to := gtk_label_new('Destinatario:');
  gtk_misc_set_alignment(GTK_MISC(label_to), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), label_to, 0, 1, 0, 1,
                   GTK_FILL, GTK_FILL, 0, 0);

  FScheduleToEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FScheduleToEntry), 100);
  gtk_table_attach(GTK_TABLE(table), FScheduleToEntry, 1, 2, 0, 1,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // Campo Asunto
  label_subject := gtk_label_new('Asunto:');
  gtk_misc_set_alignment(GTK_MISC(label_subject), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), label_subject, 0, 1, 1, 2,
                   GTK_FILL, GTK_FILL, 0, 0);

  FScheduleSubjectEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FScheduleSubjectEntry), 200);
  gtk_table_attach(GTK_TABLE(table), FScheduleSubjectEntry, 1, 2, 1, 2,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // Campo Fecha
  label_date := gtk_label_new('Fecha (DD/MM/YYYY):');
  gtk_misc_set_alignment(GTK_MISC(label_date), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), label_date, 0, 1, 2, 3,
                   GTK_FILL, GTK_FILL, 0, 0);

  FScheduleDateEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FScheduleDateEntry), 10);
  gtk_entry_set_text(GTK_ENTRY(FScheduleDateEntry), FormatDateTime('dd/mm/yyyy', Now));
  gtk_table_attach(GTK_TABLE(table), FScheduleDateEntry, 1, 2, 2, 3,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // Campo Hora
  label_time := gtk_label_new('Hora (HH:MM):');
  gtk_misc_set_alignment(GTK_MISC(label_time), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), label_time, 0, 1, 3, 4,
                   GTK_FILL, GTK_FILL, 0, 0);

  FScheduleTimeEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FScheduleTimeEntry), 5);
  gtk_entry_set_text(GTK_ENTRY(FScheduleTimeEntry), FormatDateTime('hh:nn', Now + 1/24));
  gtk_table_attach(GTK_TABLE(table), FScheduleTimeEntry, 1, 2, 3, 4,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // Campo Mensaje
  label_message := gtk_label_new('Mensaje:');
  gtk_misc_set_alignment(GTK_MISC(label_message), 0.0, 0.0);
  gtk_table_attach(GTK_TABLE(table), label_message, 0, 1, 4, 5,
                   GTK_FILL, GTK_FILL, 0, 0);

  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
                                GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_set_size_request(scrolled, -1, 150);
  FScheduleMessageText := gtk_text_view_new();
  gtk_container_add(GTK_CONTAINER(scrolled), FScheduleMessageText);
  gtk_table_attach(GTK_TABLE(table), scrolled, 1, 2, 4, 5,
                   GTK_FILL or GTK_EXPAND, GTK_FILL or GTK_EXPAND, 0, 0);

  // Botón Programar
  hbox := gtk_hbox_new(False, 0);
  FScheduleButton := gtk_button_new_with_label('Programar Envío');
  gtk_box_pack_start(GTK_BOX(hbox), FScheduleButton, False, False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);
  g_signal_connect(G_OBJECT(FScheduleButton), 'clicked', G_CALLBACK(@OnScheduleEmailClicked), Self);

  FSchedulePage := vbox;
  gtk_notebook_append_page(GTK_NOTEBOOK(FNotebook), FSchedulePage,
                          gtk_label_new('Programar Correo'));
end;

procedure TUserWindow.CreateScheduledPage;
var
  vbox, hbox: PGtkWidget;
  scrolled: PGtkWidget;
  renderer: PGtkCellRenderer;
  column: PGtkTreeViewColumn;
begin
  vbox := gtk_vbox_new(False, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Header con botón de procesamiento
  hbox := gtk_hbox_new(False, 10);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  FProcessScheduledButton := gtk_button_new_with_label('Procesar Correos Programados');
  gtk_box_pack_start(GTK_BOX(hbox), FProcessScheduledButton, False, False, 0);
  g_signal_connect(G_OBJECT(FProcessScheduledButton), 'clicked',
                  G_CALLBACK(@OnProcessScheduledClicked), Self);

  // TreeView para correos programados
  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
                                GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled, True, True, 0);

  FScheduledListStore := gtk_list_store_new(4, G_TYPE_INT, G_TYPE_STRING,
                                               G_TYPE_STRING, G_TYPE_STRING);
  FScheduledTreeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(FScheduledListStore));
  gtk_container_add(GTK_CONTAINER(scrolled), FScheduledTreeView);

  // Columnas
  renderer := gtk_cell_renderer_text_new();
  column := gtk_tree_view_column_new_with_attributes('Asunto', renderer, 'text', 1, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FScheduledTreeView), column);

  column := gtk_tree_view_column_new_with_attributes('Destinatario', renderer, 'text', 2, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FScheduledTreeView), column);

  column := gtk_tree_view_column_new_with_attributes('Fecha de Envío', renderer, 'text', 3, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FScheduledTreeView), column);

  FScheduledPage := vbox;
  gtk_notebook_append_page(GTK_NOTEBOOK(FNotebook), FScheduledPage,
                          gtk_label_new('Correos Programados'));
end;

procedure TUserWindow.CreateContactsPage;
var
  vbox, hbox, table: PGtkWidget;
  scrolled: PGtkWidget;
  renderer: PGtkCellRenderer;
  column: PGtkTreeViewColumn;
begin
  vbox := gtk_vbox_new(False, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Header para agregar contacto
  hbox := gtk_hbox_new(False, 10);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new('Email del contacto:'), False, False, 0);
  FAddContactEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FAddContactEntry), 100);
  gtk_box_pack_start(GTK_BOX(hbox), FAddContactEntry, True, True, 0);

  FAddContactButton := gtk_button_new_with_label('Agregar');
  gtk_box_pack_start(GTK_BOX(hbox), FAddContactButton, False, False, 0);
  g_signal_connect(G_OBJECT(FAddContactButton), 'clicked', G_CALLBACK(@OnAddContactClicked), Self);

  // Navegación circular
  hbox := gtk_hbox_new(False, 10);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  FPrevContactButton := gtk_button_new_with_label('<');
  gtk_box_pack_start(GTK_BOX(hbox), FPrevContactButton, False, False, 0);
  g_signal_connect(G_OBJECT(FPrevContactButton), 'clicked', G_CALLBACK(@OnPrevContactClicked), Self);

  FNextContactButton := gtk_button_new_with_label('>');
  gtk_box_pack_start(GTK_BOX(hbox), FNextContactButton, False, False, 0);
  g_signal_connect(G_OBJECT(FNextContactButton), 'clicked', G_CALLBACK(@OnNextContactClicked), Self);

  // TreeView para mostrar contacto actual
  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
                                GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled, True, True, 0);

  FContactListStore := gtk_list_store_new(4, G_TYPE_STRING, G_TYPE_STRING,
                                             G_TYPE_STRING, G_TYPE_STRING);
  FContactTreeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(FContactListStore));
  gtk_container_add(GTK_CONTAINER(scrolled), FContactTreeView);

  // Columnas
  renderer := gtk_cell_renderer_text_new();
  column := gtk_tree_view_column_new_with_attributes('Campo', renderer, 'text', 0, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FContactTreeView), column);

  column := gtk_tree_view_column_new_with_attributes('Valor', renderer, 'text', 1, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FContactTreeView), column);

  FContactsPage := vbox;
  gtk_notebook_append_page(GTK_NOTEBOOK(FNotebook), FContactsPage,
                          gtk_label_new('Contactos'));
end;

procedure TUserWindow.CreateProfilePage;
var
  vbox, hbox, table: PGtkWidget;
  label_nombre, label_usuario, label_email, label_telefono: PGtkWidget;
  email_label: PGtkWidget;
  user: PUser;
begin
  vbox := gtk_vbox_new(False, 10);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 10);

  // Título
  hbox := gtk_hbox_new(False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 20);
  gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(nil), True, True, 0);

  var title_label: PGtkWidget;
  title_label := gtk_label_new(nil);
  gtk_label_set_markup(GTK_LABEL(title_label), '<big><b>Actualizar Perfil</b></big>');
  gtk_box_pack_start(GTK_BOX(hbox), title_label, False, False, 0);
  gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(nil), True, True, 0);

  // Tabla para campos
  table := gtk_table_new(4, 2, False);
  gtk_table_set_row_spacings(GTK_TABLE(table), 15);
  gtk_table_set_col_spacings(GTK_TABLE(table), 10);
  gtk_box_pack_start(GTK_BOX(vbox), table, False, False, 20);

  // Campo Nombre
  label_nombre := gtk_label_new('Nombre:');
  gtk_misc_set_alignment(GTK_MISC(label_nombre), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), label_nombre, 0, 1, 0, 1,
                   GTK_FILL, GTK_FILL, 0, 0);

  FProfileNombreEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FProfileNombreEntry), 100);
  gtk_table_attach(GTK_TABLE(table), FProfileNombreEntry, 1, 2, 0, 1,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // Campo Usuario
  label_usuario := gtk_label_new('Usuario:');
  gtk_misc_set_alignment(GTK_MISC(label_usuario), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), label_usuario, 0, 1, 1, 2,
                   GTK_FILL, GTK_FILL, 0, 0);

  FProfileUsuarioEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FProfileUsuarioEntry), 50);
  gtk_table_attach(GTK_TABLE(table), FProfileUsuarioEntry, 1, 2, 1, 2,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // Campo Email (solo lectura)
  label_email := gtk_label_new('Email:');
  gtk_misc_set_alignment(GTK_MISC(label_email), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), label_email, 0, 1, 2, 3,
                   GTK_FILL, GTK_FILL, 0, 0);

  user := SystemCore.GetCurrentUser;
  email_label := gtk_label_new(if user <> nil then PChar(user^.Email) else '');
  gtk_misc_set_alignment(GTK_MISC(email_label), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), email_label, 1, 2, 2, 3,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // Campo Teléfono
  label_telefono := gtk_label_new('Teléfono:');
  gtk_misc_set_alignment(GTK_MISC(label_telefono), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), label_telefono, 0, 1, 3, 4,
                   GTK_FILL, GTK_FILL, 0, 0);

  FProfileTelefonoEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FProfileTelefonoEntry), 20);
  gtk_table_attach(GTK_TABLE(table), FProfileTelefonoEntry, 1, 2, 3, 4,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // Botón Actualizar
  hbox := gtk_hbox_new(False, 0);
  gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(nil), True, True, 0);
  FUpdateProfileButton := gtk_button_new_with_label('Actualizar');
  gtk_box_pack_start(GTK_BOX(hbox), FUpdateProfileButton, False, False, 0);
  gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(nil), True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 20);
  g_signal_connect(G_OBJECT(FUpdateProfileButton), 'clicked', G_CALLBACK(@OnUpdateProfileClicked), Self);

  FProfilePage := vbox;
  gtk_notebook_append_page(GTK_NOTEBOOK(FNotebook), FProfilePage,
                          gtk_label_new('Actualizar Perfil'));
end;

procedure TUserWindow.CreateReportsPage;
var
  vbox, hbox: PGtkWidget;
  title_label, info_label: PGtkWidget;
begin
  vbox := gtk_vbox_new(False, 20);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 20);

  // Título
  hbox := gtk_hbox_new(False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);
  gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(nil), True, True, 0);

  title_label := gtk_label_new(nil);
  gtk_label_set_markup(GTK_LABEL(title_label), '<big><b>Generar Reportes</b></big>');
  gtk_box_pack_start(GTK_BOX(hbox), title_label, False, False, 0);
  gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(nil), True, True, 0);

  // Información
  info_label := gtk_label_new('Los reportes se generarán en formato Graphviz\nen la carpeta de tu usuario.');
  gtk_misc_set_alignment(GTK_MISC(info_label), 0.5, 0.5);
  gtk_box_pack_start(GTK_BOX(vbox), info_label, False, False, 0);

  // Botón Generar
  hbox := gtk_hbox_new(False, 0);
  gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(nil), True, True, 0);
  FGenerateReportsButton := gtk_button_new_with_label('Generar Todos los Reportes');
  gtk_widget_set_size_request(FGenerateReportsButton, 250, 40);
  gtk_box_pack_start(GTK_BOX(hbox), FGenerateReportsButton, False, False, 0);
  gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(nil), True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);
  g_signal_connect(G_OBJECT(FGenerateReportsButton), 'clicked', G_CALLBACK(@OnGenerateReportsClicked), Self);

  FReportsPage := vbox;
  gtk_notebook_append_page(GTK_NOTEBOOK(FNotebook), FReportsPage,
                          gtk_label_new('Generar Reportes'));
end;

procedure TUserWindow.RefreshInbox;
var
  user: PUser;
  inbox: TEmailList;
  email: PEmail;
  iter: TGtkTreeIter;
begin
  user := SystemCore.GetCurrentUser;
  if user = nil then Exit;

  gtk_list_store_clear(FInboxListStore);
  inbox := SystemCore.GetUserInbox(user^.Email);
  if inbox = nil then Exit;

  email := inbox.GetFirst;
  while email <> nil do
  begin
    gtk_list_store_append(FInboxListStore, @iter);
    gtk_list_store_set(FInboxListStore, @iter,
                      0, email^.Id,
                      1, PChar(email^.Estado),
                      2, PChar(email^.Asunto),
                      3, PChar(email^.Remitente),
                      -1);
    email := email^.Next;
  end;

  UpdateUnreadCount;
end;

procedure TUserWindow.RefreshTrash;
var
  user: PUser;
  trash: TEmailStack;
  email: PEmail;
  iter: TGtkTreeIter;
begin
  user := SystemCore.GetCurrentUser;
  if user = nil then Exit;

  gtk_list_store_clear(FTrashListStore);
  trash := SystemCore.GetUserTrash(user^.Email);
  if trash = nil then Exit;

  email := trash.GetTop;
  while email <> nil do
  begin
    gtk_list_store_append(FTrashListStore, @iter);
    gtk_list_store_set(FTrashListStore, @iter,
                      0, email^.Id,
                      1, PChar(email^.Asunto),
                      2, PChar(email^.Remitente),
                      3, PChar(email^.Mensaje),
                      -1);
    email := email^.Next;
  end;
end;

procedure TUserWindow.RefreshScheduled;
var
  queue: TEmailQueue;
  email: PEmail;
  iter: TGtkTreeIter;
  fecha_str: String;
begin
  gtk_list_store_clear(FScheduledListStore);
  queue := SystemCore.GetScheduledEmailsQueue;
  if queue = nil then Exit;

  email := queue.GetFirst;
  while email <> nil do
  begin
    fecha_str := FormatDateTime('dd/mm/yyyy hh:nn', email^.Fecha);
    gtk_list_store_append(FScheduledListStore, @iter);
    gtk_list_store_set(FScheduledListStore, @iter,
                      0, email^.Id,
                      1, PChar(email^.Asunto),
                      2, PChar(email^.Destinatario),
                      3, PChar(fecha_str),
                      -1);
    email := email^.Next;
  end;
end;

procedure TUserWindow.RefreshContacts;
var
  user: PUser;
  contacts: TContactList;
  contact: PContact;
  iter: TGtkTreeIter;
  i: Integer;
begin
  user := SystemCore.GetCurrentUser;
  if user = nil then Exit;

  gtk_list_store_clear(FContactListStore);
  contacts := SystemCore.GetUserContacts(user^.Email);
  if (contacts = nil) or (contacts.GetCount = 0) then Exit;

  contact := contacts.GetFirst;
  if contact = nil then Exit;

  // Navegar al contacto actual
  for i := 0 to FCurrentContactIndex - 1 do
  begin
    contact := contacts.GetNext(contact);
    if contact = contacts.GetFirst then Break; // Evitar bucle infinito
  end;

  // Mostrar datos del contacto actual
  gtk_list_store_append(FContactListStore, @iter);
  gtk_list_store_set(FContactListStore, @iter, 0, 'Nombre', 1, PChar(contact^.Nombre), -1);

  gtk_list_store_append(FContactListStore, @iter);
  gtk_list_store_set(FContactListStore, @iter, 0, 'Usuario', 1, PChar(contact^.Usuario), -1);

  gtk_list_store_append(FContactListStore, @iter);
  gtk_list_store_set(FContactListStore, @iter, 0, 'Correo', 1, PChar(contact^.Email), -1);

  gtk_list_store_append(FContactListStore, @iter);
  gtk_list_store_set(FContactListStore, @iter, 0, 'Teléfono', 1, PChar(contact^.Telefono), -1);
end;

procedure TUserWindow.LoadProfileData;
var
  user: PUser;
begin
  user := SystemCore.GetCurrentUser;
  if user = nil then Exit;

  gtk_entry_set_text(GTK_ENTRY(FProfileNombreEntry), PChar(user^.Nombre));
  gtk_entry_set_text(GTK_ENTRY(FProfileUsuarioEntry), PChar(user^.Usuario));
  gtk_entry_set_text(GTK_ENTRY(FProfileTelefonoEntry), PChar(user^.Telefono));
end;

procedure TUserWindow.ShowMessage(const Msg: String);
begin
  gtk_label_set_text(GTK_LABEL(FStatusLabel), PChar(Msg));
end;

procedure TUserWindow.UpdateUnreadCount;
var
  user: PUser;
  inbox: TEmailList;
  count: Integer;
  count_text: String;
begin
  user := SystemCore.GetCurrentUser;
  if user = nil then Exit;

  inbox := SystemCore.GetUserInbox(user^.Email);
  if inbox = nil then
    count := 0
  else
    count := inbox.GetUnreadCount;

  count_text := 'No leídos: ' + IntToStr(count);
  gtk_label_set_text(GTK_LABEL(FUnreadLabel), PChar(count_text));
end;

procedure TUserWindow.Show;
begin
  if FWindow <> nil then
  begin
    gtk_widget_show_all(FWindow);
    RefreshAll;
  end;
end;

procedure TUserWindow.Hide;
begin
  if FWindow <> nil then
    gtk_widget_hide(FWindow);
end;

procedure TUserWindow.RefreshAll;
begin
  RefreshInbox;
  RefreshTrash;
  RefreshScheduled;
  RefreshContacts;
  LoadProfileData;
end;
end;

