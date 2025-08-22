unit ScheduleWindow;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, GLib2, SysUtils, Classes, DateUtils, DataStructures, SystemCore, UIBase,
  EmailManager, ContactManager;

type
  TScheduleWindow = class(TBaseWindow)
  private
    FMainVBox: PGtkWidget;
    FRecipientEntry: PGtkWidget;
    FSubjectEntry: PGtkWidget;
    FMessageTextView: PGtkWidget;
    FDateEntry: PGtkWidget;
    FTimeEntry: PGtkWidget;
    FScheduleButton: PGtkWidget;
    FClearButton: PGtkWidget;
    FContactsButton: PGtkWidget;
    FStatusLabel: PGtkWidget;

    // Ventana de selección de contactos
    FContactsWindow: PGtkWidget;
    FContactsList: PGtkWidget;

    procedure CreateContactsWindow;
    procedure ShowContactsWindow;
    procedure ClearForm;
    function ValidateForm: Boolean;
    function ParseDateTime: TDateTime;

  protected
    procedure SetupComponents; override;
    procedure ConnectSignals; override;

  public
    constructor Create(AParent: PGtkWidget = nil);
    destructor Destroy; override;
    procedure ScheduleEmail;
    procedure LoadContactIntoRecipient(ContactEmail: String);
  end;

// Callbacks
procedure OnScheduleClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnClearClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnContactsClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnContactSelected(widget: PGtkWidget; path: PGtkTreePath;
                           column: PGtkTreeViewColumn; data: gpointer); cdecl;
procedure OnContactsWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;

implementation

constructor TScheduleWindow.Create(AParent: PGtkWidget);
begin
  inherited Create('Programar Correo', 600, 550, AParent);
  ClearForm;
end;

destructor TScheduleWindow.Destroy;
begin
  if FContactsWindow <> nil then
    gtk_widget_destroy(FContactsWindow);
  inherited;
end;

procedure TScheduleWindow.SetupComponents;
var
  Table: PGtkWidget;
  ScrolledWin: PGtkWidget;
  ButtonsHBox: PGtkWidget;
  DateTimeHBox: PGtkWidget;
  Label1: PGtkWidget;
  CurrentDateTime: TDateTime;
  DateStr, TimeStr: String;
begin
  // Contenedor principal
  FMainVBox := TUIUtils.CreateVBox(10);
  gtk_container_add(GTK_CONTAINER(FWindow), FMainVBox);
  gtk_container_set_border_width(GTK_CONTAINER(FMainVBox), 15);

  // Título
  Label1 := TUIUtils.CreateLabel('Programar Envío de Correo', True);
  gtk_label_set_markup(GTK_LABEL(Label1), '<span size="large" weight="bold">Programar Envío de Correo</span>');
  gtk_box_pack_start(GTK_BOX(FMainVBox), Label1, False, False, 10);

  // Tabla para organizar campos
  Table := TUIUtils.CreateTable(5, 3);
  gtk_box_pack_start(GTK_BOX(FMainVBox), Table, False, False, 10);

  // Campo Destinatario
  Label1 := TUIUtils.CreateLabel('Para:');
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 0, 1, GTK_FILL, GTK_FILL, 5, 5);

  FRecipientEntry := TUIUtils.CreateEntry;
  gtk_table_attach(GTK_TABLE(Table), FRecipientEntry, 1, 2, 0, 1,
                  GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 5);

  FContactsButton := TUIUtils.CreateButton('📋 Contactos', @OnContactsClicked, Self);
  gtk_table_attach(GTK_TABLE(Table), FContactsButton, 2, 3, 0, 1, GTK_FILL, GTK_FILL, 5, 5);

  // Campo Asunto
  Label1 := TUIUtils.CreateLabel('Asunto:');
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 1, 2, GTK_FILL, GTK_FILL, 5, 5);

  FSubjectEntry := TUIUtils.CreateEntry;
  gtk_table_attach(GTK_TABLE(Table), FSubjectEntry, 1, 3, 1, 2,
                  GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 5);

  // Fecha y Hora de envío
  Label1 := TUIUtils.CreateLabel('Fecha de envío:');
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 2, 3, GTK_FILL, GTK_FILL, 5, 5);

  DateTimeHBox := TUIUtils.CreateHBox(5);
  gtk_table_attach(GTK_TABLE(Table), DateTimeHBox, 1, 3, 2, 3,
                  GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 5);

  FDateEntry := TUIUtils.CreateEntry;
  FTimeEntry := TUIUtils.CreateEntry;

  // CORREGIDO: Establecer valores por defecto con manejo de errores
  try
    CurrentDateTime := Now + (1/24); // Agregar 1 hora
    DateStr := SysUtils.FormatDateTime('dd/mm/yyyy', CurrentDateTime);
    TimeStr := SysUtils.FormatDateTime('hh:nn', CurrentDateTime);
  except
    // Si falla, usar valores por defecto
    DateStr := '01/01/2025';
    TimeStr := '12:00';
  end;

  gtk_entry_set_text(GTK_ENTRY(FDateEntry), PChar(DateStr));
  gtk_entry_set_text(GTK_ENTRY(FTimeEntry), PChar(TimeStr));

  gtk_box_pack_start(GTK_BOX(DateTimeHBox), FDateEntry, True, True, 5);

  Label1 := TUIUtils.CreateLabel('Hora:');
  gtk_box_pack_start(GTK_BOX(DateTimeHBox), Label1, False, False, 5);
  gtk_box_pack_start(GTK_BOX(DateTimeHBox), FTimeEntry, True, True, 5);

  // Información de formato
  Label1 := TUIUtils.CreateLabel('Formato: dd/mm/aaaa hh:mm');
  gtk_misc_set_alignment(GTK_MISC(Label1), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(Table), Label1, 1, 3, 3, 4, GTK_FILL, GTK_FILL, 5, 5);

  // Campo Mensaje
  Label1 := TUIUtils.CreateLabel('Mensaje:');
  gtk_misc_set_alignment(GTK_MISC(Label1), 0.0, 0.0);
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 4, 5, GTK_FILL, GTK_FILL, 5, 5);

  ScrolledWin := TUIUtils.CreateScrolledWindow;
  gtk_widget_set_size_request(ScrolledWin, -1, 200);
  gtk_table_attach(GTK_TABLE(Table), ScrolledWin, 1, 3, 4, 5,
                  GTK_EXPAND or GTK_FILL, GTK_EXPAND or GTK_FILL, 5, 5);

  FMessageTextView := TUIUtils.CreateTextView;
  gtk_container_add(GTK_CONTAINER(ScrolledWin), FMessageTextView);

  // Botones
  ButtonsHBox := TUIUtils.CreateHBox(10);
  gtk_box_pack_start(GTK_BOX(FMainVBox), ButtonsHBox, False, False, 10);

  FScheduleButton := TUIUtils.CreateButton('⏰ Programar Envío', @OnScheduleClicked, Self);
  FClearButton := TUIUtils.CreateButton('🧹 Limpiar', @OnClearClicked, Self);

  gtk_widget_set_size_request(FScheduleButton, 150, 35);
  gtk_widget_set_size_request(FClearButton, 100, 35);

  gtk_box_pack_start(GTK_BOX(ButtonsHBox), FScheduleButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(ButtonsHBox), FClearButton, False, False, 5);

  // Label de estado
  FStatusLabel := TUIUtils.CreateLabel('');
  gtk_box_pack_start(GTK_BOX(FMainVBox), FStatusLabel, False, False, 5);

  CreateContactsWindow;
end;

procedure TScheduleWindow.ConnectSignals;
begin
  inherited;
end;

procedure TScheduleWindow.CreateContactsWindow;
var
  MainVBox: PGtkWidget;
  ScrolledWin: PGtkWidget;
  ButtonsHBox: PGtkWidget;
  CloseButton: PGtkWidget;
  ListStore: PGtkListStore;
  Renderer: PGtkCellRenderer;
  Column: PGtkTreeViewColumn;
begin
  // Crear ventana de contactos
  FContactsWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(FContactsWindow), 'Seleccionar Contacto');
  gtk_window_set_default_size(GTK_WINDOW(FContactsWindow), 400, 300);
  gtk_window_set_position(GTK_WINDOW(FContactsWindow), GTK_WIN_POS_CENTER_ON_PARENT);
  gtk_window_set_transient_for(GTK_WINDOW(FContactsWindow), GTK_WINDOW(FWindow));
  gtk_window_set_modal(GTK_WINDOW(FContactsWindow), True);

  MainVBox := TUIUtils.CreateVBox(10);
  gtk_container_add(GTK_CONTAINER(FContactsWindow), MainVBox);
  gtk_container_set_border_width(GTK_CONTAINER(MainVBox), 10);

  // Lista de contactos
  ScrolledWin := TUIUtils.CreateScrolledWindow;
  gtk_box_pack_start(GTK_BOX(MainVBox), ScrolledWin, True, True, 5);

  // Crear list store: Nombre, Email
  ListStore := gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);

  FContactsList := gtk_tree_view_new_with_model(GTK_TREE_MODEL(ListStore));
  gtk_container_add(GTK_CONTAINER(ScrolledWin), FContactsList);

  // Columna Nombre
  Renderer := gtk_cell_renderer_text_new;
  Column := gtk_tree_view_column_new_with_attributes('Nombre', Renderer, 'text', 0, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FContactsList), Column);

  // Columna Email
  Renderer := gtk_cell_renderer_text_new;
  Column := gtk_tree_view_column_new_with_attributes('Email', Renderer, 'text', 1, nil);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FContactsList), Column);

  // Conectar señal de selección
  g_signal_connect(G_OBJECT(FContactsList), 'row-activated',
                   G_CALLBACK(@OnContactSelected), Self);

  // Botón cerrar
  ButtonsHBox := TUIUtils.CreateHBox(10);
  gtk_box_pack_start(GTK_BOX(MainVBox), ButtonsHBox, False, False, 5);

  CloseButton := TUIUtils.CreateButton('Cerrar', @OnContactsWindowDestroy, Self);
  gtk_box_pack_end(GTK_BOX(ButtonsHBox), CloseButton, False, False, 5);

  // Conectar señal de cierre
  g_signal_connect(G_OBJECT(FContactsWindow), 'destroy',
                   G_CALLBACK(@OnContactsWindowDestroy), Self);
end;

procedure TScheduleWindow.ShowContactsWindow;
var
  Contacts: TContactList;
  Current: PContact;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
begin
  if CurrentUser = nil then Exit;

  // Cargar contactos del usuario actual
  Contacts := GetUserContacts(CurrentUser^.Email);
  if Contacts = nil then Exit;

  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(FContactsList)));
  gtk_list_store_clear(ListStore);

  Current := Contacts.GetFirst;
  while Current <> nil do
  begin
    gtk_list_store_append(ListStore, @Iter);
    gtk_list_store_set(ListStore, @Iter,
                       0, PChar(Current^.Nombre),
                       1, PChar(Current^.Email),
                       -1);
    Current := Current^.Next;
  end;

  gtk_widget_show_all(FContactsWindow);
end;

procedure TScheduleWindow.ClearForm;
var
  Buffer: PGtkTextBuffer;
  CurrentDateTime: TDateTime;
  DateStr, TimeStr: String;
begin
  gtk_entry_set_text(GTK_ENTRY(FRecipientEntry), '');
  gtk_entry_set_text(GTK_ENTRY(FSubjectEntry), '');

  Buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(FMessageTextView));
  gtk_text_buffer_set_text(Buffer, '', -1);

  // CORREGIDO: Restablecer fecha y hora con manejo de errores
  try
    CurrentDateTime := Now + (1/24); // Agregar 1 hora
    DateStr := SysUtils.FormatDateTime('dd/mm/yyyy', CurrentDateTime);
    TimeStr := SysUtils.FormatDateTime('hh:nn', CurrentDateTime);
  except
    // Si falla, usar valores por defecto
    DateStr := '01/01/2025';
    TimeStr := '12:00';
  end;

  gtk_entry_set_text(GTK_ENTRY(FDateEntry), PChar(DateStr));
  gtk_entry_set_text(GTK_ENTRY(FTimeEntry), PChar(TimeStr));

  gtk_label_set_text(GTK_LABEL(FStatusLabel), '');
end;

function TScheduleWindow.ValidateForm: Boolean;
var
  Recipient, Subject, DateStr, TimeStr: String;
  Buffer: PGtkTextBuffer;
  StartIter, EndIter: TGtkTextIter;
  Message: PChar;
  ScheduleDateTime: TDateTime;
begin
  Result := False;

  Recipient := gtk_entry_get_text(GTK_ENTRY(FRecipientEntry));
  Subject := gtk_entry_get_text(GTK_ENTRY(FSubjectEntry));
  DateStr := gtk_entry_get_text(GTK_ENTRY(FDateEntry));
  TimeStr := gtk_entry_get_text(GTK_ENTRY(FTimeEntry));

  Buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(FMessageTextView));
  gtk_text_buffer_get_bounds(Buffer, @StartIter, @EndIter);
  Message := gtk_text_buffer_get_text(Buffer, @StartIter, @EndIter, False);

  if Length(Recipient) = 0 then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor ingrese el destinatario');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Destinatario requerido');
    g_free(Message);
    Exit;
  end;

  if Length(Subject) = 0 then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor ingrese el asunto');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Asunto requerido');
    g_free(Message);
    Exit;
  end;

  if Length(String(Message)) = 0 then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor ingrese el mensaje');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Mensaje requerido');
    g_free(Message);
    Exit;
  end;

  if (Length(DateStr) = 0) or (Length(TimeStr) = 0) then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor ingrese la fecha y hora de envío');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Fecha y hora requeridas');
    g_free(Message);
    Exit;
  end;

  // CORREGIDO: Validar formato de fecha y hora con mejor manejo de errores
  try
    ScheduleDateTime := ParseDateTime;
    if ScheduleDateTime <= 0 then
    begin
      TUIUtils.ShowErrorMessage(FWindow, 'Formato de fecha u hora inválido. Use: dd/mm/aaaa hh:mm');
      gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Formato de fecha/hora inválido');
      g_free(Message);
      Exit;
    end;

    if ScheduleDateTime <= Now then
    begin
      TUIUtils.ShowErrorMessage(FWindow, 'La fecha y hora de envío debe ser futura');
      gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Fecha debe ser futura');
      g_free(Message);
      Exit;
    end;
  except
    on E: Exception do
    begin
      TUIUtils.ShowErrorMessage(FWindow, 'Error en fecha/hora: ' + E.Message + '. Use: dd/mm/aaaa hh:mm');
      gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Formato de fecha/hora inválido');
      g_free(Message);
      Exit;
    end;
  end;

  g_free(Message);
  Result := True;
end;

function TScheduleWindow.ParseDateTime: TDateTime;
var
  DateStr, TimeStr: String;
  DateParts, TimeParts: TStringList;
  Day, Month, Year, Hour, Minute: Integer;
begin
  Result := 0; // CORREGIDO: Valor por defecto seguro

  DateStr := gtk_entry_get_text(GTK_ENTRY(FDateEntry));
  TimeStr := gtk_entry_get_text(GTK_ENTRY(FTimeEntry));

  DateParts := TStringList.Create;
  TimeParts := TStringList.Create;

  try
    // Parsear fecha (dd/mm/yyyy)
    DateParts.Delimiter := '/';
    DateParts.DelimitedText := DateStr;

    if DateParts.Count <> 3 then
      raise Exception.Create('Formato de fecha inválido');

    // CORREGIDO: Validar conversión de strings a enteros
    if not TryStrToInt(DateParts[0], Day) or
       not TryStrToInt(DateParts[1], Month) or
       not TryStrToInt(DateParts[2], Year) then
      raise Exception.Create('Valores de fecha no numéricos');

    // Parsear hora (hh:mm)
    TimeParts.Delimiter := ':';
    TimeParts.DelimitedText := TimeStr;

    if TimeParts.Count <> 2 then
      raise Exception.Create('Formato de hora inválido');

    // CORREGIDO: Validar conversión de strings a enteros
    if not TryStrToInt(TimeParts[0], Hour) or
       not TryStrToInt(TimeParts[1], Minute) then
      raise Exception.Create('Valores de hora no numéricos');

    // CORREGIDO: Validar rangos más estrictos
    if (Day < 1) or (Day > 31) or (Month < 1) or (Month > 12) or
       (Year < 2025) or (Year > 2030) or (Hour < 0) or (Hour > 23) or
       (Minute < 0) or (Minute > 59) then
      raise Exception.Create('Valores de fecha/hora fuera de rango válido');

    // CORREGIDO: Usar TryEncodeDateTime para evitar excepciones de punto flotante
    if not TryEncodeDateTime(Year, Month, Day, Hour, Minute, 0, 0, Result) then
      raise Exception.Create('Fecha/hora inválida - verifique que el día existe en ese mes');

  finally
    DateParts.Free;
    TimeParts.Free;
  end;
end;

procedure TScheduleWindow.ScheduleEmail;
var
  Recipient, Subject: String;
  Buffer: PGtkTextBuffer;
  StartIter, EndIter: TGtkTextIter;
  Message: PChar;
  MessageStr: String;
  ScheduleDateTime: TDateTime;
begin
  if not ValidateForm then Exit;

  Recipient := gtk_entry_get_text(GTK_ENTRY(FRecipientEntry));
  Subject := gtk_entry_get_text(GTK_ENTRY(FSubjectEntry));

  Buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(FMessageTextView));
  gtk_text_buffer_get_bounds(Buffer, @StartIter, @EndIter);
  Message := gtk_text_buffer_get_text(Buffer, @StartIter, @EndIter, False);
  MessageStr := String(Message);
  g_free(Message);

  ScheduleDateTime := ParseDateTime;

  // Verificar que el destinatario sea un contacto
  if not IsContact(CurrentUser^.Email, Recipient) then
  begin
    TUIUtils.ShowErrorMessage(FWindow,
      'Error: Solo puedes programar correos a tus contactos. ' +
      'Agrega primero a ' + Recipient + ' como contacto.');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error: Destinatario no es contacto');
    Exit;
  end;

  // Programar email
  if EmailManager.ScheduleEmail(Recipient, Subject, MessageStr, ScheduleDateTime) then
  begin
    TUIUtils.ShowInfoMessage(FWindow,
      Format('Correo programado exitosamente para %s el %s',
             [Recipient, SysUtils.FormatDateTime('dd/mm/yyyy hh:nn', ScheduleDateTime)]));
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Correo programado exitosamente');
    ClearForm;
  end
  else
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Error al programar el correo');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Error al programar correo');
  end;
end;

procedure TScheduleWindow.LoadContactIntoRecipient(ContactEmail: String);
begin
  gtk_entry_set_text(GTK_ENTRY(FRecipientEntry), PChar(ContactEmail));
end;

// ============================================================================
// Callbacks
// ============================================================================

procedure OnScheduleClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ScheduleWindow: TScheduleWindow;
begin
  ScheduleWindow := TScheduleWindow(data);
  ScheduleWindow.ScheduleEmail;
end;

procedure OnClearClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ScheduleWindow: TScheduleWindow;
begin
  ScheduleWindow := TScheduleWindow(data);

  if TUIUtils.ShowConfirmDialog(ScheduleWindow.Window, 'Limpiar Formulario',
                               '¿Está seguro que desea limpiar el formulario?') then
  begin
    ScheduleWindow.ClearForm;
  end;
end;

procedure OnContactsClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ScheduleWindow: TScheduleWindow;
begin
  ScheduleWindow := TScheduleWindow(data);
  ScheduleWindow.ShowContactsWindow;
end;

procedure OnContactSelected(widget: PGtkWidget; path: PGtkTreePath;
                           column: PGtkTreeViewColumn; data: gpointer); cdecl;
var
  ScheduleWindow: TScheduleWindow;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
  ContactEmail: PChar;
begin
  ScheduleWindow := TScheduleWindow(data);
  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(widget)));

  if gtk_tree_model_get_iter(GTK_TREE_MODEL(ListStore), @Iter, path) then
  begin
    gtk_tree_model_get(GTK_TREE_MODEL(ListStore), @Iter, 1, @ContactEmail, -1);

    ScheduleWindow.LoadContactIntoRecipient(String(ContactEmail));
    gtk_widget_hide(ScheduleWindow.FContactsWindow);

    g_free(ContactEmail);
  end;
end;

procedure OnContactsWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
var
  ScheduleWindow: TScheduleWindow;
begin
  ScheduleWindow := TScheduleWindow(data);
  gtk_widget_hide(ScheduleWindow.FContactsWindow);
end;

end.
