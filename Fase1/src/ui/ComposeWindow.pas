unit ComposeWindow;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, GLib2, SysUtils, Classes, DataStructures, SystemCore, UIBase,
  EmailManager, ContactManager;

type
  TComposeWindow = class(TBaseWindow)
  private
    FMainVBox: PGtkWidget;
    FRecipientEntry: PGtkWidget;
    FSubjectEntry: PGtkWidget;
    FMessageTextView: PGtkWidget;
    FSendButton: PGtkWidget;
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

  protected
    procedure SetupComponents; override;
    procedure ConnectSignals; override;

  public
    constructor Create(AParent: PGtkWidget = nil);
    destructor Destroy; override;
    procedure SendEmail;
    procedure LoadContactIntoRecipient(ContactEmail: String);
  end;

// Callbacks
procedure OnSendClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnClearClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnContactsClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnContactSelected(widget: PGtkWidget; path: PGtkTreePath;
                           column: PGtkTreeViewColumn; data: gpointer); cdecl;
procedure OnContactsWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;

implementation

constructor TComposeWindow.Create(AParent: PGtkWidget);
begin
  inherited Create('Enviar Correo', 600, 500, AParent);
  ClearForm;
end;

destructor TComposeWindow.Destroy;
begin
  if FContactsWindow <> nil then
    gtk_widget_destroy(FContactsWindow);
  inherited;
end;

procedure TComposeWindow.SetupComponents;
var
  Table: PGtkWidget;
  ScrolledWin: PGtkWidget;
  ButtonsHBox: PGtkWidget;
  Label1: PGtkWidget;
begin
  // Contenedor principal
  FMainVBox := TUIUtils.CreateVBox(10);
  gtk_container_add(GTK_CONTAINER(FWindow), FMainVBox);
  gtk_container_set_border_width(GTK_CONTAINER(FMainVBox), 15);

  // Título
  Label1 := TUIUtils.CreateLabel('Redactar Nuevo Correo', True);
  gtk_label_set_markup(GTK_LABEL(Label1),
    Pgchar(UTF8String('<span size="large" weight="bold">Redactar Nuevo Correo</span>')));
  gtk_box_pack_start(GTK_BOX(FMainVBox), Label1, False, False, 10);

  // Tabla para organizar campos
  Table := TUIUtils.CreateTable(3, 3);
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

  // Campo Mensaje
  Label1 := TUIUtils.CreateLabel('Mensaje:');
  gtk_misc_set_alignment(GTK_MISC(Label1), 0.0, 0.0);
  gtk_table_attach(GTK_TABLE(Table), Label1, 0, 1, 2, 3, GTK_FILL, GTK_FILL, 5, 5);

  ScrolledWin := TUIUtils.CreateScrolledWindow;
  gtk_widget_set_size_request(ScrolledWin, -1, 250);
  gtk_table_attach(GTK_TABLE(Table), ScrolledWin, 1, 3, 2, 3,
                  GTK_EXPAND or GTK_FILL, GTK_EXPAND or GTK_FILL, 5, 5);

  FMessageTextView := TUIUtils.CreateTextView;
  gtk_container_add(GTK_CONTAINER(ScrolledWin), FMessageTextView);

  // Botones
  ButtonsHBox := TUIUtils.CreateHBox(10);
  gtk_box_pack_start(GTK_BOX(FMainVBox), ButtonsHBox, False, False, 10);

  FSendButton := TUIUtils.CreateButton('📤 Enviar', @OnSendClicked, Self);
  FClearButton := TUIUtils.CreateButton('🧹 Limpiar', @OnClearClicked, Self);

  gtk_widget_set_size_request(FSendButton, 100, 35);
  gtk_widget_set_size_request(FClearButton, 100, 35);

  gtk_box_pack_start(GTK_BOX(ButtonsHBox), FSendButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(ButtonsHBox), FClearButton, False, False, 5);

  // Label de estado
  FStatusLabel := TUIUtils.CreateLabel('');
  gtk_box_pack_start(GTK_BOX(FMainVBox), FStatusLabel, False, False, 5);

  CreateContactsWindow;
end;

procedure TComposeWindow.ConnectSignals;
begin
  inherited;
end;

procedure TComposeWindow.CreateContactsWindow;
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

  g_object_unref(ListStore);

  // Conectar señal de selección
  g_signal_connect(G_OBJECT(FContactsList), 'row-activated',
                  G_CALLBACK(@OnContactSelected), Self);

  // Botones
  ButtonsHBox := TUIUtils.CreateHBox(10);
  gtk_box_pack_start(GTK_BOX(MainVBox), ButtonsHBox, False, False, 5);

  CloseButton := TUIUtils.CreateButton('Cerrar', nil);
  g_signal_connect(G_OBJECT(CloseButton), 'clicked',
                  G_CALLBACK(@OnContactsWindowDestroy), Self);
  gtk_box_pack_end(GTK_BOX(ButtonsHBox), CloseButton, False, False, 5);

  g_signal_connect(G_OBJECT(FContactsWindow), 'destroy',
                  G_CALLBACK(@OnContactsWindowDestroy), Self);
end;

procedure TComposeWindow.ShowContactsWindow;
var
  UserContacts: TContactList;
  Current: PContact;
  FirstContact: PContact;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
  Count: Integer;
begin
  if not IsUserLoggedIn then Exit;

  UserContacts := ContactManager.UserContactManager.GetUserContacts(CurrentUser^.Email);
  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(FContactsList)));

  // Limpiar lista
  gtk_list_store_clear(ListStore);

  if UserContacts.GetCount = 0 then
  begin
    TUIUtils.ShowInfoMessage(FWindow, 'No tienes contactos agregados. Agrega contactos primero.');
    Exit;
  end;

  // Agregar contactos a la lista
  Count := 0;
  FirstContact := UserContacts.GetFirst;
  Current := FirstContact;

  if Current <> nil then
  begin
    repeat
      Inc(Count);
      gtk_list_store_append(ListStore, @Iter);
      gtk_list_store_set(ListStore, @Iter,
                        0, PChar(Current^.Nombre),
                        1, PChar(Current^.Email),
                        -1);

      Current := UserContacts.GetNext(Current);
    until (Current = FirstContact) or (Count >= UserContacts.GetCount);
  end;

  gtk_widget_show_all(FContactsWindow);
end;

procedure TComposeWindow.ClearForm;
var
  Buffer: PGtkTextBuffer;
begin
  gtk_entry_set_text(GTK_ENTRY(FRecipientEntry), Pgchar(UTF8String('')));
  gtk_entry_set_text(GTK_ENTRY(FSubjectEntry),   Pgchar(UTF8String('')));

  Buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(FMessageTextView));
  gtk_text_buffer_set_text(Buffer, Pgchar(UTF8String('')), -1);

  gtk_label_set_text(GTK_LABEL(FStatusLabel), Pgchar(UTF8String('')));
end;

function TComposeWindow.ValidateForm: Boolean;
var
  Recipient, Subject: String;
  Buffer: PGtkTextBuffer;
  StartIter, EndIter: TGtkTextIter;
  Message: PChar;
begin
  Result := False;

  Recipient := UTF8String(gtk_entry_get_text(GTK_ENTRY(FRecipientEntry)));
  Subject   := UTF8String(gtk_entry_get_text(GTK_ENTRY(FSubjectEntry)));

  Buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(FMessageTextView));
  gtk_text_buffer_get_bounds(Buffer, @StartIter, @EndIter);
  Message := gtk_text_buffer_get_text(Buffer, @StartIter, @EndIter, False);

  if Length(Recipient) = 0 then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor ingrese el destinatario');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), Pgchar(UTF8String('Error: Destinatario requerido')));
    g_free(Message);
    Exit;
  end;

  if Length(Subject) = 0 then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor ingrese el asunto');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), Pgchar(UTF8String('Error: Asunto requerido')));
    g_free(Message);
    Exit;
  end;

  if Length(String(Message)) = 0 then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor ingrese el mensaje');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), Pgchar(UTF8String('Error: Mensaje requerido')));
    g_free(Message);
    Exit;
  end;

  g_free(Message);
  Result := True;
end;

procedure TComposeWindow.SendEmail;
var
  Recipient, Subject: String;
  Buffer: PGtkTextBuffer;
  StartIter, EndIter: TGtkTextIter;
  Message: PChar;
  MessageStr: String;
begin
  if not ValidateForm then Exit;

  Recipient := UTF8String(gtk_entry_get_text(GTK_ENTRY(FRecipientEntry)));
  Subject   := UTF8String(gtk_entry_get_text(GTK_ENTRY(FSubjectEntry)));

  Buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(FMessageTextView));
  gtk_text_buffer_get_bounds(Buffer, @StartIter, @EndIter);
  Message := gtk_text_buffer_get_text(Buffer, @StartIter, @EndIter, False);
  MessageStr := String(Message);
  g_free(Message);

  // Verificar que el destinatario sea un contacto
  if not IsContact(CurrentUser^.Email, Recipient) then
  begin
    TUIUtils.ShowErrorMessage(FWindow,
      'Error: Solo puedes enviar correos a tus contactos. ' +
      'Agrega primero a ' + Recipient + ' como contacto.');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), Pgchar(UTF8String('Error: Destinatario no es contacto')));
    Exit;
  end;

  // Enviar email
  if EmailManager.SendEmail(Recipient, Subject, MessageStr) then
  begin
    TUIUtils.ShowInfoMessage(FWindow, 'Correo enviado exitosamente a ' + Recipient);
    gtk_label_set_text(GTK_LABEL(FStatusLabel), Pgchar(UTF8String('Correo enviado exitosamente')));
    ClearForm;
  end
  else
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Error al enviar el correo');
    gtk_label_set_text(GTK_LABEL(FStatusLabel), Pgchar(UTF8String('Error al enviar correo')));
  end;
end;

procedure TComposeWindow.LoadContactIntoRecipient(ContactEmail: String);
begin
  // ← Aquí estaba la referencia a CommunitySelected/FCommunityCombo.
  // Solo cargamos el email en el campo.
  gtk_entry_set_text(GTK_ENTRY(FRecipientEntry), Pgchar(UTF8String(ContactEmail)));
end;

// ============================================================================
// Callbacks
// ============================================================================

procedure OnSendClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ComposeWindow: TComposeWindow;
begin
  ComposeWindow := TComposeWindow(data);
  ComposeWindow.SendEmail;
end;

procedure OnClearClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ComposeWindow: TComposeWindow;
begin
  ComposeWindow := TComposeWindow(data);

  if TUIUtils.ShowConfirmDialog(ComposeWindow.Window, 'Limpiar Formulario',
                               '¿Está seguro que desea limpiar el formulario?') then
  begin
    ComposeWindow.ClearForm;
  end;
end;

procedure OnContactsClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ComposeWindow: TComposeWindow;
begin
  ComposeWindow := TComposeWindow(data);
  ComposeWindow.ShowContactsWindow;
end;

procedure OnContactSelected(widget: PGtkWidget; path: PGtkTreePath;
                           column: PGtkTreeViewColumn; data: gpointer); cdecl;
var
  ComposeWindow: TComposeWindow;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
  ContactEmail: PChar;
begin
  ComposeWindow := TComposeWindow(data);
  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(widget)));

  if gtk_tree_model_get_iter(GTK_TREE_MODEL(ListStore), @Iter, path) then
  begin
    gtk_tree_model_get(GTK_TREE_MODEL(ListStore), @Iter, 1, @ContactEmail, -1);

    ComposeWindow.LoadContactIntoRecipient(String(ContactEmail));
    gtk_widget_hide(ComposeWindow.FContactsWindow);

    g_free(ContactEmail);
  end;
end;

procedure OnContactsWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
var
  ComposeWindow: TComposeWindow;
begin
  ComposeWindow := TComposeWindow(data);
  gtk_widget_hide(ComposeWindow.FContactsWindow);
end;

end.

