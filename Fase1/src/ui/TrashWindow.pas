unit TrashWindow;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, SysUtils, Classes, DataStructures, SystemCore, UIBase, EmailManager;

type
  TTrashWindow = class(TBaseWindow)
  private
    FMainVBox: PGtkWidget;
    FButtonsHBox: PGtkWidget;
    FEmailList: PGtkWidget;
    FScrolledWindow: PGtkWidget;
    FSearchEntry: PGtkWidget;
    FSearchButton: PGtkWidget;
    FDeleteButton: PGtkWidget;
    FRestoreButton: PGtkWidget;
    FClearAllButton: PGtkWidget;
    FStatusLabel: PGtkWidget;

    // Ventana de detalles del email
    FDetailWindow: PGtkWidget;
    FDetailTextView: PGtkWidget;
    FSelectedEmail: PEmail;

    procedure CreateEmailList;
    procedure ShowEmailDetails(Email: PEmail);
    procedure UpdateStatusLabel;
    procedure SearchInTrash;
    procedure DeleteSelectedEmail;
    procedure RestoreSelectedEmail;
    procedure ClearAllTrash;

  protected
    procedure SetupComponents; override;
    procedure ConnectSignals; override;

  public
    constructor Create(AParent: PGtkWidget = nil);
    destructor Destroy; override;
    procedure RefreshTrash;
  end;

// Callbacks
procedure OnEmailRowActivated(widget: PGtkWidget; path: PGtkTreePath;
                             column: PGtkTreeViewColumn; data: gpointer); cdecl;
procedure OnSearchClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnDeleteClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnRestoreClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnClearAllClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnDetailWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;

implementation

constructor TTrashWindow.Create(AParent: PGtkWidget);
begin
  inherited Create('Papelera', 650, 450, AParent);
  FSelectedEmail := nil;
  RefreshTrash;
end;

destructor TTrashWindow.Destroy;
begin
  if FDetailWindow <> nil then
    gtk_widget_destroy(FDetailWindow);
  inherited;
end;

procedure TTrashWindow.SetupComponents;
var
  SearchHBox: PGtkWidget;
  Label1: PGtkWidget;
begin
  // Contenedor principal
  FMainVBox := TUIUtils.CreateVBox(10);
  gtk_container_add(GTK_CONTAINER(FWindow), FMainVBox);
  gtk_container_set_border_width(GTK_CONTAINER(FMainVBox), 10);

  // Título
  Label1 := TUIUtils.CreateLabel('Papelera de Correos', True);
  gtk_label_set_markup(GTK_LABEL(Label1), '<span size="large" weight="bold">Papelera de Correos</span>');
  gtk_box_pack_start(GTK_BOX(FMainVBox), Label1, False, False, 10);

  // Búsqueda
  SearchHBox := TUIUtils.CreateHBox(10);
  gtk_box_pack_start(GTK_BOX(FMainVBox), SearchHBox, False, False, 5);

  Label1 := TUIUtils.CreateLabel('Buscar:');
  gtk_box_pack_start(GTK_BOX(SearchHBox), Label1, False, False, 5);

  FSearchEntry := TUIUtils.CreateEntry;
  gtk_box_pack_start(GTK_BOX(SearchHBox), FSearchEntry, True, True, 5);

  FSearchButton := TUIUtils.CreateButton('🔍 Buscar', @OnSearchClicked, Self);
  gtk_box_pack_start(GTK_BOX(SearchHBox), FSearchButton, False, False, 5);

  // Botones de acción
  FButtonsHBox := TUIUtils.CreateHBox(10);
  gtk_box_pack_start(GTK_BOX(FMainVBox), FButtonsHBox, False, False, 5);

  FRestoreButton := TUIUtils.CreateButton('📧 Restaurar', @OnRestoreClicked, Self);
  FDeleteButton := TUIUtils.CreateButton('🗑️ Eliminar Definitivamente', @OnDeleteClicked, Self);
  FClearAllButton := TUIUtils.CreateButton('🧹 Vaciar Papelera', @OnClearAllClicked, Self);

  gtk_box_pack_start(GTK_BOX(FButtonsHBox), FRestoreButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(FButtonsHBox), FDeleteButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(FButtonsHBox), FClearAllButton, False, False, 5);

  // Lista de emails
  CreateEmailList;

  // Label de estado
  FStatusLabel := TUIUtils.CreateLabel('');
  gtk_box_pack_start(GTK_BOX(FMainVBox), FStatusLabel, False, False, 5);

  UpdateStatusLabel;
end;

procedure TTrashWindow.CreateEmailList;
var
  ListStore: PGtkListStore;
  Renderer: PGtkCellRenderer;
  Column: PGtkTreeViewColumn;
begin
  // Crear scrolled window
  FScrolledWindow := TUIUtils.CreateScrolledWindow;
  gtk_box_pack_start(GTK_BOX(FMainVBox), FScrolledWindow, True, True, 5);

  // Crear list store con columnas: Asunto, Remitente, Fecha, Mensaje, ID
  ListStore := gtk_list_store_new(5, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_INT);

  // Crear tree view
  FEmailList := gtk_tree_view_new_with_model(GTK_TREE_MODEL(ListStore));
  gtk_container_add(GTK_CONTAINER(FScrolledWindow), FEmailList);

  // Columna Asunto
  Renderer := gtk_cell_renderer_text_new;
  Column := gtk_tree_view_column_new_with_attributes('Asunto', Renderer, 'text', 0, nil);
  gtk_tree_view_column_set_resizable(Column, True);
  gtk_tree_view_column_set_min_width(Column, 200);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FEmailList), Column);

  // Columna Remitente
  Renderer := gtk_cell_renderer_text_new;
  Column := gtk_tree_view_column_new_with_attributes('Remitente', Renderer, 'text', 1, nil);
  gtk_tree_view_column_set_resizable(Column, True);
  gtk_tree_view_column_set_min_width(Column, 150);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FEmailList), Column);

  // Columna Fecha
  Renderer := gtk_cell_renderer_text_new;
  Column := gtk_tree_view_column_new_with_attributes('Fecha', Renderer, 'text', 2, nil);
  gtk_tree_view_column_set_resizable(Column, True);
  gtk_tree_view_column_set_min_width(Column, 120);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FEmailList), Column);

  // Columna Mensaje (preview)
  Renderer := gtk_cell_renderer_text_new;
  Column := gtk_tree_view_column_new_with_attributes('Vista Previa', Renderer, 'text', 3, nil);
  gtk_tree_view_column_set_resizable(Column, True);
  gtk_tree_view_column_set_min_width(Column, 200);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FEmailList), Column);

  g_object_unref(ListStore);
end;

procedure TTrashWindow.ConnectSignals;
begin
  inherited;
  g_signal_connect(G_OBJECT(FEmailList), 'row-activated', G_CALLBACK(@OnEmailRowActivated), Self);

  // Conectar Enter en el campo de búsqueda
  g_signal_connect(G_OBJECT(FSearchEntry), 'activate', G_CALLBACK(@OnSearchClicked), Self);
end;

procedure TTrashWindow.RefreshTrash;
var
  UserTrash: TEmailStack;
  Current: PEmail;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
  FechaText, MessagePreview: String;
begin
  if not IsUserLoggedIn then Exit;

  UserTrash := GetUserTrash(CurrentUser^.Email);
  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(FEmailList)));

  // Limpiar lista
  gtk_list_store_clear(ListStore);

  // Agregar emails a la lista
  Current := UserTrash.GetTop;
  while Current <> nil do
  begin
    FechaText := FormatDateTime('dd/mm/yyyy hh:nn', Current^.Fecha);

    // Vista previa del mensaje (primeros 50 caracteres)
    MessagePreview := Current^.Mensaje;
    if Length(MessagePreview) > 50 then
      MessagePreview := Copy(MessagePreview, 1, 50) + '...';

    gtk_list_store_append(ListStore, @Iter);
    gtk_list_store_set(ListStore, @Iter,
                      0, PChar(Current^.Asunto),
                      1, PChar(Current^.Remitente),
                      2, PChar(FechaText),
                      3, PChar(MessagePreview),
                      4, Current^.Id,
                      -1);

    Current := Current^.Next;
  end;

  UpdateStatusLabel;
end;

procedure TTrashWindow.UpdateStatusLabel;
var
  UserTrash: TEmailStack;
  Count: Integer;
  StatusText: String;
begin
  if not IsUserLoggedIn then Exit;

  UserTrash := GetUserTrash(CurrentUser^.Email);
  Count := UserTrash.GetCount;

  if Count = 0 then
    StatusText := 'La papelera está vacía'
  else if Count = 1 then
    StatusText := '1 correo en la papelera'
  else
    StatusText := Format('%d correos en la papelera', [Count]);

  gtk_label_set_text(GTK_LABEL(FStatusLabel), PChar(StatusText));
end;

procedure TTrashWindow.SearchInTrash;
var
  Keyword: String;
  UserTrash: TEmailStack;
  FoundEmail: PEmail;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
  FechaText, MessagePreview: String;
begin
  if not IsUserLoggedIn then Exit;

  Keyword := gtk_entry_get_text(GTK_ENTRY(FSearchEntry));

  if Length(Keyword) = 0 then
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor ingrese una palabra clave para buscar');
    Exit;
  end;

  UserTrash := GetUserTrash(CurrentUser^.Email);
  FoundEmail := UserTrash.Search(Keyword);

  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(FEmailList)));
  gtk_list_store_clear(ListStore);

  if FoundEmail <> nil then
  begin
    FechaText := FormatDateTime('dd/mm/yyyy hh:nn', FoundEmail^.Fecha);
    MessagePreview := FoundEmail^.Mensaje;
    if Length(MessagePreview) > 50 then
      MessagePreview := Copy(MessagePreview, 1, 50) + '...';

    gtk_list_store_append(ListStore, @Iter);
    gtk_list_store_set(ListStore, @Iter,
                      0, PChar(FoundEmail^.Asunto),
                      1, PChar(FoundEmail^.Remitente),
                      2, PChar(FechaText),
                      3, PChar(MessagePreview),
                      4, FoundEmail^.Id,
                      -1);

    TUIUtils.ShowInfoMessage(FWindow, 'Se encontró 1 correo con la palabra clave: ' + Keyword);
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Búsqueda: 1 resultado encontrado');
  end
  else
  begin
    TUIUtils.ShowInfoMessage(FWindow, 'No se encontraron correos con la palabra clave: ' + Keyword);
    gtk_label_set_text(GTK_LABEL(FStatusLabel), 'Búsqueda: Sin resultados');
  end;
end;

procedure TTrashWindow.DeleteSelectedEmail;
var
  Selection: PGtkTreeSelection;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
  EmailId: Integer;
  UserTrash: TEmailStack;
  Current, Prev: PEmail;
begin
  Selection := gtk_tree_view_get_selection(GTK_TREE_VIEW(FEmailList));
  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(FEmailList)));

  if gtk_tree_selection_get_selected(Selection, nil, @Iter) then
  begin
    gtk_tree_model_get(GTK_TREE_MODEL(ListStore), @Iter, 4, @EmailId, -1);

    if TUIUtils.ShowConfirmDialog(FWindow, 'Eliminar Definitivamente',
                                 '¿Está seguro que desea eliminar este email definitivamente? ' +
                                 'Esta acción no se puede deshacer.') then
    begin
      UserTrash := GetUserTrash(CurrentUser^.Email);
      Current := UserTrash.GetTop;
      Prev := nil;

      // Buscar y eliminar el email de la pila
      while Current <> nil do
      begin
        if Current^.Id = EmailId then
        begin
          if Prev = nil then
            UserTrash.Pop // Es el primero
          else
          begin
            // Eliminar del medio de la pila (implementación simplificada)
            // En una implementación real, necesitarías un método específico
            Break;
          end;

          Dispose(Current);
          RefreshTrash;
          TUIUtils.ShowInfoMessage(FWindow, 'Email eliminado definitivamente');
          Exit;
        end;
        Prev := Current;
        Current := Current^.Next;
      end;

      TUIUtils.ShowErrorMessage(FWindow, 'Error al eliminar el email');
    end;
  end
  else
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor seleccione un email para eliminar');
  end;
end;

procedure TTrashWindow.RestoreSelectedEmail;
var
  Selection: PGtkTreeSelection;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
  EmailId: Integer;
  UserTrash: TEmailStack;
  UserInbox: TEmailList;
  EmailToRestore: PEmail;
begin
  Selection := gtk_tree_view_get_selection(GTK_TREE_VIEW(FEmailList));
  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(FEmailList)));

  if gtk_tree_selection_get_selected(Selection, nil, @Iter) then
  begin
    gtk_tree_model_get(GTK_TREE_MODEL(ListStore), @Iter, 4, @EmailId, -1);

    if TUIUtils.ShowConfirmDialog(FWindow, 'Restaurar Email',
                                 '¿Está seguro que desea restaurar este email a la bandeja de entrada?') then
    begin
      UserTrash := GetUserTrash(CurrentUser^.Email);
      UserInbox := GetUserInbox(CurrentUser^.Email);

      // Buscar email en la papelera
      EmailToRestore := UserTrash.GetTop;
      while EmailToRestore <> nil do
      begin
        if EmailToRestore^.Id = EmailId then
        begin
          // Remover de la papelera
          UserTrash.Pop;

          // Agregar a la bandeja de entrada
          UserInbox.Add(EmailToRestore^.Id, EmailToRestore^.Remitente,
                       EmailToRestore^.Destinatario, EmailToRestore^.Asunto,
                       EmailToRestore^.Mensaje, EmailToRestore^.Programado,
                       EmailToRestore^.Fecha);

          RefreshTrash;
          TUIUtils.ShowInfoMessage(FWindow, 'Email restaurado a la bandeja de entrada');
          Exit;
        end;
        EmailToRestore := EmailToRestore^.Next;
      end;

      TUIUtils.ShowErrorMessage(FWindow, 'Error al restaurar el email');
    end;
  end
  else
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor seleccione un email para restaurar');
  end;
end;

procedure TTrashWindow.ClearAllTrash;
var
  UserTrash: TEmailStack;
begin
  if TUIUtils.ShowConfirmDialog(FWindow, 'Vaciar Papelera',
                               '¿Está seguro que desea eliminar TODOS los correos de la papelera? ' +
                               'Esta acción no se puede deshacer.') then
  begin
    UserTrash := GetUserTrash(CurrentUser^.Email);
    UserTrash.Clear;

    RefreshTrash;
    TUIUtils.ShowInfoMessage(FWindow, 'Papelera vaciada completamente');
  end;
end;

procedure TTrashWindow.ShowEmailDetails(Email: PEmail);
var
  MainVBox, ButtonsHBox: PGtkWidget;
  ScrolledWin: PGtkWidget;
  CloseButton, RestoreButton: PGtkWidget;
  Buffer: PGtkTextBuffer;
  DetailText: String;
begin
  if FDetailWindow <> nil then
    gtk_widget_destroy(FDetailWindow);

  // Crear ventana de detalles
  FDetailWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(FDetailWindow), PChar('Email Eliminado - ' + Email^.Asunto));
  gtk_window_set_default_size(GTK_WINDOW(FDetailWindow), 600, 400);
  gtk_window_set_position(GTK_WINDOW(FDetailWindow), GTK_WIN_POS_CENTER_ON_PARENT);
  gtk_window_set_transient_for(GTK_WINDOW(FDetailWindow), GTK_WINDOW(FWindow));
  gtk_window_set_modal(GTK_WINDOW(FDetailWindow), True);

  MainVBox := TUIUtils.CreateVBox(10);
  gtk_container_add(GTK_CONTAINER(FDetailWindow), MainVBox);
  gtk_container_set_border_width(GTK_CONTAINER(MainVBox), 10);

  // Text view para mostrar detalles
  ScrolledWin := TUIUtils.CreateScrolledWindow;
  gtk_box_pack_start(GTK_BOX(MainVBox), ScrolledWin, True, True, 5);

  FDetailTextView := TUIUtils.CreateTextView;
  gtk_text_view_set_editable(GTK_TEXT_VIEW(FDetailTextView), False);
  gtk_container_add(GTK_CONTAINER(ScrolledWin), FDetailTextView);

  // Formatear texto con detalles del email
  DetailText := '🗑️ CORREO ELIMINADO' + LineEnding + LineEnding +
                'De: ' + Email^.Remitente + LineEnding +
                'Asunto: ' + Email^.Asunto + LineEnding +
                'Fecha: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Email^.Fecha) + LineEnding +
                LineEnding + '--- Mensaje ---' + LineEnding +
                Email^.Mensaje;

  Buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(FDetailTextView));
  gtk_text_buffer_set_text(Buffer, PChar(DetailText), -1);

  // Botones
  ButtonsHBox := TUIUtils.CreateHBox(10);
  gtk_box_pack_start(GTK_BOX(MainVBox), ButtonsHBox, False, False, 5);

  RestoreButton := TUIUtils.CreateButton('📧 Restaurar', nil);
  CloseButton := TUIUtils.CreateButton('Cerrar', nil);

  g_signal_connect(G_OBJECT(RestoreButton), 'clicked', G_CALLBACK(@OnRestoreClicked), Self);
  g_signal_connect(G_OBJECT(CloseButton), 'clicked', G_CALLBACK(@OnDetailWindowDestroy), Self);

  gtk_box_pack_start(GTK_BOX(ButtonsHBox), RestoreButton, False, False, 5);
  gtk_box_pack_end(GTK_BOX(ButtonsHBox), CloseButton, False, False, 5);

  g_signal_connect(G_OBJECT(FDetailWindow), 'destroy', G_CALLBACK(@OnDetailWindowDestroy), Self);

  gtk_widget_show_all(FDetailWindow);
end;

// ============================================================================
// Callbacks
// ============================================================================

procedure OnEmailRowActivated(widget: PGtkWidget; path: PGtkTreePath;
                             column: PGtkTreeViewColumn; data: gpointer); cdecl;
var
  TrashWindow: TTrashWindow;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
  EmailId: Integer;
  UserTrash: TEmailStack;
  Email, Current: PEmail;
begin
  TrashWindow := TTrashWindow(data);
  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(widget)));

  if gtk_tree_model_get_iter(GTK_TREE_MODEL(ListStore), @Iter, path) then
  begin
    gtk_tree_model_get(GTK_TREE_MODEL(ListStore), @Iter, 4, @EmailId, -1);

    UserTrash := GetUserTrash(CurrentUser^.Email);
    Current := UserTrash.GetTop;
    Email := nil;

    // Buscar el email por ID
    while Current <> nil do
    begin
      if Current^.Id = EmailId then
      begin
        Email := Current;
        Break;
      end;
      Current := Current^.Next;
    end;

    if Email <> nil then
    begin
      TrashWindow.FSelectedEmail := Email;
      TrashWindow.ShowEmailDetails(Email);
    end;
  end;
end;

procedure OnSearchClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  TrashWindow: TTrashWindow;
begin
  TrashWindow := TTrashWindow(data);
  TrashWindow.SearchInTrash;
end;

procedure OnDeleteClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  TrashWindow: TTrashWindow;
begin
  TrashWindow := TTrashWindow(data);
  TrashWindow.DeleteSelectedEmail;
end;

procedure OnRestoreClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  TrashWindow: TTrashWindow;
begin
  TrashWindow := TTrashWindow(data);
  TrashWindow.RestoreSelectedEmail;
end;

procedure OnClearAllClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  TrashWindow: TTrashWindow;
begin
  TrashWindow := TTrashWindow(data);
  TrashWindow.ClearAllTrash;
end;

procedure OnDetailWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
var
  TrashWindow: TTrashWindow;
begin
  TrashWindow := TTrashWindow(data);
  TrashWindow.FDetailWindow := nil;
end;

end.

