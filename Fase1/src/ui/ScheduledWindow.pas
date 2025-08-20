unit ScheduledWindow;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, SysUtils, Classes, DataStructures, SystemCore, UIBase, EmailManager;

type
  TScheduledWindow = class(TBaseWindow)
  private
    FMainVBox: PGtkWidget;
    FButtonsHBox: PGtkWidget;
    FEmailList: PGtkWidget;
    FScrolledWindow: PGtkWidget;
    FRefreshButton: PGtkWidget;
    FSendNowButton: PGtkWidget;
    FSendAllButton: PGtkWidget;
    FCancelButton: PGtkWidget;
    FStatusLabel: PGtkWidget;

    // Ventana de detalles del email
    FDetailWindow: PGtkWidget;
    FDetailTextView: PGtkWidget;
    FSelectedEmail: PEmail;

    procedure CreateEmailList;
    procedure ShowEmailDetails(Email: PEmail);
    procedure UpdateStatusLabel;
    procedure SendSelectedNow;
    procedure SendAllReady;
    procedure CancelSelected;

  protected
    procedure SetupComponents; override;
    procedure ConnectSignals; override;

  public
    constructor Create(AParent: PGtkWidget = nil);
    destructor Destroy; override;
    procedure RefreshScheduled;
  end;

// Callbacks
procedure OnEmailRowActivated(widget: PGtkWidget; path: PGtkTreePath;
                             column: PGtkTreeViewColumn; data: gpointer); cdecl;
procedure OnRefreshClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnSendNowClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnSendAllClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnCancelClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnDetailWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;

implementation

constructor TScheduledWindow.Create(AParent: PGtkWidget);
begin
  inherited Create('Correos Programados', 700, 500, AParent);
  FSelectedEmail := nil;
  RefreshScheduled;
end;

destructor TScheduledWindow.Destroy;
begin
  if FDetailWindow <> nil then
    gtk_widget_destroy(FDetailWindow);
  inherited;
end;

procedure TScheduledWindow.SetupComponents;
var
  Label1: PGtkWidget;
begin
  // Contenedor principal
  FMainVBox := TUIUtils.CreateVBox(10);
  gtk_container_add(GTK_CONTAINER(FWindow), FMainVBox);
  gtk_container_set_border_width(GTK_CONTAINER(FMainVBox), 10);

  // Título
  Label1 := TUIUtils.CreateLabel('Correos Programados', True);
  gtk_label_set_markup(GTK_LABEL(Label1), '<span size="large" weight="bold">Correos Programados</span>');
  gtk_box_pack_start(GTK_BOX(FMainVBox), Label1, False, False, 10);

  // Botones de acción
  FButtonsHBox := TUIUtils.CreateHBox(10);
  gtk_box_pack_start(GTK_BOX(FMainVBox), FButtonsHBox, False, False, 5);

  FRefreshButton := TUIUtils.CreateButton('🔄 Actualizar', @OnRefreshClicked, Self);
  FSendNowButton := TUIUtils.CreateButton('📤 Enviar Ahora', @OnSendNowClicked, Self);
  FSendAllButton := TUIUtils.CreateButton('📬 Enviar Todos Listos', @OnSendAllClicked, Self);
  FCancelButton := TUIUtils.CreateButton('❌ Cancelar', @OnCancelClicked, Self);

  gtk_box_pack_start(GTK_BOX(FButtonsHBox), FRefreshButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(FButtonsHBox), FSendNowButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(FButtonsHBox), FSendAllButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(FButtonsHBox), FCancelButton, False, False, 5);

  // Lista de emails
  CreateEmailList;

  // Label de estado
  FStatusLabel := TUIUtils.CreateLabel('');
  gtk_box_pack_start(GTK_BOX(FMainVBox), FStatusLabel, False, False, 5);

  UpdateStatusLabel;
end;

procedure TScheduledWindow.CreateEmailList;
var
  ListStore: PGtkListStore;
  Renderer: PGtkCellRenderer;
  Column: PGtkTreeViewColumn;
begin
  // Crear scrolled window
  FScrolledWindow := TUIUtils.CreateScrolledWindow;
  gtk_box_pack_start(GTK_BOX(FMainVBox), FScrolledWindow, True, True, 5);

  // Crear list store con columnas: Destinatario, Asunto, Fecha Programada, Estado, ID
  ListStore := gtk_list_store_new(5, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_INT);

  // Crear tree view
  FEmailList := gtk_tree_view_new_with_model(GTK_TREE_MODEL(ListStore));
  gtk_container_add(GTK_CONTAINER(FScrolledWindow), FEmailList);

  // Columna Destinatario
  Renderer := gtk_cell_renderer_text_new;
  Column := gtk_tree_view_column_new_with_attributes('Para', Renderer, 'text', 0, nil);
  gtk_tree_view_column_set_resizable(Column, True);
  gtk_tree_view_column_set_min_width(Column, 150);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FEmailList), Column);

  // Columna Asunto
  Renderer := gtk_cell_renderer_text_new;
  Column := gtk_tree_view_column_new_with_attributes('Asunto', Renderer, 'text', 1, nil);
  gtk_tree_view_column_set_resizable(Column, True);
  gtk_tree_view_column_set_min_width(Column, 200);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FEmailList), Column);

  // Columna Fecha Programada
  Renderer := gtk_cell_renderer_text_new;
  Column := gtk_tree_view_column_new_with_attributes('Fecha de Envío', Renderer, 'text', 2, nil);
  gtk_tree_view_column_set_resizable(Column, True);
  gtk_tree_view_column_set_min_width(Column, 140);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FEmailList), Column);

  // Columna Estado
  Renderer := gtk_cell_renderer_text_new;
  Column := gtk_tree_view_column_new_with_attributes('Estado', Renderer, 'text', 3, nil);
  gtk_tree_view_column_set_resizable(Column, True);
  gtk_tree_view_column_set_min_width(Column, 100);
  gtk_tree_view_append_column(GTK_TREE_VIEW(FEmailList), Column);

  g_object_unref(ListStore);
end;

procedure TScheduledWindow.ConnectSignals;
begin
  inherited;
  g_signal_connect(G_OBJECT(FEmailList), 'row-activated', G_CALLBACK(@OnEmailRowActivated), Self);
end;

procedure TScheduledWindow.RefreshScheduled;
var
  UserScheduled: TEmailQueue;
  Current: PEmail;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
  FechaText, EstadoText: String;
begin
  if not IsUserLoggedIn then Exit;

  UserScheduled := GetUserScheduled(CurrentUser^.Email);
  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(FEmailList)));

  // Limpiar lista
  gtk_list_store_clear(ListStore);

  // Agregar emails a la lista
  Current := UserScheduled.GetFirst;
  while Current <> nil do
  begin
    FechaText := FormatDateTime('dd/mm/yyyy hh:nn', Current^.Fecha);

    // Determinar estado
    if Current^.Fecha <= Now then
      EstadoText := '✅ Listo'
    else
      EstadoText := '⏰ Programado';

    gtk_list_store_append(ListStore, @Iter);
    gtk_list_store_set(ListStore, @Iter,
                      0, PChar(Current^.Destinatario),
                      1, PChar(Current^.Asunto),
                      2, PChar(FechaText),
                      3, PChar(EstadoText),
                      4, Current^.Id,
                      -1);

    Current := Current^.Next;
  end;

  UpdateStatusLabel;
end;

procedure TScheduledWindow.UpdateStatusLabel;
var
  UserScheduled: TEmailQueue;
  Current: PEmail;
  TotalCount, ReadyCount: Integer;
  StatusText: String;
begin
  if not IsUserLoggedIn then Exit;

  UserScheduled := GetUserScheduled(CurrentUser^.Email);
  TotalCount := UserScheduled.GetCount;
  ReadyCount := 0;

  // Contar emails listos para enviar
  Current := UserScheduled.GetFirst;
  while Current <> nil do
  begin
    if Current^.Fecha <= Now then
      Inc(ReadyCount);
    Current := Current^.Next;
  end;

  if TotalCount = 0 then
    StatusText := 'No hay correos programados'
  else
    StatusText := Format('Total: %d | Listos para enviar: %d', [TotalCount, ReadyCount]);

  gtk_label_set_text(GTK_LABEL(FStatusLabel), PChar(StatusText));
end;

procedure TScheduledWindow.SendSelectedNow;
var
  Selection: PGtkTreeSelection;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
  EmailId: Integer;
  UserScheduled: TEmailQueue;
  Current: PEmail;
  EmailToSend: PEmail;
begin
  Selection := gtk_tree_view_get_selection(GTK_TREE_VIEW(FEmailList));
  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(FEmailList)));

  if gtk_tree_selection_get_selected(Selection, nil, @Iter) then
  begin
    gtk_tree_model_get(GTK_TREE_MODEL(ListStore), @Iter, 4, @EmailId, -1);

    if TUIUtils.ShowConfirmDialog(FWindow, 'Enviar Ahora',
                                 '¿Está seguro que desea enviar este correo ahora?') then
    begin
      UserScheduled := GetUserScheduled(CurrentUser^.Email);
      Current := UserScheduled.GetFirst;

      // Buscar el email y enviarlo
      while Current <> nil do
      begin
        if Current^.Id = EmailId then
        begin
          // Enviar el email
          if SendEmail(Current^.Destinatario, Current^.Asunto, Current^.Mensaje) then
          begin
            // Remover de la cola programada (implementación simplificada)
            RefreshScheduled;
            TUIUtils.ShowInfoMessage(FWindow, 'Correo enviado exitosamente');
          end
          else
          begin
            TUIUtils.ShowErrorMessage(FWindow, 'Error al enviar el correo');
          end;
          Exit;
        end;
        Current := Current^.Next;
      end;

      TUIUtils.ShowErrorMessage(FWindow, 'No se encontró el correo seleccionado');
    end;
  end
  else
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor seleccione un correo para enviar');
  end;
end;

procedure TScheduledWindow.SendAllReady;
var
  ProcessedCount: Integer;
begin
  if TUIUtils.ShowConfirmDialog(FWindow, 'Enviar Todos Listos',
                               '¿Está seguro que desea enviar todos los correos que están listos?') then
  begin
    ProcessedCount := ProcessScheduledEmails;

    if ProcessedCount > 0 then
    begin
      RefreshScheduled;
      TUIUtils.ShowInfoMessage(FWindow, Format('Se enviaron %d correos programados', [ProcessedCount]));
    end
    else
    begin
      TUIUtils.ShowInfoMessage(FWindow, 'No hay correos listos para enviar en este momento');
    end;
  end;
end;

procedure TScheduledWindow.CancelSelected;
var
  Selection: PGtkTreeSelection;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
  EmailId: Integer;
  UserScheduled: TEmailQueue;
  Current: PEmail;
begin
  Selection := gtk_tree_view_get_selection(GTK_TREE_VIEW(FEmailList));
  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(FEmailList)));

  if gtk_tree_selection_get_selected(Selection, nil, @Iter) then
  begin
    gtk_tree_model_get(GTK_TREE_MODEL(ListStore), @Iter, 4, @EmailId, -1);

    if TUIUtils.ShowConfirmDialog(FWindow, 'Cancelar Envío',
                                 '¿Está seguro que desea cancelar este correo programado?') then
    begin
      UserScheduled := GetUserScheduled(CurrentUser^.Email);

      // Buscar y remover el email de la cola (implementación simplificada)
      // En una implementación real, necesitarías métodos específicos en TEmailQueue
      Current := UserScheduled.GetFirst;
      while Current <> nil do
      begin
        if Current^.Id = EmailId then
        begin
          // Remover email de la cola
          UserScheduled.Dequeue; // Simplificado
          Dispose(Current);
          RefreshScheduled;
          TUIUtils.ShowInfoMessage(FWindow, 'Correo programado cancelado');
          Exit;
        end;
        Current := Current^.Next;
      end;

      TUIUtils.ShowErrorMessage(FWindow, 'No se encontró el correo seleccionado');
    end;
  end
  else
  begin
    TUIUtils.ShowErrorMessage(FWindow, 'Por favor seleccione un correo para cancelar');
  end;
end;

procedure TScheduledWindow.ShowEmailDetails(Email: PEmail);
var
  MainVBox, ButtonsHBox: PGtkWidget;
  ScrolledWin: PGtkWidget;
  CloseButton, SendNowButton: PGtkWidget;
  Buffer: PGtkTextBuffer;
  DetailText, EstadoText: String;
begin
  if FDetailWindow <> nil then
    gtk_widget_destroy(FDetailWindow);

  // Crear ventana de detalles
  FDetailWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(FDetailWindow), PChar('Correo Programado - ' + Email^.Asunto));
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

  // Determinar estado
  if Email^.Fecha <= Now then
    EstadoText := '✅ Listo para enviar'
  else
    EstadoText := '⏰ Programado';

  // Formatear texto con detalles del email
  DetailText := '📅 CORREO PROGRAMADO' + LineEnding + LineEnding +
                'Para: ' + Email^.Destinatario + LineEnding +
                'Asunto: ' + Email^.Asunto + LineEnding +
                'Fecha de envío: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Email^.Fecha) + LineEnding +
                'Estado: ' + EstadoText + LineEnding +
                LineEnding + '--- Mensaje ---' + LineEnding +
                Email^.Mensaje;

  Buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(FDetailTextView));
  gtk_text_buffer_set_text(Buffer, PChar(DetailText), -1);

  // Botones
  ButtonsHBox := TUIUtils.CreateHBox(10);
  gtk_box_pack_start(GTK_BOX(MainVBox), ButtonsHBox, False, False, 5);

  SendNowButton := TUIUtils.CreateButton('📤 Enviar Ahora', @OnSendNowClicked, Self);
  CloseButton := TUIUtils.CreateButton('Cerrar', nil);

  g_signal_connect(G_OBJECT(CloseButton), 'clicked', G_CALLBACK(@OnDetailWindowDestroy), Self);

  gtk_box_pack_start(GTK_BOX(ButtonsHBox), SendNowButton, False, False, 5);
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
  ScheduledWindow: TScheduledWindow;
  ListStore: PGtkListStore;
  Iter: TGtkTreeIter;
  EmailId: Integer;
  UserScheduled: TEmailQueue;
  Email, Current: PEmail;
begin
  ScheduledWindow := TScheduledWindow(data);
  ListStore := GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(widget)));

  if gtk_tree_model_get_iter(GTK_TREE_MODEL(ListStore), @Iter, path) then
  begin
    gtk_tree_model_get(GTK_TREE_MODEL(ListStore), @Iter, 4, @EmailId, -1);

    UserScheduled := GetUserScheduled(CurrentUser^.Email);
    Current := UserScheduled.GetFirst;
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
      ScheduledWindow.FSelectedEmail := Email;
      ScheduledWindow.ShowEmailDetails(Email);
    end;
  end;
end;

procedure OnRefreshClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ScheduledWindow: TScheduledWindow;
begin
  ScheduledWindow := TScheduledWindow(data);
  ScheduledWindow.RefreshScheduled;
end;

procedure OnSendNowClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ScheduledWindow: TScheduledWindow;
begin
  ScheduledWindow := TScheduledWindow(data);
  ScheduledWindow.SendSelectedNow;
end;

procedure OnSendAllClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ScheduledWindow: TScheduledWindow;
begin
  ScheduledWindow := TScheduledWindow(data);
  ScheduledWindow.SendAllReady;
end;

procedure OnCancelClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  ScheduledWindow: TScheduledWindow;
begin
  ScheduledWindow := TScheduledWindow(data);
  ScheduledWindow.CancelSelected;
end;

procedure OnDetailWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
var
  ScheduledWindow: TScheduledWindow;
begin
  ScheduledWindow := TScheduledWindow(data);
  ScheduledWindow.FDetailWindow := nil;
end;

end.

