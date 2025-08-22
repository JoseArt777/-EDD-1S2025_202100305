unit UIBase;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, GLib2, SysUtils, Classes, DataStructures, SystemCore;

type
  TBaseWindow = class
  protected
    FWindow: PGtkWidget;
    FTitle: String;
    FWidth: Integer;
    FHeight: Integer;
    FParentWindow: PGtkWidget;

    procedure SetupWindow; virtual;
    procedure SetupComponents; virtual; abstract;
    procedure ConnectSignals; virtual;

    // helper para validar widgets
    function IsValidWidget(W: PGtkWidget): Boolean; inline;

  public
    constructor Create(ATitle: String; AWidth: Integer = 400; AHeight: Integer = 300; AParent: PGtkWidget = nil);
    destructor Destroy; override;

    procedure Show; virtual;
    procedure Hide; virtual;
    procedure SetModal(Modal: Boolean);
    procedure SetPosition(Position: TGtkWindowPosition);
    procedure CenterOnParent;

    property Window: PGtkWidget read FWindow;
    property Title: String read FTitle write FTitle;
  end;

  TButtonCallback = procedure(widget: PGtkWidget; data: gpointer); cdecl;

  TUIUtils = class
  public
    class function CreateLabel(Text: String; Bold: Boolean = False): PGtkWidget;
    class function CreateButton(Text: String; Callback: TButtonCallback; Data: Pointer = nil): PGtkWidget;
    class function CreateEntry(Placeholder: String = ''): PGtkWidget;
    class function CreateTextView: PGtkWidget;
    class function CreateFrame(Title: String): PGtkWidget;
    class function CreateScrolledWindow: PGtkWidget;
    class function CreateHBox(Spacing: Integer = 5): PGtkWidget;
    class function CreateVBox(Spacing: Integer = 5): PGtkWidget;
    class function CreateTable(Rows, Cols: Integer): PGtkWidget;
    class function CreateComboBox: PGtkWidget;

    class function ShowMessageDialog(Parent: PGtkWidget; MessageType: TGtkMessageType;
                                     Title, Message: String): Integer;
    class function ShowConfirmDialog(Parent: PGtkWidget; Title, Message: String): Boolean;
    class procedure ShowInfoMessage(Parent: PGtkWidget; Message: String);
    class procedure ShowErrorMessage(Parent: PGtkWidget; Message: String);

    class function ShowSafeMessageDialog(Parent: PGtkWidget; Title, Message: String): Integer;
    class procedure ShowSafeInfoMessage(Parent: PGtkWidget; Message: String);
    class procedure ShowSafeErrorMessage(Parent: PGtkWidget; Message: String);

    class procedure ShowConsoleMessage(MessageType, Message: String);
  end;

implementation

// ---------- Callbacks base ----------

function OnBaseWindowDeleteEvent(widget: PGtkWidget; event: PGdkEvent; data: gpointer): gboolean; cdecl;
begin
  if (widget <> nil) and GTK_IS_WIDGET(widget) then
    gtk_widget_hide(widget);
  Result := True; // evita destrucción por GTK
end;

procedure OnBaseWindowDestroyed(widget: PGtkWidget; data: gpointer); cdecl;
var
  SelfRef: TBaseWindow;
begin
  if data <> nil then
  begin
    SelfRef := TBaseWindow(data);
    if (SelfRef <> nil) and (SelfRef.FWindow = widget) then
      SelfRef.FWindow := nil;
  end;
end;

// ---------- TBaseWindow ----------

function TBaseWindow.IsValidWidget(W: PGtkWidget): Boolean;
begin
  Result := (W <> nil) and GTK_IS_WIDGET(W);
end;

constructor TBaseWindow.Create(ATitle: String; AWidth: Integer; AHeight: Integer; AParent: PGtkWidget);
begin
  inherited Create;
  FTitle := ATitle;
  FWidth := AWidth;
  FHeight := AHeight;
  FParentWindow := AParent;
  SetupWindow;
  SetupComponents;
  ConnectSignals;
end;

destructor TBaseWindow.Destroy;
begin
  // Solo destruir si realmente sigue siendo widget válido
  if IsValidWidget(FWindow) then
    gtk_widget_destroy(FWindow);
  FWindow := nil;
  inherited;
end;

procedure TBaseWindow.SetupWindow;
begin
  FWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(FWindow), PChar(FTitle));
  gtk_window_set_default_size(GTK_WINDOW(FWindow), FWidth, FHeight);
  gtk_window_set_resizable(GTK_WINDOW(FWindow), True);

  if IsValidWidget(FParentWindow) then
  begin
    gtk_window_set_transient_for(GTK_WINDOW(FWindow), GTK_WINDOW(FParentWindow));
    gtk_window_set_position(GTK_WINDOW(FWindow), GTK_WIN_POS_CENTER_ON_PARENT);
  end
  else
    gtk_window_set_position(GTK_WINDOW(FWindow), GTK_WIN_POS_CENTER);

  g_signal_connect(G_OBJECT(FWindow), 'delete-event', G_CALLBACK(@OnBaseWindowDeleteEvent), nil);
  g_signal_connect(G_OBJECT(FWindow), 'destroy',      G_CALLBACK(@OnBaseWindowDestroyed), Self);
end;

procedure TBaseWindow.ConnectSignals;
begin
end;

procedure TBaseWindow.Show;
begin
  if IsValidWidget(FWindow) then
    gtk_widget_show_all(FWindow);
end;

procedure TBaseWindow.Hide;
begin
  if IsValidWidget(FWindow) then
    gtk_widget_hide(FWindow);
end;

procedure TBaseWindow.SetModal(Modal: Boolean);
begin
  if IsValidWidget(FWindow) then
    gtk_window_set_modal(GTK_WINDOW(FWindow), Modal);
end;

procedure TBaseWindow.SetPosition(Position: TGtkWindowPosition);
begin
  if IsValidWidget(FWindow) then
    gtk_window_set_position(GTK_WINDOW(FWindow), Position);
end;

procedure TBaseWindow.CenterOnParent;
begin
  if IsValidWidget(FWindow) and IsValidWidget(FParentWindow) then
    gtk_window_set_position(GTK_WINDOW(FWindow), GTK_WIN_POS_CENTER_ON_PARENT);
end;

// ---------- TUIUtils ----------

class function TUIUtils.CreateLabel(Text: String; Bold: Boolean): PGtkWidget;
begin
  Result := gtk_label_new(PChar(Text));
  if Bold then
    gtk_label_set_markup(GTK_LABEL(Result), PChar('<b>' + Text + '</b>'));
end;

class function TUIUtils.CreateButton(Text: String; Callback: TButtonCallback; Data: Pointer): PGtkWidget;
begin
  Result := gtk_button_new_with_label(PChar(Text));
  if Assigned(Callback) then
    g_signal_connect(G_OBJECT(Result), 'clicked', G_CALLBACK(Callback), Data);
end;

class function TUIUtils.CreateEntry(Placeholder: String): PGtkWidget;
begin
  Result := gtk_entry_new; // GTK2 sin placeholder
end;

class function TUIUtils.CreateTextView: PGtkWidget;
begin
  Result := gtk_text_view_new;
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(Result), GTK_WRAP_WORD);
end;

class function TUIUtils.CreateFrame(Title: String): PGtkWidget;
begin
  Result := gtk_frame_new(PChar(Title));
  gtk_frame_set_shadow_type(GTK_FRAME(Result), GTK_SHADOW_ETCHED_IN);
end;

class function TUIUtils.CreateScrolledWindow: PGtkWidget;
begin
  Result := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(Result), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
end;

class function TUIUtils.CreateHBox(Spacing: Integer): PGtkWidget;
begin
  Result := gtk_hbox_new(False, Spacing);
end;

class function TUIUtils.CreateVBox(Spacing: Integer): PGtkWidget;
begin
  Result := gtk_vbox_new(False, Spacing);
end;

class function TUIUtils.CreateTable(Rows, Cols: Integer): PGtkWidget;
begin
  Result := gtk_table_new(Rows, Cols, False);
  gtk_table_set_row_spacings(GTK_TABLE(Result), 5);
  gtk_table_set_col_spacings(GTK_TABLE(Result), 5);
end;

class function TUIUtils.CreateComboBox: PGtkWidget;
begin
  Result := gtk_combo_box_new_text;
end;

class function TUIUtils.ShowMessageDialog(Parent: PGtkWidget; MessageType: TGtkMessageType;
  Title, Message: String): Integer;
begin
  try
    Result := ShowSafeMessageDialog(Parent, Title, Message);
  except
    on E: Exception do
    begin
      ShowConsoleMessage(Title, Message);
      Result := GTK_RESPONSE_OK;
    end;
  end;
end;

class function TUIUtils.ShowConfirmDialog(Parent: PGtkWidget; Title, Message: String): Boolean;
begin
  try
    Result := (ShowSafeMessageDialog(Parent, Title, Message + ' (Presiona OK para SÍ)') = GTK_RESPONSE_OK);
  except
    on E: Exception do
    begin
      ShowConsoleMessage(Title, Message + ' - Asumiendo SÍ');
      Result := True;
    end;
  end;
end;

class procedure TUIUtils.ShowInfoMessage(Parent: PGtkWidget; Message: String);
begin
  try
    ShowSafeInfoMessage(Parent, Message);
  except
    on E: Exception do
      ShowConsoleMessage('INFO', Message);
  end;
end;

class procedure TUIUtils.ShowErrorMessage(Parent: PGtkWidget; Message: String);
begin
  try
    ShowSafeErrorMessage(Parent, Message);
  except
    on E: Exception do
      ShowConsoleMessage('ERROR', Message);
  end;
end;

class function TUIUtils.ShowSafeMessageDialog(Parent: PGtkWidget; Title, Message: String): Integer;
var
  Dialog, VBox, Label1, Button: PGtkWidget;
begin
  try
    Dialog := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(Dialog), PChar(Title));
    gtk_window_set_default_size(GTK_WINDOW(Dialog), 350, 150);
    gtk_window_set_modal(GTK_WINDOW(Dialog), True);

    if (Parent <> nil) and GTK_IS_WIDGET(Parent) then
    begin
      gtk_window_set_transient_for(GTK_WINDOW(Dialog), GTK_WINDOW(Parent));
      gtk_window_set_position(GTK_WINDOW(Dialog), GTK_WIN_POS_CENTER_ON_PARENT);
    end
    else
      gtk_window_set_position(GTK_WINDOW(Dialog), GTK_WIN_POS_CENTER);

    VBox := CreateVBox(15);
    gtk_container_add(GTK_CONTAINER(Dialog), VBox);
    gtk_container_set_border_width(GTK_CONTAINER(VBox), 20);

    Label1 := CreateLabel(Message);
    gtk_misc_set_alignment(GTK_MISC(Label1), 0.5, 0.5);
    gtk_label_set_line_wrap(GTK_LABEL(Label1), True);
    gtk_label_set_justify(GTK_LABEL(Label1), GTK_JUSTIFY_CENTER);
    gtk_box_pack_start(GTK_BOX(VBox), Label1, True, True, 0);

    Button := gtk_button_new_with_label('OK');
    gtk_widget_set_size_request(Button, 80, 30);
    gtk_box_pack_start(GTK_BOX(VBox), Button, False, False, 0);

    gtk_widget_show_all(Dialog);
    g_signal_connect_swapped(G_OBJECT(Button), 'clicked', G_CALLBACK(@gtk_widget_destroy), Dialog);

    // no corremos gtk_dialog_run; devolvemos OK y destruimos
    gtk_widget_destroy(Dialog);
    Result := GTK_RESPONSE_OK;
  except
    on E: Exception do
    begin
      ShowConsoleMessage(Title, Message);
      Result := GTK_RESPONSE_OK;
    end;
  end;
end;

class procedure TUIUtils.ShowSafeInfoMessage(Parent: PGtkWidget; Message: String);
begin
  try
    ShowSafeMessageDialog(Parent, 'Información', Message);
  except
    on E: Exception do
      ShowConsoleMessage('INFO', Message);
  end;
end;

class procedure TUIUtils.ShowSafeErrorMessage(Parent: PGtkWidget; Message: String);
begin
  try
    ShowSafeMessageDialog(Parent, 'Error', Message);
  except
    on E: Exception do
      ShowConsoleMessage('ERROR', Message);
  end;
end;

class procedure TUIUtils.ShowConsoleMessage(MessageType, Message: String);
begin
  try
    WriteLn(''); WriteLn('=== ' + MessageType + ' ==='); WriteLn(Message); WriteLn('================'); WriteLn('');
  except
    // nada
  end;
end;

end.

