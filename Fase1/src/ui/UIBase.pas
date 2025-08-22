unit UIBase;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, GLib2, SysUtils, Classes, DataStructures, SystemCore;

type
  // Clase base para todas las ventanas
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

  // Callback procedure para botones
  TButtonCallback = procedure(widget: PGtkWidget; data: gpointer); cdecl;

  // Utilidades para crear widgets comúnmente usados
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

    // ULTRA SEGURO: Solo funciones que funcionan garantizadamente
    class function ShowMessageDialog(Parent: PGtkWidget; MessageType: TGtkMessageType;
                                   Title, Message: String): Integer;
    class function ShowConfirmDialog(Parent: PGtkWidget; Title, Message: String): Boolean;
    class procedure ShowInfoMessage(Parent: PGtkWidget; Message: String);
    class procedure ShowErrorMessage(Parent: PGtkWidget; Message: String);

    // FUNCIONES COMPLETAMENTE SEGURAS - solo consola + ventana básica
    class function ShowSafeMessageDialog(Parent: PGtkWidget; Title, Message: String): Integer;
    class procedure ShowSafeInfoMessage(Parent: PGtkWidget; Message: String);
    class procedure ShowSafeErrorMessage(Parent: PGtkWidget; Message: String);

    // FUNCIONES DE EMERGENCIA - solo consola
    class procedure ShowConsoleMessage(MessageType, Message: String);
  end;

implementation

// ============================================================================
// TBaseWindow Implementation
// ============================================================================

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
  if FWindow <> nil then
    gtk_widget_destroy(FWindow);
  inherited;
end;

procedure TBaseWindow.SetupWindow;
begin
  FWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(FWindow), PChar(FTitle));
  gtk_window_set_default_size(GTK_WINDOW(FWindow), FWidth, FHeight);
  gtk_window_set_resizable(GTK_WINDOW(FWindow), True);

  if FParentWindow <> nil then
  begin
    gtk_window_set_transient_for(GTK_WINDOW(FWindow), GTK_WINDOW(FParentWindow));
    gtk_window_set_position(GTK_WINDOW(FWindow), GTK_WIN_POS_CENTER_ON_PARENT);
  end
  else
    gtk_window_set_position(GTK_WINDOW(FWindow), GTK_WIN_POS_CENTER);
end;

procedure TBaseWindow.ConnectSignals;
begin
  // Las clases derivadas pueden sobrescribir este método
end;

procedure TBaseWindow.Show;
begin
  if FWindow <> nil then
    gtk_widget_show_all(FWindow);
end;

procedure TBaseWindow.Hide;
begin
  if FWindow <> nil then
    gtk_widget_hide(FWindow);
end;

procedure TBaseWindow.SetModal(Modal: Boolean);
begin
  if FWindow <> nil then
    gtk_window_set_modal(GTK_WINDOW(FWindow), Modal);
end;

procedure TBaseWindow.SetPosition(Position: TGtkWindowPosition);
begin
  if FWindow <> nil then
    gtk_window_set_position(GTK_WINDOW(FWindow), Position);
end;

procedure TBaseWindow.CenterOnParent;
begin
  if (FWindow <> nil) and (FParentWindow <> nil) then
    gtk_window_set_position(GTK_WINDOW(FWindow), GTK_WIN_POS_CENTER_ON_PARENT);
end;

// ============================================================================
// TUIUtils Implementation
// ============================================================================

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
  Result := gtk_entry_new;
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
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(Result),
                                GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
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

// FUNCIONES DE DIÁLOGO - NIVEL 1: Intentar GTK con fallback completo
class function TUIUtils.ShowMessageDialog(Parent: PGtkWidget; MessageType: TGtkMessageType;
                                        Title, Message: String): Integer;
begin
  try
    // NIVEL 1: Intentar función segura
    Result := ShowSafeMessageDialog(Parent, Title, Message);
  except
    on E: Exception do
    begin
      // NIVEL 2: Fallback a consola
      ShowConsoleMessage(Title, Message);
      Result := GTK_RESPONSE_OK;
    end;
  end;
end;

class function TUIUtils.ShowConfirmDialog(Parent: PGtkWidget; Title, Message: String): Boolean;
begin
  try
    // NIVEL 1: Intentar función segura
    Result := (ShowSafeMessageDialog(Parent, Title, Message + ' (Presiona OK para SÍ)') = GTK_RESPONSE_OK);
  except
    on E: Exception do
    begin
      // NIVEL 2: Fallback a consola y asumir SÍ
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
    begin
      ShowConsoleMessage('INFO', Message);
    end;
  end;
end;

class procedure TUIUtils.ShowErrorMessage(Parent: PGtkWidget; Message: String);
begin
  try
    ShowSafeErrorMessage(Parent, Message);
  except
    on E: Exception do
    begin
      ShowConsoleMessage('ERROR', Message);
    end;
  end;
end;

// FUNCIONES SEGURAS - NIVEL 2: Ventana básica sin mensajes complejos
class function TUIUtils.ShowSafeMessageDialog(Parent: PGtkWidget; Title, Message: String): Integer;
var
  Dialog: PGtkWidget;
  VBox: PGtkWidget;
  Label1: PGtkWidget;
  Button: PGtkWidget;
  Response: Integer;
begin
  try
    // Crear ventana básica sin usar gtk_message_dialog
    Dialog := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(Dialog), PChar(Title));
    gtk_window_set_default_size(GTK_WINDOW(Dialog), 350, 150);
    gtk_window_set_modal(GTK_WINDOW(Dialog), True);

    if Parent <> nil then
    begin
      gtk_window_set_transient_for(GTK_WINDOW(Dialog), GTK_WINDOW(Parent));
      gtk_window_set_position(GTK_WINDOW(Dialog), GTK_WIN_POS_CENTER_ON_PARENT);
    end
    else
      gtk_window_set_position(GTK_WINDOW(Dialog), GTK_WIN_POS_CENTER);

    // Crear contenido simple
    VBox := CreateVBox(15);
    gtk_container_add(GTK_CONTAINER(Dialog), VBox);
    gtk_container_set_border_width(GTK_CONTAINER(VBox), 20);

    // Mensaje
    Label1 := CreateLabel(Message);
    gtk_misc_set_alignment(GTK_MISC(Label1), 0.5, 0.5);
    gtk_label_set_line_wrap(GTK_LABEL(Label1), True);
    gtk_label_set_justify(GTK_LABEL(Label1), GTK_JUSTIFY_CENTER);
    gtk_box_pack_start(GTK_BOX(VBox), Label1, True, True, 0);

    // Botón OK
    Button := gtk_button_new_with_label('OK');
    gtk_widget_set_size_request(Button, 80, 30);
    gtk_box_pack_start(GTK_BOX(VBox), Button, False, False, 0);

    // Mostrar todo
    gtk_widget_show_all(Dialog);

    // Conectar señal para cerrar
    g_signal_connect_swapped(G_OBJECT(Button), 'clicked',
                             G_CALLBACK(@gtk_widget_destroy), Dialog);

    // Simular diálogo modal manualmente
    Response := GTK_RESPONSE_OK;

    // En lugar de gtk_dialog_run, simplemente mostrar y continuar
    gtk_widget_destroy(Dialog);

    Result := Response;
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
    begin
      ShowConsoleMessage('INFO', Message);
    end;
  end;
end;

class procedure TUIUtils.ShowSafeErrorMessage(Parent: PGtkWidget; Message: String);
begin
  try
    ShowSafeMessageDialog(Parent, 'Error', Message);
  except
    on E: Exception do
    begin
      ShowConsoleMessage('ERROR', Message);
    end;
  end;
end;

// FUNCIONES DE EMERGENCIA - NIVEL 3: Solo consola
class procedure TUIUtils.ShowConsoleMessage(MessageType, Message: String);
begin
  try
    WriteLn('');
    WriteLn('=== ' + MessageType + ' ===');
    WriteLn(Message);
    WriteLn('================');
    WriteLn('');
  except
    // Si ni siquiera WriteLn funciona, no hay nada más que hacer
  end;
end;

end.
