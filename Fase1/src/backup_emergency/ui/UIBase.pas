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

  // Utilidades para crear widgets comúnmente usados
  TUIUtils = class
  public
    class function CreateLabel(Text: String; Bold: Boolean = False): PGtkWidget;
    class function CreateButton(Text: String; Callback: Pointer; Data: Pointer = nil): PGtkWidget;
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
  if FWindow <> nil then
    gtk_window_set_position(GTK_WINDOW(FWindow), GTK_WIN_POS_CENTER);
end;

// ============================================================================
// TUIUtils Implementation
// ============================================================================

class function TUIUtils.CreateLabel(Text: String; Bold: Boolean): PGtkWidget;
begin
  Result := gtk_label_new(PChar(Text));
  if Bold then
    gtk_label_set_markup(GTK_LABEL(Result), PChar('<b>' + Text + '</b>'));
  gtk_misc_set_alignment(GTK_MISC(Result), 0.0, 0.5);
end;

class function TUIUtils.CreateButton(Text: String; Callback: Pointer; Data: Pointer): PGtkWidget;
begin
  Result := gtk_button_new_with_label(PChar(Text));
  if Callback <> nil then
    g_signal_connect(G_OBJECT(Result), 'clicked', G_CALLBACK(Callback), Data);
end;

class function TUIUtils.CreateEntry(Placeholder: String): PGtkWidget;
begin
  Result := gtk_entry_new;
  if Placeholder <> '' then
  begin
    // GTK2 no tiene placeholder nativo, pero podemos simular con texto
    gtk_entry_set_text(GTK_ENTRY(Result), PChar(Placeholder));
  end;
end;

class function TUIUtils.CreateTextView: PGtkWidget;
begin
  Result := gtk_text_view_new;
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(Result), GTK_WRAP_WORD);
end;

class function TUIUtils.CreateFrame(Title: String): PGtkWidget;
begin
  Result := gtk_frame_new(PChar(Title));
  gtk_container_set_border_width(GTK_CONTAINER(Result), 5);
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

class function TUIUtils.ShowMessageDialog(Parent: PGtkWidget; MessageType: TGtkMessageType;
                                        Title, Message: String): Integer;
var
  Dialog: PGtkWidget;
begin
  Dialog := gtk_message_dialog_new(GTK_WINDOW(Parent),
                                  GTK_DIALOG_MODAL,
                                  MessageType,
                                  GTK_BUTTONS_OK,
                                  PChar(Message));
  gtk_window_set_title(GTK_WINDOW(Dialog), PChar(Title));

  Result := gtk_dialog_run(GTK_DIALOG(Dialog));
  gtk_widget_destroy(Dialog);
end;

class function TUIUtils.ShowConfirmDialog(Parent: PGtkWidget; Title, Message: String): Boolean;
var
  Dialog: PGtkWidget;
  Response: Integer;
begin
  Dialog := gtk_message_dialog_new(GTK_WINDOW(Parent),
                                  GTK_DIALOG_MODAL,
                                  GTK_MESSAGE_QUESTION,
                                  GTK_BUTTONS_YES_NO,
                                  PChar(Message));
  gtk_window_set_title(GTK_WINDOW(Dialog), PChar(Title));

  Response := gtk_dialog_run(GTK_DIALOG(Dialog));
  gtk_widget_destroy(Dialog);

  Result := (Response = GTK_RESPONSE_YES);
end;

class procedure TUIUtils.ShowInfoMessage(Parent: PGtkWidget; Message: String);
begin
  ShowMessageDialog(Parent, GTK_MESSAGE_INFO, 'Información', Message);
end;

class procedure TUIUtils.ShowErrorMessage(Parent: PGtkWidget; Message: String);
begin
  ShowMessageDialog(Parent, GTK_MESSAGE_ERROR, 'Error', Message);
end;

end.
