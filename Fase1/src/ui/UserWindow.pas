unit UserWindow;

{$mode objfpc}{$H+}

interface

uses
  GTK2, GDK2, GLib2, SysUtils, Classes, DataStructures, SystemCore, UIBase,
  InboxWindow, ComposeWindow, ContactWindow, TrashWindow, ScheduleWindow,
  ScheduledWindow, ProfileWindow, ReportsWindow;

type
  TUserWindow = class(TBaseWindow)
  private
    FMainVBox: PGtkWidget;
    FMenuBar: PGtkWidget;
    FStatusBar: PGtkWidget;
    FWelcomeLabel: PGtkWidget;
    FUnreadLabel: PGtkWidget;

    // Botones del menú principal
    FInboxButton: PGtkWidget;
    FComposeButton: PGtkWidget;
    FContactsButton: PGtkWidget;
    FTrashButton: PGtkWidget;
    FScheduleButton: PGtkWidget;
    FScheduledButton: PGtkWidget;
    FProfileButton: PGtkWidget;
    FReportsButton: PGtkWidget;
    FLogoutButton: PGtkWidget;

    // Ventanas secundarias
    FInboxWin: TInboxWindow;
    FComposeWin: TComposeWindow;
    FContactWin: TContactWindow;
    FTrashWin: TTrashWindow;
    FScheduleWin: TScheduleWindow;
    FScheduledWin: TScheduledWindow;
    FProfileWin: TProfileWindow;
    FReportsWin: TReportsWindow;

    procedure CreateMenuButtons;
    procedure UpdateUnreadCount;
    procedure UpdateWelcomeMessage;

  protected
    procedure SetupComponents; override;
    procedure ConnectSignals; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure RefreshData;
    procedure CloseAllSubWindows;
  end;

// Callbacks
procedure OnInboxClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnComposeClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnContactsClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnTrashClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnScheduleClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnScheduledClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnProfileClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnReportsClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnLogoutClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnUserWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;

// Callbacks destroy de subventanas (para invalidar punteros)
procedure OnInboxWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnComposeWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnContactWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnTrashWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnScheduleWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnScheduledWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnProfileWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnReportsWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;

var
  UserWin: TUserWindow;

implementation

uses EmailManager, ContactManager, LoginWindow;

constructor TUserWindow.Create;
begin
  inherited Create('EDDMail - ' + CurrentUser^.Nombre, 800, 600);
  UpdateWelcomeMessage;
  UpdateUnreadCount;
end;

destructor TUserWindow.Destroy;
begin
  CloseAllSubWindows;
  inherited;
end;

procedure TUserWindow.SetupComponents;
var
  ButtonsHBox, ButtonsVBox1, ButtonsVBox2: PGtkWidget;
  Separator: PGtkWidget;
begin
  // Contenedor principal
  FMainVBox := TUIUtils.CreateVBox(10);
  gtk_container_add(GTK_CONTAINER(FWindow), FMainVBox);
  gtk_container_set_border_width(GTK_CONTAINER(FMainVBox), 10);

  // Mensaje de bienvenida
  FWelcomeLabel := TUIUtils.CreateLabel('', True);
  gtk_box_pack_start(GTK_BOX(FMainVBox), FWelcomeLabel, False, False, 10);

  // Contador de correos no leídos
  FUnreadLabel := TUIUtils.CreateLabel('');
  gtk_box_pack_start(GTK_BOX(FMainVBox), FUnreadLabel, False, False, 5);

  // Separador
  Separator := gtk_hseparator_new;
  gtk_box_pack_start(GTK_BOX(FMainVBox), Separator, False, False, 10);

  // Contenedor para botones
  ButtonsHBox := TUIUtils.CreateHBox(20);
  gtk_box_pack_start(GTK_BOX(FMainVBox), ButtonsHBox, True, True, 10);

  // Primera columna de botones
  ButtonsVBox1 := TUIUtils.CreateVBox(10);
  gtk_box_pack_start(GTK_BOX(ButtonsHBox), ButtonsVBox1, True, True, 10);

  // Segunda columna de botones
  ButtonsVBox2 := TUIUtils.CreateVBox(10);
  gtk_box_pack_start(GTK_BOX(ButtonsHBox), ButtonsVBox2, True, True, 10);

  // Crear botones del menú
  CreateMenuButtons;

  // Agrupar botones en columnas
  gtk_box_pack_start(GTK_BOX(ButtonsVBox1), FInboxButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(ButtonsVBox1), FComposeButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(ButtonsVBox1), FContactsButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(ButtonsVBox1), FTrashButton, False, False, 5);

  gtk_box_pack_start(GTK_BOX(ButtonsVBox2), FScheduleButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(ButtonsVBox2), FScheduledButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(ButtonsVBox2), FProfileButton, False, False, 5);
  gtk_box_pack_start(GTK_BOX(ButtonsVBox2), FReportsButton, False, False, 5);

  // Separador y botón de logout
  Separator := gtk_hseparator_new;
  gtk_box_pack_start(GTK_BOX(FMainVBox), Separator, False, False, 10);

  gtk_box_pack_start(GTK_BOX(FMainVBox), FLogoutButton, False, False, 5);

  // Barra de estado
  FStatusBar := gtk_statusbar_new;
  gtk_box_pack_start(GTK_BOX(FMainVBox), FStatusBar, False, False, 0);
  gtk_statusbar_push(GTK_STATUSBAR(FStatusBar), 0, Pgchar(UTF8String('Listo')));
end;

procedure TUserWindow.CreateMenuButtons;
begin
  FInboxButton     := TUIUtils.CreateButton('📥 Bandeja de Entrada',   @OnInboxClicked,      Self);
  FComposeButton   := TUIUtils.CreateButton('✉️ Enviar Correo',        @OnComposeClicked,    Self);
  FContactsButton  := TUIUtils.CreateButton('👥 Contactos',            @OnContactsClicked,   Self);
  FTrashButton     := TUIUtils.CreateButton('🗑️ Papelera',             @OnTrashClicked,      Self);
  FScheduleButton  := TUIUtils.CreateButton('⏰ Programar Correo',      @OnScheduleClicked,   Self);
  FScheduledButton := TUIUtils.CreateButton('📅 Correos Programados',  @OnScheduledClicked,  Self);
  FProfileButton   := TUIUtils.CreateButton('👤 Actualizar Perfil',     @OnProfileClicked,    Self);
  FReportsButton   := TUIUtils.CreateButton('📊 Generar Reportes',      @OnReportsClicked,    Self);
  FLogoutButton    := TUIUtils.CreateButton('🚪 Cerrar Sesión',         @OnLogoutClicked,     Self);

  // Configurar tamaños de botones
  gtk_widget_set_size_request(FInboxButton,     200, 40);
  gtk_widget_set_size_request(FComposeButton,   200, 40);
  gtk_widget_set_size_request(FContactsButton,  200, 40);
  gtk_widget_set_size_request(FTrashButton,     200, 40);
  gtk_widget_set_size_request(FScheduleButton,  200, 40);
  gtk_widget_set_size_request(FScheduledButton, 200, 40);
  gtk_widget_set_size_request(FProfileButton,   200, 40);
  gtk_widget_set_size_request(FReportsButton,   200, 40);
  gtk_widget_set_size_request(FLogoutButton,    300, 40);
end;

procedure TUserWindow.ConnectSignals;
begin
  inherited;
  g_signal_connect(G_OBJECT(FWindow), 'destroy', G_CALLBACK(@OnUserWindowDestroy), Self);
end;

procedure TUserWindow.UpdateUnreadCount;
var
  UserInbox: TEmailList;
  UnreadCount: Integer;
  Text: String;
begin
  if not IsUserLoggedIn then Exit;

  UserInbox := GetUserInbox(CurrentUser^.Email);
  UnreadCount := UserInbox.GetUnreadCount;

  if UnreadCount > 0 then
    Text := '📬 Tienes ' + IntToStr(UnreadCount) + ' correos no leídos'
  else
    Text := '✅ No tienes correos no leídos';

  gtk_label_set_text(GTK_LABEL(FUnreadLabel), Pgchar(UTF8String(Text)));
end;

procedure TUserWindow.UpdateWelcomeMessage;
var
  WelcomeText: String;
begin
  if IsUserLoggedIn then
  begin
    WelcomeText := Format('Bienvenido, %s (%s)', [CurrentUser^.Nombre, CurrentUser^.Email]);
    gtk_label_set_markup(GTK_LABEL(FWelcomeLabel),
      Pgchar(UTF8String('<span size="large" weight="bold">' + WelcomeText + '</span>')));
  end;
end;

procedure TUserWindow.RefreshData;
begin
  UpdateUnreadCount;
  // Refrescar otras ventanas si están abiertas
  if FInboxWin <> nil then
    FInboxWin.RefreshInbox;
  if FScheduledWin <> nil then
    FScheduledWin.RefreshScheduled;
end;

procedure TUserWindow.CloseAllSubWindows;
begin
  if FInboxWin <> nil then
  begin
    FInboxWin.Free;   FInboxWin := nil;
  end;
  if FComposeWin <> nil then
  begin
    FComposeWin.Free; FComposeWin := nil;
  end;
  if FContactWin <> nil then
  begin
    FContactWin.Free; FContactWin := nil;
  end;
  if FTrashWin <> nil then
  begin
    FTrashWin.Free;   FTrashWin := nil;
  end;
  if FScheduleWin <> nil then
  begin
    FScheduleWin.Free; FScheduleWin := nil;
  end;
  if FScheduledWin <> nil then
  begin
    FScheduledWin.Free; FScheduledWin := nil;
  end;
  if FProfileWin <> nil then
  begin
    FProfileWin.Free; FProfileWin := nil;
  end;
  if FReportsWin <> nil then
  begin
    FReportsWin.Free; FReportsWin := nil;
  end;
end;

// ============================================================================
// Helpers para recreación segura de subventanas (evita punteros zombie)
// ============================================================================

procedure EnsureInboxWindow(UW: TUserWindow);
begin
  if (UW.FInboxWin = nil) or (UW.FInboxWin.Window = nil) or (not GTK_IS_WIDGET(UW.FInboxWin.Window)) then
  begin
    if (UW.FInboxWin <> nil) then begin UW.FInboxWin.Free; UW.FInboxWin := nil; end;
    UW.FInboxWin := TInboxWindow.Create(UW.Window);
    g_signal_connect(UW.FInboxWin.Window, 'destroy', G_CALLBACK(@OnInboxWindowDestroy), UW);
  end;
end;

procedure EnsureComposeWindow(UW: TUserWindow);
begin
  if (UW.FComposeWin = nil) or (UW.FComposeWin.Window = nil) or (not GTK_IS_WIDGET(UW.FComposeWin.Window)) then
  begin
    if (UW.FComposeWin <> nil) then begin UW.FComposeWin.Free; UW.FComposeWin := nil; end;
    UW.FComposeWin := TComposeWindow.Create(UW.Window);
    g_signal_connect(UW.FComposeWin.Window, 'destroy', G_CALLBACK(@OnComposeWindowDestroy), UW);
  end;
end;

procedure EnsureContactWindow(UW: TUserWindow);
begin
  if (UW.FContactWin = nil) or (UW.FContactWin.Window = nil) or (not GTK_IS_WIDGET(UW.FContactWin.Window)) then
  begin
    if (UW.FContactWin <> nil) then begin UW.FContactWin.Free; UW.FContactWin := nil; end;
    UW.FContactWin := TContactWindow.Create(UW.Window);
    g_signal_connect(UW.FContactWin.Window, 'destroy', G_CALLBACK(@OnContactWindowDestroy), UW);
  end;
end;

procedure EnsureTrashWindow(UW: TUserWindow);
begin
  if (UW.FTrashWin = nil) or (UW.FTrashWin.Window = nil) or (not GTK_IS_WIDGET(UW.FTrashWin.Window)) then
  begin
    if (UW.FTrashWin <> nil) then begin UW.FTrashWin.Free; UW.FTrashWin := nil; end;
    UW.FTrashWin := TTrashWindow.Create(UW.Window);
    g_signal_connect(UW.FTrashWin.Window, 'destroy', G_CALLBACK(@OnTrashWindowDestroy), UW);
  end;
end;

procedure EnsureScheduleWindow(UW: TUserWindow);
begin
  if (UW.FScheduleWin = nil) or (UW.FScheduleWin.Window = nil) or (not GTK_IS_WIDGET(UW.FScheduleWin.Window)) then
  begin
    if (UW.FScheduleWin <> nil) then begin UW.FScheduleWin.Free; UW.FScheduleWin := nil; end;
    UW.FScheduleWin := TScheduleWindow.Create(UW.Window);
    g_signal_connect(UW.FScheduleWin.Window, 'destroy', G_CALLBACK(@OnScheduleWindowDestroy), UW);
  end;
end;

procedure EnsureScheduledWindow(UW: TUserWindow);
begin
  if (UW.FScheduledWin = nil) or (UW.FScheduledWin.Window = nil) or (not GTK_IS_WIDGET(UW.FScheduledWin.Window)) then
  begin
    if (UW.FScheduledWin <> nil) then begin UW.FScheduledWin.Free; UW.FScheduledWin := nil; end;
    UW.FScheduledWin := TScheduledWindow.Create(UW.Window);
    g_signal_connect(UW.FScheduledWin.Window, 'destroy', G_CALLBACK(@OnScheduledWindowDestroy), UW);
  end;
end;

procedure EnsureProfileWindow(UW: TUserWindow);
begin
  if (UW.FProfileWin = nil) or (UW.FProfileWin.Window = nil) or (not GTK_IS_WIDGET(UW.FProfileWin.Window)) then
  begin
    if (UW.FProfileWin <> nil) then begin UW.FProfileWin.Free; UW.FProfileWin := nil; end;
    UW.FProfileWin := TProfileWindow.Create(UW.Window);
    g_signal_connect(UW.FProfileWin.Window, 'destroy', G_CALLBACK(@OnProfileWindowDestroy), UW);
  end;
end;

procedure EnsureReportsWindow(UW: TUserWindow);
begin
  if (UW.FReportsWin = nil) or (UW.FReportsWin.Window = nil) or (not GTK_IS_WIDGET(UW.FReportsWin.Window)) then
  begin
    if (UW.FReportsWin <> nil) then begin UW.FReportsWin.Free; UW.FReportsWin := nil; end;
    UW.FReportsWin := TReportsWindow.Create(UW.Window);
    g_signal_connect(UW.FReportsWin.Window, 'destroy', G_CALLBACK(@OnReportsWindowDestroy), UW);
  end;
end;

// ============================================================================
// Callbacks
// ============================================================================

procedure OnInboxClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UW: TUserWindow;
begin
  UW := TUserWindow(data);
  EnsureInboxWindow(UW);
  UW.FInboxWin.Show;
  UW.FInboxWin.RefreshInbox;
end;

procedure OnComposeClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UW: TUserWindow;
begin
  UW := TUserWindow(data);
  EnsureComposeWindow(UW);
  UW.FComposeWin.Show;
end;

procedure OnContactsClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UW: TUserWindow;
begin
  UW := TUserWindow(data);
  EnsureContactWindow(UW);
  UW.FContactWin.Show;
end;

procedure OnTrashClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UW: TUserWindow;
begin
  UW := TUserWindow(data);
  EnsureTrashWindow(UW);
  UW.FTrashWin.Show;
  UW.FTrashWin.RefreshTrash;
end;

procedure OnScheduleClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UW: TUserWindow;
begin
  UW := TUserWindow(data);
  EnsureScheduleWindow(UW);
  UW.FScheduleWin.Show;
end;

procedure OnScheduledClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UW: TUserWindow;
begin
  UW := TUserWindow(data);
  EnsureScheduledWindow(UW);
  UW.FScheduledWin.Show;
  UW.FScheduledWin.RefreshScheduled;
end;

procedure OnProfileClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UW: TUserWindow;
begin
  UW := TUserWindow(data);
  EnsureProfileWindow(UW);

  if (UW.FProfileWin = nil) or (UW.FProfileWin.Window = nil) or (not GTK_IS_WIDGET(UW.FProfileWin.Window)) then
  begin
    TUIUtils.ShowErrorMessage(UW.Window, 'No se pudo abrir la ventana de perfil (widget inválido).');
    Exit;
  end;

  UW.FProfileWin.Show;
  UW.FProfileWin.LoadCurrentUser;
end;

procedure OnReportsClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UW: TUserWindow;
begin
  UW := TUserWindow(data);
  EnsureReportsWindow(UW);
  UW.FReportsWin.Show;
end;

procedure OnLogoutClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  UW: TUserWindow;
begin
  UW := TUserWindow(data);

  if TUIUtils.ShowConfirmDialog(UW.Window, 'Cerrar Sesión',
                               '¿Está seguro que desea cerrar sesión?') then
  begin
    UW.CloseAllSubWindows;
    LogoutUser;
    UW.Hide;

    // Mostrar ventana de login
    if LoginWin = nil then
      LoginWin := TLoginWindow.Create;
    LoginWin.Show;
  end;
end;

procedure OnUserWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if gtk_main_level > 0 then
    gtk_main_quit;
end;

// ============================================================================
// Callbacks destroy de subventanas (invalidan punteros en TUserWindow)
// ============================================================================

procedure OnInboxWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if data <> nil then TUserWindow(data).FInboxWin := nil;
end;

procedure OnComposeWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if data <> nil then TUserWindow(data).FComposeWin := nil;
end;

procedure OnContactWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if data <> nil then TUserWindow(data).FContactWin := nil;
end;

procedure OnTrashWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if data <> nil then TUserWindow(data).FTrashWin := nil;
end;

procedure OnScheduleWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if data <> nil then TUserWindow(data).FScheduleWin := nil;
end;

procedure OnScheduledWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if data <> nil then TUserWindow(data).FScheduledWin := nil;
end;

procedure OnProfileWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if data <> nil then TUserWindow(data).FProfileWin := nil;
end;

procedure OnReportsWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if data <> nil then TUserWindow(data).FReportsWin := nil;
end;

end.

