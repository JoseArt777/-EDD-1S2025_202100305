unit LoginWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTK2, GDK2, SystemCore, DataStructures;

type
  TLoginWindow = class
  private
    FWindow: PGtkWidget;
    FEmailEntry: PGtkWidget;
    FPasswordEntry: PGtkWidget;
    FLoginButton: PGtkWidget;
    FRegisterButton: PGtkWidget;
    FStatusLabel: PGtkWidget;

    // Campos para registro
    FRegisterWindow: PGtkWidget;
    FRegNombreEntry: PGtkWidget;
    FRegUsuarioEntry: PGtkWidget;
    FRegEmailEntry: PGtkWidget;
    FRegTelefonoEntry: PGtkWidget;
    FRegPasswordEntry: PGtkWidget;
    FRegConfirmEntry: PGtkWidget;

    procedure CreateLoginWindow;
    procedure CreateRegisterWindow;
    procedure ShowMessage(const Msg: String);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
  end;

// Callbacks
procedure OnLoginClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnRegisterClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnCreateAccountClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnCancelRegisterClicked(widget: PGtkWidget; data: gpointer); cdecl;
procedure OnLoginWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;

var
  LoginWin: TLoginWindow;

implementation

uses
  RootWindow, UserWindow;

constructor TLoginWindow.Create;
begin
  CreateLoginWindow;
  CreateRegisterWindow;
end;

destructor TLoginWindow.Destroy;
begin
  if FWindow <> nil then
    gtk_widget_destroy(FWindow);
  if FRegisterWindow <> nil then
    gtk_widget_destroy(FRegisterWindow);
  inherited;
end;

procedure TLoginWindow.CreateLoginWindow;
var
  vbox, hbox, table: PGtkWidget;
  label_title, label_email, label_password: PGtkWidget;
begin
  // Crear ventana principal
  FWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(FWindow), 'EDDMail - Inicio de Sesión');
  gtk_window_set_default_size(GTK_WINDOW(FWindow), 400, 300);
  gtk_window_set_position(GTK_WINDOW(FWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_resizable(GTK_WINDOW(FWindow), False);

  // Conectar señal de destrucción
  g_signal_connect(G_OBJECT(FWindow), 'destroy', G_CALLBACK(@OnLoginWindowDestroy), nil);

  // VBox principal
  vbox := gtk_vbox_new(False, 10);
  gtk_container_add(GTK_CONTAINER(FWindow), vbox);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 20);

  // Título
  label_title := gtk_label_new(nil);
  gtk_label_set_markup(GTK_LABEL(label_title), '<big><b>EDDMail</b></big>');
  gtk_misc_set_alignment(GTK_MISC(label_title), 0.5, 0.5);
  gtk_box_pack_start(GTK_BOX(vbox), label_title, False, False, 20);

  // Tabla para campos
  table := gtk_table_new(2, 2, False);
  gtk_table_set_row_spacings(GTK_TABLE(table), 10);
  gtk_table_set_col_spacings(GTK_TABLE(table), 10);
  gtk_box_pack_start(GTK_BOX(vbox), table, False, False, 0);

  // Campo Email
  label_email := gtk_label_new('Email:');
  gtk_misc_set_alignment(GTK_MISC(label_email), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), label_email, 0, 1, 0, 1,
                   GTK_FILL, GTK_FILL, 0, 0);

  FEmailEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FEmailEntry), 100);
  gtk_table_attach(GTK_TABLE(table), FEmailEntry, 1, 2, 0, 1,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // Campo Password
  label_password := gtk_label_new('Password:');
  gtk_misc_set_alignment(GTK_MISC(label_password), 0.0, 0.5);
  gtk_table_attach(GTK_TABLE(table), label_password, 0, 1, 1, 2,
                   GTK_FILL, GTK_FILL, 0, 0);

  FPasswordEntry := gtk_entry_new();
  gtk_entry_set_visibility(GTK_ENTRY(FPasswordEntry), False);
  gtk_entry_set_max_length(GTK_ENTRY(FPasswordEntry), 50);
  gtk_table_attach(GTK_TABLE(table), FPasswordEntry, 1, 2, 1, 2,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // HBox para botones
  hbox := gtk_hbox_new(True, 10);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 20);

  // Botón Iniciar Sesión
  FLoginButton := gtk_button_new_with_label('Iniciar Sesión');
  gtk_box_pack_start(GTK_BOX(hbox), FLoginButton, True, True, 0);
  g_signal_connect(G_OBJECT(FLoginButton), 'clicked', G_CALLBACK(@OnLoginClicked), Self);

  // Botón Crear Cuenta
  FRegisterButton := gtk_button_new_with_label('Crear Cuenta');
  gtk_box_pack_start(GTK_BOX(hbox), FRegisterButton, True, True, 0);
  g_signal_connect(G_OBJECT(FRegisterButton), 'clicked', G_CALLBACK(@OnRegisterClicked), Self);

  // Label de estado
  FStatusLabel := gtk_label_new('');
  gtk_box_pack_start(GTK_BOX(vbox), FStatusLabel, False, False, 0);

  gtk_widget_show_all(FWindow);
end;

procedure TLoginWindow.CreateRegisterWindow;
var
  vbox, hbox, table: PGtkWidget;
  label_title: PGtkWidget;
  labels: array[0..5] of PGtkWidget;
  create_button, cancel_button: PGtkWidget;
  i: Integer;
  label_texts: array[0..5] of PChar = (
    'Nombre:', 'Usuario:', 'Email:', 'Teléfono:', 'Password:', 'Confirmar:'
  );
begin
  // Crear ventana de registro
  FRegisterWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(FRegisterWindow), 'EDDMail - Crear Cuenta');
  gtk_window_set_default_size(GTK_WINDOW(FRegisterWindow), 400, 400);
  gtk_window_set_position(GTK_WINDOW(FRegisterWindow), GTK_WIN_POS_CENTER);
  gtk_window_set_resizable(GTK_WINDOW(FRegisterWindow), False);
  gtk_window_set_modal(GTK_WINDOW(FRegisterWindow), True);

  // VBox principal
  vbox := gtk_vbox_new(False, 10);
  gtk_container_add(GTK_CONTAINER(FRegisterWindow), vbox);
  gtk_container_set_border_width(GTK_CONTAINER(vbox), 20);

  // Título
  label_title := gtk_label_new(nil);
  gtk_label_set_markup(GTK_LABEL(label_title), '<big><b>Crear Nueva Cuenta</b></big>');
  gtk_misc_set_alignment(GTK_MISC(label_title), 0.5, 0.5);
  gtk_box_pack_start(GTK_BOX(vbox), label_title, False, False, 20);

  // Tabla para campos
  table := gtk_table_new(6, 2, False);
  gtk_table_set_row_spacings(GTK_TABLE(table), 10);
  gtk_table_set_col_spacings(GTK_TABLE(table), 10);
  gtk_box_pack_start(GTK_BOX(vbox), table, False, False, 0);

  // Crear campos
  for i := 0 to 5 do
  begin
    labels[i] := gtk_label_new(label_texts[i]);
    gtk_misc_set_alignment(GTK_MISC(labels[i]), 0.0, 0.5);
    gtk_table_attach(GTK_TABLE(table), labels[i], 0, 1, i, i + 1,
                     GTK_FILL, GTK_FILL, 0, 0);
  end;

  FRegNombreEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FRegNombreEntry), 100);
  gtk_table_attach(GTK_TABLE(table), FRegNombreEntry, 1, 2, 0, 1,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  FRegUsuarioEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FRegUsuarioEntry), 50);
  gtk_table_attach(GTK_TABLE(table), FRegUsuarioEntry, 1, 2, 1, 2,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  FRegEmailEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FRegEmailEntry), 100);
  gtk_table_attach(GTK_TABLE(table), FRegEmailEntry, 1, 2, 2, 3,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  FRegTelefonoEntry := gtk_entry_new();
  gtk_entry_set_max_length(GTK_ENTRY(FRegTelefonoEntry), 20);
  gtk_table_attach(GTK_TABLE(table), FRegTelefonoEntry, 1, 2, 3, 4,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  FRegPasswordEntry := gtk_entry_new();
  gtk_entry_set_visibility(GTK_ENTRY(FRegPasswordEntry), False);
  gtk_entry_set_max_length(GTK_ENTRY(FRegPasswordEntry), 50);
  gtk_table_attach(GTK_TABLE(table), FRegPasswordEntry, 1, 2, 4, 5,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  FRegConfirmEntry := gtk_entry_new();
  gtk_entry_set_visibility(GTK_ENTRY(FRegConfirmEntry), False);
  gtk_entry_set_max_length(GTK_ENTRY(FRegConfirmEntry), 50);
  gtk_table_attach(GTK_TABLE(table), FRegConfirmEntry, 1, 2, 5, 6,
                   GTK_FILL or GTK_EXPAND, GTK_FILL, 0, 0);

  // HBox para botones
  hbox := gtk_hbox_new(True, 10);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 20);

  // Botón Crear
  create_button := gtk_button_new_with_label('Crear Cuenta');
  gtk_box_pack_start(GTK_BOX(hbox), create_button, True, True, 0);
  g_signal_connect(G_OBJECT(create_button), 'clicked', G_CALLBACK(@OnCreateAccountClicked), Self);

  // Botón Cancelar
  cancel_button := gtk_button_new_with_label('Cancelar');
  gtk_box_pack_start(GTK_BOX(hbox), cancel_button, True, True, 0);
  g_signal_connect(G_OBJECT(cancel_button), 'clicked', G_CALLBACK(@OnCancelRegisterClicked), Self);
end;

procedure TLoginWindow.ShowMessage(const Msg: String);
begin
  gtk_label_set_text(GTK_LABEL(FStatusLabel), PChar(Msg));
end;

procedure TLoginWindow.Show;
begin
  if FWindow <> nil then
    gtk_widget_show_all(FWindow);
end;

procedure TLoginWindow.Hide;
begin
  if FWindow <> nil then
    gtk_widget_hide(FWindow);
end;

// ============================================================================
// Callbacks
// ============================================================================

procedure OnLoginClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  LoginWin: TLoginWindow;
  Email, Password: String;
begin
  LoginWin := TLoginWindow(data);

  Email := gtk_entry_get_text(GTK_ENTRY(LoginWin.FEmailEntry));
  Password := gtk_entry_get_text(GTK_ENTRY(LoginWin.FPasswordEntry));

  if (Email = '') or (Password = '') then
  begin
    LoginWin.ShowMessage('Por favor complete todos los campos.');
    Exit;
  end;

  if SystemCore.LoginUser(Email, Password) then
  begin
    LoginWin.Hide;

    // Mostrar ventana apropiada según el tipo de usuario
    if SystemCore.IsRootUser then
    begin
      if RootWin = nil then
        RootWin := TRootWindow.Create;
      RootWin.Show;
    end
    else
    begin
      if UserWin = nil then
        UserWin := TUserWindow.Create;
      UserWin.Show;
    end;

    LoginWin.ShowMessage('¡Inicio de sesión exitoso!');
  end
  else
  begin
    LoginWin.ShowMessage('Email o contraseña incorrectos.');
  end;
end;

procedure OnRegisterClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  LoginWin: TLoginWindow;
begin
  LoginWin := TLoginWindow(data);

  // Limpiar campos del formulario de registro
  gtk_entry_set_text(GTK_ENTRY(LoginWin.FRegNombreEntry), '');
  gtk_entry_set_text(GTK_ENTRY(LoginWin.FRegUsuarioEntry), '');
  gtk_entry_set_text(GTK_ENTRY(LoginWin.FRegEmailEntry), '');
  gtk_entry_set_text(GTK_ENTRY(LoginWin.FRegTelefonoEntry), '');
  gtk_entry_set_text(GTK_ENTRY(LoginWin.FRegPasswordEntry), '');
  gtk_entry_set_text(GTK_ENTRY(LoginWin.FRegConfirmEntry), '');

  gtk_widget_show_all(LoginWin.FRegisterWindow);
end;

procedure OnCreateAccountClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  LoginWin: TLoginWindow;
  Nombre, Usuario, Email, Telefono, Password, Confirm: String;
  dialog: PGtkWidget;
begin
  LoginWin := TLoginWindow(data);

  Nombre := gtk_entry_get_text(GTK_ENTRY(LoginWin.FRegNombreEntry));
  Usuario := gtk_entry_get_text(GTK_ENTRY(LoginWin.FRegUsuarioEntry));
  Email := gtk_entry_get_text(GTK_ENTRY(LoginWin.FRegEmailEntry));
  Telefono := gtk_entry_get_text(GTK_ENTRY(LoginWin.FRegTelefonoEntry));
  Password := gtk_entry_get_text(GTK_ENTRY(LoginWin.FRegPasswordEntry));
  Confirm := gtk_entry_get_text(GTK_ENTRY(LoginWin.FRegConfirmEntry));

  // Validaciones
  if (Nombre = '') or (Usuario = '') or (Email = '') or
     (Telefono = '') or (Password = '') or (Confirm = '') then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(LoginWin.FRegisterWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Por favor complete todos los campos.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    Exit;
  end;

  if Password <> Confirm then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(LoginWin.FRegisterWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Las contraseñas no coinciden.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    Exit;
  end;

  if Pos('@', Email) = 0 then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(LoginWin.FRegisterWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'Por favor ingrese un email válido.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    Exit;
  end;

  // Intentar registrar usuario
  if SystemCore.RegisterUser(Nombre, Usuario, Email, Telefono, Password) then
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(LoginWin.FRegisterWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    'Cuenta creada exitosamente.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    gtk_widget_hide(LoginWin.FRegisterWindow);
  end
  else
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(LoginWin.FRegisterWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_ERROR,
                                    GTK_BUTTONS_OK,
                                    'El email ya está registrado.');
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
  end;
end;

procedure OnCancelRegisterClicked(widget: PGtkWidget; data: gpointer); cdecl;
var
  LoginWin: TLoginWindow;
begin
  LoginWin := TLoginWindow(data);
  gtk_widget_hide(LoginWin.FRegisterWindow);
end;

procedure OnLoginWindowDestroy(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_main_quit;
end;

end.

