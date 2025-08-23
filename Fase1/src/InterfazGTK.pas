unit InterfazGTK;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, glib2, gdk2, gtk2, EstructurasDatos;

type
  TInterfazEDDMail = class
  private
    FSistema: TEDDMailSystem;
    FVentanaPrincipal: PGtkWidget;
    FVentanaLogin: PGtkWidget;
    FUsuarioActivo: Boolean;
    FEntryEmail: PGtkWidget;
    FEntryPassword: PGtkWidget;
    
    procedure CrearVentanaLogin;
    procedure CrearVentanaPrincipal;
    procedure CrearInterfazRoot;
    procedure CrearInterfazUsuario;
    procedure MostrarMensaje(Titulo: String; Mensaje: String);
    
  public
    constructor Create;
    destructor Destroy; override;
    procedure Ejecutar;
    
    procedure OnLoginClick(widget: PGtkWidget; data: gpointer);
    procedure OnCrearCuentaClick(widget: PGtkWidget; data: gpointer);
    procedure OnCargaMasivaClick(widget: PGtkWidget; data: gpointer);
    procedure OnReporteUsuariosClick(widget: PGtkWidget; data: gpointer);
    procedure OnReporteRelacionesClick(widget: PGtkWidget; data: gpointer);
    procedure OnCerrarSesionClick(widget: PGtkWidget; data: gpointer);
  end;

var
  InterfazGlobal: TInterfazEDDMail;

procedure LoginCallback(widget: PGtkWidget; data: gpointer); cdecl;
procedure CrearCuentaCallback(widget: PGtkWidget; data: gpointer); cdecl;
procedure CargaMasivaCallback(widget: PGtkWidget; data: gpointer); cdecl;
procedure ReporteUsuariosCallback(widget: PGtkWidget; data: gpointer); cdecl;
procedure ReporteRelacionesCallback(widget: PGtkWidget; data: gpointer); cdecl;
procedure CerrarSesionCallback(widget: PGtkWidget; data: gpointer); cdecl;
procedure DestruirVentana(widget: PGtkWidget; data: gpointer); cdecl;

implementation

procedure LoginCallback(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if InterfazGlobal <> nil then
    InterfazGlobal.OnLoginClick(widget, data);
end;

procedure CrearCuentaCallback(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if InterfazGlobal <> nil then
    InterfazGlobal.OnCrearCuentaClick(widget, data);
end;

procedure CargaMasivaCallback(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if InterfazGlobal <> nil then
    InterfazGlobal.OnCargaMasivaClick(widget, data);
end;

procedure ReporteUsuariosCallback(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if InterfazGlobal <> nil then
    InterfazGlobal.OnReporteUsuariosClick(widget, data);
end;

procedure ReporteRelacionesCallback(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if InterfazGlobal <> nil then
    InterfazGlobal.OnReporteRelacionesClick(widget, data);
end;

procedure CerrarSesionCallback(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if InterfazGlobal <> nil then
    InterfazGlobal.OnCerrarSesionClick(widget, data);
end;

procedure DestruirVentana(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_main_quit();
end;

constructor TInterfazEDDMail.Create;
begin
  inherited Create;
  FSistema := TEDDMailSystem.Create;
  FUsuarioActivo := False;
  InterfazGlobal := Self;
end;

destructor TInterfazEDDMail.Destroy;
begin
  FSistema.Free;
  inherited Destroy;
end;

procedure TInterfazEDDMail.Ejecutar;
begin
  gtk_init(@argc, @argv);
  CrearVentanaLogin;
  gtk_main();
end;

procedure TInterfazEDDMail.CrearVentanaLogin;
var
  VBox: PGtkWidget;
  HBoxBotones: PGtkWidget;
  Label: PGtkWidget;
  BtnLogin: PGtkWidget;
  BtnCrearCuenta: PGtkWidget;
begin
  FVentanaLogin := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(FVentanaLogin), 'EDDMail - Iniciar Sesión');
  gtk_window_set_default_size(GTK_WINDOW(FVentanaLogin), 400, 300);
  gtk_window_set_position(GTK_WINDOW(FVentanaLogin), GTK_WIN_POS_CENTER);
  
  g_signal_connect(FVentanaLogin, 'destroy', G_CALLBACK(@DestruirVentana), nil);
  
  VBox := gtk_vbox_new(False, 10);
  gtk_container_add(GTK_CONTAINER(FVentanaLogin), VBox);
  gtk_container_set_border_width(GTK_CONTAINER(VBox), 20);
  
  Label := gtk_label_new('EDDMail');
  gtk_box_pack_start(GTK_BOX(VBox), Label, False, False, 20);
  
  Label := gtk_label_new('Email:');
  gtk_misc_set_alignment(GTK_MISC(Label), 0.0, 0.5);
  gtk_box_pack_start(GTK_BOX(VBox), Label, False, False, 0);
  
  FEntryEmail := gtk_entry_new();
  gtk_box_pack_start(GTK_BOX(VBox), FEntryEmail, False, False, 5);
  
  Label := gtk_label_new('Password:');
  gtk_misc_set_alignment(GTK_MISC(Label), 0.0, 0.5);
  gtk_box_pack_start(GTK_BOX(VBox), Label, False, False, 0);
  
  FEntryPassword := gtk_entry_new();
  gtk_entry_set_visibility(GTK_ENTRY(FEntryPassword), False);
  gtk_box_pack_start(GTK_BOX(VBox), FEntryPassword, False, False, 5);
  
  HBoxBotones := gtk_hbox_new(True, 10);
  gtk_box_pack_start(GTK_BOX(VBox), HBoxBotones, False, False, 20);
  
  BtnLogin := gtk_button_new_with_label('Iniciar Sesión');
  gtk_box_pack_start(GTK_BOX(HBoxBotones), BtnLogin, True, True, 0);
  g_signal_connect(BtnLogin, 'clicked', G_CALLBACK(@LoginCallback), nil);
  
  BtnCrearCuenta := gtk_button_new_with_label('Crear Cuenta');
  gtk_box_pack_start(GTK_BOX(HBoxBotones), BtnCrearCuenta, True, True, 0);
  g_signal_connect(BtnCrearCuenta, 'clicked', G_CALLBACK(@CrearCuentaCallback), nil);
  
  gtk_widget_show_all(FVentanaLogin);
end;

procedure TInterfazEDDMail.CrearVentanaPrincipal;
begin
  gtk_widget_hide(FVentanaLogin);
  
  FVentanaPrincipal := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(FVentanaPrincipal), 'EDDMail - Sistema de Correos');
  gtk_window_set_default_size(GTK_WINDOW(FVentanaPrincipal), 800, 600);
  gtk_window_set_position(GTK_WINDOW(FVentanaPrincipal), GTK_WIN_POS_CENTER);
  
  g_signal_connect(FVentanaPrincipal, 'destroy', G_CALLBACK(@DestruirVentana), nil);
  
  if FSistema.GetUsuarioActual^.Email = 'root@edd.com' then
    CrearInterfazRoot
  else
    CrearInterfazUsuario;
    
  gtk_widget_show_all(FVentanaPrincipal);
end;

procedure TInterfazEDDMail.CrearInterfazRoot;
var
  VBox: PGtkWidget;
  Label: PGtkWidget;
  BtnCargaMasiva: PGtkWidget;
  BtnReporteUsuarios: PGtkWidget;
  BtnReporteRelaciones: PGtkWidget;
  BtnCerrarSesion: PGtkWidget;
begin
  VBox := gtk_vbox_new(False, 10);
  gtk_container_add(GTK_CONTAINER(FVentanaPrincipal), VBox);
  gtk_container_set_border_width(GTK_CONTAINER(VBox), 20);
  
  Label := gtk_label_new('Root - Panel de Administración');
  gtk_box_pack_start(GTK_BOX(VBox), Label, False, False, 10);
  
  BtnCargaMasiva := gtk_button_new_with_label('Carga Masiva');
  gtk_box_pack_start(GTK_BOX(VBox), BtnCargaMasiva, False, False, 5);
  g_signal_connect(BtnCargaMasiva, 'clicked', G_CALLBACK(@CargaMasivaCallback), nil);
  
  BtnReporteUsuarios := gtk_button_new_with_label('Reporte de Usuarios');
  gtk_box_pack_start(GTK_BOX(VBox), BtnReporteUsuarios, False, False, 5);
  g_signal_connect(BtnReporteUsuarios, 'clicked', G_CALLBACK(@ReporteUsuariosCallback), nil);
  
  BtnReporteRelaciones := gtk_button_new_with_label('Reporte de Relaciones');
  gtk_box_pack_start(GTK_BOX(VBox), BtnReporteRelaciones, False, False, 5);
  g_signal_connect(BtnReporteRelaciones, 'clicked', G_CALLBACK(@ReporteRelacionesCallback), nil);
  
  BtnCerrarSesion := gtk_button_new_with_label('Cerrar Sesión');
  gtk_box_pack_start(GTK_BOX(VBox), BtnCerrarSesion, False, False, 20);
  g_signal_connect(BtnCerrarSesion, 'clicked', G_CALLBACK(@CerrarSesionCallback), nil);
end;

procedure TInterfazEDDMail.CrearInterfazUsuario;
var
  VBox: PGtkWidget;
  Label: PGtkWidget;
  Usuario: PUsuario;
  BtnCerrarSesion: PGtkWidget;
begin
  VBox := gtk_vbox_new(False, 5);
  gtk_container_add(GTK_CONTAINER(FVentanaPrincipal), VBox);
  gtk_container_set_border_width(GTK_CONTAINER(VBox), 15);
  
  Usuario := FSistema.GetUsuarioActual;
  
  Label := gtk_label_new(PChar('Hola: ' + Usuario^.Nombre));
  gtk_box_pack_start(GTK_BOX(VBox), Label, False, False, 10);
  
  Label := gtk_label_new('Menu de Usuario Estándar');
  gtk_box_pack_start(GTK_BOX(VBox), Label, False, False, 10);
  
  BtnCerrarSesion := gtk_button_new_with_label('Cerrar Sesión');
  gtk_box_pack_start(GTK_BOX(VBox), BtnCerrarSesion, False, False, 10);
  g_signal_connect(BtnCerrarSesion, 'clicked', G_CALLBACK(@CerrarSesionCallback), nil);
end;

procedure TInterfazEDDMail.OnLoginClick(widget: PGtkWidget; data: gpointer);
var
  Email: String;
  Password: String;
begin
  Email := gtk_entry_get_text(GTK_ENTRY(FEntryEmail));
  Password := gtk_entry_get_text(GTK_ENTRY(FEntryPassword));
  
  if FSistema.IniciarSesion(Email, Password) then
  begin
    FUsuarioActivo := True;
    CrearVentanaPrincipal;
  end
  else
  begin
    MostrarMensaje('Error', 'Credenciales incorrectas');
  end;
end;

procedure TInterfazEDDMail.OnCrearCuentaClick(widget: PGtkWidget; data: gpointer);
begin
  MostrarMensaje('Info', 'Crear cuenta - Funcionalidad no implementada');
end;

procedure TInterfazEDDMail.OnCargaMasivaClick(widget: PGtkWidget; data: gpointer);
begin
  MostrarMensaje('Info', 'Carga masiva - Funcionalidad no implementada');
end;

procedure TInterfazEDDMail.OnReporteUsuariosClick(widget: PGtkWidget; data: gpointer);
begin
  FSistema.GenerarReporteUsuarios('Root-Reportes');
  MostrarMensaje('Éxito', 'Reporte de usuarios generado');
end;

procedure TInterfazEDDMail.OnReporteRelacionesClick(widget: PGtkWidget; data: gpointer);
begin
  FSistema.GenerarReporteRelaciones('Root-Reportes');
  MostrarMensaje('Éxito', 'Reporte de relaciones generado');
end;

procedure TInterfazEDDMail.OnCerrarSesionClick(widget: PGtkWidget; data: gpointer);
begin
  FSistema.CerrarSesion;
  FUsuarioActivo := False;
  gtk_widget_destroy(FVentanaPrincipal);
  gtk_widget_show(FVentanaLogin);
  
  gtk_entry_set_text(GTK_ENTRY(FEntryEmail), '');
  gtk_entry_set_text(GTK_ENTRY(FEntryPassword), '');
end;

procedure TInterfazEDDMail.MostrarMensaje(Titulo: String; Mensaje: String);
var
  Dialogo: PGtkWidget;
begin
  Dialogo := gtk_message_dialog_new(GTK_WINDOW(FVentanaPrincipal),
    GTK_DIALOG_MODAL, GTK_MESSAGE_INFO, GTK_BUTTONS_OK, PChar(Mensaje));
  gtk_window_set_title(GTK_WINDOW(Dialogo), PChar(Titulo));
  gtk_dialog_run(GTK_DIALOG(Dialogo));
  gtk_widget_destroy(Dialogo);
end;

end.
