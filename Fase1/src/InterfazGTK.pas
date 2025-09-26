unit InterfazGTK;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  FileUtil, EstructurasDatos, CorreoManager;

type
  TInterfazEDDMail = class
  private
    FSistema: TEDDMailSystem;
    FFormLogin: TForm;
    FFormPrincipal: TForm;
    FUsuarioActivo: Boolean;
    FEditEmail: TEdit;
    FEditPassword: TEdit;
    FCorreoManager: TCorreoManager;


      FContactoActual: PContacto;
      FPrimerContacto: PContacto;
      FIndiceContactoActual: Integer;
      FTotalContactos: Integer;

      FFormContactos: TForm;
    FLabelContadorContactos: TLabel;
    FLabelNombreContacto: TLabel;
    FLabelUsuarioContacto: TLabel;
    FLabelEmailContacto: TLabel;
    FLabelTelefonoContacto: TLabel;
    // Controles para comunidades
    FEditNombreComunidad: TEdit;
    FEditEmailUsuario: TEdit;
    FMemoComunidades: TMemo;
    // --- Bandeja de entrada ---
  FFormBandeja: TForm;
  FListBandeja: TListBox;
  FMemoMensaje: TMemo;
  FLabelNoLeidosInbox: TLabel;
    // --- Papelera ---
    FFormPapelera: TForm;
    FListPapelera: TListBox;
    FMemoPapelera: TMemo;
    FEditBuscarPapelera: TEdit;

    // --- Correos Programados ---
    FFormCorreosProgramados: TForm;
    FListCorreosProgramados: TListBox;
    FMemoCorreoProgramado: TMemo;
    FLabelTotalProgramados: TLabel;
    procedure CrearFormLogin;
    procedure CrearFormPrincipal;
    procedure CrearInterfazRoot;
    procedure CrearInterfazUsuario;
    procedure MostrarMensaje(Titulo, Mensaje: String);
    procedure Papelera_OnCerrarClick(Sender: TObject);

    // Agregar estos procedimientos:
    procedure OnGenerarReportesClick(Sender: TObject);
    procedure OnReporteCorreosRecibidosClick(Sender: TObject);
    procedure OnReportePapeleraClick(Sender: TObject);
    procedure OnReporteCorreosProgramadosClick(Sender: TObject);

    procedure OnBandejaClick(Sender: TObject);
    procedure OnFormBandejaClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Inbox_RellenarLista;           // llena la lista desde la estructura
    procedure Inbox_OnSeleccion(Sender: TObject);
    procedure Inbox_OnOrdenarClick(Sender: TObject);
    procedure Inbox_OnEliminarClick(Sender: TObject);
    procedure Inbox_OnMarcarLeidoClick(Sender: TObject);
    procedure Inbox_OnCerrarClick(Sender: TObject);

    procedure OnPapeleraClick(Sender: TObject);
    procedure OnFormPapeleraClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Papelera_RellenarLista;
    procedure Papelera_OnSeleccion(Sender: TObject);
    procedure Papelera_OnBuscarClick(Sender: TObject);
    procedure Papelera_OnEliminarDefClick(Sender: TObject);
    // Event handlers
    procedure OnEnviarCorreoClick(Sender: TObject);

    procedure OnLoginClick(Sender: TObject);
    procedure OnCrearCuentaClick(Sender: TObject);
    procedure OnCargaMasivaClick(Sender: TObject);
    procedure OnReporteUsuariosClick(Sender: TObject);
    procedure OnReporteRelacionesClick(Sender: TObject);
    procedure OnCerrarSesionClick(Sender: TObject);
    procedure OnFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
    procedure OnFormContactosClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ActualizarVistaContacto; // Método auxiliar
    // Nuevos event handlers para comunidades
    procedure OnGestionarComunidadesClick(Sender: TObject);
    procedure OnReporteComunidadesClick(Sender: TObject);
    procedure OnActualizarPerfilClick(Sender: TObject);
    procedure OnCrearComunidadClick(Sender: TObject);
    procedure OnAsignarUsuarioClick(Sender: TObject);
    procedure OnListarComunidadesClick(Sender: TObject);

     // Event handlers para contactos
    procedure OnAgregarContactoClick(Sender: TObject);
    procedure OnVerContactosClick(Sender: TObject);
    procedure OnContactoAnteriorClick(Sender: TObject);
    procedure OnContactoSiguienteClick(Sender: TObject);
    procedure OnGenerarReporteContactosClick(Sender: TObject);

    procedure OnProgramarCorreoClick(Sender: TObject);


    procedure OnCorreosProgramadosClick(Sender: TObject);
    procedure OnFormCorreosProgramadosClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure CorreosProgramados_RellenarLista;
    procedure CorreosProgramados_OnSeleccion(Sender: TObject);
    procedure CorreosProgramados_OnEnviarClick(Sender: TObject);
    procedure CorreosProgramados_OnEliminarClick(Sender: TObject);
    procedure CorreosProgramados_OnCerrarClick(Sender: TObject);


    //Fase 2

    // Agregar estos event handlers en la sección private de TInterfazEDDMail

// Event handlers para nuevas funcionalidades
procedure OnVerFavoritosClick(Sender: TObject);
procedure OnVerBorradoresClick(Sender: TObject);
procedure OnPublicarComunidadClick(Sender: TObject);
procedure OnEliminarContactoClick(Sender: TObject);

// Event handlers para ventana de favoritos
procedure OnFormFavoritosClose(Sender: TObject; var CloseAction: TCloseAction);
procedure Favoritos_OnSeleccion(Sender: TObject);
procedure Favoritos_OnEliminarClick(Sender: TObject);

// Event handlers para ventana de borradores
procedure OnFormBorradoresClose(Sender: TObject; var CloseAction: TCloseAction);
procedure Borradores_OnSeleccion(Sender: TObject);
procedure Borradores_OnEditarClick(Sender: TObject);
procedure Borradores_OnEliminarClick(Sender: TObject);
procedure Borradores_OnRecorridoChange(Sender: TObject);

// Variables para nuevas ventanas
FFormFavoritos: TForm;
FListFavoritos: TListBox;
FMemoFavorito: TMemo;
FLabelTotalFavoritos: TLabel;

FFormBorradores: TForm;
FListBorradores: TListBox;
FMemoBorrador: TMemo;
FComboRecorrido: TComboBox;

    procedure Inbox_OnMarcarFavoritoClick(Sender: TObject);
    // Agregar estos event handlers en la sección private:
procedure OnCrearComunidadBSTClick(Sender: TObject);
procedure OnVerMensajesComunidadClick(Sender: TObject);

// Event handlers para reportes Fase 2
procedure OnReporteFavoritosClick(Sender: TObject);
procedure OnReporteBorradoresClick(Sender: TObject);

// Event handlers para funcionalidades ROOT Fase 2
procedure OnCrearComunidadBSTClick(Sender: TObject);
procedure OnVerMensajesComunidadClick(Sender: TObject);

// Función auxiliar para generar reportes BST
procedure GenerarNodosBST(var Archivo: TextFile; nodo: PNodoBST);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Ejecutar;
  end;

implementation

uses
  LCLIntf, LCLType;

constructor TInterfazEDDMail.Create;
begin
  inherited Create;
  FSistema := TEDDMailSystem.Create;
    FCorreoManager := TCorreoManager.Create;

  FUsuarioActivo := False;
  FFormLogin := nil;
  FFormPrincipal := nil;
end;

destructor TInterfazEDDMail.Destroy;
begin
  FSistema.Free;
    FCorreoManager.Free;

  if Assigned(FFormLogin) then
    FFormLogin.Free;
  if Assigned(FFormPrincipal) then
    FFormPrincipal.Free;
  inherited Destroy;
end;
procedure TInterfazEDDMail.Inbox_OnCerrarClick(Sender: TObject);
begin
  if Assigned(FFormBandeja) then
    FFormBandeja.Close;
end;

procedure TInterfazEDDMail.Ejecutar;
begin
  Application.Initialize;
  CrearFormLogin;
  Application.Run;
end;
procedure TInterfazEDDMail.CrearInterfazUsuario;
var
  Panel: TPanel;
  LabelSaludo, LabelMenu, LabelInfo: TLabel;
  Usuario: PUsuario;
  BtnBandeja, BtnEnviar, BtnPapelera, BtnProgramar,
  BtnCorreosProgramados, BtnAgregarContacto, BtnContactos,
  BtnPerfil, BtnReportes, BtnCerrarSesion: TButton;
  YPos: Integer;
begin
  Panel := TPanel.Create(FFormPrincipal);
  with Panel do
  begin
    Parent := FFormPrincipal;
    Align := alClient;
    BevelOuter := bvNone;
    BorderWidth := 20;
    Color := clTeal;

  end;

  Usuario := FSistema.GetUsuarioActual;

  LabelSaludo := TLabel.Create(Panel);
  with LabelSaludo do
  begin
    Parent := Panel;
    Caption := 'Hola: ' + Usuario^.Nombre+ ' 👋 ';
    Font.Size := 16;
    Font.Style := [fsBold];
    Font.Color := clInfoBk;
    Left := 20;
    Top := 20;
    AutoSize := True;
  end;

  LabelMenu := TLabel.Create(Panel);
  with LabelMenu do
  begin
    Parent := Panel;
    Caption := 'Menú de Usuario Estándar';
    Left := 20;
    Top := 50;
    Font.Style := [fsBold];
  end;

  LabelInfo := TLabel.Create(Panel);
  with LabelInfo do
  begin
    Parent := Panel;
    Caption := 'Email: ' + Usuario^.Email + ' | Teléfono: ' + Usuario^.Telefono;
    Left := 20;
    Top := 70;
    Font.Color := clWhite;
  end;

  YPos := 110;

  // FILA 1: Bandeja de Entrada y Enviar Correo
  BtnBandeja := TButton.Create(Panel);
  with BtnBandeja do
  begin
    Parent := Panel;
    Caption := '✉️ Bandeja de Entrada';
    Left := 20;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Ver correos recibidos';
    ShowHint := True;
    OnClick := @OnBandejaClick;
    Color:= clTeal;
  end;

  BtnEnviar := TButton.Create(Panel);
  with BtnEnviar do
  begin
    Parent := Panel;
    Caption := '📩 Enviar Correo';
    Left := 220;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Enviar un nuevo correo';
    Color:= clTeal;
    ShowHint := True;
    OnClick := @OnEnviarCorreoClick;
  end;
  Inc(YPos, 50);

  // FILA 2: Papelera y Programar Correo
  BtnPapelera := TButton.Create(Panel);
  with BtnPapelera do
  begin
    Parent := Panel;
    Caption := '🗑️ Papelera';
    Left := 20;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Ver correos eliminados';
    ShowHint := True;
    OnClick := @OnPapeleraClick;
    Color:=clTeal;
  end;

  BtnProgramar := TButton.Create(Panel);
  with BtnProgramar do
  begin
    Parent := Panel;
    Caption := '📒 Programar Correo';
    Left := 220;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Programar envío automático';
    ShowHint := True;
    OnClick := @OnProgramarCorreoClick;
    Color:= clTeal;
  end;
  Inc(YPos, 50);

  // FILA 3: Correos Programados y Agregar Contacto
  BtnCorreosProgramados := TButton.Create(Panel);
  with BtnCorreosProgramados do
  begin
    Parent := Panel;
    Caption := '📅 Correos Programados';
    Left := 20;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Ver y enviar correos programados';
    ShowHint := True;
     OnClick := @OnCorreosProgramadosClick;
     Color:= clTeal;
  end;

  BtnAgregarContacto := TButton.Create(Panel);
  with BtnAgregarContacto do
  begin
    Parent := Panel;
    Caption := '📇 Agregar Contacto';
    Left := 220;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Agregar nuevo contacto📇';
    ShowHint := True;
    OnClick := @OnAgregarContactoClick;
    Font.Style := [fsBold];
    Color := clTeal;
  end;
  Inc(YPos, 50);

  // FILA 4: Contactos y Actualizar Perfil
  BtnContactos := TButton.Create(Panel);
  with BtnContactos do
  begin
    Parent := Panel;
    Caption := '📇 Contactos';
    Left := 20;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Ver y navegar entre contactos';
    ShowHint := True;
    OnClick := @OnVerContactosClick;
    Font.Style := [fsBold];
    Color := clTeal;
  end;

  BtnPerfil := TButton.Create(Panel);
  with BtnPerfil do
  begin
    Parent := Panel;
    Caption := '👤 Actualizar Perfil';
    Left := 220;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Modificar información personal';
    ShowHint := True;
    OnClick := @OnActualizarPerfilClick;
    Color:=clTeal;
  end;
  Inc(YPos, 50);

  // FILA 5: Generar Reportes
  BtnReportes := TButton.Create(Panel);
  with BtnReportes do
  begin
    Parent := Panel;
    Caption := '📊 Generar Reportes';
    Left := 120;
    Top := YPos;
    Width := 200;
    Height := 35;
    Hint := 'Generar reportes personales (Correos, Papelera, Programados, Contactos)';
    ShowHint := True;
    OnClick := @OnGenerarReportesClick;
    Font.Style := [fsBold];
    Color := clTeal;
  end;
  Inc(YPos, 80);
  // FILA 5: Ver Favoritos y Ver Borradores
BtnFavoritos := TButton.Create(Panel);
with BtnFavoritos do
begin
  Parent := Panel;
  Caption := '⭐ Ver Favoritos';
  Left := 20;
  Top := YPos;
  Width := 180;
  Height := 35;
  Hint := 'Ver correos marcados como favoritos';
  ShowHint := True;
  OnClick := @OnVerFavoritosClick;
  Font.Style := [fsBold];
  Color := clTeal;
end;

BtnBorradores := TButton.Create(Panel);
with BtnBorradores do
begin
  Parent := Panel;
  Caption := '📝 Ver Borradores';
  Left := 220;
  Top := YPos;
  Width := 180;
  Height := 35;
  Hint := 'Ver y editar borradores de mensajes';
  ShowHint := True;
  OnClick := @OnVerBorradoresClick;
  Font.Style := [fsBold];
  Color := clTeal;
end;
Inc(YPos, 50);

// FILA 6: Publicar en Comunidad y Eliminar Contacto
BtnPublicarComunidad := TButton.Create(Panel);
with BtnPublicarComunidad do
begin
  Parent := Panel;
  Caption := '📢 Publicar en Comunidad';
  Left := 20;
  Top := YPos;
  Width := 180;
  Height := 35;
  Hint := 'Publicar mensaje en comunidad';
  ShowHint := True;
  OnClick := @OnPublicarComunidadClick;
  Font.Style := [fsBold];
  Color := clTeal;
end;

BtnEliminarContacto := TButton.Create(Panel);
with BtnEliminarContacto do
begin
  Parent := Panel;
  Caption := '❌ Eliminar Contacto';
  Left := 220;
  Top := YPos;
  Width := 180;
  Height := 35;
  Hint := 'Eliminar contacto de la lista';
  ShowHint := True;
  OnClick := @OnEliminarContactoClick;
  Font.Style := [fsBold];
  Color := clTeal;
end;
Inc(YPos, 50);

  // CERRAR SESIÓN (separado)
  BtnCerrarSesion := TButton.Create(Panel);
  with BtnCerrarSesion do
  begin
    Parent := Panel;
    Caption := '❌ Cerrar Sesión';
    Left := 120;
    Top := YPos;
    Width := 180;
    Height := 35;
    OnClick := @OnCerrarSesionClick;
    Font.Color := clRed;
    Font.Style := [fsBold];
  end;
end;
procedure TInterfazEDDMail.CrearFormLogin;
var
  Panel: TPanel;
  LabelTitulo, LabelEmail, LabelPassword: TLabel;
  BtnLogin, BtnCrearCuenta: TButton;
begin
  FFormLogin := TForm.Create(nil);
  with FFormLogin do
  begin
    Caption := ' EDDMail - Iniciar Sesión';
    Width := 430;
    Height := 350;
    Position := poScreenCenter;
    BorderStyle := bsDialog;


    OnClose := @Self.OnFormClose;
    KeyPreview := True;
    OnKeyPress := @Self.OnKeyPress;
  end;

  Panel := TPanel.Create(FFormLogin);
  with Panel do
  begin
    Parent := FFormLogin;
    Align := alClient;
    BevelOuter := bvNone;
    BorderWidth := 20;
    Color := clTeal                           ;
  end;

  LabelTitulo := TLabel.Create(Panel);
  with LabelTitulo do
  begin
    Parent := Panel;
    Caption := '🔐 EDDMail - Sistema de Correos';
    Font.Size := 16;
    Font.Style := [fsBold];
    Font.Color := clWhite;
    Left := 80;
    Top := 30;
    AutoSize := True;
  end;

  LabelEmail := TLabel.Create(Panel);
  with LabelEmail do
  begin
    Parent := Panel;
    Caption := 'Email:';
    Left := 20;
    Top := 90;
    Font.Style := [fsBold];
  end;

  FEditEmail := TEdit.Create(Panel);
  with FEditEmail do
  begin
    Parent := Panel;
    Left := 20;
    Top := 110;
    Width := 370;
    Text := 'root@edd.com';
    TabOrder := 0;
  end;

  LabelPassword := TLabel.Create(Panel);
  with LabelPassword do
  begin
    Parent := Panel;
    Caption := 'Password:';
    Left := 20;
    Top := 150;
    Font.Style := [fsBold];
  end;

  FEditPassword := TEdit.Create(Panel);
  with FEditPassword do
  begin
    Parent := Panel;
    Left := 20;
    Top := 170;
    Width := 370;
    PasswordChar := '*';
    Text := 'root123';
    TabOrder := 1;
  end;

  BtnLogin := TButton.Create(Panel);
  with BtnLogin do
  begin
    Parent := Panel;
    Caption := 'Iniciar Sesión';
    Left := 90;
    Top := 220;
    Width := 120;
    Height := 35;
    TabOrder := 2;
    Default := True;

    OnClick := @Self.OnLoginClick;
    Font.Style := [fsBold];
  end;

  BtnCrearCuenta := TButton.Create(Panel);
  with BtnCrearCuenta do
  begin
    Parent := Panel;
    Caption := 'Crear Cuenta';
    Left := 230;
    Top := 220;
    Width := 120;
    Height := 35;
    TabOrder := 3;
    OnClick := @Self.OnCrearCuentaClick;

  end;

  FFormLogin.Show;
end;


procedure TInterfazEDDMail.CrearFormPrincipal;
begin
  FFormLogin.Hide;
  
  FFormPrincipal := TForm.Create(nil);
  with FFormPrincipal do
  begin
    Caption := 'EDDMail - Sistema de Correos';
    Width := 450;
    Height := 520;
    Position := poScreenCenter;
    OnClose := @OnFormClose;
  end;
  
  if FSistema.GetUsuarioActual^.Email = 'root@edd.com' then
    CrearInterfazRoot
  else
    CrearInterfazUsuario;
    
  FFormPrincipal.Show;
end;

procedure TInterfazEDDMail.CrearInterfazRoot;
var
  Panel: TPanel;
  LabelTitulo, LabelInfo: TLabel;
  BtnCargaMasiva, BtnReporteUsuarios, BtnReporteRelaciones,
  BtnGestionarComunidades, BtnReporteComunidades, BtnCerrarSesion: TButton;
  YPos: Integer;
begin
  Panel := TPanel.Create(FFormPrincipal);
  with Panel do
  begin
    Parent := FFormPrincipal;
    Align := alClient;
    BevelOuter := bvNone;
    BorderWidth := 20;
    Color := clTeal              ;


  end;

  LabelTitulo := TLabel.Create(Panel);
  with LabelTitulo do
  begin
    Parent := Panel;
    Caption := 'Root - Panel de Administración';
    Font.Size := 16;
    Font.Style := [fsBold];
    Font.Color := clMaroon;
    Left := 20;
    Top := 20;
    AutoSize := True;
  end;

  LabelInfo := TLabel.Create(Panel);
  with LabelInfo do
  begin
    Parent := Panel;
    Caption := 'Bienvenido Administrador 🧑💼 . Seleccione una opción:';
    Left := 20;
    Top := 50;
    Font.Color := clWhite;
  end;

  YPos := 90;

  BtnCargaMasiva := TButton.Create(Panel);
  with BtnCargaMasiva do
  begin
    Parent := Panel;
    Caption := 'Carga Masiva de Usuarios (JSON)';
    Left := 20;
    Top := YPos;
    Width := 300;
    Height := 40;
    OnClick := @OnCargaMasivaClick;
    Font.Style := [fsBold];
  end;
  Inc(YPos, 60);

  BtnReporteUsuarios := TButton.Create(Panel);
  with BtnReporteUsuarios do
  begin
    Parent := Panel;
    Caption := 'Generar Reporte de Usuarios';
    Left := 20;
    Top := YPos;
    Width := 300;
    Height := 40;
    OnClick := @OnReporteUsuariosClick;
    Font.Style := [fsBold];
  end;
  Inc(YPos, 60);

  BtnReporteRelaciones := TButton.Create(Panel);
  with BtnReporteRelaciones do
  begin
    Parent := Panel;
    Caption := 'Generar Reporte de Relaciones';
    Left := 20;
    Top := YPos;
    Width := 300;
    Height := 40;
    OnClick := @OnReporteRelacionesClick;
    Font.Style := [fsBold];
  end;
  Inc(YPos, 60);

  // NUEVOS BOTONES
  BtnGestionarComunidades := TButton.Create(Panel);
  with BtnGestionarComunidades do
  begin
    Parent := Panel;
    Caption := 'Gestionar Comunidades';
    Left := 20;
    Top := YPos;
    Width := 300;
    Height := 40;
    OnClick := @OnGestionarComunidadesClick;
    Font.Style := [fsBold];
  end;
  Inc(YPos, 60);

  BtnReporteComunidades := TButton.Create(Panel);
  with BtnReporteComunidades do
  begin
    Parent := Panel;
    Caption := 'Generar Reporte de Comunidades';
    Left := 20;
    Top := YPos;
    Width := 300;
    Height := 40;
    OnClick := @OnReporteComunidadesClick;
    Font.Style := [fsBold];
  end;
  Inc(YPos, 80);

  // NUEVOS BOTONES PARA FASE 2
BtnCrearComunidadBST := TButton.Create(Panel);
with BtnCrearComunidadBST do
begin
  Parent := Panel;
  Caption := 'Crear Comunidad (BST)';
  Left := 20;
  Top := YPos;
  Width := 300;
  Height := 40;
  OnClick := @OnCrearComunidadBSTClick;
  Font.Style := [fsBold];
  Color := clLime;
end;
Inc(YPos, 60);

BtnVerMensajesComunidad := TButton.Create(Panel);
with BtnVerMensajesComunidad do
begin
  Parent := Panel;
  Caption := 'Ver Mensajes de Comunidad';
  Left := 20;
  Top := YPos;
  Width := 300;
  Height := 40;
  OnClick := @OnVerMensajesComunidadClick;
  Font.Style := [fsBold];
  Color := clLime;
end;
Inc(YPos, 60);
  BtnCerrarSesion := TButton.Create(Panel);
  with BtnCerrarSesion do
  begin
    Parent := Panel;
    Caption := 'Cerrar Sesión';
    Left := 20;
    Top := YPos;
    Width := 200;
    Height := 35;
    OnClick := @OnCerrarSesionClick;
    Font.Color := clRed;
    Font.Style := [fsBold];
  end;
end;


// Event handlers
procedure TInterfazEDDMail.OnLoginClick(Sender: TObject);
var
  Email, Password: String;
begin
  Email := Trim(FEditEmail.Text);
  Password := Trim(FEditPassword.Text);
  
  if (Email = '') or (Password = '') then
  begin
    MostrarMensaje('Error', 'Por favor ingrese email y password');
    Exit;
  end;
  
  if FSistema.IniciarSesion(Email, Password) then
  begin
    FUsuarioActivo := True;
    CrearFormPrincipal;
  end
  else
  begin
    MostrarMensaje('Error', 'Credenciales incorrectas');
    FEditPassword.Text := '';
    FEditPassword.SetFocus;
  end;
end;

procedure TInterfazEDDMail.OnCrearCuentaClick(Sender: TObject);
var
  FormRegistro: TForm;
  PanelRegistro: TPanel;
  EditNombre, EditUsuario, EditEmail, EditTelefono, EditPassword: TEdit;
  LabelNombre, LabelUsuario, LabelEmail, LabelTelefono, LabelPassword: TLabel;
  BtnRegistrar, BtnCancelar: TButton;
  YPos: Integer;
  ModalResult: Integer;
begin
  FormRegistro := TForm.Create(nil);
  try
    with FormRegistro do
    begin
      Caption := 'Crear Nueva Cuenta';
      Width := 400;
      Height := 450;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
    end;
    
    PanelRegistro := TPanel.Create(FormRegistro);
    with PanelRegistro do
    begin
      Parent := FormRegistro;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 15;
      Color:=clMoneyGreen ;
    end;
    
    YPos := 20;
    
    // Nombre
    LabelNombre := TLabel.Create(PanelRegistro);
    with LabelNombre do
    begin
      Parent := PanelRegistro;
      Caption := 'Nombre completo:';
      Left := 20;
      Top := YPos;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 25);
    
    EditNombre := TEdit.Create(PanelRegistro);
    with EditNombre do
    begin
      Parent := PanelRegistro;
      Left := 20;
      Top := YPos;
      Width := 340;
      TabOrder := 0;
    end;
    Inc(YPos, 40);
    
    // Usuario
    LabelUsuario := TLabel.Create(PanelRegistro);
    with LabelUsuario do
    begin
      Parent := PanelRegistro;
      Caption := 'Nombre de usuario:';
      Left := 20;
      Top := YPos;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 25);
    
    EditUsuario := TEdit.Create(PanelRegistro);
    with EditUsuario do
    begin
      Parent := PanelRegistro;
      Left := 20;
      Top := YPos;
      Width := 340;
      TabOrder := 1;
    end;
    Inc(YPos, 40);
    
    // Email
    LabelEmail := TLabel.Create(PanelRegistro);
    with LabelEmail do
    begin
      Parent := PanelRegistro;
      Caption := 'Email:';
      Left := 20;
      Top := YPos;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 25);
    
    EditEmail := TEdit.Create(PanelRegistro);
    with EditEmail do
    begin
      Parent := PanelRegistro;
      Left := 20;
      Top := YPos;
      Width := 340;
      TabOrder := 2;
    end;
    Inc(YPos, 40);
    
    // Teléfono
    LabelTelefono := TLabel.Create(PanelRegistro);
    with LabelTelefono do
    begin
      Parent := PanelRegistro;
      Caption := 'Teléfono:';
      Left := 20;
      Top := YPos;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 25);
    
    EditTelefono := TEdit.Create(PanelRegistro);
    with EditTelefono do
    begin
      Parent := PanelRegistro;
      Left := 20;
      Top := YPos;
      Width := 340;
      TabOrder := 3;
    end;
    Inc(YPos, 40);
    
    // Password
    LabelPassword := TLabel.Create(PanelRegistro);
    with LabelPassword do
    begin
      Parent := PanelRegistro;
      Caption := 'Password:';
      Left := 20;
      Top := YPos;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 25);
    
    EditPassword := TEdit.Create(PanelRegistro);
    with EditPassword do
    begin
      Parent := PanelRegistro;
      Left := 20;
      Top := YPos;
      Width := 340;
      PasswordChar := '*';
      TabOrder := 4;
    end;
    Inc(YPos, 50);
    
    // Botones
    BtnRegistrar := TButton.Create(PanelRegistro);
    with BtnRegistrar do
    begin
      Parent := PanelRegistro;
      Caption := 'Registrar';
      Left := 100;
      Top := YPos;
      Width := 80;
      Height := 30;
      ModalResult := mrOk;
      Default := True;
    end;
    
    BtnCancelar := TButton.Create(PanelRegistro);
    with BtnCancelar do
    begin
      Parent := PanelRegistro;
      Caption := 'Cancelar';
      Left := 200;
      Top := YPos;
      Width := 80;
      Height := 30;
      ModalResult := mrCancel;
      Cancel := True;
    end;
    
    ModalResult := FormRegistro.ShowModal;
    
    if ModalResult = mrOk then
    begin
      if (Trim(EditNombre.Text) = '') or (Trim(EditUsuario.Text) = '') or 
         (Trim(EditEmail.Text) = '') or (Trim(EditPassword.Text) = '') then
      begin
        MostrarMensaje('Error', 'Todos los campos son obligatorios');
        Exit;
      end;
      
      if FSistema.RegistrarUsuario(
        Trim(EditNombre.Text),
        Trim(EditUsuario.Text),
        Trim(EditEmail.Text),
        Trim(EditTelefono.Text),
        Trim(EditPassword.Text)
      ) then
      begin
        MostrarMensaje('Éxito', 'Usuario registrado correctamente');
        FEditEmail.Text := Trim(EditEmail.Text);
        FEditPassword.Text := '';
      end
      else
        MostrarMensaje('Error', 'Error al registrar usuario. El email ya existe.');
    end;
    
  finally
    FormRegistro.Free;
  end;
end;

procedure TInterfazEDDMail.OnCargaMasivaClick(Sender: TObject);
var
  OpenDialogUsuarios, OpenDialogCorreos: TOpenDialog;
begin
  // 1) Cargar USUARIOS
  OpenDialogUsuarios := TOpenDialog.Create(nil);
  try
    with OpenDialogUsuarios do
    begin
      Title := 'Seleccionar JSON de USUARIOS';
      Filter := 'Archivos JSON|*.json|Todos los archivos|*.*';
      DefaultExt := 'json';
      if Execute then
      begin
        try
          FSistema.CargarUsuariosDesdeJSON(FileName);
          MostrarMensaje('Éxito', 'Usuarios cargados desde: ' + ExtractFileName(FileName));
        except
          on E: Exception do
          begin
            MostrarMensaje('Error', 'Error al cargar usuarios: ' + E.Message);
            Exit;
          end;
        end;
      end
      else
        Exit; // cancelado
    end;
  finally
    OpenDialogUsuarios.Free;
  end;

  // 2) Cargar CORREOS
  OpenDialogCorreos := TOpenDialog.Create(nil);
  try
    with OpenDialogCorreos do
    begin
      Title := 'Seleccionar JSON de CORREOS';
      Filter := 'Archivos JSON|*.json|Todos los archivos|*.*';
      DefaultExt := 'json';
      if Execute then
      begin
        try
          FSistema.CargarCorreosDesdeJSON(FileName);
          MostrarMensaje('Éxito', 'Correos cargados desde: ' + ExtractFileName(FileName));
        except
          on E: Exception do
            MostrarMensaje('Error', 'Error al cargar correos: ' + E.Message);
        end;
      end;
    end;
  finally
    OpenDialogCorreos.Free;
  end;
end;

procedure TInterfazEDDMail.OnReporteUsuariosClick(Sender: TObject);
begin
  try
    FSistema.GenerarReporteUsuarios('Root-Reportes');
    MostrarMensaje('Éxito', 'Reporte de usuarios generado en: Root-Reportes/' + LineEnding +
      'Archivos generados:' + LineEnding +
      '- usuarios.dot (código Graphviz)' + LineEnding +
      '- usuarios.png (imagen)');
  except
    on E: Exception do
      MostrarMensaje('Error', 'Error al generar reporte: ' + E.Message);
  end;
end;

procedure TInterfazEDDMail.OnReporteRelacionesClick(Sender: TObject);
begin
  try
    FSistema.GenerarReporteRelaciones('Root-Reportes');
    MostrarMensaje('Éxito', 'Reporte de relaciones generado en: Root-Reportes/' + LineEnding +
      'Archivos generados:' + LineEnding +
      '- relaciones.dot (código Graphviz)' + LineEnding +
      '- relaciones.png (imagen)');
  except
    on E: Exception do
      MostrarMensaje('Error', 'Error al generar reporte: ' + E.Message);
  end;
end;

procedure TInterfazEDDMail.OnCerrarSesionClick(Sender: TObject);
begin
  FSistema.CerrarSesion;
  FUsuarioActivo := False;
  
  if Assigned(FFormPrincipal) then
  begin
    FFormPrincipal.Close;
    FFormPrincipal := nil;
  end;
  
  FEditEmail.Text := '';
  FEditPassword.Text := '';
  FFormLogin.Show;
   FFormLogin.BringToFront;    // <- Agregar esta línea
  FFormLogin.SetFocus;        // <- Agregar esta línea
  FEditEmail.SetFocus;
end;

procedure TInterfazEDDMail.OnFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Sender = FFormLogin then
  begin
    Application.Terminate;
  end
  else if Sender = FFormPrincipal then
  begin
    CloseAction := caFree;
    FFormPrincipal := nil;
    if Assigned(FFormLogin) then
      FFormLogin.Show;
  end;
end;

procedure TInterfazEDDMail.OnKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then // Enter
  begin
    OnLoginClick(Sender);
    Key := #0;
  end;
end;

procedure TInterfazEDDMail.MostrarMensaje(Titulo, Mensaje: String);
begin
  ShowMessage(Mensaje);
end;
procedure TInterfazEDDMail.OnGestionarComunidadesClick(Sender: TObject);
var
  FormComunidades: TForm;
  PanelComunidades: TPanel;
  LabelTitulo, LabelNombreCom, LabelUsuario: TLabel;
  BtnCrearComunidad, BtnAsignarUsuario, BtnListarComunidades, BtnCerrar: TButton;
  YPos: Integer;
begin
  FormComunidades := TForm.Create(nil);
  try
    with FormComunidades do
    begin
      Caption := 'Gestión de Comunidades';
      Width := 600;
      Height := 500;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color:=clMoneyGreen;
    end;

    PanelComunidades := TPanel.Create(FormComunidades);
    with PanelComunidades do
    begin
      Parent := FormComunidades;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 15;
      Color:=clMoneyGreen;
    end;

    YPos := 20;

    LabelTitulo := TLabel.Create(PanelComunidades);
    with LabelTitulo do
    begin
      Parent := PanelComunidades;
      Caption := 'Gestión de Comunidades';
      Font.Size := 14;
      Font.Style := [fsBold];
      Left := 20;
      Top := YPos;
    end;
    Inc(YPos, 40);

    // Crear Comunidad
    LabelNombreCom := TLabel.Create(PanelComunidades);
    with LabelNombreCom do
    begin
      Parent := PanelComunidades;
      Caption := 'Nombre de la Comunidad:';
      Left := 20;
      Top := YPos;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 25);

    FEditNombreComunidad := TEdit.Create(PanelComunidades);
    with FEditNombreComunidad do
    begin
      Parent := PanelComunidades;
      Left := 20;
      Top := YPos;
      Width := 300;
    end;

    BtnCrearComunidad := TButton.Create(PanelComunidades);
    with BtnCrearComunidad do
    begin
      Parent := PanelComunidades;
      Caption := 'Crear';
      Left := 330;
      Top := YPos - 2;
      Width := 80;
      Height := 25;
      OnClick := @Self.OnCrearComunidadClick;
    end;
    Inc(YPos, 50);

    // Asignar Usuario
    LabelUsuario := TLabel.Create(PanelComunidades);
    with LabelUsuario do
    begin
      Parent := PanelComunidades;
      Caption := 'Email del Usuario a Asignar:';
      Left := 20;
      Top := YPos;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 25);

    FEditEmailUsuario := TEdit.Create(PanelComunidades);
    with FEditEmailUsuario do
    begin
      Parent := PanelComunidades;
      Left := 20;
      Top := YPos;
      Width := 300;
    end;

    BtnAsignarUsuario := TButton.Create(PanelComunidades);
    with BtnAsignarUsuario do
    begin
      Parent := PanelComunidades;
      Caption := 'Asignar';
      Left := 330;
      Top := YPos - 2;
      Width := 80;
      Height := 25;
      OnClick := @Self.OnAsignarUsuarioClick;
    end;
    Inc(YPos, 50);

    // Lista de comunidades
    FMemoComunidades := TMemo.Create(PanelComunidades);
    with FMemoComunidades do
    begin
      Parent := PanelComunidades;
      Left := 20;
      Top := YPos;
      Width := 520;
      Height := 200;
      ReadOnly := True;
      ScrollBars := ssVertical;
    end;

    BtnListarComunidades := TButton.Create(PanelComunidades);
    with BtnListarComunidades do
    begin
      Parent := PanelComunidades;
      Caption := 'Listar Comunidades';
      Left := 20;
      Top := YPos + 210;
      Width := 150;
      Height := 30;
      OnClick := @Self.OnListarComunidadesClick;
    end;

    BtnCerrar := TButton.Create(PanelComunidades);
    with BtnCerrar do
    begin
      Parent := PanelComunidades;
      Caption := 'Cerrar';
      Left := 460;
      Top := YPos + 210;
      Width := 80;
      Height := 30;
      ModalResult := mrCancel;
    end;

    FormComunidades.ShowModal;

  finally
    FormComunidades.Free;
  end;
end;

procedure TInterfazEDDMail.OnCrearComunidadClick(Sender: TObject);
begin
  if Trim(FEditNombreComunidad.Text) <> '' then
  begin
    if FSistema.CrearComunidad(Trim(FEditNombreComunidad.Text)) then
    begin
      MostrarMensaje('Éxito', 'Comunidad creada: ' + FEditNombreComunidad.Text);
      FEditNombreComunidad.Text := '';
    end
    else
      MostrarMensaje('Error', 'Error: La comunidad ya existe');
  end
  else
    MostrarMensaje('Error', 'Ingrese un nombre para la comunidad');
end;

procedure TInterfazEDDMail.OnAsignarUsuarioClick(Sender: TObject);
begin
  if (Trim(FEditNombreComunidad.Text) <> '') and (Trim(FEditEmailUsuario.Text) <> '') then
  begin
    if FSistema.AgregarUsuarioAComunidad(Trim(FEditNombreComunidad.Text), Trim(FEditEmailUsuario.Text)) then
    begin
      MostrarMensaje('Éxito', 'Usuario asignado correctamente');
      FEditEmailUsuario.Text := '';
    end
    else
      MostrarMensaje('Error', 'Error: Comunidad no existe o usuario no encontrado');
  end
  else
    MostrarMensaje('Error', 'Complete ambos campos');
end;

procedure TInterfazEDDMail.OnListarComunidadesClick(Sender: TObject);
begin
  FMemoComunidades.Lines.Text := FSistema.ListarComunidades;
end;

// Actualizar OnReporteComunidadesClick para usar el nuevo árbol BST
procedure TInterfazEDDMail.OnReporteComunidadesClick(Sender: TObject);
var
  FormTipoReporte: TForm;
  Panel: TPanel;
  LabelTitulo: TLabel;
  BtnReporteAntiguo, BtnReporteNuevo, BtnCancelar: TButton;
begin
  // Crear formulario para elegir tipo de reporte
  FormTipoReporte := TForm.Create(nil);
  try
    with FormTipoReporte do
    begin
      Caption := 'Tipo de Reporte de Comunidades';
      Width := 400;
      Height := 250;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color := clMoneyGreen;
    end;

    Panel := TPanel.Create(FormTipoReporte);
    with Panel do
    begin
      Parent := FormTipoReporte;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 15;
      Color := clMoneyGreen;
    end;

    LabelTitulo := TLabel.Create(Panel);
    with LabelTitulo do
    begin
      Parent := Panel;
      Caption := 'Seleccione el tipo de reporte:';
      Font.Size := 12;
      Font.Style := [fsBold];
      Left := 20;
      Top := 20;
    end;

    BtnReporteAntiguo := TButton.Create(Panel);
    with BtnReporteAntiguo do
    begin
      Parent := Panel;
      Caption := 'Reporte Fase 1 (Lista de Listas)';
      Left := 20;
      Top := 60;
      Width := 340;
      Height := 40;
      ModalResult := mrYes;
      Hint := 'Reporte usando la estructura original';
      ShowHint := True;
    end;

    BtnReporteNuevo := TButton.Create(Panel);
    with BtnReporteNuevo do
    begin
      Parent := Panel;
      Caption := 'Reporte Fase 2 (Árbol BST) 🆕';
      Left := 20;
      Top := 110;
      Width := 340;
      Height := 40;
      ModalResult := mrOk;
      Font.Style := [fsBold];
      Color := clLime;
      Hint := 'Reporte usando árbol BST';
      ShowHint := True;
    end;

    BtnCancelar := TButton.Create(Panel);
    with BtnCancelar do
    begin
      Parent := Panel;
      Caption := 'Cancelar';
      Left := 280;
      Top := 170;
      Width := 80;
      Height := 30;
      ModalResult := mrCancel;
    end;

    case FormTipoReporte.ShowModal of
      mrOk: begin // Reporte nuevo (BST)
        try
          FSistema.GenerarReporteComunidadesBST('Root-Reportes');
          MostrarMensaje('Éxito',
            'Reporte de comunidades BST generado en: Root-Reportes/' + LineEnding +
            'Archivos generados:' + LineEnding +
            '- comunidades_bst.dot (código Graphviz)' + LineEnding +
            '- comunidades_bst.png (imagen)');
        except
          on E: Exception do
            MostrarMensaje('Error', 'Error al generar reporte BST: ' + E.Message);
        end;
      end;

      mrYes: begin // Reporte antiguo (Lista de Listas)
        try
          FSistema.GenerarReporteComunidades('Root-Reportes');
          MostrarMensaje('Éxito',
            'Reporte de comunidades (Fase 1) generado en: Root-Reportes/' + LineEnding +
            'Archivos generados:' + LineEnding +
            '- comunidades.dot (código Graphviz)' + LineEnding +
            '- comunidades.png (imagen)');
        except
          on E: Exception do
            MostrarMensaje('Error', 'Error al generar reporte: ' + E.Message);
        end;
      end;
    end;

  finally
    FormTipoReporte.Free;
  end;
end;

// Función para mostrar mensajes de una comunidad específica (ROOT)
function TEDDMailSystem.ObtenerMensajesComunidad(nombreComunidad: String): String;
var
  Comunidad: PNodoBST;
  Mensaje: PMensajeComunidad;
begin
  Result := '';
  Comunidad := BuscarComunidadBST(FArbolComunidades, nombreComunidad);

  if Comunidad = nil then
  begin
    Result := 'Comunidad no encontrada: ' + nombreComunidad;
    Exit;
  end;

  Result := 'Comunidad: ' + Comunidad^.NombreComunidad + LineEnding;
  Result := Result + 'Fecha de creación: ' + Comunidad^.FechaCreacion + LineEnding;
  Result := Result + 'Total de mensajes: ' + IntToStr(Comunidad^.NumeroMensajes) + LineEnding;
  Result := Result + '----------------------------------------' + LineEnding + LineEnding;

  Mensaje := Comunidad^.ListaMensajes;
  if Mensaje = nil then
  begin
    Result := Result + 'No hay mensajes publicados en esta comunidad.';
    Exit;
  end;

  while Mensaje <> nil do
  begin
    Result := Result + 'De: ' + Mensaje^.Correo + LineEnding;
    Result := Result + 'Fecha: ' + Mensaje^.FechaPublicacion + LineEnding;
    Result := Result + 'Mensaje: ' + Mensaje^.Mensaje + LineEnding;
    Result := Result + '------------------------' + LineEnding;
    Mensaje := Mensaje^.Siguiente;
  end;
end;
procedure TInterfazEDDMail.OnActualizarPerfilClick(Sender: TObject);
var
  FormPerfil: TForm;
  PanelPerfil: TPanel;
  LabelTitulo, LabelNombre, LabelUsuario, LabelTelefono, LabelEmail: TLabel;
  EditNombre, EditUsuario, EditTelefono: TEdit;
  BtnActualizar, BtnCancelar: TButton;
  Usuario: PUsuario;
  YPos: Integer;
  ModalResult: Integer;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then
    Exit;

  FormPerfil := TForm.Create(nil);
  try
    with FormPerfil do
    begin
      Caption := 'Actualizar Perfil';
      Width := 400;
      Height := 350;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color:=clMoneyGreen ;
    end;

    PanelPerfil := TPanel.Create(FormPerfil);
    with PanelPerfil do
    begin
      Parent := FormPerfil;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 15;
    end;

    YPos := 20;

    LabelTitulo := TLabel.Create(PanelPerfil);
    with LabelTitulo do
    begin
      Parent := PanelPerfil;
      Caption := 'Actualizar Información Personal';
      Font.Size := 12;
      Font.Style := [fsBold];
      Left := 20;
      Top := YPos;
    end;
    Inc(YPos, 40);

    // Email (solo mostrar, no editable)
    LabelEmail := TLabel.Create(PanelPerfil);
    with LabelEmail do
    begin
      Parent := PanelPerfil;
      Caption := 'Email (no modificable): ' + Usuario^.Email;
      Left := 20;
      Top := YPos;
      Font.Color := clGray;
    end;
    Inc(YPos, 30);

    // Nombre
    LabelNombre := TLabel.Create(PanelPerfil);
    with LabelNombre do
    begin
      Parent := PanelPerfil;
      Caption := 'Nombre completo:';
      Left := 20;
      Top := YPos;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 20);

    EditNombre := TEdit.Create(PanelPerfil);
    with EditNombre do
    begin
      Parent := PanelPerfil;
      Left := 20;
      Top := YPos;
      Width := 340;
      Text := Usuario^.Nombre;
      TabOrder := 0;
    end;
    Inc(YPos, 40);

    // Usuario
    LabelUsuario := TLabel.Create(PanelPerfil);
    with LabelUsuario do
    begin
      Parent := PanelPerfil;
      Caption := 'Nombre de usuario:';
      Left := 20;
      Top := YPos;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 20);

    EditUsuario := TEdit.Create(PanelPerfil);
    with EditUsuario do
    begin
      Parent := PanelPerfil;
      Left := 20;
      Top := YPos;
      Width := 340;
      Text := Usuario^.Usuario;
      TabOrder := 1;
    end;
    Inc(YPos, 40);

    // Teléfono
    LabelTelefono := TLabel.Create(PanelPerfil);
    with LabelTelefono do
    begin
      Parent := PanelPerfil;
      Caption := 'Teléfono:';
      Left := 20;
      Top := YPos;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 20);

    EditTelefono := TEdit.Create(PanelPerfil);
    with EditTelefono do
    begin
      Parent := PanelPerfil;
      Left := 20;
      Top := YPos;
      Width := 340;
      Text := Usuario^.Telefono;
      TabOrder := 2;
    end;
    Inc(YPos, 50);

    // Botones
    BtnActualizar := TButton.Create(PanelPerfil);
    with BtnActualizar do
    begin
      Parent := PanelPerfil;
      Caption := 'Actualizar';
      Left := 100;
      Top := YPos;
      Width := 80;
      Height := 30;
      ModalResult := mrOk;
      Default := True;
    end;

    BtnCancelar := TButton.Create(PanelPerfil);
    with BtnCancelar do
    begin
      Parent := PanelPerfil;
      Caption := 'Cancelar';
      Left := 200;
      Top := YPos;
      Width := 80;
      Height := 30;
      ModalResult := mrCancel;
      Cancel := True;
    end;

    ModalResult := FormPerfil.ShowModal;

    if ModalResult = mrOk then
    begin
      if Trim(EditNombre.Text) = '' then
      begin
        MostrarMensaje('Error', 'El nombre no puede estar vacío');
        Exit;
      end;

      FSistema.ActualizarPerfil(Usuario,
        Trim(EditNombre.Text),
        Trim(EditUsuario.Text),
        Trim(EditTelefono.Text));

      MostrarMensaje('Éxito', 'Perfil actualizado correctamente');

    end;

  finally
    FormPerfil.Free;
  end;
end;
procedure TInterfazEDDMail.OnAgregarContactoClick(Sender: TObject);
var
  FormAgregarContacto: TForm;
  PanelContacto: TPanel;
  LabelTitulo, LabelEmail: TLabel;
  EditEmail: TEdit;
  BtnAgregar, BtnCancelar: TButton;
  ModalResult: Integer;
begin
  if FSistema.GetUsuarioActual = nil then
    Exit;

  FormAgregarContacto := TForm.Create(nil);
  try
    with FormAgregarContacto do
    begin
      Caption := 'Agregar Contacto';
      Width := 400;
      Height := 250;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color:= clMoneyGreen ;
    end;

    PanelContacto := TPanel.Create(FormAgregarContacto);
    with PanelContacto do
    begin
      Parent := FormAgregarContacto;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 15;
    end;

    LabelTitulo := TLabel.Create(PanelContacto);
    with LabelTitulo do
    begin
      Parent := PanelContacto;
      Caption := 'Agregar Nuevo Contacto';
      Font.Size := 12;
      Font.Style := [fsBold];
      Left := 20;
      Top := 20;
      Color:=clTeal;
    end;

    LabelEmail := TLabel.Create(PanelContacto);
    with LabelEmail do
    begin
      Parent := PanelContacto;
      Caption := 'Email del contacto:';
      Left := 20;
      Top := 60;
      Font.Style := [fsBold];
    end;

    EditEmail := TEdit.Create(PanelContacto);
    with EditEmail do
    begin
      Parent := PanelContacto;
      Left := 20;
      Top := 80;
      Width := 340;
      TabOrder := 0;
      Hint := 'Ingrese el email del usuario que desea agregar';
      ShowHint := True;
    end;

    BtnAgregar := TButton.Create(PanelContacto);
    with BtnAgregar do
    begin
      Parent := PanelContacto;
      Caption := 'Agregar';
      Left := 120;
      Top := 130;
      Width := 80;
      Height := 30;
      ModalResult := mrOk;
      Default := True;
    end;

    BtnCancelar := TButton.Create(PanelContacto);
    with BtnCancelar do
    begin
      Parent := PanelContacto;
      Caption := 'Cancelar';
      Left := 210;
      Top := 130;
      Width := 80;
      Height := 30;
      ModalResult := mrCancel;
      Cancel := True;
    end;

    ModalResult := FormAgregarContacto.ShowModal;

    if ModalResult = mrOk then
    begin
      if Trim(EditEmail.Text) = '' then
      begin
        MostrarMensaje('Error', 'Debe ingresar un email');
        Exit;
      end;

      if FSistema.AgregarContacto(FSistema.GetUsuarioActual, Trim(EditEmail.Text)) then
        MostrarMensaje('Éxito', 'Contacto agregado correctamente')
      else
        MostrarMensaje('Error', 'No se pudo agregar el contacto. Verifique que:' + LineEnding +
                       '- El email existe en el sistema' + LineEnding +
                       '- No sea su propio email' + LineEnding +
                       '- No esté ya en sus contactos');
    end;

  finally
    FormAgregarContacto.Free;
  end;
end;
procedure TInterfazEDDMail.OnVerContactosClick(Sender: TObject);
var
  PanelContactos, PanelInfo: TPanel;
  LabelTitulo: TLabel;
  BtnAnterior, BtnSiguiente, BtnAgregar, BtnReporte, BtnCerrar: TButton;
  Usuario: PUsuario;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then
    Exit;

  // Si ya existe la ventana, solo mostrarla
  if FFormContactos <> nil then
  begin
    FFormContactos.Show;
    FFormContactos.BringToFront;
    Exit;
  end;

  // Inicializar variables de navegación
  FPrimerContacto := FSistema.GetContactos(Usuario);
  FContactoActual := FPrimerContacto;
  FIndiceContactoActual := 1;
  FTotalContactos := FSistema.ContarContactos(FPrimerContacto);

  FFormContactos := TForm.Create(nil);
  with FFormContactos do
  begin
    Caption := 'Mis Contactos';
    Width := 500;
    Height := 400;
    Position := poOwnerFormCenter;
    BorderStyle := bsSizeable;
    OnClose := @OnFormContactosClose;  // Agregar evento de cierre
    Color:=clMoneyGreen;
  end;

  PanelContactos := TPanel.Create(FFormContactos);
  with PanelContactos do
  begin
    Parent := FFormContactos;
    Align := alClient;
    BevelOuter := bvNone;
    BorderWidth := 10;
  end;

  LabelTitulo := TLabel.Create(PanelContactos);
  with LabelTitulo do
  begin
    Parent := PanelContactos;
    Caption := 'Lista de Contactos 📇';
    Font.Size := 14;
    Font.Style := [fsBold];
    Left := 20;
    Top := 20;
  end;

  FLabelContadorContactos := TLabel.Create(PanelContactos);
  with FLabelContadorContactos do
  begin
    Parent := PanelContactos;
    Left := 20;
    Top := 50;
    Font.Color := clGray;
    if FTotalContactos = 0 then
      Caption := 'No tiene contactos agregados'
    else
      Caption := Format('Contacto %d de %d', [FIndiceContactoActual, FTotalContactos]);
  end;

  // Panel para información del contacto
  PanelInfo := TPanel.Create(PanelContactos);
  with PanelInfo do
  begin
    Parent := PanelContactos;
    Left := 20;
    Top := 80;
    Width := 440;
    Height := 180;
    BevelOuter := bvRaised;
    Color := clWhite;
  end;

  // Crear labels para información del contacto con variables de instancia
  if FContactoActual <> nil then
  begin
    FLabelNombreContacto := TLabel.Create(PanelInfo);
    with FLabelNombreContacto do
    begin
      Parent := PanelInfo;
      Caption := 'Nombre: ' + FContactoActual^.Nombre;
      Left := 20;
      Top := 20;
      Font.Style := [fsBold];
    end;

    FLabelUsuarioContacto := TLabel.Create(PanelInfo);
    with FLabelUsuarioContacto do
    begin
      Parent := PanelInfo;
      Caption := 'Usuario: ' + FContactoActual^.Usuario;
      Left := 20;
      Top := 50;
    end;

    FLabelEmailContacto := TLabel.Create(PanelInfo);
    with FLabelEmailContacto do
    begin
      Parent := PanelInfo;
      Caption := 'Email: ' + FContactoActual^.Email;
      Left := 20;
      Top := 80;
      Font.Color := clBlue;
    end;

    FLabelTelefonoContacto := TLabel.Create(PanelInfo);
    with FLabelTelefonoContacto do
    begin
      Parent := PanelInfo;
      Caption := 'Teléfono: ' + FContactoActual^.Telefono;
      Left := 20;
      Top := 110;
    end;
  end;

  // Botones de navegación
  BtnAnterior := TButton.Create(PanelContactos);
  with BtnAnterior do
  begin
    Parent := PanelContactos;
    Caption := '< Anterior';
    Left := 20;
    Top := 280;
    Width := 100;
    Height := 30;
    Enabled := FTotalContactos > 1;
    OnClick := @OnContactoAnteriorClick;
  end;

  BtnSiguiente := TButton.Create(PanelContactos);
  with BtnSiguiente do
  begin
    Parent := PanelContactos;
    Caption := 'Siguiente >';
    Left := 130;
    Top := 280;
    Width := 100;
    Height := 30;
    Enabled := FTotalContactos > 1;
    OnClick := @OnContactoSiguienteClick;
  end;

  BtnAgregar := TButton.Create(PanelContactos);
  with BtnAgregar do
  begin
    Parent := PanelContactos;
    Caption := 'Agregar Contacto';
    Left := 250;
    Top := 280;
    Width := 120;
    Height := 30;
    OnClick := @OnAgregarContactoClick;
  end;

  BtnReporte := TButton.Create(PanelContactos);
  with BtnReporte do
  begin
    Parent := PanelContactos;
    Caption := 'Generar Reporte';
    Left := 20;
    Top := 320;
    Width := 120;
    Height := 30;
    OnClick := @OnGenerarReporteContactosClick;
  end;

  BtnCerrar := TButton.Create(PanelContactos);
  with BtnCerrar do
  begin
    Parent := PanelContactos;
    Caption := 'Cerrar';
    Left := 380;
    Top := 320;
    Width := 80;
    Height := 30;
    ModalResult := mrCancel;
  end;

  FFormContactos.ShowModal;
end;

procedure TInterfazEDDMail.OnContactoAnteriorClick(Sender: TObject);
var
  Actual: PContacto;
  Contador: Integer;
begin
  if (FContactoActual = nil) or (FTotalContactos <= 1) then
    Exit;

  // Buscar el contacto anterior en la lista circular
  Actual := FPrimerContacto;
  Contador := 1;

  while (Actual^.Siguiente <> FContactoActual) and (Contador < FTotalContactos) do
  begin
    Actual := Actual^.Siguiente;
    Inc(Contador);
  end;

  FContactoActual := Actual;
  Dec(FIndiceContactoActual);
  if FIndiceContactoActual < 1 then
    FIndiceContactoActual := FTotalContactos;

  // Actualizar la vista
  ActualizarVistaContacto;
end;

procedure TInterfazEDDMail.OnContactoSiguienteClick(Sender: TObject);
begin
  if (FContactoActual = nil) or (FTotalContactos <= 1) then
    Exit;

  FContactoActual := FContactoActual^.Siguiente;
  Inc(FIndiceContactoActual);
  if FIndiceContactoActual > FTotalContactos then
    FIndiceContactoActual := 1;

  // Actualizar la vista
  ActualizarVistaContacto;
end;


procedure TInterfazEDDMail.OnGenerarReporteContactosClick(Sender: TObject);
var
  Usuario: PUsuario;
  CarpetaReportes: String;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then
    Exit;

  CarpetaReportes := Usuario^.Usuario + '-Reportes';

  try
    FSistema.GenerarReporteContactos(Usuario, CarpetaReportes);
    MostrarMensaje('Éxito', 'Reporte de contactos generado en: ' + CarpetaReportes + LineEnding +
      'Archivos generados:' + LineEnding +
      '- contactos_' + Usuario^.Usuario + '.dot (código Graphviz)' + LineEnding +
      '- contactos_' + Usuario^.Usuario + '.png (imagen)');
  except
    on E: Exception do
      MostrarMensaje('Error', 'Error al generar reporte: ' + E.Message);
  end;
end;
procedure TInterfazEDDMail.ActualizarVistaContacto;
begin
  if FContactoActual = nil then
    Exit;

  if FLabelContadorContactos <> nil then
    FLabelContadorContactos.Caption := Format('Contacto %d de %d', [FIndiceContactoActual, FTotalContactos]);

  if FLabelNombreContacto <> nil then
    FLabelNombreContacto.Caption := 'Nombre: ' + FContactoActual^.Nombre;

  if FLabelUsuarioContacto <> nil then
    FLabelUsuarioContacto.Caption := 'Usuario: ' + FContactoActual^.Usuario;

  if FLabelEmailContacto <> nil then
    FLabelEmailContacto.Caption := 'Email: ' + FContactoActual^.Email;

  if FLabelTelefonoContacto <> nil then
    FLabelTelefonoContacto.Caption := 'Teléfono: ' + FContactoActual^.Telefono;
end;
procedure TInterfazEDDMail.OnFormContactosClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  FFormContactos := nil;
  FLabelContadorContactos := nil;
  FLabelNombreContacto := nil;
  FLabelUsuarioContacto := nil;
  FLabelEmailContacto := nil;
  FLabelTelefonoContacto := nil;
end;
procedure TInterfazEDDMail.OnEnviarCorreoClick(Sender: TObject);
var
  FormEnviar: TForm;
  Panel: TPanel;
  LabelPara, LabelAsunto: TLabel;
  EditPara, EditAsunto: TEdit;
  MemoCuerpo: TMemo;
  BtnEnviar, BtnBorrador, BtnCancelar: TButton;
  Usuario: PUsuario;
  ResultadoModal: Integer;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  FormEnviar := TForm.Create(nil);
  try
    with FormEnviar do
    begin
      Caption := 'Enviar correo';
      Width := 650; // Aumentar ancho para el nuevo botón
      Height := 450;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color := clMoneyGreen;
    end;

    Panel := TPanel.Create(FormEnviar);
    with Panel do
    begin
      Parent := FormEnviar;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 12;
      Color := clMoneyGreen;
    end;

    LabelPara := TLabel.Create(Panel);
    with LabelPara do
    begin
      Parent := Panel;
      Caption := 'Para:';
      Left := 12;
      Top := 12;
      Font.Style := [fsBold];
    end;

    EditPara := TEdit.Create(Panel);
    with EditPara do
    begin
      Parent := Panel;
      Left := 12;
      Top := 30;
      Width := 600; // Ajustar ancho
      TabOrder := 0;
    end;

    LabelAsunto := TLabel.Create(Panel);
    with LabelAsunto do
    begin
      Parent := Panel;
      Caption := 'Asunto:';
      Left := 12;
      Top := 60;
      Font.Style := [fsBold];
    end;

    EditAsunto := TEdit.Create(Panel);
    with EditAsunto do
    begin
      Parent := Panel;
      Left := 12;
      Top := 78;
      Width := 600; // Ajustar ancho
      Height := 25; // Corregir altura
      TabOrder := 1;
    end;

    MemoCuerpo := TMemo.Create(Panel);
    with MemoCuerpo do
    begin
      Parent := Panel;
      Left := 12;
      Top := 115;
      Width := 600; // Ajustar ancho
      Height := 250;
      ScrollBars := ssVertical;
      TabOrder := 2;
    end;

    // BOTÓN ENVIAR
    BtnEnviar := TButton.Create(Panel);
    with BtnEnviar do
    begin
      Parent := Panel;
      Caption := 'Enviar';
      Left := 320;
      Top := 380;
      Width := 90;
      Height := 30;
      ModalResult := mrOk;
      Default := True;
      TabOrder := 3;
      Font.Style := [fsBold];
      Color := clLime;
    end;

    // BOTÓN GUARDAR BORRADOR (NUEVO)
    BtnBorrador := TButton.Create(Panel);
    with BtnBorrador do
    begin
      Parent := Panel;
      Caption := '📝 Guardar Borrador';
      Left := 420;
      Top := 380;
      Width := 130;
      Height := 30;
      ModalResult := mrYes; // Usar mrYes para identificar borrador
      TabOrder := 4;
      Font.Style := [fsBold];
      Color := clAqua;
      Hint := 'Guarda el correo como borrador para enviarlo después';
      ShowHint := True;
    end;

    // BOTÓN CANCELAR
    BtnCancelar := TButton.Create(Panel);
    with BtnCancelar do
    begin
      Parent := Panel;
      Caption := 'Cancelar';
      Left := 560;
      Top := 380;
      Width := 70;
      Height := 30;
      ModalResult := mrCancel;
      Cancel := True;
      TabOrder := 5;
    end;

    // MOSTRAR MODAL Y PROCESAR RESULTADO
    ResultadoModal := FormEnviar.ShowModal;

    case ResultadoModal of
      mrOk: begin // ENVIAR CORREO
        if (Trim(EditPara.Text) = '') or (Trim(EditAsunto.Text) = '') then
        begin
          MostrarMensaje('Error', 'Debe ingresar al menos destinatario y asunto');
          Exit;
        end;

        // 🔹 ENVIAR CORREO INMEDIATAMENTE
        if FCorreoManager.EnviarCorreo(
              FSistema,           // sistema para buscar usuarios y actualizar matriz
              Usuario^.Email,     // remitente (el usuario logueado)
              Trim(EditPara.Text),
              Trim(EditAsunto.Text),
              MemoCuerpo.Lines.Text) then
        begin
          MostrarMensaje('Éxito', 'Correo enviado correctamente a: ' + Trim(EditPara.Text));
        end
        else
        begin
          MostrarMensaje('Error',
            'No se pudo enviar el correo.' + LineEnding +
            'Verifique que el destinatario exista y que esté en su lista de contactos.');
        end;
      end;

      mrYes: begin // GUARDAR COMO BORRADOR
        if (Trim(EditPara.Text) = '') or (Trim(EditAsunto.Text) = '') then
        begin
          MostrarMensaje('Error', 'Debe ingresar al menos destinatario y asunto para guardar el borrador');
          Exit;
        end;

        // 🔹 GUARDAR EN ÁRBOL AVL DE BORRADORES
        if FSistema.GuardarBorrador(
              Usuario,
              Trim(EditPara.Text),
              Trim(EditAsunto.Text),
              MemoCuerpo.Lines.Text) then
        begin
          MostrarMensaje('Éxito',
            'Borrador guardado correctamente' + LineEnding +
            'Podrá editarlo y enviarlo desde "📝 Ver Borradores"');
        end
        else
        begin
          MostrarMensaje('Error', 'No se pudo guardar el borrador');
        end;
      end;

      mrCancel: begin
        // Usuario canceló - no hacer nada
      end;
    end;

  finally
    FormEnviar.Free;
  end;
end;
procedure TInterfazEDDMail.OnBandejaClick(Sender: TObject);
var
  Panel: TPanel;
  LabelTitulo: TLabel;
  BtnOrdenar, BtnMarcarLeido, BtnEliminar, BtnCerrar: TButton;
begin
  if FSistema.GetUsuarioActual = nil then Exit;

  // si ya existe, solo mostrar
  if Assigned(FFormBandeja) then
  begin
    FFormBandeja.Show;
    FFormBandeja.BringToFront;
    Exit;
  end;

  FFormBandeja := TForm.Create(nil);
  with FFormBandeja do
  begin
    Caption := 'Bandeja de Entrada';
    Width := 700;
    Height := 480;
    Position := poOwnerFormCenter;
    BorderStyle := bsSizeable;
    OnClose := @OnFormBandejaClose;
    Color:=clMoneyGreen ;
  end;

  Panel := TPanel.Create(FFormBandeja);
  with Panel do
  begin
    Parent := FFormBandeja;
    Align := alClient;
    BevelOuter := bvNone;
    BorderWidth := 10;
  end;

  LabelTitulo := TLabel.Create(Panel);
  with LabelTitulo do
  begin
    Parent := Panel;
    Caption := 'Bandeja de Entrada✉️ ';
    Font.Size := 14;
    Font.Style := [fsBold];
    Left := 10;
    Top := 10;
  end;

  FLabelNoLeidosInbox := TLabel.Create(Panel);
  with FLabelNoLeidosInbox do
  begin
    Parent := Panel;
    Caption := 'No leídos: 0';
    Left := 200;
    Top := 14;
    Font.Color := clGray;
  end;

  // Lista de correos
  FListBandeja := TListBox.Create(Panel);
  with FListBandeja do
  begin
    Parent := Panel;
    Left := 10;
    Top := 40;
    Width := 660;
    Height := 240;
    OnClick := @Inbox_OnSeleccion;
    ItemHeight := 16;
  end;

  // Memo mensaje
  FMemoMensaje := TMemo.Create(Panel);
  with FMemoMensaje do
  begin
    Parent := Panel;
    Left := 10;
    Top := 290;
    Width := 660;
    Height := 120;
    ReadOnly := True;
    ScrollBars := ssVertical;
  end;

  BtnOrdenar := TButton.Create(Panel);
  with BtnOrdenar do
  begin
    Parent := Panel;
    Caption := 'Ordenar A-Z';
    Left := 10; Top := 420; Width := 120; Height := 30;
    OnClick := @Inbox_OnOrdenarClick;
  end;

  BtnMarcarLeido := TButton.Create(Panel);
  with BtnMarcarLeido do
  begin
    Parent := Panel;
    Caption := 'Marcar Leído';
    Left := 140; Top := 420; Width := 120; Height := 30;
    OnClick := @Inbox_OnMarcarLeidoClick;
  end;

  // NUEVO BOTÓN: Marcar como Favorito
  BtnMarcarFavorito := TButton.Create(Panel);
  with BtnMarcarFavorito do
  begin
    Parent := Panel;
    Caption := '⭐ Favorito';
    Left := 270; Top := 420; Width := 120; Height := 30;
    OnClick := @Inbox_OnMarcarFavoritoClick;
    Font.Style := [fsBold];
    Color := clYellow;
  end;

  BtnEliminar := TButton.Create(Panel);
  with BtnEliminar do
  begin
    Parent := Panel;
    Caption := 'Eliminar';
    Left := 400; Top := 420; Width := 120; Height := 30;
    OnClick := @Inbox_OnEliminarClick;
  end;

  BtnCerrar := TButton.Create(Panel);
  with BtnCerrar do
  begin
    Parent := Panel;
    Caption := 'Cerrar';
    Left := 530; Top := 420; Width := 140; Height := 30;
    OnClick := @Inbox_OnCerrarClick;
    Cancel := True;
  end;

  // Cargar datos
  Inbox_RellenarLista;

  FFormBandeja.Show;
end;

procedure TInterfazEDDMail.OnFormBandejaClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  FFormBandeja := nil;
  FListBandeja := nil;
  FMemoMensaje := nil;
  FLabelNoLeidosInbox := nil;
end;

// Llena la lista desde la estructura de datos
procedure TInterfazEDDMail.Inbox_RellenarLista;
var
  Usuario: PUsuario;
  Correo: PCorreo;
  Display, EstadoTxt: String;
begin
  if (FListBandeja = nil) or (FSistema = nil) then Exit;
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  FListBandeja.Items.BeginUpdate;
  try
    FListBandeja.Items.Clear;

    Correo := FCorreoManager.GetBandejaEntrada(Usuario); // también puedes usar FSistema.GetBandejaEntrada(Usuario)
    while Correo <> nil do
    begin
      if Correo^.Estado = 'NL' then EstadoTxt := '[NL]' else EstadoTxt := '[L ]';
      // Mostramos: [Estado] Asunto — Remitente (Fecha)
      Display := Format('%s %s — %s (%s)', [EstadoTxt, Correo^.Asunto, Correo^.Remitente, Correo^.Fecha]);

      // Guardamos el Id en Objects usando cast (PtrInt <-> TObject)
      FListBandeja.Items.AddObject(Display, TObject(PtrInt(Correo^.Id)));

      Correo := Correo^.Siguiente;
    end;
  finally
    FListBandeja.Items.EndUpdate;
  end;

  // Actualizar contador de no leídos
  if Assigned(FLabelNoLeidosInbox) then
    FLabelNoLeidosInbox.Caption := 'No leídos: ' + IntToStr(FCorreoManager.ContarCorreosNoLeidos(Usuario));

  // Limpiar mensaje mostrado
  if Assigned(FMemoMensaje) then
    FMemoMensaje.Clear;
end;

// Al seleccionar un correo: mostrar cuerpo y marcar leído si estaba NL
procedure TInterfazEDDMail.Inbox_OnSeleccion(Sender: TObject);
var
  Usuario: PUsuario;
  IdSel: Integer;
  Correo: PCorreo;
begin
  if (FListBandeja = nil) or (FMemoMensaje = nil) then Exit;
  if FListBandeja.ItemIndex < 0 then Exit;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  IdSel := Integer(PtrInt(FListBandeja.Items.Objects[FListBandeja.ItemIndex]));

  // Buscar el correo seleccionado para mostrar el mensaje
  Correo := FCorreoManager.GetBandejaEntrada(Usuario);
  while (Correo <> nil) and (Correo^.Id <> IdSel) do
    Correo := Correo^.Siguiente;

  if Correo <> nil then
  begin
    FMemoMensaje.Lines.Text := Correo^.Mensaje;

    // Si está NL, marcar como leído
    if Correo^.Estado = 'NL' then
    begin
      if FCorreoManager.MarcarCorreoLeido(Usuario, IdSel) then
        Inbox_RellenarLista;
    end;
  end;
end;

procedure TInterfazEDDMail.Inbox_OnOrdenarClick(Sender: TObject);
var
  Usuario: PUsuario;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  if FCorreoManager.OrdenarBandejaPorAsunto(Usuario) then
    Inbox_RellenarLista;
end;

procedure TInterfazEDDMail.Inbox_OnEliminarClick(Sender: TObject);
var
  Usuario: PUsuario;
  IdSel: Integer;
begin
  if (FListBandeja = nil) or (FListBandeja.ItemIndex < 0) then Exit;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  IdSel := Integer(PtrInt(FListBandeja.Items.Objects[FListBandeja.ItemIndex]));

  if FCorreoManager.EliminarCorreoDeBandeja(Usuario, IdSel) then
  begin
    MostrarMensaje('Éxito', 'Correo movido a Papelera.');
    Inbox_RellenarLista;
  end
  else
    MostrarMensaje('Error', 'No se pudo eliminar el correo.');
end;

procedure TInterfazEDDMail.Inbox_OnMarcarLeidoClick(Sender: TObject);
var
  Usuario: PUsuario;
  IdSel: Integer;
begin
  if (FListBandeja = nil) or (FListBandeja.ItemIndex < 0) then Exit;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  IdSel := Integer(PtrInt(FListBandeja.Items.Objects[FListBandeja.ItemIndex]));

  if FCorreoManager.MarcarCorreoLeido(Usuario, IdSel) then
    Inbox_RellenarLista
  else
    MostrarMensaje('Error', 'No se pudo marcar como leído.');
end;
procedure TInterfazEDDMail.OnPapeleraClick(Sender: TObject);
var
  Panel: TPanel;
  LabelTitulo: TLabel;
  BtnBuscar, BtnEliminarDef, BtnCerrar: TButton;
begin
  if FSistema.GetUsuarioActual = nil then Exit;

  if Assigned(FFormPapelera) then
  begin
    FFormPapelera.Show;
    FFormPapelera.BringToFront;
    Exit;
  end;

  FFormPapelera := TForm.Create(nil);
  with FFormPapelera do
  begin
    Caption := 'Papelera (Pila LIFO)';
    Width := 700;
    Height := 480;
    Position := poOwnerFormCenter;
    BorderStyle := bsSizeable;
    OnClose := @OnFormPapeleraClose;
    Color:=clMoneyGreen;
  end;

  Panel := TPanel.Create(FFormPapelera);
  with Panel do
  begin
    Parent := FFormPapelera;
    Align := alClient;
    BevelOuter := bvNone;
    BorderWidth := 10;
    Color:=clMoneyGreen;
  end;

  LabelTitulo := TLabel.Create(Panel);
  with LabelTitulo do
  begin
    Parent := Panel;
    Caption := 'Papelera (último en entrar, primero en salir) 🗑️';
    Font.Size := 14;
    Font.Style := [fsBold];
    Left := 10;
    Top := 10;
    Color:=clMoneyGreen ;
  end;

  // Buscar
  FEditBuscarPapelera := TEdit.Create(Panel);
  with FEditBuscarPapelera do
  begin
    Parent := Panel;
    Left := 10; Top := 40; Width := 400;
    Hint := 'Ingrese palabra clave para buscar en asunto';
    ShowHint := True;
  end;

  BtnBuscar := TButton.Create(Panel);
  with BtnBuscar do
  begin
    Parent := Panel;
    Caption := 'Buscar';
    Left := 420; Top := 38; Width := 80; Height := 26;
    OnClick := @Papelera_OnBuscarClick;
  end;

  // Lista de correos eliminados
  FListPapelera := TListBox.Create(Panel);
  with FListPapelera do
  begin
    Parent := Panel;
    Left := 10; Top := 100; Width := 660; Height := 150;
    OnClick := @Papelera_OnSeleccion;
    ItemHeight := 16;
  end;

  // Memo cuerpo
  FMemoPapelera := TMemo.Create(Panel);
  with FMemoPapelera do
  begin
    Parent := Panel;
    Left := 10; Top := 270; Width := 660; Height := 120;
    ReadOnly := True;
    ScrollBars := ssVertical;
  end;

  // Botones
  BtnEliminarDef := TButton.Create(Panel);
  with BtnEliminarDef do
  begin
    Parent := Panel;
    Caption := 'Eliminar Definitivamente';
    Left := 10; Top := 410; Width := 200; Height := 30;
    OnClick := @Papelera_OnEliminarDefClick;
  end;

  BtnCerrar := TButton.Create(Panel);
  with BtnCerrar do
  begin
    Parent := Panel;
    Caption := 'Cerrar';
    Left := 550; Top := 410; Width := 120; Height := 30;
          OnClick := @Papelera_OnCerrarClick;  // ✅ Correcto: cierra la PAPELERA



  end;

  // Llenar lista
  Papelera_RellenarLista;

  FFormPapelera.Show;
end;

procedure TInterfazEDDMail.OnFormPapeleraClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  FFormPapelera := nil;
  FListPapelera := nil;
  FMemoPapelera := nil;
  FEditBuscarPapelera := nil;
end;

procedure TInterfazEDDMail.Papelera_RellenarLista;
var
  Usuario: PUsuario;
  Correo: PCorreo;
  Display: String;
begin
  if (FListPapelera = nil) then Exit;
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  FListPapelera.Items.BeginUpdate;
  try
    FListPapelera.Items.Clear;
    Correo := FCorreoManager.GetPapelera(Usuario);
    while Correo <> nil do
    begin
      Display := Format('[ID %d] %s — %s (%s)',
        [Correo^.Id, Correo^.Asunto, Correo^.Remitente, Correo^.Fecha]);
      FListPapelera.Items.AddObject(Display, TObject(PtrInt(Correo^.Id)));
      Correo := Correo^.Siguiente;
    end;
  finally
    FListPapelera.Items.EndUpdate;
  end;

  if Assigned(FMemoPapelera) then
    FMemoPapelera.Clear;
end;

procedure TInterfazEDDMail.Papelera_OnSeleccion(Sender: TObject);
var
  Usuario: PUsuario;
  IdSel: Integer;
  Correo: PCorreo;
begin
  if (FListPapelera = nil) or (FMemoPapelera = nil) then Exit;
  if FListPapelera.ItemIndex < 0 then Exit;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  IdSel := Integer(PtrInt(FListPapelera.Items.Objects[FListPapelera.ItemIndex]));

  Correo := FCorreoManager.BuscarEnPapelera(Usuario, ''); // recorre todos
  while (Correo <> nil) and (Correo^.Id <> IdSel) do
    Correo := Correo^.Siguiente;

  if Correo <> nil then
    FMemoPapelera.Lines.Text := Correo^.Mensaje;
end;

procedure TInterfazEDDMail.Papelera_OnBuscarClick(Sender: TObject);
var
  Usuario: PUsuario;
  Correo: PCorreo;
  Palabra: String;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  Palabra := Trim(FEditBuscarPapelera.Text);
  if Palabra = '' then
  begin
    MostrarMensaje('Aviso', 'Ingrese una palabra clave');
    Exit;
  end;

  Correo := FCorreoManager.BuscarEnPapelera(Usuario, Palabra);
  if Correo <> nil then
  begin
    MostrarMensaje('Éxito', 'Correo encontrado: ' + Correo^.Asunto);
    FMemoPapelera.Lines.Text := Correo^.Mensaje;
  end
  else
    MostrarMensaje('Aviso', 'No se encontró ningún correo con "'+Palabra+'"');
end;

procedure TInterfazEDDMail.Papelera_OnEliminarDefClick(Sender: TObject);
var
  Usuario: PUsuario;
  IdSel: Integer;
begin
  if (FListPapelera = nil) or (FListPapelera.ItemIndex < 0) then Exit;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  // El Id lo guardaste en Objects de la ListBox
  IdSel := Integer(PtrInt(FListPapelera.Items.Objects[FListPapelera.ItemIndex]));

  if FCorreoManager.EliminarCorreoDePapelera(Usuario, IdSel) then
  begin
    Papelera_RellenarLista; // refrescar UI
    FMemoPapelera.Clear;
    MostrarMensaje('Éxito', 'Correo eliminado definitivamente.');
  end
  else
    MostrarMensaje('Error', 'No se pudo eliminar definitivamente.');
end;

procedure TInterfazEDDMail.Papelera_OnCerrarClick(Sender: TObject);
begin
  if Assigned(FFormPapelera) then
    FFormPapelera.Close;
end;
// Agregar este procedimiento en InterfazGTK.pas después de OnEnviarCorreoClick

procedure TInterfazEDDMail.OnProgramarCorreoClick(Sender: TObject);
var
  FormProgramar: TForm;
  Panel: TPanel;
  LabelPara, LabelAsunto, LabelFecha: TLabel;
  EditPara, EditAsunto, EditFecha: TEdit;
  MemoCuerpo: TMemo;
  BtnProgramar, BtnCancelar: TButton;
  Usuario: PUsuario;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  FormProgramar := TForm.Create(nil);
  try
    with FormProgramar do
    begin
      Caption := 'Programar Correo';
      Width := 600;
      Height := 500;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color:=clMoneyGreen ;
    end;

    Panel := TPanel.Create(FormProgramar);
    with Panel do
    begin
      Parent := FormProgramar;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 12;
      Color:=clMoneyGreen ;
    end;

    // Para
    LabelPara := TLabel.Create(Panel);
    with LabelPara do
    begin
      Parent := Panel;
      Caption := 'Para:';
      Left := 12;
      Top := 12;
      Font.Style := [fsBold];
    end;

    EditPara := TEdit.Create(Panel);
    with EditPara do
    begin
      Parent := Panel;
      Left := 12;
      Top := 30;
      Width := 560;
      TabOrder := 0;
    end;

    // Asunto
    LabelAsunto := TLabel.Create(Panel);
    with LabelAsunto do
    begin
      Parent := Panel;
      Caption := 'Asunto:';
      Left := 12;
      Top := 65;
      Font.Style := [fsBold];
    end;

    EditAsunto := TEdit.Create(Panel);
    with EditAsunto do
    begin
      Parent := Panel;
      Left := 12;
      Top := 80;
      Width := 560;
      TabOrder := 1;
    end;

    // Fecha y hora de envío
    LabelFecha := TLabel.Create(Panel);
    with LabelFecha do
    begin
      Parent := Panel;
      Caption := 'Fecha y hora de envío (dd/mm/yy hh:nn):';
      Left := 12;
      Top := 120;
      Font.Style := [fsBold];
    end;

    EditFecha := TEdit.Create(Panel);
    with EditFecha do
    begin
      Parent := Panel;
      Left := 12;
      Top := 140;
      Width := 560;
      TabOrder := 2;
      Hint := 'Formato: 25/08/25 14:30';
      ShowHint := True;
      // Sugerir fecha futura como ejemplo
      Text := FormatDateTime('dd/mm/yy hh:nn', Now + 1); // Mañana a la misma hora
    end;

    // Mensaje
    MemoCuerpo := TMemo.Create(Panel);
    with MemoCuerpo do
    begin
      Parent := Panel;
      Left := 12;
      Top := 180;
      Width := 560;
      Height := 240;
      ScrollBars := ssVertical;
      TabOrder := 3;
    end;

    // Botones
    BtnProgramar := TButton.Create(Panel);
    with BtnProgramar do
    begin
      Parent := Panel;
      Caption := 'Programar';
      Left := 380;
      Top := 420;
      Width := 90;
      Height := 30;
      ModalResult := mrOk;
      Default := True;
    end;

    BtnCancelar := TButton.Create(Panel);
    with BtnCancelar do
    begin
      Parent := Panel;
      Caption := 'Cancelar';
      Left := 480;
      Top := 420;
      Width := 90;
      Height := 30;
      ModalResult := mrCancel;
      Cancel := True;
    end;

    if FormProgramar.ShowModal = mrOk then
    begin
      if (Trim(EditPara.Text) = '') or (Trim(EditAsunto.Text) = '') or (Trim(EditFecha.Text) = '') then
      begin
        MostrarMensaje('Error', 'Debe completar todos los campos obligatorios');
        Exit;
      end;

      // Validar formato básico de fecha
      try
        StrToDateTime(Trim(EditFecha.Text));
      except
        MostrarMensaje('Error', 'Formato de fecha inválido. Use: dd/mm/yy hh:nn');
        Exit;
      end;

      // Programar el correo usando CorreoManager
      if FCorreoManager.ProgramarCorreo(
            FSistema,
            Usuario^.Email,
            Trim(EditPara.Text),
            Trim(EditAsunto.Text),
            MemoCuerpo.Lines.Text,
            Trim(EditFecha.Text)) then
      begin
        MostrarMensaje('Éxito',
          'Correo programado exitosamente' + LineEnding +
          'Para: ' + Trim(EditPara.Text) + LineEnding +
          'Fecha de envío: ' + Trim(EditFecha.Text));
      end
      else
      begin
        MostrarMensaje('Error',
          'No se pudo programar el correo.' + LineEnding +
          'Verifique que el destinatario exista y esté en sus contactos.');
      end;
    end;

  finally
    FormProgramar.Free;
  end;
end;
// IMPLEMENTACIÓN DE LOS PROCEDIMIENTOS:

procedure TInterfazEDDMail.OnCorreosProgramadosClick(Sender: TObject);
var
  Panel: TPanel;
  LabelTitulo: TLabel;
  BtnEnviar, BtnEliminar, BtnCerrar: TButton;
begin
  if FSistema.GetUsuarioActual = nil then Exit;

  // Si ya existe la ventana, solo mostrarla
  if Assigned(FFormCorreosProgramados) then
  begin
    FFormCorreosProgramados.Show;
    FFormCorreosProgramados.BringToFront;
    Exit;
  end;

  FFormCorreosProgramados := TForm.Create(nil);
  with FFormCorreosProgramados do
  begin
    Caption := 'Correos Programados (Cola FIFO)';
    Width := 700;
    Height := 480;
    Position := poOwnerFormCenter;
    BorderStyle := bsSizeable;
    OnClose := @OnFormCorreosProgramadosClose;
    Color:=clMoneyGreen;
  end;

  Panel := TPanel.Create(FFormCorreosProgramados);
  with Panel do
  begin
    Parent := FFormCorreosProgramados;
    Align := alClient;
    BevelOuter := bvNone;
    BorderWidth := 10;
    Color:=clMoneyGreen;
  end;

  LabelTitulo := TLabel.Create(Panel);
  with LabelTitulo do
  begin
    Parent := Panel;
    Caption := 'Correos Programados para Envío ';
    Font.Size := 14;
    Font.Style := [fsBold];
    Left := 10;
    Top := 10;
  end;

  FLabelTotalProgramados := TLabel.Create(Panel);
  with FLabelTotalProgramados do
  begin
    Parent := Panel;
    Caption := 'Total: 0';
    Left := 300;
    Top := 14;
    Font.Color := clGray;
  end;

  // Lista de correos programados
  FListCorreosProgramados := TListBox.Create(Panel);
  with FListCorreosProgramados do
  begin
    Parent := Panel;
    Left := 10;
    Top := 40;
    Width := 660;
    Height := 240;
    OnClick := @CorreosProgramados_OnSeleccion;
    ItemHeight := 16;
  end;

  // Memo para mostrar el mensaje del correo seleccionado
  FMemoCorreoProgramado := TMemo.Create(Panel);
  with FMemoCorreoProgramado do
  begin
    Parent := Panel;
    Left := 10;
    Top := 290;
    Width := 660;
    Height := 120;
    ReadOnly := True;
    ScrollBars := ssVertical;
  end;

  // Botones
  BtnEnviar := TButton.Create(Panel);
  with BtnEnviar do
  begin
    Parent := Panel;
    Caption := 'Enviar Programados';
    Left := 10;
    Top := 420;
    Width := 150;
    Height := 30;
    OnClick := @CorreosProgramados_OnEnviarClick;
    Hint := 'Envía todos los correos cuya fecha ya llegó';
    ShowHint := True;
    Font.Style := [fsBold];
    Color := clTeal;
  end;

  BtnEliminar := TButton.Create(Panel);
  with BtnEliminar do
  begin
    Parent := Panel;
    Caption := 'Eliminar Seleccionado';
    Left := 170;
    Top := 420;
    Width := 150;
    Height := 30;
    OnClick := @CorreosProgramados_OnEliminarClick;
    Hint := 'Elimina el correo programado seleccionado';
    ShowHint := True;
  end;

  BtnCerrar := TButton.Create(Panel);
  with BtnCerrar do
  begin
    Parent := Panel;
    Caption := 'Cerrar';
    Left := 520;
    Top := 420;
    Width := 150;
    Height := 30;
    OnClick := @CorreosProgramados_OnCerrarClick;
    Cancel := True;
  end;

  // Cargar datos
  CorreosProgramados_RellenarLista;

  FFormCorreosProgramados.Show;
end;
procedure TInterfazEDDMail.OnFormCorreosProgramadosClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  FFormCorreosProgramados := nil;
  FListCorreosProgramados := nil;
  FMemoCorreoProgramado := nil;
  FLabelTotalProgramados := nil;
end;

procedure TInterfazEDDMail.CorreosProgramados_RellenarLista;
var
  Usuario: PUsuario;
  Correo: PCorreo;
  Display: String;
  Contador: Integer;
begin
  if (FListCorreosProgramados = nil) then Exit;
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  FListCorreosProgramados.Items.BeginUpdate;
  try
    FListCorreosProgramados.Items.Clear;
    Contador := 0;

    Correo := FCorreoManager.GetCorreosProgramados(Usuario);
    while Correo <> nil do
    begin
      Display := Format('[Para: %s] %s — Envío: %s',
        [Correo^.Destinatario, Correo^.Asunto, Correo^.FechaEnvio]);

      FListCorreosProgramados.Items.AddObject(Display, TObject(PtrInt(Correo^.Id)));
      Inc(Contador);
      Correo := Correo^.Siguiente;
    end;
  finally
    FListCorreosProgramados.Items.EndUpdate;
  end;

  // Actualizar contador
  if Assigned(FLabelTotalProgramados) then
    FLabelTotalProgramados.Caption := Format('Total: %d', [Contador]);

  // Limpiar memo
  if Assigned(FMemoCorreoProgramado) then
    FMemoCorreoProgramado.Clear;
end;

procedure TInterfazEDDMail.CorreosProgramados_OnSeleccion(Sender: TObject);
var
  Usuario: PUsuario;
  IdSel: Integer;
  Correo: PCorreo;
begin
  if (FListCorreosProgramados = nil) or (FMemoCorreoProgramado = nil) then Exit;
  if FListCorreosProgramados.ItemIndex < 0 then Exit;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  IdSel := Integer(PtrInt(FListCorreosProgramados.Items.Objects[FListCorreosProgramados.ItemIndex]));

  // Buscar el correo seleccionado
  Correo := FCorreoManager.GetCorreosProgramados(Usuario);
  while (Correo <> nil) and (Correo^.Id <> IdSel) do
    Correo := Correo^.Siguiente;

  if Correo <> nil then
  begin
    FMemoCorreoProgramado.Lines.Clear;
    FMemoCorreoProgramado.Lines.Add('Para: ' + Correo^.Destinatario);
    FMemoCorreoProgramado.Lines.Add('Asunto: ' + Correo^.Asunto);
    FMemoCorreoProgramado.Lines.Add('Fecha de envío: ' + Correo^.FechaEnvio);
    FMemoCorreoProgramado.Lines.Add('');
    FMemoCorreoProgramado.Lines.Add('Mensaje:');
    FMemoCorreoProgramado.Lines.Add(Correo^.Mensaje);
  end;
end;

procedure TInterfazEDDMail.CorreosProgramados_OnEnviarClick(Sender: TObject);
var
  Usuario: PUsuario;
  CorreosEnviados: Integer;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  // Procesar correos programados usando CorreoManager
  CorreosEnviados := FCorreoManager.ProcesarCorreosProgramados(FSistema, Usuario);

  if CorreosEnviados > 0 then
  begin
    MostrarMensaje('Éxito',
      Format('Se enviaron %d correos programados exitosamente.', [CorreosEnviados]));
    CorreosProgramados_RellenarLista; // Refrescar la lista
  end
  else
  begin
    MostrarMensaje('Información',
      'No hay correos programados listos para enviar.' + LineEnding +
      'Verifique que las fechas de envío ya hayan llegado.');
  end;
end;

procedure TInterfazEDDMail.CorreosProgramados_OnEliminarClick(Sender: TObject);
var
  Usuario: PUsuario;
  IdSel: Integer;
  Correo, CorreoAnterior: PCorreo;
  Encontrado: Boolean;
begin
  if (FListCorreosProgramados = nil) or (FListCorreosProgramados.ItemIndex < 0) then Exit;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  IdSel := Integer(PtrInt(FListCorreosProgramados.Items.Objects[FListCorreosProgramados.ItemIndex]));

  // Buscar y eliminar el correo de la cola
  Correo := Usuario^.CorreosProgramados;
  CorreoAnterior := nil;
  Encontrado := False;

  while (Correo <> nil) and not Encontrado do
  begin
    if Correo^.Id = IdSel then
    begin
      // Desenlazar de la cola FIFO
      if CorreoAnterior = nil then
        Usuario^.CorreosProgramados := Correo^.Siguiente
      else
        CorreoAnterior^.Siguiente := Correo^.Siguiente;

      // Si había enlace hacia atrás, ajustarlo
      if Correo^.Siguiente <> nil then
        Correo^.Siguiente^.Anterior := CorreoAnterior;

      Dispose(Correo);
      Encontrado := True;
      MostrarMensaje('Éxito', 'Correo programado eliminado.');
    end
    else
    begin
      CorreoAnterior := Correo;
      Correo := Correo^.Siguiente;
    end;
  end;

  if Encontrado then
  begin
    CorreosProgramados_RellenarLista; // Refrescar lista
    FMemoCorreoProgramado.Clear;
  end
  else
    MostrarMensaje('Error', 'No se pudo eliminar el correo programado.');
end;

procedure TInterfazEDDMail.CorreosProgramados_OnCerrarClick(Sender: TObject);
begin
  if Assigned(FFormCorreosProgramados) then
    FFormCorreosProgramados.Close;
end;
// Actualizar OnGenerarReportesClick para incluir nuevos reportes
procedure TInterfazEDDMail.OnGenerarReportesClick(Sender: TObject);
var
  FormReportes: TForm;
  Panel: TPanel;
  LabelTitulo, LabelInfo: TLabel;
  BtnCorreosRecibidos, BtnPapelera, BtnCorreosProgramados,
  BtnContactos, BtnFavoritos, BtnBorradores, BtnCerrar: TButton;
  Usuario: PUsuario;
  CarpetaReportes: String;
  YPos: Integer;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  CarpetaReportes := Usuario^.Usuario + '-Reportes';

  FormReportes := TForm.Create(nil);
  try
    with FormReportes do
    begin
      Caption := '📊 Generar Reportes';
      Width := 500;
      Height := 550; // ← Aumentar altura para nuevos botones
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color := clMoneyGreen;
    end;

    Panel := TPanel.Create(FormReportes);
    with Panel do
    begin
      Parent := FormReportes;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 15;
      Color := clMoneyGreen;
    end;

    YPos := 20;

    LabelTitulo := TLabel.Create(Panel);
    with LabelTitulo do
    begin
      Parent := Panel;
      Caption := 'Generar Reportes Personales 📊';
      Font.Size := 14;
      Font.Style := [fsBold];
      Left := 20;
      Top := YPos;
    end;
    Inc(YPos, 40);

    LabelInfo := TLabel.Create(Panel);
    with LabelInfo do
    begin
      Parent := Panel;
      Caption := 'Usuario: ' + Usuario^.Nombre + LineEnding +
                 'Carpeta: ' + CarpetaReportes + LineEnding +
                 'Seleccione el reporte que desea generar:';
      Left := 20;
      Top := YPos;
      AutoSize := True;
      Font.Color := clGray;
    end;
    Inc(YPos, 80);

    // Reportes existentes (Fase 1)
    BtnCorreosRecibidos := TButton.Create(Panel);
    with BtnCorreosRecibidos do
    begin
      Parent := Panel;
      Caption := 'Reporte de Correos Recibidos';
      Left := 20; Top := YPos; Width := 430; Height := 35;
      Hint := 'Lista doblemente enlazada - Bandeja de entrada';
      ShowHint := True;
      OnClick := @OnReporteCorreosRecibidosClick;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 45);

    BtnPapelera := TButton.Create(Panel);
    with BtnPapelera do
    begin
      Parent := Panel;
      Caption := 'Reporte de Papelera';
      Left := 20; Top := YPos; Width := 430; Height := 35;
      Hint := 'Pila LIFO - Correos eliminados';
      ShowHint := True;
      OnClick := @OnReportePapeleraClick;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 45);

    BtnCorreosProgramados := TButton.Create(Panel);
    with BtnCorreosProgramados do
    begin
      Parent := Panel;
      Caption := 'Reporte de Correos Programados';
      Left := 20; Top := YPos; Width := 430; Height := 35;
      Hint := 'Cola FIFO - Correos pendientes de envío';
      ShowHint := True;
      OnClick := @OnReporteCorreosProgramadosClick;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 45);

    BtnContactos := TButton.Create(Panel);
    with BtnContactos do
    begin
      Parent := Panel;
      Caption := 'Reporte de Contactos';
      Left := 20; Top := YPos; Width := 430; Height := 35;
      Hint := 'Lista circular - Contactos del usuario';
      ShowHint := True;
      OnClick := @OnGenerarReporteContactosClick;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 45);

    // NUEVOS REPORTES (Fase 2)
    BtnFavoritos := TButton.Create(Panel);
    with BtnFavoritos do
    begin
      Parent := Panel;
      Caption := '⭐ Reporte de Favoritos (Árbol B)';
      Left := 20; Top := YPos; Width := 430; Height := 35;
      Hint := 'Árbol B orden 5 - Correos favoritos';
      ShowHint := True;
      OnClick := @OnReporteFavoritosClick; // ← Nuevo event handler
      Font.Style := [fsBold];
      Color := clYellow;
    end;
    Inc(YPos, 45);

    BtnBorradores := TButton.Create(Panel);
    with BtnBorradores do
    begin
      Parent := Panel;
      Caption := '📝 Reporte de Borradores (Árbol AVL)';
      Left := 20; Top := YPos; Width := 430; Height := 35;
      Hint := 'Árbol AVL - Borradores de correos';
      ShowHint := True;
      OnClick := @OnReporteBorradoresClick; // ← Nuevo event handler
      Font.Style := [fsBold];
      Color := clAqua;
    end;
    Inc(YPos, 70);

    BtnCerrar := TButton.Create(Panel);
    with BtnCerrar do
    begin
      Parent := Panel;
      Caption := 'Cerrar';
      Left := 350; Top := YPos; Width := 100; Height := 30;
      ModalResult := mrCancel;
    end;

    FormReportes.ShowModal;

  finally
    FormReportes.Free;
  end;
end;

// Nuevos event handlers para reportes Fase 2
procedure TInterfazEDDMail.OnReporteFavoritosClick(Sender: TObject);
var
  Usuario: PUsuario;
  CarpetaReportes: String;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  CarpetaReportes := Usuario^.Usuario + '-Reportes';

  try
    FSistema.GenerarReporteFavoritos(Usuario, CarpetaReportes);
    MostrarMensaje('Éxito',
      'Reporte de favoritos generado en: ' + CarpetaReportes + LineEnding +
      'Archivos generados:' + LineEnding +
      '- favoritos_' + Usuario^.Usuario + '.dot (código Graphviz)' + LineEnding +
      '- favoritos_' + Usuario^.Usuario + '.png (imagen)');
  except
    on E: Exception do
      MostrarMensaje('Error', 'Error al generar reporte: ' + E.Message);
  end;
end;

procedure TInterfazEDDMail.OnReporteBorradoresClick(Sender: TObject);
var
  Usuario: PUsuario;
  CarpetaReportes: String;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  CarpetaReportes := Usuario^.Usuario + '-Reportes';

  try
    FSistema.GenerarReporteBorradores(Usuario, CarpetaReportes);
    MostrarMensaje('Éxito',
      'Reporte de borradores generado en: ' + CarpetaReportes + LineEnding +
      'Archivos generados:' + LineEnding +
      '- borradores_' + Usuario^.Usuario + '.dot (código Graphviz)' + LineEnding +
      '- borradores_' + Usuario^.Usuario + '.png (imagen)');
  except
    on E: Exception do
      MostrarMensaje('Error', 'Error al generar reporte: ' + E.Message);
  end;
end;
// Implementar los event handlers para cada reporte
procedure TInterfazEDDMail.OnReporteCorreosRecibidosClick(Sender: TObject);
var
  Usuario: PUsuario;
  CarpetaReportes: String;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  CarpetaReportes := Usuario^.Usuario + '-Reportes';

  try
    FCorreoManager.GenerarReporteCorreosRecibidos(Usuario, CarpetaReportes);
    MostrarMensaje('Éxito',
      'Reporte de correos recibidos generado en: ' + CarpetaReportes + LineEnding +
      'Archivos generados:' + LineEnding +
      '- correos_recibidos_' + Usuario^.Usuario + '.dot (código Graphviz)' + LineEnding +
      '- correos_recibidos_' + Usuario^.Usuario + '.png (imagen)');
  except
    on E: Exception do
      MostrarMensaje('Error', 'Error al generar reporte: ' + E.Message);
  end;
end;

procedure TInterfazEDDMail.OnReportePapeleraClick(Sender: TObject);
var
  Usuario: PUsuario;
  CarpetaReportes: String;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  CarpetaReportes := Usuario^.Usuario + '-Reportes';

  try
    FCorreoManager.GenerarReportePapelera(Usuario, CarpetaReportes);
    MostrarMensaje('Éxito',
      'Reporte de papelera generado en: ' + CarpetaReportes + LineEnding +
      'Archivos generados:' + LineEnding +
      '- papelera_' + Usuario^.Usuario + '.dot (código Graphviz)' + LineEnding +
      '- papelera_' + Usuario^.Usuario + '.png (imagen)');
  except
    on E: Exception do
      MostrarMensaje('Error', 'Error al generar reporte: ' + E.Message);
  end;
end;

procedure TInterfazEDDMail.OnReporteCorreosProgramadosClick(Sender: TObject);
var
  Usuario: PUsuario;
  CarpetaReportes: String;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  CarpetaReportes := Usuario^.Usuario + '-Reportes';

  try
    FCorreoManager.GenerarReporteCorreosProgramados(Usuario, CarpetaReportes);
    MostrarMensaje('Éxito',
      'Reporte de correos programados generado en: ' + CarpetaReportes + LineEnding +
      'Archivos generados:' + LineEnding +
      '- correos_programados_' + Usuario^.Usuario + '.dot (código Graphviz)' + LineEnding +
      '- correos_programados_' + Usuario^.Usuario + '.png (imagen)');
  except
    on E: Exception do
      MostrarMensaje('Error', 'Error al generar reporte: ' + E.Message);
  end;
end;

// Implementar ventana de Favoritos
procedure TInterfazEDDMail.OnVerFavoritosClick(Sender: TObject);
var
  Panel: TPanel;
  LabelTitulo: TLabel;
  BtnEliminar, BtnCerrar: TButton;
begin
  if FSistema.GetUsuarioActual = nil then Exit;

  if Assigned(FFormFavoritos) then
  begin
    FFormFavoritos.Show;
    FFormFavoritos.BringToFront;
    Exit;
  end;

  FFormFavoritos := TForm.Create(nil);
  with FFormFavoritos do
  begin
    Caption := 'Correos Favoritos (Árbol B)';
    Width := 700;
    Height := 480;
    Position := poOwnerFormCenter;
    BorderStyle := bsSizeable;
    OnClose := @OnFormFavoritosClose;
    Color := clMoneyGreen;
  end;

  Panel := TPanel.Create(FFormFavoritos);
  with Panel do
  begin
    Parent := FFormFavoritos;
    Align := alClient;
    BevelOuter := bvNone;
    BorderWidth := 10;
    Color := clMoneyGreen;
  end;

  LabelTitulo := TLabel.Create(Panel);
  with LabelTitulo do
  begin
    Parent := Panel;
    Caption := 'Correos Favoritos ⭐';
    Font.Size := 14;
    Font.Style := [fsBold];
    Left := 10;
    Top := 10;
  end;

  FLabelTotalFavoritos := TLabel.Create(Panel);
  with FLabelTotalFavoritos do
  begin
    Parent := Panel;
    Caption := 'Total: 0';
    Left := 200;
    Top := 14;
    Font.Color := clGray;
  end;

  // Lista de favoritos
  FListFavoritos := TListBox.Create(Panel);
  with FListFavoritos do
  begin
    Parent := Panel;
    Left := 10;
    Top := 40;
    Width := 660;
    Height := 200;
    OnClick := @Favoritos_OnSeleccion;
    ItemHeight := 16;
  end;

  // Memo detalles
  FMemoFavorito := TMemo.Create(Panel);
  with FMemoFavorito do
  begin
    Parent := Panel;
    Left := 10;
    Top := 250;
    Width := 660;
    Height := 150;
    ReadOnly := True;
    ScrollBars := ssVertical;
  end;

  // Botones
  BtnEliminar := TButton.Create(Panel);
  with BtnEliminar do
  begin
    Parent := Panel;
    Caption := 'Eliminar de Favoritos';
    Left := 10;
    Top := 410;
    Width := 150;
    Height := 30;
    OnClick := @Favoritos_OnEliminarClick;
  end;

  BtnCerrar := TButton.Create(Panel);
  with BtnCerrar do
  begin
    Parent := Panel;
    Caption := 'Cerrar';
    Left := 520;
    Top := 410;
    Width := 150;
    Height := 30;
    OnClick := @OnFormFavoritosClose;
  end;

  // Cargar favoritos
  Favoritos_RellenarLista;
  FFormFavoritos.Show;
end;

procedure TInterfazEDDMail.OnFormFavoritosClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  FFormFavoritos := nil;
  FListFavoritos := nil;
  FMemoFavorito := nil;
  FLabelTotalFavoritos := nil;
end;

procedure TInterfazEDDMail.Favoritos_OnSeleccion(Sender: TObject);
var
  CorreoId: Integer;
  Usuario: PUsuario;
  Correo: PCorreo;
begin
  if (FListFavoritos = nil) or (FMemoFavorito = nil) then Exit;
  if FListFavoritos.ItemIndex < 0 then Exit;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  CorreoId := Integer(PtrInt(FListFavoritos.Items.Objects[FListFavoritos.ItemIndex]));

  // Buscar en árbol B de favoritos
  Correo := FSistema.BuscarB(Usuario^.ArbolFavoritos, CorreoId);
  if Correo <> nil then
  begin
    FMemoFavorito.Lines.Clear;
    FMemoFavorito.Lines.Add('Remitente: ' + Correo^.Remitente);
    FMemoFavorito.Lines.Add('Asunto: ' + Correo^.Asunto);
    FMemoFavorito.Lines.Add('Fecha: ' + Correo^.Fecha);
    FMemoFavorito.Lines.Add('');
    FMemoFavorito.Lines.Add('Mensaje:');
    FMemoFavorito.Lines.Add(Correo^.Mensaje);
  end;
end;

procedure TInterfazEDDMail.Favoritos_OnEliminarClick(Sender: TObject);
var
  CorreoId: Integer;
  Usuario: PUsuario;
begin
  if (FListFavoritos = nil) or (FListFavoritos.ItemIndex < 0) then Exit;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  CorreoId := Integer(PtrInt(FListFavoritos.Items.Objects[FListFavoritos.ItemIndex]));

  if FSistema.EliminarFavorito(Usuario, CorreoId) then
  begin
    MostrarMensaje('Éxito', 'Correo eliminado de favoritos');
    Favoritos_RellenarLista;
    FMemoFavorito.Clear;
  end
  else
    MostrarMensaje('Error', 'No se pudo eliminar de favoritos');
end;
// Implementar ventana de Borradores
procedure TInterfazEDDMail.OnVerBorradoresClick(Sender: TObject);
var
  Panel: TPanel;
  LabelTitulo, LabelRecorrido: TLabel;
  BtnEditar, BtnEliminar, BtnCerrar: TButton;
begin
  if FSistema.GetUsuarioActual = nil then Exit;

  if Assigned(FFormBorradores) then
  begin
    FFormBorradores.Show;
    FFormBorradores.BringToFront;
    Exit;
  end;

  FFormBorradores := TForm.Create(nil);
  with FFormBorradores do
  begin
    Caption := 'Borradores (Árbol AVL)';
    Width := 700;
    Height := 500;
    Position := poOwnerFormCenter;
    BorderStyle := bsSizeable;
    OnClose := @OnFormBorradoresClose;
    Color := clMoneyGreen;
  end;

  Panel := TPanel.Create(FFormBorradores);
  with Panel do
  begin
    Parent := FFormBorradores;
    Align := alClient;
    BevelOuter := bvNone;
    BorderWidth := 10;
    Color := clMoneyGreen;
  end;

  LabelTitulo := TLabel.Create(Panel);
  with LabelTitulo do
  begin
    Parent := Panel;
    Caption := 'Borradores de Correos 📝';
    Font.Size := 14;
    Font.Style := [fsBold];
    Left := 10;
    Top := 10;
  end;

  // Selector de tipo de recorrido
  LabelRecorrido := TLabel.Create(Panel);
  with LabelRecorrido do
  begin
    Parent := Panel;
    Caption := 'Tipo de recorrido:';
    Left := 10;
    Top := 45;
    Font.Style := [fsBold];
  end;

  FComboRecorrido := TComboBox.Create(Panel);
  with FComboRecorrido do
  begin
    Parent := Panel;
    Left := 130;
    Top := 42;
    Width := 150;
    Style := csDropDownList;
    Items.Add('In-Orden');
    Items.Add('Pre-Orden');
    Items.Add('Post-Orden');
    ItemIndex := 0;
    OnChange := @Borradores_OnRecorridoChange;
  end;

  // Lista de borradores
  FListBorradores := TListBox.Create(Panel);
  with FListBorradores do
  begin
    Parent := Panel;
    Left := 10;
    Top := 75;
    Width := 660;
    Height := 200;
    OnClick := @Borradores_OnSeleccion;
    ItemHeight := 16;
  end;

  // Memo detalles
  FMemoBorrador := TMemo.Create(Panel);
  with FMemoBorrador do
  begin
    Parent := Panel;
    Left := 10;
    Top := 285;
    Width := 660;
    Height := 150;
    ReadOnly := True;
    ScrollBars := ssVertical;
  end;

  // Botones
  BtnEditar := TButton.Create(Panel);
  with BtnEditar do
  begin
    Parent := Panel;
    Caption := 'Editar y Enviar';
    Left := 10;
    Top := 445;
    Width := 120;
    Height := 30;
    OnClick := @Borradores_OnEditarClick;
  end;

  BtnEliminar := TButton.Create(Panel);
  with BtnEliminar do
  begin
    Parent := Panel;
    Caption := 'Eliminar';
    Left := 140;
    Top := 445;
    Width := 100;
    Height := 30;
    OnClick := @Borradores_OnEliminarClick;
  end;

  BtnCerrar := TButton.Create(Panel);
  with BtnCerrar do
  begin
    Parent := Panel;
    Caption := 'Cerrar';
    Left := 520;
    Top := 445;
    Width := 150;
    Height := 30;
    OnClick := @OnFormBorradoresClose;
  end;

  // Cargar borradores
  Borradores_RellenarLista;
  FFormBorradores.Show;
end;

procedure TInterfazEDDMail.OnFormBorradoresClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  FFormBorradores := nil;
  FListBorradores := nil;
  FMemoBorrador := nil;
  FComboRecorrido := nil;
end;

procedure TInterfazEDDMail.Borradores_OnRecorridoChange(Sender: TObject);
begin
  Borradores_RellenarLista;
end;

procedure TInterfazEDDMail.Borradores_RellenarLista;
var
  Usuario: PUsuario;
  ListaBorradores: TStringList;
  TipoRecorrido: String;
  i: Integer;
begin
  if FListBorradores = nil then Exit;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  case FComboRecorrido.ItemIndex of
    0: TipoRecorrido := 'InOrden';
    1: TipoRecorrido := 'PreOrden';
    2: TipoRecorrido := 'PostOrden';
    else TipoRecorrido := 'InOrden';
  end;

  ListaBorradores := FSistema.ObtenerBorradores(Usuario, TipoRecorrido);
  try
    FListBorradores.Items.Clear;
    for i := 0 to ListaBorradores.Count - 1 do
    begin
      FListBorradores.Items.Add(ListaBorradores[i]);
    end;
  finally
    ListaBorradores.Free;
  end;
end;

procedure TInterfazEDDMail.Borradores_OnSeleccion(Sender: TObject);
begin
  // Implementar selección de borrador
  // Similar a otras ventanas, mostrar detalles en el memo
end;

procedure TInterfazEDDMail.Borradores_OnEditarClick(Sender: TObject);
begin
  // Abrir ventana de edición de borrador
  MostrarMensaje('Info', 'Funcionalidad de editar borrador por implementar');
end;

procedure TInterfazEDDMail.Borradores_OnEliminarClick(Sender: TObject);
begin
  // Eliminar borrador del árbol AVL
  MostrarMensaje('Info', 'Funcionalidad de eliminar borrador por implementar');
end;
procedure TInterfazEDDMail.Inbox_OnMarcarFavoritoClick(Sender: TObject);
var
  Usuario: PUsuario;
  IdSel: Integer;
  CorreoSeleccionado: PCorreo;
begin
  // Validar que hay una lista y algo seleccionado
  if (FListBandeja = nil) or (FListBandeja.ItemIndex < 0) then
  begin
    MostrarMensaje('Aviso', 'Debe seleccionar un correo de la lista para marcarlo como favorito');
    Exit;
  end;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then
  begin
    MostrarMensaje('Error', 'No hay usuario activo');
    Exit;
  end;

  // Obtener el ID del correo seleccionado
  IdSel := Integer(PtrInt(FListBandeja.Items.Objects[FListBandeja.ItemIndex]));

  // Buscar el correo en la bandeja para validar que existe
  CorreoSeleccionado := FCorreoManager.GetBandejaEntrada(Usuario);
  while (CorreoSeleccionado <> nil) and (CorreoSeleccionado^.Id <> IdSel) do
    CorreoSeleccionado := CorreoSeleccionado^.Siguiente;

  if CorreoSeleccionado = nil then
  begin
    MostrarMensaje('Error', 'Correo no encontrado en la bandeja');
    Exit;
  end;

  // Intentar marcar como favorito usando el sistema
  if FSistema.MarcarComoFavorito(Usuario, IdSel) then
  begin
    MostrarMensaje('Éxito',
      '⭐ Correo marcado como favorito' + LineEnding +
      'Asunto: ' + CorreoSeleccionado^.Asunto + LineEnding +
      'Puede verlo en "⭐ Ver Favoritos"');
  end
  else
  begin
    MostrarMensaje('Error',
      'No se pudo marcar como favorito' + LineEnding +
      'Es posible que el correo ya esté en favoritos');
  end;
end;
// Implementar funcionalidad de Publicar en Comunidad
procedure TInterfazEDDMail.OnPublicarComunidadClick(Sender: TObject);
var
  FormPublicar: TForm;
  Panel: TPanel;
  LabelTitulo, LabelComunidad, LabelMensaje: TLabel;
  EditComunidad: TEdit;
  MemoMensaje: TMemo;
  BtnPublicar, BtnCancelar: TButton;
  Usuario: PUsuario;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  FormPublicar := TForm.Create(nil);
  try
    with FormPublicar do
    begin
      Caption := 'Publicar Mensaje en Comunidad';
      Width := 500;
      Height := 400;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color := clMoneyGreen;
    end;

    Panel := TPanel.Create(FormPublicar);
    with Panel do
    begin
      Parent := FormPublicar;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 15;
      Color := clMoneyGreen;
    end;

    LabelTitulo := TLabel.Create(Panel);
    with LabelTitulo do
    begin
      Parent := Panel;
      Caption := 'Publicar en Comunidad 📢';
      Font.Size := 14;
      Font.Style := [fsBold];
      Left := 20;
      Top := 20;
    end;

    LabelComunidad := TLabel.Create(Panel);
    with LabelComunidad do
    begin
      Parent := Panel;
      Caption := 'Nombre de la Comunidad:';
      Left := 20;
      Top := 60;
      Font.Style := [fsBold];
    end;

    EditComunidad := TEdit.Create(Panel);
    with EditComunidad do
    begin
      Parent := Panel;
      Left := 20;
      Top := 80;
      Width := 440;
      TabOrder := 0;
    end;

    LabelMensaje := TLabel.Create(Panel);
    with LabelMensaje do
    begin
      Parent := Panel;
      Caption := 'Mensaje:';
      Left := 20;
      Top := 115;
      Font.Style := [fsBold];
    end;

    MemoMensaje := TMemo.Create(Panel);
    with MemoMensaje do
    begin
      Parent := Panel;
      Left := 20;
      Top := 135;
      Width := 440;
      Height := 150;
      ScrollBars := ssVertical;
      TabOrder := 1;
    end;

    BtnPublicar := TButton.Create(Panel);
    with BtnPublicar do
    begin
      Parent := Panel;
      Caption := 'Publicar';
      Left := 280;
      Top := 300;
      Width := 90;
      Height := 30;
      ModalResult := mrOk;
      Default := True;
    end;

    BtnCancelar := TButton.Create(Panel);
    with BtnCancelar do
    begin
      Parent := Panel;
      Caption := 'Cancelar';
      Left := 380;
      Top := 300;
      Width := 90;
      Height := 30;
      ModalResult := mrCancel;
      Cancel := True;
    end;

    if FormPublicar.ShowModal = mrOk then
    begin
      if (Trim(EditComunidad.Text) = '') or (Trim(MemoMensaje.Lines.Text) = '') then
      begin
        MostrarMensaje('Error', 'Complete todos los campos');
        Exit;
      end;

      if FSistema.PublicarMensajeAComunidad(
            Trim(EditComunidad.Text),
            Usuario^.Email,
            Trim(MemoMensaje.Lines.Text)) then
      begin
        MostrarMensaje('Éxito', 'Mensaje publicado en la comunidad: ' + Trim(EditComunidad.Text));
      end
      else
      begin
        MostrarMensaje('Error', 'No se pudo publicar el mensaje. Verifique que la comunidad existe.');
      end;
    end;

  finally
    FormPublicar.Free;
  end;
end;

// Implementar funcionalidad de Eliminar Contacto
procedure TInterfazEDDMail.OnEliminarContactoClick(Sender: TObject);
var
  FormEliminar: TForm;
  Panel: TPanel;
  LabelTitulo, LabelEmail: TLabel;
  EditEmail: TEdit;
  BtnEliminar, BtnCancelar: TButton;
  Usuario: PUsuario;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  FormEliminar := TForm.Create(nil);
  try
    with FormEliminar do
    begin
      Caption := 'Eliminar Contacto';
      Width := 400;
      Height := 250;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color := clMoneyGreen;
    end;

    Panel := TPanel.Create(FormEliminar);
    with Panel do
    begin
      Parent := FormEliminar;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 15;
      Color := clMoneyGreen;
    end;

    LabelTitulo := TLabel.Create(Panel);
    with LabelTitulo do
    begin
      Parent := Panel;
      Caption := 'Eliminar Contacto ❌';
      Font.Size := 12;
      Font.Style := [fsBold];
      Left := 20;
      Top := 20;
    end;

    LabelEmail := TLabel.Create(Panel);
    with LabelEmail do
    begin
      Parent := Panel;
      Caption := 'Email del contacto a eliminar:';
      Left := 20;
      Top := 60;
      Font.Style := [fsBold];
    end;

    EditEmail := TEdit.Create(Panel);
    with EditEmail do
    begin
      Parent := Panel;
      Left := 20;
      Top := 80;
      Width := 340;
      TabOrder := 0;
      Hint := 'Ingrese el email del contacto que desea eliminar';
      ShowHint := True;
    end;

    BtnEliminar := TButton.Create(Panel);
    with BtnEliminar do
    begin
      Parent := Panel;
      Caption := 'Eliminar';
      Left := 180;
      Top := 130;
      Width := 80;
      Height := 30;
      ModalResult := mrOk;
      Default := True;
    end;

    BtnCancelar := TButton.Create(Panel);
    with BtnCancelar do
    begin
      Parent := Panel;
      Caption := 'Cancelar';
      Left := 270;
      Top := 130;
      Width := 80;
      Height := 30;
      ModalResult := mrCancel;
      Cancel := True;
    end;

    if FormEliminar.ShowModal = mrOk then
    begin
      if Trim(EditEmail.Text) = '' then
      begin
        MostrarMensaje('Error', 'Debe ingresar un email');
        Exit;
      end;

      if FSistema.EliminarContacto(Usuario, Trim(EditEmail.Text)) then
        MostrarMensaje('Éxito', 'Contacto eliminado correctamente')
      else
        MostrarMensaje('Error', 'No se pudo eliminar el contacto. Verifique que:' + LineEnding +
                       '- El email sea un contacto válido' + LineEnding +
                       '- El contacto existe en su lista');
    end;

  finally
    FormEliminar.Free;
  end;
end;
// Implementación de los event handlers:
procedure TInterfazEDDMail.OnCrearComunidadBSTClick(Sender: TObject);
var
  FormCrearComunidad: TForm;
  Panel: TPanel;
  LabelTitulo, LabelNombre: TLabel;
  EditNombre: TEdit;
  BtnCrear, BtnCancelar: TButton;
begin
  FormCrearComunidad := TForm.Create(nil);
  try
    with FormCrearComunidad do
    begin
      Caption := 'Crear Nueva Comunidad';
      Width := 400;
      Height := 250;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color := clMoneyGreen;
    end;

    Panel := TPanel.Create(FormCrearComunidad);
    with Panel do
    begin
      Parent := FormCrearComunidad;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 15;
      Color := clMoneyGreen;
    end;

    LabelTitulo := TLabel.Create(Panel);
    with LabelTitulo do
    begin
      Parent := Panel;
      Caption := 'Crear Nueva Comunidad 🏘️';
      Font.Size := 14;
      Font.Style := [fsBold];
      Left := 20;
      Top := 20;
    end;

    LabelNombre := TLabel.Create(Panel);
    with LabelNombre do
    begin
      Parent := Panel;
      Caption := 'Nombre de la Comunidad:';
      Left := 20;
      Top := 60;
      Font.Style := [fsBold];
    end;

    EditNombre := TEdit.Create(Panel);
    with EditNombre do
    begin
      Parent := Panel;
      Left := 20;
      Top := 80;
      Width := 340;
      TabOrder := 0;
    end;

    BtnCrear := TButton.Create(Panel);
    with BtnCrear do
    begin
      Parent := Panel;
      Caption := 'Crear';
      Left := 180;
      Top := 130;
      Width := 80;
      Height := 30;
      ModalResult := mrOk;
      Default := True;
    end;

    BtnCancelar := TButton.Create(Panel);
    with BtnCancelar do
    begin
      Parent := Panel;
      Caption := 'Cancelar';
      Left := 270;
      Top := 130;
      Width := 80;
      Height := 30;
      ModalResult := mrCancel;
      Cancel := True;
    end;

    if FormCrearComunidad.ShowModal = mrOk then
    begin
      if Trim(EditNombre.Text) = '' then
      begin
        MostrarMensaje('Error', 'Ingrese un nombre para la comunidad');
        Exit;
      end;

      if FSistema.CrearComunidadBST(Trim(EditNombre.Text)) then
        MostrarMensaje('Éxito', 'Comunidad creada: ' + Trim(EditNombre.Text))
      else
        MostrarMensaje('Error', 'Error: La comunidad ya existe');
    end;

  finally
    FormCrearComunidad.Free;
  end;
end;

// Completar la implementación de OnVerMensajesComunidadClick
procedure TInterfazEDDMail.OnVerMensajesComunidadClick(Sender: TObject);
var
  FormVerMensajes: TForm;
  Panel: TPanel;
  LabelTitulo, LabelComunidad: TLabel;
  EditComunidad: TEdit;
  MemoMensajes: TMemo;
  BtnVer, BtnCerrar: TButton;
  Mensajes: String;
begin
  FormVerMensajes := TForm.Create(nil);
  try
    with FormVerMensajes do
    begin
      Caption := 'Ver Mensajes de Comunidad';
      Width := 600;
      Height := 500;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color := clMoneyGreen;
    end;

    Panel := TPanel.Create(FormVerMensajes);
    with Panel do
    begin
      Parent := FormVerMensajes;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 15;
      Color := clMoneyGreen;
    end;

    LabelTitulo := TLabel.Create(Panel);
    with LabelTitulo do
    begin
      Parent := Panel;
      Caption := 'Mensajes de Comunidad 💬';
      Font.Size := 14;
      Font.Style := [fsBold];
      Left := 20;
      Top := 20;
    end;

    LabelComunidad := TLabel.Create(Panel);
    with LabelComunidad do
    begin
      Parent := Panel;
      Caption := 'Nombre de la Comunidad:';
      Left := 20;
      Top := 60;
      Font.Style := [fsBold];
    end;

    EditComunidad := TEdit.Create(Panel);
    with EditComunidad do
    begin
      Parent := Panel;
      Left := 20;
      Top := 80;
      Width := 400;
      TabOrder := 0;
    end;

    BtnVer := TButton.Create(Panel);
    with BtnVer do
    begin
      Parent := Panel;
      Caption := 'Ver Mensajes';
      Left := 430;
      Top := 78;
      Width := 120;
      Height := 26;
      OnClick := procedure(ASender: TObject)
      begin
        if Trim(EditComunidad.Text) = '' then
        begin
          MostrarMensaje('Error', 'Ingrese el nombre de la comunidad');
          Exit;
        end;

        Mensajes := FSistema.ObtenerMensajesComunidad(Trim(EditComunidad.Text));
        MemoMensajes.Lines.Text := Mensajes;
      end;
    end;

    MemoMensajes := TMemo.Create(Panel);
    with MemoMensajes do
    begin
      Parent := Panel;
      Left := 20;
      Top := 120;
      Width := 530;
      Height := 280;
      ReadOnly := True;
      ScrollBars := ssVertical;
      Lines.Add('Ingrese el nombre de una comunidad y presione "Ver Mensajes"');
    end;

    BtnCerrar := TButton.Create(Panel);
    with BtnCerrar do
    begin
      Parent := Panel;
      Caption := 'Cerrar';
      Left := 470;
      Top := 410;
      Width := 80;
      Height := 30;
      ModalResult := mrCancel;
    end;

    FormVerMensajes.ShowModal;

  finally
    FormVerMensajes.Free;
  end;
end;

// Función auxiliar para liberar memoria del árbol AVL
procedure TEDDMailSystem.LiberarArbolAVL(var nodo: PNodoAVL);
begin
  if nodo = nil then Exit;

  LiberarArbolAVL(nodo^.Izquierdo);
  LiberarArbolAVL(nodo^.Derecho);

  // Liberar el correo asociado
  if nodo^.Correo <> nil then
    Dispose(nodo^.Correo);

  Dispose(nodo);
  nodo := nil;
end;

// Función auxiliar para liberar memoria del árbol BST
procedure TEDDMailSystem.LiberarArbolBST(var nodo: PNodoBST);
var
  Mensaje, TempMensaje: PMensajeComunidad;
begin
  if nodo = nil then Exit;

  LiberarArbolBST(nodo^.Izquierdo);
  LiberarArbolBST(nodo^.Derecho);

  // Liberar lista de mensajes
  Mensaje := nodo^.ListaMensajes;
  while Mensaje <> nil do
  begin
    TempMensaje := Mensaje;
    Mensaje := Mensaje^.Siguiente;
    Dispose(TempMensaje);
  end;

  Dispose(nodo);
  nodo := nil;
end;

// Función auxiliar para liberar memoria del árbol B
procedure TEDDMailSystem.LiberarArbolB(var nodo: PNodoB);
var
  i: Integer;
begin
  if nodo = nil then Exit;

  if not nodo^.EsHoja then
  begin
    for i := 0 to nodo^.NumClaves do
    begin
      if nodo^.Hijos[i] <> nil then
        LiberarArbolB(nodo^.Hijos[i]);
    end;
  end;

  Dispose(nodo);
  nodo := nil;
end;

// Actualizar el destructor para liberar las nuevas estructuras
destructor TEDDMailSystem.Destroy;
var
  TempUsuario: PUsuario;
  TempComunidad: PComunidad;
begin
  // Liberar memoria de usuarios y sus estructuras
  while FUsuarios <> nil do
  begin
    TempUsuario := FUsuarios;
    FUsuarios := FUsuarios^.Siguiente;

    // Liberar nuevas estructuras del usuario
    LiberarArbolAVL(TempUsuario^.ArbolBorradores);
    LiberarArbolB(TempUsuario^.ArbolFavoritos);

    Dispose(TempUsuario);
  end;

  // Liberar árbol BST de comunidades (Fase 2)
  LiberarArbolBST(FArbolComunidades);

  // Liberar memoria de comunidades (Fase 1)
  while FComunidades <> nil do
  begin
    TempComunidad := FComunidades;
    FComunidades := FComunidades^.Siguiente;
    Dispose(TempComunidad);
  end;

  inherited Destroy;
end;
// Implementación del generador de reportes de comunidades BST (ROOT)
procedure TEDDMailSystem.GenerarReporteComunidadesBST(RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
begin
  try
    ForceDirectories(RutaCarpeta);
    NombreArchivo := RutaCarpeta + '/comunidades_bst.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Árbol BST - Comunidades";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    node [shape=record, style=filled, fillcolor=lightgreen];');

    if FArbolComunidades = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin comunidades", fillcolor=lightgray];');
    end
    else
    begin
      GenerarNodosBST(Archivo, FArbolComunidades);
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen
    try
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'dot';
        Process.Parameters.Add('-Tpng');
        Process.Parameters.Add(NombreArchivo);
        Process.Parameters.Add('-o');
        Process.Parameters.Add(ChangeFileExt(NombreArchivo, '.png'));
        Process.Options := Process.Options + [poWaitOnExit];
        Process.Execute;
        WriteLn('Reporte de comunidades BST generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte de comunidades BST: ', E.Message);
  end;
end;

// Función auxiliar para generar nodos del BST
procedure TEDDMailSystem.GenerarNodosBST(var Archivo: TextFile; nodo: PNodoBST);
var
  NombreLimpio: String;
begin
  if nodo = nil then Exit;

  NombreLimpio := StringReplace(nodo^.NombreComunidad, ' ', '_', [rfReplaceAll]);
  NombreLimpio := StringReplace(NombreLimpio, '-', '_', [rfReplaceAll]);

  WriteLn(Archivo, Format('    com_%s [label="Comunidad: %s|Creada: %s|Mensajes: %d"];',
    [NombreLimpio, nodo^.NombreComunidad, nodo^.FechaCreacion, nodo^.NumeroMensajes]));

  if nodo^.Izquierdo <> nil then
  begin
    WriteLn(Archivo, Format('    com_%s -> com_%s [label="<"];',
      [NombreLimpio, StringReplace(StringReplace(nodo^.Izquierdo^.NombreComunidad, ' ', '_', [rfReplaceAll]), '-', '_', [rfReplaceAll])]));
    GenerarNodosBST(Archivo, nodo^.Izquierdo);
  end;

  if nodo^.Derecho <> nil then
  begin
    WriteLn(Archivo, Format('    com_%s -> com_%s [label=">"];',
      [NombreLimpio, StringReplace(StringReplace(nodo^.Derecho^.NombreComunidad, ' ', '_', [rfReplaceAll]), '-', '_', [rfReplaceAll])]));
    GenerarNodosBST(Archivo, nodo^.Derecho);
  end;
end;

// Función para generar reporte de favoritos (Árbol B)
procedure TEDDMailSystem.GenerarReporteFavoritos(Usuario: PUsuario; RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
begin
  if Usuario = nil then Exit;

  try
    ForceDirectories(RutaCarpeta);
    NombreArchivo := RutaCarpeta + '/favoritos_' +
                   StringReplace(Usuario^.Usuario, ' ', '_', [rfReplaceAll]) + '.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Árbol B Orden 5 - Favoritos - ' + Usuario^.Nombre + '";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    node [shape=record, style=filled, fillcolor=lightyellow];');

    if Usuario^.ArbolFavoritos = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin favoritos", fillcolor=lightgray];');
    end
    else
    begin
      GenerarNodosB(Archivo, Usuario^.ArbolFavoritos, 0);
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen
    try
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'dot';
        Process.Parameters.Add('-Tpng');
        Process.Parameters.Add(NombreArchivo);
        Process.Parameters.Add('-o');
        Process.Parameters.Add(ChangeFileExt(NombreArchivo, '.png'));
        Process.Options := Process.Options + [poWaitOnExit];
        Process.Execute;
        WriteLn('Reporte de favoritos generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte de favoritos: ', E.Message);
  end;
end;

// Función auxiliar para generar nodos del Árbol B
procedure TEDDMailSystem.GenerarNodosB(var Archivo: TextFile; nodo: PNodoB; nivel: Integer);
var
  i: Integer;
  NodoLabel: String;
begin
  if nodo = nil then Exit;

  NodoLabel := 'nodo_' + IntToStr(nivel);

  // Generar label con todas las claves del nodo
  Write(Archivo, Format('    %s [label="', [NodoLabel]));
  for i := 0 to nodo^.NumClaves - 1 do
  begin
    if i > 0 then Write(Archivo, '|');
    Write(Archivo, Format('ID:%d', [nodo^.Claves[i]]));
  end;
  WriteLn(Archivo, '"];');

  // Generar conexiones a hijos si no es hoja
  if not nodo^.EsHoja then
  begin
    for i := 0 to nodo^.NumClaves do
    begin
      if nodo^.Hijos[i] <> nil then
      begin
        WriteLn(Archivo, Format('    %s -> nodo_%d;', [NodoLabel, nivel * 10 + i]));
        GenerarNodosB(Archivo, nodo^.Hijos[i], nivel * 10 + i);
      end;
    end;
  end;
  // Función para rellenar lista de favoritos
procedure TInterfazEDDMail.Favoritos_RellenarLista;
var
  Usuario: PUsuario;
  // Implementación simplificada - en una implementación completa
  // necesitarías recorrer el árbol B
begin
  if FListFavoritos = nil then Exit;
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  FListFavoritos.Items.Clear;
  // TODO: Implementar recorrido del árbol B de favoritos

  if Assigned(FLabelTotalFavoritos) then
    FLabelTotalFavoritos.Caption := Format('Total: %d', [FListFavoritos.Items.Count]);
end;
// Completar la implementación de Borradores_OnSeleccion
procedure TInterfazEDDMail.Borradores_OnSeleccion(Sender: TObject);
var
  Usuario: PUsuario;
  CorreoId: Integer;
  Correo: PCorreo;
begin
  if (FListBorradores = nil) or (FMemoBorrador = nil) then Exit;
  if FListBorradores.ItemIndex < 0 then Exit;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  CorreoId := Integer(PtrInt(FListBorradores.Items.Objects[FListBorradores.ItemIndex]));

  // Buscar el correo en el árbol AVL de borradores
  Correo := BuscarCorreoEnAVL(Usuario^.ArbolBorradores, CorreoId);
  if Correo <> nil then
  begin
    FMemoBorrador.Lines.Clear;
    FMemoBorrador.Lines.Add('Para: ' + Correo^.Destinatario);
    FMemoBorrador.Lines.Add('Asunto: ' + Correo^.Asunto);
    FMemoBorrador.Lines.Add('Fecha creación: ' + Correo^.Fecha);
    FMemoBorrador.Lines.Add('');
    FMemoBorrador.Lines.Add('Mensaje:');
    FMemoBorrador.Lines.Add(Correo^.Mensaje);
  end;
end;

// Mejorar la implementación de Borradores_OnEditarClick
procedure TInterfazEDDMail.Borradores_OnEditarClick(Sender: TObject);
var
  FormEditar: TForm;
  Panel: TPanel;
  LabelPara, LabelAsunto: TLabel;
  EditPara, EditAsunto: TEdit;
  MemoCuerpo: TMemo;
  BtnEnviar, BtnGuardar, BtnCancelar: TButton;
  Usuario: PUsuario;
  CorreoId: Integer;
  Correo: PCorreo;
begin
  if (FListBorradores = nil) or (FListBorradores.ItemIndex < 0) then
  begin
    MostrarMensaje('Error', 'Seleccione un borrador para editar');
    Exit;
  end;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  CorreoId := Integer(PtrInt(FListBorradores.Items.Objects[FListBorradores.ItemIndex]));
  Correo := BuscarCorreoEnAVL(Usuario^.ArbolBorradores, CorreoId);

  if Correo = nil then
  begin
    MostrarMensaje('Error', 'Borrador no encontrado');
    Exit;
  end;

  FormEditar := TForm.Create(nil);
  try
    with FormEditar do
    begin
      Caption := 'Editar Borrador';
      Width := 600;
      Height := 450;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color := clMoneyGreen;
    end;

    Panel := TPanel.Create(FormEditar);
    with Panel do
    begin
      Parent := FormEditar;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 12;
      Color := clMoneyGreen;
    end;

    LabelPara := TLabel.Create(Panel);
    with LabelPara do
    begin
      Parent := Panel;
      Caption := 'Para:';
      Left := 12; Top := 12;
      Font.Style := [fsBold];
    end;

    EditPara := TEdit.Create(Panel);
    with EditPara do
    begin
      Parent := Panel;
      Left := 12; Top := 30;
      Width := 560;
      Text := Correo^.Destinatario;
    end;

    LabelAsunto := TLabel.Create(Panel);
    with LabelAsunto do
    begin
      Parent := Panel;
      Caption := 'Asunto:';
      Left := 12; Top := 60;
      Font.Style := [fsBold];
    end;

    EditAsunto := TEdit.Create(Panel);
    with EditAsunto do
    begin
      Parent := Panel;
      Left := 12; Top := 78;
      Width := 560;
      Text := Correo^.Asunto;
    end;

    MemoCuerpo := TMemo.Create(Panel);
    with MemoCuerpo do
    begin
      Parent := Panel;
      Left := 12; Top := 115;
      Width := 560; Height := 250;
      ScrollBars := ssVertical;
      Lines.Text := Correo^.Mensaje;
    end;

    BtnEnviar := TButton.Create(Panel);
    with BtnEnviar do
    begin
      Parent := Panel;
      Caption := 'Enviar Ahora';
      Left := 280; Top := 380;
      Width := 90; Height := 30;
      ModalResult := mrOk;
      Default := True;
    end;

    BtnGuardar := TButton.Create(Panel);
    with BtnGuardar do
    begin
      Parent := Panel;
      Caption := 'Actualizar Borrador';
      Left := 380; Top := 380;
      Width := 120; Height := 30;
      ModalResult := mrYes;
    end;

    BtnCancelar := TButton.Create(Panel);
    with BtnCancelar do
    begin
      Parent := Panel;
      Caption := 'Cancelar';
      Left := 510; Top := 380;
      Width := 70; Height := 30;
      ModalResult := mrCancel;
      Cancel := True;
    end;

    case FormEditar.ShowModal of
      mrOk: begin // Enviar correo
        if FCorreoManager.EnviarCorreo(
              FSistema,
              Usuario^.Email,
              Trim(EditPara.Text),
              Trim(EditAsunto.Text),
              MemoCuerpo.Lines.Text) then
        begin
          MostrarMensaje('Éxito', 'Correo enviado y borrador eliminado');
          // TODO: Eliminar del árbol AVL
          Borradores_RellenarLista;
        end;
      end;

      mrYes: begin // Actualizar borrador
        // TODO: Actualizar el correo en el árbol AVL
        Correo^.Destinatario := Trim(EditPara.Text);
        Correo^.Asunto := Trim(EditAsunto.Text);
        Correo^.Mensaje := MemoCuerpo.Lines.Text;
        MostrarMensaje('Éxito', 'Borrador actualizado');
        Borradores_RellenarLista;
      end;
    end;

  finally
    FormEditar.Free;
  end;
end;
// Implementación del event handler
procedure TInterfazEDDMail.Inbox_OnMarcarFavoritoClick(Sender: TObject);
var
  Usuario: PUsuario;
  IdSel: Integer;
begin
  if (FListBandeja = nil) or (FListBandeja.ItemIndex < 0) then Exit;

  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  IdSel := Integer(PtrInt(FListBandeja.Items.Objects[FListBandeja.ItemIndex]));

  if FSistema.MarcarComoFavorito(Usuario, IdSel) then
    MostrarMensaje('Éxito', 'Correo marcado como favorito ⭐')
  else
    MostrarMensaje('Error', 'No se pudo marcar como favorito');
end;
end;
end.
