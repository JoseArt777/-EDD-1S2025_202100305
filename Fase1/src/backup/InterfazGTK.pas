unit InterfazGTK;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  FileUtil, EstructurasDatos;

type
  TInterfazEDDMail = class
  private
    FSistema: TEDDMailSystem;
    FFormLogin: TForm;
    FFormPrincipal: TForm;
    FUsuarioActivo: Boolean;
    FEditEmail: TEdit;
    FEditPassword: TEdit;

      FContactoActual: PContacto;
  FPrimerContacto: PContacto;
  FIndiceContactoActual: Integer;
  FTotalContactos: Integer;
    // Controles para comunidades
    FEditNombreComunidad: TEdit;
    FEditEmailUsuario: TEdit;
    FMemoComunidades: TMemo;

    procedure CrearFormLogin;
    procedure CrearFormPrincipal;
    procedure CrearInterfazRoot;
    procedure CrearInterfazUsuario;
    procedure MostrarMensaje(Titulo, Mensaje: String);
    
    // Event handlers
    procedure OnLoginClick(Sender: TObject);
    procedure OnCrearCuentaClick(Sender: TObject);
    procedure OnCargaMasivaClick(Sender: TObject);
    procedure OnReporteUsuariosClick(Sender: TObject);
    procedure OnReporteRelacionesClick(Sender: TObject);
    procedure OnCerrarSesionClick(Sender: TObject);
    procedure OnFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OnKeyPress(Sender: TObject; var Key: Char);

        // Nuevos event handlers para comunidades
    procedure OnGestionarComunidadesClick(Sender: TObject);
    procedure OnReporteComunidadesClick(Sender: TObject);
    procedure OnActualizarPerfilClick(Sender: TObject);  // <- Agregar esto
    procedure OnCrearComunidadClick(Sender: TObject);
    procedure OnAsignarUsuarioClick(Sender: TObject);
    procedure OnListarComunidadesClick(Sender: TObject);

     // Event handlers para contactos
  procedure OnAgregarContactoClick(Sender: TObject);
  procedure OnVerContactosClick(Sender: TObject);
  procedure OnContactoAnteriorClick(Sender: TObject);
  procedure OnContactoSiguienteClick(Sender: TObject);
  procedure OnGenerarReporteContactosClick(Sender: TObject);
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
  FUsuarioActivo := False;
  FFormLogin := nil;
  FFormPrincipal := nil;
end;

destructor TInterfazEDDMail.Destroy;
begin
  FSistema.Free;
  if Assigned(FFormLogin) then
    FFormLogin.Free;
  if Assigned(FFormPrincipal) then
    FFormPrincipal.Free;
  inherited Destroy;
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
    Color := clForm;
  end;

  Usuario := FSistema.GetUsuarioActual;

  LabelSaludo := TLabel.Create(Panel);
  with LabelSaludo do
  begin
    Parent := Panel;
    Caption := 'Hola: ' + Usuario^.Nombre;
    Font.Size := 16;
    Font.Style := [fsBold];
    Font.Color := clNavy;
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
    Font.Color := clGray;
  end;

  YPos := 110;

  // FILA 1: Bandeja de Entrada y Enviar Correo
  BtnBandeja := TButton.Create(Panel);
  with BtnBandeja do
  begin
    Parent := Panel;
    Caption := 'Bandeja de Entrada';
    Left := 20;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Ver correos recibidos';
    ShowHint := True;
    // OnClick := @OnBandejaClick; // Implementar después
  end;

  BtnEnviar := TButton.Create(Panel);
  with BtnEnviar do
  begin
    Parent := Panel;
    Caption := 'Enviar Correo';
    Left := 220;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Enviar un nuevo correo';
    ShowHint := True;
    // OnClick := @OnEnviarCorreoClick; // Implementar después
  end;
  Inc(YPos, 50);

  // FILA 2: Papelera y Programar Correo
  BtnPapelera := TButton.Create(Panel);
  with BtnPapelera do
  begin
    Parent := Panel;
    Caption := 'Papelera';
    Left := 20;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Ver correos eliminados';
    ShowHint := True;
    // OnClick := @OnPapeleraClick; // Implementar después
  end;

  BtnProgramar := TButton.Create(Panel);
  with BtnProgramar do
  begin
    Parent := Panel;
    Caption := 'Programar Correo';
    Left := 220;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Programar envío automático';
    ShowHint := True;
    // OnClick := @OnProgramarCorreoClick; // Implementar después
  end;
  Inc(YPos, 50);

  // FILA 3: Correos Programados y Agregar Contacto
  BtnCorreosProgramados := TButton.Create(Panel);
  with BtnCorreosProgramados do
  begin
    Parent := Panel;
    Caption := 'Correos Programados';
    Left := 20;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Ver y enviar correos programados';
    ShowHint := True;
    // OnClick := @OnCorreosProgramadosClick; // Implementar después
  end;

  BtnAgregarContacto := TButton.Create(Panel);
  with BtnAgregarContacto do
  begin
    Parent := Panel;
    Caption := 'Agregar Contacto';
    Left := 220;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Agregar nuevo contacto';
    ShowHint := True;
    OnClick := @OnAgregarContactoClick; // YA IMPLEMENTADO
    Font.Style := [fsBold];
    Color := clLime;
  end;
  Inc(YPos, 50);

  // FILA 4: Contactos y Actualizar Perfil
  BtnContactos := TButton.Create(Panel);
  with BtnContactos do
  begin
    Parent := Panel;
    Caption := 'Contactos';
    Left := 20;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Ver y navegar entre contactos';
    ShowHint := True;
    OnClick := @OnVerContactosClick; // YA IMPLEMENTADO
    Font.Style := [fsBold];
    Color := clSkyBlue;
  end;

  BtnPerfil := TButton.Create(Panel);
  with BtnPerfil do
  begin
    Parent := Panel;
    Caption := 'Actualizar Perfil';
    Left := 220;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Modificar información personal';
    ShowHint := True;
    OnClick := @OnActualizarPerfilClick; // YA IMPLEMENTADO
  end;
  Inc(YPos, 50);

  // FILA 5: Generar Reportes (centrado)
  BtnReportes := TButton.Create(Panel);
  with BtnReportes do
  begin
    Parent := Panel;
    Caption := 'Generar Reportes';
    Left := 120; // Centrado
    Top := YPos;
    Width := 200;
    Height := 35;
    Hint := 'Generar reportes personales (Correos, Papelera, Programados, Contactos)';
    ShowHint := True;
    // OnClick := @OnGenerarReportesClick; // Implementar después
    Font.Style := [fsBold];
    Color := clLightYellow;
  end;
  Inc(YPos, 80);

  // CERRAR SESIÓN (separado)
  BtnCerrarSesion := TButton.Create(Panel);
  with BtnCerrarSesion do
  begin
    Parent := Panel;
    Caption := 'Cerrar Sesión';
    Left := 120; // Centrado
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
    Caption := 'EDDMail - Iniciar Sesión';
    Width := 450;
    Height := 350;
    Position := poScreenCenter;
    BorderStyle := bsDialog;
    OnClose := @Self.OnFormClose;   // <- uso Self
    KeyPreview := True;
    OnKeyPress := @Self.OnKeyPress; // <- uso Self
  end;

  Panel := TPanel.Create(FFormLogin);
  with Panel do
  begin
    Parent := FFormLogin;
    Align := alClient;
    BevelOuter := bvNone;
    BorderWidth := 20;
    Color := clForm;
  end;

  LabelTitulo := TLabel.Create(Panel);
  with LabelTitulo do
  begin
    Parent := Panel;
    Caption := 'EDDMail - Sistema de Correos';
    Font.Size := 16;
    Font.Style := [fsBold];
    Font.Color := clNavy;
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
    OnClick := @Self.OnLoginClick;   // <- uso Self
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
    OnClick := @Self.OnCrearCuentaClick; // <- uso Self
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
    Width := 800;
    Height := 600;
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
    Color := clForm;
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
    Caption := 'Bienvenido Administrador. Seleccione una opción:';
    Left := 20;
    Top := 50;
    Font.Color := clGray;
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
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    with OpenDialog do
    begin
      Title := 'Seleccionar archivo JSON';
      Filter := 'Archivos JSON|*.json|Todos los archivos|*.*';
      DefaultExt := 'json';
      if Execute then
      begin
        try
          FSistema.CargarUsuariosDesdeJSON(FileName);
          MostrarMensaje('Éxito', 'Usuarios cargados desde: ' + ExtractFileName(FileName));
        except
          on E: Exception do
            MostrarMensaje('Error', 'Error al cargar JSON: ' + E.Message);
        end;
      end;
    end;
  finally
    OpenDialog.Free;
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
    end;

    PanelComunidades := TPanel.Create(FormComunidades);
    with PanelComunidades do
    begin
      Parent := FormComunidades;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 15;
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

procedure TInterfazEDDMail.OnReporteComunidadesClick(Sender: TObject);
begin
  try
    FSistema.GenerarReporteComunidades('Root-Reportes');
    MostrarMensaje('Éxito', 'Reporte de comunidades generado en: Root-Reportes/' + LineEnding +
      'Archivos generados:' + LineEnding +
      '- comunidades.dot (código Graphviz)' + LineEnding +
      '- comunidades.png (imagen)');
  except
    on E: Exception do
      MostrarMensaje('Error', 'Error al generar reporte: ' + E.Message);
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
  FormContactos: TForm;
  PanelContactos, PanelInfo, PanelBotones: TPanel;
  LabelTitulo, LabelNombre, LabelUsuario, LabelEmail, LabelTelefono, LabelContador: TLabel;
  BtnAnterior, BtnSiguiente, BtnAgregar, BtnReporte, BtnCerrar: TButton;
  Usuario: PUsuario;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then
    Exit;

  // Inicializar variables de navegación
  FPrimerContacto := FSistema.GetContactos(Usuario);
  FContactoActual := FPrimerContacto;
  FIndiceContactoActual := 1;
  FTotalContactos := FSistema.ContarContactos(FPrimerContacto);

  FormContactos := TForm.Create(nil);
  try
    with FormContactos do
    begin
      Caption := 'Mis Contactos';
      Width := 500;
      Height := 400;
      Position := poOwnerFormCenter;
      BorderStyle := bsSizeable;
    end;

    PanelContactos := TPanel.Create(FormContactos);
    with PanelContactos do
    begin
      Parent := FormContactos;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 10;
    end;

    LabelTitulo := TLabel.Create(PanelContactos);
    with LabelTitulo do
    begin
      Parent := PanelContactos;
      Caption := 'Lista de Contactos';
      Font.Size := 14;
      Font.Style := [fsBold];
      Left := 20;
      Top := 20;
    end;

    LabelContador := TLabel.Create(PanelContactos);
    with LabelContador do
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

    // Crear labels para información del contacto
    if FContactoActual <> nil then
    begin
      LabelNombre := TLabel.Create(PanelInfo);
      with LabelNombre do
      begin
        Parent := PanelInfo;
        Caption := 'Nombre: ' + FContactoActual^.Nombre;
        Left := 20;
        Top := 20;
        Font.Style := [fsBold];
      end;

      LabelUsuario := TLabel.Create(PanelInfo);
      with LabelUsuario do
      begin
        Parent := PanelInfo;
        Caption := 'Usuario: ' + FContactoActual^.Usuario;
        Left := 20;
        Top := 50;
      end;

      LabelEmail := TLabel.Create(PanelInfo);
      with LabelEmail do
      begin
        Parent := PanelInfo;
        Caption := 'Email: ' + FContactoActual^.Email;
        Left := 20;
        Top := 80;
        Font.Color := clBlue;
      end;

      LabelTelefono := TLabel.Create(PanelInfo);
      with LabelTelefono do
      begin
        Parent := PanelInfo;
        Caption := 'Teléfono: ' + FContactoActual^.Telefono;
        Left := 20;
        Top := 110;
      end;
    end;

    FormContactos.ShowModal;

  finally
    FormContactos.Free;
  end;
end;

procedure TInterfazEDDMail.OnContactoAnteriorClick(Sender: TObject);
var
  Actual: PContacto;
  Contador: Integer;
begin
  if (FContactoActual = nil) or (FTotalContactos <= 1) then
    Exit;

  // En lista circular, para ir al anterior necesitamos recorrer hasta encontrar
  // el nodo que apunta al actual
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

  // Actualizar la interfaz
  OnVerContactosClick(nil);
end;

procedure TInterfazEDDMail.OnContactoSiguienteClick(Sender: TObject);
begin
  if (FContactoActual = nil) or (FTotalContactos <= 1) then
    Exit;

  FContactoActual := FContactoActual^.Siguiente;
  Inc(FIndiceContactoActual);
  if FIndiceContactoActual > FTotalContactos then
    FIndiceContactoActual := 1;

  // Actualizar la interfaz
  OnVerContactosClick(nil);
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
end.
