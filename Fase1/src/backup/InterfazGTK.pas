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
    procedure OnCrearComunidadClick(Sender: TObject);
    procedure OnAsignarUsuarioClick(Sender: TObject);
    procedure OnListarComunidadesClick(Sender: TObject);
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

procedure TInterfazEDDMail.CrearInterfazUsuario;
var
  Panel: TPanel;
  LabelSaludo, LabelMenu, LabelInfo: TLabel;
  Usuario: PUsuario;
  BtnBandeja, BtnEnviar, BtnPapelera, BtnProgramar, BtnContactos, BtnPerfil, BtnReportes, BtnCerrarSesion: TButton;
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
  
  // Primera columna de botones
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
  end;
  Inc(YPos, 50);
  
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
  end;
  Inc(YPos, 50);
  
  BtnContactos := TButton.Create(Panel);
  with BtnContactos do
  begin
    Parent := Panel;
    Caption := 'Contactos';
    Left := 20;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Gestionar contactos';
    ShowHint := True;
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
  end;
  Inc(YPos, 50);
  
  BtnReportes := TButton.Create(Panel);
  with BtnReportes do
  begin
    Parent := Panel;
    Caption := 'Generar Reportes';
    Left := 20;
    Top := YPos;
    Width := 180;
    Height := 35;
    Hint := 'Generar reportes personales';
    ShowHint := True;
  end;
  Inc(YPos, 80);
  
  BtnCerrarSesion := TButton.Create(Panel);
  with BtnCerrarSesion do
  begin
    Parent := Panel;
    Caption := 'Cerrar Sesión';
    Left := 20;
    Top := YPos;
    Width := 180;
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

end.
