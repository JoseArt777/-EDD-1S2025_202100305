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
  BtnEnviar, BtnCancelar: TButton;
  Usuario: PUsuario;
begin
  Usuario := FSistema.GetUsuarioActual;
  if Usuario = nil then Exit;

  FormEnviar := TForm.Create(nil);
  try
    with FormEnviar do
    begin
      Caption := 'Enviar correo';
      Width := 600;
      Height := 450;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color:=clMoneyGreen ;
    end;

    Panel := TPanel.Create(FormEnviar);
    with Panel do
    begin
      Parent := FormEnviar;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 12;
      Color:=clMoneyGreen  ;
    end;

    LabelPara := TLabel.Create(Panel);
    with LabelPara do
    begin
      Parent := Panel; Caption := 'Para:'; Left := 12; Top := 12; Font.Style := [fsBold];
    end;

    EditPara := TEdit.Create(Panel);
    with EditPara do
    begin
      Parent := Panel; Left := 12; Top := 30; Width := 560;
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
      Parent := Panel; Left := 12; Top := 78; Width := 560; ;Height:=120;
    end;

    MemoCuerpo := TMemo.Create(Panel);
    with MemoCuerpo do
    begin
      Parent := Panel; Left := 12; Top := 115; Width := 560; Height := 250; ScrollBars := ssVertical;
    end;

    BtnEnviar := TButton.Create(Panel);
    with BtnEnviar do
    begin
      Parent := Panel; Caption := 'Enviar'; Left := 380; Top := 380; Width := 90; Height := 30;
      ModalResult := mrOk; Default := True;
    end;

    BtnCancelar := TButton.Create(Panel);
    with BtnCancelar do
    begin
      Parent := Panel; Caption := 'Cancelar'; Left := 480; Top := 380; Width := 90; Height := 30;
      ModalResult := mrCancel; Cancel := True;
    end;

    if FormEnviar.ShowModal = mrOk then
    begin
      if (Trim(EditPara.Text) = '') or (Trim(EditAsunto.Text) = '') then
      begin
        MostrarMensaje('Error', 'Debe ingresar al menos destinatario y asunto');
        Exit;
      end;

      // 🔹 AQUÍ ES DONDE YA “SE ENVÍA DE VERDAD”
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

  // Botones
  BtnOrdenar := TButton.Create(Panel);
  with BtnOrdenar do
  begin
    Parent := Panel;
    Caption := 'Ordenar A-Z (Asunto)';
    Left := 10; Top := 420; Width := 150; Height := 30;
    OnClick := @Inbox_OnOrdenarClick;
  end;

  BtnMarcarLeido := TButton.Create(Panel);
  with BtnMarcarLeido do
  begin
    Parent := Panel;
    Caption := 'Marcar como leído';
    Left := 170; Top := 420; Width := 150; Height := 30;
    OnClick := @Inbox_OnMarcarLeidoClick;
  end;

  BtnEliminar := TButton.Create(Panel);
  with BtnEliminar do
  begin
    Parent := Panel;
    Caption := 'Eliminar a Papelera';
    Left := 330; Top := 420; Width := 150; Height := 30;
    OnClick := @Inbox_OnEliminarClick;
  end;

  BtnCerrar := TButton.Create(Panel);
  with BtnCerrar do
  begin
    Parent := Panel;
    Caption := 'Cerrar';
    Left := 520; Top := 420; Width := 150; Height := 30;
          OnClick := @Inbox_OnCerrarClick;   // ✅ Correcto: cierra la BANDEJA


      Cancel := True; // permite cerrar con ESC

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
procedure TInterfazEDDMail.OnGenerarReportesClick(Sender: TObject);
var
  FormReportes: TForm;
  Panel: TPanel;
  LabelTitulo, LabelInfo: TLabel;
  BtnCorreosRecibidos, BtnPapelera, BtnCorreosProgramados, BtnContactos, BtnCerrar: TButton;
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
      Caption := '📊Generar Reportes';
      Width := 500;
      Height := 450;
      Position := poOwnerFormCenter;
      BorderStyle := bsDialog;
      Color:=clMoneyGreen;
    end;

    Panel := TPanel.Create(FormReportes);
    with Panel do
    begin
      Parent := FormReportes;
      Align := alClient;
      BevelOuter := bvNone;
      BorderWidth := 15;
      Color:=clMoneyGreen;
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

    // Botón: Reporte de Correos Recibidos
    BtnCorreosRecibidos := TButton.Create(Panel);
    with BtnCorreosRecibidos do
    begin
      Parent := Panel;
      Caption := 'Reporte de Correos Recibidos';
      Left := 20;
      Top := YPos;
      Width := 430;
      Height := 40;
      Hint := 'Lista doblemente enlazada - Bandeja de entrada';
      ShowHint := True;
      OnClick := @OnReporteCorreosRecibidosClick;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 50);

    // Botón: Reporte de Papelera
    BtnPapelera := TButton.Create(Panel);
    with BtnPapelera do
    begin
      Parent := Panel;
      Caption := 'Reporte de Papelera';
      Left := 20;
      Top := YPos;
      Width := 430;
      Height := 40;
      Hint := 'Pila LIFO - Correos eliminados';
      ShowHint := True;
      OnClick := @OnReportePapeleraClick;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 50);

    // Botón: Reporte de Correos Programados
    BtnCorreosProgramados := TButton.Create(Panel);
    with BtnCorreosProgramados do
    begin
      Parent := Panel;
      Caption := 'Reporte de Correos Programados';
      Left := 20;
      Top := YPos;
      Width := 430;
      Height := 40;
      Hint := 'Cola FIFO - Correos pendientes de envío';
      ShowHint := True;
      OnClick := @OnReporteCorreosProgramadosClick;
      Font.Style := [fsBold];
    end;
    Inc(YPos, 50);

    // Botón: Reporte de Contactos (ya implementado)
    BtnContactos := TButton.Create(Panel);
    with BtnContactos do
    begin
      Parent := Panel;
      Caption := 'Reporte de Contactos';
      Left := 20;
      Top := YPos;
      Width := 430;
      Height := 40;
      Hint := 'Lista circular - Contactos del usuario';
      ShowHint := True;
      OnClick := @OnGenerarReporteContactosClick; // Ya existe
      Font.Style := [fsBold];
    end;
    Inc(YPos, 70);

    // Botón Cerrar
    BtnCerrar := TButton.Create(Panel);
    with BtnCerrar do
    begin
      Parent := Panel;
      Caption := 'Cerrar';
      Left := 350;
      Top := YPos;
      Width := 100;
      Height := 30;
      ModalResult := mrCancel;
    end;

    FormReportes.ShowModal;

  finally
    FormReportes.Free;
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
end.
