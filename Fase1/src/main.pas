program EDDMail;

{$mode objfpc}{$H+}

uses
  gtk2, glib2, Classes, SysUtils,
  UStructures, UUser, ULogin, URoot, UCommunities, UReports,
  login_interface, root_interface, user_interface;

type
  TMainApplication = class
  private
    FUserManager: TUserManager;
    FLoginSession: TLoginSession;
    FRootManager: TRootManager;
    FReportGenerator: TReportGenerator;
    
    FLoginInterface: TLoginInterface;
    FRootInterface: TRootInterface;
    FUserInterface: TUserInterface;
    
    procedure OnLoginSuccess(Sender: TObject);
    procedure OnLogout(Sender: TObject);
    
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Initialize;
    procedure Run;
  end;

var
  Application: TMainApplication;

// TMainApplication Implementation
constructor TMainApplication.Create;
begin
  inherited Create;
  
  // Inicializar GTK
  gtk_init(@argc, @argv);
  
  // Crear managers
  FUserManager := TUserManager.Create;
  FLoginSession := TLoginSession.Create(FUserManager);
  FRootManager := TRootManager.Create(FUserManager);
  FReportGenerator := TReportGenerator.Create(FUserManager, FRootManager.CommunityManager);
  
  // Crear interfaces
  FLoginInterface := TLoginInterface.Create(FLoginSession);
  FRootInterface := TRootInterface.Create(FRootManager, FLoginSession);
  FUserInterface := TUserInterface.Create(FLoginSession, FReportGenerator);
  
  // Configurar eventos
  FLoginInterface.OnLoginSuccess := @OnLoginSuccess;
  FRootInterface.OnLogout := @OnLogout;
  FUserInterface.OnLogout := @OnLogout;
end;

destructor TMainApplication.Destroy;
begin
  FUserInterface.Free;
  FRootInterface.Free;
  FLoginInterface.Free;
  FReportGenerator.Free;
  FRootManager.Free;
  FLoginSession.Free;
  FUserManager.Free;
  inherited Destroy;
end;

procedure TMainApplication.Initialize;
begin
  // Cargar datos iniciales si existe el archivo
  if FileExists('data/usuarios.json') then
  begin
    var errorMsg: string;
    FRootManager.LoadUsersFromJSON('data/usuarios.json', errorMsg);
    if errorMsg <> '' then
      WriteLn('Warning: ', errorMsg);
  end;
  
  // Mostrar ventana de login
  FLoginInterface.Show;
end;

procedure TMainApplication.OnLoginSuccess(Sender: TObject);
begin
  FLoginInterface.Hide;
  
  if FLoginSession.IsRootUser then
  begin
    FRootInterface.Show;
  end
  else
  begin
    FUserInterface.Initialize;
    FUserInterface.Show;
  end;
end;

procedure TMainApplication.OnLogout(Sender: TObject);
begin
  if FRootInterface <> nil then
    FRootInterface.Hide;
  if FUserInterface <> nil then
    FUserInterface.Hide;
    
  FLoginInterface.ClearFields;
  FLoginInterface.Show;
end;

procedure TMainApplication.Run;
begin
  gtk_main;
end;

// Programa principal
begin
  try
    Application := TMainApplication.Create;
    try
      Application.Initialize;
      Application.Run;
    finally
      Application.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      Halt(1);
    end;
  end;
end.
