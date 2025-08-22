unit ULogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UStructures, UUser;

type
  TLoginSession = class
  private
    FUserManager: TUserManager;
    FIsLoggedIn: Boolean;
    FIsRootUser: Boolean;
  public
    constructor Create(userManager: TUserManager);
    destructor Destroy; override;
    
    // Autenticación
    function Login(email, password: string; out errorMsg: string): Boolean;
    procedure Logout;
    
    // Estado de sesión
    function IsLoggedIn: Boolean;
    function IsRootUser: Boolean;
    function GetCurrentUser: PUser;
    function GetCurrentUserEmail: string;
    function GetCurrentUserName: string;
    
    // Registro de nuevos usuarios
    function RegisterNewUser(nombre, usuario, email, telefono, password: string; 
                           out errorMsg: string): Boolean;
  end;

implementation

constructor TLoginSession.Create(userManager: TUserManager);
begin
  inherited Create;
  FUserManager := userManager;
  FIsLoggedIn := False;
  FIsRootUser := False;
end;

destructor TLoginSession.Destroy;
begin
  inherited Destroy;
end;

function TLoginSession.Login(email, password: string; out errorMsg: string): Boolean;
begin
  Result := False;
  errorMsg := '';
  
  // Validaciones básicas
  if Trim(email) = '' then
  begin
    errorMsg := 'El email es requerido';
    Exit;
  end;
  
  if Trim(password) = '' then
  begin
    errorMsg := 'La contraseña es requerida';
    Exit;
  end;
  
  // Intentar autenticar
  if FUserManager.LoginUser(email, password) then
  begin
    FIsLoggedIn := True;
    FIsRootUser := (email = 'root@edd.com');
    Result := True;
  end
  else
  begin
    errorMsg := 'Email o contraseña incorrectos';
  end;
end;

procedure TLoginSession.Logout;
begin
  FUserManager.LogoutUser;
  FIsLoggedIn := False;
  FIsRootUser := False;
end;

function TLoginSession.IsLoggedIn: Boolean;
begin
  Result := FIsLoggedIn and FUserManager.IsUserLoggedIn;
end;

function TLoginSession.IsRootUser: Boolean;
begin
  Result := FIsRootUser and IsLoggedIn;
end;

function TLoginSession.GetCurrentUser: PUser;
begin
  if IsLoggedIn then
    Result := FUserManager.GetCurrentUser
  else
    Result := nil;
end;

function TLoginSession.GetCurrentUserEmail: string;
var
  user: PUser;
begin
  user := GetCurrentUser;
  if user <> nil then
    Result := user^.email
  else
    Result := '';
end;

function TLoginSession.GetCurrentUserName: string;
var
  user: PUser;
begin
  user := GetCurrentUser;
  if user <> nil then
    Result := user^.nombre
  else
    Result := '';
end;

function TLoginSession.RegisterNewUser(nombre, usuario, email, telefono, password: string; 
                                     out errorMsg: string): Boolean;
begin
  Result := False;
  errorMsg := '';
  
  // Validaciones básicas
  if Trim(nombre) = '' then
  begin
    errorMsg := 'El nombre es requerido';
    Exit;
  end;
  
  if Trim(usuario) = '' then
  begin
    errorMsg := 'El nombre de usuario es requerido';
    Exit;
  end;
  
  if Trim(email) = '' then
  begin
    errorMsg := 'El email es requerido';
    Exit;
  end;
  
  if Trim(password) = '' then
  begin
    errorMsg := 'La contraseña es requerida';
    Exit;
  end;
  
  if not FUserManager.ValidateEmail(email) then
  begin
    errorMsg := 'Formato de email inválido';
    Exit;
  end;
  
  if not FUserManager.ValidateUniqueEmail(email) then
  begin
    errorMsg := 'El email ya está registrado';
    Exit;
  end;
  
  if not FUserManager.ValidateUniqueUsername(usuario) then
  begin
    errorMsg := 'El nombre de usuario ya está en uso';
    Exit;
  end;
  
  // Registrar usuario
  Result := FUserManager.RegisterUser(nombre, usuario, email, telefono, password);
  if not Result then
    errorMsg := 'Error al registrar el usuario';
end;

end.
