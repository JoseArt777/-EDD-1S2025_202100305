unit ULogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, UStructures;

type
  TLoginResult = (lrSuccess, lrInvalidCredentials, lrUserNotFound, lrError);
  
  TLoginManager = class
  private
    function ValidateEmail(email: String): Boolean;
    function ValidatePassword(password: String): Boolean;
    function GenerateUserId: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Métodos de autenticación
    function Login(email, password: String; out user: PUser): TLoginResult;
    function RegisterUser(nombre, usuario, email, telefono, password: String): Boolean;
    
    // Carga masiva desde JSON
    function LoadUsersFromJSON(fileName: String): Boolean;
    
    // Validaciones
    function IsEmailAvailable(email: String): Boolean;
    function IsUsernameAvailable(username: String): Boolean;
    
    // Usuario root por defecto
    procedure InitializeRootUser;
  end;

var
  LoginManager: TLoginManager;

implementation

constructor TLoginManager.Create;
begin
  inherited Create;
  InitializeRootUser;
end;

destructor TLoginManager.Destroy;
begin
  inherited Destroy;
end;

procedure TLoginManager.InitializeRootUser;
begin
  // Crear usuario root si no existe
  if UserList.FindUserByEmail('root@edd.com') = nil then
  begin
    UserList.AddUser(0, 'Root User', 'root', 'root@edd.com', '00000000', 'root123');
  end;
end;

function TLoginManager.Login(email, password: String; out user: PUser): TLoginResult;
begin
  try
    user := UserList.ValidateUser(email, password);
    if user <> nil then
      Result := lrSuccess
    else if UserList.FindUserByEmail(email) <> nil then
      Result := lrInvalidCredentials
    else
      Result := lrUserNotFound;
  except
    Result := lrError;
    user := nil;
  end;
end;

function TLoginManager.RegisterUser(nombre, usuario, email, telefono, password: String): Boolean;
var
  userId: Integer;
begin
  Result := False;
  
  // Validaciones
  if not ValidateEmail(email) then Exit;
  if not ValidatePassword(password) then Exit;
  if not IsEmailAvailable(email) then Exit;
  if not IsUsernameAvailable(usuario) then Exit;
  
  try
    userId := GenerateUserId;
    UserList.AddUser(userId, nombre, usuario, email, telefono, password);
    Result := True;
  except
    Result := False;
  end;
end;

function TLoginManager.LoadUsersFromJSON(fileName: String): Boolean;
var
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  UsersArray: TJSONArray;
  UserObject: TJSONObject;
  i: Integer;
  FileContent: String;
  Parser: TJSONParser;
  Stream: TStringStream;
begin
  Result := False;
  
  try
    // Leer archivo JSON
    if not FileExists(fileName) then
    begin
      WriteLn('Error: Archivo ', fileName, ' no encontrado');
      Exit;
    end;
    
    // Cargar contenido del archivo
    with TStringList.Create do
    try
      LoadFromFile(fileName);
      FileContent := Text;
    finally
      Free;
    end;
    
    // Parsear JSON
    Stream := TStringStream.Create(FileContent);
    try
      Parser := TJSONParser.Create(Stream);
      try
        JSONData := Parser.Parse;
        try
          if JSONData is TJSONObject then
          begin
            JSONObject := TJSONObject(JSONData);
            UsersArray := JSONObject.Arrays['usuarios'];
            
            if UsersArray <> nil then
            begin
              for i := 0 to UsersArray.Count - 1 do
              begin
                UserObject := UsersArray.Objects[i];
                if UserObject <> nil then
                begin
                  // Agregar usuario con contraseña por defecto
                  UserList.AddUser(
                    UserObject.Get('id', 0),
                    UserObject.Get('nombre', ''),
                    UserObject.Get('usuario', ''),
                    UserObject.Get('email', ''),
                    UserObject.Get('telefono', ''),
                    '123456' // Contraseña por defecto
                  );
                end;
              end;
              Result := True;
              WriteLn('Usuarios cargados exitosamente desde ', fileName);
            end;
          end;
        finally
          JSONData.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      Stream.Free;
    end;
    
  except
    on E: Exception do
    begin
      WriteLn('Error al cargar usuarios desde JSON: ', E.Message);
      Result := False;
    end;
  end;
end;

function TLoginManager.IsEmailAvailable(email: String): Boolean;
begin
  Result := UserList.FindUserByEmail(email) = nil;
end;

function TLoginManager.IsUsernameAvailable(username: String): Boolean;
begin
  Result := UserList.FindUserByUsername(username) = nil;
end;

function TLoginManager.ValidateEmail(email: String): Boolean;
begin
  // Validación básica de email
  Result := (Pos('@', email) > 0) and (Pos('.', email) > 0) and (Length(email) > 5);
end;

function TLoginManager.ValidatePassword(password: String): Boolean;
begin
  // Validación básica de contraseña
  Result := Length(password) >= 6;
end;

function TLoginManager.GenerateUserId: Integer;
var
  maxId: Integer;
  current: PUser;
begin
  maxId := 0;
  current := UserList.head;
  
  while current <> nil do
  begin
    if current^.id > maxId then
      maxId := current^.id;
    current := current^.next;
  end;
  
  Result := maxId + 1;
end;

// Inicialización global
initialization
  LoginManager := TLoginManager.Create;

finalization
  LoginManager.Free;

end.
