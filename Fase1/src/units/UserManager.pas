unit UserManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, DataStructures, SystemCore;

// Funciones de gestión de usuarios
function RegisterUser(Nombre, Usuario, Email, Telefono, Password: String): Boolean;
function LoginUser(Email, Password: String): PUser;
function UpdateUserProfile(Email, NewNombre, NewUsuario, NewTelefono: String): Boolean;
function LoadUsersFromJSON(FileName: String): Boolean;
function UserExists(Email: String): Boolean;
function GenerateUserReport: String;

implementation

function RegisterUser(Nombre, Usuario, Email, Telefono, Password: String): Boolean;
var
  ExistingUser: PUser;
begin
  Result := False;

  // Verificar si el usuario ya existe
  ExistingUser := UserList.Find(Email);
  if ExistingUser <> nil then
  begin
    WriteLn('Error: El usuario con email ', Email, ' ya existe');
    Exit;
  end;

  // Agregar nuevo usuario
  UserList.Add(NextUserId, Nombre, Usuario, Email, Telefono, Password);
  Inc(NextUserId);

  WriteLn('Usuario registrado exitosamente: ', Email);
  Result := True;
end;

function LoginUser(Email, Password: String): PUser;
var
  User: PUser;
begin
  Result := nil;

  User := UserList.Login(Email, Password);
  if User <> nil then
  begin
    CurrentUser := User;
    WriteLn('Login exitoso para: ', Email);
    Result := User;
  end
  else
  begin
    WriteLn('Error: Credenciales incorrectas para: ', Email);
  end;
end;

function UpdateUserProfile(Email, NewNombre, NewUsuario, NewTelefono: String): Boolean;
begin
  Result := False;

  if not IsUserLoggedIn then
  begin
    WriteLn('Error: No hay usuario logueado');
    Exit;
  end;

  if CurrentUser^.Email <> Email then
  begin
    WriteLn('Error: Solo puedes actualizar tu propio perfil');
    Exit;
  end;

  UserList.UpdateUser(Email, NewNombre, NewUsuario, NewTelefono);
  WriteLn('Perfil actualizado exitosamente');
  Result := True;
end;

// Reemplaza la función LoadUsersFromJSON en src/units/UserManager.pas

function LoadUsersFromJSON(FileName: String): Boolean;
var
  JSONFile: TextFile;
  JSONString, Line: String;
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  UsersArray: TJSONArray;
  UserObject: TJSONObject;
  i: Integer;
  Id: Integer;
  Nombre, Usuario, Email, Telefono: String;
  DefaultPassword: String;
begin
  Result := False;
  DefaultPassword := '123456'; // Contraseña por defecto para usuarios cargados

  try
    if not FileExists(FileName) then
    begin
      WriteLn('Error: El archivo ', FileName, ' no existe');
      Exit;
    end;

    WriteLn('DEBUG: Leyendo archivo JSON: ', FileName);

    // Leer archivo JSON completo - CORREGIDO
    AssignFile(JSONFile, FileName);
    Reset(JSONFile);
    JSONString := '';
    while not EOF(JSONFile) do
    begin
      ReadLn(JSONFile, Line);
      JSONString := JSONString + Line + LineEnding;  // CONCATENAR, no sobrescribir
    end;
    CloseFile(JSONFile);

    WriteLn('DEBUG: Contenido JSON leído (', Length(JSONString), ' caracteres)');
    WriteLn('DEBUG: Primeros 100 caracteres: ', Copy(JSONString, 1, 100));

    // Parsear JSON
    JSONData := GetJSON(JSONString);
    try
      if JSONData is TJSONObject then
      begin
        JSONObject := TJSONObject(JSONData);

        // Verificar que existe la clave "usuarios"
        if not JSONObject.Find('usuarios', JSONData) then
        begin
          WriteLn('Error: No se encontró la clave "usuarios" en el JSON');
          Exit;
        end;

        UsersArray := JSONObject.Arrays['usuarios'];
        WriteLn('DEBUG: Array de usuarios encontrado con ', UsersArray.Count, ' elementos');

        for i := 0 to UsersArray.Count - 1 do
        begin
          UserObject := TJSONObject(UsersArray[i]);

          Id := UserObject.Integers['id'];
          Nombre := UserObject.Strings['nombre'];
          Usuario := UserObject.Strings['usuario'];
          Email := UserObject.Strings['email'];
          Telefono := UserObject.Strings['telefono'];

          WriteLn('DEBUG: Procesando usuario: ', Email);

          // Verificar si el usuario ya existe
          if not UserExists(Email) then
          begin
            UserList.Add(Id, Nombre, Usuario, Email, Telefono, DefaultPassword);
            if Id >= NextUserId then
              NextUserId := Id + 1;
            WriteLn('DEBUG: Usuario agregado: ', Email);
          end
          else
          begin
            WriteLn('DEBUG: Usuario ya existe, omitiendo: ', Email);
          end;
        end;

        WriteLn('Usuarios cargados exitosamente desde: ', FileName);
        WriteLn('Total de usuarios en el sistema: ', UserList.GetCount);
        Result := True;
      end
      else
      begin
        WriteLn('Error: El JSON no es un objeto válido');
      end;
    finally
      JSONData.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('Error al cargar usuarios desde JSON: ', E.Message);
      WriteLn('Clase de error: ', E.ClassName);
    end;
  end;
end;
function UserExists(Email: String): Boolean;
begin
  Result := UserList.Find(Email) <> nil;
end;

function GenerateUserReport: String;
var
  Report: String;
  Current: PUser;
  Count: Integer;
begin
  Report := 'REPORTE DE USUARIOS' + LineEnding;
  Report := Report + '===================' + LineEnding + LineEnding;

  Count := 0;
  Current := UserList.GetFirst;

  while Current <> nil do
  begin
    Inc(Count);
    Report := Report + 'Usuario #' + IntToStr(Count) + LineEnding;
    Report := Report + 'ID: ' + IntToStr(Current^.Id) + LineEnding;
    Report := Report + 'Nombre: ' + Current^.Nombre + LineEnding;
    Report := Report + 'Usuario: ' + Current^.Usuario + LineEnding;
    Report := Report + 'Email: ' + Current^.Email + LineEnding;
    Report := Report + 'Teléfono: ' + Current^.Telefono + LineEnding;
    Report := Report + '------------------------' + LineEnding;

    Current := Current^.Next;
  end;

  Report := Report + LineEnding + 'Total de usuarios: ' + IntToStr(Count);
  Result := Report;
end;

end.

