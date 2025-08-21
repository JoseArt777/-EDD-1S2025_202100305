unit CommunityManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataStructures, SystemCore;

// Funciones de gestión de comunidades
function CreateCommunity(Nombre: String): Boolean;
function AddUserToCommunity(CommunityId: Integer; UserEmail: String): Boolean;
function RemoveUserFromCommunity(CommunityId: Integer; UserEmail: String): Boolean;
function FindCommunity(Id: Integer): PCommunity;
function FindCommunityByName(Nombre: String): PCommunity;
function GetCommunityUsers(CommunityId: Integer): String;
function IsUserInCommunity(CommunityId: Integer; UserEmail: String): Boolean;
function GenerateCommunitiesReport: String;

implementation

function CreateCommunity(Nombre: String): Boolean;
var
  ExistingCommunity: PCommunity;
begin
  Result := False;

  if not IsRootUser then
  begin
    WriteLn('Error: Solo el usuario root puede crear comunidades');
    Exit;
  end;

  // Verificar si ya existe una comunidad con ese nombre
  ExistingCommunity := FindCommunityByName(Nombre);
  if ExistingCommunity <> nil then
  begin
    WriteLn('Error: Ya existe una comunidad con el nombre: ', Nombre);
    Exit;
  end;

  // Crear nueva comunidad
  CommunityList.Add(NextCommunityId, Nombre);
  Inc(NextCommunityId);

  WriteLn('Comunidad creada exitosamente: ', Nombre);
  Result := True;
end;

function AddUserToCommunity(CommunityId: Integer; UserEmail: String): Boolean;
var
  Community: PCommunity;
  User: PUser;
begin
  Result := False;

  if not IsRootUser then
  begin
    WriteLn('Error: Solo el usuario root puede agregar usuarios a comunidades');
    Exit;
  end;

  // Verificar que la comunidad existe
  Community := CommunityList.Find(CommunityId);
  if Community = nil then
  begin
    WriteLn('Error: La comunidad con ID ', CommunityId, ' no existe');
    Exit;
  end;

  // Verificar que el usuario existe
  User := UserList.Find(UserEmail);
  if User = nil then
  begin
    WriteLn('Error: El usuario ', UserEmail, ' no existe');
    Exit;
  end;

  // Verificar si el usuario ya está en la comunidad
  if IsUserInCommunity(CommunityId, UserEmail) then
  begin
    WriteLn('Error: El usuario ya está en esta comunidad');
    Exit;
  end;

  // Agregar usuario a la comunidad
  CommunityList.AddUserToCommunity(CommunityId, UserEmail);

  WriteLn('Usuario ', UserEmail, ' agregado a la comunidad ID ', CommunityId);
  Result := True;
end;

function RemoveUserFromCommunity(CommunityId: Integer; UserEmail: String): Boolean;
var
  Community: PCommunity;
  CurrentUser, PrevUser: PUserCommunity;
begin
  Result := False;

  if not IsRootUser then
  begin
    WriteLn('Error: Solo el usuario root puede remover usuarios de comunidades');
    Exit;
  end;

  // Verificar que la comunidad existe
  Community := CommunityList.Find(CommunityId);
  if Community = nil then
  begin
    WriteLn('Error: La comunidad con ID ', CommunityId, ' no existe');
    Exit;
  end;

  // Buscar y remover usuario de la comunidad
  CurrentUser := Community^.Users;
  PrevUser := nil;

  while CurrentUser <> nil do
  begin
    if CurrentUser^.Email = UserEmail then
    begin
      // Remover de la lista
      if PrevUser = nil then
        Community^.Users := CurrentUser^.Next
      else
        PrevUser^.Next := CurrentUser^.Next;

      Dispose(CurrentUser);
      WriteLn('Usuario ', UserEmail, ' removido de la comunidad ID ', CommunityId);
      Result := True;
      Exit;
    end;

    PrevUser := CurrentUser;
    CurrentUser := CurrentUser^.Next;
  end;

  WriteLn('Error: El usuario no está en esta comunidad');
end;

function FindCommunity(Id: Integer): PCommunity;
begin
  Result := CommunityList.Find(Id);
end;

function FindCommunityByName(Nombre: String): PCommunity;
var
  Current: PCommunity;
begin
  Current := CommunityList.GetFirst;

  while Current <> nil do
  begin
    if Current^.Nombre = Nombre then
    begin
      Result := Current;
      Exit;
    end;
    Current := Current^.Next;
  end;

  Result := nil;
end;

function GetCommunityUsers(CommunityId: Integer): String;
var
  Community: PCommunity;
  CurrentUser: PUserCommunity;
  UserList: String;
  Count: Integer;
begin
  Community := CommunityList.Find(CommunityId);
  if Community = nil then
  begin
    Result := 'Comunidad no encontrada';
    Exit;
  end;

  UserList := 'Usuarios en la comunidad "' + Community^.Nombre + '":' + LineEnding;
  Count := 0;

  CurrentUser := Community^.Users;
  while CurrentUser <> nil do
  begin
    Inc(Count);
    UserList := UserList + IntToStr(Count) + '. ' + CurrentUser^.Email + LineEnding;
    CurrentUser := CurrentUser^.Next;
  end;

  if Count = 0 then
    UserList := UserList + 'No hay usuarios en esta comunidad.' + LineEnding;

  Result := UserList;
end;

function IsUserInCommunity(CommunityId: Integer; UserEmail: String): Boolean;
var
  Community: PCommunity;
  CurrentUser: PUserCommunity;
begin
  Result := False;

  Community := CommunityList.Find(CommunityId);
  if Community = nil then
    Exit;

  CurrentUser := Community^.Users;
  while CurrentUser <> nil do
  begin
    if CurrentUser^.Email = UserEmail then
    begin
      Result := True;
      Exit;
    end;
    CurrentUser := CurrentUser^.Next;
  end;
end;

function GenerateCommunitiesReport: String;
var
  Report: String;
  CurrentCommunity: PCommunity;
  CurrentUser: PUserCommunity;
  Count, UserCount: Integer;
begin
  Report := 'REPORTE DE COMUNIDADES' + LineEnding;
  Report := Report + '=====================' + LineEnding + LineEnding;

  Count := 0;
  CurrentCommunity := CommunityList.GetFirst;

  if CurrentCommunity = nil then
  begin
    Report := Report + 'No hay comunidades registradas.' + LineEnding;
  end
  else
  begin
    while CurrentCommunity <> nil do
    begin
      Inc(Count);
      Report := Report + 'Comunidad #' + IntToStr(Count) + LineEnding;
      Report := Report + 'ID: ' + IntToStr(CurrentCommunity^.Id) + LineEnding;
      Report := Report + 'Nombre: ' + CurrentCommunity^.Nombre + LineEnding;

      // Contar usuarios en la comunidad
      UserCount := 0;
      CurrentUser := CurrentCommunity^.Users;
      while CurrentUser <> nil do
      begin
        Inc(UserCount);
        CurrentUser := CurrentUser^.Next;
      end;

      Report := Report + 'Número de usuarios: ' + IntToStr(UserCount) + LineEnding;

      // Listar usuarios
      if UserCount > 0 then
      begin
        Report := Report + 'Usuarios:' + LineEnding;
        CurrentUser := CurrentCommunity^.Users;
        while CurrentUser <> nil do
        begin
          Report := Report + '  - ' + CurrentUser^.Email + LineEnding;
          CurrentUser := CurrentUser^.Next;
        end;
      end;

      Report := Report + '------------------------' + LineEnding;
      CurrentCommunity := CurrentCommunity^.Next;
    end;
  end;

  Report := Report + LineEnding + 'Total de comunidades: ' + IntToStr(CommunityList.GetCount);
  Result := Report;
end;

end.

