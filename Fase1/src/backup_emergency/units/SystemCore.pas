unit SystemCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, DataStructures;

var
  // Estructuras globales del sistema
  UserList: TUserList;
  CommunityList: TCommunityList;
  EmailRelationMatrix: TSparseMatrix;

  // Usuario actual logueado
  CurrentUser: PUser;

  // Contadores globales
  NextEmailId: Integer;
  NextUserId: Integer;
  NextCommunityId: Integer;
  NextContactId: Integer;

// Funciones principales del sistema
procedure Initialize;
procedure Finalize;
function IsUserLoggedIn: Boolean;
function IsRootUser: Boolean;
procedure LogoutUser;
function GetUserEmailsReceived(UserEmail: String): TEmailList;
function GetUserEmailsDeleted(UserEmail: String): TEmailStack;
function GetUserEmailsScheduled(UserEmail: String): TEmailQueue;
function GetUserContacts(UserEmail: String): TContactList;

implementation

procedure Initialize;
begin
  // Inicializar estructuras
  UserList := TUserList.Create;
  CommunityList := TCommunityList.Create;
  EmailRelationMatrix := TSparseMatrix.Create(1000); // Tamaño inicial de 1000x1000

  // Inicializar contadores
  NextEmailId := 1;
  NextUserId := 1;
  NextCommunityId := 1;
  NextContactId := 1;

  // Usuario actual
  CurrentUser := nil;

  // Crear usuario root por defecto
  UserList.Add(NextUserId, 'Administrador', 'root', 'root@edd.com', '00000000', 'root123');
  Inc(NextUserId);

  WriteLn('Sistema EDDMail inicializado correctamente');
end;

procedure Finalize;
begin
  if UserList <> nil then
    UserList.Free;
  if CommunityList <> nil then
    CommunityList.Free;
  if EmailRelationMatrix <> nil then
    EmailRelationMatrix.Free;

  WriteLn('Sistema EDDMail finalizado');
end;

function IsUserLoggedIn: Boolean;
begin
  Result := CurrentUser <> nil;
end;

function IsRootUser: Boolean;
begin
  Result := (CurrentUser <> nil) and (CurrentUser^.Email = 'root@edd.com');
end;

procedure LogoutUser;
begin
  CurrentUser := nil;
end;

function GetUserEmailsReceived(UserEmail: String): TEmailList;
var
  EmailsReceived: TEmailList;
  // Aquí deberías implementar la lógica para obtener emails del usuario
  // Por ahora retorna una lista vacía
begin
  EmailsReceived := TEmailList.Create;
  Result := EmailsReceived;
end;

function GetUserEmailsDeleted(UserEmail: String): TEmailStack;
var
  EmailsDeleted: TEmailStack;
begin
  EmailsDeleted := TEmailStack.Create;
  Result := EmailsDeleted;
end;

function GetUserEmailsScheduled(UserEmail: String): TEmailQueue;
var
  EmailsScheduled: TEmailQueue;
begin
  EmailsScheduled := TEmailQueue.Create;
  Result := EmailsScheduled;
end;

function GetUserContacts(UserEmail: String): TContactList;
var
  Contacts: TContactList;
begin
  Contacts := TContactList.Create;
  Result := Contacts;
end;

end.
