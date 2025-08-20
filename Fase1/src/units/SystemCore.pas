unit SystemCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataStructures, DateUtils;

type
  TSystemCore = class
  private
    FUsers: TUserList;
    FUserEmails: array of TEmailList; // Un inbox por usuario
    FUserContacts: array of TContactList; // Contactos por usuario
    FUserTrash: array of TEmailStack; // Papelera por usuario
    FScheduledEmails: TEmailQueue;
    FCommunities: TCommunityList;
    FEmailMatrix: TSparseMatrix;
    FCurrentUser: PUser;
    FEmailIdCounter: Integer;
    FUserIdCounter: Integer;
    FCommunityIdCounter: Integer;
    FContactIdCounter: Integer;

    procedure InitializeRootUser;
    function GetUserIndex(Email: String): Integer;
    procedure EnsureUserArrays(UserCount: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    // User Management
    function RegisterUser(Nombre, Usuario, Email, Telefono, Password: String): Boolean;
    function LoginUser(Email, Password: String): Boolean;
    procedure LogoutUser;
    function GetCurrentUser: PUser;
    function IsUserLoggedIn: Boolean;
    function IsRootUser: Boolean;

    // Email Management
    function SendEmail(Destinatario, Asunto, Mensaje: String): Boolean;
    function ScheduleEmail(Destinatario, Asunto, Mensaje: String; FechaHora: TDateTime): Boolean;
    function GetUserInbox(UserEmail: String): TEmailList;
    function GetUserTrash(UserEmail: String): TEmailStack;
    procedure DeleteEmail(EmailId: Integer);
    procedure MarkEmailAsRead(EmailId: Integer);
    function ProcessScheduledEmails: Integer; // Retorna cantidad procesada

    // Contact Management
    function AddContact(ContactEmail: String): Boolean;
    function GetUserContacts(UserEmail: String): TContactList;
    function IsContact(UserEmail, ContactEmail: String): Boolean;

    // Community Management (Root only)
    function CreateCommunity(Nombre: String): Boolean;
    function AddUserToCommunity(CommunityId: Integer; UserEmail: String): Boolean;
    function GetCommunities: TCommunityList;

    // File Operations
    function LoadUsersFromJSON(FileName: String): Boolean;

    // Profile Management
    function UpdateProfile(NewNombre, NewUsuario, NewTelefono: String): Boolean;

    // Reports and Matrix
    function GetEmailMatrix: TSparseMatrix;
    procedure UpdateEmailMatrix(Remitente, Destinatario: String);

    // Statistics
    function GetUserCount: Integer;
    function GetTotalEmails: Integer;
    function GetScheduledEmailsQueue: TEmailQueue;

    // System Operations
    procedure InitializeSystem;
    function GetNextEmailId: Integer;
    function GetNextUserId: Integer;
    function GetNextCommunityId: Integer;
    function GetNextContactId: Integer;
  end;

var
  SystemCore: TSystemCore;

implementation

uses
  fpjson, jsonparser;

constructor TSystemCore.Create;
begin
  FUsers := TUserList.Create;
  FScheduledEmails := TEmailQueue.Create;
  FCommunities := TCommunityList.Create;
  FEmailMatrix := TSparseMatrix.Create(1000); // Matriz 1000x1000
  FCurrentUser := nil;
  FEmailIdCounter := 1;
  FUserIdCounter := 1;
  FCommunityIdCounter := 1;
  FContactIdCounter := 1;

  SetLength(FUserEmails, 0);
  SetLength(FUserContacts, 0);
  SetLength(FUserTrash, 0);

  InitializeRootUser;
end;

destructor TSystemCore.Destroy;
var
  i: Integer;
begin
  // Limpiar arrays de usuarios
  for i := 0 to High(FUserEmails) do
  begin
    if FUserEmails[i] <> nil then
      FUserEmails[i].Free;
    if FUserContacts[i] <> nil then
      FUserContacts[i].Free;
    if FUserTrash[i] <> nil then
      FUserTrash[i].Free;
  end;

  FUsers.Free;
  FScheduledEmails.Free;
  FCommunities.Free;
  FEmailMatrix.Free;
  inherited;
end;

procedure TSystemCore.InitializeRootUser;
begin
  RegisterUser('Root Administrator', 'root', 'root@edd.com', '00000000', 'root123');
end;

function TSystemCore.GetUserIndex(Email: String): Integer;
var
  User: PUser;
  Current: PUser;
  Index: Integer;
begin
  Result := -1;
  Current := FUsers.GetFirst;
  Index := 0;

  while Current <> nil do
  begin
    if Current^.Email = Email then
    begin
      Result := Index;
      Exit;
    end;
    Current := Current^.Next;
    Inc(Index);
  end;
end;

procedure TSystemCore.EnsureUserArrays(UserCount: Integer);
var
  OldLength, i: Integer;
begin
  OldLength := Length(FUserEmails);
  if UserCount > OldLength then
  begin
    SetLength(FUserEmails, UserCount);
    SetLength(FUserContacts, UserCount);
    SetLength(FUserTrash, UserCount);

    for i := OldLength to UserCount - 1 do
    begin
      FUserEmails[i] := TEmailList.Create;
      FUserContacts[i] := TContactList.Create;
      FUserTrash[i] := TEmailStack.Create;
    end;
  end;
end;

function TSystemCore.RegisterUser(Nombre, Usuario, Email, Telefono, Password: String): Boolean;
begin
  Result := False;

  // Verificar si el usuario ya existe
  if FUsers.Find(Email) <> nil then
    Exit;

  // Crear el usuario
  FUsers.Add(FUserIdCounter, Nombre, Usuario, Email, Telefono, Password);
  Inc(FUserIdCounter);

  // Asegurar que los arrays tengan espacio suficiente
  EnsureUserArrays(FUsers.GetCount);

  Result := True;
end;

function TSystemCore.LoginUser(Email, Password: String): Boolean;
begin
  FCurrentUser := FUsers.Login(Email, Password);
  Result := FCurrentUser <> nil;
end;

procedure TSystemCore.LogoutUser;
begin
  FCurrentUser := nil;
end;

function TSystemCore.GetCurrentUser: PUser;
begin
  Result := FCurrentUser;
end;

function TSystemCore.IsUserLoggedIn: Boolean;
begin
  Result := FCurrentUser <> nil;
end;

function TSystemCore.IsRootUser: Boolean;
begin
  Result := (FCurrentUser <> nil) and (FCurrentUser^.Email = 'root@edd.com');
end;

function TSystemCore.SendEmail(Destinatario, Asunto, Mensaje: String): Boolean;
var
  DestIndex: Integer;
begin
  Result := False;

  if not IsUserLoggedIn then Exit;

  // Verificar que el destinatario existe
  if FUsers.Find(Destinatario) = nil then Exit;

  // Verificar que es un contacto (excepto para root)
  if not IsRootUser then
  begin
    if not IsContact(FCurrentUser^.Email, Destinatario) then Exit;
  end;

  // Obtener índice del destinatario
  DestIndex := GetUserIndex(Destinatario);
  if DestIndex = -1 then Exit;

  // Agregar email al inbox del destinatario
  FUserEmails[DestIndex].Add(FEmailIdCounter, FCurrentUser^.Email, Destinatario,
                             Asunto, Mensaje, False, Now);

  // Actualizar matriz de relaciones
  UpdateEmailMatrix(FCurrentUser^.Email, Destinatario);

  Inc(FEmailIdCounter);
  Result := True;
end;

function TSystemCore.ScheduleEmail(Destinatario, Asunto, Mensaje: String; FechaHora: TDateTime): Boolean;
begin
  Result := False;

  if not IsUserLoggedIn then Exit;

  // Verificar que el destinatario existe
  if FUsers.Find(Destinatario) = nil then Exit;

  // Verificar que es un contacto (excepto para root)
  if not IsRootUser then
  begin
    if not IsContact(FCurrentUser^.Email, Destinatario) then Exit;
  end;

  // Agregar a la cola de programados
  FScheduledEmails.Enqueue(FEmailIdCounter, FCurrentUser^.Email, Destinatario,
                          Asunto, Mensaje, FechaHora);

  Inc(FEmailIdCounter);
  Result := True;
end;

function TSystemCore.GetUserInbox(UserEmail: String): TEmailList;
var
  Index: Integer;
begin
  Index := GetUserIndex(UserEmail);
  if (Index >= 0) and (Index < Length(FUserEmails)) then
    Result := FUserEmails[Index]
  else
    Result := nil;
end;

function TSystemCore.GetUserTrash(UserEmail: String): TEmailStack;
var
  Index: Integer;
begin
  Index := GetUserIndex(UserEmail);
  if (Index >= 0) and (Index < Length(FUserTrash)) then
    Result := FUserTrash[Index]
  else
    Result := nil;
end;

procedure TSystemCore.DeleteEmail(EmailId: Integer);
var
  UserIndex: Integer;
  Email: PEmail;
begin
  if not IsUserLoggedIn then Exit;

  UserIndex := GetUserIndex(FCurrentUser^.Email);
  if UserIndex = -1 then Exit;

  Email := FUserEmails[UserIndex].Find(EmailId);
  if Email <> nil then
  begin
    // Remover de la lista de emails
    FUserEmails[UserIndex].Remove(Email);

    // Agregar a la papelera
    FUserTrash[UserIndex].Push(Email);
  end;
end;

procedure TSystemCore.MarkEmailAsRead(EmailId: Integer);
var
  UserIndex: Integer;
  Email: PEmail;
begin
  if not IsUserLoggedIn then Exit;

  UserIndex := GetUserIndex(FCurrentUser^.Email);
  if UserIndex = -1 then Exit;

  Email := FUserEmails[UserIndex].Find(EmailId);
  if Email <> nil then
    Email^.Estado := 'L';
end;

function TSystemCore.ProcessScheduledEmails: Integer;
var
  Email: PEmail;
  DestIndex: Integer;
  Count: Integer;
begin
  Count := 0;

  while not FScheduledEmails.IsEmpty do
  begin
    Email := FScheduledEmails.Peek;

    // Verificar si es hora de enviar
    if Email^.Fecha <= Now then
    begin
      Email := FScheduledEmails.Dequeue;

      // Enviar el email
      DestIndex := GetUserIndex(Email^.Destinatario);
      if DestIndex >= 0 then
      begin
        Email^.Estado := 'NL'; // Cambiar estado a No Leído
        Email^.Programado := False;
        FUserEmails[DestIndex].Add(Email^.Id, Email^.Remitente, Email^.Destinatario,
                                  Email^.Asunto, Email^.Mensaje, False, Email^.Fecha);

        // Actualizar matriz
        UpdateEmailMatrix(Email^.Remitente, Email^.Destinatario);
        Inc(Count);
      end;

      Dispose(Email);
    end
    else
      Break; // Los siguientes emails aún no es hora de enviarlos
  end;

  Result := Count;
end;

function TSystemCore.AddContact(ContactEmail: String): Boolean;
var
  UserIndex: Integer;
  ContactUser: PUser;
begin
  Result := False;

  if not IsUserLoggedIn then Exit;

  // Verificar que el contacto existe
  ContactUser := FUsers.Find(ContactEmail);
  if ContactUser = nil then Exit;

  // Verificar que no sea el mismo usuario
  if ContactEmail = FCurrentUser^.Email then Exit;

  UserIndex := GetUserIndex(FCurrentUser^.Email);
  if UserIndex = -1 then Exit;

  // Verificar que no esté ya agregado
  if FUserContacts[UserIndex].Find(ContactEmail) <> nil then Exit;

  // Agregar contacto
  FUserContacts[UserIndex].Add(FContactIdCounter, ContactUser^.Nombre,
                              ContactUser^.Usuario, ContactUser^.Email,
                              ContactUser^.Telefono);
  Inc(FContactIdCounter);
  Result := True;
end;

function TSystemCore.GetUserContacts(UserEmail: String): TContactList;
var
  Index: Integer;
begin
  Index := GetUserIndex(UserEmail);
  if (Index >= 0) and (Index < Length(FUserContacts)) then
    Result := FUserContacts[Index]
  else
    Result := nil;
end;

function TSystemCore.IsContact(UserEmail, ContactEmail: String): Boolean;
var
  UserIndex: Integer;
begin
  Result := False;
  UserIndex := GetUserIndex(UserEmail);
  if (UserIndex >= 0) and (UserIndex < Length(FUserContacts)) then
    Result := FUserContacts[UserIndex].Find(ContactEmail) <> nil;
end;

function TSystemCore.CreateCommunity(Nombre: String): Boolean;
begin
  Result := False;
  if not IsRootUser then Exit;

  FCommunities.Add(FCommunityIdCounter, Nombre);
  Inc(FCommunityIdCounter);
  Result := True;
end;

function TSystemCore.AddUserToCommunity(CommunityId: Integer; UserEmail: String): Boolean;
begin
  Result := False;
  if not IsRootUser then Exit;

  // Verificar que el usuario existe
  if FUsers.Find(UserEmail) = nil then Exit;

  FCommunities.AddUserToCommunity(CommunityId, UserEmail);
  Result := True;
end;

function TSystemCore.GetCommunities: TCommunityList;
begin
  Result := FCommunities;
end;

function TSystemCore.LoadUsersFromJSON(FileName: String): Boolean;
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  i: Integer;
  Nombre, Usuario, Email, Telefono: String;
  Id: Integer;
begin
  Result := False;

  if not IsRootUser then Exit;
  if not FileExists(FileName) then Exit;

  try
    JSONData := GetJSON(TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite));
    try
      if JSONData.JSONType = jtObject then
      begin
        JSONObject := TJSONObject(JSONData);
        if JSONObject.Find('usuarios') <> nil then
        begin
          JSONArray := JSONObject.Arrays['usuarios'];
          for i := 0 to JSONArray.Count - 1 do
          begin
            JSONObject := JSONArray.Objects[i];
            Id := JSONObject.Get('id', 0);
            Nombre := JSONObject.Get('nombre', '');
            Usuario := JSONObject.Get('usuario', '');
            Email := JSONObject.Get('email', '');
            Telefono := JSONObject.Get('telefono', '');

            if (Email <> '') and (FUsers.Find(Email) = nil) then
            begin
              RegisterUser(Nombre, Usuario, Email, Telefono, 'defaultpass');
            end;
          end;
          Result := True;
        end;
      end;
    finally
      JSONData.Free;
    end;
  except
    Result := False;
  end;
end;

function TSystemCore.UpdateProfile(NewNombre, NewUsuario, NewTelefono: String): Boolean;
begin
  Result := False;
  if not IsUserLoggedIn then Exit;

  FUsers.UpdateUser(FCurrentUser^.Email, NewNombre, NewUsuario, NewTelefono);

  // Actualizar puntero actual
  FCurrentUser := FUsers.Find(FCurrentUser^.Email);
  Result := True;
end;

function TSystemCore.GetEmailMatrix: TSparseMatrix;
begin
  Result := FEmailMatrix;
end;

procedure TSystemCore.UpdateEmailMatrix(Remitente, Destinatario: String);
var
  RemIndex, DestIndex: Integer;
  RemUser, DestUser: PUser;
begin
  RemUser := FUsers.Find(Remitente);
  DestUser := FUsers.Find(Destinatario);

  if (RemUser <> nil) and (DestUser <> nil) then
  begin
    RemIndex := RemUser^.Id;
    DestIndex := DestUser^.Id;

    if (RemIndex < 1000) and (DestIndex < 1000) then
      FEmailMatrix.IncrementValue(RemIndex, DestIndex);
  end;
end;

function TSystemCore.GetUserCount: Integer;
begin
  Result := FUsers.GetCount;
end;

function TSystemCore.GetTotalEmails: Integer;
var
  i, Total: Integer;
begin
  Total := 0;
  for i := 0 to High(FUserEmails) do
  begin
    if FUserEmails[i] <> nil then
      Total := Total + FUserEmails[i].GetCount;
  end;
  Result := Total;
end;

function TSystemCore.GetScheduledEmailsQueue: TEmailQueue;
begin
  Result := FScheduledEmails;
end;

procedure TSystemCore.InitializeSystem;
begin
  // Método para inicializar datos de prueba si es necesario
end;

function TSystemCore.GetNextEmailId: Integer;
begin
  Result := FEmailIdCounter;
  Inc(FEmailIdCounter);
end;

function TSystemCore.GetNextUserId: Integer;
begin
  Result := FUserIdCounter;
  Inc(FUserIdCounter);
end;

function TSystemCore.GetNextCommunityId: Integer;
begin
  Result := FCommunityIdCounter;
  Inc(FCommunityIdCounter);
end;

function TSystemCore.GetNextContactId: Integer;
begin
  Result := FContactIdCounter;
  Inc(FContactIdCounter);
end;

initialization
  SystemCore := TSystemCore.Create;

finalization
  SystemCore.Free;
