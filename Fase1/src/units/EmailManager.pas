unit EmailManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, DataStructures, SystemCore;

type
  // Almacenamiento de emails por usuario
  TUserEmailData = record
    UserEmail: String;
    ReceivedEmails: TEmailList;
    DeletedEmails: TEmailStack;
    ScheduledEmails: TEmailQueue;
  end;
  PUserEmailData = ^TUserEmailData;

  // Lista de datos de email por usuario
  TUserEmailList = class
  private
    FHead: PUserEmailData;
  public
    constructor Create;
    destructor Destroy; override;
    function GetUserData(UserEmail: String): PUserEmailData;
    procedure Clear;
  end;

var
  UserEmailManager: TUserEmailList;

// Funciones de gestión de emails
function SendEmail(Destinatario, Asunto, Mensaje: String): Boolean;
function ScheduleEmail(Destinatario, Asunto, Mensaje: String; FechaEnvio: TDateTime): Boolean;
function DeleteEmail(EmailId: Integer): Boolean;
function MarkEmailAsRead(EmailId: Integer): Boolean;
function SearchInTrash(UserEmail, Keyword: String): PEmail;
function ProcessScheduledEmails: Integer;
function GetUserInbox(UserEmail: String): TEmailList;
function GetUserTrash(UserEmail: String): TEmailStack;
function GetUserScheduled(UserEmail: String): TEmailQueue;

implementation

// Implementación de TUserEmailList
constructor TUserEmailList.Create;
begin
  FHead := nil;
end;

destructor TUserEmailList.Destroy;
begin
  Clear;
  inherited;
end;

function TUserEmailList.GetUserData(UserEmail: String): PUserEmailData;
var
  Current: PUserEmailData;
  NewData: PUserEmailData;
begin
  // Buscar datos existentes del usuario
  Current := FHead;
  while Current <> nil do
  begin
    if Current^.UserEmail = UserEmail then
    begin
      Result := Current;
      Exit;
    end;
    Current := PUserEmailData(Current^.ReceivedEmails.GetFirst); // Usar como Next pointer
  end;

  // Si no existe, crear nuevo
  New(NewData);
  NewData^.UserEmail := UserEmail;
  NewData^.ReceivedEmails := TEmailList.Create;
  NewData^.DeletedEmails := TEmailStack.Create;
  NewData^.ScheduledEmails := TEmailQueue.Create;

  // Agregar al inicio de la lista (simplificado)
  FHead := NewData;

  Result := NewData;
end;

procedure TUserEmailList.Clear;
var
  Current, Next: PUserEmailData;
begin
  Current := FHead;
  while Current <> nil do
  begin
    Next := PUserEmailData(Current^.ReceivedEmails.GetFirst); // Usar como Next pointer

    Current^.ReceivedEmails.Free;
    Current^.DeletedEmails.Free;
    Current^.ScheduledEmails.Free;
    Dispose(Current);

    Current := Next;
  end;
  FHead := nil;
end;

// Funciones principales
function SendEmail(Destinatario, Asunto, Mensaje: String): Boolean;
var
  DestUser: PUser;
  UserData: PUserEmailData;
  SenderUser, ReceiverUser: PUser;
  SenderIndex, ReceiverIndex: Integer;
begin
  Result := False;

  if not IsUserLoggedIn then
  begin
    WriteLn('Error: No hay usuario logueado');
    Exit;
  end;

  // Verificar que el destinatario existe
  DestUser := UserList.Find(Destinatario);
  if DestUser = nil then
  begin
    WriteLn('Error: El destinatario no existe: ', Destinatario);
    Exit;
  end;

  // Obtener datos del destinatario
  UserData := UserEmailManager.GetUserData(Destinatario);

  // Crear y agregar email a la bandeja del destinatario
  UserData^.ReceivedEmails.Add(
    NextEmailId,
    CurrentUser^.Email,
    Destinatario,
    Asunto,
    Mensaje,
    False,
    Now
  );

  // Actualizar matriz de relaciones
  SenderUser := UserList.Find(CurrentUser^.Email);
  ReceiverUser := UserList.Find(Destinatario);

  if (SenderUser <> nil) and (ReceiverUser <> nil) then
  begin
    SenderIndex := SenderUser^.Id;
    ReceiverIndex := ReceiverUser^.Id;
    EmailRelationMatrix.IncrementValue(SenderIndex, ReceiverIndex);
  end;

  Inc(NextEmailId);
  WriteLn('Email enviado exitosamente a: ', Destinatario);
  Result := True;
end;

function ScheduleEmail(Destinatario, Asunto, Mensaje: String; FechaEnvio: TDateTime): Boolean;
var
  DestUser: PUser;
  UserData: PUserEmailData;
begin
  Result := False;

  if not IsUserLoggedIn then
  begin
    WriteLn('Error: No hay usuario logueado');
    Exit;
  end;

  // Verificar que el destinatario existe
  DestUser := UserList.Find(Destinatario);
  if DestUser = nil then
  begin
    WriteLn('Error: El destinatario no existe: ', Destinatario);
    Exit;
  end;

  // Obtener datos del usuario actual para emails programados
  UserData := UserEmailManager.GetUserData(CurrentUser^.Email);

  // Agregar a la cola de emails programados
  UserData^.ScheduledEmails.Enqueue(
    NextEmailId,
    CurrentUser^.Email,
    Destinatario,
    Asunto,
    Mensaje,
    FechaEnvio
  );

  Inc(NextEmailId);
  WriteLn('Email programado exitosamente para: ', DateTimeToStr(FechaEnvio));
  Result := True;
end;

function DeleteEmail(EmailId: Integer): Boolean;
var
  UserData: PUserEmailData;
  Email: PEmail;
begin
  Result := False;

  if not IsUserLoggedIn then
  begin
    WriteLn('Error: No hay usuario logueado');
    Exit;
  end;

  UserData := UserEmailManager.GetUserData(CurrentUser^.Email);

  // Buscar email en la bandeja de entrada
  Email := UserData^.ReceivedEmails.Find(EmailId);
  if Email <> nil then
  begin
    // Remover de la bandeja de entrada
    UserData^.ReceivedEmails.Remove(Email);

    // Agregar a la papelera
    UserData^.DeletedEmails.Push(Email);

    WriteLn('Email movido a la papelera');
    Result := True;
  end
  else
  begin
    WriteLn('Error: Email no encontrado');
  end;
end;

function MarkEmailAsRead(EmailId: Integer): Boolean;
var
  UserData: PUserEmailData;
  Email: PEmail;
begin
  Result := False;

  if not IsUserLoggedIn then
  begin
    WriteLn('Error: No hay usuario logueado');
    Exit;
  end;

  UserData := UserEmailManager.GetUserData(CurrentUser^.Email);
  Email := UserData^.ReceivedEmails.Find(EmailId);

  if Email <> nil then
  begin
    Email^.Estado := 'L'; // Marcar como leído
    WriteLn('Email marcado como leído');
    Result := True;
  end
  else
  begin
    WriteLn('Error: Email no encontrado');
  end;
end;

function SearchInTrash(UserEmail, Keyword: String): PEmail;
var
  UserData: PUserEmailData;
begin
  UserData := UserEmailManager.GetUserData(UserEmail);
  Result := UserData^.DeletedEmails.Search(Keyword);
end;

function ProcessScheduledEmails: Integer;
var
  Count: Integer;
  UserData: PUserEmailData;
  ScheduledEmail: PEmail;
  Current: PUserEmailData;
begin
  Count := 0;

  // Procesar emails programados de todos los usuarios
  Current := UserEmailManager.FHead;
  while Current <> nil do
  begin
    UserData := Current;

    // Procesar emails que ya deben ser enviados
    while not UserData^.ScheduledEmails.IsEmpty do
    begin
      ScheduledEmail := UserData^.ScheduledEmails.Peek;

      if ScheduledEmail^.Fecha <= Now then
      begin
        // Quitar de la cola
        ScheduledEmail := UserData^.ScheduledEmails.Dequeue;

        // Enviar el email (simular)
        WriteLn('Enviando email programado de ', ScheduledEmail^.Remitente, ' a ', ScheduledEmail^.Destinatario);

        // Agregar a la bandeja del destinatario
        UserData := UserEmailManager.GetUserData(ScheduledEmail^.Destinatario);
        UserData^.ReceivedEmails.Add(
          ScheduledEmail^.Id,
          ScheduledEmail^.Remitente,
          ScheduledEmail^.Destinatario,
          ScheduledEmail^.Asunto,
          ScheduledEmail^.Mensaje,
          False,
          Now
        );

        // Liberar memoria del email programado
        Dispose(ScheduledEmail);
        Inc(Count);
      end
      else
        Break; // Los siguientes emails aún no deben enviarse
    end;

    // Mover al siguiente usuario (simplificado)
    Current := nil; // En una implementación real, habría un puntero Next
  end;

  Result := Count;
end;

function GetUserInbox(UserEmail: String): TEmailList;
var
  UserData: PUserEmailData;
begin
  UserData := UserEmailManager.GetUserData(UserEmail);
  Result := UserData^.ReceivedEmails;
end;

function GetUserTrash(UserEmail: String): TEmailStack;
var
  UserData: PUserEmailData;
begin
  UserData := UserEmailManager.GetUserData(UserEmail);
  Result := UserData^.DeletedEmails;
end;

function GetUserScheduled(UserEmail: String): TEmailQueue;
var
  UserData: PUserEmailData;
begin
  UserData := UserEmailManager.GetUserData(UserEmail);
  Result := UserData^.ScheduledEmails;
end;

initialization
  UserEmailManager := TUserEmailList.Create;

finalization
  if UserEmailManager <> nil then
    UserEmailManager.Free;

end.

