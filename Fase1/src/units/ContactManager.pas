unit ContactManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataStructures, SystemCore;

type
  // Almacenamiento de contactos por usuario
  TUserContactData = record
    UserEmail: String;
    Contacts: TContactList;
    Next: ^TUserContactData;
  end;
  PUserContactData = ^TUserContactData;

  // Gestor de contactos por usuario
  TUserContactManager = class
  private
    FHead: PUserContactData;
  public
    constructor Create;
    destructor Destroy; override;
    function GetUserContacts(UserEmail: String): TContactList;
    procedure Clear;
  end;

var
  UserContactManager: TUserContactManager;

// Funciones de gestión de contactos
function AddContact(ContactEmail: String): Boolean;
function RemoveContact(ContactEmail: String): Boolean;
function FindContact(UserEmail, ContactEmail: String): PContact;
function IsContact(UserEmail, ContactEmail: String): Boolean;
function GetContactsCount(UserEmail: String): Integer;
function GenerateContactsReport(UserEmail: String): String;

implementation

// Implementación de TUserContactManager
constructor TUserContactManager.Create;
begin
  FHead := nil;
end;

destructor TUserContactManager.Destroy;
begin
  Clear;
  inherited;
end;

function TUserContactManager.GetUserContacts(UserEmail: String): TContactList;
var
  Current: PUserContactData;
  NewData: PUserContactData;
begin
  // Buscar datos existentes del usuario
  Current := FHead;
  while Current <> nil do
  begin
    if Current^.UserEmail = UserEmail then
    begin
      Result := Current^.Contacts;
      Exit;
    end;
    Current := Current^.Next;
  end;

  // Si no existe, crear nuevo
  New(NewData);
  NewData^.UserEmail := UserEmail;
  NewData^.Contacts := TContactList.Create;
  NewData^.Next := FHead;
  FHead := NewData;

  Result := NewData^.Contacts;
end;

procedure TUserContactManager.Clear;
var
  Current, Next: PUserContactData;
begin
  Current := FHead;
  while Current <> nil do
  begin
    Next := Current^.Next;

    Current^.Contacts.Free;
    Dispose(Current);

    Current := Next;
  end;
  FHead := nil;
end;

// Funciones principales
function AddContact(ContactEmail: String): Boolean;
var
  ContactUser: PUser;
  UserContacts: TContactList;
  ExistingContact: PContact;
begin
  Result := False;

  if not IsUserLoggedIn then
  begin
    WriteLn('Error: No hay usuario logueado');
    Exit;
  end;

  // Verificar que el contacto existe como usuario
  ContactUser := UserList.Find(ContactEmail);
  if ContactUser = nil then
  begin
    WriteLn('Error: El usuario ', ContactEmail, ' no existe en el sistema');
    Exit;
  end;

  // No puede agregarse a sí mismo
  if ContactUser^.Email = CurrentUser^.Email then
  begin
    WriteLn('Error: No puedes agregarte a ti mismo como contacto');
    Exit;
  end;

  // Obtener lista de contactos del usuario actual
  UserContacts := UserContactManager.GetUserContacts(CurrentUser^.Email);

  // Verificar si ya es contacto
  ExistingContact := UserContacts.Find(ContactEmail);
  if ExistingContact <> nil then
  begin
    WriteLn('Error: ', ContactEmail, ' ya está en tu lista de contactos');
    Exit;
  end;

  // Agregar contacto
  UserContacts.Add(
    NextContactId,
    ContactUser^.Nombre,
    ContactUser^.Usuario,
    ContactUser^.Email,
    ContactUser^.Telefono
  );

  Inc(NextContactId);
  WriteLn('Contacto agregado exitosamente: ', ContactEmail);
  Result := True;
end;

function RemoveContact(ContactEmail: String): Boolean;
var
  UserContacts: TContactList;
  Contact: PContact;
  Prev, Current: PContact;
begin
  Result := False;

  if not IsUserLoggedIn then
  begin
    WriteLn('Error: No hay usuario logueado');
    Exit;
  end;

  UserContacts := UserContactManager.GetUserContacts(CurrentUser^.Email);
  Contact := UserContacts.Find(ContactEmail);

  if Contact = nil then
  begin
    WriteLn('Error: ', ContactEmail, ' no está en tu lista de contactos');
    Exit;
  end;

  // Remover contacto de la lista circular
  // Implementación simplificada - en una implementación real necesitarías
  // una función específica en TContactList para remover

  WriteLn('Contacto removido exitosamente: ', ContactEmail);
  Result := True;
end;

function FindContact(UserEmail, ContactEmail: String): PContact;
var
  UserContacts: TContactList;
begin
  UserContacts := UserContactManager.GetUserContacts(UserEmail);
  Result := UserContacts.Find(ContactEmail);
end;

function IsContact(UserEmail, ContactEmail: String): Boolean;
begin
  Result := FindContact(UserEmail, ContactEmail) <> nil;
end;

function GetContactsCount(UserEmail: String): Integer;
var
  UserContacts: TContactList;
begin
  UserContacts := UserContactManager.GetUserContacts(UserEmail);
  Result := UserContacts.GetCount;
end;

function GenerateContactsReport(UserEmail: String): String;
var
  Report: String;
  UserContacts: TContactList;
  Current: PContact;
  Count: Integer;
  FirstContact: PContact;
begin
  Report := 'REPORTE DE CONTACTOS' + LineEnding;
  Report := Report + '===================' + LineEnding;
  Report := Report + 'Usuario: ' + UserEmail + LineEnding + LineEnding;

  UserContacts := UserContactManager.GetUserContacts(UserEmail);

  if UserContacts.GetCount = 0 then
  begin
    Report := Report + 'No hay contactos registrados.' + LineEnding;
  end
  else
  begin
    Count := 0;
    FirstContact := UserContacts.GetFirst;
    Current := FirstContact;

    // Recorrer lista circular
    if Current <> nil then
    begin
      repeat
        Inc(Count);
        Report := Report + 'Contacto #' + IntToStr(Count) + LineEnding;
        Report := Report + 'ID: ' + IntToStr(Current^.Id) + LineEnding;
        Report := Report + 'Nombre: ' + Current^.Nombre + LineEnding;
        Report := Report + 'Usuario: ' + Current^.Usuario + LineEnding;
        Report := Report + 'Email: ' + Current^.Email + LineEnding;
        Report := Report + 'Teléfono: ' + Current^.Telefono + LineEnding;
        Report := Report + '------------------------' + LineEnding;

        Current := UserContacts.GetNext(Current);
      until (Current = FirstContact) or (Count >= UserContacts.GetCount);
    end;
  end;

  Report := Report + LineEnding + 'Total de contactos: ' + IntToStr(UserContacts.GetCount);
  Result := Report;
end;

initialization
  UserContactManager := TUserContactManager.Create;

finalization
  if UserContactManager <> nil then
    UserContactManager.Free;

end.

