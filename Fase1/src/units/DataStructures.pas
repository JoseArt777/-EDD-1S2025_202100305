unit DataStructures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  // Forward declarations
  PUser = ^TUser;
  PEmail = ^TEmail;
  PContact = ^TContact;
  PCommunity = ^TCommunity;
  PUserCommunity = ^TUserCommunity;
  PSparseNode = ^TSparseNode;

  // Usuario
  TUser = record
    Id: Integer;
    Nombre: String;
    Usuario: String;
    Email: String;
    Telefono: String;
    Password: String;
    Next: PUser;
  end;

  // Email
  TEmail = record
    Id: Integer;
    Remitente: String;
    Destinatario: String;
    Estado: String; // 'NL' = No Leído, 'L' = Leído
    Programado: Boolean;
    Asunto: String;
    Fecha: TDateTime;
    Mensaje: String;
    Next: PEmail;
    Prev: PEmail;
  end;

  // Contacto (Lista Circular)
  TContact = record
    Id: Integer;
    Nombre: String;
    Usuario: String;
    Email: String;
    Telefono: String;
    Next: PContact;
  end;

  // Comunidad
  TCommunity = record
    Id: Integer;
    Nombre: String;
    Next: PCommunity;
    Users: PUserCommunity; // Lista de usuarios en la comunidad
  end;

  // Usuario en comunidad
  TUserCommunity = record
    Email: String;
    Next: PUserCommunity;
  end;

  // Nodo para matriz dispersa
  TSparseNode = record
    Row: Integer;
    Col: Integer;
    Value: Integer;
    NextRow: PSparseNode;
    NextCol: PSparseNode;
  end;

  // Lista Simple de Usuarios
  TUserList = class
  private
    FHead: PUser;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Id: Integer; Nombre, Usuario, Email, Telefono, Password: String);
    function Find(Email: String): PUser;
    function FindById(Id: Integer): PUser;
    function Login(Email, Password: String): PUser;
    procedure UpdateUser(Email, NewNombre, NewUsuario, NewTelefono: String);
    function GetCount: Integer;
    function GetFirst: PUser;
    procedure Clear;
  end;

  // Lista Doblemente Enlazada de Emails
  TEmailList = class
  private
    FHead: PEmail;
    FTail: PEmail;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Id: Integer; Remitente, Destinatario, Asunto, Mensaje: String;
                  Programado: Boolean = False; Fecha: TDateTime = 0);
    procedure Remove(Email: PEmail);
    function Find(Id: Integer): PEmail;
    procedure SortBySubject;
    function GetUnreadCount: Integer;
    function GetFirst: PEmail;
    function GetCount: Integer;
    procedure Clear;
  end;

  // Lista Circular de Contactos
  TContactList = class
  private
    FHead: PContact;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Id: Integer; Nombre, Usuario, Email, Telefono: String);
    function Find(Email: String): PContact;
    function GetNext(Current: PContact): PContact;
    function GetPrev(Current: PContact): PContact;
    function GetFirst: PContact;
    function GetCount: Integer;
    procedure Clear;
  end;

  // Cola de Emails Programados
  TEmailQueue = class
  private
    FHead: PEmail;
    FTail: PEmail;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(Id: Integer; Remitente, Destinatario, Asunto, Mensaje: String; Fecha: TDateTime);
    function Dequeue: PEmail;
    function IsEmpty: Boolean;
    function Peek: PEmail;
    function GetCount: Integer;
    function GetFirst: PEmail;
    procedure Clear;
  end;

  // Pila de Emails Eliminados
  TEmailStack = class
  private
    FTop: PEmail;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(Email: PEmail);
    function Pop: PEmail;
    function IsEmpty: Boolean;
    function Peek: PEmail;
    function Search(Keyword: String): PEmail;
    function GetCount: Integer;
    function GetTop: PEmail;
    procedure Clear;
  end;

  // Lista de Comunidades
  TCommunityList = class
  private
    FHead: PCommunity;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Id: Integer; Nombre: String);
    function Find(Id: Integer): PCommunity;
    procedure AddUserToCommunity(CommunityId: Integer; UserEmail: String);
    function GetFirst: PCommunity;
    function GetCount: Integer;
    procedure Clear;
  end;

  // Matriz Dispersa para relaciones emisor-receptor
  TSparseMatrix = class
  private
    FHead: PSparseNode;
    FRowHeaders: array of PSparseNode;
    FColHeaders: array of PSparseNode;
    FSize: Integer;
  public
    constructor Create(Size: Integer);
    destructor Destroy; override;
    procedure SetValue(Row, Col, Value: Integer);
    function GetValue(Row, Col: Integer): Integer;
    procedure IncrementValue(Row, Col: Integer);
    procedure Clear;
  end;

implementation

// ============================================================================
// TUserList Implementation
// ============================================================================

constructor TUserList.Create;
begin
  FHead := nil;
  FCount := 0;
end;

destructor TUserList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TUserList.Add(Id: Integer; Nombre, Usuario, Email, Telefono, Password: String);
var
  NewUser: PUser;
begin
  New(NewUser);
  NewUser^.Id := Id;
  NewUser^.Nombre := Nombre;
  NewUser^.Usuario := Usuario;
  NewUser^.Email := Email;
  NewUser^.Telefono := Telefono;
  NewUser^.Password := Password;
  NewUser^.Next := FHead;
  FHead := NewUser;
  Inc(FCount);
end;

function TUserList.Find(Email: String): PUser;
var
  Current: PUser;
begin
  Current := FHead;
  while (Current <> nil) and (Current^.Email <> Email) do
    Current := Current^.Next;
  Result := Current;
end;

function TUserList.FindById(Id: Integer): PUser;
var
  Current: PUser;
begin
  Current := FHead;
  while (Current <> nil) and (Current^.Id <> Id) do
    Current := Current^.Next;
  Result := Current;
end;

function TUserList.Login(Email, Password: String): PUser;
var
  User: PUser;
begin
  User := Find(Email);
  if (User <> nil) and (User^.Password = Password) then
    Result := User
  else
    Result := nil;
end;

procedure TUserList.UpdateUser(Email, NewNombre, NewUsuario, NewTelefono: String);
var
  User: PUser;
begin
  User := Find(Email);
  if User <> nil then
  begin
    User^.Nombre := NewNombre;
    User^.Usuario := NewUsuario;
    User^.Telefono := NewTelefono;
  end;
end;

function TUserList.GetCount: Integer;
begin
  Result := FCount;
end;

function TUserList.GetFirst: PUser;
begin
  Result := FHead;
end;

procedure TUserList.Clear;
var
  Current, Next: PUser;
begin
  Current := FHead;
  while Current <> nil do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;
  FHead := nil;
  FCount := 0;
end;

// ============================================================================
// TEmailList Implementation
// ============================================================================

constructor TEmailList.Create;
begin
  FHead := nil;
  FTail := nil;
  FCount := 0;
end;

destructor TEmailList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TEmailList.Add(Id: Integer; Remitente, Destinatario, Asunto, Mensaje: String;
                        Programado: Boolean; Fecha: TDateTime);
var
  NewEmail: PEmail;
begin
  New(NewEmail);
  NewEmail^.Id := Id;
  NewEmail^.Remitente := Remitente;
  NewEmail^.Destinatario := Destinatario;
  NewEmail^.Estado := 'NL'; // No Leído por defecto
  NewEmail^.Programado := Programado;
  NewEmail^.Asunto := Asunto;
  if Fecha = 0 then
    NewEmail^.Fecha := Now
  else
    NewEmail^.Fecha := Fecha;
  NewEmail^.Mensaje := Mensaje;
  NewEmail^.Next := nil;
  NewEmail^.Prev := FTail;

  if FTail <> nil then
    FTail^.Next := NewEmail
  else
    FHead := NewEmail;

  FTail := NewEmail;
  Inc(FCount);
end;

procedure TEmailList.Remove(Email: PEmail);
begin
  if Email = nil then Exit;

  if Email^.Prev <> nil then
    Email^.Prev^.Next := Email^.Next
  else
    FHead := Email^.Next;

  if Email^.Next <> nil then
    Email^.Next^.Prev := Email^.Prev
  else
    FTail := Email^.Prev;

  Dec(FCount);
end;

function TEmailList.Find(Id: Integer): PEmail;
var
  Current: PEmail;
begin
  Current := FHead;
  while (Current <> nil) and (Current^.Id <> Id) do
    Current := Current^.Next;
  Result := Current;
end;

procedure TEmailList.SortBySubject;
var
  i, j: Integer;
  Current, Next: PEmail;
  TempEmail: TEmail;
begin
  if FHead = nil then Exit;

  Current := FHead;
  while Current^.Next <> nil do
  begin
    Next := Current^.Next;
    while Next <> nil do
    begin
      if Current^.Asunto > Next^.Asunto then
      begin
        // Intercambiar contenido (excepto punteros)
        TempEmail.Id := Current^.Id;
        TempEmail.Remitente := Current^.Remitente;
        TempEmail.Destinatario := Current^.Destinatario;
        TempEmail.Estado := Current^.Estado;
        TempEmail.Programado := Current^.Programado;
        TempEmail.Asunto := Current^.Asunto;
        TempEmail.Fecha := Current^.Fecha;
        TempEmail.Mensaje := Current^.Mensaje;

        Current^.Id := Next^.Id;
        Current^.Remitente := Next^.Remitente;
        Current^.Destinatario := Next^.Destinatario;
        Current^.Estado := Next^.Estado;
        Current^.Programado := Next^.Programado;
        Current^.Asunto := Next^.Asunto;
        Current^.Fecha := Next^.Fecha;
        Current^.Mensaje := Next^.Mensaje;

        Next^.Id := TempEmail.Id;
        Next^.Remitente := TempEmail.Remitente;
        Next^.Destinatario := TempEmail.Destinatario;
        Next^.Estado := TempEmail.Estado;
        Next^.Programado := TempEmail.Programado;
        Next^.Asunto := TempEmail.Asunto;
        Next^.Fecha := TempEmail.Fecha;
        Next^.Mensaje := TempEmail.Mensaje;
      end;
      Next := Next^.Next;
    end;
    Current := Current^.Next;
  end;
end;

function TEmailList.GetUnreadCount: Integer;
var
  Current: PEmail;
  Count: Integer;
begin
  Count := 0;
  Current := FHead;
  while Current <> nil do
  begin
    if Current^.Estado = 'NL' then
      Inc(Count);
    Current := Current^.Next;
  end;
  Result := Count;
end;

function TEmailList.GetFirst: PEmail;
begin
  Result := FHead;
end;

function TEmailList.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TEmailList.Clear;
var
  Current, Next: PEmail;
begin
  Current := FHead;
  while Current <> nil do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;
  FHead := nil;
  FTail := nil;
  FCount := 0;
end;

// ============================================================================
// TContactList Implementation
// ============================================================================

constructor TContactList.Create;
begin
  FHead := nil;
  FCount := 0;
end;

destructor TContactList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TContactList.Add(Id: Integer; Nombre, Usuario, Email, Telefono: String);
var
  NewContact: PContact;
begin
  New(NewContact);
  NewContact^.Id := Id;
  NewContact^.Nombre := Nombre;
  NewContact^.Usuario := Usuario;
  NewContact^.Email := Email;
  NewContact^.Telefono := Telefono;

  if FHead = nil then
  begin
    NewContact^.Next := NewContact; // Apunta a sí mismo
    FHead := NewContact;
  end
  else
  begin
    NewContact^.Next := FHead^.Next;
    FHead^.Next := NewContact;
  end;

  Inc(FCount);
end;

function TContactList.Find(Email: String): PContact;
var
  Current: PContact;
  Count: Integer;
begin
  if FHead = nil then
  begin
    Result := nil;
    Exit;
  end;

  Current := FHead;
  Count := 0;
  repeat
    if Current^.Email = Email then
    begin
      Result := Current;
      Exit;
    end;
    Current := Current^.Next;
    Inc(Count);
  until (Current = FHead) or (Count >= FCount);

  Result := nil;
end;

function TContactList.GetNext(Current: PContact): PContact;
begin
  if Current <> nil then
    Result := Current^.Next
  else
    Result := FHead;
end;

function TContactList.GetPrev(Current: PContact): PContact;
var
  Prev: PContact;
begin
  if (Current = nil) or (FHead = nil) then
  begin
    Result := nil;
    Exit;
  end;

  Prev := FHead;
  while (Prev^.Next <> Current) and (Prev^.Next <> FHead) do
    Prev := Prev^.Next;

  Result := Prev;
end;

function TContactList.GetFirst: PContact;
begin
  Result := FHead;
end;

function TContactList.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TContactList.Clear;
var
  Current, Next: PContact;
  Count: Integer;
begin
  if FHead = nil then Exit;

  Current := FHead^.Next;
  Count := 0;
  while (Current <> FHead) and (Count < FCount) do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
    Inc(Count);
  end;

  Dispose(FHead);
  FHead := nil;
  FCount := 0;
end;

// ============================================================================
// TEmailQueue Implementation
// ============================================================================

constructor TEmailQueue.Create;
begin
  FHead := nil;
  FTail := nil;
  FCount := 0;
end;

destructor TEmailQueue.Destroy;
begin
  Clear;
  inherited;
end;

procedure TEmailQueue.Enqueue(Id: Integer; Remitente, Destinatario, Asunto, Mensaje: String; Fecha: TDateTime);
var
  NewEmail: PEmail;
begin
  New(NewEmail);
  NewEmail^.Id := Id;
  NewEmail^.Remitente := Remitente;
  NewEmail^.Destinatario := Destinatario;
  NewEmail^.Estado := 'P'; // Programado
  NewEmail^.Programado := True;
  NewEmail^.Asunto := Asunto;
  NewEmail^.Fecha := Fecha;
  NewEmail^.Mensaje := Mensaje;
  NewEmail^.Next := nil;
  NewEmail^.Prev := nil;

  if FTail <> nil then
  begin
    FTail^.Next := NewEmail;
    NewEmail^.Prev := FTail;
  end
  else
    FHead := NewEmail;

  FTail := NewEmail;
  Inc(FCount);
end;

function TEmailQueue.Dequeue: PEmail;
begin
  Result := FHead;
  if FHead <> nil then
  begin
    FHead := FHead^.Next;
    if FHead <> nil then
      FHead^.Prev := nil
    else
      FTail := nil;
    Dec(FCount);
    Result^.Next := nil;
    Result^.Prev := nil;
  end;
end;

function TEmailQueue.IsEmpty: Boolean;
begin
  Result := FHead = nil;
end;

function TEmailQueue.Peek: PEmail;
begin
  Result := FHead;
end;

function TEmailQueue.GetCount: Integer;
begin
  Result := FCount;
end;

function TEmailQueue.GetFirst: PEmail;
begin
  Result := FHead;
end;

procedure TEmailQueue.Clear;
var
  Current, Next: PEmail;
begin
  Current := FHead;
  while Current <> nil do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;
  FHead := nil;
  FTail := nil;
  FCount := 0;
end;

// ============================================================================
// TEmailStack Implementation
// ============================================================================

constructor TEmailStack.Create;
begin
  FTop := nil;
  FCount := 0;
end;

destructor TEmailStack.Destroy;
begin
  Clear;
  inherited;
end;

procedure TEmailStack.Push(Email: PEmail);
begin
  if Email <> nil then
  begin
    Email^.Next := FTop;
    Email^.Prev := nil;
    if FTop <> nil then
      FTop^.Prev := Email;
    FTop := Email;
    Inc(FCount);
  end;
end;

function TEmailStack.Pop: PEmail;
begin
  Result := FTop;
  if FTop <> nil then
  begin
    FTop := FTop^.Next;
    if FTop <> nil then
      FTop^.Prev := nil;
    Dec(FCount);
    Result^.Next := nil;
    Result^.Prev := nil;
  end;
end;

function TEmailStack.IsEmpty: Boolean;
begin
  Result := FTop = nil;
end;

function TEmailStack.Peek: PEmail;
begin
  Result := FTop;
end;

function TEmailStack.Search(Keyword: String): PEmail;
var
  Current: PEmail;
begin
  Current := FTop;
  while Current <> nil do
  begin
    if Pos(LowerCase(Keyword), LowerCase(Current^.Asunto)) > 0 then
    begin
      Result := Current;
      Exit;
    end;
    Current := Current^.Next;
  end;
  Result := nil;
end;

function TEmailStack.GetCount: Integer;
begin
  Result := FCount;
end;

function TEmailStack.GetTop: PEmail;
begin
  Result := FTop;
end;

procedure TEmailStack.Clear;
var
  Current, Next: PEmail;
begin
  Current := FTop;
  while Current <> nil do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;
  FTop := nil;
  FCount := 0;
end;

// ============================================================================
// TCommunityList Implementation
// ============================================================================

constructor TCommunityList.Create;
begin
  FHead := nil;
  FCount := 0;
end;

destructor TCommunityList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TCommunityList.Add(Id: Integer; Nombre: String);
var
  NewCommunity: PCommunity;
begin
  New(NewCommunity);
  NewCommunity^.Id := Id;
  NewCommunity^.Nombre := Nombre;
  NewCommunity^.Next := FHead;
  NewCommunity^.Users := nil;
  FHead := NewCommunity;
  Inc(FCount);
end;

function TCommunityList.Find(Id: Integer): PCommunity;
var
  Current: PCommunity;
begin
  Current := FHead;
  while (Current <> nil) and (Current^.Id <> Id) do
    Current := Current^.Next;
  Result := Current;
end;

procedure TCommunityList.AddUserToCommunity(CommunityId: Integer; UserEmail: String);
var
  Community: PCommunity;
  NewUserCommunity: PUserCommunity;
begin
  Community := Find(CommunityId);
  if Community <> nil then
  begin
    New(NewUserCommunity);
    NewUserCommunity^.Email := UserEmail;
    NewUserCommunity^.Next := Community^.Users;
    Community^.Users := NewUserCommunity;
  end;
end;

function TCommunityList.GetFirst: PCommunity;
begin
  Result := FHead;
end;

function TCommunityList.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TCommunityList.Clear;
var
  CurrentCommunity, NextCommunity: PCommunity;
  CurrentUser, NextUser: PUserCommunity;
begin
  CurrentCommunity := FHead;
  while CurrentCommunity <> nil do
  begin
    NextCommunity := CurrentCommunity^.Next;

    // Limpiar usuarios de la comunidad
    CurrentUser := CurrentCommunity^.Users;
    while CurrentUser <> nil do
    begin
      NextUser := CurrentUser^.Next;
      Dispose(CurrentUser);
      CurrentUser := NextUser;
    end;

    Dispose(CurrentCommunity);
    CurrentCommunity := NextCommunity;
  end;
  FHead := nil;
  FCount := 0;
end;

// ============================================================================
// TSparseMatrix Implementation
// ============================================================================

constructor TSparseMatrix.Create(Size: Integer);
begin
  FSize := Size;
  SetLength(FRowHeaders, Size);
  SetLength(FColHeaders, Size);
  FHead := nil;
end;

destructor TSparseMatrix.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSparseMatrix.SetValue(Row, Col, Value: Integer);
var
  NewNode, Current, Prev: PSparseNode;
begin
  if (Row >= FSize) or (Col >= FSize) or (Row < 0) or (Col < 0) then Exit;

  New(NewNode);
  NewNode^.Row := Row;
  NewNode^.Col := Col;
  NewNode^.Value := Value;
  NewNode^.NextRow := nil;
  NewNode^.NextCol := nil;

  // Insertar en lista de fila
  if FRowHeaders[Row] = nil then
    FRowHeaders[Row] := NewNode
  else
  begin
    Current := FRowHeaders[Row];
    Prev := nil;
    while (Current <> nil) and (Current^.Col < Col) do
    begin
      Prev := Current;
      Current := Current^.NextRow;
    end;

    if Prev = nil then
    begin
      NewNode^.NextRow := FRowHeaders[Row];
      FRowHeaders[Row] := NewNode;
    end
    else
    begin
      NewNode^.NextRow := Current;
      Prev^.NextRow := NewNode;
    end;
  end;

  // Insertar en lista de columna
  if FColHeaders[Col] = nil then
    FColHeaders[Col] := NewNode
  else
  begin
    Current := FColHeaders[Col];
    Prev := nil;
    while (Current <> nil) and (Current^.Row < Row) do
    begin
      Prev := Current;
      Current := Current^.NextCol;
    end;

    if Prev = nil then
    begin
      NewNode^.NextCol := FColHeaders[Col];
      FColHeaders[Col] := NewNode;
    end
    else
    begin
      NewNode^.NextCol := Current;
      Prev^.NextCol := NewNode;
    end;
  end;
end;

function TSparseMatrix.GetValue(Row, Col: Integer): Integer;
var
  Current: PSparseNode;
begin
  Result := 0;
  if (Row >= FSize) or (Col >= FSize) or (Row < 0) or (Col < 0) then Exit;

  Current := FRowHeaders[Row];
  while Current <> nil do
  begin
    if Current^.Col = Col then
    begin
      Result := Current^.Value;
      Exit;
    end
    else if Current^.Col > Col then
      Break;
    Current := Current^.NextRow;
  end;
end;

procedure TSparseMatrix.IncrementValue(Row, Col: Integer);
var
  CurrentValue: Integer;
begin
  CurrentValue := GetValue(Row, Col);
  SetValue(Row, Col, CurrentValue + 1);
end;

procedure TSparseMatrix.Clear;
var
  i: Integer;
  Current, Next: PSparseNode;
begin
  for i := 0 to FSize - 1 do
  begin
    Current := FRowHeaders[i];
    while Current <> nil do
    begin
      Next := Current^.NextRow;
      Dispose(Current);
      Current := Next;
    end;
    FRowHeaders[i] := nil;
    FColHeaders[i] := nil;
  end;
  FHead := nil;
end;

end.

