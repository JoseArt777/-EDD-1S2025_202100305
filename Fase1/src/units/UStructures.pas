unit UStructures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  // ======= ESTRUCTURAS PARA USUARIOS =======
  PUser = ^TUser;
  TUser = record
    id: Integer;
    nombre: String;
    usuario: String;
    email: String;
    telefono: String;
    password: String;
    next: PUser;
  end;

  // Lista Simple para Usuarios
  TUserList = class
  private
    head: PUser;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddUser(id: Integer; nombre, usuario, email, telefono, password: String);
    function FindUserByEmail(email: String): PUser;
    function FindUserByUsername(username: String): PUser;
    function ValidateUser(email, password: String): PUser;
    procedure PrintUsers;
    function GetUserCount: Integer;
  end;

  // ======= ESTRUCTURAS PARA CORREOS =======
  PEmail = ^TEmail;
  TEmail = record
    id: Integer;
    remitente: String;
    destinatario: String;
    asunto: String;
    mensaje: String;
    fecha: String;
    estado: String; // 'NL' = No Leído, 'L' = Leído
    programado: Boolean;
    fechaEnvio: String;
    prev: PEmail;
    next: PEmail;
  end;

  // Lista Doblemente Enlazada para Correos Recibidos
  TEmailList = class
  private
    head: PEmail;
    tail: PEmail;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEmail(id: Integer; remitente, destinatario, asunto, mensaje, fecha: String; programado: Boolean = False; fechaEnvio: String = '');
    procedure DeleteEmail(emailId: Integer);
    function FindEmailById(id: Integer): PEmail;
    procedure MarkAsRead(emailId: Integer);
    procedure SortBySubject;
    function GetUnreadCount: Integer;
    procedure PrintEmails;
  end;

  // ======= ESTRUCTURAS PARA CONTACTOS =======
  PContact = ^TContact;
  TContact = record
    id: Integer;
    nombre: String;
    usuario: String;
    email: String;
    telefono: String;
    next: PContact;
  end;

  // Lista Circular para Contactos
  TContactList = class
  private
    head: PContact;
    current: PContact;
    count: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddContact(id: Integer; nombre, usuario, email, telefono: String);
    function FindContactByEmail(email: String): PContact;
    function GetNextContact: PContact;
    function GetPrevContact: PContact;
    function GetCurrentContact: PContact;
    procedure PrintContacts;
    function GetContactCount: Integer;
  end;

  // ======= ESTRUCTURAS PARA CORREOS PROGRAMADOS =======
  PScheduledEmail = ^TScheduledEmail;
  TScheduledEmail = record
    id: Integer;
    remitente: String;
    destinatario: String;
    asunto: String;
    mensaje: String;
    fechaEnvio: String;
    next: PScheduledEmail;
  end;

  // Cola para Correos Programados
  TEmailQueue = class
  private
    front: PScheduledEmail;
    rear: PScheduledEmail;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(id: Integer; remitente, destinatario, asunto, mensaje, fechaEnvio: String);
    function Dequeue: PScheduledEmail;
    function IsEmpty: Boolean;
    procedure PrintQueue;
  end;

  // ======= ESTRUCTURAS PARA CORREOS ELIMINADOS =======
  PDeletedEmail = ^TDeletedEmail;
  TDeletedEmail = record
    id: Integer;
    remitente: String;
    asunto: String;
    mensaje: String;
    fecha: String;
    next: PDeletedEmail;
  end;

  // Pila para Correos Eliminados
  TEmailStack = class
  private
    top: PDeletedEmail;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(id: Integer; remitente, asunto, mensaje, fecha: String);
    function Pop: PDeletedEmail;
    function IsEmpty: Boolean;
    function SearchBySubject(keyword: String): PDeletedEmail;
    procedure PrintStack;
  end;

  // ======= MATRIZ DISPERSA PARA RELACIONES =======
  PMatrixNode = ^TMatrixNode;
  TMatrixNode = record
    row: Integer;
    col: Integer;
    value: Integer;
    nextRow: PMatrixNode;
    nextCol: PMatrixNode;
  end;

  TSparseMatrix = class
  private
    head: PMatrixNode;
    userEmails: TStringList; // Para mapear emails a índices
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRelation(senderEmail, receiverEmail: String);
    function GetRelationValue(senderEmail, receiverEmail: String): Integer;
    function GetUserIndex(email: String): Integer;
    procedure PrintMatrix;
  end;

  // ======= ESTRUCTURAS PARA COMUNIDADES =======
  PMember = ^TMember;
  TMember = record
    email: String;
    next: PMember;
  end;

  PCommunity = ^TCommunity;
  TCommunity = record
    id: Integer;
    nombre: String;
    members: PMember;
    next: PCommunity;
  end;

  // Lista de Listas para Comunidades
  TCommunityList = class
  private
    head: PCommunity;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateCommunity(id: Integer; nombre: String);
    procedure AddMemberToCommunity(communityId: Integer; memberEmail: String);
    function FindCommunity(id: Integer): PCommunity;
    procedure PrintCommunities;
  end;

var
  // Variables globales para las estructuras
  UserList: TUserList;
  EmailList: TEmailList;
  ContactList: TContactList;
  EmailQueue: TEmailQueue;
  EmailStack: TEmailStack;
  RelationMatrix: TSparseMatrix;
  CommunityList: TCommunityList;

implementation

// ======= IMPLEMENTACIÓN LISTA DE USUARIOS =======
constructor TUserList.Create;
begin
  head := nil;
end;

destructor TUserList.Destroy;
var
  current, temp: PUser;
begin
  current := head;
  while current <> nil do
  begin
    temp := current;
    current := current^.next;
    Dispose(temp);
  end;
  inherited Destroy;
end;

procedure TUserList.AddUser(id: Integer; nombre, usuario, email, telefono, password: String);
var
  newUser: PUser;
begin
  New(newUser);
  newUser^.id := id;
  newUser^.nombre := nombre;
  newUser^.usuario := usuario;
  newUser^.email := email;
  newUser^.telefono := telefono;
  newUser^.password := password;
  newUser^.next := head;
  head := newUser;
end;

function TUserList.FindUserByEmail(email: String): PUser;
var
  current: PUser;
begin
  current := head;
  while current <> nil do
  begin
    if current^.email = email then
    begin
      Result := current;
      Exit;
    end;
    current := current^.next;
  end;
  Result := nil;
end;

function TUserList.FindUserByUsername(username: String): PUser;
var
  current: PUser;
begin
  current := head;
  while current <> nil do
  begin
    if current^.usuario = username then
    begin
      Result := current;
      Exit;
    end;
    current := current^.next;
  end;
  Result := nil;
end;

function TUserList.ValidateUser(email, password: String): PUser;
var
  user: PUser;
begin
  user := FindUserByEmail(email);
  if (user <> nil) and (user^.password = password) then
    Result := user
  else
    Result := nil;
end;

procedure TUserList.PrintUsers;
var
  current: PUser;
begin
  current := head;
  while current <> nil do
  begin
    WriteLn('ID: ', current^.id, ', Nombre: ', current^.nombre, ', Email: ', current^.email);
    current := current^.next;
  end;
end;

function TUserList.GetUserCount: Integer;
var
  current: PUser;
  count: Integer;
begin
  count := 0;
  current := head;
  while current <> nil do
  begin
    Inc(count);
    current := current^.next;
  end;
  Result := count;
end;

// ======= IMPLEMENTACIÓN LISTA DE CORREOS =======
constructor TEmailList.Create;
begin
  head := nil;
  tail := nil;
end;

destructor TEmailList.Destroy;
var
  current, temp: PEmail;
begin
  current := head;
  while current <> nil do
  begin
    temp := current;
    current := current^.next;
    Dispose(temp);
  end;
  inherited Destroy;
end;

procedure TEmailList.AddEmail(id: Integer; remitente, destinatario, asunto, mensaje, fecha: String; programado: Boolean; fechaEnvio: String);
var
  newEmail: PEmail;
begin
  New(newEmail);
  newEmail^.id := id;
  newEmail^.remitente := remitente;
  newEmail^.destinatario := destinatario;
  newEmail^.asunto := asunto;
  newEmail^.mensaje := mensaje;
  newEmail^.fecha := fecha;
  newEmail^.estado := 'NL'; // No Leído por defecto
  newEmail^.programado := programado;
  newEmail^.fechaEnvio := fechaEnvio;
  newEmail^.next := nil;
  newEmail^.prev := tail;
  
  if tail <> nil then
    tail^.next := newEmail
  else
    head := newEmail;
  tail := newEmail;
end;

procedure TEmailList.DeleteEmail(emailId: Integer);
var
  current: PEmail;
begin
  current := head;
  while current <> nil do
  begin
    if current^.id = emailId then
    begin
      // Agregar a la pila de eliminados antes de eliminar
      EmailStack.Push(current^.id, current^.remitente, current^.asunto, current^.mensaje, current^.fecha);
      
      // Ajustar enlaces
      if current^.prev <> nil then
        current^.prev^.next := current^.next
      else
        head := current^.next;
        
      if current^.next <> nil then
        current^.next^.prev := current^.prev
      else
        tail := current^.prev;
        
      Dispose(current);
      Break;
    end;
    current := current^.next;
  end;
end;

function TEmailList.FindEmailById(id: Integer): PEmail;
var
  current: PEmail;
begin
  current := head;
  while current <> nil do
  begin
    if current^.id = id then
    begin
      Result := current;
      Exit;
    end;
    current := current^.next;
  end;
  Result := nil;
end;

procedure TEmailList.MarkAsRead(emailId: Integer);
var
  email: PEmail;
begin
  email := FindEmailById(emailId);
  if email <> nil then
    email^.estado := 'L';
end;

procedure TEmailList.SortBySubject;
var
  current, index, temp: PEmail;
  tempData: TEmail;
begin
  if head = nil then Exit;
  
  current := head;
  while current <> nil do
  begin
    index := current^.next;
    while index <> nil do
    begin
      if current^.asunto > index^.asunto then
      begin
        // Intercambiar datos (no punteros)
        tempData := current^;
        current^.id := index^.id;
        current^.remitente := index^.remitente;
        current^.destinatario := index^.destinatario;
        current^.asunto := index^.asunto;
        current^.mensaje := index^.mensaje;
        current^.fecha := index^.fecha;
        current^.estado := index^.estado;
        current^.programado := index^.programado;
        current^.fechaEnvio := index^.fechaEnvio;
        
        index^.id := tempData.id;
        index^.remitente := tempData.remitente;
        index^.destinatario := tempData.destinatario;
        index^.asunto := tempData.asunto;
        index^.mensaje := tempData.mensaje;
        index^.fecha := tempData.fecha;
        index^.estado := tempData.estado;
        index^.programado := tempData.programado;
        index^.fechaEnvio := tempData.fechaEnvio;
      end;
      index := index^.next;
    end;
    current := current^.next;
  end;
end;

function TEmailList.GetUnreadCount: Integer;
var
  current: PEmail;
  count: Integer;
begin
  count := 0;
  current := head;
  while current <> nil do
  begin
    if current^.estado = 'NL' then
      Inc(count);
    current := current^.next;
  end;
  Result := count;
end;

procedure TEmailList.PrintEmails;
var
  current: PEmail;
begin
  current := head;
  while current <> nil do
  begin
    WriteLn('ID: ', current^.id, ', De: ', current^.remitente, ', Asunto: ', current^.asunto, ', Estado: ', current^.estado);
    current := current^.next;
  end;
end;

// ======= IMPLEMENTACIÓN LISTA CIRCULAR DE CONTACTOS =======
constructor TContactList.Create;
begin
  head := nil;
  current := nil;
  count := 0;
end;

destructor TContactList.Destroy;
var
  temp: PContact;
  i: Integer;
begin
  if head <> nil then
  begin
    current := head;
    for i := 0 to count - 1 do
    begin
      temp := current;
      current := current^.next;
      Dispose(temp);
    end;
  end;
  inherited Destroy;
end;

procedure TContactList.AddContact(id: Integer; nombre, usuario, email, telefono: String);
var
  newContact: PContact;
begin
  New(newContact);
  newContact^.id := id;
  newContact^.nombre := nombre;
  newContact^.usuario := usuario;
  newContact^.email := email;
  newContact^.telefono := telefono;
  
  if head = nil then
  begin
    head := newContact;
    newContact^.next := head; // Apunta a sí mismo
    current := head;
  end
  else
  begin
    newContact^.next := head;
    // Encontrar el último nodo
    current := head;
    while current^.next <> head do
      current := current^.next;
    current^.next := newContact;
    current := head;
  end;
  Inc(count);
end;

function TContactList.FindContactByEmail(email: String): PContact;
var
  temp: PContact;
  i: Integer;
begin
  if head = nil then
  begin
    Result := nil;
    Exit;
  end;
  
  temp := head;
  for i := 0 to count - 1 do
  begin
    if temp^.email = email then
    begin
      Result := temp;
      Exit;
    end;
    temp := temp^.next;
  end;
  Result := nil;
end;

function TContactList.GetNextContact: PContact;
begin
  if current <> nil then
  begin
    current := current^.next;
    Result := current;
  end
  else
    Result := nil;
end;

function TContactList.GetPrevContact: PContact;
var
  temp: PContact;
begin
  if (head = nil) or (count <= 1) then
  begin
    Result := current;
    Exit;
  end;
  
  temp := head;
  while temp^.next <> current do
    temp := temp^.next;
  current := temp;
  Result := current;
end;

function TContactList.GetCurrentContact: PContact;
begin
  Result := current;
end;

procedure TContactList.PrintContacts;
var
  temp: PContact;
  i: Integer;
begin
  if head = nil then
  begin
    WriteLn('No hay contactos');
    Exit;
  end;
  
  temp := head;
  for i := 0 to count - 1 do
  begin
    WriteLn('ID: ', temp^.id, ', Nombre: ', temp^.nombre, ', Email: ', temp^.email);
    temp := temp^.next;
  end;
end;

function TContactList.GetContactCount: Integer;
begin
  Result := count;
end;

// ======= IMPLEMENTACIÓN COLA DE CORREOS PROGRAMADOS =======
constructor TEmailQueue.Create;
begin
  front := nil;
  rear := nil;
end;

destructor TEmailQueue.Destroy;
begin
  while not IsEmpty do
    Dequeue;
  inherited Destroy;
end;

procedure TEmailQueue.Enqueue(id: Integer; remitente, destinatario, asunto, mensaje, fechaEnvio: String);
var
  newEmail: PScheduledEmail;
begin
  New(newEmail);
  newEmail^.id := id;
  newEmail^.remitente := remitente;
  newEmail^.destinatario := destinatario;
  newEmail^.asunto := asunto;
  newEmail^.mensaje := mensaje;
  newEmail^.fechaEnvio := fechaEnvio;
  newEmail^.next := nil;
  
  if rear = nil then
  begin
    front := newEmail;
    rear := newEmail;
  end
  else
  begin
    rear^.next := newEmail;
    rear := newEmail;
  end;
end;

function TEmailQueue.Dequeue: PScheduledEmail;
var
  temp: PScheduledEmail;
begin
  if front = nil then
  begin
    Result := nil;
    Exit;
  end;
  
  temp := front;
  front := front^.next;
  if front = nil then
    rear := nil;
  Result := temp;
end;

function TEmailQueue.IsEmpty: Boolean;
begin
  Result := front = nil;
end;

procedure TEmailQueue.PrintQueue;
var
  current: PScheduledEmail;
begin
  current := front;
  while current <> nil do
  begin
    WriteLn('ID: ', current^.id, ', Para: ', current^.destinatario, ', Asunto: ', current^.asunto, ', Envío: ', current^.fechaEnvio);
    current := current^.next;
  end;
end;

// ======= IMPLEMENTACIÓN PILA DE CORREOS ELIMINADOS =======
constructor TEmailStack.Create;
begin
  top := nil;
end;

destructor TEmailStack.Destroy;
begin
  while not IsEmpty do
    Pop;
  inherited Destroy;
end;

procedure TEmailStack.Push(id: Integer; remitente, asunto, mensaje, fecha: String);
var
  newEmail: PDeletedEmail;
begin
  New(newEmail);
  newEmail^.id := id;
  newEmail^.remitente := remitente;
  newEmail^.asunto := asunto;
  newEmail^.mensaje := mensaje;
  newEmail^.fecha := fecha;
  newEmail^.next := top;
  top := newEmail;
end;

function TEmailStack.Pop: PDeletedEmail;
var
  temp: PDeletedEmail;
begin
  if top = nil then
  begin
    Result := nil;
    Exit;
  end;
  
  temp := top;
  top := top^.next;
  Result := temp;
end;

function TEmailStack.IsEmpty: Boolean;
begin
  Result := top = nil;
end;

function TEmailStack.SearchBySubject(keyword: String): PDeletedEmail;
var
  current: PDeletedEmail;
begin
  current := top;
  while current <> nil do
  begin
    if Pos(keyword, current^.asunto) > 0 then
    begin
      Result := current;
      Exit;
    end;
    current := current^.next;
  end;
  Result := nil;
end;

procedure TEmailStack.PrintStack;
var
  current: PDeletedEmail;
begin
  current := top;
  while current <> nil do
  begin
    WriteLn('ID: ', current^.id, ', De: ', current^.remitente, ', Asunto: ', current^.asunto);
    current := current^.next;
  end;
end;

// ======= IMPLEMENTACIÓN MATRIZ DISPERSA =======
constructor TSparseMatrix.Create;
begin
  head := nil;
  userEmails := TStringList.Create;
end;

destructor TSparseMatrix.Destroy;
var
  current, temp: PMatrixNode;
begin
  current := head;
  while current <> nil do
  begin
    temp := current;
    current := current^.nextRow;
    Dispose(temp);
  end;
  userEmails.Free;
  inherited Destroy;
end;

function TSparseMatrix.GetUserIndex(email: String): Integer;
var
  index: Integer;
begin
  index := userEmails.IndexOf(email);
  if index = -1 then
  begin
    userEmails.Add(email);
    Result := userEmails.Count - 1;
  end
  else
    Result := index;
end;

procedure TSparseMatrix.AddRelation(senderEmail, receiverEmail: String);
var
  senderIndex, receiverIndex: Integer;
  current, newNode: PMatrixNode;
begin
  senderIndex := GetUserIndex(senderEmail);
  receiverIndex := GetUserIndex(receiverEmail);
  
  // Buscar si ya existe la relación
  current := head;
  while current <> nil do
  begin
    if (current^.row = senderIndex) and (current^.col = receiverIndex) then
    begin
      Inc(current^.value);
      Exit;
    end;
    current := current^.nextRow;
  end;
  
  // Crear nueva relación
  New(newNode);
  newNode^.row := senderIndex;
  newNode^.col := receiverIndex;
  newNode^.value := 1;
  newNode^.nextRow := head;
  newNode^.nextCol := nil;
  head := newNode;
end;

function TSparseMatrix.GetRelationValue(senderEmail, receiverEmail: String): Integer;
var
  senderIndex, receiverIndex: Integer;
  current: PMatrixNode;
begin
  senderIndex := GetUserIndex(senderEmail);
  receiverIndex := GetUserIndex(receiverEmail);
  
  current := head;
  while current <> nil do
  begin
    if (current^.row = senderIndex) and (current^.col = receiverIndex) then
    begin
      Result := current^.value;
      Exit;
    end;
    current := current^.nextRow;
  end;
  Result := 0;
end;

procedure TSparseMatrix.PrintMatrix;
var
  current: PMatrixNode;
begin
  current := head;
  while current <> nil do
  begin
    WriteLn('De: ', userEmails[current^.row], ' Para: ', userEmails[current^.col], ' Cantidad: ', current^.value);
    current := current^.nextRow;
  end;
end;

// ======= IMPLEMENTACIÓN LISTA DE COMUNIDADES =======
constructor TCommunityList.Create;
begin
  head := nil;
end;

destructor TCommunityList.Destroy;
var
  currentCommunity, tempCommunity: PCommunity;
  currentMember, tempMember: PMember;
begin
  currentCommunity := head;
  while currentCommunity <> nil do
  begin
    // Liberar miembros
    currentMember := currentCommunity^.members;
    while currentMember <> nil do
    begin
      tempMember := currentMember;
      currentMember := currentMember^.next;
      Dispose(tempMember);
    end;
    
    tempCommunity := currentCommunity;
    currentCommunity := currentCommunity^.next;
    Dispose(tempCommunity);
  end;
  inherited Destroy;
end;

procedure TCommunityList.CreateCommunity(id: Integer; nombre: String);
var
  newCommunity: PCommunity;
begin
  New(newCommunity);
  newCommunity^.id := id;
  newCommunity^.nombre := nombre;
  newCommunity^.members := nil;
  newCommunity^.next := head;
  head := newCommunity;
end;

procedure TCommunityList.AddMemberToCommunity(communityId: Integer; memberEmail: String);
var
  community: PCommunity;
  newMember: PMember;
begin
  community := FindCommunity(communityId);
  if community <> nil then
  begin
    New(newMember);
    newMember^.email := memberEmail;
    newMember^.next := community^.members;
    community^.members := newMember;
  end;
end;

function TCommunityList.FindCommunity(id: Integer): PCommunity;
var
  current: PCommunity;
begin
  current := head;
  while current <> nil do
  begin
    if current^.id = id then
    begin
      Result := current;
      Exit;
    end;
    current := current^.next;
  end;
  Result := nil;
end;

procedure TCommunityList.PrintCommunities;
var
  currentCommunity: PCommunity;
  currentMember: PMember;
begin
  currentCommunity := head;
  while currentCommunity <> nil do
  begin
    WriteLn('Comunidad: ', currentCommunity^.nombre, ' (ID: ', currentCommunity^.id, ')');
    currentMember := currentCommunity^.members;
    while currentMember <> nil do
    begin
      WriteLn('  - ', currentMember^.email);
      currentMember := currentMember^.next;
    end;
    currentCommunity := currentCommunity^.next;
  end;
end;

// ======= INICIALIZACIÓN =======
initialization
  UserList := TUserList.Create;
  EmailList := TEmailList.Create;
  ContactList := TContactList.Create;
  EmailQueue := TEmailQueue.Create;
  EmailStack := TEmailStack.Create;
  RelationMatrix := TSparseMatrix.Create;
  CommunityList := TCommunityList.Create;

finalization
  UserList.Free;
  EmailList.Free;
  ContactList.Free;
  EmailQueue.Free;
  EmailStack.Free;
  RelationMatrix.Free;
  CommunityList.Free;

end.
