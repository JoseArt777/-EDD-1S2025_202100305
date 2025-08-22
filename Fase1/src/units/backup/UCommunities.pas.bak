unit UCommunities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UStructures, UUser;

type
  TCommunityManager = class
  private
    FCommunities: TCommunityList;
    FUserManager: TUserManager;
    FNextCommunityId: Integer;
  public
    constructor Create(userManager: TUserManager);
    destructor Destroy; override;
    
    // Gestión de comunidades
    function CreateCommunity(nombre: string; out errorMsg: string): Boolean;
    function AddUserToCommunity(communityId: Integer; userEmail: string; out errorMsg: string): Boolean;
    function RemoveUserFromCommunity(communityId: Integer; userEmail: string): Boolean;
    
    // Búsquedas y consultas
    function FindCommunity(id: Integer): PCommunity;
    function FindCommunityByName(nombre: string): PCommunity;
    function GetUserCommunities(userEmail: string): TStringList;
    function IsUserInCommunity(communityId: Integer; userEmail: string): Boolean;
    
    // Getters
    property Communities: TCommunityList read FCommunities;
    property NextCommunityId: Integer read FNextCommunityId;
  end;

implementation

constructor TCommunityManager.Create(userManager: TUserManager);
begin
  inherited Create;
  FCommunities := TCommunityList.Create;
  FUserManager := userManager;
  FNextCommunityId := 1;
end;

destructor TCommunityManager.Destroy;
begin
  FCommunities.Free;
  inherited Destroy;
end;

function TCommunityManager.CreateCommunity(nombre: string; out errorMsg: string): Boolean;
begin
  Result := False;
  errorMsg := '';
  
  // Validaciones
  if Trim(nombre) = '' then
  begin
    errorMsg := 'El nombre de la comunidad es requerido';
    Exit;
  end;
  
  if FindCommunityByName(nombre) <> nil then
  begin
    errorMsg := 'Ya existe una comunidad con ese nombre';
    Exit;
  end;
  
  // Crear comunidad
  FCommunities.AddCommunity(FNextCommunityId, nombre);
  Inc(FNextCommunityId);
  Result := True;
end;

function TCommunityManager.AddUserToCommunity(communityId: Integer; userEmail: string; out errorMsg: string): Boolean;
var
  community: PCommunity;
  user: PUser;
begin
  Result := False;
  errorMsg := '';
  
  // Buscar comunidad
  community := FindCommunity(communityId);
  if community = nil then
  begin
    errorMsg := 'Comunidad no encontrada';
    Exit;
  end;
  
  // Buscar usuario
  user := FUserManager.GetUserByEmail(userEmail);
  if user = nil then
  begin
    errorMsg := 'Usuario no encontrado';
    Exit;
  end;
  
  // Verificar si el usuario ya está en la comunidad
  if IsUserInCommunity(communityId, userEmail) then
  begin
    errorMsg := 'El usuario ya pertenece a esta comunidad';
    Exit;
  end;
  
  // Agregar usuario a la comunidad
  FCommunities.AddUserToCommunity(communityId, user);
  Result := True;
end;

function TCommunityManager.RemoveUserFromCommunity(communityId: Integer; userEmail: string): Boolean;
var
  community: PCommunity;
  current, previous: PUser;
begin
  Result := False;
  
  community := FindCommunity(communityId);
  if community = nil then Exit;
  
  current := community^.usuarios;
  previous := nil;
  
  while current <> nil do
  begin
    if current^.email = userEmail then
    begin
      if previous = nil then
        community^.usuarios := current^.siguiente
      else
        previous^.siguiente := current^.siguiente;
      
      Dispose(current);
      Result := True;
      Break;
    end;
    previous := current;
    current := current^.siguiente;
  end;
end;

function TCommunityManager.FindCommunity(id: Integer): PCommunity;
begin
  Result := FCommunities.FindCommunity(id);
end;

function TCommunityManager.FindCommunityByName(nombre: string): PCommunity;
var
  current: PCommunity;
begin
  Result := nil;
  current := FCommunities.Head;
  
  while current <> nil do
  begin
    if current^.nombre = nombre then
    begin
      Result := current;
      Break;
    end;
    current := current^.siguiente;
  end;
end;

function TCommunityManager.GetUserCommunities(userEmail: string): TStringList;
var
  current: PCommunity;
  userCurrent: PUser;
begin
  Result := TStringList.Create;
  current := FCommunities.Head;
  
  while current <> nil do
  begin
    userCurrent := current^.usuarios;
    while userCurrent <> nil do
    begin
      if userCurrent^.email = userEmail then
      begin
        Result.Add(current^.nombre);
        Break;
      end;
      userCurrent := userCurrent^.siguiente;
    end;
    current := current^.siguiente;
  end;
end;

function TCommunityManager.IsUserInCommunity(communityId: Integer; userEmail: string): Boolean;
var
  community: PCommunity;
  current: PUser;
begin
  Result := False;
  
  community := FindCommunity(communityId);
  if community = nil then Exit;
  
  current := community^.usuarios;
  while current <> nil do
  begin
    if current^.email = userEmail then
    begin
      Result := True;
      Break;
    end;
    current := current^.siguiente;
  end;
end;

end.
