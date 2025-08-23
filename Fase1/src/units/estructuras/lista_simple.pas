unit lista_simple;

{$mode objfpc}{$H+}

interface

type
  // Registro para almacenar datos del usuario
  TUsuario = record
    id: Integer;
    nombre: string;
    usuario: string;
    email: string;
    telefono: string;
    password: string;
  end;

  // Puntero al nodo de la lista
  PNodoUsuario = ^TNodoUsuario;

  // Nodo de la lista simple
  TNodoUsuario = record
    datos: TUsuario;
    siguiente: PNodoUsuario;
  end;

  // Clase para manejar la lista simple de usuarios
  TListaSimpleUsuarios = class
  private
    cabeza: PNodoUsuario;
    contador: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Operaciones básicas
    procedure Insertar(usuario: TUsuario);
    function Buscar(email: string): PNodoUsuario;
    function BuscarPorUsuario(nombreUsuario: string): PNodoUsuario;
    function EliminarPorEmail(email: string): Boolean;
    procedure Modificar(email: string; nuevosDatos: TUsuario);

    // Operaciones de consulta
    function EstaVacia: Boolean;
    function ObtenerCantidad: Integer;
    procedure Mostrar;
    procedure MostrarUsuario(nodo: PNodoUsuario);

    // Validaciones
    function EmailExiste(email: string): Boolean;
    function UsuarioExiste(nombreUsuario: string): Boolean;
    function ValidarCredenciales(email, password: string): Boolean;

    // Operaciones para reportes
    function GenerarReporte: string;
    procedure OrdenarPorNombre;

    // Iteradores
    function PrimerNodo: PNodoUsuario;
    function SiguienteNodo(nodo: PNodoUsuario): PNodoUsuario;
  end;

implementation

uses SysUtils;

constructor TListaSimpleUsuarios.Create;
begin
  cabeza := nil;
  contador := 0;
end;

destructor TListaSimpleUsuarios.Destroy;
var
  actual, siguiente: PNodoUsuario;
begin
  actual := cabeza;
  while actual <> nil do
  begin
    siguiente := actual^.siguiente;
    Dispose(actual);
    actual := siguiente;
  end;
  inherited Destroy;
end;

procedure TListaSimpleUsuarios.Insertar(usuario: TUsuario);
var
  nuevoNodo: PNodoUsuario;
begin
  // Verificar que el email no exista
  if EmailExiste(usuario.email) then
    raise Exception.Create('El email ya está registrado');

  // Verificar que el nombre de usuario no exista
  if UsuarioExiste(usuario.usuario) then
    raise Exception.Create('El nombre de usuario ya está registrado');

  // Crear nuevo nodo
  New(nuevoNodo);
  nuevoNodo^.datos := usuario;
  nuevoNodo^.datos.id := contador + 1; // Asignar ID automáticamente
  nuevoNodo^.siguiente := cabeza;

  // Actualizar cabeza y contador
  cabeza := nuevoNodo;
  Inc(contador);
end;

function TListaSimpleUsuarios.Buscar(email: string): PNodoUsuario;
var
  actual: PNodoUsuario;
begin
  Result := nil;
  actual := cabeza;

  while actual <> nil do
  begin
    if LowerCase(actual^.datos.email) = LowerCase(email) then
    begin
      Result := actual;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
end;

function TListaSimpleUsuarios.BuscarPorUsuario(nombreUsuario: string): PNodoUsuario;
var
  actual: PNodoUsuario;
begin
  Result := nil;
  actual := cabeza;

  while actual <> nil do
  begin
    if LowerCase(actual^.datos.usuario) = LowerCase(nombreUsuario) then
    begin
      Result := actual;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
end;

function TListaSimpleUsuarios.EliminarPorEmail(email: string): Boolean;
var
  actual, anterior: PNodoUsuario;
begin
  Result := False;
  actual := cabeza;
  anterior := nil;

  while actual <> nil do
  begin
    if LowerCase(actual^.datos.email) = LowerCase(email) then
    begin
      // Si es el primer nodo
      if anterior = nil then
        cabeza := actual^.siguiente
      else
        anterior^.siguiente := actual^.siguiente;

      Dispose(actual);
      Dec(contador);
      Result := True;
      Exit;
    end;
    anterior := actual;
    actual := actual^.siguiente;
  end;
end;

procedure TListaSimpleUsuarios.Modificar(email: string; nuevosDatos: TUsuario);
var
  nodo: PNodoUsuario;
begin
  nodo := Buscar(email);
  if nodo <> nil then
  begin
    // Mantener ID y email original
    nuevosDatos.id := nodo^.datos.id;
    nuevosDatos.email := nodo^.datos.email;
    nodo^.datos := nuevosDatos;
  end
  else
    raise Exception.Create('Usuario no encontrado');
end;

function TListaSimpleUsuarios.EstaVacia: Boolean;
begin
  Result := cabeza = nil;
end;

function TListaSimpleUsuarios.ObtenerCantidad: Integer;
begin
  Result := contador;
end;

procedure TListaSimpleUsuarios.Mostrar;
var
  actual: PNodoUsuario;
begin
  if EstaVacia then
  begin
    WriteLn('La lista de usuarios está vacía');
    Exit;
  end;

  WriteLn('=== LISTA DE USUARIOS ===');
  actual := cabeza;
  while actual <> nil do
  begin
    MostrarUsuario(actual);
    actual := actual^.siguiente;
  end;
end;

procedure TListaSimpleUsuarios.MostrarUsuario(nodo: PNodoUsuario);
begin
  if nodo <> nil then
  begin
    WriteLn('ID: ', nodo^.datos.id);
    WriteLn('Nombre: ', nodo^.datos.nombre);
    WriteLn('Usuario: ', nodo^.datos.usuario);
    WriteLn('Email: ', nodo^.datos.email);
    WriteLn('Teléfono: ', nodo^.datos.telefono);
    WriteLn('------------------------');
  end;
end;

function TListaSimpleUsuarios.EmailExiste(email: string): Boolean;
begin
  Result := Buscar(email) <> nil;
end;

function TListaSimpleUsuarios.UsuarioExiste(nombreUsuario: string): Boolean;
begin
  Result := BuscarPorUsuario(nombreUsuario) <> nil;
end;

function TListaSimpleUsuarios.ValidarCredenciales(email, password: string): Boolean;
var
  nodo: PNodoUsuario;
begin
  Result := False;
  nodo := Buscar(email);
  if nodo <> nil then
    Result := nodo^.datos.password = password;
end;

function TListaSimpleUsuarios.GenerarReporte: string;
var
  actual: PNodoUsuario;
begin
  Result := 'digraph ListaUsuarios {' + LineEnding;
  Result := Result + '  rankdir=LR;' + LineEnding;
  Result := Result + '  node [shape=record];' + LineEnding;

  if EstaVacia then
  begin
    Result := Result + '  vacia [label="Lista Vacía"];' + LineEnding;
  end
  else
  begin
    actual := cabeza;
    while actual <> nil do
    begin
      Result := Result + Format('  user%d [label="ID: %d|Nombre: %s|Usuario: %s|Email: %s|Teléfono: %s"];' + LineEnding,
        [actual^.datos.id, actual^.datos.id, actual^.datos.nombre,
         actual^.datos.usuario, actual^.datos.email, actual^.datos.telefono]);

      if actual^.siguiente <> nil then
        Result := Result + Format('  user%d -> user%d;' + LineEnding,
          [actual^.datos.id, actual^.siguiente^.datos.id]);

      actual := actual^.siguiente;
    end;
  end;

  Result := Result + '}' + LineEnding;
end;

procedure TListaSimpleUsuarios.OrdenarPorNombre;
var
  i, j: Integer;
  actual, siguiente: PNodoUsuario;
  temp: TUsuario;
  intercambiado: Boolean;
begin
  if (cabeza = nil) or (cabeza^.siguiente = nil) then
    Exit;

  // Algoritmo burbuja adaptado para lista enlazada
  repeat
    intercambiado := False;
    actual := cabeza;

    while (actual <> nil) and (actual^.siguiente <> nil) do
    begin
      if actual^.datos.nombre > actual^.siguiente^.datos.nombre then
      begin
        // Intercambiar datos
        temp := actual^.datos;
        actual^.datos := actual^.siguiente^.datos;
        actual^.siguiente^.datos := temp;
        intercambiado := True;
      end;
      actual := actual^.siguiente;
    end;
  until not intercambiado;
end;

function TListaSimpleUsuarios.PrimerNodo: PNodoUsuario;
begin
  Result := cabeza;
end;

function TListaSimpleUsuarios.SiguienteNodo(nodo: PNodoUsuario): PNodoUsuario;
begin
  if nodo <> nil then
    Result := nodo^.siguiente
  else
    Result := nil;
end;

end.

