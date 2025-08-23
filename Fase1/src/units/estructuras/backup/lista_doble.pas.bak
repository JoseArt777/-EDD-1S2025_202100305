unit lista_doble;

{$mode objfpc}{$H+}

interface

uses SysUtils, DateUtils;

type
  // Estados del correo
  TEstadoCorreo = (ecNoLeido, ecLeido, ecEliminado);

  // Registro para almacenar datos del correo
  TCorreo = record
    id: Integer;
    remitente: string;
    destinatario: string;
    asunto: string;
    mensaje: string;
    fecha: TDateTime;
    estado: TEstadoCorreo;
    programado: Boolean;
    fechaProgramada: TDateTime;
  end;

  // Puntero al nodo de la lista doble
  PNodoCorreo = ^TNodoCorreo;

  // Nodo de la lista doblemente enlazada
  TNodoCorreo = record
    datos: TCorreo;
    anterior: PNodoCorreo;
    siguiente: PNodoCorreo;
  end;

  // Clase para manejar la lista doblemente enlazada de correos
  TListaDobleCorreos = class
  private
    cabeza: PNodoCorreo;
    cola: PNodoCorreo;
    contador: Integer;
    contadorID: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Operaciones básicas
    procedure InsertarAlInicio(correo: TCorreo);
    procedure InsertarAlFinal(correo: TCorreo);
    procedure InsertarOrdenado(correo: TCorreo); // Por fecha
    function EliminarPorID(id: Integer): Boolean;
    function BuscarPorID(id: Integer): PNodoCorreo;

    // Operaciones específicas de correos
    procedure MarcarComoLeido(id: Integer);
    procedure MarcarComoNoLeido(id: Integer);
    function ContarNoLeidos: Integer;
    function ObtenerCorreosNoLeidos: TListaDobleCorreos;

    // Operaciones de ordenamiento
    procedure OrdenarPorAsunto;
    procedure OrdenarPorFecha;
    procedure OrdenarPorRemitente;

    // Búsquedas
    function BuscarPorAsunto(palabraClave: string): TListaDobleCorreos;
    function BuscarPorRemitente(remitente: string): TListaDobleCorreos;
    function ObtenerCorreosPorEstado(estado: TEstadoCorreo): TListaDobleCorreos;

    // Operaciones de consulta
    function EstaVacia: Boolean;
    function ObtenerCantidad: Integer;
    procedure MostrarTodos;
    procedure MostrarCorreo(nodo: PNodoCorreo);

    // Navegación
    function PrimerNodo: PNodoCorreo;
    function UltimoNodo: PNodoCorreo;
    function SiguienteNodo(nodo: PNodoCorreo): PNodoCorreo;
    function AnteriorNodo(nodo: PNodoCorreo): PNodoCorreo;

    // Operaciones para reportes
    function GenerarReporte: string;
    function GenerarReporteHTML: string;

    // Utilidades
    procedure LimpiarLista;
    function ClonarLista: TListaDobleCorreos;
  end;

implementation

constructor TListaDobleCorreos.Create;
begin
  cabeza := nil;
  cola := nil;
  contador := 0;
  contadorID := 0;
end;

destructor TListaDobleCorreos.Destroy;
begin
  LimpiarLista;
  inherited Destroy;
end;

procedure TListaDobleCorreos.InsertarAlInicio(correo: TCorreo);
var
  nuevoNodo: PNodoCorreo;
begin
  // Crear nuevo nodo
  New(nuevoNodo);
  Inc(contadorID);
  correo.id := contadorID;
  nuevoNodo^.datos := correo;
  nuevoNodo^.anterior := nil;
  nuevoNodo^.siguiente := cabeza;

  // Actualizar enlaces
  if cabeza <> nil then
    cabeza^.anterior := nuevoNodo
  else
    cola := nuevoNodo; // Es el primer nodo

  cabeza := nuevoNodo;
  Inc(contador);
end;

procedure TListaDobleCorreos.InsertarAlFinal(correo: TCorreo);
var
  nuevoNodo: PNodoCorreo;
begin
  // Crear nuevo nodo
  New(nuevoNodo);
  Inc(contadorID);
  correo.id := contadorID;
  nuevoNodo^.datos := correo;
  nuevoNodo^.siguiente := nil;
  nuevoNodo^.anterior := cola;

  // Actualizar enlaces
  if cola <> nil then
    cola^.siguiente := nuevoNodo
  else
    cabeza := nuevoNodo; // Es el primer nodo

  cola := nuevoNodo;
  Inc(contador);
end;

procedure TListaDobleCorreos.InsertarOrdenado(correo: TCorreo);
var
  nuevoNodo, actual: PNodoCorreo;
begin
  // Si la lista está vacía o el correo va al principio
  if (cabeza = nil) or (correo.fecha >= cabeza^.datos.fecha) then
  begin
    InsertarAlInicio(correo);
    Exit;
  end;

  // Si el correo va al final
  if correo.fecha <= cola^.datos.fecha then
  begin
    InsertarAlFinal(correo);
    Exit;
  end;

  // Buscar posición correcta
  actual := cabeza;
  while (actual <> nil) and (actual^.datos.fecha > correo.fecha) do
    actual := actual^.siguiente;

  // Crear e insertar el nuevo nodo
  New(nuevoNodo);
  Inc(contadorID);
  correo.id := contadorID;
  nuevoNodo^.datos := correo;

  nuevoNodo^.anterior := actual^.anterior;
  nuevoNodo^.siguiente := actual;
  actual^.anterior^.siguiente := nuevoNodo;
  actual^.anterior := nuevoNodo;

  Inc(contador);
end;

function TListaDobleCorreos.EliminarPorID(id: Integer): Boolean;
var
  nodo: PNodoCorreo;
begin
  Result := False;
  nodo := BuscarPorID(id);

  if nodo <> nil then
  begin
    // Actualizar enlaces
    if nodo^.anterior <> nil then
      nodo^.anterior^.siguiente := nodo^.siguiente
    else
      cabeza := nodo^.siguiente; // Era el primer nodo

    if nodo^.siguiente <> nil then
      nodo^.siguiente^.anterior := nodo^.anterior
    else
      cola := nodo^.anterior; // Era el último nodo

    Dispose(nodo);
    Dec(contador);
    Result := True;
  end;
end;

function TListaDobleCorreos.BuscarPorID(id: Integer): PNodoCorreo;
var
  actual: PNodoCorreo;
begin
  Result := nil;
  actual := cabeza;

  while actual <> nil do
  begin
    if actual^.datos.id = id then
    begin
      Result := actual;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
end;

procedure TListaDobleCorreos.MarcarComoLeido(id: Integer);
var
  nodo: PNodoCorreo;
begin
  nodo := BuscarPorID(id);
  if nodo <> nil then
    nodo^.datos.estado := ecLeido;
end;

procedure TListaDobleCorreos.MarcarComoNoLeido(id: Integer);
var
  nodo: PNodoCorreo;
begin
  nodo := BuscarPorID(id);
  if nodo <> nil then
    nodo^.datos.estado := ecNoLeido;
end;

function TListaDobleCorreos.ContarNoLeidos: Integer;
var
  actual: PNodoCorreo;
begin
  Result := 0;
  actual := cabeza;

  while actual <> nil do
  begin
    if actual^.datos.estado = ecNoLeido then
      Inc(Result);
    actual := actual^.siguiente;
  end;
end;

function TListaDobleCorreos.ObtenerCorreosNoLeidos: TListaDobleCorreos;
var
  actual: PNodoCorreo;
begin
  Result := TListaDobleCorreos.Create;
  actual := cabeza;

  while actual <> nil do
  begin
    if actual^.datos.estado = ecNoLeido then
      Result.InsertarAlFinal(actual^.datos);
    actual := actual^.siguiente;
  end;
end;

procedure TListaDobleCorreos.OrdenarPorAsunto;
var
  intercambiado: Boolean;
  actual: PNodoCorreo;
  temp: TCorreo;
begin
  if (cabeza = nil) or (cabeza^.siguiente = nil) then
    Exit;

  repeat
    intercambiado := False;
    actual := cabeza;

    while actual^.siguiente <> nil do
    begin
      if LowerCase(actual^.datos.asunto) > LowerCase(actual^.siguiente^.datos.asunto) then
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

procedure TListaDobleCorreos.OrdenarPorFecha;
var
  intercambiado: Boolean;
  actual: PNodoCorreo;
  temp: TCorreo;
begin
  if (cabeza = nil) or (cabeza^.siguiente = nil) then
    Exit;

  repeat
    intercambiado := False;
    actual := cabeza;

    while actual^.siguiente <> nil do
    begin
      if actual^.datos.fecha < actual^.siguiente^.datos.fecha then
      begin
        // Intercambiar datos (más recientes primero)
        temp := actual^.datos;
        actual^.datos := actual^.siguiente^.datos;
        actual^.siguiente^.datos := temp;
        intercambiado := True;
      end;
      actual := actual^.siguiente;
    end;
  until not intercambiado;
end;

procedure TListaDobleCorreos.OrdenarPorRemitente;
var
  intercambiado: Boolean;
  actual: PNodoCorreo;
  temp: TCorreo;
begin
  if (cabeza = nil) or (cabeza^.siguiente = nil) then
    Exit;

  repeat
    intercambiado := False;
    actual := cabeza;

    while actual^.siguiente <> nil do
    begin
      if LowerCase(actual^.datos.remitente) > LowerCase(actual^.siguiente^.datos.remitente) then
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

function TListaDobleCorreos.BuscarPorAsunto(palabraClave: string): TListaDobleCorreos;
var
  actual: PNodoCorreo;
begin
  Result := TListaDobleCorreos.Create;
  actual := cabeza;
  palabraClave := LowerCase(palabraClave);

  while actual <> nil do
  begin
    if Pos(palabraClave, LowerCase(actual^.datos.asunto)) > 0 then
      Result.InsertarAlFinal(actual^.datos);
    actual := actual^.siguiente;
  end;
end;

function TListaDobleCorreos.BuscarPorRemitente(remitente: string): TListaDobleCorreos;
var
  actual: PNodoCorreo;
begin
  Result := TListaDobleCorreos.Create;
  actual := cabeza;

  while actual <> nil do
  begin
    if LowerCase(actual^.datos.remitente) = LowerCase(remitente) then
      Result.InsertarAlFinal(actual^.datos);
    actual := actual^.siguiente;
  end;
end;

function TListaDobleCorreos.ObtenerCorreosPorEstado(estado: TEstadoCorreo): TListaDobleCorreos;
var
  actual: PNodoCorreo;
begin
  Result := TListaDobleCorreos.Create;
  actual := cabeza;

  while actual <> nil do
  begin
    if actual^.datos.estado = estado then
      Result.InsertarAlFinal(actual^.datos);
    actual := actual^.siguiente;
  end;
end;

function TListaDobleCorreos.EstaVacia: Boolean;
begin
  Result := cabeza = nil;
end;

function TListaDobleCorreos.ObtenerCantidad: Integer;
begin
  Result := contador;
end;

procedure TListaDobleCorreos.MostrarTodos;
var
  actual: PNodoCorreo;
begin
  if EstaVacia then
  begin
    WriteLn('La bandeja de entrada está vacía');
    Exit;
  end;

  WriteLn('=== BANDEJA DE ENTRADA ===');
  actual := cabeza;
  while actual <> nil do
  begin
    MostrarCorreo(actual);
    actual := actual^.siguiente;
  end;
end;

procedure TListaDobleCorreos.MostrarCorreo(nodo: PNodoCorreo);
var
  estadoStr: string;
begin
  if nodo <> nil then
  begin
    case nodo^.datos.estado of
      ecNoLeido: estadoStr := 'NL';
      ecLeido: estadoStr := 'L';
      ecEliminado: estadoStr := 'E';
    end;

    WriteLn('ID: ', nodo^.datos.id);
    WriteLn('Estado: ', estadoStr);
    WriteLn('Remitente: ', nodo^.datos.remitente);
    WriteLn('Asunto: ', nodo^.datos.asunto);
    WriteLn('Fecha: ', DateTimeToStr(nodo^.datos.fecha));
    WriteLn('Mensaje: ', nodo^.datos.mensaje);
    WriteLn('------------------------');
  end;
end;

function TListaDobleCorreos.PrimerNodo: PNodoCorreo;
begin
  Result := cabeza;
end;

function TListaDobleCorreos.UltimoNodo: PNodoCorreo;
begin
  Result := cola;
end;

function TListaDobleCorreos.SiguienteNodo(nodo: PNodoCorreo): PNodoCorreo;
begin
  if nodo <> nil then
    Result := nodo^.siguiente
  else
    Result := nil;
end;

function TListaDobleCorreos.AnteriorNodo(nodo: PNodoCorreo): PNodoCorreo;
begin
  if nodo <> nil then
    Result := nodo^.anterior
  else
    Result := nil;
end;

function TListaDobleCorreos.GenerarReporte: string;
var
  actual: PNodoCorreo;
  estadoStr: string;
begin
  Result := 'digraph ListaDoblementeEnlazada {' + LineEnding;
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
      case actual^.datos.estado of
        ecNoLeido: estadoStr := 'NL';
        ecLeido: estadoStr := 'L';
        ecEliminado: estadoStr := 'E';
      end;

      Result := Result + Format('  correo%d [label="ID: %d|Estado: %s|Remitente: %s|Asunto: %s|Fecha: %s"];' + LineEnding,
        [actual^.datos.id, actual^.datos.id, estadoStr, actual^.datos.remitente,
         actual^.datos.asunto, DateTimeToStr(actual^.datos.fecha)]);

      if actual^.siguiente <> nil then
        Result := Result + Format('  correo%d -> correo%d [dir=both];' + LineEnding,
          [actual^.datos.id, actual^.siguiente^.datos.id]);

      actual := actual^.siguiente;
    end;
  end;

  Result := Result + '}' + LineEnding;
end;

function TListaDobleCorreos.GenerarReporteHTML: string;
var
  actual: PNodoCorreo;
  estadoStr, estadoColor: string;
begin
  Result := '<html><head><title>Bandeja de Entrada</title></head><body>' + LineEnding;
  Result := Result + '<h1>Bandeja de Entrada</h1>' + LineEnding;
  Result := Result + '<table border="1">' + LineEnding;
  Result := Result + '<tr><th>Estado</th><th>Asunto</th><th>Remitente</th><th>Fecha</th></tr>' + LineEnding;

  actual := cabeza;
  while actual <> nil do
  begin
    case actual^.datos.estado of
      ecNoLeido: begin
        estadoStr := 'NL';
        estadoColor := 'style="background-color: #ffcccc"';
      end;
      ecLeido: begin
        estadoStr := 'L';
        estadoColor := 'style="background-color: #ccffcc"';
      end;
      ecEliminado: begin
        estadoStr := 'E';
        estadoColor := 'style="background-color: #cccccc"';
      end;
    end;

    Result := Result + Format('<tr %s><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>' + LineEnding,
      [estadoColor, estadoStr, actual^.datos.asunto, actual^.datos.remitente,
       DateTimeToStr(actual^.datos.fecha)]);

    actual := actual^.siguiente;
  end;

  Result := Result + '</table>' + LineEnding;
  Result := Result + '<p>Total de correos: ' + IntToStr(contador) + '</p>' + LineEnding;
  Result := Result + '<p>Correos no leídos: ' + IntToStr(ContarNoLeidos) + '</p>' + LineEnding;
  Result := Result + '</body></html>' + LineEnding;
end;

procedure TListaDobleCorreos.LimpiarLista;
var
  actual, siguiente: PNodoCorreo;
begin
  actual := cabeza;
  while actual <> nil do
  begin
    siguiente := actual^.siguiente;
    Dispose(actual);
    actual := siguiente;
  end;
  cabeza := nil;
  cola := nil;
  contador := 0;
end;

function TListaDobleCorreos.ClonarLista: TListaDobleCorreos;
var
  actual: PNodoCorreo;
begin
  Result := TListaDobleCorreos.Create;
  actual := cabeza;

  while actual <> nil do
  begin
    Result.InsertarAlFinal(actual^.datos);
    actual := actual^.siguiente;
  end;
end;

end.

