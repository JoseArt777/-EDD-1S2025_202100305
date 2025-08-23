unit pila;

{$mode objfpc}{$H+}

interface

uses SysUtils, DateUtils;

type
  // Estados del correo en papelera
  TEstadoPapelera = (epEliminado, epRestaurado, epEliminadoDefinitivo);

  // Registro para almacenar datos del correo eliminado
  TCorreoEliminado = record
    id: Integer;
    idOriginal: Integer;       // ID del correo original
    remitente: string;
    destinatario: string;
    asunto: string;
    mensaje: string;
    fechaOriginal: TDateTime;  // Fecha del correo original
    fechaEliminacion: TDateTime; // Fecha cuando se eliminó
    estado: TEstadoPapelera;
    motivoEliminacion: string;
  end;

  // Puntero al nodo de la pila
  PNodoPila = ^TNodoPila;

  // Nodo de la pila
  TNodoPila = record
    datos: TCorreoEliminado;
    siguiente: PNodoPila;
  end;

  // Clase para manejar la pila de correos eliminados (papelera)
  TPilaPapelera = class
  private
    tope: PNodoPila;
    contador: Integer;
    contadorID: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Operaciones básicas de pila
    procedure Apilar(correo: TCorreoEliminado);
    function Desapilar: TCorreoEliminado;
    function Tope: TCorreoEliminado;
    function EstaVacia: Boolean;
    function ObtenerCantidad: Integer;

    // Operaciones específicas de papelera
    function BuscarPorAsunto(palabraClave: string): TArray<TCorreoEliminado>;
    function BuscarPorRemitente(remitente: string): TArray<TCorreoEliminado>;
    function BuscarPorID(id: Integer): PNodoPila;
    function BuscarPorIDOriginal(idOriginal: Integer): PNodoPila;

    // Gestión de eliminación
    function EliminarDefinitivamente(id: Integer): Boolean;
    function RestaurarCorreo(id: Integer): TCorreoEliminado;
    procedure VaciarPapelera;
    procedure PurgarCorreosAntiguos(dias: Integer);

    // Operaciones de consulta
    function ObtenerCorreosPorEstado(estado: TEstadoPapelera): TArray<TCorreoEliminado>;
    function ObtenerCorreosRecientes(horas: Integer): TArray<TCorreoEliminado>;
    function ContarPorRemitente(remitente: string): Integer;

    // Visualización
    procedure MostrarTodos;
    procedure MostrarCorreo(nodo: PNodoPila);
    procedure MostrarEstadisticas;
    procedure MostrarUltimosEliminados(cantidad: Integer);

    // Operaciones para reportes
    function GenerarReporte: string;
    function GenerarReporteHTML: string;
    function ListarCorreosArray: TArray<TCorreoEliminado>;

    // Utilidades
    procedure LimpiarPila;
    function ClonarPila: TPilaPapelera;
    function ObtenerEspacioUtilizado: string;

    // Validaciones y helpers
    function ValidarCorreo(correo: TCorreoEliminado): Boolean;
    function ObtenerTiempoTranscurrido(fechaEliminacion: TDateTime): string;
    function PuedeRestaurar(id: Integer): Boolean;
  end;

implementation

constructor TPilaPapelera.Create;
begin
  tope := nil;
  contador := 0;
  contadorID := 0;
end;

destructor TPilaPapelera.Destroy;
begin
  LimpiarPila;
  inherited Destroy;
end;

procedure TPilaPapelera.Apilar(correo: TCorreoEliminado);
var
  nuevoNodo: PNodoPila;
begin
  if not ValidarCorreo(correo) then
    raise Exception.Create('Datos del correo inválidos');

  // Crear nuevo nodo
  New(nuevoNodo);
  Inc(contadorID);
  correo.id := contadorID;
  correo.fechaEliminacion := Now;
  correo.estado := epEliminado;
  nuevoNodo^.datos := correo;
  nuevoNodo^.siguiente := tope;

  // Actualizar tope
  tope := nuevoNodo;
  Inc(contador);
end;

function TPilaPapelera.Desapilar: TCorreoEliminado;
var
  nodoAEliminar: PNodoPila;
begin
  if EstaVacia then
    raise Exception.Create('No se puede desapilar de una pila vacía');

  // Obtener datos del tope
  Result := tope^.datos;
  nodoAEliminar := tope;

  // Actualizar tope
  tope := tope^.siguiente;

  // Eliminar nodo
  Dispose(nodoAEliminar);
  Dec(contador);
end;

function TPilaPapelera.Tope: TCorreoEliminado;
begin
  if EstaVacia then
    raise Exception.Create('La pila está vacía');

  Result := tope^.datos;
end;

function TPilaPapelera.EstaVacia: Boolean;
begin
  Result := tope = nil;
end;

function TPilaPapelera.ObtenerCantidad: Integer;
begin
  Result := contador;
end;

function TPilaPapelera.BuscarPorAsunto(palabraClave: string): TArray<TCorreoEliminado>;
var
  actual: PNodoPila;
  correos: TArray<TCorreoEliminado>;
  count: Integer;
begin
  SetLength(correos, 0);
  count := 0;
  actual := tope;
  palabraClave := LowerCase(palabraClave);

  while actual <> nil do
  begin
    if Pos(palabraClave, LowerCase(actual^.datos.asunto)) > 0 then
    begin
      SetLength(correos, count + 1);
      correos[count] := actual^.datos;
      Inc(count);
    end;
    actual := actual^.siguiente;
  end;

  Result := correos;
end;

function TPilaPapelera.BuscarPorRemitente(remitente: string): TArray<TCorreoEliminado>;
var
  actual: PNodoPila;
  correos: TArray<TCorreoEliminado>;
  count: Integer;
begin
  SetLength(correos, 0);
  count := 0;
  actual := tope;

  while actual <> nil do
  begin
    if LowerCase(actual^.datos.remitente) = LowerCase(remitente) then
    begin
      SetLength(correos, count + 1);
      correos[count] := actual^.datos;
      Inc(count);
    end;
    actual := actual^.siguiente;
  end;

  Result := correos;
end;

function TPilaPapelera.BuscarPorID(id: Integer): PNodoPila;
var
  actual: PNodoPila;
begin
  Result := nil;
  actual := tope;

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

function TPilaPapelera.BuscarPorIDOriginal(idOriginal: Integer): PNodoPila;
var
  actual: PNodoPila;
begin
  Result := nil;
  actual := tope;

  while actual <> nil do
  begin
    if actual^.datos.idOriginal = idOriginal then
    begin
      Result := actual;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
end;

function TPilaPapelera.EliminarDefinitivamente(id: Integer): Boolean;
var
  actual, anterior: PNodoPila;
begin
  Result := False;
  actual := tope;
  anterior := nil;

  while actual <> nil do
  begin
    if actual^.datos.id = id then
    begin
      // Marcar como eliminado definitivamente antes de eliminar
      actual^.datos.estado := epEliminadoDefinitivo;

      // Actualizar enlaces
      if anterior = nil then
        tope := actual^.siguiente
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

function TPilaPapelera.RestaurarCorreo(id: Integer): TCorreoEliminado;
var
  actual, anterior: PNodoPila;
begin
  if not PuedeRestaurar(id) then
    raise Exception.Create('No se puede restaurar este correo');

  actual := tope;
  anterior := nil;

  while actual <> nil do
  begin
    if actual^.datos.id = id then
    begin
      // Marcar como restaurado
      actual^.datos.estado := epRestaurado;
      Result := actual^.datos;

      // Remover de la pila
      if anterior = nil then
        tope := actual^.siguiente
      else
        anterior^.siguiente := actual^.siguiente;

      Dispose(actual);
      Dec(contador);
      Exit;
    end;

    anterior := actual;
    actual := actual^.siguiente;
  end;

  raise Exception.Create('Correo no encontrado');
end;

procedure TPilaPapelera.VaciarPapelera;
begin
  LimpiarPila;
end;

procedure TPilaPapelera.PurgarCorreosAntiguos(dias: Integer);
var
  actual, siguiente: PNodoPila;
  anterior: PNodoPila;
  fechaLimite: TDateTime;
begin
  fechaLimite := IncDay(Now, -dias);
  actual := tope;
  anterior := nil;

  while actual <> nil do
  begin
    siguiente := actual^.siguiente;

    if actual^.datos.fechaEliminacion < fechaLimite then
    begin
      // Eliminar nodo
      if anterior = nil then
        tope := siguiente
      else
        anterior^.siguiente := siguiente;

      Dispose(actual);
      Dec(contador);
    end
    else
      anterior := actual;

    actual := siguiente;
  end;
end;

function TPilaPapelera.ObtenerCorreosPorEstado(estado: TEstadoPapelera): TArray<TCorreoEliminado>;
var
  actual: PNodoPila;
  correos: TArray<TCorreoEliminado>;
  count: Integer;
begin
  SetLength(correos, 0);
  count := 0;
  actual := tope;

  while actual <> nil do
  begin
    if actual^.datos.estado = estado then
    begin
      SetLength(correos, count + 1);
      correos[count] := actual^.datos;
      Inc(count);
    end;
    actual := actual^.siguiente;
  end;

  Result := correos;
end;

function TPilaPapelera.ObtenerCorreosRecientes(horas: Integer): TArray<TCorreoEliminado>;
var
  actual: PNodoPila;
  correos: TArray<TCorreoEliminado>;
  count: Integer;
  fechaLimite: TDateTime;
begin
  SetLength(correos, 0);
  count := 0;
  actual := tope;
  fechaLimite := IncHour(Now, -horas);

  while actual <> nil do
  begin
    if actual^.datos.fechaEliminacion >= fechaLimite then
    begin
      SetLength(correos, count + 1);
      correos[count] := actual^.datos;
      Inc(count);
    end;
    actual := actual^.siguiente;
  end;

  Result := correos;
end;

function TPilaPapelera.ContarPorRemitente(remitente: string): Integer;
var
  actual: PNodoPila;
begin
  Result := 0;
  actual := tope;

  while actual <> nil do
  begin
    if LowerCase(actual^.datos.remitente) = LowerCase(remitente) then
      Inc(Result);
    actual := actual^.siguiente;
  end;
end;

procedure TPilaPapelera.MostrarTodos;
var
  actual: PNodoPila;
  posicion: Integer;
begin
  if EstaVacia then
  begin
    WriteLn('La papelera está vacía');
    Exit;
  end;

  WriteLn('=== PAPELERA (PILA) ===');
  WriteLn('Total de correos eliminados: ', contador);

  actual := tope;
  posicion := 1;

  while actual <> nil do
  begin
    WriteLn('--- Posición ', posicion, ' (más reciente) ---');
    MostrarCorreo(actual);
    actual := actual^.siguiente;
    Inc(posicion);
  end;
end;

procedure TPilaPapelera.MostrarCorreo(nodo: PNodoPila);
var
  estadoStr: string;
begin
  if nodo <> nil then
  begin
    case nodo^.datos.estado of
      epEliminado: estadoStr := 'Eliminado';
      epRestaurado: estadoStr := 'Restaurado';
      epEliminadoDefinitivo: estadoStr := 'Eliminado Definitivamente';
    end;

    WriteLn('ID: ', nodo^.datos.id);
    WriteLn('ID Original: ', nodo^.datos.idOriginal);
    WriteLn('De: ', nodo^.datos.remitente);
    WriteLn('Para: ', nodo^.datos.destinatario);
    WriteLn('Asunto: ', nodo^.datos.asunto);
    WriteLn('Fecha original: ', DateTimeToStr(nodo^.datos.fechaOriginal));
    WriteLn('Fecha eliminación: ', DateTimeToStr(nodo^.datos.fechaEliminacion));
    WriteLn('Estado: ', estadoStr);
    WriteLn('Motivo eliminación: ', nodo^.datos.motivoEliminacion);
    WriteLn('Tiempo transcurrido: ', ObtenerTiempoTranscurrido(nodo^.datos.fechaEliminacion));
    WriteLn('------------------------');
  end;
end;

procedure TPilaPapelera.MostrarEstadisticas;
var
  eliminados, restaurados, definitivos: Integer;
  actual: PNodoPila;
begin
  eliminados := 0;
  restaurados := 0;
  definitivos := 0;

  actual := tope;
  while actual <> nil do
  begin
    case actual^.datos.estado of
      epEliminado: Inc(eliminados);
      epRestaurado: Inc(restaurados);
      epEliminadoDefinitivo: Inc(definitivos);
    end;
    actual := actual^.siguiente;
  end;

  WriteLn('=== ESTADÍSTICAS DE LA PAPELERA ===');
  WriteLn('Total de correos: ', contador);
  WriteLn('Eliminados: ', eliminados);
  WriteLn('Restaurados: ', restaurados);
  WriteLn('Eliminados definitivamente: ', definitivos);
  WriteLn('Espacio utilizado: ', ObtenerEspacioUtilizado);
end;

procedure TPilaPapelera.MostrarUltimosEliminados(cantidad: Integer);
var
  actual: PNodoPila;
  mostrados: Integer;
begin
  if EstaVacia then
  begin
    WriteLn('La papelera está vacía');
    Exit;
  end;

  WriteLn('=== ÚLTIMOS ', cantidad, ' CORREOS ELIMINADOS ===');

  actual := tope;
  mostrados := 0;

  while (actual <> nil) and (mostrados < cantidad) do
  begin
    WriteLn('--- Eliminado #', mostrados + 1, ' ---');
    MostrarCorreo(actual);
    actual := actual^.siguiente;
    Inc(mostrados);
  end;
end;

function TPilaPapelera.GenerarReporte: string;
var
  actual: PNodoPila;
  posicion: Integer;
  estadoStr: string;
begin
  Result := 'digraph PilaPapelera {' + LineEnding;
  Result := Result + '  rankdir=TB;' + LineEnding;
  Result := Result + '  node [shape=record];' + LineEnding;

  if EstaVacia then
  begin
    Result := Result + '  vacia [label="Papelera Vacía"];' + LineEnding;
  end
  else
  begin
    actual := tope;
    posicion := 1;

    // Agregar nodos
    while actual <> nil do
    begin
      case actual^.datos.estado of
        epEliminado: estadoStr := 'Eliminado';
        epRestaurado: estadoStr := 'Restaurado';
        epEliminadoDefinitivo: estadoStr := 'Definitivo';
      end;

      Result := Result + Format('  correo%d [label="Pos: %d (TOPE)|ID: %d|ID Orig: %d|De: %s|Para: %s|Asunto: %s|Estado: %s"',
        [actual^.datos.id, posicion, actual^.datos.id, actual^.datos.idOriginal,
         actual^.datos.remitente, actual^.datos.destinatario,
         actual^.datos.asunto, estadoStr]);

      // Colorear según estado
      case actual^.datos.estado of
        epEliminado: Result := Result + ', style=filled, fillcolor=lightgray';
        epRestaurado: Result := Result + ', style=filled, fillcolor=lightgreen';
        epEliminadoDefinitivo: Result := Result + ', style=filled, fillcolor=red';
      end;

      Result := Result + '];' + LineEnding;

      // Agregar flecha al siguiente
      if actual^.siguiente <> nil then
        Result := Result + Format('  correo%d -> correo%d;' + LineEnding,
          [actual^.datos.id, actual^.siguiente^.datos.id]);

      actual := actual^.siguiente;
      Inc(posicion);
    end;

    // Indicador del tope
    Result := Result + Format('  topeIndicador [label="TOPE", style=filled, fillcolor=yellow];' + LineEnding);
    Result := Result + Format('  topeIndicador -> correo%d [style=dashed];' + LineEnding, [tope^.datos.id]);
  end;

  Result := Result + '}' + LineEnding;
end;

function TPilaPapelera.GenerarReporteHTML: string;
var
  actual: PNodoPila;
  posicion: Integer;
  colorFondo, estadoStr: string;
begin
  Result := '<html><head><title>Papelera de Correos</title></head><body>' + LineEnding;
  Result := Result + '<h1>Papelera de Correos (Pila LIFO)</h1>' + LineEnding;
  Result := Result + '<p>Total de correos en papelera: ' + IntToStr(contador) + '</p>' + LineEnding;

  if EstaVacia then
  begin
    Result := Result + '<p>La papelera está vacía.</p>' + LineEnding;
  end
  else
  begin
    Result := Result + '<table border="1">' + LineEnding;
    Result := Result + '<tr><th>Posición</th><th>ID</th><th>ID Original</th><th>De</th><th>Para</th><th>Asunto</th><th>Fecha Eliminación</th><th>Estado</th><th>Motivo</th></tr>' + LineEnding;

    actual := tope;
    posicion := 1;

    while actual <> nil do
    begin
      case actual^.datos.estado of
        epEliminado: begin
          estadoStr := 'Eliminado';
          colorFondo := '#F0F0F0';
        end;
        epRestaurado: begin
          estadoStr := 'Restaurado';
          colorFondo := '#E6FFE6';
        end;
        epEliminadoDefinitivo: begin
          estadoStr := 'Definitivo';
          colorFondo := '#FFE6E6';
        end;
        else begin
          estadoStr := 'Desconocido';
          colorFondo := '#FFFFFF';
        end;
      end;

      Result := Result + Format('<tr style="background-color: %s">', [colorFondo]) + LineEnding;
      Result := Result + Format('<td>%d %s</td><td>%d</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>',
        [posicion, IfThen(posicion = 1, '(TOPE)', ''),
         actual^.datos.id, actual^.datos.idOriginal,
         actual^.datos.remitente, actual^.datos.destinatario,
         actual^.datos.asunto, DateTimeToStr(actual^.datos.fechaEliminacion),
         estadoStr, actual^.datos.motivoEliminacion]);
      Result := Result + '</tr>' + LineEnding;

      actual := actual^.siguiente;
      Inc(posicion);
    end;

    Result := Result + '</table>' + LineEnding;
  end;

  // Agregar estadísticas
  MostrarEstadisticas;

  Result := Result + '</body></html>' + LineEnding;
end;

function TPilaPapelera.ListarCorreosArray: TArray<TCorreoEliminado>;
var
  actual: PNodoPila;
  i: Integer;
begin
  SetLength(Result, contador);
  actual := tope;
  i := 0;

  while actual <> nil do
  begin
    Result[i] := actual^.datos;
    actual := actual^.siguiente;
    Inc(i);
  end;
end;

procedure TPilaPapelera.LimpiarPila;
var
  actual, siguiente: PNodoPila;
begin
  actual := tope;
  while actual <> nil do
  begin
    siguiente := actual^.siguiente;
    Dispose(actual);
    actual := siguiente;
  end;

  tope := nil;
  contador := 0;
end;

function TPilaPapelera.ClonarPila: TPilaPapelera;
var
  correos: TArray<TCorreoEliminado>;
  i: Integer;
begin
  Result := TPilaPapelera.Create;
  correos := ListarCorreosArray;

  // Apilar en orden inverso para mantener la misma estructura
  for i := Length(correos) - 1 downto 0 do
    Result.Apilar(correos[i]);
end;

function TPilaPapelera.ObtenerEspacioUtilizado: string;
var
  totalCaracteres: Integer;
  actual: PNodoPila;
begin
  totalCaracteres := 0;
  actual := tope;

  while actual <> nil do
  begin
    Inc(totalCaracteres, Length(actual^.datos.asunto));
    Inc(totalCaracteres, Length(actual^.datos.mensaje));
    Inc(totalCaracteres, Length(actual^.datos.remitente));
    Inc(totalCaracteres, Length(actual^.datos.destinatario));
    Inc(totalCaracteres, Length(actual^.datos.motivoEliminacion));
    actual := actual^.siguiente;
  end;

  if totalCaracteres < 1024 then
    Result := Format('%d bytes', [totalCaracteres])
  else if totalCaracteres < 1024 * 1024 then
    Result := Format('%.2f KB', [totalCaracteres / 1024])
  else
    Result := Format('%.2f MB', [totalCaracteres / (1024 * 1024)]);
end;

function TPilaPapelera.ValidarCorreo(correo: TCorreoEliminado): Boolean;
begin
  Result := (correo.idOriginal > 0) and
            (Length(correo.remitente) > 0) and
            (Length(correo.destinatario) > 0) and
            (Length(correo.asunto) > 0) and
            (correo.fechaOriginal > 0);
end;

function TPilaPapelera.ObtenerTiempoTranscurrido(fechaEliminacion: TDateTime): string;
var
  diferencia: Double;
  dias, horas, minutos: Integer;
begin
  diferencia := Now - fechaEliminacion;

  if diferencia < 0 then
  begin
    Result := 'Fecha inválida';
    Exit;
  end;

  dias := Trunc(diferencia);
  diferencia := diferencia - dias;
  horas := Trunc(diferencia * 24);
  diferencia := (diferencia * 24) - horas;
  minutos := Trunc(diferencia * 60);

  if dias > 0 then
    Result := Format('Hace %d días, %d horas', [dias, horas])
  else if horas > 0 then
    Result := Format('Hace %d horas, %d minutos', [horas, minutos])
  else if minutos > 0 then
    Result := Format('Hace %d minutos', [minutos])
  else
    Result := 'Hace menos de un minuto';
end;

function TPilaPapelera.PuedeRestaurar(id: Integer): Boolean;
var
  nodo: PNodoPila;
begin
  Result := False;
  nodo := BuscarPorID(id);

  if nodo <> nil then
    Result := nodo^.datos.estado = epEliminado;
end;

end.

