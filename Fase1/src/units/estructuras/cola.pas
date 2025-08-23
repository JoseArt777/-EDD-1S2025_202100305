unit cola;

{$mode objfpc}{$H+}

interface

uses SysUtils, DateUtils;

type
  // Registro para almacenar datos del correo programado
  TCorreoProgramado = record
    id: Integer;
    remitente: string;
    destinatario: string;
    asunto: string;
    mensaje: string;
    fechaCreacion: TDateTime;
    fechaProgramada: TDateTime;
    estado: string; // 'PROGRAMADO', 'ENVIADO', 'ERROR'
    intentosEnvio: Integer;
  end;

  // Array dinámico para compatibilidad
  TArrayCorreosProgramados = array of TCorreoProgramado;

  // Puntero al nodo de la cola
  PNodoCola = ^TNodoCola;

  // Nodo de la cola
  TNodoCola = record
    datos: TCorreoProgramado;
    siguiente: PNodoCola;
  end;

  // Clase para manejar la cola de correos programados
  TColaCorreosProgramados = class
  private
    frente_nodo: PNodoCola;    // Primer elemento (para desencollar) - RENOMBRADO
    final: PNodoCola;     // Último elemento (para encollar)
    contador: Integer;
    contadorID: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Operaciones básicas de cola
    procedure Encollar(correo: TCorreoProgramado);
    function Desencollar: TCorreoProgramado;
    function ObtenerFrente: TCorreoProgramado; // RENOMBRADO
    function EstaVacia: Boolean;
    function ObtenerCantidad: Integer;

    // Operaciones específicas para correos programados
    function ProcesarCorreosListos: TArrayCorreosProgramados; // CAMBIADO
    function ObtenerProximoEnvio: TDateTime;
    function BuscarPorID(id: Integer): PNodoCola;
    function EliminarPorID(id: Integer): Boolean;

    // Gestión de estados
    procedure MarcarComoEnviado(id: Integer);
    procedure MarcarComoError(id: Integer);
    procedure IncrementarIntentos(id: Integer);

    // Operaciones de consulta
    function ObtenerCorreosPorEstado(estado: string): TArrayCorreosProgramados; // CAMBIADO
    function ObtenerCorreosVencidos: TArrayCorreosProgramados; // CAMBIADO
    function ObtenerCorreosProximos(minutos: Integer): TArrayCorreosProgramados; // CAMBIADO

    // Ordenamiento por prioridad
    procedure OrdenarPorFechaProgramada;
    procedure ReorganizarCola;

    // Visualización y debug
    procedure MostrarTodos;
    procedure MostrarCorreo(nodo: PNodoCola);
    procedure MostrarEstadisticas;

    // Operaciones para reportes
    function GenerarReporte: string;
    function GenerarReporteHTML: string;
    function ListarCorreosArray: TArrayCorreosProgramados; // CAMBIADO

    // Utilidades
    procedure LimpiarCola;
    function ClonarCola: TColaCorreosProgramados;
    procedure PurgarCorreosEnviados;
    procedure PurgarCorreosViejos(dias: Integer);

    // Validaciones
    function ValidarFechaProgramada(fecha: TDateTime): Boolean;
    function ObtenerTiempoRestante(fechaProgramada: TDateTime): string;
  end;

implementation

constructor TColaCorreosProgramados.Create;
begin
  frente_nodo := nil;  // CAMBIADO
  final := nil;
  contador := 0;
  contadorID := 0;
end;

destructor TColaCorreosProgramados.Destroy;
begin
  LimpiarCola;
  inherited Destroy;
end;

procedure TColaCorreosProgramados.Encollar(correo: TCorreoProgramado);
var
  nuevoNodo: PNodoCola;
begin
  // Validar fecha programada
  if not ValidarFechaProgramada(correo.fechaProgramada) then
    raise Exception.Create('La fecha programada debe ser futura');

  // Crear nuevo nodo
  New(nuevoNodo);
  Inc(contadorID);
  correo.id := contadorID;
  correo.fechaCreacion := Now;
  correo.estado := 'PROGRAMADO';
  correo.intentosEnvio := 0;
  nuevoNodo^.datos := correo;
  nuevoNodo^.siguiente := nil;

  // Insertar en la cola
  if final = nil then
  begin
    // Primera inserción
    frente_nodo := nuevoNodo;  // CAMBIADO
    final := nuevoNodo;
  end
  else
  begin
    // Insertar al final
    final^.siguiente := nuevoNodo;
    final := nuevoNodo;
  end;

  Inc(contador);

  // Reorganizar para mantener orden por fecha programada
  OrdenarPorFechaProgramada;
end;

function TColaCorreosProgramados.Desencollar: TCorreoProgramado;
var
  nodoAEliminar: PNodoCola;
begin
  if EstaVacia then
    raise Exception.Create('No se puede desencollar de una cola vacía');

  // Obtener datos del frente
  Result := frente_nodo^.datos;  // CAMBIADO
  nodoAEliminar := frente_nodo;  // CAMBIADO

  // Actualizar frente
  frente_nodo := frente_nodo^.siguiente;  // CAMBIADO
  if frente_nodo = nil then  // CAMBIADO
    final := nil; // La cola quedó vacía

  // Eliminar nodo
  Dispose(nodoAEliminar);
  Dec(contador);
end;

function TColaCorreosProgramados.ObtenerFrente: TCorreoProgramado;  // RENOMBRADO
begin
  if EstaVacia then
    raise Exception.Create('La cola está vacía');

  Result := frente_nodo^.datos;  // CAMBIADO
end;

function TColaCorreosProgramados.EstaVacia: Boolean;
begin
  Result := frente_nodo = nil;  // CAMBIADO
end;

function TColaCorreosProgramados.ObtenerCantidad: Integer;
begin
  Result := contador;
end;

function TColaCorreosProgramados.ProcesarCorreosListos: TArrayCorreosProgramados;  // CAMBIADO
var
  correosListos: TArrayCorreosProgramados;  // CAMBIADO
  correoActual: TCorreoProgramado;
  count: Integer;
begin
  SetLength(correosListos, 0);
  count := 0;

  // Procesar todos los correos cuya fecha programada ya pasó
  while not EstaVacia do
  begin
    correoActual := ObtenerFrente;  // CAMBIADO

    // Si la fecha programada ya pasó o es ahora
    if correoActual.fechaProgramada <= Now then
    begin
      // Desencollar y agregar al array de listos
      correoActual := Desencollar;
      SetLength(correosListos, count + 1);
      correosListos[count] := correoActual;
      Inc(count);
    end
    else
      Break; // Los siguientes aún no están listos (cola ordenada)
  end;

  Result := correosListos;
end;

function TColaCorreosProgramados.ObtenerProximoEnvio: TDateTime;
begin
  if EstaVacia then
    Result := 0
  else
    Result := frente_nodo^.datos.fechaProgramada;  // CAMBIADO
end;

function TColaCorreosProgramados.BuscarPorID(id: Integer): PNodoCola;
var
  actual: PNodoCola;
begin
  Result := nil;
  actual := frente_nodo;  // CAMBIADO

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

function TColaCorreosProgramados.EliminarPorID(id: Integer): Boolean;
var
  actual, anterior: PNodoCola;
begin
  Result := False;
  actual := frente_nodo;  // CAMBIADO
  anterior := nil;

  while actual <> nil do
  begin
    if actual^.datos.id = id then
    begin
      // Actualizar enlaces
      if anterior = nil then
        frente_nodo := actual^.siguiente  // CAMBIADO
      else
        anterior^.siguiente := actual^.siguiente;

      if actual = final then
        final := anterior;

      Dispose(actual);
      Dec(contador);
      Result := True;
      Exit;
    end;

    anterior := actual;
    actual := actual^.siguiente;
  end;
end;

procedure TColaCorreosProgramados.MarcarComoEnviado(id: Integer);
var
  nodo: PNodoCola;
begin
  nodo := BuscarPorID(id);
  if nodo <> nil then
    nodo^.datos.estado := 'ENVIADO';
end;

procedure TColaCorreosProgramados.MarcarComoError(id: Integer);
var
  nodo: PNodoCola;
begin
  nodo := BuscarPorID(id);
  if nodo <> nil then
    nodo^.datos.estado := 'ERROR';
end;

procedure TColaCorreosProgramados.IncrementarIntentos(id: Integer);
var
  nodo: PNodoCola;
begin
  nodo := BuscarPorID(id);
  if nodo <> nil then
    Inc(nodo^.datos.intentosEnvio);
end;

function TColaCorreosProgramados.ObtenerCorreosPorEstado(estado: string): TArrayCorreosProgramados;  // CAMBIADO
var
  actual: PNodoCola;
  correos: TArrayCorreosProgramados;  // CAMBIADO
  count: Integer;
begin
  SetLength(correos, 0);
  count := 0;
  actual := frente_nodo;  // CAMBIADO

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

function TColaCorreosProgramados.ObtenerCorreosVencidos: TArrayCorreosProgramados;  // CAMBIADO
var
  actual: PNodoCola;
  correos: TArrayCorreosProgramados;  // CAMBIADO
  count: Integer;
begin
  SetLength(correos, 0);
  count := 0;
  actual := frente_nodo;  // CAMBIADO

  while actual <> nil do
  begin
    if (actual^.datos.fechaProgramada <= Now) and (actual^.datos.estado = 'PROGRAMADO') then
    begin
      SetLength(correos, count + 1);
      correos[count] := actual^.datos;
      Inc(count);
    end;
    actual := actual^.siguiente;
  end;

  Result := correos;
end;

function TColaCorreosProgramados.ObtenerCorreosProximos(minutos: Integer): TArrayCorreosProgramados;  // CAMBIADO
var
  actual: PNodoCola;
  correos: TArrayCorreosProgramados;  // CAMBIADO
  count: Integer;
  fechaLimite: TDateTime;
begin
  SetLength(correos, 0);
  count := 0;
  actual := frente_nodo;  // CAMBIADO
  fechaLimite := IncMinute(Now, minutos);

  while actual <> nil do
  begin
    if (actual^.datos.fechaProgramada <= fechaLimite) and
       (actual^.datos.fechaProgramada > Now) and
       (actual^.datos.estado = 'PROGRAMADO') then
    begin
      SetLength(correos, count + 1);
      correos[count] := actual^.datos;
      Inc(count);
    end;
    actual := actual^.siguiente;
  end;

  Result := correos;
end;

procedure TColaCorreosProgramados.OrdenarPorFechaProgramada;
var
  correos: TArrayCorreosProgramados;  // CAMBIADO
  i, j: Integer;
  temp: TCorreoProgramado;
begin
  if contador <= 1 then
    Exit;

  // Convertir cola a array
  correos := ListarCorreosArray;

  // Ordenar por fecha programada (más próximos primero)
  for i := 0 to Length(correos) - 2 do
    for j := 0 to Length(correos) - 2 - i do
      if correos[j].fechaProgramada > correos[j + 1].fechaProgramada then
      begin
        temp := correos[j];
        correos[j] := correos[j + 1];
        correos[j + 1] := temp;
      end;

  // Reconstruir la cola
  LimpiarCola;
  for i := 0 to Length(correos) - 1 do
  begin
    // Inserción directa sin validación para mantener orden
    var nuevoNodo: PNodoCola;
    New(nuevoNodo);
    nuevoNodo^.datos := correos[i];
    nuevoNodo^.siguiente := nil;

    if final = nil then
    begin
      frente_nodo := nuevoNodo;  // CAMBIADO
      final := nuevoNodo;
    end
    else
    begin
      final^.siguiente := nuevoNodo;
      final := nuevoNodo;
    end;
    Inc(contador);
  end;
end;

procedure TColaCorreosProgramados.ReorganizarCola;
begin
  OrdenarPorFechaProgramada;
end;

procedure TColaCorreosProgramados.MostrarTodos;
var
  actual: PNodoCola;
  posicion: Integer;
begin
  if EstaVacia then
  begin
    WriteLn('La cola de correos programados está vacía');
    Exit;
  end;

  WriteLn('=== COLA DE CORREOS PROGRAMADOS ===');
  WriteLn('Total de correos en cola: ', contador);

  actual := frente_nodo;  // CAMBIADO
  posicion := 1;

  while actual <> nil do
  begin
    WriteLn('--- Posición ', posicion, ' en la cola ---');
    MostrarCorreo(actual);
    actual := actual^.siguiente;
    Inc(posicion);
  end;
end;

procedure TColaCorreosProgramados.MostrarCorreo(nodo: PNodoCola);
begin
  if nodo <> nil then
  begin
    WriteLn('ID: ', nodo^.datos.id);
    WriteLn('De: ', nodo^.datos.remitente);
    WriteLn('Para: ', nodo^.datos.destinatario);
    WriteLn('Asunto: ', nodo^.datos.asunto);
    WriteLn('Fecha programada: ', DateTimeToStr(nodo^.datos.fechaProgramada));
    WriteLn('Estado: ', nodo^.datos.estado);
    WriteLn('Intentos de envío: ', nodo^.datos.intentosEnvio);
    WriteLn('Tiempo restante: ', ObtenerTiempoRestante(nodo^.datos.fechaProgramada));
    WriteLn('------------------------');
  end;
end;

procedure TColaCorreosProgramados.MostrarEstadisticas;
var
  programados, enviados, errores: Integer;
  actual: PNodoCola;
begin
  programados := 0;
  enviados := 0;
  errores := 0;

  actual := frente_nodo;  // CAMBIADO
  while actual <> nil do
  begin
    case actual^.datos.estado of
      'PROGRAMADO': Inc(programados);
      'ENVIADO': Inc(enviados);
      'ERROR': Inc(errores);
    end;
    actual := actual^.siguiente;
  end;

  WriteLn('=== ESTADÍSTICAS DE LA COLA ===');
  WriteLn('Total de correos: ', contador);
  WriteLn('Programados: ', programados);
  WriteLn('Enviados: ', enviados);
  WriteLn('Con errores: ', errores);

  if not EstaVacia then
    WriteLn('Próximo envío: ', DateTimeToStr(ObtenerProximoEnvio));
end;

function TColaCorreosProgramados.GenerarReporte: string;
var
  actual: PNodoCola;
  posicion: Integer;
begin
  Result := 'digraph ColaCorreosProgramados {' + LineEnding;
  Result := Result + '  rankdir=LR;' + LineEnding;
  Result := Result + '  node [shape=record];' + LineEnding;

  if EstaVacia then
  begin
    Result := Result + '  vacia [label="Cola Vacía"];' + LineEnding;
  end
  else
  begin
    actual := frente_nodo;  // CAMBIADO
    posicion := 1;

    // Agregar nodos
    while actual <> nil do
    begin
      Result := Result + Format('  correo%d [label="Pos: %d|ID: %d|De: %s|Para: %s|Asunto: %s|Programado: %s|Estado: %s"',
        [actual^.datos.id, posicion, actual^.datos.id, actual^.datos.remitente,
         actual^.datos.destinatario, actual^.datos.asunto,
         DateTimeToStr(actual^.datos.fechaProgramada), actual^.datos.estado]);

      // Colorear según estado
      case actual^.datos.estado of
        'PROGRAMADO': Result := Result + ', style=filled, fillcolor=lightblue';
        'ENVIADO': Result := Result + ', style=filled, fillcolor=lightgreen';
        'ERROR': Result := Result + ', style=filled, fillcolor=lightcoral';
      end;

      Result := Result + '];' + LineEnding;

      // Agregar flecha al siguiente
      if actual^.siguiente <> nil then
        Result := Result + Format('  correo%d -> correo%d;' + LineEnding,
          [actual^.datos.id, actual^.siguiente^.datos.id]);

      actual := actual^.siguiente;
      Inc(posicion);
    end;

    // Indicadores de frente y final
    Result := Result + Format('  frente [label="FRENTE", style=filled, fillcolor=yellow];' + LineEnding);
    Result := Result + Format('  final [label="FINAL", style=filled, fillcolor=orange];' + LineEnding);
    Result := Result + Format('  frente -> correo%d [style=dashed];' + LineEnding, [frente_nodo^.datos.id]);  // CAMBIADO
    Result := Result + Format('  final -> correo%d [style=dashed];' + LineEnding, [final^.datos.id]);
  end;

  Result := Result + '}' + LineEnding;
end;

function TColaCorreosProgramados.GenerarReporteHTML: string;
var
  actual: PNodoCola;
  posicion: Integer;
  colorFondo: string;
begin
  Result := '<html><head><title>Cola de Correos Programados</title></head><body>' + LineEnding;
  Result := Result + '<h1>Cola de Correos Programados (FIFO)</h1>' + LineEnding;
  Result := Result + '<p>Total de correos en cola: ' + IntToStr(contador) + '</p>' + LineEnding;

  if not EstaVacia then
    Result := Result + '<p>Próximo envío: ' + DateTimeToStr(ObtenerProximoEnvio) + '</p>' + LineEnding;

  if EstaVacia then
  begin
    Result := Result + '<p>La cola está vacía.</p>' + LineEnding;
  end
  else
  begin
    Result := Result + '<table border="1">' + LineEnding;
    Result := Result + '<tr><th>Posición</th><th>ID</th><th>De</th><th>Para</th><th>Asunto</th><th>Fecha Programada</th><th>Estado</th><th>Intentos</th></tr>' + LineEnding;

    actual := frente_nodo;  // CAMBIADO
    posicion := 1;

    while actual <> nil do
    begin
      case actual^.datos.estado of
        'PROGRAMADO': colorFondo := '#E6F3FF';
        'ENVIADO': colorFondo := '#E6FFE6';
        'ERROR': colorFondo := '#FFE6E6';
        else colorFondo := '#FFFFFF';
      end;

      Result := Result + Format('<tr style="background-color: %s">', [colorFondo]) + LineEnding;
      Result := Result + Format('<td>%d</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%d</td>',
        [posicion, actual^.datos.id, actual^.datos.remitente, actual^.datos.destinatario,
         actual^.datos.asunto, DateTimeToStr(actual^.datos.fechaProgramada),
         actual^.datos.estado, actual^.datos.intentosEnvio]);
      Result := Result + '</tr>' + LineEnding;

      actual := actual^.siguiente;
      Inc(posicion);
    end;

    Result := Result + '</table>' + LineEnding;
  end;

  Result := Result + '</body></html>' + LineEnding;
end;

function TColaCorreosProgramados.ListarCorreosArray: TArrayCorreosProgramados;  // CAMBIADO
var
  actual: PNodoCola;
  i: Integer;
begin
  SetLength(Result, contador);
  actual := frente_nodo;  // CAMBIADO
  i := 0;

  while actual <> nil do
  begin
    Result[i] := actual^.datos;
    actual := actual^.siguiente;
    Inc(i);
  end;
end;

procedure TColaCorreosProgramados.LimpiarCola;
var
  actual, siguiente: PNodoCola;
begin
  actual := frente_nodo;  // CAMBIADO
  while actual <> nil do
  begin
    siguiente := actual^.siguiente;
    Dispose(actual);
    actual := siguiente;
  end;

  frente_nodo := nil;  // CAMBIADO
  final := nil;
  contador := 0;
end;

function TColaCorreosProgramados.ClonarCola: TColaCorreosProgramados;
var
  actual: PNodoCola;
begin
  Result := TColaCorreosProgramados.Create;
  actual := frente_nodo;  // CAMBIADO

  while actual <> nil do
  begin
    Result.Encollar(actual^.datos);
    actual := actual^.siguiente;
  end;
end;

procedure TColaCorreosProgramados.PurgarCorreosEnviados;
var
  actual, siguiente: PNodoCola;
  anterior: PNodoCola;
begin
  actual := frente_nodo;  // CAMBIADO
  anterior := nil;

  while actual <> nil do
  begin
    siguiente := actual^.siguiente;

    if actual^.datos.estado = 'ENVIADO' then
    begin
      // Eliminar nodo
      if anterior = nil then
        frente_nodo := siguiente  // CAMBIADO
      else
        anterior^.siguiente := siguiente;

      if actual = final then
        final := anterior;

      Dispose(actual);
      Dec(contador);
    end
    else
      anterior := actual;

    actual := siguiente;
  end;
end;

procedure TColaCorreosProgramados.PurgarCorreosViejos(dias: Integer);
var
  actual, siguiente: PNodoCola;
  anterior: PNodoCola;
  fechaLimite: TDateTime;
begin
  fechaLimite := IncDay(Now, -dias);
  actual := frente_nodo;  // CAMBIADO
  anterior := nil;

  while actual <> nil do
  begin
    siguiente := actual^.siguiente;

    if actual^.datos.fechaCreacion < fechaLimite then
    begin
      // Eliminar nodo
      if anterior = nil then
        frente_nodo := siguiente  // CAMBIADO
      else
        anterior^.siguiente := siguiente;

      if actual = final then
        final := anterior;

      Dispose(actual);
      Dec(contador);
    end
    else
      anterior := actual;

    actual := siguiente;
  end;
end;

function TColaCorreosProgramados.ValidarFechaProgramada(fecha: TDateTime): Boolean;
begin
  Result := fecha > Now;
end;

function TColaCorreosProgramados.ObtenerTiempoRestante(fechaProgramada: TDateTime): string;
var
  diferencia: Double;
  dias, horas, minutos: Integer;
begin
  diferencia := fechaProgramada - Now;

  if diferencia <= 0 then
  begin
    Result := 'Vencido';
    Exit;
  end;

  dias := Trunc(diferencia);
  diferencia := diferencia - dias;
  horas := Trunc(diferencia * 24);
  diferencia := (diferencia * 24) - horas;
  minutos := Trunc(diferencia * 60);

  if dias > 0 then
    Result := Format('%dd %dh %dm', [dias, horas, minutos])
  else if horas > 0 then
    Result := Format('%dh %dm', [horas, minutos])
  else
    Result := Format('%dm', [minutos]);
end;

end.
