unit matriz_dispersa;

{$mode objfpc}{$H+}

interface

uses SysUtils, DateUtils;

type
  // Nodo para almacenar datos en la matriz dispersa
  PNodoMatriz = ^TNodoMatriz;
  TNodoMatriz = record
    fila: Integer;        // Índice del remitente
    columna: Integer;     // Índice del destinatario
    valor: Integer;       // Cantidad de correos enviados
    siguiente: PNodoMatriz; // Siguiente en la fila
    abajo: PNodoMatriz;     // Siguiente en la columna
  end;

  // Nodo cabecera para filas y columnas
  PNodoCabecera = ^TNodoCabecera;
  TNodoCabecera = record
    indice: Integer;
    email: string;
    siguiente: PNodoCabecera;
    acceso: PNodoMatriz;  // Primer nodo de la fila/columna
  end;

  // Registro para estadísticas
  TEstadisticaUsuario = record
    email: string;
    enviados: Integer;
    recibidos: Integer;
  end;

  // Clase principal de la matriz dispersa
  TMatrizDispersa = class
  private
    cabeceras_filas: PNodoCabecera;     // Cabeceras de filas (remitentes)
    cabeceras_columnas: PNodoCabecera;   // Cabeceras de columnas (destinatarios)
    contador_usuarios: Integer;
    total_correos: Integer;

    // Métodos privados para manejo de cabeceras
    function CrearCabecera(indice: Integer; email: string): PNodoCabecera;
    function BuscarCabeceraFila(email: string): PNodoCabecera;
    function BuscarCabeceraColumna(email: string): PNodoCabecera;
    function ObtenerIndiceFila(email: string): Integer;
    function ObtenerIndiceColumna(email: string): Integer;
    function InsertarCabeceraFila(email: string): PNodoCabecera;
    function InsertarCabeceraColumna(email: string): PNodoCabecera;

    // Métodos privados para manejo de nodos
    function CrearNodoMatriz(fila, columna, valor: Integer): PNodoMatriz;
    function BuscarNodo(fila, columna: Integer): PNodoMatriz;
    procedure InsertarNodoEnFila(cabecera_fila: PNodoCabecera; nuevo_nodo: PNodoMatriz);
    procedure InsertarNodoEnColumna(cabecera_columna: PNodoCabecera; nuevo_nodo: PNodoMatriz);

    // Métodos auxiliares
    function ContarCabecerasFila: Integer;
    function ContarCabecerasColumna: Integer;
    procedure OrdenarCabeceras;

  public
    constructor Create;
    destructor Destroy; override;

    // Operaciones principales
    procedure AgregarRelacion(emailRemitente, emailDestinatario: string);
    procedure IncrementarRelacion(emailRemitente, emailDestinatario: string);
    function ObtenerCantidadCorreos(emailRemitente, emailDestinatario: string): Integer;
    function ExisteRelacion(emailRemitente, emailDestinatario: string): Boolean;

    // Operaciones de consulta
    function ObtenerTotalCorreos: Integer;
    function ObtenerCorreosEnviados(emailRemitente: string): Integer;
    function ObtenerCorreosRecibidos(emailDestinatario: string): Integer;
    function ObtenerUsuarioMasActivo: string;
    function ObtenerTopRemitentesByEnviados(cantidad: Integer): TArray<TEstadisticaUsuario>;
    function ObtenerTopDestinatariosByRecibidos(cantidad: Integer): TArray<TEstadisticaUsuario>;

    // Visualización y debug
    procedure MostrarMatriz;
    procedure MostrarEstadisticas;
    procedure MostrarRelacionesUsuario(email: string);

    // Operaciones para reportes
    function GenerarReporteGraphviz: string;
    function GenerarReporteHTML: string;
    function GenerarReporteDetallado: string;
    function ExportarCSV: string;

    // Utilidades
    procedure LimpiarMatriz;
    function ClonarMatriz: TMatrizDispersa;
    function ObtenerListaUsuarios: TArray<string>;
    procedure ImportarRelaciones(relaciones: TArray<string>);

    // Validaciones
    function ValidarEmail(email: string): Boolean;
    function ObtenerDensidadMatriz: Double;
    function EstaVacia: Boolean;
  end;

implementation

constructor TMatrizDispersa.Create;
begin
  cabeceras_filas := nil;
  cabeceras_columnas := nil;
  contador_usuarios := 0;
  total_correos := 0;
end;

destructor TMatrizDispersa.Destroy;
begin
  LimpiarMatriz;
  inherited Destroy;
end;

function TMatrizDispersa.CrearCabecera(indice: Integer; email: string): PNodoCabecera;
var
  nueva_cabecera: PNodoCabecera;
begin
  New(nueva_cabecera);
  nueva_cabecera^.indice := indice;
  nueva_cabecera^.email := LowerCase(email);
  nueva_cabecera^.siguiente := nil;
  nueva_cabecera^.acceso := nil;
  Result := nueva_cabecera;
end;

function TMatrizDispersa.BuscarCabeceraFila(email: string): PNodoCabecera;
var
  actual: PNodoCabecera;
begin
  Result := nil;
  actual := cabeceras_filas;
  email := LowerCase(email);

  while actual <> nil do
  begin
    if actual^.email = email then
    begin
      Result := actual;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
end;

function TMatrizDispersa.BuscarCabeceraColumna(email: string): PNodoCabecera;
var
  actual: PNodoCabecera;
begin
  Result := nil;
  actual := cabeceras_columnas;
  email := LowerCase(email);

  while actual <> nil do
  begin
    if actual^.email = email then
    begin
      Result := actual;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
end;

function TMatrizDispersa.ObtenerIndiceFila(email: string): Integer;
var
  cabecera: PNodoCabecera;
begin
  cabecera := BuscarCabeceraFila(email);
  if cabecera <> nil then
    Result := cabecera^.indice
  else
    Result := -1;
end;

function TMatrizDispersa.ObtenerIndiceColumna(email: string): Integer;
var
  cabecera: PNodoCabecera;
begin
  cabecera := BuscarCabeceraColumna(email);
  if cabecera <> nil then
    Result := cabecera^.indice
  else
    Result := -1;
end;

function TMatrizDispersa.InsertarCabeceraFila(email: string): PNodoCabecera;
var
  nueva_cabecera, actual, anterior: PNodoCabecera;
begin
  nueva_cabecera := CrearCabecera(ContarCabecerasFila, email);

  if cabeceras_filas = nil then
  begin
    cabeceras_filas := nueva_cabecera;
  end
  else
  begin
    // Insertar ordenado por email
    actual := cabeceras_filas;
    anterior := nil;

    while (actual <> nil) and (actual^.email < nueva_cabecera^.email) do
    begin
      anterior := actual;
      actual := actual^.siguiente;
    end;

    if anterior = nil then
    begin
      nueva_cabecera^.siguiente := cabeceras_filas;
      cabeceras_filas := nueva_cabecera;
    end
    else
    begin
      nueva_cabecera^.siguiente := actual;
      anterior^.siguiente := nueva_cabecera;
    end;
  end;

  Result := nueva_cabecera;
end;

function TMatrizDispersa.InsertarCabeceraColumna(email: string): PNodoCabecera;
var
  nueva_cabecera, actual, anterior: PNodoCabecera;
begin
  nueva_cabecera := CrearCabecera(ContarCabecerasColumna, email);

  if cabeceras_columnas = nil then
  begin
    cabeceras_columnas := nueva_cabecera;
  end
  else
  begin
    // Insertar ordenado por email
    actual := cabeceras_columnas;
    anterior := nil;

    while (actual <> nil) and (actual^.email < nueva_cabecera^.email) do
    begin
      anterior := actual;
      actual := actual^.siguiente;
    end;

    if anterior = nil then
    begin
      nueva_cabecera^.siguiente := cabeceras_columnas;
      cabeceras_columnas := nueva_cabecera;
    end
    else
    begin
      nueva_cabecera^.siguiente := actual;
      anterior^.siguiente := nueva_cabecera;
    end;
  end;

  Result := nueva_cabecera;
end;

function TMatrizDispersa.CrearNodoMatriz(fila, columna, valor: Integer): PNodoMatriz;
var
  nuevo_nodo: PNodoMatriz;
begin
  New(nuevo_nodo);
  nuevo_nodo^.fila := fila;
  nuevo_nodo^.columna := columna;
  nuevo_nodo^.valor := valor;
  nuevo_nodo^.siguiente := nil;
  nuevo_nodo^.abajo := nil;
  Result := nuevo_nodo;
end;

function TMatrizDispersa.BuscarNodo(fila, columna: Integer): PNodoMatriz;
var
  cabecera_fila: PNodoCabecera;
  actual: PNodoMatriz;
begin
  Result := nil;

  // Buscar la cabecera de fila
  cabecera_fila := cabeceras_filas;
  while (cabecera_fila <> nil) and (cabecera_fila^.indice <> fila) do
    cabecera_fila := cabecera_fila^.siguiente;

  if cabecera_fila = nil then
    Exit;

  // Buscar en la fila
  actual := cabecera_fila^.acceso;
  while actual <> nil do
  begin
    if actual^.columna = columna then
    begin
      Result := actual;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
end;

procedure TMatrizDispersa.InsertarNodoEnFila(cabecera_fila: PNodoCabecera; nuevo_nodo: PNodoMatriz);
var
  actual, anterior: PNodoMatriz;
begin
  if cabecera_fila^.acceso = nil then
  begin
    cabecera_fila^.acceso := nuevo_nodo;
  end
  else
  begin
    actual := cabecera_fila^.acceso;
    anterior := nil;

    // Insertar ordenado por columna
    while (actual <> nil) and (actual^.columna < nuevo_nodo^.columna) do
    begin
      anterior := actual;
      actual := actual^.siguiente;
    end;

    if anterior = nil then
    begin
      nuevo_nodo^.siguiente := cabecera_fila^.acceso;
      cabecera_fila^.acceso := nuevo_nodo;
    end
    else
    begin
      nuevo_nodo^.siguiente := actual;
      anterior^.siguiente := nuevo_nodo;
    end;
  end;
end;

procedure TMatrizDispersa.InsertarNodoEnColumna(cabecera_columna: PNodoCabecera; nuevo_nodo: PNodoMatriz);
var
  actual, anterior: PNodoMatriz;
begin
  if cabecera_columna^.acceso = nil then
  begin
    cabecera_columna^.acceso := nuevo_nodo;
  end
  else
  begin
    actual := cabecera_columna^.acceso;
    anterior := nil;

    // Insertar ordenado por fila
    while (actual <> nil) and (actual^.fila < nuevo_nodo^.fila) do
    begin
      anterior := actual;
      actual := actual^.abajo;
    end;

    if anterior = nil then
    begin
      nuevo_nodo^.abajo := cabecera_columna^.acceso;
      cabecera_columna^.acceso := nuevo_nodo;
    end
    else
    begin
      nuevo_nodo^.abajo := actual;
      anterior^.abajo := nuevo_nodo;
    end;
  end;
end;

function TMatrizDispersa.ContarCabecerasFila: Integer;
var
  actual: PNodoCabecera;
  count: Integer;
begin
  count := 0;
  actual := cabeceras_filas;
  while actual <> nil do
  begin
    Inc(count);
    actual := actual^.siguiente;
  end;
  Result := count;
end;

function TMatrizDispersa.ContarCabecerasColumna: Integer;
var
  actual: PNodoCabecera;
  count: Integer;
begin
  count := 0;
  actual := cabeceras_columnas;
  while actual <> nil do
  begin
    Inc(count);
    actual := actual^.siguiente;
  end;
  Result := count;
end;

procedure TMatrizDispersa.OrdenarCabeceras;
begin
  // Las cabeceras ya se insertan ordenadas, pero podríamos implementar
  // un reordenamiento si fuera necesario
end;

procedure TMatrizDispersa.AgregarRelacion(emailRemitente, emailDestinatario: string);
var
  cabecera_fila, cabecera_columna: PNodoCabecera;
  indice_fila, indice_columna: Integer;
  nodo_existente, nuevo_nodo: PNodoMatriz;
begin
  if not ValidarEmail(emailRemitente) or not ValidarEmail(emailDestinatario) then
    raise Exception.Create('Emails inválidos');

  // Obtener o crear cabeceras
  cabecera_fila := BuscarCabeceraFila(emailRemitente);
  if cabecera_fila = nil then
    cabecera_fila := InsertarCabeceraFila(emailRemitente);

  cabecera_columna := BuscarCabeceraColumna(emailDestinatario);
  if cabecera_columna = nil then
    cabecera_columna := InsertarCabeceraColumna(emailDestinatario);

  indice_fila := cabecera_fila^.indice;
  indice_columna := cabecera_columna^.indice;

  // Buscar si ya existe la relación
  nodo_existente := BuscarNodo(indice_fila, indice_columna);

  if nodo_existente <> nil then
  begin
    // Incrementar el valor existente
    Inc(nodo_existente^.valor);
  end
  else
  begin
    // Crear nuevo nodo con valor 1
    nuevo_nodo := CrearNodoMatriz(indice_fila, indice_columna, 1);

    // Insertar en fila y columna
    InsertarNodoEnFila(cabecera_fila, nuevo_nodo);
    InsertarNodoEnColumna(cabecera_columna, nuevo_nodo);
  end;

  Inc(total_correos);
end;

procedure TMatrizDispersa.IncrementarRelacion(emailRemitente, emailDestinatario: string);
begin
  AgregarRelacion(emailRemitente, emailDestinatario);
end;

function TMatrizDispersa.ObtenerCantidadCorreos(emailRemitente, emailDestinatario: string): Integer;
var
  indice_fila, indice_columna: Integer;
  nodo: PNodoMatriz;
begin
  Result := 0;

  indice_fila := ObtenerIndiceFila(emailRemitente);
  indice_columna := ObtenerIndiceColumna(emailDestinatario);

  if (indice_fila = -1) or (indice_columna = -1) then
    Exit;

  nodo := BuscarNodo(indice_fila, indice_columna);
  if nodo <> nil then
    Result := nodo^.valor;
end;

function TMatrizDispersa.ExisteRelacion(emailRemitente, emailDestinatario: string): Boolean;
begin
  Result := ObtenerCantidadCorreos(emailRemitente, emailDestinatario) > 0;
end;

function TMatrizDispersa.ObtenerTotalCorreos: Integer;
begin
  Result := total_correos;
end;

function TMatrizDispersa.ObtenerCorreosEnviados(emailRemitente: string): Integer;
var
  cabecera_fila: PNodoCabecera;
  actual: PNodoMatriz;
begin
  Result := 0;
  cabecera_fila := BuscarCabeceraFila(emailRemitente);

  if cabecera_fila = nil then
    Exit;

  actual := cabecera_fila^.acceso;
  while actual <> nil do
  begin
    Inc(Result, actual^.valor);
    actual := actual^.siguiente;
  end;
end;

function TMatrizDispersa.ObtenerCorreosRecibidos(emailDestinatario: string): Integer;
var
  cabecera_columna: PNodoCabecera;
  actual: PNodoMatriz;
begin
  Result := 0;
  cabecera_columna := BuscarCabeceraColumna(emailDestinatario);

  if cabecera_columna = nil then
    Exit;

  actual := cabecera_columna^.acceso;
  while actual <> nil do
  begin
    Inc(Result, actual^.valor);
    actual := actual^.abajo;
  end;
end;

function TMatrizDispersa.ObtenerUsuarioMasActivo: string;
var
  cabecera: PNodoCabecera;
  max_correos, correos_usuario: Integer;
begin
  Result := '';
  max_correos := 0;

  cabecera := cabeceras_filas;
  while cabecera <> nil do
  begin
    correos_usuario := ObtenerCorreosEnviados(cabecera^.email);
    if correos_usuario > max_correos then
    begin
      max_correos := correos_usuario;
      Result := cabecera^.email;
    end;
    cabecera := cabecera^.siguiente;
  end;
end;

function TMatrizDispersa.ObtenerTopRemitentesByEnviados(cantidad: Integer): TArray<TEstadisticaUsuario>;
var
  estadisticas: TArray<TEstadisticaUsuario>;
  cabecera: PNodoCabecera;
  count, i, j: Integer;
  temp: TEstadisticaUsuario;
begin
  SetLength(estadisticas, 0);
  count := 0;

  cabecera := cabeceras_filas;
  while cabecera <> nil do
  begin
    SetLength(estadisticas, count + 1);
    estadisticas[count].email := cabecera^.email;
    estadisticas[count].enviados := ObtenerCorreosEnviados(cabecera^.email);
    estadisticas[count].recibidos := ObtenerCorreosRecibidos(cabecera^.email);
    Inc(count);
    cabecera := cabecera^.siguiente;
  end;

  // Ordenar por enviados (descendente)
  for i := 0 to Length(estadisticas) - 2 do
    for j := 0 to Length(estadisticas) - 2 - i do
      if estadisticas[j].enviados < estadisticas[j + 1].enviados then
      begin
        temp := estadisticas[j];
        estadisticas[j] := estadisticas[j + 1];
        estadisticas[j + 1] := temp;
      end;

  // Limitar a la cantidad solicitada
  if Length(estadisticas) > cantidad then
    SetLength(estadisticas, cantidad);

  Result := estadisticas;
end;

function TMatrizDispersa.ObtenerTopDestinatariosByRecibidos(cantidad: Integer): TArray<TEstadisticaUsuario>;
var
  estadisticas: TArray<TEstadisticaUsuario>;
  cabecera: PNodoCabecera;
  count, i, j: Integer;
  temp: TEstadisticaUsuario;
begin
  SetLength(estadisticas, 0);
  count := 0;

  cabecera := cabeceras_columnas;
  while cabecera <> nil do
  begin
    SetLength(estadisticas, count + 1);
    estadisticas[count].email := cabecera^.email;
    estadisticas[count].enviados := ObtenerCorreosEnviados(cabecera^.email);
    estadisticas[count].recibidos := ObtenerCorreosRecibidos(cabecera^.email);
    Inc(count);
    cabecera := cabecera^.siguiente;
  end;

  // Ordenar por recibidos (descendente)
  for i := 0 to Length(estadisticas) - 2 do
    for j := 0 to Length(estadisticas) - 2 - i do
      if estadisticas[j].recibidos < estadisticas[j + 1].recibidos then
      begin
        temp := estadisticas[j];
        estadisticas[j] := estadisticas[j + 1];
        estadisticas[j + 1] := temp;
      end;

  // Limitar a la cantidad solicitada
  if Length(estadisticas) > cantidad then
    SetLength(estadisticas, cantidad);

  Result := estadisticas;
end;

procedure TMatrizDispersa.MostrarMatriz;
var
  cabecera_fila, cabecera_columna: PNodoCabecera;
  nodo: PNodoMatriz;
  cantidad: Integer;
begin
  if EstaVacia then
  begin
    WriteLn('La matriz está vacía');
    Exit;
  end;

  WriteLn('=== MATRIZ DISPERSA DE RELACIONES ===');
  WriteLn('Total de correos: ', total_correos);
  WriteLn;

  // Mostrar cabeceras de columnas
  Write('Remitente\Destinatario');
  cabecera_columna := cabeceras_columnas;
  while cabecera_columna <> nil do
  begin
    Write(Format('%15s', [cabecera_columna^.email]));
    cabecera_columna := cabecera_columna^.siguiente;
  end;
  WriteLn;

  // Mostrar filas
  cabecera_fila := cabeceras_filas;
  while cabecera_fila <> nil do
  begin
    Write(Format('%20s', [cabecera_fila^.email]));

    cabecera_columna := cabeceras_columnas;
    while cabecera_columna <> nil do
    begin
      cantidad := ObtenerCantidadCorreos(cabecera_fila^.email, cabecera_columna^.email);
      Write(Format('%15d', [cantidad]));
      cabecera_columna := cabecera_columna^.siguiente;
    end;
    WriteLn;

    cabecera_fila := cabecera_fila^.siguiente;
  end;
end;

procedure TMatrizDispersa.MostrarEstadisticas;
var
  cabecera: PNodoCabecera;
  enviados, recibidos: Integer;
begin
  WriteLn('=== ESTADÍSTICAS DE LA MATRIZ ===');
  WriteLn('Total de usuarios: ', ContarCabecerasFila);
  WriteLn('Total de correos: ', total_correos);
  WriteLn('Usuario más activo: ', ObtenerUsuarioMasActivo);
  WriteLn('Densidad de la matriz: ', FormatFloat('0.00%', ObtenerDensidadMatriz * 100));
  WriteLn;

  WriteLn('Estadísticas por usuario:');
  cabecera := cabeceras_filas;
  while cabecera <> nil do
  begin
    enviados := ObtenerCorreosEnviados(cabecera^.email);
    recibidos := ObtenerCorreosRecibidos(cabecera^.email);
    WriteLn(Format('%s: Enviados=%d, Recibidos=%d', [cabecera^.email, enviados, recibidos]));
    cabecera := cabecera^.siguiente;
  end;
end;

procedure TMatrizDispersa.MostrarRelacionesUsuario(email: string);
var
  cabecera_fila: PNodoCabecera;
  cabecera_columna: PNodoCabecera;
  actual: PNodoMatriz;
  email_columna: string;
begin
  WriteLn('=== RELACIONES DEL USUARIO: ', email, ' ===');

  // Mostrar correos enviados
  WriteLn('Correos enviados:');
  cabecera_fila := BuscarCabeceraFila(email);
  if cabecera_fila <> nil then
  begin
    actual := cabecera_fila^.acceso;
    while actual <> nil do
    begin
      // Buscar email de la columna
      cabecera_columna := cabeceras_columnas;
      while (cabecera_columna <> nil) and (cabecera_columna^.indice <> actual^.columna) do
        cabecera_columna := cabecera_columna^.siguiente;

      if cabecera_columna <> nil then
        WriteLn(Format('  -> %s: %d correos', [cabecera_columna^.email, actual^.valor]));

      actual := actual^.siguiente;
    end;
  end
  else
    WriteLn('  No ha enviado correos');

  WriteLn;

  // Mostrar correos recibidos
  WriteLn('Correos recibidos:');
  cabecera_columna := BuscarCabeceraColumna(email);
  if cabecera_columna <> nil then
  begin
    actual := cabecera_columna^.acceso;
    while actual <> nil do
    begin
      // Buscar email de la fila
      cabecera_fila := cabeceras_filas;
      while (cabecera_fila <> nil) and (cabecera_fila^.indice <> actual^.fila) do
        cabecera_fila := cabecera_fila^.siguiente;

      if cabecera_fila <> nil then
        WriteLn(Format('  <- %s: %d correos', [cabecera_fila^.email, actual^.valor]));

      actual := actual^.abajo;
    end;
  end
  else
    WriteLn('  No ha recibido correos');
end;

function TMatrizDispersa.GenerarReporteGraphviz: string;
var
  cabecera_fila, cabecera_columna: PNodoCabecera;
  actual: PNodoMatriz;
begin
  Result := 'digraph MatrizDispersa {' + LineEnding;
  Result := Result + '  rankdir=LR;' + LineEnding;
  Result := Result + '  node [shape=box];' + LineEnding;
  Result := Result + '  edge [fontsize=10];' + LineEnding;
  Result := Result + LineEnding;

  if EstaVacia then
  begin
    Result := Result + '  vacia [label="Matriz Vacía"];' + LineEnding;
  end
  else
  begin
    // Agregar nodos (usuarios)
    cabecera_fila := cabeceras_filas;
    while cabecera_fila <> nil do
    begin
      Result := Result + Format('  "%s" [style=filled, fillcolor=lightblue];' + LineEnding,
        [cabecera_fila^.email]);
      cabecera_fila := cabecera_fila^.siguiente;
    end;

    Result := Result + LineEnding;

    // Agregar aristas (relaciones)
    cabecera_fila := cabeceras_filas;
    while cabecera_fila <> nil do
    begin
      actual := cabecera_fila^.acceso;
      while actual <> nil do
      begin
        // Buscar email de destinatario
        cabecera_columna := cabeceras_columnas;
        while (cabecera_columna <> nil) and (cabecera_columna^.indice <> actual^.columna) do
          cabecera_columna := cabecera_columna^.siguiente;

        if cabecera_columna <> nil then
        begin
          Result := Result + Format('  "%s" -> "%s" [label="%d", weight=%d];' + LineEnding,
            [cabecera_fila^.email, cabecera_columna^.email, actual^.valor, actual^.valor]);
        end;

        actual := actual^.siguiente;
      end;
      cabecera_fila := cabecera_fila^.siguiente;
    end;
  end;

  Result := Result + '}' + LineEnding;
end;

function TMatrizDispersa.GenerarReporteHTML: string;
var
  cabecera_fila, cabecera_columna: PNodoCabecera;
  cantidad: Integer;
  color: string;
begin
  Result := '<html><head><title>Matriz de Relaciones</title>' + LineEnding;
  Result := Result + '<style>' + LineEnding;
  Result := Result + 'table { border-collapse: collapse; margin: 20px; }' + LineEnding;
  Result := Result + 'th, td { border: 1px solid #ddd; padding: 8px; text-align: center; }' + LineEnding;
  Result := Result + 'th { background-color: #f2f2f2; font-weight: bold; }' + LineEnding;
  Result := Result + '.zero { background-color: #f9f9f9; color: #ccc; }' + LineEnding;
  Result := Result + '.low { background-color: #fff2cc; }' + LineEnding;
  Result := Result + '.medium { background-color: #ffcc99; }' + LineEnding;
  Result := Result + '.high { background-color: #ff9999; }' + LineEnding;
  Result := Result + '</style></head><body>' + LineEnding;

  Result := Result + '<h1>Matriz de Relaciones - EDDMail</h1>' + LineEnding;
  Result := Result + '<p>Total de correos enviados: ' + IntToStr(total_correos) + '</p>' + LineEnding;
  Result := Result + '<p>Total de usuarios: ' + IntToStr(ContarCabecerasFila) + '</p>' + LineEnding;

  if EstaVacia then
  begin
    Result := Result + '<p>No hay datos para mostrar.</p>' + LineEnding;
  end
  else
  begin
    Result := Result + '<table>' + LineEnding;

    // Cabeceras de columnas
    Result := Result + '<tr><th>Remitente \ Destinatario</th>';
    cabecera_columna := cabeceras_columnas;
    while cabecera_columna <> nil do
    begin
      Result := Result + '<th>' + cabecera_columna^.email + '</th>';
      cabecera_columna := cabecera_columna^.siguiente;
    end;
    Result := Result + '</tr>' + LineEnding;

    // Filas de datos
    cabecera_fila := cabeceras_filas;
    while cabecera_fila <> nil do
    begin
      Result := Result + '<tr><th>' + cabecera_fila^.email + '</th>';

      cabecera_columna := cabeceras_columnas;
      while cabecera_columna <> nil do
      begin
        cantidad := ObtenerCantidadCorreos(cabecera_fila^.email, cabecera_columna^.email);

        // Determinar color basado en cantidad
        if cantidad = 0 then
          color := 'zero'
        else if cantidad <= 2 then
          color := 'low'
        else if cantidad <= 5 then
          color := 'medium'
        else
          color := 'high';

        Result := Result + Format('<td class="%s">%d</td>', [color, cantidad]);
        cabecera_columna := cabecera_columna^.siguiente;
      end;

      Result := Result + '</tr>' + LineEnding;
      cabecera_fila := cabecera_fila^.siguiente;
    end;

    Result := Result + '</table>' + LineEnding;
  end;

  Result := Result + '</body></html>' + LineEnding;
end;

function TMatrizDispersa.GenerarReporteDetallado: string;
var
  cabecera: PNodoCabecera;
  estadisticas: TArray<TEstadisticaUsuario>;
  i: Integer;
begin
  Result := '=== REPORTE DETALLADO DE MATRIZ DISPERSA ===' + LineEnding;
  Result := Result + 'Fecha: ' + DateTimeToStr(Now) + LineEnding;
  Result := Result + 'Total de usuarios: ' + IntToStr(ContarCabecerasFila) + LineEnding;
  Result := Result + 'Total de correos: ' + IntToStr(total_correos) + LineEnding;
  Result := Result + 'Densidad de matriz: ' + FormatFloat('0.00%', ObtenerDensidadMatriz * 100) + LineEnding;
  Result := Result + LineEnding;

  if not EstaVacia then
  begin
    Result := Result + 'TOP 5 REMITENTES MÁS ACTIVOS:' + LineEnding;
    estadisticas := ObtenerTopRemitentesByEnviados(5);
    for i := 0 to Length(estadisticas) - 1 do
    begin
      Result := Result + Format('%d. %s - %d correos enviados' + LineEnding,
        [i + 1, estadisticas[i].email, estadisticas[i].enviados]);
    end;

    Result := Result + LineEnding + 'TOP 5 DESTINATARIOS MÁS POPULARES:' + LineEnding;
    estadisticas := ObtenerTopDestinatariosByRecibidos(5);
    for i := 0 to Length(estadisticas) - 1 do
    begin
      Result := Result + Format('%d. %s - %d correos recibidos' + LineEnding,
        [i + 1, estadisticas[i].email, estadisticas[i].recibidos]);
    end;
  end;

  Result := Result + LineEnding + '=== FIN DEL REPORTE ===' + LineEnding;
end;

function TMatrizDispersa.ExportarCSV: string;
var
  cabecera_fila, cabecera_columna: PNodoCabecera;
  cantidad: Integer;
begin
  Result := 'Remitente,Destinatario,Cantidad' + LineEnding;

  cabecera_fila := cabeceras_filas;
  while cabecera_fila <> nil do
  begin
    cabecera_columna := cabeceras_columnas;
    while cabecera_columna <> nil do
    begin
      cantidad := ObtenerCantidadCorreos(cabecera_fila^.email, cabecera_columna^.email);
      if cantidad > 0 then
      begin
        Result := Result + Format('%s,%s,%d' + LineEnding,
          [cabecera_fila^.email, cabecera_columna^.email, cantidad]);
      end;
      cabecera_columna := cabecera_columna^.siguiente;
    end;
    cabecera_fila := cabecera_fila^.siguiente;
  end;
end;

procedure TMatrizDispersa.LimpiarMatriz;
var
  cabecera_actual, cabecera_siguiente: PNodoCabecera;
  nodo_actual, nodo_siguiente: PNodoMatriz;
begin
  // Limpiar nodos de datos
  cabecera_actual := cabeceras_filas;
  while cabecera_actual <> nil do
  begin
    nodo_actual := cabecera_actual^.acceso;
    while nodo_actual <> nil do
    begin
      nodo_siguiente := nodo_actual^.siguiente;
      Dispose(nodo_actual);
      nodo_actual := nodo_siguiente;
    end;
    cabecera_actual := cabecera_actual^.siguiente;
  end;

  // Limpiar cabeceras de filas
  cabecera_actual := cabeceras_filas;
  while cabecera_actual <> nil do
  begin
    cabecera_siguiente := cabecera_actual^.siguiente;
    Dispose(cabecera_actual);
    cabecera_actual := cabecera_siguiente;
  end;

  // Limpiar cabeceras de columnas
  cabecera_actual := cabeceras_columnas;
  while cabecera_actual <> nil do
  begin
    cabecera_siguiente := cabecera_actual^.siguiente;
    Dispose(cabecera_actual);
    cabecera_actual := cabecera_siguiente;
  end;

  cabeceras_filas := nil;
  cabeceras_columnas := nil;
  contador_usuarios := 0;
  total_correos := 0;
end;

function TMatrizDispersa.ClonarMatriz: TMatrizDispersa;
var
  nueva_matriz: TMatrizDispersa;
  cabecera_fila, cabecera_columna: PNodoCabecera;
  actual: PNodoMatriz;
  i: Integer;
begin
  nueva_matriz := TMatrizDispersa.Create;

  // Clonar todas las relaciones
  cabecera_fila := cabeceras_filas;
  while cabecera_fila <> nil do
  begin
    actual := cabecera_fila^.acceso;
    while actual <> nil do
    begin
      // Buscar email de la columna
      cabecera_columna := cabeceras_columnas;
      while (cabecera_columna <> nil) and (cabecera_columna^.indice <> actual^.columna) do
        cabecera_columna := cabecera_columna^.siguiente;

      if cabecera_columna <> nil then
      begin
        // Agregar la relación las veces que sea necesario
        for i := 1 to actual^.valor do
          nueva_matriz.AgregarRelacion(cabecera_fila^.email, cabecera_columna^.email);
      end;

      actual := actual^.siguiente;
    end;
    cabecera_fila := cabecera_fila^.siguiente;
  end;

  Result := nueva_matriz;
end;

function TMatrizDispersa.ObtenerListaUsuarios: TArray<string>;
var
  usuarios: TArray<string>;
  cabecera: PNodoCabecera;
  count, i: Integer;
  encontrado: Boolean;
begin
  SetLength(usuarios, 0);
  count := 0;

  // Agregar usuarios de filas
  cabecera := cabeceras_filas;
  while cabecera <> nil do
  begin
    SetLength(usuarios, count + 1);
    usuarios[count] := cabecera^.email;
    Inc(count);
    cabecera := cabecera^.siguiente;
  end;

  // Agregar usuarios de columnas que no estén ya incluidos
  cabecera := cabeceras_columnas;
  while cabecera <> nil do
  begin
    encontrado := False;
    for i := 0 to Length(usuarios) - 1 do
    begin
      if usuarios[i] = cabecera^.email then
      begin
        encontrado := True;
        Break;
      end;
    end;

    if not encontrado then
    begin
      SetLength(usuarios, count + 1);
      usuarios[count] := cabecera^.email;
      Inc(count);
    end;

    cabecera := cabecera^.siguiente;
  end;

  Result := usuarios;
end;

procedure TMatrizDispersa.ImportarRelaciones(relaciones: TArray<string>);
var
  i: Integer;
  partes: TArray<string>;
  remitente, destinatario: string;
  cantidad, j: Integer;
begin
  for i := 0 to Length(relaciones) - 1 do
  begin
    // Formato esperado: "remitente,destinatario,cantidad"
    partes := relaciones[i].Split(',');
    if Length(partes) = 3 then
    begin
      remitente := Trim(partes[0]);
      destinatario := Trim(partes[1]);
      cantidad := StrToIntDef(Trim(partes[2]), 0);

      // Agregar la relación las veces indicadas
      for j := 1 to cantidad do
        AgregarRelacion(remitente, destinatario);
    end;
  end;
end;

function TMatrizDispersa.ValidarEmail(email: string): Boolean;
begin
  Result := (Length(email) > 0) and (Pos('@', email) > 0) and (Pos('.', email) > 0);
end;

function TMatrizDispersa.ObtenerDensidadMatriz: Double;
var
  total_posiciones, posiciones_ocupadas: Integer;
  cabecera_fila: PNodoCabecera;
  actual: PNodoMatriz;
begin
  Result := 0.0;

  if (ContarCabecerasFila = 0) or (ContarCabecerasColumna = 0) then
    Exit;

  total_posiciones := ContarCabecerasFila * ContarCabecerasColumna;
  posiciones_ocupadas := 0;

  cabecera_fila := cabeceras_filas;
  while cabecera_fila <> nil do
  begin
    actual := cabecera_fila^.acceso;
    while actual <> nil do
    begin
      Inc(posiciones_ocupadas);
      actual := actual^.siguiente;
    end;
    cabecera_fila := cabecera_fila^.siguiente;
  end;

  if total_posiciones > 0 then
    Result := posiciones_ocupadas / total_posiciones;
end;

function TMatrizDispersa.EstaVacia: Boolean;
begin
  Result := (cabeceras_filas = nil) and (cabeceras_columnas = nil);
end;

end.

