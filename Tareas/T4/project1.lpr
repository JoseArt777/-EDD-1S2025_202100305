program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Process;

type
  TArista = class
  private
    FDestino: String;
    FPeso: Integer;
  public
    constructor Create(ADestino: String; APeso: Integer = 1);
    property Destino: String read FDestino write FDestino;
    property Peso: Integer read FPeso write FPeso;
  end;

  TListaAristas = class
  private
    FItems: array of TArista;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: TArista);
    function GetItem(Index: Integer): TArista;
    property Count: Integer read FCount;
    property Items[Index: Integer]: TArista read GetItem; default;
  end;

  TGrafoNoDirigido = class
  private
    FAdyacencia: TStringList;
    function GetListaAristas(Ciudad: String): TListaAristas;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AgregarCiudad(Ciudad: String);
    procedure AgregarConexion(Ciudad1, Ciudad2: String; Peso: Integer = 1);
    function ExisteCiudad(Ciudad: String): Boolean;
    function ObtenerCiudades: TStringList;
    function ObtenerListaAdyacencia: String;
    function ObtenerCantidadCiudades: Integer;
    function GenerarGraphviz: String;
    procedure GuardarGraphviz(Archivo: String);
    function GenerarImagen(ArchivoDot, ArchivoImagen: String): Boolean;
  end;

  Tarea4 = class(TCustomApplication)
  private
    FGrafo: TGrafoNoDirigido;
    procedure MostrarMenu;
    procedure CargarEjemploPDF;
    procedure EjecutarMenu;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

constructor TArista.Create(ADestino: String; APeso: Integer);
begin
  FDestino := ADestino;
  FPeso := APeso;
end;

constructor TListaAristas.Create;
begin
  SetLength(FItems, 0);
  FCount := 0;
end;

destructor TListaAristas.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FItems[i].Free;
  inherited Destroy;
end;

procedure TListaAristas.Add(Item: TArista);
begin
  SetLength(FItems, FCount + 1);
  FItems[FCount] := Item;
  Inc(FCount);
end;

function TListaAristas.GetItem(Index: Integer): TArista;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FItems[Index]
  else
    Result := nil;
end;

constructor TGrafoNoDirigido.Create;
begin
  FAdyacencia := TStringList.Create;
  FAdyacencia.OwnsObjects := True;
  FAdyacencia.Sorted := True;
  FAdyacencia.Duplicates := dupIgnore;
end;

destructor TGrafoNoDirigido.Destroy;
begin
  FAdyacencia.Free;
  inherited Destroy;
end;

function TGrafoNoDirigido.GetListaAristas(Ciudad: String): TListaAristas;
var
  Index: Integer;
begin
  Index := FAdyacencia.IndexOf(Ciudad);
  if Index >= 0 then
    Result := TListaAristas(FAdyacencia.Objects[Index])
  else
    Result := nil;
end;

procedure TGrafoNoDirigido.AgregarCiudad(Ciudad: String);
var
  Lista: TListaAristas;
begin
  if not ExisteCiudad(Ciudad) then
  begin
    Lista := TListaAristas.Create;
    FAdyacencia.AddObject(Ciudad, Lista);
  end;
end;

procedure TGrafoNoDirigido.AgregarConexion(Ciudad1, Ciudad2: String; Peso: Integer);
var
  Lista1, Lista2: TListaAristas;
begin
  if not ExisteCiudad(Ciudad1) then
    AgregarCiudad(Ciudad1);
  if not ExisteCiudad(Ciudad2) then
    AgregarCiudad(Ciudad2);

  Lista1 := GetListaAristas(Ciudad1);
  Lista2 := GetListaAristas(Ciudad2);

  if Assigned(Lista1) then
    Lista1.Add(TArista.Create(Ciudad2, Peso));
  if Assigned(Lista2) then
    Lista2.Add(TArista.Create(Ciudad1, Peso));
end;

function TGrafoNoDirigido.ExisteCiudad(Ciudad: String): Boolean;
begin
  Result := FAdyacencia.IndexOf(Ciudad) >= 0;
end;

function TGrafoNoDirigido.ObtenerCiudades: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to FAdyacencia.Count - 1 do
    Result.Add(FAdyacencia[i]);
end;

function TGrafoNoDirigido.ObtenerCantidadCiudades: Integer;
begin
  Result := FAdyacencia.Count;
end;

function TGrafoNoDirigido.ObtenerListaAdyacencia: String;
var
  i, j: Integer;
  Lista: TListaAristas;
  Resultado: String;
begin
  Resultado := 'Lista de Adyacencia:' + LineEnding + LineEnding;

  for i := 0 to FAdyacencia.Count - 1 do
  begin
    Resultado := Resultado + FAdyacencia[i] + ' -> ';
    Lista := TListaAristas(FAdyacencia.Objects[i]);

    if Lista.Count > 0 then
    begin
      for j := 0 to Lista.Count - 1 do
      begin
        Resultado := Resultado + Lista[j].Destino;
        if Lista[j].Peso > 1 then
          Resultado := Resultado + ' (peso: ' + IntToStr(Lista[j].Peso) + ')';
        if j < Lista.Count - 1 then
          Resultado := Resultado + ', ';
      end;
    end
    else
      Resultado := Resultado + '(sin conexiones)';

    Resultado := Resultado + LineEnding;
  end;

  Result := Resultado;
end;

function TGrafoNoDirigido.GenerarGraphviz: String;
var
  i, j: Integer;
  Lista: TListaAristas;
  Resultado: String;
  Conexiones: TStringList;
  Conexion: String;
begin
  Resultado := 'graph GrafoNoDirigido {' + LineEnding;
  Resultado := Resultado + '  node [shape=ellipse, style=filled, fillcolor=lightblue];' + LineEnding;
  Resultado := Resultado + '  edge [color=black];' + LineEnding + LineEnding;

  Conexiones := TStringList.Create;
  try
    Conexiones.Sorted := True;
    Conexiones.Duplicates := dupIgnore;

    for i := 0 to FAdyacencia.Count - 1 do
    begin
      Lista := TListaAristas(FAdyacencia.Objects[i]);
      for j := 0 to Lista.Count - 1 do
      begin
        if FAdyacencia[i] < Lista[j].Destino then
          Conexion := FAdyacencia[i] + '--' + Lista[j].Destino
        else
          Conexion := Lista[j].Destino + '--' + FAdyacencia[i];

        if Conexiones.IndexOf(Conexion) < 0 then
        begin
          Conexiones.Add(Conexion);
          Resultado := Resultado + '  ' + FAdyacencia[i] + ' -- ' + Lista[j].Destino;
          if Lista[j].Peso > 1 then
            Resultado := Resultado + ' [label="' + IntToStr(Lista[j].Peso) + '"]';
          Resultado := Resultado + ';' + LineEnding;
        end;
      end;
    end;
  finally
    Conexiones.Free;
  end;

  Resultado := Resultado + '}';
  Result := Resultado;
end;

procedure TGrafoNoDirigido.GuardarGraphviz(Archivo: String);
var
  Contenido: String;
  ArchivoTexto: TextFile;
begin
  Contenido := GenerarGraphviz;
  AssignFile(ArchivoTexto, Archivo);
  try
    Rewrite(ArchivoTexto);
    Write(ArchivoTexto, Contenido);
  finally
    CloseFile(ArchivoTexto);
  end;
end;

function TGrafoNoDirigido.GenerarImagen(ArchivoDot, ArchivoImagen: String): Boolean;
var
  AProcess: TProcess;
  ExitCode: Integer;
begin
  Result := False;

  try
    AProcess := TProcess.Create(nil);
    try
      AProcess.Executable := 'dot';
      AProcess.Parameters.Add('-Tpng');
      AProcess.Parameters.Add(ArchivoDot);
      AProcess.Parameters.Add('-o');
      AProcess.Parameters.Add(ArchivoImagen);
      AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];

      AProcess.Execute;
      ExitCode := AProcess.ExitStatus;

      Result := (ExitCode = 0) and FileExists(ArchivoImagen);
    finally
      AProcess.Free;
    end;
  except
    Result := False;
  end;
end;

constructor Tarea4.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FGrafo := TGrafoNoDirigido.Create;
end;

destructor Tarea4.Destroy;
begin
  FGrafo.Free;
  inherited Destroy;
end;

procedure Tarea4.WriteHelp;
begin
  WriteLn('========================================');
  WriteLn('  TAREA #4 - GRAFO NO DIRIGIDO');
  WriteLn('  Universidad San Carlos de Guatemala');
  WriteLn('========================================');
  WriteLn;
  WriteLn('Uso: ', ExeName);
  WriteLn;
  WriteLn('Opciones:');
  WriteLn('  -h, --help    Muestra esta ayuda');
end;

procedure Tarea4.MostrarMenu;
begin
  WriteLn;
  WriteLn('========================================');
  WriteLn('   GRAFO NO DIRIGIDO - SISTEMA CIUDADES');
  WriteLn('========================================');
  WriteLn('1. Agregar ciudad');
  WriteLn('2. Agregar conexion entre ciudades');
  WriteLn('3. Mostrar lista de adyacencia');
  WriteLn('4. Mostrar codigo Graphviz');
  WriteLn('5. Guardar .dot y generar imagen .png');
  WriteLn('6. Cargar ejemplo del PDF (A,B,C,D)');
  WriteLn('7. Limpiar grafo');
  WriteLn('0. Salir');
  WriteLn('========================================');
  Write('Seleccione una opcion: ');
end;

procedure Tarea4.CargarEjemploPDF;
begin
  WriteLn;
  WriteLn('Cargando ejemplo...');

  FGrafo.AgregarCiudad('A');
  FGrafo.AgregarCiudad('B');
  FGrafo.AgregarCiudad('C');
  FGrafo.AgregarCiudad('D');

  FGrafo.AgregarConexion('A', 'B');
  FGrafo.AgregarConexion('A', 'C');
  FGrafo.AgregarConexion('B', 'D');

  WriteLn('Ejemplo cargado: Ciudades A, B, C, D con sus conexiones.');
end;

procedure Tarea4.EjecutarMenu;
var
  Opcion: Integer;
  Ciudad, Ciudad1, Ciudad2, Respuesta: String;
  Peso: Integer;
  PesoStr: String;
begin
  repeat
    MostrarMenu;
    try
      ReadLn(Opcion);
    except
      Opcion := -1;
    end;

    case Opcion of
      1: begin
           WriteLn;
           Write('Ingrese el nombre de la ciudad: ');
           ReadLn(Ciudad);
           Ciudad := Trim(Ciudad);

           if Ciudad = '' then
             WriteLn('Error: Debe ingresar el nombre de la ciudad.')
           else if FGrafo.ExisteCiudad(Ciudad) then
             WriteLn('Advertencia: La ciudad "' + Ciudad + '" ya existe.')
           else
           begin
             FGrafo.AgregarCiudad(Ciudad);
             WriteLn('Ciudad "' + Ciudad + '" agregada.');
           end;
         end;

      2: begin
           WriteLn;
           Write('Ingrese la primera ciudad: ');
           ReadLn(Ciudad1);
           Ciudad1 := Trim(Ciudad1);

           Write('Ingrese la segunda ciudad: ');
           ReadLn(Ciudad2);
           Ciudad2 := Trim(Ciudad2);

           Write('Ingrese el peso (Enter para 1): ');
           ReadLn(PesoStr);
           if Trim(PesoStr) = '' then
             Peso := 1
           else
           begin
             try
               Peso := StrToInt(PesoStr);
             except
               Peso := 1;
             end;
           end;

           if Peso <= 0 then Peso := 1;

           if (Ciudad1 = '') or (Ciudad2 = '') then
             WriteLn('Error: Debe ingresar ambas ciudades.')
           else if Ciudad1 = Ciudad2 then
             WriteLn('Error: No puede conectar una ciudad consigo misma.')
           else
           begin
             FGrafo.AgregarConexion(Ciudad1, Ciudad2, Peso);
             WriteLn('Conexion agregada: ' + Ciudad1 + ' <--> ' + Ciudad2 +
                     ' (peso: ' + IntToStr(Peso) + ')');
           end;
         end;

      3: begin
           WriteLn;
           if FGrafo.ObtenerCantidadCiudades = 0 then
             WriteLn('El grafo esta vacio.')
           else
           begin
             WriteLn('Total de ciudades: ' + IntToStr(FGrafo.ObtenerCantidadCiudades));
             WriteLn;
             WriteLn(FGrafo.ObtenerListaAdyacencia);
           end;
         end;

      4: begin
           WriteLn;
           if FGrafo.ObtenerCantidadCiudades = 0 then
             WriteLn('El grafo esta vacio.')
           else
           begin
             WriteLn('Codigo Graphviz:');
             WriteLn;
             WriteLn(FGrafo.GenerarGraphviz);
           end;
         end;

      5: begin
           WriteLn;
           if FGrafo.ObtenerCantidadCiudades = 0 then
             WriteLn('El grafo esta vacio.')
           else
           begin
             try
               FGrafo.GuardarGraphviz('grafo.dot');
               WriteLn('Archivo grafo.dot guardado.');

               if FGrafo.GenerarImagen('grafo.dot', 'grafo.png') then
                 WriteLn('Imagen grafo.png generada.')
               else
                 WriteLn('No se pudo generar la imagen (verificar Graphviz).');
             except
               on E: Exception do
                 WriteLn('Error: ' + E.Message);
             end;
           end;
         end;

      6: begin
           if FGrafo.ObtenerCantidadCiudades > 0 then
           begin
             Write('El grafo ya tiene datos. Desea limpiarlo? (S/N): ');
             ReadLn(Respuesta);
             if UpperCase(Trim(Respuesta)) = 'S' then
             begin
               FGrafo.Free;
               FGrafo := TGrafoNoDirigido.Create;
               CargarEjemploPDF;
             end;
           end
           else
             CargarEjemploPDF;
         end;

      7: begin
           if FGrafo.ObtenerCantidadCiudades > 0 then
           begin
             Write('Esta seguro de limpiar el grafo? (S/N): ');
             ReadLn(Respuesta);
             if UpperCase(Trim(Respuesta)) = 'S' then
             begin
               FGrafo.Free;
               FGrafo := TGrafoNoDirigido.Create;
               WriteLn('Grafo limpiado.');
             end;
           end
           else
             WriteLn('El grafo ya esta vacio.');
         end;

      0: WriteLn('Saliendo...');

      else
        WriteLn('Opcion invalida.');
    end;

    if Opcion <> 0 then
    begin
      WriteLn;
      Write('Presione Enter para continuar...');
      ReadLn;
    end;

  until Opcion = 0;
end;

procedure Tarea4.DoRun;
var
  ErrorMsg: String;
begin
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  WriteLn('===============================================');
  WriteLn('  TAREA #4 - GRAFO NO DIRIGIDO');
  WriteLn('  Universidad San Carlos de Guatemala - USAC');
  WriteLn('  Facultad de Ingenieria');
  WriteLn('  Escuela de Ingenieria en Ciencias y Sistemas');
  WriteLn('===============================================');

  EjecutarMenu;
  Terminate;
end;

var
  Application: Tarea4;

begin
  Application := Tarea4.Create(nil);
  Application.Title := 'Tarea4';
  Application.Run;
  Application.Free;
end.
