unit CorreoManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EstructurasDatos;

type
  TCorreoManager = class
  private
    function CrearCorreo(Remitente, Destinatario, Asunto, Mensaje, Fecha: String; Programado: Boolean = False): PCorreo;
    function AgregarABandejaEntrada(Usuario: PUsuario; Correo: PCorreo): Boolean;
    function AgregarAPapelera(Usuario: PUsuario; Correo: PCorreo): Boolean;
    function AgregarACorreosProgramados(Usuario: PUsuario; Correo: PCorreo): Boolean;
    function RemoverDeBandejaEntrada(Usuario: PUsuario; CorreoId: Integer): PCorreo;
    function BuscarCorreoEnBandeja(Usuario: PUsuario; CorreoId: Integer): PCorreo;

  public
    constructor Create;
    destructor Destroy; override;

    // Funciones principales
    function EnviarCorreo(Sistema: TEDDMailSystem; RemitenteEmail, Destinatario, Asunto, Mensaje: String): Boolean;
    function ProgramarCorreo(Sistema: TEDDMailSystem; RemitenteEmail, Destinatario, Asunto, Mensaje, FechaEnvio: String): Boolean;

    // Gestión de bandeja de entrada (Lista doblemente enlazada)
    function GetBandejaEntrada(Usuario: PUsuario): PCorreo;
    function MarcarCorreoLeido(Usuario: PUsuario; CorreoId: Integer): Boolean;
    function EliminarCorreoDeBandeja(Usuario: PUsuario; CorreoId: Integer): Boolean;
    function ContarCorreosNoLeidos(Usuario: PUsuario): Integer;
    function OrdenarBandejaPorAsunto(Usuario: PUsuario): Boolean;

    // Gestión de papelera (Pila - LIFO)
    function GetPapelera(Usuario: PUsuario): PCorreo;
    //function RestaurarCorreo(Usuario: PUsuario; CorreoId: Integer): Boolean;
    //function EliminarCorreoDePapelera(Usuario: PUsuario; CorreoId: Integer): Boolean;
    function BuscarEnPapelera(Usuario: PUsuario; PalabraClave: String): PCorreo;

    // Gestión de correos programados (Cola - FIFO)
    function GetCorreosProgramados(Usuario: PUsuario): PCorreo;
    function ProcesarCorreosProgramados(Sistema: TEDDMailSystem; Usuario: PUsuario): Integer;
    //function EliminarCorreoProgramado(Usuario: PUsuario; CorreoId: Integer): Boolean;

    // Inicialización de usuario
    procedure InicializarEstructurasCorreo(Usuario: PUsuario);
    procedure LiberarEstructurasCorreo(Usuario: PUsuario);

    // Reportes
    procedure GenerarReporteCorreosRecibidos(Usuario: PUsuario; RutaCarpeta: String);
    procedure GenerarReportePapelera(Usuario: PUsuario; RutaCarpeta: String);
    procedure GenerarReporteCorreosProgramados(Usuario: PUsuario; RutaCarpeta: String);
    function EliminarCorreoDePapelera(Usuario: PUsuario; Id: Integer): Boolean;
f   function VaciarPapelera(Usuario: PUsuario): Integer; // opcional
  end;

implementation

uses
  process, DateUtils;

constructor TCorreoManager.Create;
begin
  inherited Create;
  WriteLn('CorreoManager inicializado');
end;

destructor TCorreoManager.Destroy;
begin
  WriteLn('CorreoManager destruido');
  inherited Destroy;
end;

function TCorreoManager.CrearCorreo(Remitente, Destinatario, Asunto, Mensaje, Fecha: String; Programado: Boolean): PCorreo;
begin
  New(Result);
  Result^.Id := Random(9999) + 1000; // ID único
  Result^.Remitente := Remitente;
  Result^.Destinatario := Destinatario;
  Result^.Estado := 'NL'; // No Leído por defecto
  Result^.Programado := Programado;
  Result^.Asunto := Asunto;
  Result^.Fecha := Fecha;
  Result^.Mensaje := Mensaje;

  if Programado then
    Result^.FechaEnvio := Fecha
  else
    Result^.FechaEnvio := FormatDateTime('dd/mm/yy hh:nn', Now);

  Result^.Anterior := nil;
  Result^.Siguiente := nil;
end;

procedure TCorreoManager.InicializarEstructurasCorreo(Usuario: PUsuario);
begin
  if Usuario = nil then
    Exit;

  Usuario^.BandejaEntrada := nil;
  Usuario^.Papelera := nil;
  Usuario^.CorreosProgramados := nil;
  WriteLn('Estructuras de correo inicializadas para: ', Usuario^.Email);
end;

procedure TCorreoManager.LiberarEstructurasCorreo(Usuario: PUsuario);
var
  CorreoActual, CorreoTemp: PCorreo;
begin
  if Usuario = nil then
    Exit;

  // Liberar bandeja de entrada
  CorreoActual := Usuario^.BandejaEntrada;
  while CorreoActual <> nil do
  begin
    CorreoTemp := CorreoActual;
    CorreoActual := CorreoActual^.Siguiente;
    Dispose(CorreoTemp);
  end;

  // Liberar papelera
  CorreoActual := Usuario^.Papelera;
  while CorreoActual <> nil do
  begin
    CorreoTemp := CorreoActual;
    CorreoActual := CorreoActual^.Siguiente;
    Dispose(CorreoTemp);
  end;

  // Liberar correos programados
  CorreoActual := Usuario^.CorreosProgramados;
  while CorreoActual <> nil do
  begin
    CorreoTemp := CorreoActual;
    CorreoActual := CorreoActual^.Siguiente;
    Dispose(CorreoTemp);
  end;

  WriteLn('Estructuras de correo liberadas para: ', Usuario^.Email);
end;

function TCorreoManager.AgregarABandejaEntrada(Usuario: PUsuario; Correo: PCorreo): Boolean;
begin
  Result := False;
  if (Usuario = nil) or (Correo = nil) then
    Exit;

  // Agregar al inicio de la lista doblemente enlazada
  Correo^.Siguiente := Usuario^.BandejaEntrada;
  Correo^.Anterior := nil;

  if Usuario^.BandejaEntrada <> nil then
    Usuario^.BandejaEntrada^.Anterior := Correo;

  Usuario^.BandejaEntrada := Correo;

  WriteLn('Correo agregado a bandeja de entrada de: ', Usuario^.Email);
  Result := True;
end;

function TCorreoManager.EnviarCorreo(Sistema: TEDDMailSystem; RemitenteEmail, Destinatario, Asunto, Mensaje: String): Boolean;
var
  NuevoCorreo: PCorreo;
  UsuarioRemitente, UsuarioDestino: PUsuario;
begin
  Result := False;

  // Buscar usuario remitente
  UsuarioRemitente := Sistema.BuscarUsuario(RemitenteEmail);
  if UsuarioRemitente = nil then
  begin
    WriteLn('Error: Usuario remitente no encontrado');
    Exit;
  end;

  // Buscar usuario destinatario
  UsuarioDestino := Sistema.BuscarUsuario(Destinatario);
  if UsuarioDestino = nil then
  begin
    WriteLn('Error: El usuario destinatario no existe en el sistema');
    Exit;
  end;

  // VALIDACIÓN CRÍTICA: Verificar que sea contacto (según enunciado)
  if Sistema.BuscarContacto(UsuarioRemitente, Destinatario) = nil then
  begin
    WriteLn('Error: Solo puede enviar correos a usuarios en su lista de contactos');
    WriteLn('Agregue primero a ', Destinatario, ' como contacto');
    Exit;
  end;

  // Crear el correo
  NuevoCorreo := CrearCorreo(
    RemitenteEmail,
    Destinatario,
    Asunto,
    Mensaje,
    FormatDateTime('dd/mm/yy hh:nn', Now),
    False // No es programado
  );

  // Agregar a bandeja del destinatario
  if AgregarABandejaEntrada(UsuarioDestino, NuevoCorreo) then
  begin
    // Actualizar matriz de relaciones (si está implementada)
    try
      Sistema.ActualizarMatrizRelaciones(RemitenteEmail, Destinatario);
    except
      // Si no está implementada, continuar sin error
    end;

    WriteLn('✓ Correo enviado exitosamente');
    WriteLn('  De: ', RemitenteEmail);
    WriteLn('  Para: ', Destinatario);
    WriteLn('  Asunto: ', Asunto);
    Result := True;
  end
  else
  begin
    Dispose(NuevoCorreo);
    WriteLn('Error: No se pudo agregar el correo a la bandeja del destinatario');
  end;
end;

function TCorreoManager.GetBandejaEntrada(Usuario: PUsuario): PCorreo;
begin
  Result := nil;
  if Usuario = nil then
    Exit;

  Result := Usuario^.BandejaEntrada;
end;

function TCorreoManager.ContarCorreosNoLeidos(Usuario: PUsuario): Integer;
var
  CorreoActual: PCorreo;
begin
  Result := 0;
  if Usuario = nil then
    Exit;

  CorreoActual := Usuario^.BandejaEntrada;
  while CorreoActual <> nil do
  begin
    if CorreoActual^.Estado = 'NL' then
      Inc(Result);
    CorreoActual := CorreoActual^.Siguiente;
  end;
end;

function TCorreoManager.BuscarCorreoEnBandeja(Usuario: PUsuario; CorreoId: Integer): PCorreo;
var
  CorreoActual: PCorreo;
begin
  Result := nil;
  if Usuario = nil then
    Exit;

  CorreoActual := Usuario^.BandejaEntrada;
  while CorreoActual <> nil do
  begin
    if CorreoActual^.Id = CorreoId then
    begin
      Result := CorreoActual;
      Exit;
    end;
    CorreoActual := CorreoActual^.Siguiente;
  end;
end;

function TCorreoManager.MarcarCorreoLeido(Usuario: PUsuario; CorreoId: Integer): Boolean;
var
  Correo: PCorreo;
begin
  Result := False;
  if Usuario = nil then
    Exit;

  Correo := BuscarCorreoEnBandeja(Usuario, CorreoId);
  if Correo <> nil then
  begin
    Correo^.Estado := 'L'; // Leído
    WriteLn('Correo marcado como leído: ', CorreoId);
    Result := True;
  end;
end;

function TCorreoManager.RemoverDeBandejaEntrada(Usuario: PUsuario; CorreoId: Integer): PCorreo;
var
  CorreoActual: PCorreo;
begin
  Result := nil;
  if Usuario = nil then
    Exit;

  CorreoActual := Usuario^.BandejaEntrada;
  while CorreoActual <> nil do
  begin
    if CorreoActual^.Id = CorreoId then
    begin
      // Remover de lista doblemente enlazada
      if CorreoActual^.Anterior <> nil then
        CorreoActual^.Anterior^.Siguiente := CorreoActual^.Siguiente
      else
        Usuario^.BandejaEntrada := CorreoActual^.Siguiente;

      if CorreoActual^.Siguiente <> nil then
        CorreoActual^.Siguiente^.Anterior := CorreoActual^.Anterior;

      // Limpiar punteros del correo removido
      CorreoActual^.Anterior := nil;
      CorreoActual^.Siguiente := nil;

      Result := CorreoActual;
      Exit;
    end;
    CorreoActual := CorreoActual^.Siguiente;
  end;
end;

function TCorreoManager.AgregarAPapelera(Usuario: PUsuario; Correo: PCorreo): Boolean;
begin
  Result := False;
  if (Usuario = nil) or (Correo = nil) then
    Exit;

  // Agregar al tope de la pila (LIFO)
  Correo^.Siguiente := Usuario^.Papelera;
  Correo^.Anterior := nil;

  if Usuario^.Papelera <> nil then
    Usuario^.Papelera^.Anterior := Correo;

  Usuario^.Papelera := Correo;

  WriteLn('Correo agregado a papelera (PILA): ', Correo^.Id);
  Result := True;
end;

function TCorreoManager.EliminarCorreoDeBandeja(Usuario: PUsuario; CorreoId: Integer): Boolean;
var
  CorreoEliminado: PCorreo;
begin
  Result := False;
  if Usuario = nil then
    Exit;

  // Remover de bandeja de entrada
  CorreoEliminado := RemoverDeBandejaEntrada(Usuario, CorreoId);
  if CorreoEliminado <> nil then
  begin
    // Cambiar estado a eliminado
    CorreoEliminado^.Estado := 'Eliminado';

    // Agregar a papelera (pila)
    if AgregarAPapelera(Usuario, CorreoEliminado) then
    begin
      WriteLn('Correo movido a papelera: ', CorreoId);
      Result := True;
    end;
  end;
end;

function TCorreoManager.GetPapelera(Usuario: PUsuario): PCorreo;
begin
  Result := nil;
  if Usuario = nil then
    Exit;

  Result := Usuario^.Papelera;
end;

function TCorreoManager.BuscarEnPapelera(Usuario: PUsuario; PalabraClave: String): PCorreo;
var
  CorreoActual: PCorreo;
begin
  Result := nil;
  if (Usuario = nil) or (PalabraClave = '') then
    Exit;

  CorreoActual := Usuario^.Papelera;
  while CorreoActual <> nil do
  begin
    // Búsqueda case-insensitive en asunto
    if Pos(UpperCase(PalabraClave), UpperCase(CorreoActual^.Asunto)) > 0 then
    begin
      Result := CorreoActual;
      WriteLn('Correo encontrado en papelera: ', CorreoActual^.Asunto);
      Exit;
    end;
    CorreoActual := CorreoActual^.Siguiente;
  end;

  WriteLn('No se encontraron correos con la palabra: ', PalabraClave);
end;

function TCorreoManager.ProgramarCorreo(Sistema: TEDDMailSystem; RemitenteEmail, Destinatario, Asunto, Mensaje, FechaEnvio: String): Boolean;
var
  CorreoProgramado: PCorreo;
  UsuarioRemitente, UsuarioDestino: PUsuario;
begin
  Result := False;

  // Buscar usuarios
  UsuarioRemitente := Sistema.BuscarUsuario(RemitenteEmail);
  UsuarioDestino := Sistema.BuscarUsuario(Destinatario);

  if (UsuarioRemitente = nil) or (UsuarioDestino = nil) then
  begin
    WriteLn('Error: Usuario remitente o destinatario no encontrado');
    Exit;
  end;

  // Verificar contacto
  if Sistema.BuscarContacto(UsuarioRemitente, Destinatario) = nil then
  begin
    WriteLn('Error: Solo puede programar correos a usuarios en su lista de contactos');
    Exit;
  end;

  // Crear correo programado
  CorreoProgramado := CrearCorreo(
    RemitenteEmail,
    Destinatario,
    Asunto,
    Mensaje,
    FechaEnvio,
    True // Es programado
  );

  // Agregar a cola de correos programados (FIFO)
  if AgregarACorreosProgramados(UsuarioRemitente, CorreoProgramado) then
  begin
    WriteLn('✓ Correo programado exitosamente para: ', FechaEnvio);
    Result := True;
  end
  else
  begin
    Dispose(CorreoProgramado);
    WriteLn('Error: No se pudo programar el correo');
  end;
end;

function TCorreoManager.AgregarACorreosProgramados(Usuario: PUsuario; Correo: PCorreo): Boolean;
var
  UltimoCorreo: PCorreo;
begin
  Result := False;
  if (Usuario = nil) or (Correo = nil) then
    Exit;

  // Agregar al final de la cola (FIFO)
  Correo^.Siguiente := nil;
  Correo^.Anterior := nil;

  if Usuario^.CorreosProgramados = nil then
  begin
    Usuario^.CorreosProgramados := Correo;
  end
  else
  begin
    // Buscar el último correo
    UltimoCorreo := Usuario^.CorreosProgramados;
    while UltimoCorreo^.Siguiente <> nil do
      UltimoCorreo := UltimoCorreo^.Siguiente;

    UltimoCorreo^.Siguiente := Correo;
    Correo^.Anterior := UltimoCorreo;
  end;

  WriteLn('Correo agregado a cola de programados (FIFO)');
  Result := True;
end;

function TCorreoManager.GetCorreosProgramados(Usuario: PUsuario): PCorreo;
begin
  Result := nil;
  if Usuario = nil then
    Exit;

  Result := Usuario^.CorreosProgramados;
end;

function TCorreoManager.ProcesarCorreosProgramados(Sistema: TEDDMailSystem; Usuario: PUsuario): Integer;
var
  CorreoActual, CorreoSiguiente: PCorreo;
  FechaActual: TDateTime;
  FechaEnvio: TDateTime;
  CorreosEnviados: Integer;
begin
  Result := 0;
  if Usuario = nil then
    Exit;

  CorreosEnviados := 0;
  FechaActual := Now;
  CorreoActual := Usuario^.CorreosProgramados;

  while CorreoActual <> nil do
  begin
    CorreoSiguiente := CorreoActual^.Siguiente;

    try
      // Parsear fecha de envío (formato: dd/mm/yy hh:nn)
      FechaEnvio := StrToDateTime(CorreoActual^.FechaEnvio);

      if FechaEnvio <= FechaActual then
      begin
        WriteLn('Procesando correo programado ID: ', CorreoActual^.Id);

        // Enviar el correo
        if EnviarCorreo(Sistema, CorreoActual^.Remitente, CorreoActual^.Destinatario,
                       CorreoActual^.Asunto, CorreoActual^.Mensaje) then
        begin
          // Remover de cola de programados
          if CorreoActual^.Anterior <> nil then
            CorreoActual^.Anterior^.Siguiente := CorreoActual^.Siguiente
          else
            Usuario^.CorreosProgramados := CorreoActual^.Siguiente;

          if CorreoActual^.Siguiente <> nil then
            CorreoActual^.Siguiente^.Anterior := CorreoActual^.Anterior;

          Dispose(CorreoActual);
          Inc(CorreosEnviados);
        end;
      end;
    except
      on E: Exception do
        WriteLn('Error al procesar correo programado: ', E.Message);
    end;

    CorreoActual := CorreoSiguiente;
  end;

  WriteLn('Correos programados enviados: ', CorreosEnviados);
  Result := CorreosEnviados;
end;

function TCorreoManager.OrdenarBandejaPorAsunto(Usuario: PUsuario): Boolean;
var
  CorreoActual, CorreoComparar, CorreoTemporal: PCorreo;
  Intercambiado: Boolean;
begin
  Result := False;
  if Usuario = nil then
    Exit;

  // Ordenamiento burbuja simple para lista doblemente enlazada
  repeat
    Intercambiado := False;
    CorreoActual := Usuario^.BandejaEntrada;

    while (CorreoActual <> nil) and (CorreoActual^.Siguiente <> nil) do
    begin
      CorreoComparar := CorreoActual^.Siguiente;

      // Comparar asuntos (A-Z ascendente)
      if UpCase(CorreoActual^.Asunto) > UpCase(CorreoComparar^.Asunto) then
      begin
        // Intercambiar datos (más simple que intercambiar nodos)
        CorreoTemporal := CrearCorreo('', '', '', '', '');

        // Copiar datos del actual al temporal
        CorreoTemporal^.Id := CorreoActual^.Id;
        CorreoTemporal^.Remitente := CorreoActual^.Remitente;
        CorreoTemporal^.Destinatario := CorreoActual^.Destinatario;
        CorreoTemporal^.Estado := CorreoActual^.Estado;
        CorreoTemporal^.Asunto := CorreoActual^.Asunto;
        CorreoTemporal^.Mensaje := CorreoActual^.Mensaje;
        CorreoTemporal^.Fecha := CorreoActual^.Fecha;

        // Copiar datos del comparar al actual
        CorreoActual^.Id := CorreoComparar^.Id;
        CorreoActual^.Remitente := CorreoComparar^.Remitente;
        CorreoActual^.Destinatario := CorreoComparar^.Destinatario;
        CorreoActual^.Estado := CorreoComparar^.Estado;
        CorreoActual^.Asunto := CorreoComparar^.Asunto;
        CorreoActual^.Mensaje := CorreoComparar^.Mensaje;
        CorreoActual^.Fecha := CorreoComparar^.Fecha;

        // Copiar datos del temporal al comparar
        CorreoComparar^.Id := CorreoTemporal^.Id;
        CorreoComparar^.Remitente := CorreoTemporal^.Remitente;
        CorreoComparar^.Destinatario := CorreoTemporal^.Destinatario;
        CorreoComparar^.Estado := CorreoTemporal^.Estado;
        CorreoComparar^.Asunto := CorreoTemporal^.Asunto;
        CorreoComparar^.Mensaje := CorreoTemporal^.Mensaje;
        CorreoComparar^.Fecha := CorreoTemporal^.Fecha;

        Dispose(CorreoTemporal);
        Intercambiado := True;
      end;

      CorreoActual := CorreoActual^.Siguiente;
    end;
  until not Intercambiado;

  WriteLn('Bandeja de entrada ordenada por asunto (A-Z)');
  Result := True;
end;

procedure TCorreoManager.GenerarReporteCorreosRecibidos(Usuario: PUsuario; RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
  CorreoActual: PCorreo;
  EmailLimpio: String;
begin
  if Usuario = nil then
    Exit;

  try
    ForceDirectories(RutaCarpeta);

    NombreArchivo := RutaCarpeta + '/correos_recibidos_' +
                   StringReplace(Usuario^.Usuario, ' ', '_', [rfReplaceAll]) + '.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Lista Doblemente Enlazada - Correos Recibidos - ' + Usuario^.Nombre + '";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    rankdir=LR;');
    WriteLn(Archivo, '    node [shape=record, style=filled];');

    CorreoActual := Usuario^.BandejaEntrada;

    if CorreoActual = nil then
    begin
      WriteLn(Archivo, '    empty [label="Bandeja vacía", fillcolor=lightgray];');
    end
    else
    begin
      while CorreoActual <> nil do
      begin
        EmailLimpio := StringReplace(CorreoActual^.Remitente, '@', '_at_', [rfReplaceAll]);
        EmailLimpio := StringReplace(EmailLimpio, '.', '_', [rfReplaceAll]);
        EmailLimpio := StringReplace(EmailLimpio, '-', '_', [rfReplaceAll]);

        WriteLn(Archivo, Format('    correo_%d [label="ID: %d|Estado: %s|De: %s|Asunto: %s|Fecha: %s", fillcolor=lightblue];',
          [CorreoActual^.Id, CorreoActual^.Id, CorreoActual^.Estado,
           CorreoActual^.Remitente, CorreoActual^.Asunto, CorreoActual^.Fecha]));

        if CorreoActual^.Siguiente <> nil then
        begin
          WriteLn(Archivo, Format('    correo_%d -> correo_%d;', [CorreoActual^.Id, CorreoActual^.Siguiente^.Id]));
          WriteLn(Archivo, Format('    correo_%d -> correo_%d;', [CorreoActual^.Siguiente^.Id, CorreoActual^.Id]));
        end;

        CorreoActual := CorreoActual^.Siguiente;
      end;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen
    try
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'dot';
        Process.Parameters.Add('-Tpng');
        Process.Parameters.Add(NombreArchivo);
        Process.Parameters.Add('-o');
        Process.Parameters.Add(ChangeFileExt(NombreArchivo, '.png'));
        Process.Options := Process.Options + [poWaitOnExit];
        Process.Execute;
        WriteLn('Reporte de correos recibidos generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte: ', E.Message);
  end;
end;

procedure TCorreoManager.GenerarReportePapelera(Usuario: PUsuario; RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
  CorreoActual: PCorreo;
begin
  if Usuario = nil then
    Exit;

  try
    ForceDirectories(RutaCarpeta);

    NombreArchivo := RutaCarpeta + '/papelera_' +
                   StringReplace(Usuario^.Usuario, ' ', '_', [rfReplaceAll]) + '.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Pila (LIFO) - Papelera - ' + Usuario^.Nombre + '";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    rankdir=TB;');
    WriteLn(Archivo, '    node [shape=record, style=filled];');

    CorreoActual := Usuario^.Papelera;

    if CorreoActual = nil then
    begin
      WriteLn(Archivo, '    empty [label="Papelera vacía", fillcolor=lightgray];');
    end
    else
    begin
      WriteLn(Archivo, '    // PILA - El último agregado está arriba');
      while CorreoActual <> nil do
      begin
        WriteLn(Archivo, Format('    correo_%d [label="ID: %d|Estado: %s|De: %s|Asunto: %s|Fecha: %s", fillcolor=lightpink];',
          [CorreoActual^.Id, CorreoActual^.Id, CorreoActual^.Estado,
           CorreoActual^.Remitente, CorreoActual^.Asunto, CorreoActual^.Fecha]));

        if CorreoActual^.Siguiente <> nil then
          WriteLn(Archivo, Format('    correo_%d -> correo_%d [label="abajo"];', [CorreoActual^.Id, CorreoActual^.Siguiente^.Id]));

        CorreoActual := CorreoActual^.Siguiente;
      end;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen
    try
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'dot';
        Process.Parameters.Add('-Tpng');
        Process.Parameters.Add(NombreArchivo);
        Process.Parameters.Add('-o');
        Process.Parameters.Add(ChangeFileExt(NombreArchivo, '.png'));
        Process.Options := Process.Options + [poWaitOnExit];
        Process.Execute;
        WriteLn('Reporte de papelera generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte: ', E.Message);
  end;
end;

procedure TCorreoManager.GenerarReporteCorreosProgramados(Usuario: PUsuario; RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
  CorreoActual: PCorreo;
begin
  if Usuario = nil then
    Exit;

  try
    ForceDirectories(RutaCarpeta);

    NombreArchivo := RutaCarpeta + '/correos_programados_' +
                   StringReplace(Usuario^.Usuario, ' ', '_', [rfReplaceAll]) + '.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Cola (FIFO) - Correos Programados - ' + Usuario^.Nombre + '";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    rankdir=LR;');
    WriteLn(Archivo, '    node [shape=record, style=filled];');

    CorreoActual := Usuario^.CorreosProgramados;

    if CorreoActual = nil then
    begin
      WriteLn(Archivo, '    empty [label="No hay correos programados", fillcolor=lightgray];');
    end
    else
    begin
      WriteLn(Archivo, '    // COLA FIFO - Primero en entrar, primero en salir');
      while CorreoActual <> nil do
      begin
        WriteLn(Archivo, Format('    correo_%d [label="ID: %d|Para: %s|Asunto: %s|Envío: %s", fillcolor=lightyellow];',
          [CorreoActual^.Id, CorreoActual^.Id, CorreoActual^.Destinatario,
           CorreoActual^.Asunto, CorreoActual^.FechaEnvio]));

        if CorreoActual^.Siguiente <> nil then
          WriteLn(Archivo, Format('    correo_%d -> correo_%d [label="siguiente"];', [CorreoActual^.Id, CorreoActual^.Siguiente^.Id]));

        CorreoActual := CorreoActual^.Siguiente;
      end;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen
    try
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'dot';
        Process.Parameters.Add('-Tpng');
        Process.Parameters.Add(NombreArchivo);
        Process.Parameters.Add('-o');
        Process.Parameters.Add(ChangeFileExt(NombreArchivo, '.png'));
        Process.Options := Process.Options + [poWaitOnExit];
        Process.Execute;
        WriteLn('Reporte de correos programados generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte: ', E.Message);
  end;
end;
function TCorreoManager.EliminarCorreoDePapelera(Usuario: PUsuario; Id: Integer): Boolean;
var
  Cur, Prev: PCorreo;
begin
  Result := False;
  if Usuario = nil then Exit;

  // Ajusta el nombre del campo si en tu record se llama distinto a "Papelera"
  Cur := Usuario^.Papelera;
  Prev := nil;

  while (Cur <> nil) and (Cur^.Id <> Id) do
  begin
    Prev := Cur;
    Cur := Cur^.Siguiente;
  end;

  if Cur = nil then Exit; // no encontrado

  // Desenlazar
  if Prev = nil then
    Usuario^.Papelera := Cur^.Siguiente     // era la cabeza
  else
    Prev^.Siguiente := Cur^.Siguiente;      // era intermedio

  // Liberar (importante para strings/managed types)
  Finalize(Cur^);
  Dispose(Cur);

  Result := True;
end;

function TCorreoManager.VaciarPapelera(Usuario: PUsuario): Integer;
var
  Cur: PCorreo;
begin
  Result := 0;
  if Usuario = nil then Exit;

  while Usuario^.Papelera <> nil do
  begin
    Cur := Usuario^.Papelera;
    Usuario^.Papelera := Cur^.Siguiente;
    Finalize(Cur^);
    Dispose(Cur);
    Inc(Result);
  end;
end;
end.
