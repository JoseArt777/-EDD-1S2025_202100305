unit EstructurasDatos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Tipos de punteros
  PUsuario = ^TUsuario;
  PCorreo = ^TCorreo;
  PContacto = ^TContacto;
  PComunidad = ^TComunidad;
  PUsuarioComunidad = ^TUsuarioComunidad;
  PMatrizDispersaFila = ^TMatrizDispersaFila;
  PMatrizDispersaColumna = ^TMatrizDispersaColumna;
  PMatrizDispersaNodo = ^TMatrizDispersaNodo;

  // Estructura Usuario (Lista Simple)
  TUsuario = record
    Id: Integer;
    Nombre: String;
    Usuario: String;
    Email: String;
    Telefono: String;
    Password: String;
    Siguiente: PUsuario;
  end;

  // Estructura Correo (Lista Doblemente Enlazada para bandeja de entrada)
  TCorreo = record
    Id: Integer;
    Remitente: String;
    Destinatario: String;
    Estado: String; // 'NL' = No Leído, 'L' = Leído
    Programado: Boolean;
    Asunto: String;
    Fecha: String;
    Mensaje: String;
    FechaEnvio: String; // Para correos programados
    Anterior: PCorreo;
    Siguiente: PCorreo;
  end;

  // Estructura Contacto (Lista Circular)
  TContacto = record
    Id: Integer;
    Nombre: String;
    Usuario: String;
    Email: String;
    Telefono: String;
    Siguiente: PContacto;
  end;

  // Estructura para Comunidades
  TUsuarioComunidad = record
    Email: String;
    Siguiente: PUsuarioComunidad;
  end;

  TComunidad = record
    Id: Integer;
    Nombre: String;
    UsuariosList: PUsuarioComunidad;
    Siguiente: PComunidad;
  end;

  // Matriz Dispersa para relaciones remitente-destinatario
  TMatrizDispersaNodo = record
    Fila: Integer;
    Columna: Integer;
    Cantidad: Integer;
    RemitenteEmail: String;
    DestinatarioEmail: String;
    Arriba: PMatrizDispersaNodo;
    Abajo: PMatrizDispersaNodo;
    Izquierda: PMatrizDispersaNodo;
    Derecha: PMatrizDispersaNodo;
  end;

  TMatrizDispersaFila = record
    Fila: Integer;
    Email: String;
    Primero: PMatrizDispersaNodo;
    Siguiente: PMatrizDispersaFila;
  end;

  TMatrizDispersaColumna = record
    Columna: Integer;
    Email: String;
    Primero: PMatrizDispersaNodo;
    Siguiente: PMatrizDispersaColumna;
  end;

  // Clase principal para manejar todas las estructuras
  TEDDMailSystem = class
  private
    // Listas principales
    FUsuarios: PUsuario;
    FComunidades: PComunidad;
    FMatrizFilas: PMatrizDispersaFila;
    FMatrizColumnas: PMatrizDispersaColumna;
    FUsuarioActual: PUsuario;
    
    // Funciones auxiliares para usuarios
    function BuscarUsuario(Email: String): PUsuario;
    function ValidarCredenciales(Email, Password: String): PUsuario;
    
    // Funciones auxiliares para correos
    function CrearCorreo(Remitente, Destinatario, Asunto, Mensaje, Fecha: String; Programado: Boolean = False): PCorreo;
    
    // Funciones auxiliares para contactos
    function BuscarContacto(Usuario: PUsuario; Email: String): PContacto;
    
    // Funciones auxiliares para matriz dispersa
    procedure ActualizarMatrizRelaciones(Remitente, Destinatario: String);
    function BuscarFilaMatriz(Email: String): PMatrizDispersaFila;
    function BuscarColumnaMatriz(Email: String): PMatrizDispersaColumna;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Funciones de usuario
    function RegistrarUsuario(Nombre, Usuario, Email, Telefono, Password: String): Boolean;
    function IniciarSesion(Email, Password: String): Boolean;
    procedure CerrarSesion;
    function GetUsuarioActual: PUsuario;
    
    // Funciones para carga masiva (ROOT)
    procedure CargarUsuariosDesdeJSON(RutaArchivo: String);
    
    // Funciones de correo
    procedure EnviarCorreo(Destinatario, Asunto, Mensaje: String);
    procedure ProgramarCorreo(Destinatario, Asunto, Mensaje, FechaEnvio: String);
    procedure EliminarCorreo(Usuario: PUsuario; CorreoId: Integer);
    procedure MarcarCorreoLeido(Usuario: PUsuario; CorreoId: Integer);
    function GetBandejaEntrada(Usuario: PUsuario): PCorreo;
    function GetPapelera(Usuario: PUsuario): PCorreo; // Pila
    function GetCorreosProgramados(Usuario: PUsuario): PCorreo; // Cola
    procedure ProcesarCorreosProgramados;
    
    // Funciones de contactos
    function AgregarContacto(Usuario: PUsuario; Email: String): Boolean;
    function GetContactos(Usuario: PUsuario): PContacto;
    
    // Funciones de comunidades
    function CrearComunidad(Nombre: String): Boolean;
    function AgregarUsuarioAComunidad(NombreComunidad, EmailUsuario: String): Boolean;
    function GetComunidades: PComunidad;
    
    // Funciones de actualización de perfil
    procedure ActualizarPerfil(Usuario: PUsuario; NuevoNombre, NuevoUsuario, NuevoTelefono: String);
    
    // Funciones de reportes
    procedure GenerarReporteUsuarios(RutaCarpeta: String);
    procedure GenerarReporteRelaciones(RutaCarpeta: String);
    procedure GenerarReporteCorreosRecibidos(Usuario: PUsuario; RutaCarpeta: String);
    procedure GenerarReportePapelera(Usuario: PUsuario; RutaCarpeta: String);
    procedure GenerarReporteCorreosProgramados(Usuario: PUsuario; RutaCarpeta: String);
    procedure GenerarReporteContactos(Usuario: PUsuario; RutaCarpeta: String);
    procedure GenerarReporteComunidades(RutaCarpeta: String);
  end;

implementation

uses
  fpjson, jsonparser;

constructor TEDDMailSystem.Create;
begin
  inherited Create;
  FUsuarios := nil;
  FComunidades := nil;
  FMatrizFilas := nil;
  FMatrizColumnas := nil;
  FUsuarioActual := nil;
  
  // Crear usuario root por defecto
  RegistrarUsuario('Root Admin', 'root', 'root@edd.com', '00000000', 'root123');
end;

destructor TEDDMailSystem.Destroy;
var
  TempUsuario: PUsuario;
  TempComunidad: PComunidad;
begin
  // Liberar memoria de usuarios
  while FUsuarios <> nil do
  begin
    TempUsuario := FUsuarios;
    FUsuarios := FUsuarios^.Siguiente;
    Dispose(TempUsuario);
  end;
  
  // Liberar memoria de comunidades
  while FComunidades <> nil do
  begin
    TempComunidad := FComunidades;
    FComunidades := FComunidades^.Siguiente;
    Dispose(TempComunidad);
  end;
  
  inherited Destroy;
end;

function TEDDMailSystem.BuscarUsuario(Email: String): PUsuario;
var
  Actual: PUsuario;
begin
  Result := nil;
  Actual := FUsuarios;
  while Actual <> nil do
  begin
    if Actual^.Email = Email then
    begin
      Result := Actual;
      Exit;
    end;
    Actual := Actual^.Siguiente;
  end;
end;

function TEDDMailSystem.ValidarCredenciales(Email, Password: String): PUsuario;
var
  Usuario: PUsuario;
begin
  Result := nil;
  Usuario := BuscarUsuario(Email);
  if (Usuario <> nil) and (Usuario^.Password = Password) then
    Result := Usuario;
end;

function TEDDMailSystem.RegistrarUsuario(Nombre, Usuario, Email, Telefono, Password: String): Boolean;
var
  NuevoUsuario, Ultimo: PUsuario;
  IdCounter: Integer;
begin
  Result := False;
  
  // Verificar que el email no exista
  if BuscarUsuario(Email) <> nil then
    Exit;
  
  // Crear nuevo usuario
  New(NuevoUsuario);
  
  // Calcular ID
  IdCounter := 1;
  Ultimo := FUsuarios;
  while Ultimo <> nil do
  begin
    if Ultimo^.Siguiente = nil then
      Break;
    IdCounter := IdCounter + 1;
    Ultimo := Ultimo^.Siguiente;
  end;
  
  // Asignar valores
  NuevoUsuario^.Id := IdCounter;
  NuevoUsuario^.Nombre := Nombre;
  NuevoUsuario^.Usuario := Usuario;
  NuevoUsuario^.Email := Email;
  NuevoUsuario^.Telefono := Telefono;
  NuevoUsuario^.Password := Password;
  NuevoUsuario^.Siguiente := nil;
  
  // Agregar a la lista
  if FUsuarios = nil then
    FUsuarios := NuevoUsuario
  else
    Ultimo^.Siguiente := NuevoUsuario;
  
  Result := True;
end;

function TEDDMailSystem.IniciarSesion(Email, Password: String): Boolean;
begin
  FUsuarioActual := ValidarCredenciales(Email, Password);
  Result := FUsuarioActual <> nil;
end;

procedure TEDDMailSystem.CerrarSesion;
begin
  FUsuarioActual := nil;
end;

function TEDDMailSystem.GetUsuarioActual: PUsuario;
begin
  Result := FUsuarioActual;
end;

procedure TEDDMailSystem.CargarUsuariosDesdeJSON(RutaArchivo: String);
var
  JsonData: TJSONData;
  JsonObj: TJSONObject;
  UsuariosArray: TJSONArray;
  UsuarioObj: TJSONObject;
  FileStream: TFileStream;
  JsonString: String;
  i: Integer;
begin
  try
    FileStream := TFileStream.Create(RutaArchivo, fmOpenRead);
    try
      SetLength(JsonString, FileStream.Size);
      FileStream.ReadBuffer(JsonString[1], FileStream.Size);
    finally
      FileStream.Free;
    end;
    
    JsonData := GetJSON(JsonString);
    try
      JsonObj := JsonData as TJSONObject;
      UsuariosArray := JsonObj.Arrays['usuarios'];
      
      for i := 0 to UsuariosArray.Count - 1 do
      begin
        UsuarioObj := UsuariosArray.Objects[i];
        RegistrarUsuario(
          UsuarioObj.Strings['nombre'],
          UsuarioObj.Strings['usuario'],
          UsuarioObj.Strings['email'],
          UsuarioObj.Strings['telefono'],
          'password123' // Password por defecto
        );
      end;
    finally
      JsonData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error al cargar JSON: ', E.Message);
  end;
end;

function TEDDMailSystem.CrearCorreo(Remitente, Destinatario, Asunto, Mensaje, Fecha: String; Programado: Boolean): PCorreo;
begin
  New(Result);
  Result^.Id := Random(9999) + 1;
  Result^.Remitente := Remitente;
  Result^.Destinatario := Destinatario;
  Result^.Estado := 'NL'; // No Leído
  Result^.Programado := Programado;
  Result^.Asunto := Asunto;
  Result^.Fecha := Fecha;
  Result^.Mensaje := Mensaje;
  Result^.Anterior := nil;
  Result^.Siguiente := nil;
end;

procedure TEDDMailSystem.EnviarCorreo(Destinatario, Asunto, Mensaje: String);
var
  NuevoCorreo: PCorreo;
  UsuarioDestino: PUsuario;
begin
  if FUsuarioActual = nil then
    Exit;
  
  UsuarioDestino := BuscarUsuario(Destinatario);
  if UsuarioDestino = nil then
    Exit;
  
  // Verificar que sea contacto
  if BuscarContacto(FUsuarioActual, Destinatario) = nil then
    Exit;
  
  NuevoCorreo := CrearCorreo(FUsuarioActual^.Email, Destinatario, Asunto, Mensaje, 
                           FormatDateTime('dd/mm/yy hh:nn', Now));
  
  // Agregar a bandeja de entrada del destinatario (lista doblemente enlazada)
  // Aquí iría la lógica para agregar a la bandeja del destinatario
  
  // Actualizar matriz de relaciones
  ActualizarMatrizRelaciones(FUsuarioActual^.Email, Destinatario);
end;

function TEDDMailSystem.BuscarContacto(Usuario: PUsuario; Email: String): PContacto;
begin
  Result := nil;
  // Implementar búsqueda en lista circular de contactos del usuario
end;

procedure TEDDMailSystem.ActualizarMatrizRelaciones(Remitente, Destinatario: String);
begin
  // Implementar lógica de matriz dispersa
end;

function TEDDMailSystem.BuscarFilaMatriz(Email: String): PMatrizDispersaFila;
begin
  Result := nil;
  // Implementar búsqueda en filas de matriz
end;

function TEDDMailSystem.BuscarColumnaMatriz(Email: String): PMatrizDispersaColumna;
begin
  Result := nil;
  // Implementar búsqueda en columnas de matriz
end;

// Implementar resto de métodos...
procedure TEDDMailSystem.ProgramarCorreo(Destinatario, Asunto, Mensaje, FechaEnvio: String);
begin
  // Implementar cola para correos programados
end;

procedure TEDDMailSystem.EliminarCorreo(Usuario: PUsuario; CorreoId: Integer);
begin
  // Implementar pila para correos eliminados
end;

procedure TEDDMailSystem.MarcarCorreoLeido(Usuario: PUsuario; CorreoId: Integer);
begin
  // Cambiar estado de correo a 'L'
end;

function TEDDMailSystem.GetBandejaEntrada(Usuario: PUsuario): PCorreo;
begin
  Result := nil;
  // Retornar lista doblemente enlazada de correos del usuario
end;

function TEDDMailSystem.GetPapelera(Usuario: PUsuario): PCorreo;
begin
  Result := nil;
  // Retornar pila de correos eliminados
end;

function TEDDMailSystem.GetCorreosProgramados(Usuario: PUsuario): PCorreo;
begin
  Result := nil;
  // Retornar cola de correos programados
end;

procedure TEDDMailSystem.ProcesarCorreosProgramados;
begin
  // Procesar cola FIFO de correos programados
end;

function TEDDMailSystem.AgregarContacto(Usuario: PUsuario; Email: String): Boolean;
begin
  Result := False;
  // Implementar lista circular de contactos
end;

function TEDDMailSystem.GetContactos(Usuario: PUsuario): PContacto;
begin
  Result := nil;
  // Retornar lista circular de contactos
end;

function TEDDMailSystem.CrearComunidad(Nombre: String): Boolean;
begin
  Result := False;
  // Implementar lista de comunidades
end;

function TEDDMailSystem.AgregarUsuarioAComunidad(NombreComunidad, EmailUsuario: String): Boolean;
begin
  Result := False;
  // Implementar lista de listas para usuarios en comunidades
end;

function TEDDMailSystem.GetComunidades: PComunidad;
begin
  Result := FComunidades;
end;

procedure TEDDMailSystem.ActualizarPerfil(Usuario: PUsuario; NuevoNombre, NuevoUsuario, NuevoTelefono: String);
begin
  if Usuario <> nil then
  begin
    Usuario^.Nombre := NuevoNombre;
    Usuario^.Usuario := NuevoUsuario;
    Usuario^.Telefono := NuevoTelefono;
  end;
end;

// Funciones de reportes usando Graphviz
procedure TEDDMailSystem.GenerarReporteUsuarios(RutaCarpeta: String);
var
  Archivo: TextFile;
  Usuario: PUsuario;
  Comando: String;
begin
  ForceDirectories(RutaCarpeta);
  
  AssignFile(Archivo, RutaCarpeta + '/usuarios.dot');
  Rewrite(Archivo);
  
  WriteLn(Archivo, 'digraph G {');
  WriteLn(Archivo, '    rankdir=LR;');
  WriteLn(Archivo, '    node [shape=record];');
  
  Usuario := FUsuarios;
  while Usuario <> nil do
  begin
    WriteLn(Archivo, Format('    user%d [label="ID: %d|Nombre: %s|Usuario: %s|Email: %s|Telefono: %s"];',
      [Usuario^.Id, Usuario^.Id, Usuario^.Nombre, Usuario^.Usuario, Usuario^.Email, Usuario^.Telefono]));
    
    if Usuario^.Siguiente <> nil then
      WriteLn(Archivo, Format('    user%d -> user%d;', [Usuario^.Id, Usuario^.Siguiente^.Id]));
    
    Usuario := Usuario^.Siguiente;
  end;
  
  WriteLn(Archivo, '}');
  CloseFile(Archivo);
  
  // Generar imagen
  Comando := Format('dot -Tpng "%s/usuarios.dot" -o "%s/usuarios.png"', [RutaCarpeta, RutaCarpeta]);
  // Ejecutar comando del sistema para generar imagen
end;

procedure TEDDMailSystem.GenerarReporteRelaciones(RutaCarpeta: String);
begin
  // Implementar reporte de matriz dispersa
end;

procedure TEDDMailSystem.GenerarReporteCorreosRecibidos(Usuario: PUsuario; RutaCarpeta: String);
begin
  // Implementar reporte de lista doblemente enlazada
end;

procedure TEDDMailSystem.GenerarReportePapelera(Usuario: PUsuario; RutaCarpeta: String);
begin
  // Implementar reporte de pila
end;

procedure TEDDMailSystem.GenerarReporteCorreosProgramados(Usuario: PUsuario; RutaCarpeta: String);
begin
  // Implementar reporte de cola
end;

procedure TEDDMailSystem.GenerarReporteContactos(Usuario: PUsuario; RutaCarpeta: String);
begin
  // Implementar reporte de lista circular
end;

procedure TEDDMailSystem.GenerarReporteComunidades(RutaCarpeta: String);
begin
  // Implementar reporte de lista de listas
end;

end.
