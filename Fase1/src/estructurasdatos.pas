unit EstructurasDatos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, process, DateUtils;

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
  //Fase 2

    // Nodo para Árbol AVL (Borradores)
  PNodoAVL = ^TNodoAVL;
  TNodoAVL = record
    Correo: PCorreo;
    Altura: Integer;
    Izquierdo: PNodoAVL;
    Derecho: PNodoAVL;
  end;

  // Nodo para BST (Comunidades)
  PNodoBST = ^TNodoBST;
  PMensajeComunidad = ^TMensajeComunidad;

  TMensajeComunidad = record
    Correo: String;
    Mensaje: String;
    FechaPublicacion: String;
    Siguiente: PMensajeComunidad;
  end;

  TNodoBST = record
    NombreComunidad: String;
    FechaCreacion: String;
    NumeroMensajes: Integer;
    ListaMensajes: PMensajeComunidad;
    Izquierdo: PNodoBST;
    Derecho: PNodoBST;
  end;

  // Nodo para Árbol B (Favoritos)
  PNodoB = ^TNodoB;
  TNodoB = record
    NumClaves: Integer;
    Claves: array[0..3] of Integer; // Máximo 4 claves
    Correos: array[0..3] of PCorreo; // Correos asociados a claves
    Hijos: array[0..4] of PNodoB; // Máximo 5 hijos
    EsHoja: Boolean;
  end;

  // Estructura Usuario (Lista Simple)
  TUsuario = record
    Id: Integer;
    Nombre: String;
    Usuario: String;
    Email: String;
    Telefono: String;
    Password: String;
    Siguiente: PUsuario;
    ListaContactos: PContacto;    // Lista circular de contactos del usuario
    BandejaEntrada: PCorreo;      // Lista doblemente enlazada
    Papelera: PCorreo;            // Pila LIFO
    CorreosProgramados: PCorreo;  // Cola FIFO
    ArbolBorradores: PNodoAVL;      // Árbol AVL para borradores
    ArbolFavoritos: PNodoB;         // Árbol B para favoritos
    end;

  // Estructura Correo (Lista Doblemente Enlazada para bandeja de entrada)
  TCorreo = record
    Id: Integer;
    Remitente: String;
    Destinatario: String;
    Estado: String; // 'NL' = No Leído, 'L' = Leído
    Programado: Boolean;
     Cuerpo: String;        // nueva línea para manejar el cuerpo del correo
  FechaHora: TDateTime;  // nueva línea para menejar fecha
    Asunto: String;
    Fecha: String;
    Mensaje: String;
    FechaEnvio: String; // Para correos programados
    Anterior: PCorreo;  // enlace hacia el nodo previo
    Siguiente: PCorreo; // enlace hacia el nodo siguiente
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
      FArbolComunidades: PNodoBST;    // Árbol BST de comunidades

      // Funciones auxiliares para correos
      function CrearCorreo(Remitente, Destinatario, Asunto, Mensaje, Fecha: String; Programado: Boolean = False; IdFijo: Integer = -1): PCorreo;
      procedure AgregarContactoALista(var PrimerContacto: PContacto; NuevoContacto: PContacto);

      // Funciones auxiliares para matriz dispersa
      function BuscarFilaMatriz(Email: String): PMatrizDispersaFila;
      function BuscarColumnaMatriz(Email: String): PMatrizDispersaColumna;
      function BuscarUsuarioPorId(IdBuscado: Integer): PUsuario;
      procedure Inbox_InsertTail(var Head: PCorreo; NewNode: PCorreo);

      // FASE 2 - Funciones para AVL
      function RotarDerecha(y: PNodoAVL): PNodoAVL;
      function RotarIzquierda(x: PNodoAVL): PNodoAVL;
      function ObtenerAltura(nodo: PNodoAVL): Integer;
      function ObtenerBalance(nodo: PNodoAVL): Integer;
      function InsertarAVL(nodo: PNodoAVL; correo: PCorreo): PNodoAVL;

      // Funciones para métodos de Fase 2

      function BuscarCorreoEnAVL(nodo: PNodoAVL; CorreoId: Integer): PCorreo;
      procedure GenerarNodosAVL(var Archivo: TextFile; nodo: PNodoAVL);

      // Funciones para BST
      function InsertarBST(nodo: PNodoBST; nombreComunidad: String): PNodoBST;
      function BuscarComunidadBST(nodo: PNodoBST; nombre: String): PNodoBST;

      // Funciones para Árbol B
      function CrearNodoB: PNodoB;
      function InsertarB(raiz: PNodoB; correo: PCorreo): PNodoB;

      // Funciones auxiliares adicionales
      function BuscarCorreoEnBandeja(Usuario: PUsuario; CorreoId: Integer): PCorreo;

      // Recorridos del Árbol AVL
      procedure RecorridoInOrdenAVL(nodo: PNodoAVL; lista: TStringList);
      procedure RecorridoPreOrdenAVL(nodo: PNodoAVL; lista: TStringList);
      procedure RecorridoPostOrdenAVL(nodo: PNodoAVL; lista: TStringList);

      // MÉTODOS AUXILIARES ADICIONALES PARA FASE 2:
      procedure GenerarNodosB(var Archivo: TextFile; nodo: PNodoB; nivel: Integer);
      procedure GenerarNodosBST(var Archivo: TextFile; nodo: PNodoBST);


          // NUEVAS FUNCIONES PRIVATE PARA ÁRBOL B:
      procedure DividirNodoB(nodo: PNodoB; indiceHijo: Integer; var nuevaRaiz: PNodoB);
      procedure InsertarEnNodoNoLleno(nodo: PNodoB; correo: PCorreo);
      function ValidarPropiedadesB(nodo: PNodoB): Boolean;
      procedure LiberarArbolB(var raiz: PNodoB);
      function ValidarEstructuraArbolB(nodo: PNodoB): Boolean;  // ← AQUÍ
      procedure GenerarNodoHijoConId(var Archivo: TextFile; nodo: PNodoB; nivel: Integer; const NodoId: String); // Nodos hijos
      // Funciones adicionales para AVL
      function EliminarAVL(nodo: PNodoAVL; id: Integer): PNodoAVL;
      function BuscarMinimoAVL(nodo: PNodoAVL): PNodoAVL;






    public
      constructor Create;
      destructor Destroy; override;

      // Funciones de usuario
      function RegistrarUsuario(Nombre, Usuario, Email, Telefono, Password: String; IdFijo: Integer = -1): Boolean;
      function IniciarSesion(Email, Password: String): Boolean;
      procedure CerrarSesion;
      function GetUsuarioActual: PUsuario;
      function ListarComunidades: String;

      // Funciones para carga masiva (ROOT)
      procedure CargarUsuariosDesdeJSON(RutaArchivo: String);
      procedure CargarCorreosDesdeJSON(const RutaArchivo: String);

      // Funciones de correo
      procedure ActualizarMatrizRelaciones(Remitente, Destinatario: String);
      procedure EnviarCorreo(Destinatario, Asunto, Mensaje: String);
      procedure ProgramarCorreo(Destinatario, Asunto, Mensaje, FechaEnvio: String);
      procedure EliminarCorreo(Usuario: PUsuario; CorreoId: Integer);
      procedure MarcarCorreoLeido(Usuario: PUsuario; CorreoId: Integer);
      function GetBandejaEntrada(Usuario: PUsuario): PCorreo;
      function GetPapelera(Usuario: PUsuario): PCorreo; // Pila
      function GetCorreosProgramados(Usuario: PUsuario): PCorreo; // Cola
      procedure ProcesarCorreosProgramados;

      // Funciones de usuarios y contactos
      function EliminarContacto(Usuario: PUsuario; Email: String): Boolean;
      function BuscarUsuario(Email: String): PUsuario;
      function ValidarCredenciales(Email, Password: String): PUsuario;
      function AgregarContacto(Usuario: PUsuario; Email: String): Boolean;
      function GetContactos(Usuario: PUsuario): PContacto;
      function BuscarContacto(Usuario: PUsuario; Email: String): PContacto;
      function CrearContacto(Email: String): PContacto;
      function ContarContactos(PrimerContacto: PContacto): Integer;

      // Funciones de comunidades (Fase 1)
      function CrearComunidad(Nombre: String): Boolean;
      function AgregarUsuarioAComunidad(NombreComunidad, EmailUsuario: String): Boolean;
      function GetComunidades: PComunidad;

      // Funciones de actualización de perfil
      procedure ActualizarPerfil(Usuario: PUsuario; NuevoNombre, NuevoUsuario, NuevoTelefono: String);

      // Funciones de reportes (Fase 1)
      procedure GenerarReporteUsuarios(RutaCarpeta: String);
      procedure GenerarReporteRelaciones(RutaCarpeta: String);
      procedure GenerarReporteCorreosRecibidos(Usuario: PUsuario; RutaCarpeta: String);
      procedure GenerarReportePapelera(Usuario: PUsuario; RutaCarpeta: String);
      procedure GenerarReporteCorreosProgramados(Usuario: PUsuario; RutaCarpeta: String);
      procedure GenerarReporteContactos(Usuario: PUsuario; RutaCarpeta: String);
      procedure GenerarReporteComunidades(RutaCarpeta: String);

      // FASE 2 - Nuevas funciones necesarias
      function GuardarBorrador(Usuario: PUsuario; Destinatario, Asunto, Mensaje: String): Boolean;
      function ObtenerBorradores(Usuario: PUsuario; tipoRecorrido: String): TStringList;
      function MarcarComoFavorito(Usuario: PUsuario; CorreoId: Integer): Boolean;
      function CrearComunidadBST(nombreComunidad: String): Boolean;
      function PublicarMensajeAComunidad(nombreComunidad, correoUsuario, mensaje: String): Boolean;

      // MÉTODOS PÚBLICOS ADICIONALES PARA FASE 2:
      procedure GenerarReporteComunidadesBST(RutaCarpeta: String);
      procedure GenerarReporteFavoritos(Usuario: PUsuario; RutaCarpeta: String);
      function ObtenerMensajesComunidad(nombreComunidad: String): String;
      procedure GenerarReporteBorradores(Usuario: PUsuario; RutaCarpeta: String);

      function BuscarB(nodo: PNodoB; id: Integer): PCorreo;


      function EliminarFavorito(Usuario: PUsuario; CorreoId: Integer): Boolean;
      procedure RecorrerArbolB(nodo: PNodoB; lista: TStringList); // ← AGREGAR ESTA LÍNEA

                // NUEVAS FUNCIONES PUBLIC PARA ÁRBOL B:
      function DesmarcarFavorito(Usuario: PUsuario; CorreoId: Integer): Boolean;
      function BuscarEnFavoritos(Usuario: PUsuario; CorreoId: Integer): PCorreo;
      function ContarFavoritos(Usuario: PUsuario): Integer;

      // Recorridos del Árbol B
      procedure RecorridoInOrdenB(nodo: PNodoB; lista: TStringList);

      // Funciones de validación
      function ObtenerAlturaArbolB(nodo: PNodoB): Integer;
      function ObtenerNumeroNodos(nodo: PNodoB): Integer;
      function EsArbolBValido(nodo: PNodoB): Boolean;

      // Métodos  para borradores
      function BuscarBorrador(Usuario: PUsuario; Id: Integer): PCorreo;
      function EliminarBorrador(Usuario: PUsuario; Id: Integer): Boolean;
      function ActualizarBorrador(Usuario: PUsuario; Id: Integer;
        NuevoDestinatario, NuevoAsunto, NuevoCuerpo: String): Boolean;

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
    FArbolComunidades := nil;  // se inicializa árbol de comunidades


  // Crear usuario root por defecto (Id fijo = 0)
  RegistrarUsuario('Root Admin', 'root', 'root@edd.com', '00000000', 'root123', 0);
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
function TEDDMailSystem.BuscarComunidadBST(nodo: PNodoBST; nombre: String): PNodoBST;
begin
  Result := nil;
  if nodo = nil then Exit;

  if nombre = nodo^.NombreComunidad then
    Result := nodo
  else if nombre < nodo^.NombreComunidad then
    Result := BuscarComunidadBST(nodo^.Izquierdo, nombre)
  else
    Result := BuscarComunidadBST(nodo^.Derecho, nombre);
end;

function TEDDMailSystem.BuscarB(nodo: PNodoB; id: Integer): PCorreo;
var
  i: Integer;
begin
  Result := nil;
  if nodo = nil then Exit;

  for i := 0 to nodo^.NumClaves - 1 do
  begin
    if nodo^.Claves[i] = id then
    begin
      Result := nodo^.Correos[i];
      Exit;
    end;
  end;
end;

function TEDDMailSystem.EliminarFavorito(Usuario: PUsuario; CorreoId: Integer): Boolean;

  function EliminarDeNodoB(nodo: PNodoB; id: Integer): Boolean;
  var
    i, j: Integer;
  begin
    Result := False;
    if nodo = nil then Exit;

    // Buscar la clave en el nodo actual
    for i := 0 to nodo^.NumClaves - 1 do
    begin
      if nodo^.Claves[i] = id then
      begin
        // Encontrado - eliminar desplazando elementos
        for j := i to nodo^.NumClaves - 2 do
        begin
          nodo^.Claves[j] := nodo^.Claves[j + 1];
          nodo^.Correos[j] := nodo^.Correos[j + 1];
        end;

        // Limpiar último elemento
        nodo^.Claves[nodo^.NumClaves - 1] := 0;
        nodo^.Correos[nodo^.NumClaves - 1] := nil;
        Dec(nodo^.NumClaves);

        Result := True;
        Exit;
      end;
    end;

    // Si no se encontró y no es hoja, buscar en hijos
    if not nodo^.EsHoja then
    begin
      for i := 0 to nodo^.NumClaves do
      begin
        if EliminarDeNodoB(nodo^.Hijos[i], id) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;

begin
  Result := False;
  if Usuario = nil then Exit;

  Result := EliminarDeNodoB(Usuario^.ArbolFavoritos, CorreoId);
  if Result then
    WriteLn('Favorito eliminado: ID ', CorreoId)
  else
    WriteLn('Favorito no encontrado: ID ', CorreoId);
end;
procedure TEDDMailSystem.RecorrerArbolB(nodo: PNodoB; lista: TStringList);
var
  i: Integer;
  Display: String;
begin
  if nodo = nil then Exit;

  // Recorrer todas las claves del nodo actual
  for i := 0 to nodo^.NumClaves - 1 do
  begin
    if nodo^.Correos[i] <> nil then
    begin
      Display := Format('[ID: %d] %s — %s (%s)',
        [nodo^.Claves[i],
         nodo^.Correos[i]^.Asunto,
         nodo^.Correos[i]^.Remitente,
         nodo^.Correos[i]^.Fecha]);
      lista.AddObject(Display, TObject(PtrInt(nodo^.Claves[i])));
    end;
  end;

  // Recorrer los hijos si no es hoja
  if not nodo^.EsHoja then
  begin
    for i := 0 to nodo^.NumClaves do
    begin
      RecorrerArbolB(nodo^.Hijos[i], lista);
    end;
  end;
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
function TEDDMailSystem.ListarComunidades: String;
var
  Comunidad: PComunidad;
  UsuarioCom: PUsuarioComunidad;
begin
  Result := '';
  Comunidad := FComunidades;

  if Comunidad = nil then
  begin
    Result := 'No hay comunidades creadas.';
    Exit;
  end;

  while Comunidad <> nil do
  begin
    Result := Result + 'Comunidad: ' + Comunidad^.Nombre + LineEnding;
    Result := Result + 'Usuarios:' + LineEnding;

    UsuarioCom := Comunidad^.UsuariosList;
    if UsuarioCom = nil then
      Result := Result + '  (sin usuarios)' + LineEnding
    else
    begin
      while UsuarioCom <> nil do
      begin
        Result := Result + '  - ' + UsuarioCom^.Email + LineEnding;
        UsuarioCom := UsuarioCom^.Siguiente;
      end;
    end;

    Result := Result + LineEnding;
    Comunidad := Comunidad^.Siguiente;
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

// modificando  RegistrarUsuario para inicializar nuevas estructuras
function TEDDMailSystem.RegistrarUsuario(Nombre, Usuario, Email, Telefono, Password: String; IdFijo: Integer): Boolean;
var
  NuevoUsuario, Cur: PUsuario;
  MaxId: Integer;
begin
  Result := False;

  // Verificar si el email no exista
  if BuscarUsuario(Email) <> nil then
    Exit;

  // Validar que el IdFijo no esté duplicado
  if (IdFijo >= 0) and (BuscarUsuarioPorId(IdFijo) <> nil) then
  begin
    WriteLn('Error: id duplicado en JSON: ', IdFijo, ' (', Email, ')');
    Exit;
  end;

  // Crea nuevo usuario
  New(NuevoUsuario);

  // Calcular MaxId actual
  MaxId := 0;
  Cur := FUsuarios;
  while Cur <> nil do
  begin
    if Cur^.Id > MaxId then
      MaxId := Cur^.Id;
    Cur := Cur^.Siguiente;
  end;

  if IdFijo >= 0 then
    NuevoUsuario^.Id := IdFijo
  else
    NuevoUsuario^.Id := MaxId + 1;

  // Asignar valores
  NuevoUsuario^.Nombre := Nombre;
  NuevoUsuario^.Usuario := Usuario;
  NuevoUsuario^.Email := Email;
  NuevoUsuario^.Telefono := Telefono;
  NuevoUsuario^.Password := Password;
  NuevoUsuario^.Siguiente := nil;

  // INICIALIZAR ESTRUCTURAS EXISTENTES
  NuevoUsuario^.ListaContactos := nil;
  NuevoUsuario^.BandejaEntrada := nil;
  NuevoUsuario^.Papelera := nil;
  NuevoUsuario^.CorreosProgramados := nil;

  // INICIALIZAR ESTRUCTURAS FASE 2
  NuevoUsuario^.ArbolBorradores := nil;
  NuevoUsuario^.ArbolFavoritos := nil;

  // Agregar a la lista (se agregan al final)
  if FUsuarios = nil then
    FUsuarios := NuevoUsuario
  else
  begin
    Cur := FUsuarios;
    while Cur^.Siguiente <> nil do
      Cur := Cur^.Siguiente;
    Cur^.Siguiente := NuevoUsuario;
  end;

  Result := True;
end;

  // Implementación GetContactos:
function TEDDMailSystem.GetContactos(Usuario: PUsuario): PContacto;
begin
  Result := nil;
  if Usuario <> nil then
    Result := Usuario^.ListaContactos;
end;

// Implementación de AgregarContacto:
function TEDDMailSystem.AgregarContacto(Usuario: PUsuario; Email: String): Boolean;
var
  NuevoContacto: PContacto;
begin
  Result := False;

  if Usuario = nil then
    Exit;

  // Verificar que el contacto no sea el mismo usuario
  if Usuario^.Email = Email then
  begin
    WriteLn('Error: No puede agregarse a sí mismo como contacto');
    Exit;
  end;

  // Verificar que no esté ya en contactos
  if BuscarContacto(Usuario, Email) <> nil then
  begin
    WriteLn('Error: El contacto ya existe en la lista');
    Exit;
  end;

  // Crear el nuevo contacto
  NuevoContacto := CrearContacto(Email);
  if NuevoContacto = nil then
  begin
    WriteLn('Error: El usuario no existe en el sistema');
    Exit;
  end;

  // Agregar a la lista circular del usuario específico
  AgregarContactoALista(Usuario^.ListaContactos, NuevoContacto);

  WriteLn('Contacto agregado exitosamente: ', Email);
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
  PasswordUsuario: String;
  IdJson: Integer;

begin
  JsonString := '';
  try
    if not FileExists(RutaArchivo) then
    begin
      WriteLn('Error: Archivo JSON no existe: ', RutaArchivo);
      Exit;
    end;

    FileStream := TFileStream.Create(RutaArchivo, fmOpenRead);
    try
      SetLength(JsonString, FileStream.Size);
      if FileStream.Size > 0 then
        FileStream.ReadBuffer(JsonString[1], FileStream.Size);
    finally
      FileStream.Free;
    end;

    if JsonString = '' then
    begin
      WriteLn('Error: Archivo JSON está vacío');
      Exit;
    end;

    JsonData := GetJSON(JsonString);
    try
      JsonObj := JsonData as TJSONObject;
      UsuariosArray := JsonObj.Arrays['usuarios'];

      for i := 0 to UsuariosArray.Count - 1 do
      begin
        UsuarioObj := UsuariosArray.Objects[i];

        // Leer password del JSON si existe, sino usar genérico
        if UsuarioObj.Find('password') <> nil then
          PasswordUsuario := UsuarioObj.Strings['password']
        else
          PasswordUsuario := 'password123';

        // Leer id del JSON (si no está, -1)
        IdJson := UsuarioObj.Get('id', -1);

        if RegistrarUsuario(
          UsuarioObj.Strings['nombre'],
          UsuarioObj.Strings['usuario'],
          UsuarioObj.Strings['email'],
          UsuarioObj.Strings['telefono'],
          PasswordUsuario,  // Password del JSON o genérico
          IdJson            // Id del JSON (o -1)
        ) then
          WriteLn('Usuario cargado: ', UsuarioObj.Strings['email'])
        else
          WriteLn('Error al cargar usuario: ', UsuarioObj.Strings['email']);
      end;
    finally
      JsonData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error al cargar JSON: ', E.Message);
  end;
end;

function TEDDMailSystem.BuscarUsuarioPorId(IdBuscado: Integer): PUsuario;
var
  U: PUsuario;
begin
  Result := nil;
  U := FUsuarios;
  while U <> nil do
  begin
    if U^.Id = IdBuscado then
    begin
      Result := U;
      Exit;
    end;
    U := U^.Siguiente;
  end;
end;

procedure TEDDMailSystem.Inbox_InsertTail(var Head: PCorreo; NewNode: PCorreo);
var
  P: PCorreo;
begin
  if NewNode = nil then Exit;
  NewNode^.Anterior := nil;
  NewNode^.Siguiente := nil;

  if Head = nil then
  begin
    Head := NewNode;
    Exit;
  end;

  P := Head;
  while P^.Siguiente <> nil do
    P := P^.Siguiente;

  // enlazar doblemente
  P^.Siguiente := NewNode;
  NewNode^.Anterior := P;
end;

procedure TEDDMailSystem.CargarCorreosDesdeJSON(const RutaArchivo: String);
var
  JsonData: TJSONData;
  Root, ObjUsuario, MailObj: TJSONObject;
  ArrCorreos, Inbox: TJSONArray;
  i, k, UsuarioId: Integer;
  U: PUsuario;
  C: PCorreo;
  FS: TFileStream;
  S: String;
  estadoTxt, progTxt: String;
  destinatarioEmail: String;
  EsEstructuraAgrupada: Boolean;
begin
  S := '';

  if not FileExists(RutaArchivo) then
  begin
    WriteLn('Error: Archivo JSON de correos no existe: ', RutaArchivo);
    Exit;
  end;

  // Leer archivo completo
  FS := TFileStream.Create(RutaArchivo, fmOpenRead);
  try
    SetLength(S, FS.Size);
    if FS.Size > 0 then FS.ReadBuffer(S[1], FS.Size);
  finally
    FS.Free;
  end;

  if S = '' then
  begin
    WriteLn('Error: Archivo de correos vacío');
    Exit;
  end;

  JsonData := GetJSON(S);
  try
    Root := JsonData as TJSONObject;
    ArrCorreos := Root.Arrays['correos'];

    // Detectar estructura del JSON
    if ArrCorreos.Count > 0 then
    begin
      ObjUsuario := ArrCorreos.Objects[0];
      // Si tiene 'usuario_id' y 'bandeja_entrada', es estructura agrupada
      EsEstructuraAgrupada := (ObjUsuario.Find('usuario_id') <> nil) and
                               (ObjUsuario.Find('bandeja_entrada') <> nil);
    end
    else
      Exit; // Array vacío

    // ========== ESTRUCTURA AGRUPADA (por usuario) ==========
    if EsEstructuraAgrupada then
    begin
      WriteLn('Detectada estructura AGRUPADA por usuario');
      for i := 0 to ArrCorreos.Count - 1 do
      begin
        ObjUsuario := ArrCorreos.Objects[i];
        UsuarioId := ObjUsuario.Get('usuario_id', 0);

        U := BuscarUsuarioPorId(UsuarioId);
        if U = nil then
        begin
          WriteLn('Aviso: usuario_id ', UsuarioId, ' no existe. Se omite su bandeja.');
          Continue;
        end;

        Inbox := ObjUsuario.Arrays['bandeja_entrada'];
        for k := 0 to Inbox.Count - 1 do
        begin
          MailObj := Inbox.Objects[k];

          New(C);
          C^.Id          := MailObj.Get('id', 0);
          C^.Remitente   := MailObj.Get('remitente', '');
          C^.Destinatario:= U^.Email;
          estadoTxt      := LowerCase(MailObj.Get('estado',''));
          if (Pos('no', estadoTxt) > 0) or (estadoTxt = 'nl') then
            C^.Estado := 'NL'
          else
            C^.Estado := 'L';

          progTxt        := LowerCase(MailObj.Get('programado','no'));
          C^.Programado  := (progTxt = 'si') or (progTxt = 'sí');
          C^.Asunto      := MailObj.Get('asunto', '');
          C^.Fecha       := MailObj.Get('fecha', '');
          C^.Mensaje     := MailObj.Get('mensaje', '');
          C^.FechaEnvio  := '';

          C^.Anterior := nil;
          C^.Siguiente := nil;

          Inbox_InsertTail(U^.BandejaEntrada, C);
          ActualizarMatrizRelaciones(C^.Remitente, U^.Email);
        end;
      end;
    end
    // ========== ESTRUCTURA PLANA (lista de correos) ==========
    else
    begin
      WriteLn('Detectada estructura PLANA (lista de correos)');
      for i := 0 to ArrCorreos.Count - 1 do
      begin
        MailObj := ArrCorreos.Objects[i];

        // Obtener destinatario del correo
        destinatarioEmail := MailObj.Get('destinatario', '');
        if destinatarioEmail = '' then
        begin
          WriteLn('Aviso: Correo sin destinatario, se omite.');
          Continue;
        end;

        // Buscar usuario destinatario
        U := BuscarUsuario(destinatarioEmail);
        if U = nil then
        begin
          WriteLn('Aviso: destinatario ', destinatarioEmail, ' no existe. Se omite correo.');
          Continue;
        end;

        // Crear correo
        New(C);
        C^.Id          := MailObj.Get('id', 0);
        C^.Remitente   := MailObj.Get('remitente', '');
        C^.Destinatario:= destinatarioEmail;

        estadoTxt      := LowerCase(MailObj.Get('estado',''));
        if (Pos('no', estadoTxt) > 0) or (estadoTxt = 'nl') then
          C^.Estado := 'NL'
        else if (estadoTxt = 'eliminado') then
          C^.Estado := 'ELIMINADO'
        else
          C^.Estado := 'L';

        progTxt        := LowerCase(MailObj.Get('programado','no'));
        C^.Programado  := (progTxt = 'si') or (progTxt = 'sí');
        C^.Asunto      := MailObj.Get('asunto', '');
        C^.Fecha       := MailObj.Get('fecha', FormatDateTime('dd/mm/yy hh:nn', Now));
        C^.Mensaje     := MailObj.Get('mensaje', '');
        C^.FechaEnvio  := '';

        C^.Anterior := nil;
        C^.Siguiente := nil;

        // Insertar en bandeja de entrada o papelera según estado
        if C^.Estado = 'ELIMINADO' then
        begin
          Inbox_InsertTail(U^.Papelera, C);
          WriteLn('  -> Correo ID ', C^.Id, ' agregado a PAPELERA de ', U^.Email);
        end
        else
        begin
          Inbox_InsertTail(U^.BandejaEntrada, C);
          WriteLn('  -> Correo ID ', C^.Id, ' agregado a BANDEJA de ', U^.Email);
        end;

        ActualizarMatrizRelaciones(C^.Remitente, U^.Email);
      end;
    end;

    WriteLn('Carga de correos completada desde: ', RutaArchivo);
  finally
    JsonData.Free;
  end;
end;


function TEDDMailSystem.CrearCorreo(
  Remitente, Destinatario, Asunto, Mensaje, Fecha: String;
  Programado: Boolean = False; IdFijo: Integer = -1
): PCorreo;
begin
  New(Result);

  if IdFijo >= 0 then
    Result^.Id := IdFijo
  else
    Result^.Id := Random(9999) + 1;

  Result^.Remitente    := Remitente;
  Result^.Destinatario := Destinatario;
  Result^.Estado       := 'NL';        // No leído por defecto
  Result^.Programado   := Programado;
  Result^.Asunto       := Asunto;
  Result^.Fecha        := Fecha;
  Result^.Mensaje      := Mensaje;

  if Programado then
    Result^.FechaEnvio := Fecha      // para programados usas la fecha indicada
  else
    Result^.FechaEnvio := FormatDateTime('dd/mm/yy hh:nn', Now); // enviado ahora

  Result^.Anterior := nil;
  Result^.Siguiente := nil;
end;


procedure TEDDMailSystem.EnviarCorreo(Destinatario, Asunto, Mensaje: String);
var
  NuevoCorreo: PCorreo;
  UsuarioDestino: PUsuario;
begin
  if FUsuarioActual = nil then Exit;

  UsuarioDestino := BuscarUsuario(Destinatario);
  if UsuarioDestino = nil then Exit;

  // Verificar que sea contacto
  if BuscarContacto(FUsuarioActual, Destinatario) = nil then Exit;

  NuevoCorreo := CrearCorreo(
    FUsuarioActual^.Email, Destinatario, Asunto, Mensaje,
    FormatDateTime('dd/mm/yy hh:nn', Now)
  );

  // INSERTAR en bandeja del destinatario (lista doble)
  Inbox_InsertTail(UsuarioDestino^.BandejaEntrada, NuevoCorreo);

  // Actualizar matriz de relaciones (remitente destinatario)
  ActualizarMatrizRelaciones(FUsuarioActual^.Email, Destinatario);
end;


// Implementación mejorada de BuscarContacto:
function TEDDMailSystem.BuscarContacto(Usuario: PUsuario; Email: String): PContacto;
var
  Actual: PContacto;
  PrimerContacto: PContacto;
  Contador: Integer;
begin
  Result := nil;
  if Usuario = nil then
    Exit;

  PrimerContacto := Usuario^.ListaContactos;
  if PrimerContacto = nil then
    Exit;

  Actual := PrimerContacto;
  Contador := 0;
  repeat
    if Actual^.Email = Email then
    begin
      Result := Actual;
      Exit;
    end;
    Actual := Actual^.Siguiente;
    Inc(Contador);
  until (Actual = PrimerContacto) or (Contador > 1000); // previene bucle infinito
end;
function TEDDMailSystem.EliminarContacto(Usuario: PUsuario; Email: String): Boolean;
var
  Actual, Anterior: PContacto;
  PrimerContacto: PContacto;
  Contador: Integer;
begin
  Result := False;
  if Usuario = nil then
    Exit;

  PrimerContacto := Usuario^.ListaContactos;
  if PrimerContacto = nil then
    Exit;

  // Si solo hay un contacto
  if PrimerContacto^.Siguiente = PrimerContacto then
  begin
    if PrimerContacto^.Email = Email then
    begin
      Dispose(PrimerContacto);
      Usuario^.ListaContactos := nil;
      Result := True;
    end;
    Exit;
  end;

  // Buscar el contacto a eliminar
  Actual := PrimerContacto;
  Anterior := nil;
  Contador := 0;

  // Encontrar el anterior al primero
  repeat
    if Actual^.Siguiente = PrimerContacto then
    begin
      Anterior := Actual;
      Break;
    end;
    Actual := Actual^.Siguiente;
    Inc(Contador);
  until Contador > 1000;

  // Buscar el contacto específico
  Actual := PrimerContacto;
  Contador := 0;
  repeat
    if Actual^.Email = Email then
    begin
      // Encontrado, eliminar
      if Actual = PrimerContacto then
      begin

        Usuario^.ListaContactos := Actual^.Siguiente;
        Anterior^.Siguiente := Usuario^.ListaContactos;
      end
      else
      begin

        Anterior^.Siguiente := Actual^.Siguiente;
      end;

      Dispose(Actual);
      Result := True;
      Exit;
    end;

    Anterior := Actual;
    Actual := Actual^.Siguiente;
    Inc(Contador);
  until (Actual = PrimerContacto) or (Contador > 1000);
end;

procedure TEDDMailSystem.ActualizarMatrizRelaciones(Remitente, Destinatario: String);
var
  FilaHdr: PMatrizDispersaFila;
  ColHdr: PMatrizDispersaColumna;
  N, P: PMatrizDispersaNodo;
begin
  if (Remitente = '') or (Destinatario = '') then Exit;

  FilaHdr := BuscarFilaMatriz(Remitente);
  ColHdr  := BuscarColumnaMatriz(Destinatario);
  if (FilaHdr = nil) or (ColHdr = nil) then Exit;

  // 1) Buscar si ya existe nodo (misma fila/col) recorriendo por la lista de fila
  P := FilaHdr^.Primero;
  while P <> nil do
  begin
    if (P^.Columna = ColHdr^.Columna) then
    begin
      Inc(P^.Cantidad); // sumar 1 si ya existe
      Exit;
    end;
    P := P^.Derecha;
  end;

  // 2) Crear nuevo nodo
  New(N);
  N^.Fila := FilaHdr^.Fila;
  N^.Columna := ColHdr^.Columna;
  N^.Cantidad := 1;
  N^.RemitenteEmail := Remitente;
  N^.DestinatarioEmail := Destinatario;
  N^.Arriba := nil; N^.Abajo := nil; N^.Izquierda := nil; N^.Derecha := nil;

  // 3) Insertar en lista de la fila (ordenado por columna)
  if FilaHdr^.Primero = nil then
    FilaHdr^.Primero := N
  else
  begin
    P := FilaHdr^.Primero;
    // insertar ordenado por columna
    if N^.Columna < P^.Columna then
    begin
      N^.Derecha := P; P^.Izquierda := N;
      FilaHdr^.Primero := N;
    end
    else
    begin
      while (P^.Derecha <> nil) and (P^.Derecha^.Columna < N^.Columna) do
        P := P^.Derecha;
      N^.Derecha := P^.Derecha;
      if P^.Derecha <> nil then P^.Derecha^.Izquierda := N;
      P^.Derecha := N;
      N^.Izquierda := P;
    end;
  end;

  // 4) Insertar en lista de la columna (ordenado por fila)
  if ColHdr^.Primero = nil then
    ColHdr^.Primero := N
  else
  begin
    P := ColHdr^.Primero;
    if N^.Fila < P^.Fila then
    begin
      N^.Abajo := P; P^.Arriba := N;
      ColHdr^.Primero := N;
    end
    else
    begin
      while (P^.Abajo <> nil) and (P^.Abajo^.Fila < N^.Fila) do
        P := P^.Abajo;
      N^.Abajo := P^.Abajo;
      if P^.Abajo <> nil then P^.Abajo^.Arriba := N;
      P^.Abajo := N;
      N^.Arriba := P;
    end;
  end;
end;


function TEDDMailSystem.BuscarFilaMatriz(Email: String): PMatrizDispersaFila;
var
  F, Ult: PMatrizDispersaFila;
  nextIdx: Integer;
begin
  // buscar existente
  F := FMatrizFilas; Ult := nil;
  while F <> nil do
  begin
    if F^.Email = Email then
    begin
      Result := F; Exit;
    end;
    Ult := F; F := F^.Siguiente;
  end;

  // crear nueva fila
  New(Result);
  Result^.Email := Email;
  Result^.Primero := nil;
  Result^.Siguiente := nil;

  // asignar índice
  if Ult = nil then
  begin
    Result^.Fila := 1;
    FMatrizFilas := Result;
  end
  else
  begin
    nextIdx := Ult^.Fila + 1;
    Result^.Fila := nextIdx;
    Ult^.Siguiente := Result;
  end;
end;

function TEDDMailSystem.BuscarColumnaMatriz(Email: String): PMatrizDispersaColumna;
var
  C, Ult: PMatrizDispersaColumna;
  nextIdx: Integer;
begin
  // buscar existente
  C := FMatrizColumnas; Ult := nil;
  while C <> nil do
  begin
    if C^.Email = Email then
    begin
      Result := C; Exit;
    end;
    Ult := C; C := C^.Siguiente;
  end;

  // crear nueva columna
  New(Result);
  Result^.Email := Email;
  Result^.Primero := nil;
  Result^.Siguiente := nil;

  // asignar índice
  if Ult = nil then
  begin
    Result^.Columna := 1;
    FMatrizColumnas := Result;
  end
  else
  begin
    nextIdx := Ult^.Columna + 1;
    Result^.Columna := nextIdx;
    Ult^.Siguiente := Result;
  end;
end;

// Implementar resto de métodos...
procedure TEDDMailSystem.ProgramarCorreo(Destinatario, Asunto, Mensaje, FechaEnvio: String);
begin
  // Implementar cola para correos programados
  WriteLn('Programando correo para: ', Destinatario, ' en fecha: ', FechaEnvio);
end;

procedure TEDDMailSystem.EliminarCorreo(Usuario: PUsuario; CorreoId: Integer);
begin
  // Implementar pila para correos eliminados
  WriteLn('Eliminando correo ID: ', CorreoId);
end;

procedure TEDDMailSystem.MarcarCorreoLeido(Usuario: PUsuario; CorreoId: Integer);
begin
  // Cambiar estado de correo a 'L'
  WriteLn('Marcando correo como leído ID: ', CorreoId);
end;

function TEDDMailSystem.GetBandejaEntrada(Usuario: PUsuario): PCorreo;
begin
  if Usuario = nil then Exit(nil);
  Result := Usuario^.BandejaEntrada;
end;

function TEDDMailSystem.GetPapelera(Usuario: PUsuario): PCorreo;
begin
  if Usuario = nil then Exit(nil);
  Result := Usuario^.Papelera;
end;

function TEDDMailSystem.GetCorreosProgramados(Usuario: PUsuario): PCorreo;
begin
  if Usuario = nil then Exit(nil);
  Result := Usuario^.CorreosProgramados;
end;

procedure TEDDMailSystem.ProcesarCorreosProgramados;
begin
  // Procesar cola FIFO de correos programados
  WriteLn('Procesando correos programados...');
end;




function TEDDMailSystem.CrearComunidad(Nombre: String): Boolean;
var
  NuevaComunidad, Ultima: PComunidad;
  IdCounter: Integer;
begin
  Result := False;

  // Verificar que no exista
  Ultima := FComunidades;
  while Ultima <> nil do
  begin
    if Ultima^.Nombre = Nombre then
      Exit; // Ya existe
    if Ultima^.Siguiente = nil then
      Break;
    Ultima := Ultima^.Siguiente;
  end;

  // Crear nueva comunidad
  New(NuevaComunidad);
  IdCounter := 1;
  if FComunidades <> nil then
  begin
    Ultima := FComunidades;
    while Ultima^.Siguiente <> nil do
    begin
      Inc(IdCounter);
      Ultima := Ultima^.Siguiente;
    end;
  end;

  NuevaComunidad^.Id := IdCounter;
  NuevaComunidad^.Nombre := Nombre;
  NuevaComunidad^.UsuariosList := nil;
  NuevaComunidad^.Siguiente := nil;

  if FComunidades = nil then
    FComunidades := NuevaComunidad
  else
    Ultima^.Siguiente := NuevaComunidad;

  Result := True;
end;

function TEDDMailSystem.AgregarUsuarioAComunidad(NombreComunidad, EmailUsuario: String): Boolean;
var
  Comunidad: PComunidad;
  Usuario: PUsuario;
  NuevoUsuarioCom, UltimoUsuarioCom: PUsuarioComunidad;
begin
  Result := False;

  // Buscar comunidad
  Comunidad := FComunidades;
  while (Comunidad <> nil) and (Comunidad^.Nombre <> NombreComunidad) do
    Comunidad := Comunidad^.Siguiente;

  if Comunidad = nil then
    Exit;

  // Verificar que el usuario existe
  Usuario := BuscarUsuario(EmailUsuario);
  if Usuario = nil then
    Exit;

  // Verificar que no esté ya en la comunidad
  UltimoUsuarioCom := Comunidad^.UsuariosList;
  while UltimoUsuarioCom <> nil do
  begin
    if UltimoUsuarioCom^.Email = EmailUsuario then
      Exit; // Ya está en la comunidad
    if UltimoUsuarioCom^.Siguiente = nil then
      Break;
    UltimoUsuarioCom := UltimoUsuarioCom^.Siguiente;
  end;

  // Agregar usuario a la comunidad
  New(NuevoUsuarioCom);
  NuevoUsuarioCom^.Email := EmailUsuario;
  NuevoUsuarioCom^.Siguiente := nil;

  if Comunidad^.UsuariosList = nil then
    Comunidad^.UsuariosList := NuevoUsuarioCom
  else
    UltimoUsuarioCom^.Siguiente := NuevoUsuarioCom;

  Result := True;
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
    WriteLn('Perfil actualizado para: ', Usuario^.Email);
  end;
end;

// Funciones de reportes usando Graphviz
procedure TEDDMailSystem.GenerarReporteUsuarios(RutaCarpeta: String);
var
  Archivo: TextFile;
  Usuario: PUsuario;
  Process: TProcess;
begin
  try
    ForceDirectories(RutaCarpeta);

    AssignFile(Archivo, RutaCarpeta + '/usuarios.dot');
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    rankdir=LR;');
    WriteLn(Archivo, '    node [shape=record, style=filled, fillcolor=lightblue];');
    WriteLn(Archivo, '    label="Lista Simple de Usuarios";');
    WriteLn(Archivo, '    fontsize=16;');

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

    // Generar imagen usando Graphviz
    try
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'dot';
        Process.Parameters.Add('-Tpng');
        Process.Parameters.Add(RutaCarpeta + '/usuarios.dot');
        Process.Parameters.Add('-o');
        Process.Parameters.Add(RutaCarpeta + '/usuarios.png');
        Process.Options := Process.Options + [poWaitOnExit, poUsePipes];
        Process.Execute;
        WriteLn('Reporte de usuarios generado: ', RutaCarpeta, '/usuarios.png');
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen (¿Graphviz instalado?): ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte de usuarios: ', E.Message);
  end;
end;

procedure TEDDMailSystem.GenerarReporteRelaciones(RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
  F: PMatrizDispersaFila;
  N: PMatrizDispersaNodo;
    C: PMatrizDispersaColumna;  // ← declarar aquí

begin
  try
    ForceDirectories(RutaCarpeta);
    AssignFile(Archivo, RutaCarpeta + '/relaciones.dot');
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '  rankdir=LR;');
    WriteLn(Archivo, '  node [shape=box, style=filled, fillcolor=lightyellow];');
    WriteLn(Archivo, '  label="Relaciones Remitente → Destinatario (Matriz Dispersa)"; fontsize=16;');

    // declarar nodos por email para estética
    WriteLn(Archivo, '  subgraph cluster_remitentes { label="Remitentes"; color=lightgray;');
    F := FMatrizFilas;
    while F <> nil do
    begin
      WriteLn(Archivo, Format('  r_%d [label="%s", fillcolor=lightblue];', [F^.Fila, F^.Email]));
      F := F^.Siguiente;
    end;
    WriteLn(Archivo, '  }');

    WriteLn(Archivo, '  subgraph cluster_destinatarios { label="Destinatarios"; color=lightgray;');
    C := FMatrizColumnas;
    while C <> nil do
    begin
      WriteLn(Archivo, Format('  d_%d [label="%s", fillcolor=lightgreen];', [C^.Columna, C^.Email]));
      C := C^.Siguiente;
    end;
    WriteLn(Archivo, '  }');

    // aristas con cantidad
    F := FMatrizFilas;
    while F <> nil do
    begin
      N := F^.Primero;
      while N <> nil do
      begin
        WriteLn(Archivo, Format('  r_%d -> d_%d [label="%d"];', [N^.Fila, N^.Columna, N^.Cantidad]));
        N := N^.Derecha;
      end;
      F := F^.Siguiente;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // PNG con Graphviz
    try
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'dot';
        Process.Parameters.Add('-Tpng');
        Process.Parameters.Add(RutaCarpeta + '/relaciones.dot');
        Process.Parameters.Add('-o');
        Process.Parameters.Add(RutaCarpeta + '/relaciones.png');
        Process.Options := Process.Options + [poWaitOnExit];
        Process.Execute;
        WriteLn('Reporte de relaciones generado: ', RutaCarpeta, '/relaciones.png');
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte de relaciones: ', E.Message);
  end;
end;


procedure TEDDMailSystem.GenerarReporteCorreosRecibidos(Usuario: PUsuario; RutaCarpeta: String);
begin
  // Implementar reporte de lista doblemente enlazada
  WriteLn('Generando reporte de correos recibidos para: ', Usuario^.Email);
end;


procedure TEDDMailSystem.GenerarReportePapelera(Usuario: PUsuario; RutaCarpeta: String);
begin
  // Implementar reporte de pila
  WriteLn('Generando reporte de papelera para: ', Usuario^.Email);
end;

procedure TEDDMailSystem.GenerarReporteCorreosProgramados(Usuario: PUsuario; RutaCarpeta: String);
begin
  // Implementar reporte de cola
  WriteLn('Generando reporte de correos programados para: ', Usuario^.Email);
end;
procedure TEDDMailSystem.GenerarNodosBST(var Archivo: TextFile; nodo: PNodoBST);
var
  NombreLimpio: String;
begin
  if nodo = nil then Exit;

  // Limpiar el nombre para usarlo como ID en Graphviz
  NombreLimpio := StringReplace(nodo^.NombreComunidad, ' ', '_', [rfReplaceAll]);
  NombreLimpio := StringReplace(NombreLimpio, '-', '_', [rfReplaceAll]);
  NombreLimpio := StringReplace(NombreLimpio, '.', '_', [rfReplaceAll]);

  // FORMATO VERTICAL: usando \n en lugar de |
  WriteLn(Archivo, Format('    %s [label="%s\nFecha creacion: %s\nMensajes publicados: %d"];',
    [NombreLimpio, nodo^.NombreComunidad, nodo^.FechaCreacion, nodo^.NumeroMensajes]));

  // Generar nodos hijos
  if nodo^.Izquierdo <> nil then
    GenerarNodosBST(Archivo, nodo^.Izquierdo);

  if nodo^.Derecho <> nil then
    GenerarNodosBST(Archivo, nodo^.Derecho);

  // Generar conexiones
  if nodo^.Izquierdo <> nil then
  begin
    WriteLn(Archivo, Format('    %s -> %s;',
      [NombreLimpio, StringReplace(StringReplace(StringReplace(
        nodo^.Izquierdo^.NombreComunidad, ' ', '_', [rfReplaceAll]),
        '-', '_', [rfReplaceAll]), '.', '_', [rfReplaceAll])]));
  end;

  if nodo^.Derecho <> nil then
  begin
    WriteLn(Archivo, Format('    %s -> %s;',
      [NombreLimpio, StringReplace(StringReplace(StringReplace(
        nodo^.Derecho^.NombreComunidad, ' ', '_', [rfReplaceAll]),
        '-', '_', [rfReplaceAll]), '.', '_', [rfReplaceAll])]));
  end;
end;

// =============== GenerarNodosB ===============

procedure TEDDMailSystem.GenerarNodosB(var Archivo: TextFile; nodo: PNodoB; nivel: Integer);
var
  i: Integer;
  NodoId: String;
  Etiqueta: String;
  HijosIds: array[0..4] of String; // se almacenan IDs de hijos
begin
  if nodo = nil then Exit;

  // Genera ID único para este nodo
  NodoId := Format('nodoB_%d_%d', [nivel, Random(1000)]);

  // Mostrar estructura del nodo B con las claves
  Etiqueta := Format('Nodo B (Nivel %d)<BR/>Claves: ', [nivel]);
  for i := 0 to nodo^.NumClaves - 1 do
  begin
    if i > 0 then
      Etiqueta := Etiqueta + ', ';
    Etiqueta := Etiqueta + IntToStr(nodo^.Claves[i]);
  end;

  Etiqueta := Etiqueta + Format('<BR/>Hoja: %s<BR/>NumClaves: %d',
    [BoolToStr(nodo^.EsHoja, True), nodo^.NumClaves]);

  WriteLn(Archivo, Format('    %s [label=<%s>, shape=box, style=filled, fillcolor=lightgreen];',
    [NodoId, Etiqueta]));

  // Generar nodos de datos separados
  for i := 0 to nodo^.NumClaves - 1 do
  begin
    if nodo^.Correos[i] <> nil then
    begin
      WriteLn(Archivo, Format('    dato_%d [label=<ID: %d<BR/>Remitente: %s<BR/>Destinatario: %s<BR/>Asunto: %s<BR/>Mensaje: %s>, shape=box, style=filled, fillcolor=lightyellow];',
        [nodo^.Claves[i],
         nodo^.Claves[i],
         nodo^.Correos[i]^.Remitente,
         nodo^.Correos[i]^.Destinatario,
         nodo^.Correos[i]^.Asunto,
         nodo^.Correos[i]^.Mensaje]));

      // Conectar nodo B con datos
      WriteLn(Archivo, Format('    %s -> dato_%d [style=dashed, color=blue];',
        [NodoId, nodo^.Claves[i]]));
    end;
  end;

  // Procesar hijos PRIMERO para obtener sus IDs
  if not nodo^.EsHoja then
  begin
    // Inicializamos array de IDs de hijos
    for i := 0 to 4 do
      HijosIds[i] := '';

    // PRIMERO: Generar todos los nodos hijos y almacenar sus IDs
    for i := 0 to nodo^.NumClaves do
    begin
      if nodo^.Hijos[i] <> nil then
      begin
        // Generar ID del hijo ANTES de la recursión
        HijosIds[i] := Format('nodoB_%d_%d', [nivel + 1, Random(1000)]);

        // Generar el nodo hijo con ID predefinido
        GenerarNodoHijoConId(Archivo, nodo^.Hijos[i], nivel + 1, HijosIds[i]);
      end;
    end;

    // SEGUNDO: Generar las conexiones usando los IDs almacenados
    for i := 0 to nodo^.NumClaves do
    begin
      if (nodo^.Hijos[i] <> nil) and (HijosIds[i] <> '') then
      begin
        WriteLn(Archivo, Format('    %s -> %s [color=red];',
          [NodoId, HijosIds[i]]));
      end;
    end;
  end;
end;

// =============== FUNCIÓN AUXILIAR ADICIONAL ===============

procedure TEDDMailSystem.GenerarNodoHijoConId(var Archivo: TextFile; nodo: PNodoB; nivel: Integer; const NodoId: String);
var
  i: Integer;
  Etiqueta: String;
begin
  if nodo = nil then Exit;

  // Usar el ID proporcionado en lugar de generar uno nuevo
  Etiqueta := Format('Nodo B (Nivel %d)<BR/>Claves: ', [nivel]);
  for i := 0 to nodo^.NumClaves - 1 do
  begin
    if i > 0 then
      Etiqueta := Etiqueta + ', ';
    Etiqueta := Etiqueta + IntToStr(nodo^.Claves[i]);
  end;

  Etiqueta := Etiqueta + Format('<BR/>Hoja: %s<BR/>NumClaves: %d',
    [BoolToStr(nodo^.EsHoja, True), nodo^.NumClaves]);

  WriteLn(Archivo, Format('    %s [label=<%s>, shape=box, style=filled, fillcolor=lightgreen];',
    [NodoId, Etiqueta]));

  // Generar nodos de datos
  for i := 0 to nodo^.NumClaves - 1 do
  begin
    if nodo^.Correos[i] <> nil then
    begin
      WriteLn(Archivo, Format('    dato_%d [label=<ID: %d<BR/>Remitente: %s<BR/>Destinatario: %s<BR/>Asunto: %s<BR/>Mensaje: %s>, shape=box, style=filled, fillcolor=lightyellow];',
        [nodo^.Claves[i],
         nodo^.Claves[i],
         nodo^.Correos[i]^.Remitente,
         nodo^.Correos[i]^.Destinatario,
         nodo^.Correos[i]^.Asunto,
         nodo^.Correos[i]^.Mensaje]));

      WriteLn(Archivo, Format('    %s -> dato_%d [style=dashed, color=blue];',
        [NodoId, nodo^.Claves[i]]));
    end;
  end;

  // Si el nodo hijo tiene sus propios hijos, procesar recursivamente
  if not nodo^.EsHoja then
  begin
    for i := 0 to nodo^.NumClaves do
    begin
      if nodo^.Hijos[i] <> nil then
      begin
        GenerarNodosB(Archivo, nodo^.Hijos[i], nivel + 1);
      end;
    end;
  end;
end;
// 2. Implementación de GenerarReporteComunidadesBST
procedure TEDDMailSystem.GenerarReporteComunidadesBST(RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
begin
  try
    ForceDirectories(RutaCarpeta);
    NombreArchivo := RutaCarpeta + '/comunidades_bst.dot';
    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Reporte de comunidades (Árbol BST)";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    node [shape=record, style=filled, fillcolor=lightblue];');
    WriteLn(Archivo, '    rankdir=TB;');  // dirección top-bottom

    if FArbolComunidades = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin comunidades", fillcolor=lightgray];');
    end
    else
    begin
      GenerarNodosBST(Archivo, FArbolComunidades);
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

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
        WriteLn('Reporte BST generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte BST: ', E.Message);
  end;
end;
// 3. Implementación de GenerarReporteFavoritos
procedure TEDDMailSystem.GenerarReporteFavoritos(Usuario: PUsuario; RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
begin
  if Usuario = nil then Exit;

  try
    ForceDirectories(RutaCarpeta);
    NombreArchivo := RutaCarpeta + '/favoritos_' +
                   StringReplace(Usuario^.Usuario, ' ', '_', [rfReplaceAll]) + '.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Árbol B - Favoritos - ' + Usuario^.Nombre + '";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    node [shape=box, style=filled, fillcolor=lightyellow];');


    if Usuario^.ArbolFavoritos = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin favoritos", fillcolor=lightgray];');
    end
    else
    begin
      GenerarNodosB(Archivo, Usuario^.ArbolFavoritos, 0);
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen PNG
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
        WriteLn('Reporte favoritos generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte favoritos: ', E.Message);
  end;
end;

// 4. Implementación de ObtenerMensajesComunidad
function TEDDMailSystem.ObtenerMensajesComunidad(nombreComunidad: String): String;
var
  Comunidad: PNodoBST;
  Mensaje: PMensajeComunidad;
begin
  Result := '';
  Comunidad := BuscarComunidadBST(FArbolComunidades, nombreComunidad);

  if Comunidad = nil then
  begin
    Result := 'Comunidad no encontrada: ' + nombreComunidad;
    Exit;
  end;

  Result := 'Comunidad: ' + Comunidad^.NombreComunidad + LineEnding;
  Result := Result + 'Fecha de creación: ' + Comunidad^.FechaCreacion + LineEnding;
  Result := Result + 'Total de mensajes: ' + IntToStr(Comunidad^.NumeroMensajes) + LineEnding;
  Result := Result + '----------------------------------------' + LineEnding + LineEnding;

  Mensaje := Comunidad^.ListaMensajes;
  if Mensaje = nil then
  begin
    Result := Result + 'No hay mensajes publicados en esta comunidad.';
    Exit;
  end;

  while Mensaje <> nil do
  begin
    Result := Result + 'De: ' + Mensaje^.Correo + LineEnding;
    Result := Result + 'Fecha: ' + Mensaje^.FechaPublicacion + LineEnding;
    Result := Result + 'Mensaje: ' + Mensaje^.Mensaje + LineEnding;
    Result := Result + '----------------------------------------' + LineEnding;
    Mensaje := Mensaje^.Siguiente;
  end;
end;

 function TEDDMailSystem.CrearContacto(Email: String): PContacto;
var
  UsuarioExistente: PUsuario;
begin
  Result := nil;

  // Verificar que el usuario existe en el sistema
  UsuarioExistente := BuscarUsuario(Email);
  if UsuarioExistente = nil then
    Exit;

  // Crear nuevo contacto
  New(Result);
  Result^.Id := Random(9999) + 1;
  Result^.Nombre := UsuarioExistente^.Nombre;
  Result^.Usuario := UsuarioExistente^.Usuario;
  Result^.Email := UsuarioExistente^.Email;
  Result^.Telefono := UsuarioExistente^.Telefono;
  Result^.Siguiente := nil;
end;
procedure TEDDMailSystem.AgregarContactoALista(var PrimerContacto: PContacto; NuevoContacto: PContacto);
var
  Actual: PContacto;
begin
  if PrimerContacto = nil then
  begin
    // Primera inserción - crear lista circular
    PrimerContacto := NuevoContacto;
    NuevoContacto^.Siguiente := NuevoContacto; // Apunta a sí mismo
  end
  else
  begin
    // Buscar el último nodo (que apunta al primero)
    Actual := PrimerContacto;
    while Actual^.Siguiente <> PrimerContacto do
      Actual := Actual^.Siguiente;

    // Insertar el nuevo contacto
    Actual^.Siguiente := NuevoContacto;
    NuevoContacto^.Siguiente := PrimerContacto;
  end;
end;
function TEDDMailSystem.ContarContactos(PrimerContacto: PContacto): Integer;
var
  Actual: PContacto;
begin
  Result := 0;
  if PrimerContacto = nil then
    Exit;

  Actual := PrimerContacto;
  repeat
    Inc(Result);
    Actual := Actual^.Siguiente;
  until Actual = PrimerContacto;
end;

procedure TEDDMailSystem.GenerarReporteContactos(Usuario: PUsuario; RutaCarpeta: String);
var
  Archivo: TextFile;
  Contacto, PrimerContacto: PContacto;
  Process: TProcess;
  NombreArchivo: String;
  EmailLimpio: String;
  SigEmailLimpio: String;
  UltimoLimpio: String;
  PrimeroLimpio: String;
  UltimoEmail: String;
  Contador: Integer;
begin
  if Usuario = nil then
    Exit;

  try
    ForceDirectories(RutaCarpeta);

    NombreArchivo := RutaCarpeta + '/contactos_' +
                   StringReplace(Usuario^.Usuario, ' ', '_', [rfReplaceAll]) + '.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Lista Circular de Contactos - ' + Usuario^.Nombre + '";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    rankdir=LR;');
    WriteLn(Archivo, '    node [shape=record, style=filled, fillcolor=lightblue];');

    PrimerContacto := GetContactos(Usuario);

    if PrimerContacto = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin contactos", style=filled, fillcolor=lightgray];');
    end
    else
    begin
      Contacto := PrimerContacto;
      Contador := 0;

      repeat
        EmailLimpio := StringReplace(Contacto^.Email, '@', '_at_', [rfReplaceAll]);
        EmailLimpio := StringReplace(EmailLimpio, '.', '_', [rfReplaceAll]);
        EmailLimpio := StringReplace(EmailLimpio, '-', '_', [rfReplaceAll]);

        WriteLn(Archivo, Format('    contact_%s [label="ID: %d|Nombre: %s|Usuario: %s|Email: %s|Tel: %s"];',
          [EmailLimpio, Contacto^.Id, Contacto^.Nombre, Contacto^.Usuario,
           Contacto^.Email, Contacto^.Telefono]));

        Contacto := Contacto^.Siguiente;
        Inc(Contador);
      until (Contacto = PrimerContacto) or (Contador > 100); // para evitar bucle infinito

      // Generar las conexiones circulares
      Contacto := PrimerContacto;
      Contador := 0;
      repeat
        EmailLimpio := StringReplace(Contacto^.Email, '@', '_at_', [rfReplaceAll]);
        EmailLimpio := StringReplace(EmailLimpio, '.', '_', [rfReplaceAll]);
        EmailLimpio := StringReplace(EmailLimpio, '-', '_', [rfReplaceAll]);

        if Contacto^.Siguiente <> nil then
        begin

           SigEmailLimpio := StringReplace(Contacto^.Siguiente^.Email, '@', '_at_', [rfReplaceAll]);
          SigEmailLimpio := StringReplace(SigEmailLimpio, '.', '_', [rfReplaceAll]);
          SigEmailLimpio := StringReplace(SigEmailLimpio, '-', '_', [rfReplaceAll]);

          WriteLn(Archivo, Format('    contact_%s -> contact_%s;', [EmailLimpio, SigEmailLimpio]));
        end;

        Contacto := Contacto^.Siguiente;
        Inc(Contador);
      until (Contacto = PrimerContacto) or (Contador > 100);

      // Indicar la naturaleza circular
      WriteLn(Archivo, '    edge [color=red, style=dashed];');
      if PrimerContacto^.Siguiente <> PrimerContacto then
      begin
        UltimoEmail := PrimerContacto^.Email;
        while Contacto^.Siguiente <> PrimerContacto do
          Contacto := Contacto^.Siguiente;

        UltimoLimpio := StringReplace(Contacto^.Email, '@', '_at_', [rfReplaceAll]);
        UltimoLimpio := StringReplace(UltimoLimpio, '.', '_', [rfReplaceAll]);
        UltimoLimpio := StringReplace(UltimoLimpio, '-', '_', [rfReplaceAll]);

         PrimeroLimpio := StringReplace(PrimerContacto^.Email, '@', '_at_', [rfReplaceAll]);
        PrimeroLimpio := StringReplace(PrimeroLimpio, '.', '_', [rfReplaceAll]);
        PrimeroLimpio := StringReplace(PrimeroLimpio, '-', '_', [rfReplaceAll]);

        WriteLn(Archivo, Format('    contact_%s -> contact_%s [label="circular"];',
                [UltimoLimpio, PrimeroLimpio]));
      end;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar imagen usando Graphviz
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
        WriteLn('Reporte de contactos generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte de contactos: ', E.Message);
  end;
end;
procedure TEDDMailSystem.GenerarReporteComunidades(RutaCarpeta: String);
var
  Archivo: TextFile;
  Comunidad: PComunidad;
  UsuarioCom, UltimoUsuario: PUsuarioComunidad;
  Process: TProcess;
  EmailLimpio: String;
begin
  try
    ForceDirectories(RutaCarpeta);
    AssignFile(Archivo, RutaCarpeta + '/comunidades.dot');
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Lista de Listas - Comunidades";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    node [shape=box];');


    if FComunidades = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin comunidades", style=filled, fillcolor=lightgray];');
    end
    else
    begin
      // 1. Crear nodos de comunidades
      Comunidad := FComunidades;
      while Comunidad <> nil do
      begin
        WriteLn(Archivo, Format('    com%d [label="Comunidad\n%s", style=filled, fillcolor=lightblue];',
          [Comunidad^.Id, Comunidad^.Nombre]));
        Comunidad := Comunidad^.Siguiente;
      end;


      WriteLn(Archivo, '    { rank=same; '); // Fuerza comunidades al mismo nivel
      Comunidad := FComunidades;
      while Comunidad <> nil do
      begin
        Write(Archivo, Format('com%d; ', [Comunidad^.Id]));
        Comunidad := Comunidad^.Siguiente;
      end;
      WriteLn(Archivo, ' }');


      Comunidad := FComunidades;
      while (Comunidad <> nil) and (Comunidad^.Siguiente <> nil) do
      begin
        WriteLn(Archivo, Format('    com%d -> com%d [color=black, constraint=false];',
          [Comunidad^.Id, Comunidad^.Siguiente^.Id]));
        Comunidad := Comunidad^.Siguiente;
      end;

      // 3. Crear usuarios y conectarlos verticalmente ABAJO de cada comunidad
      Comunidad := FComunidades;
      while Comunidad <> nil do
      begin
        UsuarioCom := Comunidad^.UsuariosList;
        UltimoUsuario := nil;

        while UsuarioCom <> nil do
        begin
          EmailLimpio := StringReplace(UsuarioCom^.Email, '@', '_', [rfReplaceAll]);
          EmailLimpio := StringReplace(EmailLimpio, '-', '_', [rfReplaceAll]);
          EmailLimpio := StringReplace(EmailLimpio, '.', '_', [rfReplaceAll]);

          WriteLn(Archivo, Format('    user_%d_%s [label="%s", style=filled, fillcolor=lightyellow];',
            [Comunidad^.Id, EmailLimpio, UsuarioCom^.Email]));

          // Conectar comunidad al PRIMER usuario
          if UltimoUsuario = nil then
          begin
            WriteLn(Archivo, Format('    com%d -> user_%d_%s [color=blue];',
              [Comunidad^.Id, Comunidad^.Id, EmailLimpio]));
          end
          else
          begin

            EmailLimpio := StringReplace(UltimoUsuario^.Email, '@', '_', [rfReplaceAll]);
            EmailLimpio := StringReplace(EmailLimpio, '-', '_', [rfReplaceAll]);
            EmailLimpio := StringReplace(EmailLimpio, '.', '_', [rfReplaceAll]);

            WriteLn(Archivo, Format('    user_%d_%s -> user_%d_%s [color=blue];',
              [Comunidad^.Id, EmailLimpio, Comunidad^.Id,
               StringReplace(StringReplace(StringReplace(UsuarioCom^.Email, '@', '_', [rfReplaceAll]), '-', '_', [rfReplaceAll]), '.', '_', [rfReplaceAll])]));
          end;

          UltimoUsuario := UsuarioCom;
          UsuarioCom := UsuarioCom^.Siguiente;
        end;

        Comunidad := Comunidad^.Siguiente;
      end;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar PNG...
    try
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'dot';
        Process.Parameters.Add('-Tpng');
        Process.Parameters.Add(RutaCarpeta + '/comunidades.dot');
        Process.Parameters.Add('-o');
        Process.Parameters.Add(RutaCarpeta + '/comunidades.png');
        Process.Options := Process.Options + [poWaitOnExit];
        Process.Execute;
        WriteLn('Reporte de comunidades generado: ', RutaCarpeta, '/comunidades.png');
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte de comunidades: ', E.Message);
  end;
end;

//Fase 2


// =============== FUNCIONES PARA ÁRBOL AVL (BORRADORES) ===============
function TEDDMailSystem.ObtenerAltura(nodo: PNodoAVL): Integer;
begin
  if nodo = nil then
    Result := 0
  else
    Result := nodo^.Altura;
end;

function TEDDMailSystem.ObtenerBalance(nodo: PNodoAVL): Integer;
begin
  if nodo = nil then
    Result := 0
  else
    Result := ObtenerAltura(nodo^.Izquierdo) - ObtenerAltura(nodo^.Derecho);
end;

function TEDDMailSystem.RotarDerecha(y: PNodoAVL): PNodoAVL;
var
  x: PNodoAVL;
begin
  x := y^.Izquierdo;
  y^.Izquierdo := x^.Derecho;
  x^.Derecho := y;

  // Actualizar alturas
  y^.Altura := Max(ObtenerAltura(y^.Izquierdo), ObtenerAltura(y^.Derecho)) + 1;
  x^.Altura := Max(ObtenerAltura(x^.Izquierdo), ObtenerAltura(x^.Derecho)) + 1;

  Result := x;
end;

function TEDDMailSystem.RotarIzquierda(x: PNodoAVL): PNodoAVL;
var
  y: PNodoAVL;
begin
  y := x^.Derecho;
  x^.Derecho := y^.Izquierdo;
  y^.Izquierdo := x;

  // Actualizar alturas
  x^.Altura := Max(ObtenerAltura(x^.Izquierdo), ObtenerAltura(x^.Derecho)) + 1;
  y^.Altura := Max(ObtenerAltura(y^.Izquierdo), ObtenerAltura(y^.Derecho)) + 1;

  Result := y;
end;

function TEDDMailSystem.InsertarAVL(nodo: PNodoAVL; correo: PCorreo): PNodoAVL;
var
  balance: Integer;
begin
  // 1. Inserción normal BST
  if nodo = nil then
  begin
    New(Result);
    Result^.Correo := correo;
    Result^.Altura := 1;
    Result^.Izquierdo := nil;
    Result^.Derecho := nil;
    Exit;
  end;

  if correo^.Id < nodo^.Correo^.Id then
    nodo^.Izquierdo := InsertarAVL(nodo^.Izquierdo, correo)
  else if correo^.Id > nodo^.Correo^.Id then
    nodo^.Derecho := InsertarAVL(nodo^.Derecho, correo)
  else
  begin
    Result := nodo;
    Exit;
  end;

  // 2. Actualizar altura
  nodo^.Altura := 1 + Max(ObtenerAltura(nodo^.Izquierdo), ObtenerAltura(nodo^.Derecho));

  // 3. Obtener balance
  balance := ObtenerBalance(nodo);

  // 4. Casos de rotación
  // Izquierda Izquierda
  if (balance > 1) and (correo^.Id < nodo^.Izquierdo^.Correo^.Id) then
  begin
    Result := RotarDerecha(nodo);
    Exit;
  end;

  // Derecha Derecha
  if (balance < -1) and (correo^.Id > nodo^.Derecho^.Correo^.Id) then
  begin
    Result := RotarIzquierda(nodo);
    Exit;
  end;

  // Izquierda Derecha
  if (balance > 1) and (correo^.Id > nodo^.Izquierdo^.Correo^.Id) then
  begin
    nodo^.Izquierdo := RotarIzquierda(nodo^.Izquierdo);
    Result := RotarDerecha(nodo);
    Exit;
  end;

  // Derecha Izquierda
  if (balance < -1) and (correo^.Id < nodo^.Derecho^.Correo^.Id) then
  begin
    nodo^.Derecho := RotarDerecha(nodo^.Derecho);
    Result := RotarIzquierda(nodo);
    Exit;
  end;

  Result := nodo;
end;

function TEDDMailSystem.GuardarBorrador(Usuario: PUsuario; Destinatario, Asunto, Mensaje: String): Boolean;
var
  NuevoCorreo: PCorreo;
begin
  Result := False;
  if Usuario = nil then Exit;

  NuevoCorreo := CrearCorreo(
    Usuario^.Email,
    Destinatario,
    Asunto,
    Mensaje,
    FormatDateTime('dd/mm/yy hh:nn', Now),
    False
  );


  NuevoCorreo^.Cuerpo := Mensaje;
  NuevoCorreo^.FechaHora := Now;

  Usuario^.ArbolBorradores := InsertarAVL(Usuario^.ArbolBorradores, NuevoCorreo);
  WriteLn('Borrador guardado para usuario: ', Usuario^.Email);
  Result := True;
end;

// =============== FUNCIONES PARA BST (COMUNIDADES) ===============
function TEDDMailSystem.InsertarBST(nodo: PNodoBST; nombreComunidad: String): PNodoBST;
begin
  if nodo = nil then
  begin
    New(Result);
    Result^.NombreComunidad := nombreComunidad;
    Result^.FechaCreacion := FormatDateTime('dd/mm/yyyy', Now);
    Result^.NumeroMensajes := 0;
    Result^.ListaMensajes := nil;
    Result^.Izquierdo := nil;
    Result^.Derecho := nil;
    Exit;
  end;

  if nombreComunidad < nodo^.NombreComunidad then
    nodo^.Izquierdo := InsertarBST(nodo^.Izquierdo, nombreComunidad)
  else if nombreComunidad > nodo^.NombreComunidad then
    nodo^.Derecho := InsertarBST(nodo^.Derecho, nombreComunidad)
  else
    Result := nodo;

  Result := nodo;
end;



function TEDDMailSystem.CrearComunidadBST(nombreComunidad: String): Boolean;
begin
  Result := False;
  if BuscarComunidadBST(FArbolComunidades, nombreComunidad) <> nil then
  begin
    WriteLn('Error: La comunidad ya existe');
    Exit;
  end;

  FArbolComunidades := InsertarBST(FArbolComunidades, nombreComunidad);
  WriteLn('Comunidad creada: ', nombreComunidad);
  Result := True;
end;

function TEDDMailSystem.PublicarMensajeAComunidad(nombreComunidad, correoUsuario, mensaje: String): Boolean;
var
  Comunidad: PNodoBST;
  NuevoMensaje: PMensajeComunidad;
begin
  Result := False;
  Comunidad := BuscarComunidadBST(FArbolComunidades, nombreComunidad);
  if Comunidad = nil then
  begin
    WriteLn('Error: La comunidad no existe');
    Exit;
  end;

  New(NuevoMensaje);
  NuevoMensaje^.Correo := correoUsuario;
  NuevoMensaje^.Mensaje := mensaje;
  NuevoMensaje^.FechaPublicacion := FormatDateTime('dd/mm/yyyy hh:nn', Now);
  NuevoMensaje^.Siguiente := Comunidad^.ListaMensajes;

  Comunidad^.ListaMensajes := NuevoMensaje;
  Inc(Comunidad^.NumeroMensajes);

  WriteLn('Mensaje publicado en comunidad: ', nombreComunidad);
  Result := True;
end;

// =============== FUNCIONES PARA ÁRBOL B (FAVORITOS) ===============
function TEDDMailSystem.CrearNodoB: PNodoB;
var
  i: Integer;
begin
  New(Result);
  Result^.NumClaves := 0;
  Result^.EsHoja := True;

  for i := 0 to 3 do
  begin
    Result^.Claves[i] := 0;
    Result^.Correos[i] := nil;
  end;

  for i := 0 to 4 do
    Result^.Hijos[i] := nil;
end;


function TEDDMailSystem.MarcarComoFavorito(Usuario: PUsuario; CorreoId: Integer): Boolean;
var
  Correo: PCorreo;
begin
  Result := False;
  if Usuario = nil then Exit;

  // Buscar el correo en la bandeja de entrada
  Correo := BuscarCorreoEnBandeja(Usuario, CorreoId);
  if Correo = nil then
  begin
    WriteLn('Error: Correo no encontrado');
    Exit;
  end;

  // Agregar al árbol B de favoritos
  Usuario^.ArbolFavoritos := InsertarB(Usuario^.ArbolFavoritos, Correo);
  WriteLn('Correo marcado como favorito: ID ', CorreoId);
  Result := True;
end;

// Función auxiliar para buscar correo en bandeja
function TEDDMailSystem.BuscarCorreoEnBandeja(Usuario: PUsuario; CorreoId: Integer): PCorreo;
var
  Correo: PCorreo;
begin
  Result := nil;
  if Usuario = nil then Exit;

  Correo := Usuario^.BandejaEntrada;
  while Correo <> nil do
  begin
    if Correo^.Id = CorreoId then
    begin
      Result := Correo;
      Exit;
    end;
    Correo := Correo^.Siguiente;
  end;
end;

// Función básica para obtener borradores
function TEDDMailSystem.ObtenerBorradores(Usuario: PUsuario; tipoRecorrido: String): TStringList;
begin
  Result := TStringList.Create;
  if Usuario = nil then Exit;

  // Implementar recorridos del árbol AVL
  case tipoRecorrido of
    'InOrden': RecorridoInOrdenAVL(Usuario^.ArbolBorradores, Result);
    'PreOrden': RecorridoPreOrdenAVL(Usuario^.ArbolBorradores, Result);
    'PostOrden': RecorridoPostOrdenAVL(Usuario^.ArbolBorradores, Result);
  end;
end;
// Implementación de los recorridos
procedure TEDDMailSystem.RecorridoInOrdenAVL(nodo: PNodoAVL; lista: TStringList);
var
  Display: String;
begin
  if nodo = nil then Exit;

  RecorridoInOrdenAVL(nodo^.Izquierdo, lista);

  Display := Format('[ID: %d] %s → %s | %s',
    [nodo^.Correo^.Id, nodo^.Correo^.Asunto, nodo^.Correo^.Destinatario, nodo^.Correo^.Fecha]);
  lista.AddObject(Display, TObject(PtrInt(nodo^.Correo^.Id)));

  RecorridoInOrdenAVL(nodo^.Derecho, lista);
end;

procedure TEDDMailSystem.RecorridoPreOrdenAVL(nodo: PNodoAVL; lista: TStringList);
var
  Display: String;
begin
  if nodo = nil then Exit;

  Display := Format('[ID: %d] %s → %s | %s',
    [nodo^.Correo^.Id, nodo^.Correo^.Asunto, nodo^.Correo^.Destinatario, nodo^.Correo^.Fecha]);
  lista.AddObject(Display, TObject(PtrInt(nodo^.Correo^.Id)));

  RecorridoPreOrdenAVL(nodo^.Izquierdo, lista);
  RecorridoPreOrdenAVL(nodo^.Derecho, lista);
end;

procedure TEDDMailSystem.RecorridoPostOrdenAVL(nodo: PNodoAVL; lista: TStringList);
var
  Display: String;
begin
  if nodo = nil then Exit;

  RecorridoPostOrdenAVL(nodo^.Izquierdo, lista);
  RecorridoPostOrdenAVL(nodo^.Derecho, lista);

  Display := Format('[ID: %d] %s → %s | %s',
    [nodo^.Correo^.Id, nodo^.Correo^.Asunto, nodo^.Correo^.Destinatario, nodo^.Correo^.Fecha]);
  lista.AddObject(Display, TObject(PtrInt(nodo^.Correo^.Id)));
end;


function TEDDMailSystem.InsertarB(raiz: PNodoB; correo: PCorreo): PNodoB;
var
  nuevaRaiz: PNodoB;
begin
  // Si el árbol está vacío
  if raiz = nil then
  begin
    Result := CrearNodoB;
    Result^.Claves[0] := correo^.Id;
    Result^.Correos[0] := correo;
    Result^.NumClaves := 1;
    Result^.EsHoja := True;
    Exit;
  end;

  // Verificar si ya existe la clave
  if BuscarB(raiz, correo^.Id) <> nil then
  begin
    Result := raiz; // Ya existe
    Exit;
  end;

  // Si la raíz está llena, crear nueva raíz
  if raiz^.NumClaves = 4 then
  begin
    nuevaRaiz := CrearNodoB;
    nuevaRaiz^.EsHoja := False;
    nuevaRaiz^.NumClaves := 0;
    nuevaRaiz^.Hijos[0] := raiz;

    // Dividir la raíz antigua
    DividirNodoB(nuevaRaiz, 0, nuevaRaiz);

    // Insertar en la nueva estructura
    InsertarEnNodoNoLleno(nuevaRaiz, correo);
    Result := nuevaRaiz;
  end
  else
  begin
    // La raíz no está llena, insertar normalmente
    InsertarEnNodoNoLleno(raiz, correo);
    Result := raiz;
  end;
end;

// ===============  FUNCIONES AUXILIARES ===============

// =============== FUNCIÓN DividirNodoB ===============

procedure TEDDMailSystem.DividirNodoB(nodo: PNodoB; indiceHijo: Integer; var nuevaRaiz: PNodoB);
var
  hijoLleno, nuevoHijo: PNodoB;
  claveMediana: Integer;
  correoMediana: PCorreo;
  i: Integer;
begin
  hijoLleno := nodo^.Hijos[indiceHijo];
  nuevoHijo := CrearNodoB;

  WriteLn('🔧 ANTES DE DIVISIÓN:');
  WriteLn('   Claves: [', hijoLleno^.Claves[0], ', ', hijoLleno^.Claves[1],
           ', ', hijoLleno^.Claves[2], ', ', hijoLleno^.Claves[3], ']');

  // El nuevo nodo tendrá la misma propiedad de hoja que el nodo original
  nuevoHijo^.EsHoja := hijoLleno^.EsHoja;

  // Para un nodo con 4 claves [0,1,2,3], la mediana es índice 2
  // Claves 0,1 van al nodo izquierdo
  // Clave 2 se promote a padre
  // Claves 3 va al nodo derecho

  // Obtener la clave mediana (índice 2) para promover
  claveMediana := hijoLleno^.Claves[2];
  correoMediana := hijoLleno^.Correos[2];

  WriteLn('   Clave mediana a promover: ', claveMediana);


  nuevoHijo^.Claves[0] := hijoLleno^.Claves[3];
  nuevoHijo^.Correos[0] := hijoLleno^.Correos[3];
  nuevoHijo^.NumClaves := 1;


  hijoLleno^.NumClaves := 2;  // ← CAMBIO: conserva 2 claves, no 1

  // Limpiar las claves que ya no pertenecen al nodo original
  hijoLleno^.Claves[2] := 0;
  hijoLleno^.Correos[2] := nil;
  hijoLleno^.Claves[3] := 0;
  hijoLleno^.Correos[3] := nil;

  WriteLn(' DESPUÉS DE DIVISIÓN:');
  WriteLn('   Hijo izq: [', hijoLleno^.Claves[0], ', ', hijoLleno^.Claves[1], ']');
  WriteLn('   Promovida: [', claveMediana, ']');
  WriteLn('   Hijo der: [', nuevoHijo^.Claves[0], ']');

  // Si no es hoja, mover hijos correspondientes
  if not hijoLleno^.EsHoja then
  begin
    // Los hijos 3,4 van al nuevo nodo
    nuevoHijo^.Hijos[0] := hijoLleno^.Hijos[3];
    nuevoHijo^.Hijos[1] := hijoLleno^.Hijos[4];

    // Limpiar referencias en nodo original
    hijoLleno^.Hijos[3] := nil;
    hijoLleno^.Hijos[4] := nil;
  end;

  // Mover hijos en el nodo padre para hacer espacio
  for i := nodo^.NumClaves downto indiceHijo + 1 do
    nodo^.Hijos[i + 1] := nodo^.Hijos[i];

  // Insertar el nuevo hijo
  nodo^.Hijos[indiceHijo + 1] := nuevoHijo;

  // Mover claves en el nodo padre para hacer espacio
  for i := nodo^.NumClaves - 1 downto indiceHijo do
  begin
    nodo^.Claves[i + 1] := nodo^.Claves[i];
    nodo^.Correos[i + 1] := nodo^.Correos[i];
  end;

  // Insertar la clave mediana en el nodo padre
  nodo^.Claves[indiceHijo] := claveMediana;
  nodo^.Correos[indiceHijo] := correoMediana;
  Inc(nodo^.NumClaves);

  WriteLn('División completada correctamente');
end;

// =============== VALIDACIÓN ADICIONAL ===============

function TEDDMailSystem.ValidarEstructuraArbolB(nodo: PNodoB): Boolean;
var
  i: Integer;
begin
  Result := True;
  if nodo = nil then Exit;

  // Verificar que no hay nodos con más de 4 claves
  if nodo^.NumClaves > 4 then
  begin
    WriteLn('ERROR: Nodo con ', nodo^.NumClaves, ' claves (máximo 4)');
    Result := False;
  end;

  // Verificar que las claves están ordenadas
  for i := 0 to nodo^.NumClaves - 2 do
  begin
    if nodo^.Claves[i] >= nodo^.Claves[i + 1] then
    begin
      WriteLn('ERROR: Claves desordenadas: ', nodo^.Claves[i], ' >= ', nodo^.Claves[i + 1]);
      Result := False;
    end;
  end;

  // Verificar hijos recursivamente
  if not nodo^.EsHoja then
  begin
    for i := 0 to nodo^.NumClaves do
    begin
      if not ValidarEstructuraArbolB(nodo^.Hijos[i]) then
        Result := False;
    end;
  end;
end;

procedure TEDDMailSystem.InsertarEnNodoNoLleno(nodo: PNodoB; correo: PCorreo);
var
  i: Integer;
begin
  i := nodo^.NumClaves - 1;

  if nodo^.EsHoja then
  begin
    //  mover elementos y encontrar posición
    while (i >= 0) and (nodo^.Claves[i] > correo^.Id) do
    begin
      nodo^.Claves[i + 1] := nodo^.Claves[i];
      nodo^.Correos[i + 1] := nodo^.Correos[i];
      Dec(i);
    end;

    // Insertar en la posición correcta
    nodo^.Claves[i + 1] := correo^.Id;
    nodo^.Correos[i + 1] := correo;
    Inc(nodo^.NumClaves);
  end
  else
  begin
    // Nodo interno: encontrar hijo apropiado
    while (i >= 0) and (nodo^.Claves[i] > correo^.Id) do
      Dec(i);

    Inc(i);

    // Si el hijo está lleno, dividirlo primero
    if nodo^.Hijos[i]^.NumClaves = 4 then
    begin
      DividirNodoB(nodo, i, nodo);

      // Después de dividir, decidir en cuál de los dos hijos insertar
      if nodo^.Claves[i] < correo^.Id then
        Inc(i);
    end;

    // Insertar en el hijo apropiado
    InsertarEnNodoNoLleno(nodo^.Hijos[i], correo);
  end;
end;

// =============== FUNCIONES ADICIONALES ===============

function TEDDMailSystem.ContarFavoritos(Usuario: PUsuario): Integer;

  function ContarNodos(nodo: PNodoB): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    if nodo = nil then Exit;

    Result := nodo^.NumClaves;

    if not nodo^.EsHoja then
    begin
      for i := 0 to nodo^.NumClaves do
        Result := Result + ContarNodos(nodo^.Hijos[i]);
    end;
  end;

begin
  Result := 0;
  if Usuario = nil then Exit;
  Result := ContarNodos(Usuario^.ArbolFavoritos);
end;

function TEDDMailSystem.BuscarEnFavoritos(Usuario: PUsuario; CorreoId: Integer): PCorreo;
begin
  Result := nil;
  if Usuario = nil then Exit;
  Result := BuscarB(Usuario^.ArbolFavoritos, CorreoId);
end;

function TEDDMailSystem.DesmarcarFavorito(Usuario: PUsuario; CorreoId: Integer): Boolean;
begin
  Result := EliminarFavorito(Usuario, CorreoId); // función que existe arriba
end;

procedure TEDDMailSystem.RecorridoInOrdenB(nodo: PNodoB; lista: TStringList);
var
  i: Integer;
  Display: String;
begin
  if nodo = nil then Exit;

  for i := 0 to nodo^.NumClaves - 1 do
  begin
    // Recorrer hijo izquierdo
    if not nodo^.EsHoja then
      RecorridoInOrdenB(nodo^.Hijos[i], lista);

    // Procesar clave actual
    if nodo^.Correos[i] <> nil then
    begin
      Display := Format('ID: %d - %s → %s: %s',
        [nodo^.Claves[i],
         nodo^.Correos[i]^.Remitente,
         nodo^.Correos[i]^.Destinatario,
         nodo^.Correos[i]^.Asunto]);
      lista.Add(Display);
    end;
  end;

  // Recorrer último hijo
  if not nodo^.EsHoja then
    RecorridoInOrdenB(nodo^.Hijos[nodo^.NumClaves], lista);
end;

function TEDDMailSystem.EsArbolBValido(nodo: PNodoB): Boolean;
var
  i: Integer;
begin
  Result := True;
  if nodo = nil then Exit;

  // Verificar número de claves
  if (nodo^.NumClaves < 1) or (nodo^.NumClaves > 4) then
  begin
    Result := False;
    Exit;
  end;

  // Verificar orden de claves
  for i := 0 to nodo^.NumClaves - 2 do
  begin
    if nodo^.Claves[i] >= nodo^.Claves[i + 1] then
    begin
      Result := False;
      Exit;
    end;
  end;

  // Verificar hijos recursivamente
  if not nodo^.EsHoja then
  begin
    for i := 0 to nodo^.NumClaves do
    begin
      if not EsArbolBValido(nodo^.Hijos[i]) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;



//  función auxiliar para buscar en AVL
function TEDDMailSystem.BuscarCorreoEnAVL(nodo: PNodoAVL; CorreoId: Integer): PCorreo;
begin
  Result := nil;
  if nodo = nil then Exit;

  if CorreoId = nodo^.Correo^.Id then
  begin
    Result := nodo^.Correo;
    Exit;
  end;

  if CorreoId < nodo^.Correo^.Id then
    Result := BuscarCorreoEnAVL(nodo^.Izquierdo, CorreoId)
  else
    Result := BuscarCorreoEnAVL(nodo^.Derecho, CorreoId);
end;


//  función para generar reportes de las nuevas estructuras
procedure TEDDMailSystem.GenerarReporteBorradores(Usuario: PUsuario; RutaCarpeta: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
begin
  if Usuario = nil then Exit;

  try
    ForceDirectories(RutaCarpeta);
    NombreArchivo := RutaCarpeta + '/borradores_' +
                   StringReplace(Usuario^.Usuario, ' ', '_', [rfReplaceAll]) + '.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph G {');
    WriteLn(Archivo, '    label="Árbol AVL - Borradores - ' + Usuario^.Nombre + '";');
    WriteLn(Archivo, '    fontsize=16;');
    WriteLn(Archivo, '    node [shape=record, style=filled, fillcolor=lightyellow];');

    if Usuario^.ArbolBorradores = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin borradores", fillcolor=lightgray];');
    end
    else
    begin
      GenerarNodosAVL(Archivo, Usuario^.ArbolBorradores);
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
        WriteLn('Reporte de borradores generado: ', ChangeFileExt(NombreArchivo, '.png'));
      finally
        Process.Free;
      end;
    except
      on E: Exception do
        WriteLn('Error al generar imagen: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte de borradores: ', E.Message);
  end;
end;

procedure TEDDMailSystem.GenerarNodosAVL(var Archivo: TextFile; nodo: PNodoAVL);
begin
  if nodo = nil then Exit;


  WriteLn(Archivo, Format('    nodo_%d [label="ID: %d\nRemitente: %s\nEstado: %s\nAsunto: %s\nFecha: %s\nMensaje: %s", shape=box, style="filled,rounded", fillcolor=lightyellow];',
    [nodo^.Correo^.Id,
     nodo^.Correo^.Id,
     nodo^.Correo^.Remitente,
     nodo^.Correo^.Estado,
     nodo^.Correo^.Asunto,
     nodo^.Correo^.Fecha,
     nodo^.Correo^.Mensaje]));

  if nodo^.Izquierdo <> nil then
  begin
    WriteLn(Archivo, Format('    nodo_%d -> nodo_%d [label="L"];',
      [nodo^.Correo^.Id, nodo^.Izquierdo^.Correo^.Id]));
    GenerarNodosAVL(Archivo, nodo^.Izquierdo);
  end;

  if nodo^.Derecho <> nil then
  begin
    WriteLn(Archivo, Format('    nodo_%d -> nodo_%d [label="R"];',
      [nodo^.Correo^.Id, nodo^.Derecho^.Correo^.Id]));
    GenerarNodosAVL(Archivo, nodo^.Derecho);
  end;
end;
function TEDDMailSystem.ObtenerAlturaArbolB(nodo: PNodoB): Integer;
begin
  if nodo = nil then
    Result := 0
  else if nodo^.EsHoja then
    Result := 1
  else
    Result := 1 + ObtenerAlturaArbolB(nodo^.Hijos[0]);
end;

function TEDDMailSystem.ObtenerNumeroNodos(nodo: PNodoB): Integer;
var
  i: Integer;
begin
  Result := 0;
  if nodo = nil then Exit;

  Result := 1; // Contar este nodo

  if not nodo^.EsHoja then
  begin
    for i := 0 to nodo^.NumClaves do
      Result := Result + ObtenerNumeroNodos(nodo^.Hijos[i]);
  end;
end;

function TEDDMailSystem.ValidarPropiedadesB(nodo: PNodoB): Boolean;
begin
  Result := EsArbolBValido(nodo);
end;

procedure TEDDMailSystem.LiberarArbolB(var raiz: PNodoB);
var
  i: Integer;
begin
  if raiz = nil then Exit;

  if not raiz^.EsHoja then
  begin
    for i := 0 to raiz^.NumClaves do
      LiberarArbolB(raiz^.Hijos[i]);
  end;

  Dispose(raiz);
  raiz := nil;
end;
           



// =================== IMPLEMENTACIONES PARA BORRADORES ===================

//  para buscar borrador
function TEDDMailSystem.BuscarBorrador(Usuario: PUsuario; Id: Integer): PCorreo;
begin
  Result := nil;
  if (Usuario = nil) or (Usuario^.ArbolBorradores = nil) then Exit;

  Result := BuscarCorreoEnAVL(Usuario^.ArbolBorradores, Id);
end;

// encontrar el nodo con valor mínimo
function TEDDMailSystem.BuscarMinimoAVL(nodo: PNodoAVL): PNodoAVL;
begin
  Result := nodo;
  if Result = nil then Exit;

  while Result^.Izquierdo <> nil do
    Result := Result^.Izquierdo;
end;

// eliminar un nodo del AVL
function TEDDMailSystem.EliminarAVL(nodo: PNodoAVL; id: Integer): PNodoAVL;
var
  temp: PNodoAVL;
  balance: Integer;
begin
  // Paso 1: Eliminación estándar de BST
  if nodo = nil then
  begin
    Result := nodo;
    Exit;
  end;

  if id < nodo^.Correo^.Id then
    nodo^.Izquierdo := EliminarAVL(nodo^.Izquierdo, id)
  else if id > nodo^.Correo^.Id then
    nodo^.Derecho := EliminarAVL(nodo^.Derecho, id)
  else
  begin
    // nodo a eliminar
    if (nodo^.Izquierdo = nil) or (nodo^.Derecho = nil) then
    begin
      if nodo^.Izquierdo <> nil then
        temp := nodo^.Izquierdo
      else
        temp := nodo^.Derecho;

      if temp = nil then
      begin
        // Sin hijos
        temp := nodo;
        nodo := nil;
      end
      else
      begin
        // Un hijo
        nodo^ := temp^;
      end;

      Dispose(temp);
    end
    else
    begin
      // Nodo con dos hijos
      temp := BuscarMinimoAVL(nodo^.Derecho);

      // Copiar los datos del sucesor inorden al nodo actual
      nodo^.Correo := temp^.Correo;

      // Eliminar el sucesor inorden
      nodo^.Derecho := EliminarAVL(nodo^.Derecho, temp^.Correo^.Id);
    end;
  end;

  // Si el árbol tenía solo un nodo
  if nodo = nil then
  begin
    Result := nodo;
    Exit;
  end;

  // Paso 2: Actualizar altura del nodo actual
  nodo^.Altura := 1 + Max(ObtenerAltura(nodo^.Izquierdo), ObtenerAltura(nodo^.Derecho));

  // Paso 3: para obtener balance
  balance := ObtenerBalance(nodo);

  // Paso 4: Balancear el árbol si es necesario

  // Caso izquierda-izquierda
  if (balance > 1) and (ObtenerBalance(nodo^.Izquierdo) >= 0) then
  begin
    Result := RotarDerecha(nodo);
    Exit;
  end;

  // Caso derecha-derecha
  if (balance < -1) and (ObtenerBalance(nodo^.Derecho) <= 0) then
  begin
    Result := RotarIzquierda(nodo);
    Exit;
  end;

  // Caso izquierda-derecha
  if (balance > 1) and (ObtenerBalance(nodo^.Izquierdo) < 0) then
  begin
    nodo^.Izquierdo := RotarIzquierda(nodo^.Izquierdo);
    Result := RotarDerecha(nodo);
    Exit;
  end;

  // Caso derecha-izquierda
  if (balance < -1) and (ObtenerBalance(nodo^.Derecho) > 0) then
  begin
    nodo^.Derecho := RotarDerecha(nodo^.Derecho);
    Result := RotarIzquierda(nodo);
    Exit;
  end;

  Result := nodo;
end;

// Función pública: eliminar borrador
function TEDDMailSystem.EliminarBorrador(Usuario: PUsuario; Id: Integer): Boolean;
begin
  Result := False;
  if (Usuario = nil) or (Usuario^.ArbolBorradores = nil) then Exit;

  try
    Usuario^.ArbolBorradores := EliminarAVL(Usuario^.ArbolBorradores, Id);
    Result := True;
  except
    Result := False;
  end;
end;

// para actualizar borrador
function TEDDMailSystem.ActualizarBorrador(Usuario: PUsuario; Id: Integer;
  NuevoDestinatario, NuevoAsunto, NuevoCuerpo: String): Boolean;
var
  BorradorExistente: PCorreo;
begin
  Result := False;
  if (Usuario = nil) or (Usuario^.ArbolBorradores = nil) then Exit;

  BorradorExistente := BuscarCorreoEnAVL(Usuario^.ArbolBorradores, Id);
  if BorradorExistente = nil then Exit;

  try
    // Actualizar los campos del borrador
    BorradorExistente^.Destinatario := NuevoDestinatario;
    BorradorExistente^.Asunto := NuevoAsunto;
    BorradorExistente^.Cuerpo := NuevoCuerpo;
    BorradorExistente^.FechaHora := Now; // Actualizamos a fecha de modificación

    Result := True;
  except
    Result := False;
  end;
end;
 end.
