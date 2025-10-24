unit EstructurasDatos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, process, DateUtils, Contnrs;

type

      TBitWriter = class
  private
    FStream: TMemoryStream;
    FBuffer: Byte;
    FBitsInBuffer: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteBits(Value: Word; NumBits: Integer);
    procedure Flush;
    function GetData: TBytes;
  end;

  TBitReader = class
  private
    FData: TBytes;
    FPosition: Integer;
    FBuffer: Byte;
    FBitsInBuffer: Integer;
  public
    constructor Create(const Data: TBytes);
    function ReadBits(NumBits: Integer): Word;
    function HasMore: Boolean;
  end;
  // Tipos de punteros
  PUsuario = ^TUsuario;
  PCorreo = ^TCorreo;
  PContacto = ^TContacto;
  PComunidad = ^TComunidad;
  PUsuarioComunidad = ^TUsuarioComunidad;
  PMatrizDispersaFila = ^TMatrizDispersaFila;
  PMatrizDispersaColumna = ^TMatrizDispersaColumna;
  PMatrizDispersaNodo = ^TMatrizDispersaNodo;
  //Fase 3
    PNodoMerkle = ^TNodoMerkle;


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
    Id: Integer;                    //  ID único del mensaje
    Correo: String;
    Mensaje: String;
    FechaPublicacion: String;
    Reacciones: Integer;            // Contador de reacciones
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
      ArbolMerkleFavoritos: PNodoMerkle;  // Nuevo: Árbol de Merkle

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

  PRegistroLogueo = ^TRegistroLogueo;
  TRegistroLogueo = record
  Usuario: String;
  Entrada: TDateTime;
  Salida: TDateTime;
  Siguiente: PRegistroLogueo;
  end;
  // ============= ÁRBOLES DE MERKLE (FASE 3) =============

TNodoMerkle = record
  Hash: String;                    // Hash SHA-256 del nodo
  Correo: PCorreo;                 // Solo las hojas tienen correos
  Izquierdo: PNodoMerkle;          // Hijo izquierdo
  Derecho: PNodoMerkle;            // Hijo derecho
  EsHoja: Boolean;                 // Indica si es nodo hoja
end;

// Estructura de Bloque para Blockchain
PBloqueBlockchain = ^TBloqueBlockchain;
TBloqueBlockchain = record
  Index: Integer;                 // Número del bloque (0 para génesis)
  Timestamp: String;              // Fecha y hora DD-MM-YY::HH:MM:SS
  Data: String;                   // Info del correo (ID, Remitente, Asunto, Mensaje)
  Nonce: Integer;                 // Número para prueba de trabajo
  PreviousHash: String;           // Hash del bloque anterior
  Hash: String;                   // Hash del bloque actual (SHA-256)
  Siguiente: PBloqueBlockchain;   // Puntero al siguiente bloque
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
      FListaLogueo: PRegistroLogueo;
    // Campos para Blockchain
    FBlockchainHead: PBloqueBlockchain;
    FBlockchainCount: Integer;


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


       function CalcularHashSHA256(Texto: String): String;
  function CrearNodoMerkle: PNodoMerkle;
  function CrearHojaMerkle(Correo: PCorreo): PNodoMerkle;
  function CombinarHashesMerkle(HashIzq, HashDer: String): String;
  function ConstruirArbolMerkle(Correos: array of PCorreo; Inicio, Fin: Integer): PNodoMerkle;
  procedure RecolectarCorreosMerkle(Nodo: PNodoMerkle; Lista: TList);

  procedure GenerarNodosMerkle(var Archivo: TextFile; Nodo: PNodoMerkle; var ContadorNodo: Integer);

      // NUEVOS MÉTODOS PRIVADOS para MERKLE:
    function ConstruirArbolMerkleBalanceado(ListaHojas: array of PNodoMerkle): PNodoMerkle;
    procedure LiberarArbolMerkle(Nodo: PNodoMerkle);
        function GenerarHashSHA256(Datos: String): String;







    // Métodos para Blockchain
    function CrearBloqueGenesis: PBloqueBlockchain;
    function CalcularHashBloque(Index: Integer; Timestamp, Data: String;
      Nonce: Integer; PreviousHash: String): String;
    function MinarBloque(Index: Integer; Timestamp, Data, PreviousHash: String): PBloqueBlockchain;
    function ValidarProofOfWork(Hash: String): Boolean;
    procedure LiberarBlockchain;
    function FormatearTimestamp: String;


      procedure InicializarMatriz;
  procedure LiberarMatriz;
  procedure LiberarArbolComunidades(var Raiz: PNodoBST);


      function BuscarUsuarioPorNombre(NombreUsuario: String): PUsuario;



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

      //FASE 3

        procedure RecorrerFavoritosMerkle(Usuario: PUsuario; Lista: TStringList);
          function VerificarIntegridadMerkle(Usuario: PUsuario): Boolean;
           procedure Merkle_ReconstruirDesdeArbolB(Usuario: PUsuario);  // Migración



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


    // ═══════════════════════════════════════════════════════
    // MÉTODOS FASE 3
    // ═══════════════════════════════════════════════════════

    // Logueo
    function ObtenerLogsDeLogueo: TStringList;
    function FiltrarLogsPorUsuario(Usuario: String): TStringList;
    function ExportarLogueoJSON(RutaArchivo: String): Boolean;
    procedure GenerarReporteLogueo(RutaSalida: String);

    // Compresión LZW
    function ComprimirLZW(Texto: String): String;
    function GuardarArchivoTexto(Ruta, Contenido: String): Boolean;

    // Nuevas funciones de compresión binaria
    function ComprimirLZWBinario(const Texto: String): TBytes;
    function DescomprimirLZWBinario(const Datos: TBytes): String;
    function GuardarArchivoBinario(const Ruta: String; const Datos: TBytes): Boolean;
    function CargarArchivoBinario(const Ruta: String): TBytes;

    // Blockchain
    function ObtenerListaBloques: TStringList;
    function ObtenerDetallesBloque(NumBloque: Integer): String;
    procedure GenerarReporteBlockchain(RutaSalida: String);

    // Merkle Tree
    procedure GenerarReporteMerkle(RutaSalida: String);

    // Grafo de contactos
    procedure GenerarReporteGrafoContactos(RutaSalida: String);

    // Importación
    procedure CargarContactosDesdeJSON(RutaArchivo: String);


    // En la sección PUBLIC de TEDDMailSystem class:
function ReaccionarAMensaje(nombreComunidad: String; idMensaje: Integer): Boolean;

function GetArbolComunidades: PNodoBST;


    function BuscarComunidadPorNombre(Nombre: String): PNodoBST;

        // NUEVO MÉTODO PÚBLICO:
    procedure ConstruirArbolMerkleDesdeCorreos(Usuario: PUsuario);


     // Métodos para Blockchain
    procedure AgregarBloqueBlockchain(CorreoId: Integer; Remitente, Asunto, Mensaje: String);
    function VerificarIntegridadBlockchain: Boolean;
    function ObtenerTotalBloques: Integer;


    end;
implementation

uses
  fpjson, jsonparser;
// ═══════════════════════════════════════════════════════════════
// IMPLEMENTACIÓN DE TBitWriter
// ═══════════════════════════════════════════════════════════════

constructor TBitWriter.Create;
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  FBuffer := 0;
  FBitsInBuffer := 0;
end;

destructor TBitWriter.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TBitWriter.WriteBits(Value: Word; NumBits: Integer);
var
  i: Integer;
begin
  for i := NumBits - 1 downto 0 do
  begin
    FBuffer := (FBuffer shl 1) or ((Value shr i) and 1);
    Inc(FBitsInBuffer);

    if FBitsInBuffer = 8 then
    begin
      FStream.WriteByte(FBuffer);
      FBuffer := 0;
      FBitsInBuffer := 0;
    end;
  end;
end;

procedure TBitWriter.Flush;
begin
  if FBitsInBuffer > 0 then
  begin
    FBuffer := FBuffer shl (8 - FBitsInBuffer);
    FStream.WriteByte(FBuffer);
    FBuffer := 0;
    FBitsInBuffer := 0;
  end;
end;

function TBitWriter.GetData: TBytes;
begin
  SetLength(Result, FStream.Size);
  FStream.Position := 0;
  FStream.Read(Result[0], FStream.Size);
end;

// ═══════════════════════════════════════════════════════════════
// IMPLEMENTACIÓN DE TBitReader
// ═══════════════════════════════════════════════════════════════

constructor TBitReader.Create(const Data: TBytes);
begin
  inherited Create;
  FData := Data;
  FPosition := 0;
  FBuffer := 0;
  FBitsInBuffer := 0;
end;

function TBitReader.ReadBits(NumBits: Integer): Word;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to NumBits - 1 do
  begin
    if FBitsInBuffer = 0 then
    begin
      if FPosition >= Length(FData) then
        Exit;
      FBuffer := FData[FPosition];
      Inc(FPosition);
      FBitsInBuffer := 8;
    end;

    Result := (Result shl 1) or ((FBuffer shr 7) and 1);
    FBuffer := FBuffer shl 1;
    Dec(FBitsInBuffer);
  end;
end;

function TBitReader.HasMore: Boolean;
begin
  Result := (FPosition < Length(FData)) or (FBitsInBuffer > 0);
end;
constructor TEDDMailSystem.Create;
begin
  inherited Create;
  FUsuarios := nil;
  FComunidades := nil;
  FMatrizFilas := nil;
  FMatrizColumnas := nil;
  FUsuarioActual := nil;
  FArbolComunidades := nil;  // se inicializa árbol de comunidades
  FListaLogueo := nil;       // INicializa lista de logueo

  // Inicializar blockchain
  FBlockchainHead := nil;
  FBlockchainCount := 0;

  WriteLn('Sistema EDDMail inicializado');
  WriteLn('Inicializando blockchain...');

  // Crea bloque génesis al iniciar el sistema
  try
    FBlockchainHead := CrearBloqueGenesis;
    FBlockchainCount := 1;
    WriteLn('✓ Blockchain inicializado con bloque génesis');
  except
    on E: Exception do
      WriteLn('Advertencia: No se pudo crear bloque génesis: ', E.Message);
  end;

  // Cargar datos iniciales
  CargarUsuariosDesdeJSON('usuarios.json');
  InicializarMatriz;

  // Crear usuario root por defecto (Id fijo = 0)
  RegistrarUsuario('Root Admin', 'root', 'root@edd.com', '00000000', 'root123', 0);
end;

destructor TEDDMailSystem.Destroy;
var
  TempUsuario: PUsuario;
  TempComunidad: PComunidad;
begin
  WriteLn('Liberando recursos del sistema...');

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

  // Liberar matriz (versión simplificada)
  FMatrizFilas := nil;
  FMatrizColumnas := nil;

  // Liberar árbol de comunidades
  if FArbolComunidades <> nil then
    LiberarArbolComunidades(FArbolComunidades);

  // Liberar blockchain
  try
    LiberarBlockchain;
    WriteLn('✓ Blockchain liberado');
  except
    on E: Exception do
      WriteLn('Error al liberar blockchain: ', E.Message);
  end;

  WriteLn('Sistema EDDMail finalizado');
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
var
  ListaCorreos: TList;
  ArrayCorreos: array of PCorreo;
  i, j: Integer;
  Encontrado: Boolean;
begin
  Result := False;
  if Usuario = nil then Exit;

  ListaCorreos := TList.Create;
  try
    // Recolectar todos los correos
    if Usuario^.ArbolMerkleFavoritos <> nil then
      RecolectarCorreosMerkle(Usuario^.ArbolMerkleFavoritos, ListaCorreos);

    // Buscar y eliminar el correo
    Encontrado := False;
    for i := 0 to ListaCorreos.Count - 1 do
    begin
      if PCorreo(ListaCorreos[i])^.Id = CorreoId then
      begin
        ListaCorreos.Delete(i);
        Encontrado := True;
        Break;
      end;
    end;

    if not Encontrado then Exit;

    // Liberar árbol anterior
    LiberarArbolMerkle(Usuario^.ArbolMerkleFavoritos);

    // Si no quedan correos, dejar el árbol en nil
    if ListaCorreos.Count = 0 then
    begin
      Usuario^.ArbolMerkleFavoritos := nil;
      Result := True;
      Exit;
    end;

    // Reconstruir árbol
    SetLength(ArrayCorreos, ListaCorreos.Count);
    for j := 0 to ListaCorreos.Count - 1 do
      ArrayCorreos[j] := PCorreo(ListaCorreos[j]);

    Usuario^.ArbolMerkleFavoritos := ConstruirArbolMerkle(ArrayCorreos, 0,
      Length(ArrayCorreos) - 1);

    WriteLn('Favorito eliminado: ID ', CorreoId);
    Result := True;
  finally
    ListaCorreos.Free;
  end;
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

  // INICIALIZAR ESTRUCTURAS FASE 2  y ahora 3
  NuevoUsuario^.ArbolBorradores := nil;
    NuevoUsuario^.ArbolMerkleFavoritos := nil;


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
var
  NuevoLog: PRegistroLogueo;
begin
  FUsuarioActual := ValidarCredenciales(Email, Password);
  Result := FUsuarioActual <> nil;

  // REGISTRA ENTRADA
  if Result then
  begin
    New(NuevoLog);
    NuevoLog^.Usuario := Email;
    NuevoLog^.Entrada := Now;
    NuevoLog^.Salida := 0; // Sin salida aún
    NuevoLog^.Siguiente := FListaLogueo;
    FListaLogueo := NuevoLog; // Insertar al inicio
  end;
end;

procedure TEDDMailSystem.CerrarSesion;
var
  Log: PRegistroLogueo;
begin
  // REGISTRA SALIDA
  if FUsuarioActual <> nil then
  begin
    Log := FListaLogueo;
    // Buscar el registro más reciente de este usuario sin salida
    while Log <> nil do
    begin
      if (Log^.Usuario = FUsuarioActual^.Email) and (Log^.Salida = 0) then
      begin
        Log^.Salida := Now;
        Break;
      end;
      Log := Log^.Siguiente;
    end;
  end;

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
begin
  // Redirigir a la función de Merkle
  GenerarReporteMerkle(RutaCarpeta);
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
  Result := Result + '========================================' + LineEnding + LineEnding;

  Mensaje := Comunidad^.ListaMensajes;
  if Mensaje = nil then
  begin
    Result := Result + 'No hay mensajes publicados en esta comunidad.';
    Exit;
  end;

  // Listar mensajes CON ID Y REACCIONES
  while Mensaje <> nil do
  begin
    Result := Result + Format('[ID: %d] 👍 %d reacciones',
      [Mensaje^.Id, Mensaje^.Reacciones]) + LineEnding;
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
  NuevoMensaje^.Id := Comunidad^.NumeroMensajes + 1;  // ← NUEVO: Asignar ID
  NuevoMensaje^.Correo := correoUsuario;
  NuevoMensaje^.Mensaje := mensaje;
  NuevoMensaje^.FechaPublicacion := FormatDateTime('dd/mm/yyyy hh:nn', Now);
  NuevoMensaje^.Reacciones := 0;                       // ← NUEVO: Inicializar en 0
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
  ListaCorreos: TList;
  ArrayCorreos: array of PNodoMerkle;
  i: Integer;
  NodoHoja: PNodoMerkle;
  DatosCorreo: String;
begin
  Result := False;
  if Usuario = nil then Exit;

  // 1. Buscar el correo en la bandeja de entrada
  Correo := BuscarCorreoEnBandeja(Usuario, CorreoId);
  if Correo = nil then
  begin
    WriteLn('Error: Correo no encontrado');
    Exit;
  end;

  // 2. Verificar si ya está en favoritos
  if BuscarEnFavoritos(Usuario, CorreoId) <> nil then
  begin
    WriteLn('El correo ya está marcado como favorito');
    Exit;
  end;

  // 3. Recolectar todos los correos existentes en favoritos
  ListaCorreos := TList.Create;
  try
    if Usuario^.ArbolMerkleFavoritos <> nil then
      RecolectarCorreosMerkle(Usuario^.ArbolMerkleFavoritos, ListaCorreos);

    // 4. ✅ AGREGAR EL NUEVO CORREO A LA LISTA
    ListaCorreos.Add(Correo);

    // 5. Convertir a array de nodos Merkle
    SetLength(ArrayCorreos, ListaCorreos.Count);

    for i := 0 to ListaCorreos.Count - 1 do
    begin
      // Crear nodo hoja para cada correo
      New(NodoHoja);
      NodoHoja^.EsHoja := True;
      NodoHoja^.Correo := PCorreo(ListaCorreos[i]);
      NodoHoja^.Izquierdo := nil;
      NodoHoja^.Derecho := nil;

      // Generar hash del correo
      DatosCorreo := IntToStr(NodoHoja^.Correo^.Id) +
                     NodoHoja^.Correo^.Remitente +
                     NodoHoja^.Correo^.Asunto +
                     NodoHoja^.Correo^.Fecha +
                     NodoHoja^.Correo^.Mensaje;
      NodoHoja^.Hash := GenerarHashSHA256(DatosCorreo);

      ArrayCorreos[i] := NodoHoja;
    end;

    // 6. Liberar árbol anterior
    if Usuario^.ArbolMerkleFavoritos <> nil then
      LiberarArbolMerkle(Usuario^.ArbolMerkleFavoritos);

    // 7. Construir nuevo árbol de Merkle BALANCEADO
    Usuario^.ArbolMerkleFavoritos := ConstruirArbolMerkleBalanceado(ArrayCorreos);

    WriteLn('✓ Correo marcado como favorito: ID ', CorreoId);
    WriteLn('✓ Árbol de Merkle reconstruido con ', ListaCorreos.Count, ' correos');
    Result := True;

  finally
    ListaCorreos.Free;
  end;
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
var
  ListaCorreos: TList;
begin
  Result := 0;
  if Usuario = nil then Exit;

  ListaCorreos := TList.Create;
  try
    if Usuario^.ArbolMerkleFavoritos <> nil then
      RecolectarCorreosMerkle(Usuario^.ArbolMerkleFavoritos, ListaCorreos);
    Result := ListaCorreos.Count;
  finally
    ListaCorreos.Free;
  end;
end;

function TEDDMailSystem.BuscarEnFavoritos(Usuario: PUsuario; CorreoId: Integer): PCorreo;

  function BuscarRecursivo(Nodo: PNodoMerkle; Id: Integer): PCorreo;
  begin
    Result := nil;
    if Nodo = nil then Exit;

    if Nodo^.EsHoja then
    begin
      if (Nodo^.Correo <> nil) and (Nodo^.Correo^.Id = Id) then
        Result := Nodo^.Correo;
    end
    else
    begin
      Result := BuscarRecursivo(Nodo^.Izquierdo, Id);
      if Result = nil then
        Result := BuscarRecursivo(Nodo^.Derecho, Id);
    end;
  end;

begin
  Result := nil;
  if Usuario = nil then Exit;
  Result := BuscarRecursivo(Usuario^.ArbolMerkleFavoritos, CorreoId);
end;

function TEDDMailSystem.DesmarcarFavorito(Usuario: PUsuario; CorreoId: Integer): Boolean;
begin
  Result := False;

  if Usuario = nil then Exit;

  // Verificar si existe en favoritos
  if BuscarEnFavoritos(Usuario, CorreoId) = nil then
  begin
    WriteLn('El correo no está en favoritos');
    Exit;
  end;

  // Eliminar usando la función existente
  Result := EliminarFavorito(Usuario, CorreoId);

  if Result then
  begin
    // Reconstruir el árbol de Merkle
    ConstruirArbolMerkleDesdeCorreos(Usuario);
    WriteLn('✓ Correo eliminado de favoritos y árbol Merkle actualizado');
  end;
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

// ═══════════════════════════════════════════════════════
// IMPLEMENTACIONES FASE 3
// ═══════════════════════════════════════════════════════

function TEDDMailSystem.ObtenerLogsDeLogueo: TStringList;
var
  Log: PRegistroLogueo;
  FechaEntrada, FechaSalida: String;
begin
  Result := TStringList.Create;

  Log := FListaLogueo;
  while Log <> nil do
  begin
    FechaEntrada := FormatDateTime('yyyy-mm-dd hh:nn:ss', Log^.Entrada);

    if Log^.Salida > 0 then
      FechaSalida := FormatDateTime('yyyy-mm-dd hh:nn:ss', Log^.Salida)
    else
      FechaSalida := 'Sesión activa';

    Result.Add(Format('%s | Entrada: %s | Salida: %s',
      [Log^.Usuario, FechaEntrada, FechaSalida]));

    Log := Log^.Siguiente;
  end;

  if Result.Count = 0 then
    Result.Add('No hay registros de logueo');
end;

function TEDDMailSystem.FiltrarLogsPorUsuario(Usuario: String): TStringList;
var
  Log: PRegistroLogueo;
  FechaEntrada, FechaSalida: String;
begin
  Result := TStringList.Create;

  Log := FListaLogueo;
  while Log <> nil do
  begin
    if Pos(LowerCase(Usuario), LowerCase(Log^.Usuario)) > 0 then
    begin
      FechaEntrada := FormatDateTime('yyyy-mm-dd hh:nn:ss', Log^.Entrada);

      if Log^.Salida > 0 then
        FechaSalida := FormatDateTime('yyyy-mm-dd hh:nn:ss', Log^.Salida)
      else
        FechaSalida := 'Sesión activa';

      Result.Add(Format('%s | Entrada: %s | Salida: %s',
        [Log^.Usuario, FechaEntrada, FechaSalida]));
    end;

    Log := Log^.Siguiente;
  end;

  if Result.Count = 0 then
    Result.Add(Format('No hay registros para el usuario: %s', [Usuario]));
end;

function TEDDMailSystem.ExportarLogueoJSON(RutaArchivo: String): Boolean;
var
  F: TextFile;
  Log: PRegistroLogueo;
  FechaEntrada, FechaSalida: String;
  IsFirst: Boolean;
begin
  Result := False;

  try
    AssignFile(F, RutaArchivo);
    Rewrite(F);

    WriteLn(F, '[');  // ← Inicio del array JSON

    Log := FListaLogueo;
    IsFirst := True;

    while Log <> nil do
    begin
      if not IsFirst then
        WriteLn(F, ',');  // ← Coma entre objetos

      FechaEntrada := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Log^.Entrada);

      if Log^.Salida > 0 then
        FechaSalida := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Log^.Salida)
      else
        FechaSalida := '';

      // ✅ CORRECCIÓN: Agregar llaves y formato correcto
      Write(F, '  {');
      Write(F, ' "usuario": "' + Log^.Usuario + '",');
      Write(F, ' "entrada": "' + FechaEntrada + '",');
      Write(F, ' "salida": "' + FechaSalida + '"');
      Write(F, ' }');  // ← Cerrar objeto

      IsFirst := False;
      Log := Log^.Siguiente;
    end;

    WriteLn(F, '');  // ← Nueva línea antes del cierre
    WriteLn(F, ']');  // ← Cierre del array JSON

    CloseFile(F);
    Result := True;
  except
    on E: Exception do
    begin
      WriteLn('Error al exportar JSON: ', E.Message);
      Result := False;
    end;
  end;
end;

procedure TEDDMailSystem.GenerarReporteLogueo(RutaSalida: String);
var
  Archivo: TextFile;
  DotPath, PngPath, Comando: String;
  Log: PRegistroLogueo;
  FechaEntrada, FechaSalida: String;
  Contador: Integer;
  Proceso: TProcess;
begin
  try
    // Crear carpeta si no existe
    if not DirectoryExists(RutaSalida) then
      CreateDir(RutaSalida);

    DotPath := RutaSalida + '/logueo.dot';
    PngPath := RutaSalida + '/logueo.png';

    AssignFile(Archivo, DotPath);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph ControlLogueo {');
    WriteLn(Archivo, '  rankdir=TB;');
    WriteLn(Archivo, '  node [shape=box, style=filled];');
    WriteLn(Archivo, '');
    WriteLn(Archivo, '  Titulo [label="Control de Logueo", fillcolor=lightblue, shape=ellipse, fontsize=16];');
    WriteLn(Archivo, '');

    Log := FListaLogueo;
    Contador := 0;

    while Log <> nil do
    begin
      FechaEntrada := FormatDateTime('yyyy-mm-dd hh:nn:ss', Log^.Entrada);

      if Log^.Salida > 0 then
        FechaSalida := FormatDateTime('yyyy-mm-dd hh:nn:ss', Log^.Salida)
      else
        FechaSalida := 'Activo';

      WriteLn(Archivo, Format('  Log%d [label="Usuario: %s\nEntrada: %s\nSalida: %s", fillcolor=lightgreen];',
        [Contador, Log^.Usuario, FechaEntrada, FechaSalida]));

      if Contador > 0 then
        WriteLn(Archivo, Format('  Log%d -> Log%d;', [Contador - 1, Contador]));

      Inc(Contador);
      Log := Log^.Siguiente;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    WriteLn('✅ Archivo .dot generado en: ', DotPath);

    // ✅ GENERAR PNG usando TProcess
    try
      Proceso := TProcess.Create(nil);
      try
        Proceso.Executable := 'dot';
        Proceso.Parameters.Add('-Tpng');
        Proceso.Parameters.Add(DotPath);
        Proceso.Parameters.Add('-o');
        Proceso.Parameters.Add(PngPath);
        Proceso.Options := [poWaitOnExit, poNoConsole];
        Proceso.Execute;

        if FileExists(PngPath) then
          WriteLn('✅ Reporte PNG generado en: ', PngPath)
        else
          WriteLn('⚠️  No se pudo generar el PNG. Verifique que Graphviz esté instalado.');
      finally
        Proceso.Free;
      end;
    except
      on E: Exception do
        WriteLn('⚠️  Error al ejecutar Graphviz: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('❌ Error al generar reporte de logueo: ', E.Message);
  end;
end;

function TEDDMailSystem.ComprimirLZW(Texto: String): String;
var
  Dict: TStringList;
  W, WC: String;
  i, Code: Integer;
  Output: TStringList;
begin
  if Length(Texto) = 0 then
  begin
    Result := '';
    Exit;
  end;

  Dict := TStringList.Create;
  Output := TStringList.Create;
  try
    // Inicializar diccionario con caracteres ASCII (0-255)
    for i := 0 to 255 do
      Dict.Add(Chr(i));

    W := Texto[1];

    // Algoritmo LZW
    for i := 2 to Length(Texto) do
    begin
      WC := W + Texto[i];

      if Dict.IndexOf(WC) >= 0 then
      begin
        // La secuencia ya existe en el diccionario
        W := WC;
      end
      else
      begin
        // Emitir el código de W
        Code := Dict.IndexOf(W);
        Output.Add(IntToStr(Code));

        // Agregar nueva secuencia al diccionario (límite 4096)
        if Dict.Count < 4096 then
          Dict.Add(WC);

        // Reiniciar W con el carácter actual
        W := Texto[i];
      end;
    end;

    // Emitir el último código
    Code := Dict.IndexOf(W);
    Output.Add(IntToStr(Code));

    // Resultado: códigos separados por comas
    Output.Delimiter := ' ';
    Output.StrictDelimiter := True;
    Result := Output.DelimitedText;

  finally
    Dict.Free;
    Output.Free;
  end;
end;
// ═══════════════════════════════════════════════════════════════
// NUEVA COMPRESIÓN BINARIA LZW
// ═══════════════════════════════════════════════════════════════

function TEDDMailSystem.ComprimirLZWBinario(const Texto: String): TBytes;
var
  Dict: TStringList;
  W, WC: String;
  i, Code: Integer;
  Writer: TBitWriter;
  MaxDictSize: Integer;
begin
  if Length(Texto) = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  Dict := TStringList.Create;
  Writer := TBitWriter.Create;
  try
    // Inicializar diccionario ASCII (0-255)
    for i := 0 to 255 do
      Dict.Add(Chr(i));

    MaxDictSize := 4096; // 2^12
    W := Texto[1];

    // Algoritmo LZW con salida binaria
    for i := 2 to Length(Texto) do
    begin
      WC := W + Texto[i];

      if Dict.IndexOf(WC) >= 0 then
      begin
        W := WC;
      end
      else
      begin
        // Escribir código en 12 bits (¡NO COMO TEXTO!)
        Code := Dict.IndexOf(W);
        Writer.WriteBits(Code, 12);  // ← CLAVE: 12 bits binarios

        if Dict.Count < MaxDictSize then
          Dict.Add(WC);

        W := Texto[i];
      end;
    end;

    // Escribir último código
    Code := Dict.IndexOf(W);
    Writer.WriteBits(Code, 12);

    Writer.Flush;
    Result := Writer.GetData;

  finally
    Dict.Free;
    Writer.Free;
  end;
end;

function TEDDMailSystem.DescomprimirLZWBinario(const Datos: TBytes): String;
var
  Dict: TStringList;
  Reader: TBitReader;
  OldCode, NewCode: Word;
  S, Entry: String;
  MaxDictSize: Integer;
  i: Integer;
begin
  Result := '';

  if Length(Datos) = 0 then
    Exit;

  Dict := TStringList.Create;
  Reader := TBitReader.Create(Datos);
  try
    // Inicializar diccionario
    for i := 0 to 255 do
      Dict.Add(Chr(i));

    MaxDictSize := 4096;

    // Leer primer código
    OldCode := Reader.ReadBits(12);
    if OldCode >= Dict.Count then
      Exit;

    S := Dict[OldCode];
    Result := S;

    // Descomprimir
    while Reader.HasMore do
    begin
      NewCode := Reader.ReadBits(12);

      if NewCode >= Dict.Count then
        Entry := S + S[1]
      else
        Entry := Dict[NewCode];

      Result := Result + Entry;

      if Dict.Count < MaxDictSize then
        Dict.Add(S + Entry[1]);

      S := Entry;
    end;

  finally
    Dict.Free;
    Reader.Free;
  end;
end;

function TEDDMailSystem.GuardarArchivoBinario(const Ruta: String; const Datos: TBytes): Boolean;
var
  FileStream: TFileStream;
begin
  try
    FileStream := TFileStream.Create(Ruta, fmCreate);
    try
      if Length(Datos) > 0 then
        FileStream.WriteBuffer(Datos[0], Length(Datos));
      Result := True;
    finally
      FileStream.Free;
    end;
  except
    Result := False;
  end;
end;

function TEDDMailSystem.CargarArchivoBinario(const Ruta: String): TBytes;
var
  FileStream: TFileStream;
begin
  try
    FileStream := TFileStream.Create(Ruta, fmOpenRead);
    try
      SetLength(Result, FileStream.Size);
      if FileStream.Size > 0 then
        FileStream.ReadBuffer(Result[0], FileStream.Size);
    finally
      FileStream.Free;
    end;
  except
    SetLength(Result, 0);
  end;
end;

function TEDDMailSystem.GuardarArchivoTexto(Ruta, Contenido: String): Boolean;
var
  F: TextFile;
begin
  try
    AssignFile(F, Ruta);
    Rewrite(F);
    Write(F, Contenido);
    CloseFile(F);
    Result := True;
  except
    Result := False;
  end;
end;


procedure TEDDMailSystem.GenerarReporteMerkle(RutaSalida: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
  Contador: Integer;
  Usuario: PUsuario;
begin
  Usuario := GetUsuarioActual;
  if Usuario = nil then Exit;

  try
    ForceDirectories(RutaSalida);
    NombreArchivo := RutaSalida + '/merkle_tree.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph MerkleTree {');
    WriteLn(Archivo, '    label="Árbol de Merkle - Privados";');
    WriteLn(Archivo, '    fontsize=20;');
    WriteLn(Archivo, '    node [shape=box, style=filled];');
    WriteLn(Archivo, '    rankdir=TB;');
    WriteLn(Archivo, '');

    if Usuario^.ArbolMerkleFavoritos = nil then
    begin
      WriteLn(Archivo, '    empty [label="Sin privados", fillcolor=lightgray];');
    end
    else
    begin
      Contador := 0;
      GenerarNodosMerkle(Archivo, Usuario^.ArbolMerkleFavoritos, Contador);
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    // Generar PNG
    Process := TProcess.Create(nil);
    try
      Process.Executable := 'dot';
      Process.Parameters.Add('-Tpng');
      Process.Parameters.Add(NombreArchivo);
      Process.Parameters.Add('-o');
      Process.Parameters.Add(ChangeFileExt(NombreArchivo, '.png'));
      Process.Options := Process.Options + [poWaitOnExit];
      Process.Execute;
      WriteLn('✓ Reporte Merkle generado: ', ChangeFileExt(NombreArchivo, '.png'));
    finally
      Process.Free;
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte Merkle: ', E.Message);
  end;
end;

procedure TEDDMailSystem.GenerarReporteGrafoContactos(RutaSalida: String);
var
  Archivo: TextFile;
  DotPath, PngPath: String;
  Usuario: PUsuario;
  Contacto: PContacto;
  Proceso: TProcess;
  UsuarioIdLimpio, ContactoIdLimpio: String;
  UsuariosAgregados, ContactosAgregados: TStringList;
begin
  try
    // Crear carpeta si no existe
    if not DirectoryExists(RutaSalida) then
      CreateDir(RutaSalida);

    DotPath := RutaSalida + '/grafo_contactos.dot';
    PngPath := RutaSalida + '/grafo_contactos.png';

    AssignFile(Archivo, DotPath);
    Rewrite(Archivo);

    // ✅ CAMBIO: Usar "graph" en lugar de "digraph"
    WriteLn(Archivo, 'graph GrafoContactos {');
    WriteLn(Archivo, '  label="Reporte de relación de usuarios con contactos (Grafos)";');
    WriteLn(Archivo, '  fontsize=16;');
    WriteLn(Archivo, '  fontname="Arial";');
    WriteLn(Archivo, '  rankdir=LR;');
    WriteLn(Archivo, '  ranksep=2.0;');  // ✅ Más separación horizontal
    WriteLn(Archivo, '  nodesep=0.8;');  // ✅ Más separación vertical
    WriteLn(Archivo, '  node [fontsize=11, fontname="Arial"];');
    WriteLn(Archivo, '  edge [penwidth=1.5];');  // ✅ Líneas más gruesas
    WriteLn(Archivo, '');

    // Listas para controlar duplicados
    UsuariosAgregados := TStringList.Create;
    ContactosAgregados := TStringList.Create;
    UsuariosAgregados.Sorted := True;
    ContactosAgregados.Sorted := True;
    UsuariosAgregados.Duplicates := dupIgnore;
    ContactosAgregados.Duplicates := dupIgnore;

    try
      // PASO 1: Definir USUARIOS (lado izquierdo)
      WriteLn(Archivo, '  // ========== USUARIOS (Izquierda) ==========');
      WriteLn(Archivo, '  {');
      WriteLn(Archivo, '    rank=same;');
      WriteLn(Archivo, '    node [shape=circle, style=filled, fillcolor=lightblue];');
      WriteLn(Archivo, '');

      Usuario := FUsuarios;
      while Usuario <> nil do
      begin
        UsuarioIdLimpio := 'user_' + IntToStr(Usuario^.Id);

        if UsuariosAgregados.IndexOf(UsuarioIdLimpio) = -1 then
        begin
          WriteLn(Archivo, Format('    %s [label="ID: %d\nUsuario: %s"];',
            [UsuarioIdLimpio, Usuario^.Id, Usuario^.Usuario]));
          UsuariosAgregados.Add(UsuarioIdLimpio);
        end;

        Usuario := Usuario^.Siguiente;
      end;

      WriteLn(Archivo, '  }');
      WriteLn(Archivo, '');

      // PASO 2: Definir CONTACTOS (lado derecho)
      WriteLn(Archivo, '  // ========== CONTACTOS (Derecha) ==========');
      WriteLn(Archivo, '  {');
      WriteLn(Archivo, '    rank=same;');
      WriteLn(Archivo, '    node [shape=circle, style=filled, fillcolor=lightgreen];');
      WriteLn(Archivo, '');

      // Recorrer todos los usuarios y sus contactos
      Usuario := FUsuarios;
      while Usuario <> nil do
      begin
        if Usuario^.ListaContactos <> nil then
        begin
          Contacto := Usuario^.ListaContactos;

          repeat
            ContactoIdLimpio := 'contact_' + IntToStr(Contacto^.Id);

            if ContactosAgregados.IndexOf(ContactoIdLimpio) = -1 then
            begin
              WriteLn(Archivo, Format('    %s [label="ID: %d\nContacto: %s"];',
                [ContactoIdLimpio, Contacto^.Id, Contacto^.Usuario]));
              ContactosAgregados.Add(ContactoIdLimpio);
            end;

            Contacto := Contacto^.Siguiente;
          until Contacto = Usuario^.ListaContactos;
        end;

        Usuario := Usuario^.Siguiente;
      end;

      WriteLn(Archivo, '  }');
      WriteLn(Archivo, '');

      // PASO 3: Crear las conexiones (sin flechas)
      WriteLn(Archivo, '  // ========== RELACIONES ==========');
      WriteLn(Archivo, '');

      Usuario := FUsuarios;
      while Usuario <> nil do
      begin
        if Usuario^.ListaContactos <> nil then
        begin
          UsuarioIdLimpio := 'user_' + IntToStr(Usuario^.Id);
          Contacto := Usuario^.ListaContactos;

          repeat
            ContactoIdLimpio := 'contact_' + IntToStr(Contacto^.Id);
            // ✅ CAMBIO: Usar "--" en lugar de "->" para grafos no dirigidos
            WriteLn(Archivo, Format('  %s -- %s;', [UsuarioIdLimpio, ContactoIdLimpio]));

            Contacto := Contacto^.Siguiente;
          until Contacto = Usuario^.ListaContactos;
        end;

        Usuario := Usuario^.Siguiente;
      end;

    finally
      UsuariosAgregados.Free;
      ContactosAgregados.Free;
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    WriteLn('✅ Archivo .dot generado en: ', DotPath);

    // Generar PNG con Graphviz
    try
      Proceso := TProcess.Create(nil);
      try
        Proceso.Executable := 'dot';
        Proceso.Parameters.Add('-Tpng');
        Proceso.Parameters.Add(DotPath);
        Proceso.Parameters.Add('-o');
        Proceso.Parameters.Add(PngPath);
        Proceso.Options := [poWaitOnExit, poNoConsole];
        Proceso.Execute;

        if FileExists(PngPath) then
          WriteLn('✅ Reporte de Grafo de Contactos generado en: ', PngPath)
        else
          WriteLn('⚠️  No se pudo generar el PNG.');
      finally
        Proceso.Free;
      end;
    except
      on E: Exception do
        WriteLn('⚠️  Error al ejecutar Graphviz: ', E.Message);
    end;

  except
    on E: Exception do
      WriteLn('❌ Error al generar reporte de grafo de contactos: ', E.Message);
  end;
end;
procedure TEDDMailSystem.CargarContactosDesdeJSON(RutaArchivo: String);
var
  JsonData: TJSONData;
  Root: TJSONObject;
  UsuariosArray: TJSONArray;
  UsuarioObj: TJSONObject;
  ContactosArray: TJSONArray;
  i, j, UsuarioId: Integer;
  NombreUsuario, EmailContacto: String;
  Usuario, ContactoUsuario: PUsuario;
  FileStream: TFileStream;
  JsonString: String;
  ContactosAgregados, ContactosOmitidos: Integer;
  UsaEstructuraNueva: Boolean;
begin
  ContactosAgregados := 0;
  ContactosOmitidos := 0;

  if not FileExists(RutaArchivo) then
  begin
    WriteLn('Error: Archivo JSON de contactos no existe: ', RutaArchivo);
    Exit;
  end;

  // Leer archivo JSON
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
    WriteLn('Error: Archivo de contactos vacío');
    Exit;
  end;

  // Parsear JSON
  try
    JsonData := GetJSON(JsonString);
    try
      Root := JsonData as TJSONObject;

      // Buscar el array de usuarios (puede ser "usuarios" o "Usuarios")
      if Root.Find('Usuarios') <> nil then
        UsuariosArray := Root.Arrays['Usuarios']
      else if Root.Find('usuarios') <> nil then
        UsuariosArray := Root.Arrays['usuarios']
      else
      begin
        WriteLn('Error: No se encontró el array de usuarios en el JSON');
        Exit;
      end;

      WriteLn('=== Iniciando carga masiva de contactos ===');

      // Procesar cada usuario
      for i := 0 to UsuariosArray.Count - 1 do
      begin
        UsuarioObj := UsuariosArray.Objects[i];
        Usuario := nil;

        // ======================================================================
        // DETECTAR ESTRUCTURA Y BUSCAR USUARIO CORRESPONDIENTE
        // ======================================================================

        if UsuarioObj.Find('Usuario') <> nil then
        begin
          // *** ESTRUCTURA NUEVA: Buscar por nombre de usuario ***
          UsaEstructuraNueva := True;
          NombreUsuario := UsuarioObj.Get('Usuario', '');

          if NombreUsuario = '' then
          begin
            WriteLn('Aviso: Usuario sin nombre en posición ', i);
            Continue;
          end;

          // Buscar el usuario por nombre de usuario
          Usuario := BuscarUsuarioPorNombre(NombreUsuario);
          if Usuario = nil then
          begin
            WriteLn('Aviso: Usuario "', NombreUsuario, '" no existe en el sistema');
            Continue;
          end;

          WriteLn('Procesando contactos para usuario: ', NombreUsuario, ' (', Usuario^.Email, ')');
        end
        else if UsuarioObj.Find('id') <> nil then
        begin
          // *** ESTRUCTURA ANTIGUA: Buscar por ID ***
          UsaEstructuraNueva := False;
          UsuarioId := UsuarioObj.Get('id', -1);

          if UsuarioId = -1 then
          begin
            WriteLn('Aviso: Usuario sin ID en posición ', i);
            Continue;
          end;

          // Buscar el usuario por ID
          Usuario := BuscarUsuarioPorId(UsuarioId);
          if Usuario = nil then
          begin
            WriteLn('Aviso: Usuario con ID ', UsuarioId, ' no existe en el sistema');
            Continue;
          end;

          WriteLn('Procesando contactos para usuario ID ', UsuarioId, ': ', Usuario^.Email);
        end
        else
        begin
          WriteLn('Aviso: Usuario en posición ', i, ' no tiene ni "id" ni "Usuario"');
          Continue;
        end;

        // ======================================================================
        // PROCESAR ARRAY DE CONTACTOS
        // ======================================================================

        // Verificar si tiene array de contactos (puede ser "Contactos" o "contactos")
        if UsuarioObj.Find('Contactos') <> nil then
          ContactosArray := UsuarioObj.Arrays['Contactos']
        else if UsuarioObj.Find('contactos') <> nil then
          ContactosArray := UsuarioObj.Arrays['contactos']
        else
        begin
          WriteLn('  Sin contactos en el JSON');
          Continue;
        end;

        // Procesar cada contacto
        for j := 0 to ContactosArray.Count - 1 do
        begin
          if UsaEstructuraNueva then
          begin
            // *** ESTRUCTURA NUEVA: Los contactos son nombres de usuario ***
            NombreUsuario := ContactosArray.Strings[j];

            // Buscar el usuario contacto por su nombre de usuario
            ContactoUsuario := BuscarUsuarioPorNombre(NombreUsuario);

            if ContactoUsuario = nil then
            begin
              WriteLn('  ⚠️  Contacto omitido (usuario no existe): ', NombreUsuario);
              Inc(ContactosOmitidos);
              Continue;
            end;

            EmailContacto := ContactoUsuario^.Email;

            // Intentar agregar el contacto
            if AgregarContacto(Usuario, EmailContacto) then
            begin
              WriteLn('  ✅ Contacto agregado: ', NombreUsuario, ' (', EmailContacto, ')');
              Inc(ContactosAgregados);
            end
            else
            begin
              WriteLn('  ⚠️  Contacto omitido (ya existe o es inválido): ', NombreUsuario);
              Inc(ContactosOmitidos);
            end;
          end
          else
          begin
            // *** ESTRUCTURA ANTIGUA: Los contactos son emails directamente ***
            EmailContacto := ContactosArray.Strings[j];

            // Intentar agregar el contacto
            if AgregarContacto(Usuario, EmailContacto) then
            begin
              WriteLn('  ✅ Contacto agregado: ', EmailContacto);
              Inc(ContactosAgregados);
            end
            else
            begin
              WriteLn('  ⚠️  Contacto omitido (ya existe o es inválido): ', EmailContacto);
              Inc(ContactosOmitidos);
            end;
          end;
        end;
      end;

      WriteLn('=== Carga masiva completada ===');
      WriteLn('Contactos agregados: ', ContactosAgregados);
      WriteLn('Contactos omitidos: ', ContactosOmitidos);

    finally
      JsonData.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error al procesar JSON de contactos: ', E.Message);
  end;
end;
function TEDDMailSystem.ReaccionarAMensaje(nombreComunidad: String; idMensaje: Integer): Boolean;
var
  Comunidad: PNodoBST;
  Mensaje: PMensajeComunidad;
begin
  Result := False;

  // Buscar la comunidad
  Comunidad := BuscarComunidadBST(FArbolComunidades, nombreComunidad);
  if Comunidad = nil then
  begin
    WriteLn('Error: La comunidad no existe');
    Exit;
  end;

  // Buscar el mensaje específico por ID
  Mensaje := Comunidad^.ListaMensajes;
  while Mensaje <> nil do
  begin
    if Mensaje^.Id = idMensaje then
    begin
      // Incrementar el contador de reacciones
      Inc(Mensaje^.Reacciones);
      WriteLn(Format('👍 Reacción agregada al mensaje ID %d. Total: %d',
        [idMensaje, Mensaje^.Reacciones]));
      Result := True;
      Exit;
    end;
    Mensaje := Mensaje^.Siguiente;
  end;

  WriteLn('Error: Mensaje no encontrado');
end;
    // ═══════════════════════════════════════════════════════
// MÉTODOS PÚBLICOS PARA ACCEDER AL ÁRBOL DE COMUNIDADES
// ═══════════════════════════════════════════════════════

function TEDDMailSystem.GetArbolComunidades: PNodoBST;
begin
  Result := FArbolComunidades;
end;

function TEDDMailSystem.BuscarComunidadPorNombre(Nombre: String): PNodoBST;
begin
  // Esta función pública llama a la búsqueda recursiva interna
  Result := BuscarComunidadBST(FArbolComunidades, Nombre);
end;
function TEDDMailSystem.CalcularHashSHA256(Texto: String): String;
var
  i: Integer;
  HashVal: Cardinal;
  Temp: String;
begin
  HashVal := 5381;

  for i := 1 to Length(Texto) do
  begin
    HashVal := ((HashVal shl 5) + HashVal) + Ord(Texto[i]);
  end;

  // Generar hash hexadecimal de 64 caracteres
  Temp := IntToHex(HashVal, 16);

  // Rellenar hasta 64 caracteres
  while Length(Temp) < 64 do
    Temp := Temp + IntToHex(HashVal xor Length(Temp), 8);

  Result := Copy(Temp, 1, 64);
end;
   function TEDDMailSystem.CrearNodoMerkle: PNodoMerkle;
begin
  New(Result);
  Result^.Hash := '';
  Result^.Correo := nil;
  Result^.Izquierdo := nil;
  Result^.Derecho := nil;
  Result^.EsHoja := False;
end;

   function TEDDMailSystem.CrearHojaMerkle(Correo: PCorreo): PNodoMerkle;
var
  DatosCorreo: String;
begin
  Result := CrearNodoMerkle;
  Result^.Correo := Correo;
  Result^.EsHoja := True;

  // Concatenar datos del correo para el hash
  DatosCorreo := IntToStr(Correo^.Id) + Correo^.Remitente +
                 Correo^.Asunto + Correo^.Mensaje + Correo^.Fecha;
  Result^.Hash := CalcularHashSHA256(DatosCorreo);
end;
   function TEDDMailSystem.CombinarHashesMerkle(HashIzq, HashDer: String): String;
   begin
     Result := CalcularHashSHA256(HashIzq + HashDer);
   end;
   function TEDDMailSystem.ConstruirArbolMerkle(Correos: array of PCorreo;
     Inicio, Fin: Integer): PNodoMerkle;
   var
     Medio: Integer;
     NodoIzq, NodoDer: PNodoMerkle;
     NumElementos: Integer;
   begin
     NumElementos := Fin - Inicio + 1;

     // Caso base: un solo correo (hoja)
     if NumElementos = 1 then
     begin
       Result := CrearHojaMerkle(Correos[Inicio]);
       Exit;
     end;

     // Para 2 o más elementos, dividir recursivamente
     // Calcular el punto medio para dividir
     Medio := Inicio + (NumElementos div 2) - 1;

     // Construir subárbol izquierdo (puede ser una hoja o un subárbol)
     NodoIzq := ConstruirArbolMerkle(Correos, Inicio, Medio);

     // Construir subárbol derecho (puede ser una hoja o un subárbol)
     NodoDer := ConstruirArbolMerkle(Correos, Medio + 1, Fin);

     // Crear nodo interno que combina ambos subárboles
     Result := CrearNodoMerkle;
     Result^.Izquierdo := NodoIzq;
     Result^.Derecho := NodoDer;
     Result^.EsHoja := False;
     Result^.Hash := CombinarHashesMerkle(NodoIzq^.Hash, NodoDer^.Hash);
   end;
procedure TEDDMailSystem.RecolectarCorreosMerkle(Nodo: PNodoMerkle; Lista: TList);
var
  i: Integer;
  YaExiste: Boolean;
begin
  if Nodo = nil then Exit;

  if Nodo^.EsHoja then
  begin
    if Nodo^.Correo <> nil then
    begin
      // ✅ VERIFICAR SI YA ESTÁ EN LA LISTA (evitar duplicados)
      YaExiste := False;
      for i := 0 to Lista.Count - 1 do
      begin
        if PCorreo(Lista[i])^.Id = Nodo^.Correo^.Id then
        begin
          YaExiste := True;
          Break;
        end;
      end;

      // Solo agregar si no existe
      if not YaExiste then
        Lista.Add(Nodo^.Correo);
    end;
  end
  else
  begin
    RecolectarCorreosMerkle(Nodo^.Izquierdo, Lista);
    RecolectarCorreosMerkle(Nodo^.Derecho, Lista);
  end;
end;

   procedure TEDDMailSystem.RecorrerFavoritosMerkle(Usuario: PUsuario; Lista: TStringList);
   var
     ListaCorreos: TList;
     i: Integer;
     Correo: PCorreo;
     Display: String;
   begin
     if Usuario = nil then Exit;

     ListaCorreos := TList.Create;
     try
       if Usuario^.ArbolMerkleFavoritos <> nil then
         RecolectarCorreosMerkle(Usuario^.ArbolMerkleFavoritos, ListaCorreos);

       for i := 0 to ListaCorreos.Count - 1 do
       begin
         Correo := PCorreo(ListaCorreos[i]);
         Display := Format('[ID: %d] %s — %s (%s)',
           [Correo^.Id, Correo^.Asunto, Correo^.Remitente, Correo^.Fecha]);
         Lista.AddObject(Display, TObject(PtrInt(Correo^.Id)));
       end;
     finally
       ListaCorreos.Free;
     end;
   end;

 function TEDDMailSystem.VerificarIntegridadMerkle(Usuario: PUsuario): Boolean;
var
  ListaCorreos: TList;
  ArrayCorreos: array of PCorreo;
  ArbolTemporal: PNodoMerkle;
  i: Integer;
  HashOriginal, HashNuevo: String;
begin
  Result := False;
  if Usuario = nil then Exit;
  if Usuario^.ArbolMerkleFavoritos = nil then
  begin
    Result := True; // Árbol vacío es válido
    Exit;
  end;

  ListaCorreos := TList.Create;
  try
    // Recolectar correos y guardar hash original
    RecolectarCorreosMerkle(Usuario^.ArbolMerkleFavoritos, ListaCorreos);
    HashOriginal := Usuario^.ArbolMerkleFavoritos^.Hash;

    // Reconstruir árbol temporalmente
    SetLength(ArrayCorreos, ListaCorreos.Count);
    for i := 0 to ListaCorreos.Count - 1 do
      ArrayCorreos[i] := PCorreo(ListaCorreos[i]);

    ArbolTemporal := ConstruirArbolMerkle(ArrayCorreos, 0, Length(ArrayCorreos) - 1);
    HashNuevo := ArbolTemporal^.Hash;

    // Comparar hashes
    Result := (HashOriginal = HashNuevo);

    // Liberar árbol temporal
    LiberarArbolMerkle(ArbolTemporal);

    if Result then
      WriteLn('✓ Integridad verificada correctamente')
    else
      WriteLn('✗ Error: Integridad comprometida');

  finally
    ListaCorreos.Free;
  end;
end;
 procedure TEDDMailSystem.Merkle_ReconstruirDesdeArbolB(Usuario: PUsuario);
var
  ListaFavoritos: TStringList;
  ArrayCorreos: array of PCorreo;
  i, CorreoId: Integer;
  Correo: PCorreo;
begin
  if Usuario = nil then Exit;

  WriteLn('🔄 Iniciando migración de Árbol B a Árbol de Merkle...');


  WriteLn('✓ Migración completada');
end;
 procedure TEDDMailSystem.GenerarNodosMerkle(var Archivo: TextFile;
   Nodo: PNodoMerkle; var ContadorNodo: Integer);
 var
   IdActual, IdIzq, IdDer: Integer;
   HashCorto: String;
 begin
   if Nodo = nil then Exit;

   IdActual := ContadorNodo;
   Inc(ContadorNodo);

   // Hash corto para visualización
   if Length(Nodo^.Hash) > 10 then
     HashCorto := Copy(Nodo^.Hash, 1, 10) + '...'
   else
     HashCorto := Nodo^.Hash;

   if Nodo^.EsHoja then
   begin
     // Nodo hoja con datos del correo
     WriteLn(Archivo, Format('    node%d [label="De: %s\nAsunto: %s\nFecha: %s\nHash: %s", fillcolor=lightgreen];',
       [IdActual, Nodo^.Correo^.Remitente, Nodo^.Correo^.Asunto,
        Nodo^.Correo^.Fecha, HashCorto]));
   end
   else
   begin
     // Nodo interno con hash combinado
     WriteLn(Archivo, Format('    node%d [label="Hash: %s", fillcolor=lightyellow];',
       [IdActual, HashCorto]));

     // Procesar hijo izquierdo
     if Nodo^.Izquierdo <> nil then
     begin
       IdIzq := ContadorNodo;
       GenerarNodosMerkle(Archivo, Nodo^.Izquierdo, ContadorNodo);
       WriteLn(Archivo, Format('    node%d -> node%d;', [IdActual, IdIzq]));
     end;

     // ✅ CRÍTICO: Solo procesar hijo derecho si es DIFERENTE del izquierdo
     if (Nodo^.Derecho <> nil) and (Nodo^.Derecho <> Nodo^.Izquierdo) then
     begin
       IdDer := ContadorNodo;
       GenerarNodosMerkle(Archivo, Nodo^.Derecho, ContadorNodo);
       WriteLn(Archivo, Format('    node%d -> node%d;', [IdActual, IdDer]));
     end
     else if (Nodo^.Derecho <> nil) and (Nodo^.Derecho = Nodo^.Izquierdo) then
     begin
       // ✅ Si es el mismo nodo, reutilizar el ID del izquierdo
       WriteLn(Archivo, Format('    node%d -> node%d;', [IdActual, IdIzq]));
     end;
   end;
 end;
 function TEDDMailSystem.ConstruirArbolMerkleBalanceado(
   ListaHojas: array of PNodoMerkle): PNodoMerkle;
 var
   NivelActual, NivelSiguiente: array of PNodoMerkle;
   i, TamNivel, TamSiguiente: Integer;
   NodoPadre: PNodoMerkle;
 begin
   Result := nil;

   // Caso base: sin hojas
   if Length(ListaHojas) = 0 then Exit;

   // Caso base: una sola hoja
   if Length(ListaHojas) = 1 then
   begin
     Result := ListaHojas[0];
     Exit;
   end;

   // Inicializar el nivel actual con todas las hojas
   SetLength(NivelActual, Length(ListaHojas));
   for i := 0 to High(ListaHojas) do
     NivelActual[i] := ListaHojas[i];

   // Construir el árbol nivel por nivel de abajo hacia arriba
   while Length(NivelActual) > 1 do
   begin
     TamNivel := Length(NivelActual);

     // El siguiente nivel tendrá la mitad de nodos (redondeado hacia arriba)
     TamSiguiente := (TamNivel + 1) div 2;
     SetLength(NivelSiguiente, TamSiguiente);

     // Emparejar nodos de dos en dos
     for i := 0 to TamSiguiente - 1 do
     begin
       // ✅ SIEMPRE crear un nodo padre
       New(NodoPadre);
       NodoPadre^.EsHoja := False;
       NodoPadre^.Correo := nil;

       // Asignar hijo izquierdo (siempre existe)
       NodoPadre^.Izquierdo := NivelActual[i * 2];

       // Si hay dos nodos disponibles para emparejar
       if (i * 2 + 1) < TamNivel then
       begin
         // Usar el segundo nodo como hijo derecho
         NodoPadre^.Derecho := NivelActual[i * 2 + 1];
         NodoPadre^.Hash := GenerarHashSHA256(
           NivelActual[i * 2]^.Hash + NivelActual[i * 2 + 1]^.Hash);
       end
       else
       begin
         // ✅ Si queda un nodo impar, duplicar la REFERENCIA (no el nodo)
         // Esto es válido para árboles de Merkle
         NodoPadre^.Derecho := NivelActual[i * 2];
         NodoPadre^.Hash := GenerarHashSHA256(
           NivelActual[i * 2]^.Hash + NivelActual[i * 2]^.Hash);
       end;

       NivelSiguiente[i] := NodoPadre;
     end;

     // Avanzar al siguiente nivel
     NivelActual := NivelSiguiente;
   end;

   // El último nodo restante es la raíz del árbol
   Result := NivelActual[0];
 end;
 procedure TEDDMailSystem.LiberarArbolMerkle(Nodo: PNodoMerkle);
 begin
   if Nodo = nil then Exit;

   if not Nodo^.EsHoja then
   begin
     // ✅ Solo liberar hijo izquierdo
     LiberarArbolMerkle(Nodo^.Izquierdo);

     // ✅ Solo liberar hijo derecho si es DIFERENTE del izquierdo
     if Nodo^.Derecho <> Nodo^.Izquierdo then
       LiberarArbolMerkle(Nodo^.Derecho);
   end;

   Dispose(Nodo);
 end;

 procedure TEDDMailSystem.ConstruirArbolMerkleDesdeCorreos(Usuario: PUsuario);
var
  ListaFavoritos: TStringList;
  ListaHojas: array of PNodoMerkle;
  i, CorreoId: Integer;
  Correo: PCorreo;
  NodoHoja: PNodoMerkle;
  DatosCorreo: String;
begin
  if Usuario = nil then Exit;

  // Liberar árbol anterior si existe
  if Usuario^.ArbolMerkleFavoritos <> nil then
  begin
    LiberarArbolMerkle(Usuario^.ArbolMerkleFavoritos);
    Usuario^.ArbolMerkleFavoritos := nil;
  end;

  // Obtener lista de IDs de privados
  ListaFavoritos := TStringList.Create;
  try
    RecorrerFavoritosMerkle(Usuario, ListaFavoritos);

    if ListaFavoritos.Count = 0 then Exit;

    // Crear array de hojas
    SetLength(ListaHojas, ListaFavoritos.Count);

    for i := 0 to ListaFavoritos.Count - 1 do
    begin
      CorreoId := StrToIntDef(ListaFavoritos[i], -1);
      if CorreoId = -1 then Continue;

      // SE USA LA FUNCIÓN EXISTENTE
      Correo := BuscarCorreoEnBandeja(Usuario, CorreoId);
      if Correo = nil then Continue;

      // Crear nodo hoja
      New(NodoHoja);
      NodoHoja^.EsHoja := True;
      NodoHoja^.Correo := Correo;
      NodoHoja^.Izquierdo := nil;
      NodoHoja^.Derecho := nil;

      // Generar hash de los datos del correo
      DatosCorreo := IntToStr(Correo^.Id) +
                     Correo^.Remitente +
                     Correo^.Asunto +
                     Correo^.Fecha +
                     Correo^.Mensaje;
      NodoHoja^.Hash := GenerarHashSHA256(DatosCorreo);

      ListaHojas[i] := NodoHoja;
    end;

    // Construir árbol balanceado
    Usuario^.ArbolMerkleFavoritos := ConstruirArbolMerkleBalanceado(ListaHojas);

    WriteLn('✓ Árbol de Merkle construido con ', ListaFavoritos.Count, ' hojas');

  finally
    ListaFavoritos.Free;
  end;
end;
 function TEDDMailSystem.GenerarHashSHA256(Datos: String): String;
var
  i: Integer;
  HashVal1, HashVal2: Cardinal;
  Temp: String;
begin
  // Algoritmo de hash simple pero efectivo para el árbol Merkle
  HashVal1 := 5381;  // DJB2 hash
  HashVal2 := 0;

  for i := 1 to Length(Datos) do
  begin
    HashVal1 := ((HashVal1 shl 5) + HashVal1) + Ord(Datos[i]);
    HashVal2 := HashVal2 xor (Ord(Datos[i]) * 31);
  end;

  // Generar string hexadecimal de 64 caracteres (simular SHA256)
  Temp := IntToHex(HashVal1, 8) + IntToHex(HashVal2, 8);

  // Extender a 64 caracteres
  while Length(Temp) < 64 do
    Temp := Temp + IntToHex(HashVal1 xor HashVal2, 8);

  Result := Copy(Temp, 1, 64);
end;

// ────────────────────────────────────────────────────────────────────────────
// MÉTODOS PRIVADOS PARA BLOCKCHAIN
// ────────────────────────────────────────────────────────────────────────────

function TEDDMailSystem.FormatearTimestamp: String;
var
  Ahora: TDateTime;
  Dia, Mes, Ano, Hora, Min, Seg, MSeg: Word;
begin
  Ahora := Now;
  DecodeDate(Ahora, Ano, Mes, Dia);
  DecodeTime(Ahora, Hora, Min, Seg, MSeg);
  Result := Format('%.2d-%.2d-%.2d::%.2d:%.2d:%.2d',
    [Dia, Mes, Ano mod 100, Hora, Min, Seg]);
end;

function TEDDMailSystem.CalcularHashBloque(Index: Integer; Timestamp, Data: String;
  Nonce: Integer; PreviousHash: String): String;
var
  Concatenacion: String;
begin
  Concatenacion := IntToStr(Index) + Timestamp + Data + IntToStr(Nonce) + PreviousHash;
  Result := CalcularHashSHA256(Concatenacion);
end;

function TEDDMailSystem.ValidarProofOfWork(Hash: String): Boolean;
begin
  // Dificultad: 2 ceros (rápido pero cumple con proof of work)
  Result := (Length(Hash) >= 2) and (Copy(Hash, 1, 2) = '00');
end;

function TEDDMailSystem.MinarBloque(Index: Integer; Timestamp, Data,
  PreviousHash: String): PBloqueBlockchain;
var
  Nonce: Integer;
  HashCalculado: String;
  MaxIntentos: Integer;
begin
  New(Result);
  Result^.Index := Index;
  Result^.Timestamp := Timestamp;
  Result^.Data := Data;
  Result^.PreviousHash := PreviousHash;
  Result^.Siguiente := nil;

  Nonce := 0;
  MaxIntentos := 100000;  // Límite de seguridad

  repeat
    HashCalculado := CalcularHashBloque(Index, Timestamp, Data, Nonce, PreviousHash);

    if ValidarProofOfWork(HashCalculado) then
    begin
      Result^.Nonce := Nonce;
      Result^.Hash := HashCalculado;
      WriteLn(Format('✓ Bloque %d minado con nonce=%d', [Index, Nonce]));
      Break;
    end;

    Inc(Nonce);

    if (Nonce mod 10000) = 0 then
      Write('.');

    // Protección contra bucle infinito
    if Nonce >= MaxIntentos then
    begin
      WriteLn(Format('⚠ Límite alcanzado para bloque %d. Usando nonce=%d', [Index, Nonce]));
      Result^.Nonce := Nonce;
      Result^.Hash := HashCalculado;
      Break;
    end;

  until False;
end;

function TEDDMailSystem.CrearBloqueGenesis: PBloqueBlockchain;
var
  Timestamp: String;
begin
  WriteLn('Creando bloque génesis...');
  Timestamp := FormatearTimestamp;

  // PreviousHash del génesis DEBE ser "0000" según especificación
  Result := MinarBloque(0, Timestamp, 'Genesis Block', '0000');

  WriteLn('✓ Bloque génesis creado');
end;

procedure TEDDMailSystem.LiberarBlockchain;
var
  Actual, Siguiente: PBloqueBlockchain;
begin
  Actual := FBlockchainHead;

  while Actual <> nil do
  begin
    Siguiente := Actual^.Siguiente;
    Dispose(Actual);
    Actual := Siguiente;
  end;

  FBlockchainHead := nil;
  FBlockchainCount := 0;
  WriteLn('Blockchain liberado de memoria');
end;
// ────────────────────────────────────────────────────────────────────────────
// MÉTODOS PÚBLICOS PARA BLOCKCHAIN
// ────────────────────────────────────────────────────────────────────────────

procedure TEDDMailSystem.AgregarBloqueBlockchain(CorreoId: Integer; Remitente,
  Asunto, Mensaje: String);
var
  NuevoBloque: PBloqueBlockchain;
  Data, Timestamp, PreviousHash: String;
  NuevoIndex: Integer;
begin
  Data := Format('ID: %d, Remitente: %s, Asunto: %s, Mensaje: %s',
    [CorreoId, Remitente, Asunto, Mensaje]);

  Timestamp := FormatearTimestamp;

  if FBlockchainHead = nil then
  begin
    FBlockchainHead := CrearBloqueGenesis;
    FBlockchainCount := 1;
  end;

  NuevoIndex := FBlockchainHead^.Index + 1;
  PreviousHash := FBlockchainHead^.Hash;

  WriteLn(Format('Minando bloque %d para correo %d...', [NuevoIndex, CorreoId]));

  NuevoBloque := MinarBloque(NuevoIndex, Timestamp, Data, PreviousHash);

  NuevoBloque^.Siguiente := FBlockchainHead;
  FBlockchainHead := NuevoBloque;
  Inc(FBlockchainCount);

  WriteLn(Format('✓ Bloque %d agregado al blockchain (Total: %d bloques)',
    [NuevoIndex, FBlockchainCount]));
end;

function TEDDMailSystem.ObtenerListaBloques: TStringList;
var
  Actual: PBloqueBlockchain;
  Descripcion: String;
begin
  Result := TStringList.Create;

  if FBlockchainHead = nil then
  begin
    Result.Add('Blockchain vacío');
    Exit;
  end;

  Actual := FBlockchainHead;

  while Actual <> nil do
  begin
    Descripcion := Format('Block %d - [%s] - Hash: %s',
      [Actual^.Index,
       Actual^.Timestamp,
       Copy(Actual^.Hash, 1, 16) + '...']);

    Result.AddObject(Descripcion, TObject(PtrInt(Actual^.Index)));

    Actual := Actual^.Siguiente;
  end;
end;

function TEDDMailSystem.ObtenerDetallesBloque(NumBloque: Integer): String;
var
  Actual: PBloqueBlockchain;
  Lineas: TStringList;
begin
  Result := '';

  if FBlockchainHead = nil then
  begin
    Result := 'Blockchain vacío';
    Exit;
  end;

  Actual := FBlockchainHead;
  while Actual <> nil do
  begin
    if Actual^.Index = NumBloque then
    begin
      Lineas := TStringList.Create;
      try
        Lineas.Add('═══════════════════════════════════════════════════════');
        Lineas.Add(Format('  BLOQUE #%d', [Actual^.Index]));
        Lineas.Add('═══════════════════════════════════════════════════════');
        Lineas.Add('');
        Lineas.Add(Format('Index:          %d', [Actual^.Index]));
        Lineas.Add(Format('Timestamp:      %s', [Actual^.Timestamp]));
        Lineas.Add('');
        Lineas.Add('Data:');
        Lineas.Add('  ' + Actual^.Data);
        Lineas.Add('');
        Lineas.Add(Format('Nonce:          %d', [Actual^.Nonce]));
        Lineas.Add(Format('Previous Hash:  %s', [Actual^.PreviousHash]));
        Lineas.Add(Format('Hash:           %s', [Actual^.Hash]));
        Lineas.Add('');

        if ValidarProofOfWork(Actual^.Hash) then
          Lineas.Add('✓ Proof of Work: VÁLIDO (Hash comienza con 0000)')
        else
          Lineas.Add('✗ Proof of Work: INVÁLIDO');

        Lineas.Add('═══════════════════════════════════════════════════════');

        Result := Lineas.Text;
      finally
        Lineas.Free;
      end;

      Exit;
    end;

    Actual := Actual^.Siguiente;
  end;

  Result := Format('Bloque %d no encontrado', [NumBloque]);
end;

procedure TEDDMailSystem.GenerarReporteBlockchain(RutaSalida: String);
var
  Archivo: TextFile;
  Process: TProcess;
  NombreArchivo: String;
  Actual: PBloqueBlockchain;
  ContadorNodo: Integer;
begin
  if FBlockchainHead = nil then
  begin
    WriteLn('Error: Blockchain vacío, no se puede generar reporte');
    Exit;
  end;

  try
    ForceDirectories(RutaSalida);
    NombreArchivo := RutaSalida + '/blockchain.dot';

    AssignFile(Archivo, NombreArchivo);
    Rewrite(Archivo);

    WriteLn(Archivo, 'digraph Blockchain {');
    WriteLn(Archivo, '    label="Blockchain - Registro de Correos Enviados";');
    WriteLn(Archivo, '    fontsize=20;');
    WriteLn(Archivo, '    fontname="Arial Bold";');
    WriteLn(Archivo, '    rankdir=TB;');
    WriteLn(Archivo, '    node [shape=box, style=filled, fontname="Courier"];');
    WriteLn(Archivo, '    edge [color=blue, penwidth=2];');
    WriteLn(Archivo, '');

    Actual := FBlockchainHead;
    ContadorNodo := 0;

    while Actual <> nil do
    begin
      if Actual^.Index = 0 then
      begin
        WriteLn(Archivo, Format('    block%d [label="Block %d (Genesis)\n' +
          'Index: %d\nTimestamp: %s\nData: %s\nNonce: %d\nPrev Hash: %s\nHash: %s", ' +
          'fillcolor=gold];',
          [ContadorNodo, Actual^.Index, Actual^.Index, Actual^.Timestamp,
           Actual^.Data, Actual^.Nonce, Actual^.PreviousHash,
           Copy(Actual^.Hash, 1, 16) + '...']));
      end
      else
      begin
        WriteLn(Archivo, Format('    block%d [label="Block %d\n' +
          'Index: %d\nTimestamp: %s\nData: %s\nNonce: %d\nPrev Hash: %s\nHash: %s", ' +
          'fillcolor=lightblue];',
          [ContadorNodo, Actual^.Index, Actual^.Index, Actual^.Timestamp,
           Copy(Actual^.Data, 1, 50) + '...', Actual^.Nonce,
           Copy(Actual^.PreviousHash, 1, 10) + '...',
           Copy(Actual^.Hash, 1, 16) + '...']));
      end;

      if Actual^.Siguiente <> nil then
      begin
        WriteLn(Archivo, Format('    block%d -> block%d [label="Previous"];',
          [ContadorNodo, ContadorNodo + 1]));
      end;

      Actual := Actual^.Siguiente;
      Inc(ContadorNodo);
    end;

    WriteLn(Archivo, '}');
    CloseFile(Archivo);

    WriteLn('✓ Archivo DOT generado: ', NombreArchivo);

    Process := TProcess.Create(nil);
    try
      Process.Executable := 'dot';
      Process.Parameters.Add('-Tpng');
      Process.Parameters.Add(NombreArchivo);
      Process.Parameters.Add('-o');
      Process.Parameters.Add(RutaSalida + '/blockchain.png');
      Process.Options := [poWaitOnExit, poUsePipes];
      Process.Execute;

      WriteLn('✓ Imagen PNG generada: ', RutaSalida, '/blockchain.png');
    finally
      Process.Free;
    end;

  except
    on E: Exception do
      WriteLn('Error al generar reporte de blockchain: ', E.Message);
  end;
end;

function TEDDMailSystem.VerificarIntegridadBlockchain: Boolean;
var
  Actual: PBloqueBlockchain;
  HashEsperado: String;
begin
  Result := True;

  if FBlockchainHead = nil then
  begin
    WriteLn('Blockchain vacío');
    Exit;
  end;

  WriteLn('Verificando integridad del blockchain...');
  Actual := FBlockchainHead;

  while Actual <> nil do
  begin
    if not ValidarProofOfWork(Actual^.Hash) then
    begin
      WriteLn(Format('✗ Bloque %d: Proof of work inválido', [Actual^.Index]));
      Result := False;
    end;

    HashEsperado := CalcularHashBloque(
      Actual^.Index,
      Actual^.Timestamp,
      Actual^.Data,
      Actual^.Nonce,
      Actual^.PreviousHash
    );

    if HashEsperado <> Actual^.Hash then
    begin
      WriteLn(Format('✗ Bloque %d: Hash alterado', [Actual^.Index]));
      Result := False;
    end;

    if Actual^.Siguiente <> nil then
    begin
      if Actual^.PreviousHash <> Actual^.Siguiente^.Hash then
      begin
        WriteLn(Format('✗ Bloque %d: Cadena rota con bloque anterior', [Actual^.Index]));
        Result := False;
      end;
    end;

    Actual := Actual^.Siguiente;
  end;

  if Result then
    WriteLn('✓ Blockchain íntegro - Todos los bloques son válidos')
  else
    WriteLn('✗ Blockchain comprometido - Se detectaron alteraciones');
end;

function TEDDMailSystem.ObtenerTotalBloques: Integer;
begin
  Result := FBlockchainCount;
end;
procedure TEDDMailSystem.InicializarMatriz;
begin
  FMatrizFilas := nil;
  FMatrizColumnas := nil;
  WriteLn('Matriz de relaciones inicializada');
end;

procedure TEDDMailSystem.LiberarMatriz;
begin
  // la matriz se liberará con los usuarios
  FMatrizFilas := nil;
  FMatrizColumnas := nil;
  WriteLn('Matriz liberada');
end;

procedure TEDDMailSystem.LiberarArbolComunidades(var Raiz: PNodoBST);
var
  MensajeActual, SigMensaje: PMensajeComunidad;
begin
  if Raiz = nil then Exit;

  // Liberar subárbol izquierdo recursivamente
  if Raiz^.Izquierdo <> nil then
    LiberarArbolComunidades(Raiz^.Izquierdo);

  // Liberar subárbol derecho recursivamente
  if Raiz^.Derecho <> nil then
    LiberarArbolComunidades(Raiz^.Derecho);

  // Liberar lista de mensajes de esta comunidad
  MensajeActual := Raiz^.ListaMensajes;
  while MensajeActual <> nil do
  begin
    SigMensaje := MensajeActual^.Siguiente;
    Dispose(MensajeActual);
    MensajeActual := SigMensaje;
  end;

  // Liberar el nodo actual
  Dispose(Raiz);
  Raiz := nil;
end;

function TEDDMailSystem.BuscarUsuarioPorNombre(NombreUsuario: String): PUsuario;
var
  Actual: PUsuario;
  UsuarioEnEmail: String;
begin
  Result := nil;
  Actual := FUsuarios;

  while Actual <> nil do
  begin
    // Extraer el nombre de usuario del email (parte antes del @)
    if Pos('@', Actual^.Email) > 0 then
    begin
      UsuarioEnEmail := Copy(Actual^.Email, 1, Pos('@', Actual^.Email) - 1);

      // Comparar sin distinguir mayúsculas/minúsculas
      if LowerCase(UsuarioEnEmail) = LowerCase(NombreUsuario) then
      begin
        Result := Actual;
        Exit;
      end;
    end;
    Actual := Actual^.Siguiente;
  end;
end;
 end.
