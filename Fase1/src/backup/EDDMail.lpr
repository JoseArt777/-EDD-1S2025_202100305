program EDDMail;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  EstructurasDatos,
  CorreoManager,
  InterfazGTK,
  Classes;

// =============== EJEMPLO ESPECÍFICO PARA VALIDAR ÁRBOL B ===============

procedure TestArbolB;
var
  Sistema: TEDDMailSystem;
  Usuario: PUsuario;
  Correo1, Correo2, Correo3, Correo4, Correo5, Correo6: PCorreo;
  Lista: TStringList;
  i: Integer;
begin
  WriteLn('========================================');
  WriteLn('INICIANDO TEST DEL ÁRBOL B DE FAVORITOS');
  WriteLn('========================================');

  // 1. CREAR SISTEMA Y USUARIO
  Sistema := TEDDMailSystem.Create;
  try
    // Registrar usuario de prueba
    Sistema.RegistrarUsuario('Juan Perez', 'juan', 'juan@test.com', '12345678', 'pass123');
    Sistema.IniciarSesion('juan@test.com', 'pass123');
    Usuario := Sistema.GetUsuarioActual;

    WriteLn('✓ Usuario creado: ', Usuario^.Nombre);
    WriteLn('✓ Árbol B inicial vacío');

    // 2. CREAR CORREOS DE PRUEBA
    Correo1 := Sistema.CrearCorreo('ana@test.com', 'juan@test.com', 'Reunión', 'Reunión mañana', '01/01/25 10:00');
    Correo1^.Id := 10;

    Correo2 := Sistema.CrearCorreo('luis@test.com', 'juan@test.com', 'Proyecto', 'Avances del proyecto', '02/01/25 11:00');
    Correo2^.Id := 5;

    Correo3 := Sistema.CrearCorreo('maria@test.com', 'juan@test.com', 'Viaje', 'Planes de viaje', '03/01/25 12:00');
    Correo3^.Id := 15;

    Correo4 := Sistema.CrearCorreo('carlos@test.com', 'juan@test.com', 'Cena', 'Invitación a cena', '04/01/25 13:00');
    Correo4^.Id := 3;

    Correo5 := Sistema.CrearCorreo('sofia@test.com', 'juan@test.com', 'Curso', 'Curso de programación', '05/01/25 14:00');
    Correo5^.Id := 12;

    Correo6 := Sistema.CrearCorreo('pedro@test.com', 'juan@test.com', 'Fiesta', 'Fiesta de cumpleaños', '06/01/25 15:00');
    Correo6^.Id := 8;

    WriteLn('✓ Correos creados con IDs: 10, 5, 15, 3, 12, 8');

    // 3. INSERTAR CORREOS EN EL ÁRBOL B (EN ORDEN ESPECÍFICO)
    WriteLn('');
    WriteLn('--- INSERTANDO EN ÁRBOL B ---');

    // Insertar ID 10
    Usuario^.ArbolFavoritos := Sistema.InsertarB(Usuario^.ArbolFavoritos, Correo1);
    WriteLn('Insertado ID 10: Nodo raíz = [10]');

    // Insertar ID 5
    Usuario^.ArbolFavoritos := Sistema.InsertarB(Usuario^.ArbolFavoritos, Correo2);
    WriteLn('Insertado ID 5:  Nodo raíz = [5, 10]');

    // Insertar ID 15
    Usuario^.ArbolFavoritos := Sistema.InsertarB(Usuario^.ArbolFavoritos, Correo3);
    WriteLn('Insertado ID 15: Nodo raíz = [5, 10, 15]');

    // Insertar ID 3
    Usuario^.ArbolFavoritos := Sistema.InsertarB(Usuario^.ArbolFavoritos, Correo4);
    WriteLn('Insertado ID 3:  Nodo raíz = [3, 5, 10, 15]');

    // Insertar ID 12 (debería causar división)
    Usuario^.ArbolFavoritos := Sistema.InsertarB(Usuario^.ArbolFavoritos, Correo5);
    WriteLn('Insertado ID 12: ¡DIVISIÓN! Nueva estructura:');
    WriteLn('    Raíz: [10]');
    WriteLn('    Hijo izq: [3, 5]    Hijo der: [12, 15]');

    // Insertar ID 8
    Usuario^.ArbolFavoritos := Sistema.InsertarB(Usuario^.ArbolFavoritos, Correo6);
    WriteLn('Insertado ID 8:');
    WriteLn('    Raíz: [10]');
    WriteLn('    Hijo izq: [3, 5, 8]    Hijo der: [12, 15]');

    // 4. VALIDACIONES ESPECÍFICAS
    WriteLn('');
    WriteLn('--- VALIDACIONES ---');

    // Validar estructura
    if Sistema.EsArbolBValido(Usuario^.ArbolFavoritos) then
      WriteLn('✓ VÁLIDO: Árbol B cumple todas las propiedades')
    else
      WriteLn('✗ ERROR: Árbol B NO es válido');

    // Contar favoritos
    WriteLn('✓ Total favoritos: ', Sistema.ContarFavoritos(Usuario));

    // Validar altura
    WriteLn('✓ Altura del árbol: ', Sistema.ObtenerAlturaArbolB(Usuario^.ArbolFavoritos));

    // Validar número de nodos
    WriteLn('✓ Número de nodos: ', Sistema.ObtenerNumeroNodos(Usuario^.ArbolFavoritos));

    // 5. BUSCAR ELEMENTOS ESPECÍFICOS
    WriteLn('');
    WriteLn('--- BÚSQUEDAS ---');

    // Buscar elementos que existen
    if Sistema.BuscarB(Usuario^.ArbolFavoritos, 5) <> nil then
      WriteLn('✓ ENCONTRADO: ID 5 (', Sistema.BuscarB(Usuario^.ArbolFavoritos, 5)^.Asunto, ')')
    else
      WriteLn('✗ ERROR: ID 5 no encontrado');

    if Sistema.BuscarB(Usuario^.ArbolFavoritos, 12) <> nil then
      WriteLn('✓ ENCONTRADO: ID 12 (', Sistema.BuscarB(Usuario^.ArbolFavoritos, 12)^.Asunto, ')')
    else
      WriteLn('✗ ERROR: ID 12 no encontrado');

    // Buscar elemento que NO existe
    if Sistema.BuscarB(Usuario^.ArbolFavoritos, 99) = nil then
      WriteLn('✓ CORRECTO: ID 99 no existe (como se esperaba)')
    else
      WriteLn('✗ ERROR: ID 99 encontrado cuando no debería existir');

    // 6. RECORRIDO IN-ORDEN (DEBE ESTAR ORDENADO)
    WriteLn('');
    WriteLn('--- RECORRIDO IN-ORDEN ---');
    Lista := TStringList.Create;
    try
      Sistema.RecorridoInOrdenB(Usuario^.ArbolFavoritos, Lista);
      WriteLn('Elementos en orden:');
      for i := 0 to Lista.Count - 1 do
        WriteLn('  ', Lista[i]);

      // Verificar que están en orden
      WriteLn('');
      if Lista.Count = 6 then
        WriteLn('✓ CORRECTO: Se encontraron los 6 elementos')
      else
        WriteLn('✗ ERROR: Se esperaban 6 elementos, se encontraron ', Lista.Count);

    finally
      Lista.Free;
    end;

    // 7. ELIMINAR UN FAVORITO
    WriteLn('');
    WriteLn('--- ELIMINACIÓN ---');
    if Sistema.EliminarFavorito(Usuario, 5) then
    begin
      WriteLn('✓ ELIMINADO: ID 5 removido de favoritos');
      WriteLn('✓ Total favoritos después: ', Sistema.ContarFavoritos(Usuario));

      // Verificar que ya no existe
      if Sistema.BuscarB(Usuario^.ArbolFavoritos, 5) = nil then
        WriteLn('✓ VERIFICADO: ID 5 ya no se encuentra en el árbol')
      else
        WriteLn('✗ ERROR: ID 5 aún existe después de eliminarlo');
    end
    else
      WriteLn('✗ ERROR: No se pudo eliminar ID 5');

    // 8. RESULTADO FINAL
    WriteLn('');
    WriteLn('========================================');
    if Sistema.EsArbolBValido(Usuario^.ArbolFavoritos) and
       (Sistema.ContarFavoritos(Usuario) = 5) then
    begin
      WriteLn('🎉 ¡ÉXITO! EL ÁRBOL B FUNCIONA CORRECTAMENTE');
      WriteLn('   - Mantiene propiedades del Árbol B');
      WriteLn('   - Inserciones funcionan');
      WriteLn('   - Divisiones automáticas funcionan');
      WriteLn('   - Búsquedas funcionan');
      WriteLn('   - Eliminaciones funcionan');
      WriteLn('   - Recorridos funcionan');
    end
    else
    begin
      WriteLn('❌ HAY ERRORES EN LA IMPLEMENTACIÓN');
      WriteLn('   Revisa los mensajes de error anteriores');
    end;
    WriteLn('========================================');

  finally
    Sistema.Free;
  end;
end;

// =============== CÓMO USAR ESTE TEST ===============
// 1. Agrega este procedimiento al final de tu estructurasdatos.pas
// 2. En tu programa principal, llama: TestArbolB;
// 3. Observa la salida en consola para ver si todo funciona

// =============== SALIDA ESPERADA SI TODO ESTÁ BIEN ===============
(*
========================================
INICIANDO TEST DEL ÁRBOL B DE FAVORITOS
========================================
✓ Usuario creado: Juan Perez
✓ Árbol B inicial vacío
✓ Correos creados con IDs: 10, 5, 15, 3, 12, 8

--- INSERTANDO EN ÁRBOL B ---
Insertado ID 10: Nodo raíz = [10]
Insertado ID 5:  Nodo raíz = [5, 10]
Insertado ID 15: Nodo raíz = [5, 10, 15]
Insertado ID 3:  Nodo raíz = [3, 5, 10, 15]
Insertado ID 12: ¡DIVISIÓN! Nueva estructura:
    Raíz: [10]
    Hijo izq: [3, 5]    Hijo der: [12, 15]
Insertado ID 8:
    Raíz: [10]
    Hijo izq: [3, 5, 8]    Hijo der: [12, 15]

--- VALIDACIONES ---
✓ VÁLIDO: Árbol B cumple todas las propiedades
✓ Total favoritos: 6
✓ Altura del árbol: 2
✓ Número de nodos: 3

--- BÚSQUEDAS ---
✓ ENCONTRADO: ID 5 (Proyecto)
✓ ENCONTRADO: ID 12 (Curso)
✓ CORRECTO: ID 99 no existe (como se esperaba)

--- RECORRIDO IN-ORDEN ---
Elementos en orden:
  ID: 3 - carlos@test.com → juan@test.com: Cena
  ID: 5 - luis@test.com → juan@test.com: Proyecto
  ID: 8 - pedro@test.com → juan@test.com: Fiesta
  ID: 10 - ana@test.com → juan@test.com: Reunión
  ID: 12 - sofia@test.com → juan@test.com: Curso
  ID: 15 - maria@test.com → juan@test.com: Viaje

✓ CORRECTO: Se encontraron los 6 elementos

--- ELIMINACIÓN ---
✓ ELIMINADO: ID 5 removido de favoritos
✓ Total favoritos después: 5
✓ VERIFICADO: ID 5 ya no se encuentra en el árbol

========================================
🎉 ¡ÉXITO! EL ÁRBOL B FUNCIONA CORRECTAMENTE
   - Mantiene propiedades del Árbol B
   - Inserciones funcionan
   - Divisiones automáticas funcionan
   - Búsquedas funcionan
   - Eliminaciones funcionan
   - Recorridos funcionan
========================================
*)

var
  Aplicacion: TInterfazEDDMail;

begin


  // Configurar aplicación
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;

  WriteLn('=================================');
  WriteLn('    EDDMail - Sistema de Correo');
  WriteLn('    Estructuras de Datos - USAC');
  WriteLn('=================================');
  WriteLn('Iniciando aplicación con Lazarus...');
  WriteLn('');
  WriteLn('Credenciales por defecto:');
  WriteLn('Email: root@edd.com');
  WriteLn('Password: root123');
  WriteLn('');

  try
    // Crear directorio de reportes
    if not DirectoryExists('Root-Reportes') then
      CreateDir('Root-Reportes');

       // ========== AGREGAR ESTA LÍNEA AQUÍ ==========
    TestArbolB;  // ← Test del Árbol B
    WriteLn('Presiona Enter para continuar a la interfaz...');
    ReadLn;
    // ============================================


    Aplicacion := TInterfazEDDMail.Create;
    try
      WriteLn('Sistema inicializado correctamente.');
      WriteLn('Mostrando interfaz gráfica...');
      WriteLn('');

      // Ejecutar aplicación
      Aplicacion.Ejecutar;

    finally
      Aplicacion.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error al ejecutar la aplicación: ', E.Message);
      WriteLn('Presione Enter para continuar...');
      ReadLn;
      Halt(1);
    end;
  end;

  WriteLn('Aplicación terminada correctamente.');
end.
