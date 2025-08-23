unit reportes_gen;
{$mode objfpc}{$H+}
interface
uses
  SysUtils,
  edd_types,
  lista_simple, lista_doble, lista_circular, pila, cola, matriz_dispersa;

// Helpers
procedure AsegurarCarpeta(const Ruta: AnsiString);
function  JoinPath(const A, B: AnsiString): AnsiString;

// Reportes Root
procedure ReporteUsuariosRoot(const Usuarios: TListaUsuarios; const CarpetaRoot: AnsiString);
procedure ReporteRelacionesRoot(const Mat: TMatrizDispersa; const CarpetaRoot: AnsiString);

// Reportes por usuario
procedure ReporteBandejaUsuario(const Bandeja: TListaDobleCorreos; const CarpetaUsuario: AnsiString);
procedure ReportePapeleraUsuario(const Papelera: TPilaPapelera; const CarpetaUsuario: AnsiString);
procedure ReporteProgramadosUsuario(const Programados: TColaProgramados; const CarpetaUsuario: AnsiString);
procedure ReporteContactosUsuario(const Contactos: TListaCircularContactos; const CarpetaUsuario: AnsiString);

implementation

procedure AsegurarCarpeta(const Ruta: AnsiString);
begin
  if (Ruta <> '') and (not DirectoryExists(Ruta)) then
    ForceDirectories(Ruta);
end;

function JoinPath(const A, B: AnsiString): AnsiString;
begin
  if (A = '') then Exit(B);
  if (B = '') then Exit(A);
  if (A[Length(A)] = PathDelim) then
    Result := A + B
  else
    Result := A + PathDelim + B;
end;

// ───────────────────────────
// Reportes ROOT
// ───────────────────────────
procedure ReporteUsuariosRoot(const Usuarios: TListaUsuarios; const CarpetaRoot: AnsiString);
var fileDot: AnsiString;
begin
  AsegurarCarpeta(CarpetaRoot);
  fileDot := JoinPath(CarpetaRoot, 'Usuarios.dot');
  LS_ToDot(Usuarios, fileDot);
end;

procedure ReporteRelacionesRoot(const Mat: TMatrizDispersa; const CarpetaRoot: AnsiString);
var fileDot: AnsiString;
begin
  AsegurarCarpeta(CarpetaRoot);
  fileDot := JoinPath(CarpetaRoot, 'Relaciones.dot');
  MD_ToDot(Mat, fileDot);
end;

// ───────────────────────────
// Reportes por Usuario
// ───────────────────────────
procedure ReporteBandejaUsuario(const Bandeja: TListaDobleCorreos; const CarpetaUsuario: AnsiString);
var fileDot: AnsiString;
begin
  AsegurarCarpeta(CarpetaUsuario);
  fileDot := JoinPath(CarpetaUsuario, 'Bandeja.dot');
  LD_ToDot(Bandeja, fileDot);
end;

procedure ReportePapeleraUsuario(const Papelera: TPilaPapelera; const CarpetaUsuario: AnsiString);
var fileDot: AnsiString;
begin
  AsegurarCarpeta(CarpetaUsuario);
  fileDot := JoinPath(CarpetaUsuario, 'Papelera.dot');
  PP_ToDot(Papelera, fileDot);
end;

procedure ReporteProgramadosUsuario(const Programados: TColaProgramados; const CarpetaUsuario: AnsiString);
var fileDot: AnsiString;
begin
  AsegurarCarpeta(CarpetaUsuario);
  fileDot := JoinPath(CarpetaUsuario, 'Programados.dot');
  Q_ToDot(Programados, fileDot);
end;

procedure ReporteContactosUsuario(const Contactos: TListaCircularContactos; const CarpetaUsuario: AnsiString);
var fileDot: AnsiString;
begin
  AsegurarCarpeta(CarpetaUsuario);
  fileDot := JoinPath(CarpetaUsuario, 'Contactos.dot');
  LC_ToDot(Contactos, fileDot);
end;

end.

