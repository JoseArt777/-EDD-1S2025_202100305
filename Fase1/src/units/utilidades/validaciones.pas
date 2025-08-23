unit validaciones;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, StrUtils,
  edd_types,
  lista_simple, lista_circular;

function TrimLower(const S: AnsiString): AnsiString;
function EsEmailValido(const Email: AnsiString): Boolean;
function ExisteUsuarioPorEmail(var Usuarios: TListaUsuarios; const Email: AnsiString): Boolean;
function PuedeEnviarAContacto(var Contactos: TListaCircularContactos; const EmailDestino: AnsiString): Boolean;

implementation

function TrimLower(const S: AnsiString): AnsiString;
begin
  Result := LowerCase(Trim(S));
end;

// Validación simple (sin regex): contiene '@' y un punto luego del '@'
function EsEmailValido(const Email: AnsiString): Boolean;
var atPos, dotPos: SizeInt;
begin
  Result := False;
  if Email = '' then Exit;
  atPos := Pos('@', Email);
  if atPos <= 1 then Exit; // no al inicio y debe existir
  dotPos := PosEx('.', Email, atPos + 2);
  if dotPos = 0 then Exit;
  // no espacios
  if Pos(' ', Email) > 0 then Exit;
  Result := True;
end;

function ExisteUsuarioPorEmail(var Usuarios: TListaUsuarios; const Email: AnsiString): Boolean;
begin
  Result := LS_FindByEmail(Usuarios, Email) <> nil;
end;

function PuedeEnviarAContacto(var Contactos: TListaCircularContactos; const EmailDestino: AnsiString): Boolean;
begin
  Result := LC_Find(Contactos, EmailDestino) <> nil;
end;

end.
