unit BasicOperations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

// Funciones de utilidad general
function ValidateEmail(Email: String): Boolean;
function ValidatePassword(Password: String): Boolean;
function HashPassword(Password: String): String;
function FormatMyDateTime(DateTime: TDateTime): String;  // CAMBIADO: Renombrado para evitar conflicto
function ParseMyDateTime(DateTimeStr: String): TDateTime;  // CAMBIADO: Renombrado para evitar conflicto
function GenerateId: Integer;
function SanitizeFileName(FileName: String): String;
function IsValidDate(DateStr: String): Boolean;
function StringToLower(Str: String): String;
function StringToUpper(Str: String): String;
function TrimString(Str: String): String;

implementation

var
  LastGeneratedId: Integer = 0;

function ValidateEmail(Email: String): Boolean;
var
  AtPos, DotPos: Integer;
begin
  Result := False;

  // Verificar longitud mínima
  if Length(Email) < 5 then
    Exit;

  // Buscar @ y .
  AtPos := Pos('@', Email);
  if AtPos <= 1 then // @ debe estar después del primer carácter
    Exit;

  DotPos := Pos('.', Email);
  if DotPos <= AtPos + 1 then // . debe estar después de @
    Exit;

  // Verificar que hay caracteres después del último .
  if DotPos >= Length(Email) then
    Exit;

  Result := True;
end;

function ValidatePassword(Password: String): Boolean;
begin
  // Validar longitud mínima
  Result := Length(Password) >= 6;
end;

function HashPassword(Password: String): String;
var
  i: Integer;
  HashValue: Integer;
begin
  // Implementación simple de hash (no para producción)
  HashValue := 0;
  for i := 1 to Length(Password) do
  begin
    HashValue := HashValue + Ord(Password[i]) * i;
  end;

  Result := IntToStr(HashValue);
end;

function FormatMyDateTime(DateTime: TDateTime): String;
begin
  // CORREGIDO: Usar explícitamente SysUtils.FormatDateTime
  Result := SysUtils.FormatDateTime('dd/mm/yyyy hh:nn:ss', DateTime);
end;

function ParseMyDateTime(DateTimeStr: String): TDateTime;
begin
  try
    // CORREGIDO: Usar TryStrToDateTime para mejor manejo de errores
    if not TryStrToDateTime(DateTimeStr, Result) then
      Result := 0; // Fecha inválida
  except
    Result := 0; // Fecha inválida
  end;
end;

function GenerateId: Integer;
begin
  Inc(LastGeneratedId);
  Result := LastGeneratedId;
end;

function SanitizeFileName(FileName: String): String;
var
  i: Integer;
  InvalidChars: String;
begin
  Result := FileName;
  InvalidChars := '<>:"/\|?*';

  // Reemplazar caracteres inválidos con _
  for i := 1 to Length(InvalidChars) do
  begin
    Result := StringReplace(Result, InvalidChars[i], '_', [rfReplaceAll]);
  end;

  // Remover espacios al inicio y final
  Result := Trim(Result);
end;

function IsValidDate(DateStr: String): Boolean;
var
  TestDate: TDateTime;
begin
  try
    // CORREGIDO: Usar TryStrToDate para mejor manejo de errores
    Result := TryStrToDate(DateStr, TestDate);
  except
    Result := False;
  end;
end;

function StringToLower(Str: String): String;
begin
  Result := LowerCase(Str);
end;

function StringToUpper(Str: String): String;
begin
  Result := UpperCase(Str);
end;

function TrimString(Str: String): String;
begin
  Result := Trim(Str);
end;

end.
