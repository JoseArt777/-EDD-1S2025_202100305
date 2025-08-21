unit FileManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SystemCore;

// Funciones de gestión de archivos
function SaveToFile(FileName, Content: String): Boolean;
function LoadFromFile(FileName: String): String;
function CreateDirectory(DirName: String): Boolean;
function FileExistsInPath(FileName: String): Boolean;
function GetUserReportsPath(UserEmail: String): String;
function GetRootReportsPath: String;
function SaveUserReport(UserEmail, ReportName, Content: String): Boolean;
function SaveRootReport(ReportName, Content: String): Boolean;

implementation

function SaveToFile(FileName, Content: String): Boolean;
var
  OutputFile: TextFile;
begin
  Result := False;

  try
    AssignFile(OutputFile, FileName);
    Rewrite(OutputFile);
    Write(OutputFile, Content);
    CloseFile(OutputFile);

    WriteLn('Archivo guardado exitosamente: ', FileName);
    Result := True;

  except
    on E: Exception do
    begin
      WriteLn('Error al guardar archivo ', FileName, ': ', E.Message);
    end;
  end;
end;

function LoadFromFile(FileName: String): String;
var
  InputFile: TextFile;
  Line: String;
  Content: String;
begin
  Content := '';

  if not FileExists(FileName) then
  begin
    WriteLn('Error: El archivo ', FileName, ' no existe');
    Result := '';
    Exit;
  end;

  try
    AssignFile(InputFile, FileName);
    Reset(InputFile);

    while not EOF(InputFile) do
    begin
      ReadLn(InputFile, Line);
      Content := Content + Line + LineEnding;
    end;

    CloseFile(InputFile);
    Result := Content;

  except
    on E: Exception do
    begin
      WriteLn('Error al leer archivo ', FileName, ': ', E.Message);
      Result := '';
    end;
  end;
end;

function CreateDirectory(DirName: String): Boolean;
begin
  Result := False;

  try
    if not DirectoryExists(DirName) then
    begin
      if CreateDir(DirName) then
      begin
        WriteLn('Directorio creado: ', DirName);
        Result := True;
      end
      else
      begin
        WriteLn('Error al crear directorio: ', DirName);
      end;
    end
    else
    begin
      WriteLn('El directorio ya existe: ', DirName);
      Result := True;
    end;

  except
    on E: Exception do
    begin
      WriteLn('Error al crear directorio ', DirName, ': ', E.Message);
    end;
  end;
end;

function FileExistsInPath(FileName: String): Boolean;
begin
  Result := FileExists(FileName);
end;

function GetUserReportsPath(UserEmail: String): String;
var
  UserName: String;
  DotPos: Integer;
begin
  // Extraer el nombre del usuario del email (parte antes del @)
  DotPos := Pos('@', UserEmail);
  if DotPos > 0 then
    UserName := Copy(UserEmail, 1, DotPos - 1)
  else
    UserName := UserEmail;

  Result := UserName + '-Reportes' + DirectorySeparator;
end;

function GetRootReportsPath: String;
begin
  Result := 'Root-Reportes' + DirectorySeparator;
end;

function SaveUserReport(UserEmail, ReportName, Content: String): Boolean;
var
  ReportsPath: String;
  FullPath: String;
begin
  Result := False;

  ReportsPath := GetUserReportsPath(UserEmail);

  // Crear directorio si no existe
  if not CreateDirectory(ReportsPath) then
  begin
    WriteLn('Error: No se pudo crear el directorio de reportes');
    Exit;
  end;

  FullPath := ReportsPath + ReportName + '.txt';
  Result := SaveToFile(FullPath, Content);
end;

function SaveRootReport(ReportName, Content: String): Boolean;
var
  ReportsPath: String;
  FullPath: String;
begin
  Result := False;

  ReportsPath := GetRootReportsPath;

  // Crear directorio si no existe
  if not CreateDirectory(ReportsPath) then
  begin
    WriteLn('Error: No se pudo crear el directorio de reportes del root');
    Exit;
  end;

  FullPath := ReportsPath + ReportName + '.txt';
  Result := SaveToFile(FullPath, Content);
end;

end.

