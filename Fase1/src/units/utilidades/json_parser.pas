unit json_parser;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Classes, fpjson, jsonparser,
  edd_types,
  lista_simple; // para insertar usuarios

// Lee usuarios desde un archivo JSON con el formato del enunciado
// y los inserta al final de la lista simple.
//
// Estructura esperada:
// {
//   "usuarios": [
//     {"id":1,"nombre":"...","usuario":"...","email":"...","telefono":"..."},
//     ...
//   ]
// }
//
// Retorna la cantidad de usuarios agregados (sin contar root).
function CargarUsuariosDesdeJSON(const FilePath: AnsiString; var Lista: TListaUsuarios): LongInt;

implementation

function CargarUsuariosDesdeJSON(const FilePath: AnsiString; var Lista: TListaUsuarios): LongInt;
var
  jsonData: TJSONData = nil;
  obj: TJSONObject;
  arr: TJSONArray;
  i: Integer;
  u: TUsuario;
  cnt: LongInt;
  fs: TFileStream = nil;
begin
  Result := 0;
  if not FileExists(FilePath) then
    Exit;
  try
    fs := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyNone);
    // En FPC 3.2.2 podemos usar GetJSON(stream)
    jsonData := GetJSON(fs);
    if (jsonData <> nil) and (jsonData.JSONType = jtObject) then
    begin
      obj := TJSONObject(jsonData);
      if obj.Find('usuarios') <> nil then
      begin
        arr := obj.Arrays['usuarios'];
        cnt := 0;
        for i := 0 to arr.Count - 1 do
        begin
          if arr.Items[i].JSONType = jtObject then
          begin
            u.Id       := TJSONObject(arr.Items[i]).Get('id', 0);
            u.Nombre   := TJSONObject(arr.Items[i]).Get('nombre', '');
            u.Usuario  := TJSONObject(arr.Items[i]).Get('usuario', '');
            u.Email    := TJSONObject(arr.Items[i]).Get('email', '');
            u.Telefono := TJSONObject(arr.Items[i]).Get('telefono', '');
            u.Password := '';
            LS_Append(Lista, u);
            Inc(cnt);
          end;
        end;
        Result := cnt;
      end;
    end;
  finally
    if fs <> nil then fs.Free;
    if jsonData <> nil then jsonData.Free;
  end;
end;

end.
