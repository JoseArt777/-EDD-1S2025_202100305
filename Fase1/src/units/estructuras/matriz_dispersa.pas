unit matriz_dispersa;
{$mode objfpc}{$H+}
interface
uses
  SysUtils;

type
  PCol = ^TCol;
  TCol = record
    Key: AnsiString; // destinatario
    Count: LongInt;
    Next: PCol;
  end;

  PRow = ^TRow;
  TRow = record
    Key: AnsiString; // remitente
    Cols: PCol;
    Down: PRow;
  end;

  TMatrizDispersa = record
    Rows: PRow;
  end;

procedure MD_Init(var M: TMatrizDispersa);
procedure MD_Clear(var M: TMatrizDispersa);
procedure MD_AddEdge(var M: TMatrizDispersa; const RemitenteEmail, DestinatarioEmail: AnsiString; Incremento: LongInt);
function  MD_GetCount(const M: TMatrizDispersa; const RemitenteEmail, DestinatarioEmail: AnsiString): LongInt;
procedure MD_ToDot(const M: TMatrizDispersa; const FilePath: AnsiString);

implementation

procedure FreeCols(var C: PCol);
var t: PCol;
begin
  while C <> nil do
  begin
    t := C^.Next;
    Dispose(C);
    C := t;
  end;
end;

procedure MD_Init(var M: TMatrizDispersa);
begin
  M.Rows := nil;
end;

procedure MD_Clear(var M: TMatrizDispersa);
var r, tr: PRow;
begin
  r := M.Rows;
  while r <> nil do
  begin
    FreeCols(r^.Cols);
    tr := r^.Down;
    Dispose(r);
    r := tr;
  end;
  M.Rows := nil;
end;

function GetOrCreateRow(var Rows: PRow; const Key: AnsiString): PRow;
var cur, prev, n: PRow;
begin
  cur := Rows; prev := nil;
  while cur <> nil do
  begin
    if AnsiCompareText(cur^.Key, Key) = 0 then Exit(cur);
    prev := cur; cur := cur^.Down;
  end;
  New(n); n^.Key := Key; n^.Cols := nil; n^.Down := nil;
  if prev = nil then Rows := n else prev^.Down := n;
  Result := n;
end;

function GetOrCreateCol(var Cols: PCol; const Key: AnsiString): PCol;
var cur, prev, n: PCol;
begin
  cur := Cols; prev := nil;
  while cur <> nil do
  begin
    if AnsiCompareText(cur^.Key, Key) = 0 then Exit(cur);
    prev := cur; cur := cur^.Next;
  end;
  New(n); n^.Key := Key; n^.Count := 0; n^.Next := nil;
  if prev = nil then Cols := n else prev^.Next := n;
  Result := n;
end;

procedure MD_AddEdge(var M: TMatrizDispersa; const RemitenteEmail, DestinatarioEmail: AnsiString; Incremento: LongInt);
var r: PRow; c: PCol;
begin
  r := GetOrCreateRow(M.Rows, RemitenteEmail);
  c := GetOrCreateCol(r^.Cols, DestinatarioEmail);
  Inc(c^.Count, Incremento);
end;

function MD_GetCount(const M: TMatrizDispersa; const RemitenteEmail, DestinatarioEmail: AnsiString): LongInt;
var r: PRow; c: PCol;
begin
  r := M.Rows;
  while r <> nil do
  begin
    if AnsiCompareText(r^.Key, RemitenteEmail) = 0 then
    begin
      c := r^.Cols;
      while c <> nil do
      begin
        if AnsiCompareText(c^.Key, DestinatarioEmail) = 0 then Exit(c^.Count);
        c := c^.Next;
      end;
      Exit(0);
    end;
    r := r^.Down;
  end;
  Result := 0;
end;

procedure MD_ToDot(const M: TMatrizDispersa; const FilePath: AnsiString);
var f: Text; r: PRow; c: PCol;
begin
  Assign(f, FilePath); Rewrite(f);
  Writeln(f, 'digraph Relaciones {');
  Writeln(f, '  rankdir=LR;');
  r := M.Rows;
  while r <> nil do
  begin
    Writeln(f, '  sender_', StringReplace(r^.Key, '@', '_', []), ' [shape=box,label="', r^.Key, '"];');
    c := r^.Cols;
    while c <> nil do
    begin
      Writeln(f, '  recv_', StringReplace(c^.Key, '@', '_', []), ' [shape=ellipse,label="', c^.Key, '"];');
      Writeln(f, '  sender_', StringReplace(r^.Key, '@', '_', []), ' -> recv_', StringReplace(c^.Key, '@', '_', []), ' [label="', IntToStr(c^.Count), '"];');
      c := c^.Next;
    end;
    r := r^.Down;
  end;
  Writeln(f, '}');
  Close(f);
end;

end.

