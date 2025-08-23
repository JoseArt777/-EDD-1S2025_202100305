unit cola;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, edd_types, lista_doble;

type
  PNodeQueueCorreo = ^TNodeQueueCorreo;
  TNodeQueueCorreo = record
    Data: TCorreo;
    Next: PNodeQueueCorreo;
  end;

  TColaProgramados = record
    Front, Back: PNodeQueueCorreo;
    Count: LongInt;
  end;

procedure Q_Init(var Q: TColaProgramados);
procedure Q_Clear(var Q: TColaProgramados);
procedure Q_Enqueue(var Q: TColaProgramados; const C: TCorreo);
function  Q_Dequeue(var Q: TColaProgramados; out C: TCorreo): Boolean;
function  Q_Peek(const Q: TColaProgramados; out C: TCorreo): Boolean;
function  Q_ProcesarHasta(var Q: TColaProgramados; Limite: TDateTime; var Inbox: TListaDobleCorreos): LongInt;
procedure Q_ToDot(const Q: TColaProgramados; const FilePath: AnsiString);

implementation

procedure Q_Init(var Q: TColaProgramados);
begin
  Q.Front := nil; Q.Back := nil; Q.Count := 0;
end;

procedure Q_Clear(var Q: TColaProgramados);
var cur, tmp: PNodeQueueCorreo;
begin
  cur := Q.Front;
  while cur <> nil do
  begin
    tmp := cur^.Next;
    Dispose(cur);
    cur := tmp;
  end;
  Q.Front := nil; Q.Back := nil; Q.Count := 0;
end;

procedure Q_Enqueue(var Q: TColaProgramados; const C: TCorreo);
var n: PNodeQueueCorreo;
begin
  New(n); n^.Data := C; n^.Next := nil;
  if Q.Back <> nil then Q.Back^.Next := n else Q.Front := n;
  Q.Back := n;
  Inc(Q.Count);
end;

function Q_Dequeue(var Q: TColaProgramados; out C: TCorreo): Boolean;
var n: PNodeQueueCorreo;
begin
  if Q.Front = nil then Exit(False);
  n := Q.Front; C := n^.Data; Q.Front := n^.Next;
  if Q.Front = nil then Q.Back := nil;
  Dispose(n); Dec(Q.Count); Result := True;
end;

function Q_Peek(const Q: TColaProgramados; out C: TCorreo): Boolean;
begin
  if Q.Front = nil then Exit(False);
  C := Q.Front^.Data; Result := True;
end;

function Q_ProcesarHasta(var Q: TColaProgramados; Limite: TDateTime; var Inbox: TListaDobleCorreos): LongInt;
var c: TCorreo; enviados: LongInt;
begin
  enviados := 0;
  while (Q.Front <> nil) and (Q.Front^.Data.Fecha <= Limite) do
  begin
    Q_Dequeue(Q, c);
    // cambia a no programado y lo mete a la bandeja del destinatario
    c.Programado := False;
    c.Estado := ecNoLeido;
    LD_PushBack(Inbox, c);
    Inc(enviados);
  end;
  Result := enviados;
end;

procedure Q_ToDot(const Q: TColaProgramados; const FilePath: AnsiString);
var f: Text; cur: PNodeQueueCorreo;
begin
  Assign(f, FilePath); Rewrite(f);
  Writeln(f, 'digraph Programados {');
  Writeln(f, '  rankdir=LR; node [shape=box];');
  cur := Q.Front;
  while cur <> nil do
  begin
    Writeln(f, '  n', PtrUInt(cur), ' [label="', cur^.Data.Asunto, ' @ ', DateTimeToStr(cur^.Data.Fecha), '"];');
    if cur^.Next <> nil then
      Writeln(f, '  n', PtrUInt(cur), ' -> n', PtrUInt(cur^.Next), ';');
    cur := cur^.Next;
  end;
  Writeln(f, '}');
  Close(f);
end;

end.

