{$mode objfpc}{$H+}
unit bytes;

interface

uses
  SysUtils, Classes;

type
  TByteBuffer = class
  private
    FData: array of Byte;
    FSize: Integer;
    procedure Grow(needed: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteU8(v: Byte);
    procedure WriteU16LE(v: Word);
    procedure WriteU32LE(v: Cardinal);
    procedure WriteU64LE(v: QWord);
    procedure WriteBytes(const buf: array of Byte);
    procedure WriteBytesFill(count: Integer; v: Byte);
    procedure WriteBuffer(const buf; len: Integer);

    procedure PatchU8(offset: Integer; v: Byte);
    procedure PatchU16LE(offset: Integer; v: Word);
    procedure PatchU32LE(offset: Integer; v: Cardinal);
    procedure PatchU64LE(offset: Integer; v: QWord);

    function ReadU8(offset: Integer): Byte;
    function ReadU16LE(offset: Integer): Word;
    function ReadU32LE(offset: Integer): Cardinal;
    function ReadU64LE(offset: Integer): QWord;

    function Size: Integer;
    function GetBuffer: PByte;
    procedure SaveToFile(const fileName: string);
    procedure Clear;
  end;

implementation

const
  InitialCapacity = 256;

{ TByteBuffer }

constructor TByteBuffer.Create;
begin
  inherited Create;
  SetLength(FData, InitialCapacity);
  FSize := 0;
end;

destructor TByteBuffer.Destroy;
begin
  FData := nil;
  inherited Destroy;
end;

procedure TByteBuffer.Grow(needed: Integer);
var
  newCap: Integer;
begin
  if FSize + needed <= Length(FData) then
    Exit;
  newCap := Length(FData);
  while newCap < FSize + needed do
    newCap := newCap * 2;
  SetLength(FData, newCap);
end;

procedure TByteBuffer.WriteU8(v: Byte);
begin
  Grow(1);
  FData[FSize] := v;
  Inc(FSize);
end;

procedure TByteBuffer.WriteU16LE(v: Word);
begin
  Grow(2);
  FData[FSize]     := Byte(v);
  FData[FSize + 1] := Byte(v shr 8);
  Inc(FSize, 2);
end;

procedure TByteBuffer.WriteU32LE(v: Cardinal);
begin
  Grow(4);
  FData[FSize]     := Byte(v);
  FData[FSize + 1] := Byte(v shr 8);
  FData[FSize + 2] := Byte(v shr 16);
  FData[FSize + 3] := Byte(v shr 24);
  Inc(FSize, 4);
end;

procedure TByteBuffer.WriteU64LE(v: QWord);
begin
  Grow(8);
  FData[FSize]     := Byte(v);
  FData[FSize + 1] := Byte(v shr 8);
  FData[FSize + 2] := Byte(v shr 16);
  FData[FSize + 3] := Byte(v shr 24);
  FData[FSize + 4] := Byte(v shr 32);
  FData[FSize + 5] := Byte(v shr 40);
  FData[FSize + 6] := Byte(v shr 48);
  FData[FSize + 7] := Byte(v shr 56);
  Inc(FSize, 8);
end;

procedure TByteBuffer.WriteBytes(const buf: array of Byte);
var
  n: Integer;
begin
  n := Length(buf);
  if n = 0 then
    Exit;
  Grow(n);
  Move(buf[0], FData[FSize], n);
  Inc(FSize, n);
end;

procedure TByteBuffer.WriteBytesFill(count: Integer; v: Byte);
begin
  Grow(count);
  FillByte(FData[FSize], count, v);
  Inc(FSize, count);
end;

procedure TByteBuffer.WriteBuffer(const buf; len: Integer);
var
  p: PByte;
  i: Integer;
begin
  if len <= 0 then Exit;
  Grow(len);
  p := @buf;
  for i := 0 to len - 1 do
  begin
    FData[FSize + i] := p[i];
  end;
  Inc(FSize, len);
end;

procedure TByteBuffer.PatchU8(offset: Integer; v: Byte);
begin
  Assert((offset >= 0) and (offset < FSize), 'PatchU8: offset out of range');
  FData[offset] := v;
end;

procedure TByteBuffer.PatchU16LE(offset: Integer; v: Word);
begin
  Assert((offset >= 0) and (offset + 1 < FSize), 'PatchU16LE: offset out of range');
  FData[offset]     := Byte(v);
  FData[offset + 1] := Byte(v shr 8);
end;

procedure TByteBuffer.PatchU32LE(offset: Integer; v: Cardinal);
begin
  Assert((offset >= 0) and (offset + 3 < FSize), 'PatchU32LE: offset out of range');
  FData[offset]     := Byte(v);
  FData[offset + 1] := Byte(v shr 8);
  FData[offset + 2] := Byte(v shr 16);
  FData[offset + 3] := Byte(v shr 24);
end;

procedure TByteBuffer.PatchU64LE(offset: Integer; v: QWord);
begin
  Assert((offset >= 0) and (offset + 7 < FSize), 'PatchU64LE: offset out of range');
  FData[offset]     := Byte(v);
  FData[offset + 1] := Byte(v shr 8);
  FData[offset + 2] := Byte(v shr 16);
  FData[offset + 3] := Byte(v shr 24);
  FData[offset + 4] := Byte(v shr 32);
  FData[offset + 5] := Byte(v shr 40);
  FData[offset + 6] := Byte(v shr 48);
  FData[offset + 7] := Byte(v shr 56);
end;

function TByteBuffer.ReadU8(offset: Integer): Byte;
begin
  Assert((offset >= 0) and (offset < FSize), 'ReadU8: offset out of range');
  Result := FData[offset];
end;

function TByteBuffer.ReadU16LE(offset: Integer): Word;
begin
  Assert((offset >= 0) and (offset + 1 < FSize), 'ReadU16LE: offset out of range');
  Result := Word(FData[offset])
         or (Word(FData[offset + 1]) shl 8);
end;

function TByteBuffer.ReadU32LE(offset: Integer): Cardinal;
begin
  Assert((offset >= 0) and (offset + 3 < FSize), 'ReadU32LE: offset out of range');
  Result := Cardinal(FData[offset])
         or (Cardinal(FData[offset + 1]) shl 8)
         or (Cardinal(FData[offset + 2]) shl 16)
         or (Cardinal(FData[offset + 3]) shl 24);
end;

function TByteBuffer.ReadU64LE(offset: Integer): QWord;
begin
  Assert((offset >= 0) and (offset + 7 < FSize), 'ReadU64LE: offset out of range');
  Result := QWord(FData[offset])
         or (QWord(FData[offset + 1]) shl 8)
         or (QWord(FData[offset + 2]) shl 16)
         or (QWord(FData[offset + 3]) shl 24)
         or (QWord(FData[offset + 4]) shl 32)
         or (QWord(FData[offset + 5]) shl 40)
         or (QWord(FData[offset + 6]) shl 48)
         or (QWord(FData[offset + 7]) shl 56);
end;

function TByteBuffer.Size: Integer;
begin
  Result := FSize;
end;

function TByteBuffer.GetBuffer: PByte;
begin
  if FSize = 0 then
    Result := nil
  else
    Result := @FData[0];
end;

procedure TByteBuffer.SaveToFile(const fileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fileName, fmCreate);
  try
    if FSize > 0 then
      fs.WriteBuffer(FData[0], FSize);
  finally
    fs.Free;
  end;
end;

procedure TByteBuffer.Clear;
begin
  FSize := 0;
end;

end.
