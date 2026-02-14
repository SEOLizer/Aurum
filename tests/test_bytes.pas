{$mode objfpc}{$H+}
program test_bytes;

uses
  SysUtils, Classes,
  fpcunit, testregistry, consoletestrunner,
  bytes;

type
  TByteBufferTest = class(TTestCase)
  published
    procedure TestCreateEmpty;
    procedure TestWriteU8;
    procedure TestWriteU16LE;
    procedure TestWriteU32LE;
    procedure TestWriteU64LE;
    procedure TestWriteBytes;
    procedure TestWriteBytesFill;
    procedure TestPatchU32LE;
    procedure TestPatchU64LE;
    procedure TestReadWriteRoundtrip;
    procedure TestGrowth;
    procedure TestClear;
    procedure TestSaveToFile;
  end;

procedure TByteBufferTest.TestCreateEmpty;
var
  buf: TByteBuffer;
begin
  buf := TByteBuffer.Create;
  try
    AssertEquals('Leerer Buffer hat Size 0', 0, buf.Size);
    AssertTrue('GetBuffer ist nil bei leerem Buffer', buf.GetBuffer = nil);
  finally
    buf.Free;
  end;
end;

procedure TByteBufferTest.TestWriteU8;
var
  buf: TByteBuffer;
begin
  buf := TByteBuffer.Create;
  try
    buf.WriteU8($AB);
    AssertEquals(1, buf.Size);
    AssertEquals($AB, buf.ReadU8(0));
  finally
    buf.Free;
  end;
end;

procedure TByteBufferTest.TestWriteU16LE;
var
  buf: TByteBuffer;
begin
  buf := TByteBuffer.Create;
  try
    buf.WriteU16LE($CDAB);
    AssertEquals(2, buf.Size);
    // Little-Endian: Low-Byte zuerst
    AssertEquals($AB, buf.ReadU8(0));
    AssertEquals($CD, buf.ReadU8(1));
    AssertEquals($CDAB, buf.ReadU16LE(0));
  finally
    buf.Free;
  end;
end;

procedure TByteBufferTest.TestWriteU32LE;
var
  buf: TByteBuffer;
begin
  buf := TByteBuffer.Create;
  try
    buf.WriteU32LE($DEADBEEF);
    AssertEquals(4, buf.Size);
    AssertEquals($EF, buf.ReadU8(0));
    AssertEquals($BE, buf.ReadU8(1));
    AssertEquals($AD, buf.ReadU8(2));
    AssertEquals($DE, buf.ReadU8(3));
    AssertEquals(Integer($DEADBEEF), Integer(buf.ReadU32LE(0)));
  finally
    buf.Free;
  end;
end;

procedure TByteBufferTest.TestWriteU64LE;
var
  buf: TByteBuffer;
  v: QWord;
begin
  buf := TByteBuffer.Create;
  try
    v := QWord($0102030405060708);
    buf.WriteU64LE(v);
    AssertEquals(8, buf.Size);
    // Niedrigstes Byte zuerst
    AssertEquals($08, buf.ReadU8(0));
    AssertEquals($07, buf.ReadU8(1));
    AssertEquals($06, buf.ReadU8(2));
    AssertEquals($05, buf.ReadU8(3));
    AssertEquals($04, buf.ReadU8(4));
    AssertEquals($03, buf.ReadU8(5));
    AssertEquals($02, buf.ReadU8(6));
    AssertEquals($01, buf.ReadU8(7));
    AssertTrue(buf.ReadU64LE(0) = v);
  finally
    buf.Free;
  end;
end;

procedure TByteBufferTest.TestWriteBytes;
var
  buf: TByteBuffer;
  data: array[0..3] of Byte;
begin
  buf := TByteBuffer.Create;
  try
    data[0] := $10;
    data[1] := $20;
    data[2] := $30;
    data[3] := $40;
    buf.WriteBytes(data);
    AssertEquals(4, buf.Size);
    AssertEquals($10, buf.ReadU8(0));
    AssertEquals($20, buf.ReadU8(1));
    AssertEquals($30, buf.ReadU8(2));
    AssertEquals($40, buf.ReadU8(3));
  finally
    buf.Free;
  end;
end;

procedure TByteBufferTest.TestWriteBytesFill;
var
  buf: TByteBuffer;
begin
  buf := TByteBuffer.Create;
  try
    buf.WriteBytesFill(5, $CC);
    AssertEquals(5, buf.Size);
    AssertEquals($CC, buf.ReadU8(0));
    AssertEquals($CC, buf.ReadU8(2));
    AssertEquals($CC, buf.ReadU8(4));
  finally
    buf.Free;
  end;
end;

procedure TByteBufferTest.TestPatchU32LE;
var
  buf: TByteBuffer;
begin
  buf := TByteBuffer.Create;
  try
    buf.WriteU32LE($00000000);
    buf.WriteU32LE($FFFFFFFF);
    // Patch erstes U32
    buf.PatchU32LE(0, $CAFEBABE);
    AssertEquals(Integer($CAFEBABE), Integer(buf.ReadU32LE(0)));
    // Zweites U32 unverändert
    AssertEquals(Integer($FFFFFFFF), Integer(buf.ReadU32LE(4)));
  finally
    buf.Free;
  end;
end;

procedure TByteBufferTest.TestPatchU64LE;
var
  buf: TByteBuffer;
  v: QWord;
begin
  buf := TByteBuffer.Create;
  try
    buf.WriteU64LE(0);
    v := QWord($DEADBEEFCAFEBABE);
    buf.PatchU64LE(0, v);
    AssertTrue(buf.ReadU64LE(0) = v);
  finally
    buf.Free;
  end;
end;

procedure TByteBufferTest.TestReadWriteRoundtrip;
var
  buf: TByteBuffer;
begin
  buf := TByteBuffer.Create;
  try
    buf.WriteU8($11);
    buf.WriteU16LE($2233);
    buf.WriteU32LE($44556677);
    buf.WriteU64LE(QWord($8899AABBCCDDEEFF));
    AssertEquals(15, buf.Size);
    AssertEquals($11, buf.ReadU8(0));
    AssertEquals($2233, buf.ReadU16LE(1));
    AssertEquals(Integer($44556677), Integer(buf.ReadU32LE(3)));
    AssertTrue(buf.ReadU64LE(7) = QWord($8899AABBCCDDEEFF));
  finally
    buf.Free;
  end;
end;

procedure TByteBufferTest.TestGrowth;
var
  buf: TByteBuffer;
  i: Integer;
begin
  buf := TByteBuffer.Create;
  try
    // Schreibe mehr als InitialCapacity (256) Bytes
    for i := 0 to 511 do
      buf.WriteU8(Byte(i and $FF));
    AssertEquals(512, buf.Size);
    AssertEquals(0, buf.ReadU8(0));
    AssertEquals(255, buf.ReadU8(255));
    AssertEquals(0, buf.ReadU8(256));
  finally
    buf.Free;
  end;
end;

procedure TByteBufferTest.TestClear;
var
  buf: TByteBuffer;
begin
  buf := TByteBuffer.Create;
  try
    buf.WriteU32LE($12345678);
    AssertEquals(4, buf.Size);
    buf.Clear;
    AssertEquals(0, buf.Size);
  finally
    buf.Free;
  end;
end;

procedure TByteBufferTest.TestSaveToFile;
var
  buf: TByteBuffer;
  fs: TFileStream;
  tmpFile: string;
  b: Byte;
begin
  tmpFile := GetTempFileName('/tmp/', 'buf');
  buf := TByteBuffer.Create;
  try
    buf.WriteU8($DE);
    buf.WriteU8($AD);
    buf.SaveToFile(tmpFile);
  finally
    buf.Free;
  end;

  // Datei prüfen
  fs := TFileStream.Create(tmpFile, fmOpenRead);
  try
    AssertEquals(2, fs.Size);
    fs.ReadBuffer(b, 1);
    AssertEquals($DE, b);
    fs.ReadBuffer(b, 1);
    AssertEquals($AD, b);
  finally
    fs.Free;
  end;
  DeleteFile(tmpFile);
end;

var
  app: TTestRunner;
begin
  RegisterTest(TByteBufferTest);
  app := TTestRunner.Create(nil);
  try
    app.Run;
  finally
    app.Free;
  end;
end.
