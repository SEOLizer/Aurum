{$mode objfpc}{$H+}
unit x86_64_emit;

interface

uses
  SysUtils, Classes, bytes, ir;

type
  TLabelPos = record
    Name: string;
    Pos: Integer;
  end;

  TJumpPatch = record
    Pos: Integer;
    LabelName: string;
    JmpSize: Integer; // 5 for rel8, 6 for rel32 (jcc rel32)
  end;

  TX86_64Emitter = class
  private
    FCode: TByteBuffer;
    FData: TByteBuffer;
    FStringOffsets: array of UInt64;
    FLeaPositions: array of Integer;
    FLeaStrIndex: array of Integer;
    FLabelPositions: array of TLabelPos;
    FJumpPatches: array of TJumpPatch;
  public
    constructor Create;
    destructor Destroy; override;
    procedure EmitFromIR(module: TIRModule);
    function GetCodeBuffer: TByteBuffer;
    function GetDataBuffer: TByteBuffer;
    function GetFunctionOffset(const name: string): Integer;
  end;

implementation

uses
  Math;

const
  RAX = 0; RCX = 1; RDX = 2; RBX = 3; RSP = 4; RBP = 5; RSI = 6; RDI = 7; R8 = 8; R9 = 9; R10 = 10; R11 = 11; R12 = 12; R13 = 13; R14 = 14; R15 = 15;
  ParamRegs: array[0..5] of Byte = (RDI, RSI, RDX, RCX, R8, R9);

procedure EmitU8(b: TByteBuffer; v: Byte); begin b.WriteU8(v); end;
procedure EmitU32(b: TByteBuffer; v: Cardinal); begin b.WriteU32LE(v); end;
procedure EmitU64(b: TByteBuffer; v: UInt64); begin b.WriteU64LE(v); end;

procedure EmitRex(buf: TByteBuffer; w, r, x, b: Integer);
var
  rex: Byte;
begin
  rex := $40 or (Byte(w and 1) shl 3) or (Byte(r and 1) shl 2) or (Byte(x and 1) shl 1) or Byte(b and 1);
  EmitU8(buf, rex);
end;


procedure WriteMovRegImm64(buf: TByteBuffer; reg: Byte; imm: UInt64);
begin
  // mov r64, imm64 : REX.W + B8+rd
  EmitRex(buf, 1, 0, 0, (reg shr 3) and 1);
  EmitU8(buf, $B8 + (reg and $7));
  EmitU64(buf, imm);
end;
procedure WriteMovRegReg(buf: TByteBuffer; dst, src: Byte);
var rexR, rexB: Integer;
begin
  // mov r/m64, r64 : REX.W + 89 /r  (encode reg=src, rm=dst)
  rexR := (src shr 3) and 1;
  rexB := (dst shr 3) and 1;
  EmitRex(buf, 1, rexR, 0, rexB);

end.
