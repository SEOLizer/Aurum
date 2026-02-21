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
  EmitU8(buf, $89);
  EmitU8(buf, $C0 or (((src and 7) shl 3) and $38) or (dst and $7));
end;
procedure WriteMovRegMem(buf: TByteBuffer; reg, base: Byte; disp: Integer);
var rexR, rexB: Integer;
begin
  // mov r64, r/m64 : REX.W + 8B /r   (reg=reg, rm=mem)
  rexR := (reg shr 3) and 1;
  rexB := (base shr 3) and 1;
  EmitRex(buf, 1, rexR, 0, rexB);
  EmitU8(buf, $8B);
  EmitU8(buf, $80 or (((reg and 7) shl 3) and $38) or (base and $7));
  // if rm == 4 (RSP) we must emit SIB byte
  if (base and 7) = 4 then
    EmitU8(buf, $24); // scale=0, index=4 (no index), base=4 (RSP)
  EmitU32(buf, Cardinal(disp));
end;
procedure WriteMovMemReg(buf: TByteBuffer; base: Byte; disp: Integer; reg: Byte);
var rexR, rexB: Integer;
begin
  // mov r/m64, r64 : REX.W + 89 /r   (reg=reg, rm=mem)
  rexR := (reg shr 3) and 1;
  rexB := (base shr 3) and 1;
  EmitRex(buf, 1, rexR, 0, rexB);
  EmitU8(buf, $89);
  EmitU8(buf, $80 or (((reg and 7) shl 3) and $38) or (base and $7));
  // if rm == 4 (RSP) we must emit SIB byte
  if (base and 7) = 4 then
    EmitU8(buf, $24); // scale=0, index=4 (no index), base=4 (RSP)
  EmitU32(buf, Cardinal(disp));
end;
procedure WriteAddRegReg(buf: TByteBuffer; dst, src: Byte);
var rexR, rexB: Integer;
begin
  // add r/m64, r64 : REX.W + 01 /r  (reg=src, rm=dst)
  rexR := (src shr 3) and 1;
  rexB := (dst shr 3) and 1;
  EmitRex(buf, 1, rexR, 0, rexB);
  EmitU8(buf, $01);
  EmitU8(buf, $C0 or (((src and 7) shl 3) and $38) or (dst and $7));
end;
procedure WriteSubRegReg(buf: TByteBuffer; dst, src: Byte);
var rexR, rexB: Integer;
begin
  // sub r/m64, r64 : REX.W + 29 /r
  rexR := (src shr 3) and 1;
  rexB := (dst shr 3) and 1;
  EmitRex(buf, 1, rexR, 0, rexB);
  EmitU8(buf, $29);
  EmitU8(buf, $C0 or (((src and 7) shl 3) and $38) or (dst and $7));
end;
procedure WriteImulRegReg(buf: TByteBuffer; dst, src: Byte);
var rexR, rexB: Integer;
begin
  // imul r64, r/m64 : REX.W 0F AF /r  (reg=dst, rm=src)
  rexR := (dst shr 3) and 1;
  rexB := (src shr 3) and 1;
  EmitRex(buf, 1, rexR, 0, rexB);
  EmitU8(buf, $0F);
  EmitU8(buf, $AF);
  EmitU8(buf, $C0 or (((dst and 7) shl 3) and $38) or (src and $7));
end;
procedure WriteCqo(buf: TByteBuffer); begin EmitU8(buf,$48); EmitU8(buf,$99); end;
procedure WriteIdivReg(buf: TByteBuffer; src: Byte);
var rexB: Integer;
begin
  // idiv r/m64 : REX.W + F7 /7 ; modrm = 0xF8 | rm (with mod=11)
  rexB := (src shr 3) and 1;
  EmitRex(buf, 1, 0, 0, rexB);
  EmitU8(buf, $F7);
  EmitU8(buf, $F8 or (src and $7));
end;
procedure WriteTestRaxRax(buf: TByteBuffer); begin EmitU8(buf,$48); EmitU8(buf,$85); EmitU8(buf,$C0); end;
procedure WriteTestRegReg(buf: TByteBuffer; r1, r2: Byte);
var rexR, rexB: Integer;
begin
  rexR := (r1 shr 3) and 1;
  rexB := (r2 shr 3) and 1;
  EmitRex(buf, 1, rexR, 0, rexB);
  EmitU8(buf, $85);
  EmitU8(buf, $C0 or (((r1 and 7) shl 3) and $38) or (r2 and $7));
end;
procedure WriteSyscall(buf: TByteBuffer); begin EmitU8(buf,$0F); EmitU8(buf,$05); end;
procedure WriteLeaRsiRipDisp(buf: TByteBuffer; disp32: Cardinal); begin EmitU8(buf,$48); EmitU8(buf,$8D); EmitU8(buf,$35); EmitU32(buf, disp32); end;

procedure WriteJeRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$84); EmitU32(buf, rel32); end;
procedure WriteJneRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$85); EmitU32(buf, rel32); end;
procedure WriteJgeRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$8D); EmitU32(buf, rel32); end;

procedure WriteJleRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$8E); EmitU32(buf, rel32); end;

procedure WriteJmpRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$E9); EmitU32(buf, rel32); end;


procedure WriteDecReg(buf: TByteBuffer; reg: Byte);
begin
  // dec r64: REX.W(+B) FF C8+reg
  EmitRex(buf, 1, 0, 0, (reg shr 3) and 1);
  EmitU8(buf, $FF); EmitU8(buf, $C8 or (reg and $7));
end;

procedure WriteIncReg(buf: TByteBuffer; reg: Byte);
begin
  // inc r64: REX.W(+B) FF C0+reg
  EmitRex(buf, 1, 0, 0, (reg shr 3) and 1);
  EmitU8(buf, $FF); EmitU8(buf, $C0 or (reg and $7));
end;

procedure WriteMovMemRegByte(buf: TByteBuffer; base: Byte; disp: Integer; reg8: Byte);
begin
  // mov byte ptr [base + disp32], r8 -> 88 /0 with mod=10 (disp32)
  EmitU8(buf, $88);
  EmitU8(buf, $80 or ((reg8 and $7) shl 3) or (base and $7));
  EmitU32(buf, Cardinal(disp));
end;

procedure WriteMovMemRegByteNoDisp(buf: TByteBuffer; base: Byte; reg8: Byte);
begin
  // mov byte ptr [base], r8 -> 88 /0 with mod=00 and rm=base
  EmitU8(buf, $88);
  EmitU8(buf, ((reg8 and $7) shl 3) or (base and $7));
end;

procedure WriteMovMemImm8(buf: TByteBuffer; base: Byte; disp: Integer; value: Byte);
begin
  // mov byte ptr [base+disp32], imm8 => C6 80 disp32 imm8
  EmitU8(buf, $C6);
  EmitU8(buf, $80 or (base and $7));
  EmitU32(buf, Cardinal(disp));
  EmitU8(buf, value);
end;

procedure WriteSetccMem8(buf: TByteBuffer; ccOpcode: Byte; baseReg: Byte; disp32: Integer);
begin
  // setcc r/m8 : opcode 0F ccOpcode modrm(mod=10) rm=base
  EmitU8(buf, $0F);
  EmitU8(buf, ccOpcode);
  EmitU8(buf, $80 or ((0 shl 3) and $38) or (baseReg and $7));
  EmitU32(buf, Cardinal(disp32));
end;

procedure WriteMovzxRegMem8(buf: TByteBuffer; dstReg: Byte; baseReg: Byte; disp32: Integer);
begin
  // movzx r64, r/m8 : rex.w 0F B6 /r with reg=dst, rm=mem
  EmitU8(buf, $48);
  EmitU8(buf, $0F);
  EmitU8(buf, $B6);
  EmitU8(buf, $80 or ((dstReg shl 3) and $38) or (baseReg and $7));
  EmitU32(buf, Cardinal(disp32));
end;

procedure WriteMovzxRegMem16(buf: TByteBuffer; dstReg: Byte; baseReg: Byte; disp32: Integer);
begin
  // movzx r64, r/m16 : rex.w 0F B7 /r
  EmitU8(buf, $48);
  EmitU8(buf, $0F);
  EmitU8(buf, $B7);
  EmitU8(buf, $80 or ((dstReg shl 3) and $38) or (baseReg and $7));
  EmitU32(buf, Cardinal(disp32));
end;

procedure WriteMovSxRegMem8(buf: TByteBuffer; dstReg: Byte; baseReg: Byte; disp32: Integer);
begin
  // movsx r64, r/m8 : rex.w 0F BE /r
  EmitU8(buf, $48);
  EmitU8(buf, $0F);
  EmitU8(buf, $BE);
  EmitU8(buf, $80 or ((dstReg shl 3) and $38) or (baseReg and $7));
  EmitU32(buf, Cardinal(disp32));
end;

procedure WriteMovSxRegMem16(buf: TByteBuffer; dstReg: Byte; baseReg: Byte; disp32: Integer);
begin
  // movsx r64, r/m16 : rex.w 0F BF /r
  EmitU8(buf, $48);
  EmitU8(buf, $0F);
  EmitU8(buf, $BF);
  EmitU8(buf, $80 or ((dstReg shl 3) and $38) or (baseReg and $7));
  EmitU32(buf, Cardinal(disp32));
end;

procedure WriteMovSxRegMem32(buf: TByteBuffer; dstReg: Byte; baseReg: Byte; disp32: Integer);
begin
  // movsxd r64, r/m32 : rex.w 63 /r
  EmitU8(buf, $48);
  EmitU8(buf, $63);
  EmitU8(buf, $80 or ((dstReg shl 3) and $38) or (baseReg and $7));
  EmitU32(buf, Cardinal(disp32));
end;

procedure WriteMovEAXMem32(buf: TByteBuffer; baseReg: Byte; disp32: Integer);
begin
  // mov eax, dword ptr [base+disp32] : 8B 80 disp32
  EmitU8(buf, $8B);
  EmitU8(buf, $80 or (baseReg and $7));
  EmitU32(buf, Cardinal(disp32));
end;

function SlotOffset(slot: Integer): Integer;
begin
  Result := -8 * (slot + 1);
end;

constructor TX86_64Emitter.Create;
begin
  inherited Create;
  FCode := TByteBuffer.Create;
  FData := TByteBuffer.Create;
  SetLength(FStringOffsets, 0);
  SetLength(FLeaPositions, 0);
  SetLength(FLeaStrIndex, 0);
  SetLength(FLabelPositions, 0);
  SetLength(FJumpPatches, 0);
end;

destructor TX86_64Emitter.Destroy;
begin
  FCode.Free; FData.Free; inherited Destroy;
end;

function TX86_64Emitter.GetCodeBuffer: TByteBuffer; begin Result := FCode; end;
function TX86_64Emitter.GetDataBuffer: TByteBuffer; begin Result := FData; end;

procedure TX86_64Emitter.EmitFromIR(module: TIRModule);

begin
  // EmitFromIR stubbed during merge-repair: full emitter implementation omitted
  Exit;
end;

;

end.
