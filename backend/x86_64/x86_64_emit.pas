{$mode objfpc}{$H+}
unit x86_64_emit;

interface

uses
  SysUtils, Classes, bytes, ir, backend_types, ast;

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
    FExternalSymbols: array of TExternalSymbol;
    FPLTStubs: array of TLabelPos; // PLT stub positions for external symbols
    FPLTGOTPatches: array of TPLTGOTPatch; // PLT → GOT patches
    // Handler exception tables
    FHandlerLabels: array of string; // label names for each handler id
    FHandlerTableLeaPositions: array of Integer; // positions of lea to handler table (patch later)
    FHandlerHeadLoadPositions: array of Integer; // lea [rip+disp32] sites used to read HandlerHead
    FHandlerHeadStorePositions: array of Integer; // lea [rip+disp32] sites used to write HandlerHead

    procedure AddExternalSymbol(const name, libName: string);
    function IsExternalSymbol(const name: string): Boolean;
    procedure GeneratePLTStubs;
  public
    constructor Create;
    destructor Destroy; override;
    procedure EmitFromIR(module: TIRModule);
    function GetCodeBuffer: TByteBuffer;
    function GetDataBuffer: TByteBuffer;
    function GetFunctionOffset(const name: string): Integer;
    function GetExternalSymbols: TExternalSymbolArray;
    function GetPLTGOTPatches: TPLTGOTPatchArray;
  end;

implementation

uses
  Math;

function GetLibraryForSymbol(const symbolName: string): string;
begin
  // v0.3.0: Filesystem and libc symbols
  if (symbolName = 'printf') or (symbolName = 'malloc') or (symbolName = 'free') or
     (symbolName = 'strlen') or (symbolName = 'strcmp') or (symbolName = 'exit') or
     (symbolName = 'puts') or
     // Filesystem functions (v0.3.0)
     (symbolName = 'open') or (symbolName = 'read') or (symbolName = 'write') or
     (symbolName = 'close') or (symbolName = 'unlink') or (symbolName = 'rename') or
     (symbolName = 'lseek') or (symbolName = 'fcntl') or (symbolName = 'access') or
     (symbolName = 'chmod') or (symbolName = 'chown') or (symbolName = 'stat') or
     (symbolName = 'fstat') or (symbolName = 'mkdir') or (symbolName = 'rmdir') or
     (symbolName = 'getcwd') or (symbolName = 'chdir') then
    Result := 'libc.so.6'
  else if (symbolName = 'sin') or (symbolName = 'cos') or (symbolName = 'sqrt') or
          (symbolName = 'exp') or (symbolName = 'log') or (symbolName = 'pow') or
          (symbolName = 'floor') or (symbolName = 'ceil') or (symbolName = 'fabs') then
    Result := 'libm.so.6'
  else
    Result := 'libc.so.6'; // Default fallback
end;

{ DoubleToQWord: Reinterprets IEEE 754 double bits as UInt64 }
function DoubleToQWord(const d: Double): UInt64;
var
  p: PUInt64;
begin
  p := PUInt64(@d);
  Result := p^;
end;

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
procedure WriteLeaRegRipDisp(buf: TByteBuffer; reg: Byte; disp32: Cardinal);
var
  rexR: Integer;
begin
  rexR := (reg shr 3) and 1;
  EmitRex(buf, 1, rexR, 0, 0);
  EmitU8(buf, $8D);
  EmitU8(buf, ((reg and 7) shl 3) or $05);
  EmitU32(buf, disp32);
end;

procedure WriteLeaRsiRipDisp(buf: TByteBuffer; disp32: Cardinal);
begin
  WriteLeaRegRipDisp(buf, RSI, disp32);
end;
procedure WriteAddRegImm32(buf: TByteBuffer; reg: Byte; imm: Integer);
begin
  // ADD r64, imm32: 48 01 /r for rax, or REX.W + 81 /0 id for r64
  // For RAX specifically: 48 05 imm32
  if reg = RAX then
  begin
    EmitU8(buf, $48); // REX.W
    EmitU8(buf, $05); // ADD rax, imm32
  end
  else
  begin
    // ADD r/m64, imm32: REX.W 81 /0 r/m64 imm32
    EmitU8(buf, $48); // REX.W
    EmitU8(buf, $81); // ADD r/m64, imm32
    EmitU8(buf, $C0 or (reg and $7)); // ModR/M: mod=11, /0, r/m=reg
  end;
  EmitU32(buf, Cardinal(imm));
end;
procedure WriteLeaRegMemOffset(buf: TByteBuffer; reg: Byte; base: Byte; offset: Integer);
begin
  // LEA r64, [r/m64 + disp32]: 48 8D /r disp32
  // For [RBP + offset], modrm = 10 (disp32) + reg + r/m
  // RBP = 101 = 5, RAX = 000 = 0
  // mod=10 (disp32) = bits 6-7 = 2
  // So for RAX (0) and RBP (5): 10 000 101 = 0x85
  EmitU8(buf, $48); // REX.W
  EmitU8(buf, $8D); // LEA
  // ModR/M: mod=10, reg=reg, r/m=base
  EmitU8(buf, $80 or ((reg and $7) shl 3) or (base and $7));
  EmitU32(buf, Cardinal(offset));
end;

procedure WriteJeRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$84); EmitU32(buf, rel32); end;
procedure WriteJneRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$85); EmitU32(buf, rel32); end;
procedure WriteJgeRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$8D); EmitU32(buf, rel32); end;

procedure WriteJleRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$8E); EmitU32(buf, rel32); end;

procedure WriteJlRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$8C); EmitU32(buf, rel32); end;

procedure WriteJgRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$8F); EmitU32(buf, rel32); end;

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

{ SSE2 Floating-Point Instructions }

procedure WriteMovsdXmmMem(buf: TByteBuffer; xmmReg: Byte; baseReg: Byte; disp32: Integer);
begin
  // movsd xmm, [base+disp32] : F2 0F 10 /r
  EmitU8(buf, $F2);
  if (xmmReg >= 8) or (baseReg >= 8) then
    EmitRex(buf, 0, (xmmReg shr 3) and 1, 0, (baseReg shr 3) and 1);
  EmitU8(buf, $0F);
  EmitU8(buf, $10);
  EmitU8(buf, $80 or ((xmmReg and $7) shl 3) or (baseReg and $7));
  if (baseReg and 7) = 4 then
    EmitU8(buf, $24); // SIB for RSP/R12
  EmitU32(buf, Cardinal(disp32));
end;

procedure WriteMovsdMemXmm(buf: TByteBuffer; baseReg: Byte; disp32: Integer; xmmReg: Byte);
begin
  // movsd [base+disp32], xmm : F2 0F 11 /r
  EmitU8(buf, $F2);
  if (xmmReg >= 8) or (baseReg >= 8) then
    EmitRex(buf, 0, (xmmReg shr 3) and 1, 0, (baseReg shr 3) and 1);
  EmitU8(buf, $0F);
  EmitU8(buf, $11);
  EmitU8(buf, $80 or ((xmmReg and $7) shl 3) or (baseReg and $7));
  if (baseReg and 7) = 4 then
    EmitU8(buf, $24);
  EmitU32(buf, Cardinal(disp32));
end;

procedure WriteMovsdXmmXmm(buf: TByteBuffer; dstXmm: Byte; srcXmm: Byte);
begin
  // movsd xmm1, xmm2 : F2 0F 10 /r (mod=11)
  EmitU8(buf, $F2);
  if (dstXmm >= 8) or (srcXmm >= 8) then
    EmitRex(buf, 0, (dstXmm shr 3) and 1, 0, (srcXmm shr 3) and 1);
  EmitU8(buf, $0F);
  EmitU8(buf, $10);
  EmitU8(buf, $C0 or ((dstXmm and $7) shl 3) or (srcXmm and $7));
end;

procedure WriteAddsdXmmXmm(buf: TByteBuffer; dstXmm: Byte; srcXmm: Byte);
begin
  // addsd xmm1, xmm2 : F2 0F 58 /r (mod=11)
  EmitU8(buf, $F2);
  if (dstXmm >= 8) or (srcXmm >= 8) then
    EmitRex(buf, 0, (dstXmm shr 3) and 1, 0, (srcXmm shr 3) and 1);
  EmitU8(buf, $0F);
  EmitU8(buf, $58);
  EmitU8(buf, $C0 or ((dstXmm and $7) shl 3) or (srcXmm and $7));
end;

procedure WriteSubsdXmmXmm(buf: TByteBuffer; dstXmm: Byte; srcXmm: Byte);
begin
  // subsd xmm1, xmm2 : F2 0F 5C /r (mod=11)
  EmitU8(buf, $F2);
  if (dstXmm >= 8) or (srcXmm >= 8) then
    EmitRex(buf, 0, (dstXmm shr 3) and 1, 0, (srcXmm shr 3) and 1);
  EmitU8(buf, $0F);
  EmitU8(buf, $5C);
  EmitU8(buf, $C0 or ((dstXmm and $7) shl 3) or (srcXmm and $7));
end;

procedure WriteMulsdXmmXmm(buf: TByteBuffer; dstXmm: Byte; srcXmm: Byte);
begin
  // mulsd xmm1, xmm2 : F2 0F 59 /r (mod=11)
  EmitU8(buf, $F2);
  if (dstXmm >= 8) or (srcXmm >= 8) then
    EmitRex(buf, 0, (dstXmm shr 3) and 1, 0, (srcXmm shr 3) and 1);
  EmitU8(buf, $0F);
  EmitU8(buf, $59);
  EmitU8(buf, $C0 or ((dstXmm and $7) shl 3) or (srcXmm and $7));
end;

procedure WriteDivsdXmmXmm(buf: TByteBuffer; dstXmm: Byte; srcXmm: Byte);
begin
  // divsd xmm1, xmm2 : F2 0F 5E /r (mod=11)
  EmitU8(buf, $F2);
  if (dstXmm >= 8) or (srcXmm >= 8) then
    EmitRex(buf, 0, (dstXmm shr 3) and 1, 0, (srcXmm shr 3) and 1);
  EmitU8(buf, $0F);
  EmitU8(buf, $5E);
  EmitU8(buf, $C0 or ((dstXmm and $7) shl 3) or (srcXmm and $7));
end;

procedure WriteXorpdXmmXmm(buf: TByteBuffer; dstXmm: Byte; srcXmm: Byte);
begin
  // xorpd xmm1, xmm2 : 66 0F 57 /r (mod=11)
  EmitU8(buf, $66);
  if (dstXmm >= 8) or (srcXmm >= 8) then
    EmitRex(buf, 0, (dstXmm shr 3) and 1, 0, (srcXmm shr 3) and 1);
  EmitU8(buf, $0F);
  EmitU8(buf, $57);
  EmitU8(buf, $C0 or ((dstXmm and $7) shl 3) or (srcXmm and $7));
end;

procedure WriteUcomisdXmmXmm(buf: TByteBuffer; dstXmm: Byte; srcXmm: Byte);
begin
  // ucomisd xmm1, xmm2 : 66 0F 2E /r (mod=11)
  EmitU8(buf, $66);
  if (dstXmm >= 8) or (srcXmm >= 8) then
    EmitRex(buf, 0, (dstXmm shr 3) and 1, 0, (srcXmm shr 3) and 1);
  EmitU8(buf, $0F);
  EmitU8(buf, $2E);
  EmitU8(buf, $C0 or ((dstXmm and $7) shl 3) or (srcXmm and $7));
end;

procedure WriteCvtsd2siRaxXmm(buf: TByteBuffer; xmmReg: Byte);
begin
  // cvtsd2si rax, xmm : F2 48 0F 2D /r
  EmitU8(buf, $F2);
  EmitRex(buf, 1, 0, 0, (xmmReg shr 3) and 1);
  EmitU8(buf, $0F);
  EmitU8(buf, $2D);
  EmitU8(buf, $C0 or (xmmReg and $7));
end;

procedure WriteCvttsd2siRaxXmm(buf: TByteBuffer; xmmReg: Byte);
begin
  // cvttsd2si rax, xmm : F2 48 0F 2C /r  (truncate toward zero)
  EmitU8(buf, $F2);
  EmitRex(buf, 1, 0, 0, (xmmReg shr 3) and 1);
  EmitU8(buf, $0F);
  EmitU8(buf, $2C);
  EmitU8(buf, $C0 or (xmmReg and $7));
end;

procedure WriteCvtsi2sdXmmRax(buf: TByteBuffer; xmmReg: Byte);
begin
  // cvtsi2sd xmm, rax : F2 48 0F 2A /r
  EmitU8(buf, $F2);
  EmitRex(buf, 1, (xmmReg shr 3) and 1, 0, 0);
  EmitU8(buf, $0F);
  EmitU8(buf, $2A);
  EmitU8(buf, $C0 or ((xmmReg and $7) shl 3));
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
  var
  i, j, k, sidx: Integer;
  totalDataOffset: UInt64;
  instr: TIRInstr;
  localCnt, maxTemp, totalSlots, slotIdx: Integer;
  // flags for data sections
  envAdded: Boolean;
  bufferAdded: Boolean;
  leaPos: Integer;
  codeVA, instrVA, dataVA: UInt64;
  disp32, rel32: Int64;
  tempStrIndex: array of Integer;
  bufferOffset: UInt64;
  bufferLeaPositions: array of Integer;
  // env data storage (argc, argv)
  envOffset: UInt64;
  envLeaPositions: array of Integer;
   len: Integer;
     nonZeroPos, jmpDonePos, jgePos, loopStartPos, jneLoopPos, jeSignPos: Integer;
     strlenLoopPos, strlenDonePos: Integer;
     // for string conversions
     parseDonePos, notNegPos, noNegPos, noSignPos: Integer;
   targetPos, jmpPos: Integer;
   jmpAfterPadPos: Integer;
   // for call/abi (v0.2.0)
   argCount: Integer;
   argTemps: array of Integer;
   sParse: string;
   ppos, ai: Integer;
   // for jump patching
   labelName: string;
   isPLTCall: Boolean;
   pltBaseName: string;
   // for call extra
   extraCount: Integer;
   // function context
   isEntryFunction: Boolean;
   frameBytes: Integer;
   framePad: Integer;
   callPad: Integer;
   // array operations
   allocSize: Integer;
   elemIndex: Integer;
   elemOffset: Integer;
   pushBytes: Integer;
   restoreBytes: Integer;
   // exception helpers
    handlerId: Integer;
    uncaughtPos: Integer;
    // handler table offsets
    handlerHeadOffset: UInt64;

   handlerTableDataPos: Integer;
   handlerTableOffset: UInt64;
    // debug dump helpers
    dumpStart: Integer;
    hexs: string;
    di: Integer;
    // metadata writer
    metaFs: TFileStream;
    tmp: string;
    // integer width helpers
    mask64: UInt64;
    sh: Integer;
     argTemp3: Integer;
     argTemp4: Integer;
     argTemp5: Integer;
     argTemp6: Integer;
  begin
  // reset patch arrays
  SetLength(FLeaPositions, 0);
  SetLength(FLeaStrIndex, 0);
  SetLength(FLabelPositions, 0);
  SetLength(FJumpPatches, 0);
  SetLength(FExternalSymbols, 0);
  SetLength(FPLTStubs, 0);
  SetLength(FPLTGOTPatches, 0);
  SetLength(FStringOffsets, 0);
  SetLength(FLeaPositions, 0);
  SetLength(bufferLeaPositions, 0);
  SetLength(envLeaPositions, 0);
  // handler tables
  SetLength(FHandlerLabels, 0);
  SetLength(FHandlerTableLeaPositions, 0);
  SetLength(FHandlerHeadLoadPositions, 0);
  SetLength(FHandlerHeadStorePositions, 0);

  // initialize data section offset and flags
  totalDataOffset := 0;
  envAdded := False;
  bufferAdded := False;

  // Emit program entry (_start): automatically initialize env data (argc/argv) and call main
  begin
    // Reserve env data in data segment (16 bytes: argc,qword + argv_ptr,qword)
    if not envAdded then
    begin
      envOffset := totalDataOffset;
      for k := 1 to 16 do FData.WriteU8(0);
      Inc(totalDataOffset, 16);
      envAdded := True;
    end;

    // Load argc from [rsp] into RAX
    WriteMovRegMem(FCode, RAX, RSP, 0);
    // lea rsi, [rip + disp32] ; patch later
    leaPos := FCode.Size;
    EmitU8(FCode, $48); EmitU8(FCode, $8D); EmitU8(FCode, $35); EmitU32(FCode, 0);
    SetLength(envLeaPositions, Length(envLeaPositions) + 1);
    envLeaPositions[High(envLeaPositions)] := leaPos;
    // store argc at [rsi]
    WriteMovMemReg(FCode, RSI, 0, RAX);

    // load argv ptr from [rsp+8] into RAX
    WriteMovRegMem(FCode, RAX, RSP, 8);
    // store argv ptr at [rsi+8]
    WriteMovMemReg(FCode, RSI, 8, RAX);

    // call main (patched later)
    SetLength(FJumpPatches, Length(FJumpPatches) + 1);
    FJumpPatches[High(FJumpPatches)].Pos := FCode.Size;
    FJumpPatches[High(FJumpPatches)].LabelName := 'main';
    FJumpPatches[High(FJumpPatches)].JmpSize := 5; // call rel32
    EmitU8(FCode, $E8); // call rel32
    EmitU32(FCode, 0);  // placeholder offset

    // move return value (in RAX) into RDI for exit
    WriteMovRegReg(FCode, RDI, RAX);
    // mov rax, 60 ; sys_exit
    WriteMovRegImm64(FCode, RAX, 60);
    // syscall
    WriteSyscall(FCode);
  end;

    for i := 0 to High(module.Functions) do
    begin
      // record function start label for calls
      SetLength(FLabelPositions, Length(FLabelPositions) + 1);
      FLabelPositions[High(FLabelPositions)].Name := module.Functions[i].Name;
      FLabelPositions[High(FLabelPositions)].Pos := FCode.Size;

      localCnt := module.Functions[i].LocalCount;
      // is this the program entry (main)? If so, irReturn should sys_exit
      isEntryFunction := (module.Functions[i].Name = 'main');
      maxTemp := -1;
      for j := 0 to High(module.Functions[i].Instructions) do
      begin
        instr := module.Functions[i].Instructions[j];
        if instr.Dest > maxTemp then maxTemp := instr.Dest;
        if instr.Src1 > maxTemp then maxTemp := instr.Src1;
        if instr.Src2 > maxTemp then maxTemp := instr.Src2;
      end;
      if maxTemp < 0 then maxTemp := 0 else Inc(maxTemp);
      totalSlots := localCnt + maxTemp;
      // sanity cap to avoid huge stack allocations from bad IR
      if totalSlots < 0 then totalSlots := 0;
      if totalSlots > 1024 then
      begin
        WriteLn('EMITTER: warning: totalSlots too large, capping. localCnt=', localCnt, ' maxTemp=', maxTemp, ' totalSlotsRaw=', localCnt+maxTemp);
        totalSlots := 1024;
      end;

      // compute prologue stack adjustment (frame + padding for alignment)
      frameBytes := totalSlots * 8;
      framePad := (16 - ((frameBytes + 8) mod 16)) mod 16; // +8 because return address after push rbp
      EmitU8(FCode, $55); // push rbp
      EmitU8(FCode, $48); EmitU8(FCode, $89); EmitU8(FCode, $E5); // mov rbp,rsp
      if frameBytes + framePad > 0 then
      begin
        EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $EC);
        EmitU32(FCode, Cardinal(frameBytes + framePad));
      end;

      // spill incoming parameters into slots
      if module.Functions[i].ParamCount > 0 then
      begin
        for k := 0 to module.Functions[i].ParamCount - 1 do
        begin
          slotIdx := k;
          if k < Length(ParamRegs) then
            WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), ParamRegs[k])
          else
          begin
            disp32 := 16 + (k - Length(ParamRegs)) * 8;
            WriteMovRegMem(FCode, RAX, RBP, Integer(disp32));
            WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), RAX);
          end;
        end;
      end;

      SetLength(tempStrIndex, maxTemp);
      for k := 0 to maxTemp - 1 do tempStrIndex[k] := -1;


    for j := 0 to High(module.Functions[i].Instructions) do
    begin
      instr := module.Functions[i].Instructions[j];
      case instr.Op of
        irConstStr:
          begin
            slotIdx := localCnt + instr.Dest;
            leaPos := FCode.Size;
            EmitU8(FCode, $48); EmitU8(FCode, $8D); EmitU8(FCode, $05); EmitU32(FCode, 0);
            SetLength(FLeaPositions, Length(FLeaPositions) + 1);
            SetLength(FLeaStrIndex, Length(FLeaStrIndex) + 1);
            FLeaPositions[High(FLeaPositions)] := leaPos;
            sidx := StrToIntDef(instr.ImmStr, 0);
            FLeaStrIndex[High(FLeaStrIndex)] := sidx;
            WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), RAX);
            if instr.Dest < Length(tempStrIndex) then
              tempStrIndex[instr.Dest] := sidx;
          end;
        irCallBuiltin:
          begin
            if instr.ImmStr = 'print_str' then
            begin
              // load pointer from slot into RSI
              WriteMovRegMem(FCode, RSI, RBP, SlotOffset(localCnt + instr.Src1));
               // Use runtime strlen: always scan for \0 starting at RSI
               // rcx = rsi (save start pointer)
               WriteMovRegReg(FCode, RCX, RSI);
               // strlen_loop: cmp byte [rcx], 0
               // je strlen_done
               // inc rcx
               // jmp strlen_loop
               // strlen_done: rdx = rcx - rsi
               //   strlen_loop:
               EmitU8(FCode, $80); EmitU8(FCode, $39); EmitU8(FCode, $00); // cmp byte [rcx], 0
               //   je +3 (skip inc + jmp = 3+5 = 8 bytes... actually inc=3, jmp=2)
               //   inc rcx = 48 FF C1 (3 bytes)
               //   jmp back = EB xx (2 bytes, short jump)
               //   je strlen_done (skip inc+jmp = 5 bytes)
               EmitU8(FCode, $74); EmitU8(FCode, $05); // je +5
               WriteIncReg(FCode, RCX);                 // inc rcx (3 bytes)
               EmitU8(FCode, $EB); EmitU8(FCode, $F6);  // jmp -10 (back to cmp)
               // strlen_done: rdx = rcx - rsi
               WriteMovRegReg(FCode, RDX, RCX);
               WriteSubRegReg(FCode, RDX, RSI);

              // syscall write(1, rsi, rdx)
              WriteMovRegImm64(FCode, RAX, 1);
              WriteMovRegImm64(FCode, RDI, 1);
              WriteSyscall(FCode);
            end
            else if instr.ImmStr = 'print_int' then
            begin
              if not bufferAdded then
              begin
                bufferOffset := totalDataOffset;
                for k := 1 to 64 do FData.WriteU8(0);
                Inc(totalDataOffset, 64);
                bufferAdded := True;
              end;

              // load value into RAX
              WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
              // lea rsi, buffer
              leaPos := FCode.Size;
              EmitU8(FCode, $48); EmitU8(FCode, $8D); EmitU8(FCode, $35); EmitU32(FCode, 0);
              SetLength(bufferLeaPositions, Length(bufferLeaPositions) + 1);
              bufferLeaPositions[High(bufferLeaPositions)] := leaPos;

              // rdi = rsi + 64
              WriteMovRegReg(FCode, RDI, RSI);
              WriteMovRegImm64(FCode, RDX, 64);
              WriteAddRegReg(FCode, RDI, RDX);

              // cmp rax,0 ; jne nonzero
              EmitU8(FCode, $48); EmitU8(FCode, $83); EmitU8(FCode, $F8); EmitU8(FCode, 0);
              nonZeroPos := FCode.Size;
              WriteJneRel32(FCode, 0);

              // zero path
              WriteMovMemImm8(FCode, RSI, 0, Ord('0'));
              WriteMovRegImm64(FCode, RDX, 1);
              WriteMovRegImm64(FCode, RAX, 1);
              WriteMovRegImm64(FCode, RDI, 1);
              WriteSyscall(FCode);
              jmpDonePos := FCode.Size;
              WriteJmpRel32(FCode, 0);

              // non-zero label
              k := FCode.Size;
              FCode.PatchU32LE(nonZeroPos + 2, Cardinal(k - nonZeroPos - 6));

              // sign flag in rbx
              WriteMovRegImm64(FCode, RBX, 0);
              EmitU8(FCode, $48); EmitU8(FCode, $83); EmitU8(FCode, $F8); EmitU8(FCode, 0);
              jgePos := FCode.Size;
              WriteJgeRel32(FCode, 0);
              EmitU8(FCode, $48); EmitU8(FCode, $F7); EmitU8(FCode, $D8); // neg rax
              WriteMovRegImm64(FCode, RBX, 1);
              k := FCode.Size;
              FCode.PatchU32LE(jgePos + 2, Cardinal(k - jgePos - 6));

              // loop over digits
              loopStartPos := FCode.Size;
              WriteCqo(FCode);
              WriteMovRegImm64(FCode, RCX, 10);
              WriteIdivReg(FCode, RCX);
              EmitU8(FCode, $80); EmitU8(FCode, $C2); EmitU8(FCode, Byte(Ord('0')));
              WriteDecReg(FCode, RDI);
              EmitU8(FCode, $88); EmitU8(FCode, $17);
              WriteTestRaxRax(FCode);
              jneLoopPos := FCode.Size;
              WriteJneRel32(FCode, 0);

              // add sign if needed
              EmitU8(FCode, $48); EmitU8(FCode, $83); EmitU8(FCode, $FB); EmitU8(FCode, 0);
              jeSignPos := FCode.Size;
              WriteJeRel32(FCode, 0);
              WriteDecReg(FCode, RDI);
              WriteMovMemImm8(FCode, RDI, 0, Ord('-'));
              k := FCode.Size;
              FCode.PatchU32LE(jeSignPos + 2, Cardinal(k - jeSignPos - 6));

              // compute length = (buffer_end) - rdi
              EmitU8(FCode, $48); EmitU8(FCode, $8D); EmitU8(FCode, $8E); EmitU32(FCode, 64);
              WriteSubRegReg(FCode, RCX, RDI);
              WriteMovRegReg(FCode, RDX, RCX);

              // syscall write(1, rdi, rdx)
              WriteMovRegImm64(FCode, RAX, 1);
              WriteMovRegReg(FCode, RSI, RDI);
              WriteMovRegImm64(FCode, RDI, 1);
              WriteSyscall(FCode);

               // patch loop jump
               k := FCode.Size;
               FCode.PatchU32LE(jneLoopPos + 2, Cardinal(loopStartPos - jneLoopPos - 6));

                // patch done jump
                FCode.PatchU32LE(jmpDonePos + 1, Cardinal(k - jmpDonePos - 5));
             end

             else if instr.ImmStr = 'env_init' then
            else if instr.ImmStr = 'env_init' then
            begin
              // env_init(argc, argv): store argc (qword) and argv pointer (qword) into data
              if not envAdded then
              begin
                envOffset := totalDataOffset;
                for k := 1 to 16 do FData.WriteU8(0);
                Inc(totalDataOffset, 16);
                envAdded := True;
              end;

              // load argc into RAX
              if instr.Src1 >= 0 then
                WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1))
              else
                WriteMovRegImm64(FCode, RAX, 0);

              // lea rsi, envData
              leaPos := FCode.Size;
              EmitU8(FCode, $48); EmitU8(FCode, $8D); EmitU8(FCode, $35); EmitU32(FCode, 0);
              SetLength(envLeaPositions, Length(envLeaPositions) + 1);
              envLeaPositions[High(envLeaPositions)] := leaPos;

              // store argc at [rsi]
              WriteMovMemReg(FCode, RSI, 0, RAX);

              // load argv ptr into RAX
              if instr.Src2 >= 0 then
                WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src2))
              else
                WriteMovRegImm64(FCode, RAX, 0);

              // store argv ptr at [rsi + 8]
              WriteMovMemReg(FCode, RSI, 8, RAX);
            end
            else if instr.ImmStr = 'env_arg_count' then
            begin
              // return stored argc
              // if not present, return 0
              if not envAdded then
              begin
                WriteMovRegImm64(FCode, RAX, 0);
              end
              else
              begin
                // lea rsi, envData
                leaPos := FCode.Size;
                EmitU8(FCode, $48); EmitU8(FCode, $8D); EmitU8(FCode, $35); EmitU32(FCode, 0);
                SetLength(envLeaPositions, Length(envLeaPositions) + 1);
                envLeaPositions[High(envLeaPositions)] := leaPos;
                // mov rax, qword ptr [rsi]
                WriteMovRegMem(FCode, RAX, RSI, 0);
              end;
              // store result into dest slot if applicable
              if instr.Dest >= 0 then
                WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
            end
            else if instr.ImmStr = 'env_arg' then
            begin
              // return argv[i] (pchar) or nil
              if not envAdded then
              begin
                WriteMovRegImm64(FCode, RAX, 0);
              end
              else
              begin
                // load index into RAX
                if instr.Src1 >= 0 then
                  WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1))
                else
                  WriteMovRegImm64(FCode, RAX, 0);
                // lea rsi, envData
                leaPos := FCode.Size;
                EmitU8(FCode, $48); EmitU8(FCode, $8D); EmitU8(FCode, $35); EmitU32(FCode, 0);
                SetLength(envLeaPositions, Length(envLeaPositions) + 1);
                envLeaPositions[High(envLeaPositions)] := leaPos;
                // load argv_base = qword ptr [rsi+8]
                WriteMovRegMem(FCode, RDI, RSI, 8);
                // compute address = argv_base + index*8
                // rdx = index * 8
                WriteMovRegReg(FCode, RDX, RAX);
                WriteMovRegImm64(FCode, RCX, 8);
                WriteImulRegReg(FCode, RDX, RCX);
                // add rdi, rdx
                WriteAddRegReg(FCode, RDI, RDX);
                // mov rax, qword ptr [rdi]
                WriteMovRegMem(FCode, RAX, RDI, 0);
              end;
              // store result into dest slot if applicable
              if instr.Dest >= 0 then
                WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
            end
            else if instr.ImmStr = 'now_unix' then
            begin
              // Use clock_gettime(CLOCK_REALTIME, &ts) syscall (228) to get seconds
              // sub rsp,16
              EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $EC); EmitU32(FCode, 16);
              // rdi = CLOCK_REALTIME (0)
              WriteMovRegImm64(FCode, RDI, 0);
              // rsi = rsp (timespec pointer)
              WriteMovRegReg(FCode, RSI, RSP);
              // syscall number for clock_gettime = 228
              WriteMovRegImm64(FCode, RAX, 228);
              WriteSyscall(FCode);
              // mov rax, qword ptr [rsp]  ; tv_sec
              WriteMovRegMem(FCode, RAX, RSP, 0);
              // add rsp,16
              EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $C4); EmitU32(FCode, 16);
              if instr.Dest >= 0 then
                WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
            end
            else if instr.ImmStr = 'now_unix_ms' then
            begin
              // Use clock_gettime to get seconds and nanoseconds, compute ms = sec*1000 + nsec/1_000_000
              EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $EC); EmitU32(FCode, 16);
              WriteMovRegImm64(FCode, RDI, 0);
              WriteMovRegReg(FCode, RSI, RSP);
              WriteMovRegImm64(FCode, RAX, 228);
              WriteSyscall(FCode);
              // rax = tv_sec ; rcx = tv_nsec
              WriteMovRegMem(FCode, RAX, RSP, 0);
              WriteMovRegMem(FCode, RCX, RSP, 8);
              // multiply sec by 1000
              WriteMovRegImm64(FCode, R8, 1000);
              WriteImulRegReg(FCode, RAX, R8); // rax = sec * 1000
              // rcx = rcx / 1000000
              WriteMovRegImm64(FCode, R9, 1000000);
              // prepare for idiv: move rcx into rdx:rax for division
              WriteMovRegReg(FCode, RDX, RCX);
              // use mov rax, rcx then cqo then idiv r9
              WriteMovRegReg(FCode, RAX, RDX);
              WriteCqo(FCode);
              WriteIdivReg(FCode, R9);
              // now rax = nsec/1000000
              // add to seconds*1000 (in R8? actually earlier we stored seconds*1000 in RAX then overwritten; redo approach)
              // Simpler: recompute: load sec into r10, mul 1000 into r10, then compute nsec/1e6 into r11 and add
              // load sec into r10
              WriteMovRegMem(FCode, R10, RSP, 0);
              WriteMovRegImm64(FCode, R8, 1000);
              WriteImulRegReg(FCode, R10, R8); // r10 = sec*1000
              // load nsec into r11
              WriteMovRegMem(FCode, R11, RSP, 8);
              WriteMovRegImm64(FCode, R9, 1000000);
              WriteMovRegReg(FCode, RAX, R11);
              WriteCqo(FCode);
              WriteIdivReg(FCode, R9); // rax = nsec/1000000
              // add r10 + rax -> result in rax
              WriteAddRegReg(FCode, RAX, R10);
              // cleanup stack
              EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $C4); EmitU32(FCode, 16);
              if instr.Dest >= 0 then
                WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
            end
            else if instr.ImmStr = 'sleep_ms' then
            begin
              // sleep_ms(ms): convert ms -> timespec and call nanosleep (syscall 35)
              // load ms into RAX
              if instr.Src1 >= 0 then
                WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1))
              else
                WriteMovRegImm64(FCode, RAX, 0);
              // seconds = ms / 1000 -> in RDX
              WriteMovRegReg(FCode, RDX, RAX);
              WriteMovRegImm64(FCode, RCX, 1000);
              // div rdx by rcx -> use idiv: sign extend RAX into RDX:RAX then idiv rcx (we have RAX=ms)
              // move ms into RAX for idiv
              WriteMovRegReg(FCode, RAX, RAX);
              WriteCqo(FCode); // sign extend RAX into RDX:RAX
              WriteMovRegImm64(FCode, RCX, 1000);
              WriteIdivReg(FCode, RCX);
              // after idiv: quotient in RAX (seconds), remainder in RDX (ms%1000)
              // move seconds to [rsp-16] and nsec to [rsp-8] and call nanosleep
              // sub rsp,16
              EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $EC); EmitU32(FCode, 16);
              // store seconds at [rsp]
              WriteMovMemReg(FCode, RSP, 0, RAX);
              // compute nsec = remainder * 1000000
              // remainder currently in RDX
              WriteMovRegReg(FCode, RCX, RDX);
              WriteMovRegImm64(FCode, R8, 1000000);
              WriteImulRegReg(FCode, RCX, R8);
              // store nsec at [rsp+8]
              WriteMovMemReg(FCode, RSP, 8, RCX);
              // prepare args: rdi = rsp (timespec*), rsi = 0
              WriteMovRegReg(FCode, RDI, RSP);
              WriteMovRegImm64(FCode, RSI, 0);
              // syscall number for nanosleep = 35
              WriteMovRegImm64(FCode, RAX, 35);
              WriteSyscall(FCode);
              // add rsp,16
              EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $C4); EmitU32(FCode, 16);
            end
            else if instr.ImmStr = 'exit' then
            begin
              // load exit code from temp slot into RDI
              WriteMovRegMem(FCode, RDI, RBP, SlotOffset(localCnt + instr.Src1));
              WriteMovRegImm64(FCode, RAX, 60);
              WriteSyscall(FCode);
            end;
          end;
        irConstInt:
           begin
             // constant immediate -> load into slot
             slotIdx := localCnt + instr.Dest;
             WriteMovRegImm64(FCode, RAX, instr.ImmInt);
             WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), RAX);
           end;
          irConstFloat:
            begin
              // Load float constant as IEEE 754 double bits into stack slot
              slotIdx := localCnt + instr.Dest;
              WriteMovRegImm64(FCode, RAX, DoubleToQWord(instr.ImmFloat));
              WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), RAX);
            end;
         irLoadLocal:
            begin
              // Load local variable into temp: dest = locals[src1]
              WriteMovRegMem(FCode, RAX, RBP, SlotOffset(instr.Src1));
              slotIdx := localCnt + instr.Dest;
              WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), RAX);
            end;
          irLoadField:
             begin
               // Compute address of the field: RCX = RBP + SlotOffset(src1) + (fieldIndex * 8)
               WriteLeaRegMemOffset(FCode, RCX, RBP, SlotOffset(instr.Src1) + 8 * Integer(instr.ImmInt));
               // Load value from [RCX] into RAX
               WriteMovRegMem(FCode, RAX, RCX, 0); // RAX = [RCX]
               // Store RAX to dest slot
               slotIdx := localCnt + instr.Dest;
               WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), RAX);
             end;
          irStoreField:
             begin
               // Load value to store into RBX
               slotIdx := localCnt + instr.Src2;
               WriteMovRegMem(FCode, RBX, RBP, SlotOffset(slotIdx)); // RBX = value to store

               // Compute address of the field: RCX = RBP + SlotOffset(src1) + (fieldIndex * 8)
               WriteLeaRegMemOffset(FCode, RCX, RBP, SlotOffset(instr.Src1) + 8 * Integer(instr.ImmInt));
               // Store RBX to [RCX]
               WriteMovMemReg(FCode, RCX, 0, RBX); // [RCX] = RBX
             end;
           irSExt:
            begin
              // sign-extend src1 (width in ImmInt) into dest using shl/sar sequence
              slotIdx := localCnt + instr.Src1;
              WriteMovRegMem(FCode, RAX, RBP, SlotOffset(slotIdx));
              // if width < 64: shift left by (64-width) and arithmetic shift right back
              if instr.ImmInt < 64 then
              begin
                sh := 64 - instr.ImmInt;
                // shl rax, sh  -> 48 C1 E0 sh
                EmitU8(FCode, $48); EmitU8(FCode, $C1); EmitU8(FCode, $E0); EmitU8(FCode, Byte(sh));
                // sar rax, sh  -> 48 C1 F8 sh
                EmitU8(FCode, $48); EmitU8(FCode, $C1); EmitU8(FCode, $F8); EmitU8(FCode, Byte(sh));
              end;
              WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
            end;
         irZExt:
           begin
             // zero-extend src1 (width in ImmInt) into dest
             slotIdx := localCnt + instr.Src1;
             case instr.ImmInt of
               8: WriteMovzxRegMem8(FCode, RAX, RBP, SlotOffset(slotIdx));
               16: WriteMovzxRegMem16(FCode, RAX, RBP, SlotOffset(slotIdx));
                32:
                  begin
                    // mov eax, dword ptr [base+disp] zero-extends into rax implicitly
                    WriteMovEAXMem32(FCode, RBP, SlotOffset(slotIdx));
                    // result already zero-extended into RAX
                  end;
              else
                WriteMovRegMem(FCode, RAX, RBP, SlotOffset(slotIdx));
              end;
             WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
           end;
         irTrunc:
           begin
             // truncate src1 to ImmInt bits and store to dest
             slotIdx := localCnt + instr.Src1;
             WriteMovRegMem(FCode, RAX, RBP, SlotOffset(slotIdx));
              if instr.ImmInt < 64 then
              begin
                // mask lower bits
                mask64 := (UInt64(1) shl instr.ImmInt) - 1;
                EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $E0); // and rax, imm32
                EmitU32(FCode, Cardinal(mask64 and $FFFFFFFF));
              end;

             WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
           end;

         irStoreLocal:
           begin
             // Store temp into local variable: locals[dest] = src1
             WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
             WriteMovMemReg(FCode, RBP, SlotOffset(instr.Dest), RAX);
           end;
         irCast:
           begin
             // Type cast: dest = cast(src1, fromType, toType)
             case instr.CastFromType of
                atInt64:
                  case instr.CastToType of
                    atInt64:
                      begin
                        // Identity cast: int64 -> int64
                        WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
                        WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
                      end;
                    atF64:
                      begin
                        // Integer to Float: int64 -> f64
                        WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
                        // cvtsi2sd xmm0, rax - Convert signed integer to double
                        EmitU8(FCode, $F2); EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $2A); EmitU8(FCode, $C0);
                        // Store XMM0 to destination slot
                        WriteMovsdMemXmm(FCode, RBP, SlotOffset(localCnt + instr.Dest), 0);
                      end;
                    atDate, atTime, atDateTime, atTimestamp:
                      begin
                        // int64 -> time type: just copy (time types stored as int64)
                        WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
                        WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
                      end;
                    else
                      begin
                        // Unsupported cast - just copy for now
                        WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
                        WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
                      end;
                  end;
                atF64:
                  case instr.CastToType of
                    atF64:
                      begin
                        // Identity cast: f64 -> f64
                        WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
                        WriteMovsdMemXmm(FCode, RBP, SlotOffset(localCnt + instr.Dest), 0);
                      end;
                    atInt64:
                      begin
                        // Float to Integer: f64 -> int64 (with truncation)
                        WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
                        // cvttsd2si rax, xmm0 - Convert with truncation toward zero
                        EmitU8(FCode, $F2); EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $2C); EmitU8(FCode, $C0);
                        WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
                      end;
                    else
                      begin
                        // Unsupported cast - copy as raw bytes
                        WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
                        WriteMovsdMemXmm(FCode, RBP, SlotOffset(localCnt + instr.Dest), 0);
                      end;
                  end;
                atDate, atTime, atDateTime, atTimestamp:
                  case instr.CastToType of
                    atInt64:
                      begin
                        // time type -> int64: just copy
                        WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
                        WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
                      end;
                    atDate, atTime, atDateTime, atTimestamp:
                      begin
                        // time type -> time type: just copy (all stored as int64)
                        WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
                        WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
                      end;
                    else
                      begin
                        // Unsupported cast - copy as raw bytes
                        WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
                        WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
                      end;
                  end;
               else
                 begin
                   // Default: simple copy for other types
                   WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
                   WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
                 end;
             end;
           end;
         irAdd:
          begin
            // dest = src1 + src2
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            WriteAddRegReg(FCode, RAX, RCX);
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
        irSub:
          begin
            // dest = src1 - src2
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            WriteSubRegReg(FCode, RAX, RCX);
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
        irMul:
          begin
            // dest = src1 * src2 (signed multiplication)
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            WriteImulRegReg(FCode, RAX, RCX);
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
        irDiv:
          begin
            // dest = src1 / src2 (signed division)
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            WriteCqo(FCode);
            WriteIdivReg(FCode, RCX);
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
        irMod:
          begin
            // dest = src1 % src2 (remainder, stored in RDX after idiv)
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            WriteCqo(FCode);
            WriteIdivReg(FCode, RCX);
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RDX);
          end;
        irNeg:
          begin
            // dest = -src1
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            EmitU8(FCode, $48); EmitU8(FCode, $F7); EmitU8(FCode, $D8); // neg rax
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
        irCmpEq:
          begin
            // dest = (src1 == src2) ? 1 : 0
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            WriteSubRegReg(FCode, RAX, RCX);  // rax = src1 - src2
            EmitU8(FCode, $0F); EmitU8(FCode, $94); EmitU8(FCode, $C0); // sete al
            EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0); // movzx rax, al
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
        irCmpNeq:
          begin
            // dest = (src1 != src2) ? 1 : 0
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            WriteSubRegReg(FCode, RAX, RCX);
            EmitU8(FCode, $0F); EmitU8(FCode, $95); EmitU8(FCode, $C0); // setne al
            EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0); // movzx rax, al
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
        irCmpLt:
          begin
            // dest = (src1 < src2) ? 1 : 0 (signed)
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            WriteSubRegReg(FCode, RAX, RCX);
            EmitU8(FCode, $0F); EmitU8(FCode, $9C); EmitU8(FCode, $C0); // setl al
            EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0); // movzx rax, al
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
        irCmpLe:
          begin
            // dest = (src1 <= src2) ? 1 : 0 (signed)
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            WriteSubRegReg(FCode, RAX, RCX);
            EmitU8(FCode, $0F); EmitU8(FCode, $9E); EmitU8(FCode, $C0); // setle al
            EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0); // movzx rax, al
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
        irCmpGt:
          begin
            // dest = (src1 > src2) ? 1 : 0 (signed)
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            WriteSubRegReg(FCode, RAX, RCX);
            EmitU8(FCode, $0F); EmitU8(FCode, $9F); EmitU8(FCode, $C0); // setg al
            EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0); // movzx rax, al
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
        irCmpGe:
          begin
            // dest = (src1 >= src2) ? 1 : 0 (signed)
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            WriteSubRegReg(FCode, RAX, RCX);
            EmitU8(FCode, $0F); EmitU8(FCode, $9D); EmitU8(FCode, $C0); // setge al
            EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0); // movzx rax, al
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
        irNot:
          begin
            // dest = !src1 (logical not: 1 if src1==0, else 0)
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteTestRaxRax(FCode);
            EmitU8(FCode, $0F); EmitU8(FCode, $94); EmitU8(FCode, $C0); // sete al
            EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0); // movzx rax, al
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
        irAnd:
          begin
            // dest = src1 & src2 (bitwise AND)
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            EmitU8(FCode, $48); EmitU8(FCode, $21); EmitU8(FCode, $C8); // and rax, rcx
            WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
         irOr:
           begin
             // dest = src1 | src2 (bitwise OR)
             WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
             WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
             EmitU8(FCode, $48); EmitU8(FCode, $09); EmitU8(FCode, $C8); // or rax, rcx
             WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
           end;
         { Float arithmetic using SSE2 }
         irFAdd:
           begin
             // dest = src1 + src2 (double)
             // Load src1 into XMM0
             WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
             // Load src2 into XMM1
             WriteMovsdXmmMem(FCode, 1, RBP, SlotOffset(localCnt + instr.Src2));
             // addsd xmm0, xmm1
             WriteAddsdXmmXmm(FCode, 0, 1);
             // store result
             WriteMovsdMemXmm(FCode, RBP, SlotOffset(localCnt + instr.Dest), 0);
           end;
         irFSub:
           begin
             // dest = src1 - src2 (double)
             WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
             WriteMovsdXmmMem(FCode, 1, RBP, SlotOffset(localCnt + instr.Src2));
             WriteSubsdXmmXmm(FCode, 0, 1);
             WriteMovsdMemXmm(FCode, RBP, SlotOffset(localCnt + instr.Dest), 0);
           end;
         irFMul:
           begin
             // dest = src1 * src2 (double)
             WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
             WriteMovsdXmmMem(FCode, 1, RBP, SlotOffset(localCnt + instr.Src2));
             WriteMulsdXmmXmm(FCode, 0, 1);
             WriteMovsdMemXmm(FCode, RBP, SlotOffset(localCnt + instr.Dest), 0);
           end;
         irFDiv:
           begin
             // dest = src1 / src2 (double)
             WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
             WriteMovsdXmmMem(FCode, 1, RBP, SlotOffset(localCnt + instr.Src2));
             WriteDivsdXmmXmm(FCode, 0, 1);
             WriteMovsdMemXmm(FCode, RBP, SlotOffset(localCnt + instr.Dest), 0);
           end;
         irFNeg:
           begin
             // dest = -src1 (double) - flip sign bit using XOR with sign mask
             // We need a memory location with 0x8000000000000000
             // Use a temporary approach: load into XMM0, XOR with XMM1 (which has sign bit)
             // First, create sign mask in RAX
             WriteMovRegImm64(FCode, RAX, UInt64($8000000000000000));
             WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX); // use dest slot temporarily
             // Load value into XMM0
             WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
             // Load sign mask into XMM1
             WriteMovsdXmmMem(FCode, 1, RBP, SlotOffset(localCnt + instr.Dest));
             // XOR to flip sign bit
             WriteXorpdXmmXmm(FCode, 0, 1);
             // Store result
             WriteMovsdMemXmm(FCode, RBP, SlotOffset(localCnt + instr.Dest), 0);
           end;
         { Float comparisons using SSE2 }
         irFCmpEq:
           begin
             // dest = (src1 == src2) ? 1 : 0
             WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
             WriteMovsdXmmMem(FCode, 1, RBP, SlotOffset(localCnt + instr.Src2));
             WriteUcomisdXmmXmm(FCode, 0, 1);
             EmitU8(FCode, $0F); EmitU8(FCode, $94); EmitU8(FCode, $C0); // sete al
             EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0); // movzx rax, al
             WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
           end;
         irFCmpNeq:
           begin
             // dest = (src1 != src2) ? 1 : 0
             WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
             WriteMovsdXmmMem(FCode, 1, RBP, SlotOffset(localCnt + instr.Src2));
             WriteUcomisdXmmXmm(FCode, 0, 1);
             EmitU8(FCode, $0F); EmitU8(FCode, $95); EmitU8(FCode, $C0); // setne al
             EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0);
             WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
           end;
         irFCmpLt:
           begin
             // dest = (src1 < src2) ? 1 : 0
             WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
             WriteMovsdXmmMem(FCode, 1, RBP, SlotOffset(localCnt + instr.Src2));
             WriteUcomisdXmmXmm(FCode, 0, 1);
             EmitU8(FCode, $0F); EmitU8(FCode, $92); EmitU8(FCode, $C0); // setb al (below, for unsigned result of ucomisd)
             EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0);
             WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
           end;
         irFCmpLe:
           begin
             // dest = (src1 <= src2) ? 1 : 0
             WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
             WriteMovsdXmmMem(FCode, 1, RBP, SlotOffset(localCnt + instr.Src2));
             WriteUcomisdXmmXmm(FCode, 0, 1);
             EmitU8(FCode, $0F); EmitU8(FCode, $96); EmitU8(FCode, $C0); // setbe al (below or equal)
             EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0);
             WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
           end;
         irFCmpGt:
           begin
             // dest = (src1 > src2) ? 1 : 0
             // For ucomisd, we check the flags differently: swap operands and use setb
             WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
             WriteMovsdXmmMem(FCode, 1, RBP, SlotOffset(localCnt + instr.Src2));
             // Compare src2 < src1 instead of src1 > src2
             WriteUcomisdXmmXmm(FCode, 1, 0);
             EmitU8(FCode, $0F); EmitU8(FCode, $92); EmitU8(FCode, $C0); // setb al
             EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0);
             WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
           end;
         irFCmpGe:
           begin
             // dest = (src1 >= src2) ? 1 : 0
             WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
             WriteMovsdXmmMem(FCode, 1, RBP, SlotOffset(localCnt + instr.Src2));
             // Compare src2 <= src1 instead of src1 >= src2
             WriteUcomisdXmmXmm(FCode, 1, 0);
             EmitU8(FCode, $0F); EmitU8(FCode, $96); EmitU8(FCode, $C0); // setbe al
             EmitU8(FCode, $48); EmitU8(FCode, $0F); EmitU8(FCode, $B6); EmitU8(FCode, $C0);
             WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
           end;
         { Float/Int conversions }
         irFToI:
           begin
             // dest = (int64)src1 - convert double to int64
             WriteMovsdXmmMem(FCode, 0, RBP, SlotOffset(localCnt + instr.Src1));
             WriteCvtsd2siRaxXmm(FCode, 0);
             WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
           end;
         irIToF:
           begin
             // dest = (double)src1 - convert int64 to double
             WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
             WriteCvtsi2sdXmmRax(FCode, 0);
             WriteMovsdMemXmm(FCode, RBP, SlotOffset(localCnt + instr.Dest), 0);
           end;
         irReturn:
          begin
            // Move return value into RAX (non-entry) or RDI (entry) if provided
            if isEntryFunction then
            begin
              if instr.Src1 >= 0 then
                WriteMovRegMem(FCode, RDI, RBP, SlotOffset(localCnt + instr.Src1));
            end
            else
            begin
              if instr.Src1 >= 0 then
                WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            end;

            // Fix stack: add frameBytes+framePad back to RSP if we allocated
            if frameBytes + framePad > 0 then
            begin
              if frameBytes + framePad <= 127 then
              begin
                EmitU8(FCode, $48); EmitU8(FCode, $83); EmitU8(FCode, $C4); EmitU8(FCode, Byte(frameBytes + framePad));
              end
              else
              begin
                EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $C4);
                EmitU32(FCode, Cardinal(frameBytes + framePad));
              end;
            end;

            if isEntryFunction then
            begin
              // sys_exit
              WriteMovRegImm64(FCode, RAX, 60);
              WriteSyscall(FCode);
            end
            else
            begin
              // leave; ret
              EmitU8(FCode, $C9); // leave
              EmitU8(FCode, $C3); // ret
            end;
          end;
        irLabel:
          begin
            // Record current position for this label
            SetLength(FLabelPositions, Length(FLabelPositions) + 1);
            FLabelPositions[High(FLabelPositions)].Name := instr.LabelName;
            FLabelPositions[High(FLabelPositions)].Pos := FCode.Size;
          end;
        irJmp:
          begin
            // Unconditional jump to label
            SetLength(FJumpPatches, Length(FJumpPatches) + 1);
            FJumpPatches[High(FJumpPatches)].Pos := FCode.Size;
            FJumpPatches[High(FJumpPatches)].LabelName := instr.LabelName;
            FJumpPatches[High(FJumpPatches)].JmpSize := 5; // jmp rel32
            WriteJmpRel32(FCode, 0); // placeholder
          end;
        irBrTrue:
          begin
            // Jump to label if src1 != 0
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteTestRaxRax(FCode);
            SetLength(FJumpPatches, Length(FJumpPatches) + 1);
            FJumpPatches[High(FJumpPatches)].Pos := FCode.Size;
            FJumpPatches[High(FJumpPatches)].LabelName := instr.LabelName;
            FJumpPatches[High(FJumpPatches)].JmpSize := 6; // jne rel32
            WriteJneRel32(FCode, 0); // placeholder
          end;
        irBrFalse:
          begin
            // Jump to label if src1 == 0
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteTestRaxRax(FCode);
            SetLength(FJumpPatches, Length(FJumpPatches) + 1);
            FJumpPatches[High(FJumpPatches)].Pos := FCode.Size;
            FJumpPatches[High(FJumpPatches)].LabelName := instr.LabelName;
            FJumpPatches[High(FJumpPatches)].JmpSize := 6; // je rel32
            WriteJeRel32(FCode, 0); // placeholder
          end;
         irCall:
           begin
             // v0.2.0: Unified Call Lowering - SysV ABI compliant
             // Supports: internal calls, imported calls, external (libc) calls
             argCount := instr.ImmInt;

             // Build argTemps from dedicated ArgTemps array (v0.2.0) with fallback to Src1/Src2
             SetLength(argTemps, argCount);
             for k := 0 to argCount - 1 do
             begin
               if k < Length(instr.ArgTemps) then
                 argTemps[k] := instr.ArgTemps[k]
               else if k = 0 then
                 argTemps[k] := instr.Src1
               else if k = 1 then
                 argTemps[k] := instr.Src2
               else
                 argTemps[k] := -1;
             end;

             // Determine effective call mode
             // If symbol is external (not found in local labels and not a builtin), treat as external
             if instr.CallMode = cmExternal then
             begin
               // External call - will use PLT stub
               AddExternalSymbol(instr.ImmStr, GetLibraryForSymbol(instr.ImmStr));
             end
             else if (instr.CallMode = cmInternal) and IsExternalSymbol(instr.ImmStr) then
             begin
               // Auto-detect: symbol not found locally -> treat as external
               instr.CallMode := cmExternal;
               AddExternalSymbol(instr.ImmStr, GetLibraryForSymbol(instr.ImmStr));
             end;

             // --- Argument Passing (SysV AMD64 ABI) ---
             // Up to 6 arguments in registers, rest on stack
             // RDI, RSI, RDX, RCX, R8, R9 for integer/pointer args

             // Move args into registers (SysV: RDI, RSI, RDX, RCX, R8, R9)
             if argCount > 0 then
             begin
               // Load args from slots into parameter registers
               if (argCount >= 1) and (argTemps[0] >= 0) then WriteMovRegMem(FCode, 7, RBP, SlotOffset(localCnt + argTemps[0])); // RDI
               if (argCount >= 2) and (argTemps[1] >= 0) then WriteMovRegMem(FCode, 6, RBP, SlotOffset(localCnt + argTemps[1])); // RSI
               if (argCount >= 3) and (argTemps[2] >= 0) then WriteMovRegMem(FCode, 2, RBP, SlotOffset(localCnt + argTemps[2])); // RDX
               if (argCount >= 4) and (argTemps[3] >= 0) then WriteMovRegMem(FCode, 1, RBP, SlotOffset(localCnt + argTemps[3])); // RCX
               if (argCount >= 5) and (argTemps[4] >= 0) then WriteMovRegMem(FCode, 8, RBP, SlotOffset(localCnt + argTemps[4])); // R8
               if (argCount >= 6) and (argTemps[5] >= 0) then WriteMovRegMem(FCode, 9, RBP, SlotOffset(localCnt + argTemps[5])); // R9
             end;

            // handle extra args >6: push them in reverse order onto stack
            extraCount := 0;
            if argCount > 6 then extraCount := argCount - 6;
            pushBytes := 0;
            if extraCount > 0 then
            begin
              // push args from last to first (arg_n ... arg_7)
              for k := argCount - 1 downto 6 do
              begin
                if argTemps[k] < 0 then Continue;
                WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + argTemps[k]));
                EmitU8(FCode, $50 + (RAX and $7));
                Inc(pushBytes, 8);
              end;
            end;
            callPad := (8 - (pushBytes mod 16)) mod 16;
            if callPad > 0 then
            begin
              EmitU8(FCode, $48); EmitU8(FCode, $83); EmitU8(FCode, $EC); EmitU8(FCode, Byte(callPad));
            end;

             // Set AL=0 for varargs calls (SysV ABI: number of vector registers used)
             // Since we don't use XMM registers for arguments, AL should always be 0
             EmitU8(FCode, $30); EmitU8(FCode, $C0); // xor al, al

             // --- Emit Call based on Call Mode ---
             case instr.CallMode of
               cmExternal:
                 begin
                   // External call via PLT stub
                   // Call to symbol@plt which will be resolved by dynamic linker
                   SetLength(FJumpPatches, Length(FJumpPatches) + 1);
                   FJumpPatches[High(FJumpPatches)].Pos := FCode.Size;
                   FJumpPatches[High(FJumpPatches)].LabelName := instr.ImmStr + '@plt';
                   FJumpPatches[High(FJumpPatches)].JmpSize := 5; // call rel32
                   EmitU8(FCode, $E8); // call rel32
                   EmitU32(FCode, 0);  // placeholder offset - patched to PLT stub
                 end;
               cmImported:
                 begin
                   // Cross-unit imported function
                   // Similar to external but from known unit - also uses PLT for now
                   SetLength(FJumpPatches, Length(FJumpPatches) + 1);
                   FJumpPatches[High(FJumpPatches)].Pos := FCode.Size;
                   FJumpPatches[High(FJumpPatches)].LabelName := instr.ImmStr;
                   FJumpPatches[High(FJumpPatches)].JmpSize := 5;
                   EmitU8(FCode, $E8);
                   EmitU32(FCode, 0);
                 end;
               cmInternal:
                 begin
                   // Direct internal call - relative offset to local function
                   SetLength(FJumpPatches, Length(FJumpPatches) + 1);
                   FJumpPatches[High(FJumpPatches)].Pos := FCode.Size;
                   FJumpPatches[High(FJumpPatches)].LabelName := instr.ImmStr;
                   FJumpPatches[High(FJumpPatches)].JmpSize := 5; // call rel32
                   EmitU8(FCode, $E8); // call rel32
                   EmitU32(FCode, 0);  // placeholder offset
                 end;
             end;

            // restore stack: remove padding + extra pushed args
            restoreBytes := callPad + pushBytes;
            if restoreBytes > 0 then
            begin
              if restoreBytes <= 127 then
              begin
                EmitU8(FCode, $48); EmitU8(FCode, $83); EmitU8(FCode, $C4); EmitU8(FCode, Byte(restoreBytes));
              end
              else
              begin
                EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $C4);
                EmitU32(FCode, Cardinal(restoreBytes));
              end;
            end;

            // Store result from RAX if there's a destination
            if instr.Dest >= 0 then
              WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
          end;
         irStackAlloc:
           begin
             // Allocate stack space for array: alloc_size = ImmInt bytes
             allocSize := instr.ImmInt;
             // Align to 8-byte boundary
             allocSize := (allocSize + 7) and not 7;
             
             // Move current RSP down by allocSize bytes: sub rsp, allocSize  
             if allocSize <= 127 then
             begin
               EmitU8(FCode, $48); EmitU8(FCode, $83); EmitU8(FCode, $EC); EmitU8(FCode, Byte(allocSize));
             end
             else
             begin
               EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $EC);
               EmitU32(FCode, Cardinal(allocSize));
             end;
             
             // Store current RSP as array base address in temp slot
             slotIdx := localCnt + instr.Dest;
             // Debug print: show slot index being written
             WriteLn('EMIT: irStackAlloc destTemp=', instr.Dest, ' slotIdx=', slotIdx, ' allocSize=', allocSize);
             WriteMovRegReg(FCode, RAX, RSP); // mov rax, rsp
             WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), RAX);
              // dump last bytes for debugging
              dumpStart := FCode.Size - 48;
              if dumpStart < 0 then dumpStart := 0;
              hexs := '';
              for di := dumpStart to FCode.Size - 1 do
                hexs := hexs + Format('%02x ', [FCode.ReadU8(di)]);
              WriteLn('EMIT DUMP around irStackAlloc: codeSize=', FCode.Size, ' bytes: ', hexs);
            end;

         // Exception handler operations
          irPushHandler:
            begin
              // handlerAddr = [rbp + SlotOffset(localCnt + instr.Src1)]
              slotIdx := localCnt + instr.Src1;
              WriteLn('EMIT: irPushHandler srcTemp=', instr.Src1, ' slotIdx=', slotIdx);
              WriteMovRegMem(FCode, RAX, RBP, SlotOffset(slotIdx));
              // Load pointer to HandlerHead via lea [rip+disp32]; patch later
              leaPos := FCode.Size;
              WriteLeaRegRipDisp(FCode, RDX, 0);
              SetLength(FHandlerHeadLoadPositions, Length(FHandlerHeadLoadPositions) + 1);
              FHandlerHeadLoadPositions[High(FHandlerHeadLoadPositions)] := leaPos;
              // Load current HandlerHead into RCX
              WriteMovRegMem(FCode, RCX, RDX, 0);
              // Store previous handler pointer into new frame
              WriteMovMemReg(FCode, RAX, 0, RCX);
              // Save RSP and RBP
              WriteMovMemReg(FCode, RAX, 8, RSP);
              WriteMovMemReg(FCode, RAX, 16, RBP);
              // Register handler label
              handlerId := Length(FHandlerLabels);
              SetLength(FHandlerLabels, handlerId + 1);
              FHandlerLabels[handlerId] := instr.LabelName;
              // mov rcx, handlerId
              WriteMovRegImm64(FCode, RCX, handlerId);
              // mov [rax + 24], rcx
              WriteMovMemReg(FCode, RAX, 24, RCX);
              // Update global HandlerHead pointer
              WriteMovMemReg(FCode, RDX, 0, RAX);
            end;


          irPopHandler:
            begin
              // handlerAddr = [rbp + SlotOffset(localCnt + instr.Src1)]
              WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
              // Load prev = [rax + 0]
              WriteMovRegMem(FCode, RDX, RAX, 0);
              // Load pointer to HandlerHead via lea [rip+disp32]; patch later
              leaPos := FCode.Size;
              WriteLeaRegRipDisp(FCode, RCX, 0);
              SetLength(FHandlerHeadStorePositions, Length(FHandlerHeadStorePositions) + 1);
              FHandlerHeadStorePositions[High(FHandlerHeadStorePositions)] := leaPos;
              // Store prev into global HandlerHead
              WriteMovMemReg(FCode, RCX, 0, RDX);
            end;


         irLoadHandlerExn:
           begin
             // handlerAddr = [rbp + SlotOffset(localCnt + instr.Src1)]
             WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
             // Load exn value at [rax + 32]
             WriteMovRegMem(FCode, RDX, RAX, 32);
             // Store into dest local slot
             slotIdx := localCnt + instr.Dest;
             WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), RDX);
           end;

          irThrow:
            begin
              // Load exception value from local temp
              WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
              // Load pointer to HandlerHead via lea [rip+disp32]; patch later
              leaPos := FCode.Size;
              WriteLeaRegRipDisp(FCode, RDX, 0);
              SetLength(FHandlerHeadLoadPositions, Length(FHandlerHeadLoadPositions) + 1);
              FHandlerHeadLoadPositions[High(FHandlerHeadLoadPositions)] := leaPos;
              // Load HandlerHead into RCX
              WriteMovRegMem(FCode, RCX, RDX, 0);
              // test rcx, rcx
              EmitU8(FCode, $48); EmitU8(FCode, $85); EmitU8(FCode, $C9);
              // if zero -> exit(1)
              // je uncaught
              uncaughtPos := FCode.Size;
              WriteJeRel32(FCode, 0);
              // store exception into [rcx+32]
              WriteMovMemReg(FCode, RCX, 32, RAX);
              // load handlerId = [rcx+24]
              WriteMovRegMem(FCode, RDX, RCX, 24);
              // compute table base via lea [rip+disp32]; patch later
              leaPos := FCode.Size;
              WriteLeaRegRipDisp(FCode, RSI, 0);
              SetLength(FHandlerTableLeaPositions, Length(FHandlerTableLeaPositions) + 1);
              FHandlerTableLeaPositions[High(FHandlerTableLeaPositions)] := leaPos;
              // index = handlerId << 3
              WriteMovRegReg(FCode, RAX, RDX);
              EmitU8(FCode, $48); EmitU8(FCode, $C1); EmitU8(FCode, $E0); EmitU8(FCode, $03);
              // add table base to index
              WriteAddRegReg(FCode, RAX, RSI);
              // load target = [rax]
              WriteMovRegMem(FCode, RDX, RAX, 0);
             // jmp rdx
             EmitU8(FCode, $FF); EmitU8(FCode, $E2);
             // uncaught: patch here
             k := FCode.Size;
             FCode.PatchU32LE(uncaughtPos + 2, Cardinal(k - uncaughtPos - 6));
             // uncaught: exit(1)
             WriteMovRegImm64(FCode, RDI, 1);
             WriteMovRegImm64(FCode, RAX, 60);
             WriteSyscall(FCode);
           end;
        irStoreElem:
          begin
            // Store element: array[index] = value
            // Src1 = array base address temp, Src2 = value temp, ImmInt = index
            elemIndex := instr.ImmInt;
            elemOffset := elemIndex * 8; // 8 bytes per element
            
            // Load array base address into RAX
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            // Load element value into RCX
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            // Store value at array[index]: mov [rax + elemOffset], rcx
            if elemOffset <= 127 then
            begin
              EmitU8(FCode, $48); EmitU8(FCode, $89); EmitU8(FCode, $48); EmitU8(FCode, Byte(elemOffset));
            end
            else
            begin
              EmitU8(FCode, $48); EmitU8(FCode, $89); EmitU8(FCode, $88);
              EmitU32(FCode, Cardinal(elemOffset));
            end;
          end;
        irLoadElem:
          begin
            // Load element: dest = array[index]
            // Src1 = array base address temp, Src2 = index temp, Dest = result
            
            // Load array base address into RAX
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            // Load index into RCX
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            
            // Calculate element address: RAX = RAX + RCX * 8
            // shl rcx, 3   (multiply index by 8)
            EmitU8(FCode, $48); EmitU8(FCode, $C1); EmitU8(FCode, $E1); EmitU8(FCode, $03);
            // add rax, rcx (add scaled offset)
            EmitU8(FCode, $48); EmitU8(FCode, $01); EmitU8(FCode, $C8);
            
            // Load value from calculated address: RCX = [RAX]
            EmitU8(FCode, $48); EmitU8(FCode, $8B); EmitU8(FCode, $08);
            
            // Store result in destination temp slot
            slotIdx := localCnt + instr.Dest;
            WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), RCX);
          end;
        irStoreElemDyn:
          begin
            // Store element dynamically: array[index] = value
            // Src1 = array base, Src2 = index, Src3 = value
            
            // Load array base address into RAX
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            // Load index into RCX
            WriteMovRegMem(FCode, RCX, RBP, SlotOffset(localCnt + instr.Src2));
            // Load value into RDX
            WriteMovRegMem(FCode, RDX, RBP, SlotOffset(localCnt + instr.Src3));
            
            // Calculate element address: RAX = RAX + RCX * 8
            // shl rcx, 3   (multiply index by 8)
            EmitU8(FCode, $48); EmitU8(FCode, $C1); EmitU8(FCode, $E1); EmitU8(FCode, $03);
            // add rax, rcx (add scaled offset)
            EmitU8(FCode, $48); EmitU8(FCode, $01); EmitU8(FCode, $C8);
            
            // Store value at calculated address: [RAX] = RDX
            EmitU8(FCode, $48); EmitU8(FCode, $89); EmitU8(FCode, $10);
          end;
      end;
    end;
  end;

  // patch string LEAs
  for i := 0 to High(FLeaPositions) do
  begin
    leaPos := FLeaPositions[i];
    sidx := FLeaStrIndex[i];
      if (sidx >= 0) and (sidx < Length(FStringOffsets)) then
     begin
       codeVA := $400000 + 4096;
       instrVA := codeVA + leaPos + 7;
       dataVA := $400000 + 4096 + ((UInt64(FCode.Size) + 4095) and not UInt64(4095)) + FStringOffsets[sidx];
       disp32 := Int64(dataVA) - Int64(instrVA);
        FCode.PatchU32LE(leaPos + 3, Cardinal(disp32));
     end;
  end;

   // patch buffer LEAs for print_int
   if bufferAdded then
   begin
     for i := 0 to High(bufferLeaPositions) do
     begin
       leaPos := bufferLeaPositions[i];
       codeVA := $400000 + 4096;
       instrVA := codeVA + leaPos + 7;
       dataVA := $400000 + 4096 + ((UInt64(FCode.Size) + 4095) and not UInt64(4095)) + bufferOffset;
       disp32 := Int64(dataVA) - Int64(instrVA);
       FCode.PatchU32LE(leaPos + 3, Cardinal(disp32));
     end;
   end;

   // patch env LEAs
   if envAdded then
   begin
     for i := 0 to High(envLeaPositions) do
     begin
       leaPos := envLeaPositions[i];
       codeVA := $400000 + 4096;
       instrVA := codeVA + leaPos + 7;
       dataVA := $400000 + 4096 + ((UInt64(FCode.Size) + 4095) and not UInt64(4095)) + envOffset;
       disp32 := Int64(dataVA) - Int64(instrVA);
       FCode.PatchU32LE(leaPos + 3, Cardinal(disp32));
     end;
   end;

    // patch handler head LEAs
    if (Length(FHandlerHeadLoadPositions) > 0) or (Length(FHandlerHeadStorePositions) > 0) then
    begin
      // HandlerHead data offset within data section
      handlerHeadOffset := totalDataOffset;
      // Reserve 8 bytes for HandlerHead pointer slot
      FData.WriteU64LE(0);
      Inc(totalDataOffset, 8);
      for i := 0 to High(FHandlerHeadLoadPositions) do
      begin
        leaPos := FHandlerHeadLoadPositions[i];
        codeVA := $400000 + 4096;
        instrVA := codeVA + leaPos + 7;
        dataVA := $400000 + 4096 + ((UInt64(FCode.Size) + 4095) and not UInt64(4095)) + handlerHeadOffset;
        disp32 := Int64(dataVA) - Int64(instrVA);
        FCode.PatchU32LE(leaPos + 3, Cardinal(LongInt(disp32)));
      end;
      for i := 0 to High(FHandlerHeadStorePositions) do
      begin
        leaPos := FHandlerHeadStorePositions[i];
        codeVA := $400000 + 4096;
        instrVA := codeVA + leaPos + 7;
        dataVA := $400000 + 4096 + ((UInt64(FCode.Size) + 4095) and not UInt64(4095)) + handlerHeadOffset;
        disp32 := Int64(dataVA) - Int64(instrVA);
        FCode.PatchU32LE(leaPos + 3, Cardinal(LongInt(disp32)));
      end;
    end;


   // build handler table in data and patch lea positions to it
   if Length(FHandlerLabels) > 0 then
   begin
     handlerTableDataPos := FData.Size;
     handlerTableOffset := totalDataOffset;
     // reserve table entries (8 bytes each)
     for i := 0 to High(FHandlerLabels) do
     begin
       FData.WriteU64LE(0);
       Inc(totalDataOffset, 8);
     end;
     // Now patch each table entry with the label address
     codeVA := $400000 + 4096;
     for i := 0 to High(FHandlerLabels) do
     begin
       // find label position
       targetPos := -1;
       for j := 0 to High(FLabelPositions) do
       begin
         if FLabelPositions[j].Name = FHandlerLabels[i] then
         begin
           targetPos := FLabelPositions[j].Pos;
           Break;
         end;
       end;
       if targetPos < 0 then
         Raise Exception.CreateFmt('Undefined handler label: %s', [FHandlerLabels[i]]);
       // label virtual address
       dataVA := codeVA + targetPos;
       // patch data at handlerTableDataPos + i*8
       FData.PatchU64LE(handlerTableDataPos + i*8, dataVA);
     end;
     // Patch any lea positions that reference the table base
      for i := 0 to High(FHandlerTableLeaPositions) do
      begin
        leaPos := FHandlerTableLeaPositions[i];
        codeVA := $400000 + 4096;
        instrVA := codeVA + leaPos + 7;
        dataVA := $400000 + 4096 + ((UInt64(FCode.Size) + 4095) and not UInt64(4095)) + handlerTableOffset;
        disp32 := Int64(dataVA) - Int64(instrVA);
        FCode.PatchU32LE(leaPos + 3, Cardinal(LongInt(disp32)));
      end;

   end;

    // Generate PLT stubs for external symbols at the end of code
    GeneratePLTStubs;

    // patch jumps to labels (v0.2.0: unified call patching)
    // DEBUG: Dump jump patches
    WriteLn('DEBUG: ', Length(FJumpPatches), ' jump patches to apply');
    for i := 0 to High(FLabelPositions) do
      WriteLn('DEBUG: Label ', FLabelPositions[i].Name, ' at pos ', FLabelPositions[i].Pos);
    for i := 0 to High(FJumpPatches) do
    begin
      // find target label position (check normal labels, PLT stubs, and PLT@suffix)
      targetPos := -1;
      labelName := FJumpPatches[i].LabelName;
      WriteLn('DEBUG: Processing jump patch ', i, ': label=', labelName, ' pos=', FJumpPatches[i].Pos, ' size=', FJumpPatches[i].JmpSize);

     // Check if this is a PLT call (symbol@plt)
     isPLTCall := Pos('@plt', labelName) > 0;
     if isPLTCall then
     begin
       // Strip @plt suffix for lookup
       pltBaseName := Copy(labelName, 1, Pos('@plt', labelName) - 1);
       for j := 0 to High(FPLTStubs) do
       begin
         if FPLTStubs[j].Name = labelName then
         begin
           targetPos := FPLTStubs[j].Pos;
           Break;
         end;
       end;
     end
     else
     begin
       // Normal label lookup
       for j := 0 to High(FLabelPositions) do
       begin
         if FLabelPositions[j].Name = labelName then
         begin
           targetPos := FLabelPositions[j].Pos;
           Break;
         end;
       end;

       // If not found in normal labels, check PLT stubs (for extern calls without @plt suffix)
       if targetPos < 0 then
       begin
         for j := 0 to High(FPLTStubs) do
         begin
           if FPLTStubs[j].Name = labelName + '@plt' then
           begin
             targetPos := FPLTStubs[j].Pos;
             Break;
           end;
         end;
       end;
     end;
     if targetPos >= 0 then
     begin
       jmpPos := FJumpPatches[i].Pos;
       rel32 := Int64(targetPos) - Int64(jmpPos + FJumpPatches[i].JmpSize);
       WriteLn('DEBUG: Patching jump ', i, ': jmpPos=', jmpPos, ' targetPos=', targetPos, ' rel32=', rel32);

       if FJumpPatches[i].JmpSize = 5 then
         FCode.PatchU32LE(jmpPos + 1, Cardinal(rel32)) // jmp rel32: opcode at pos, rel32 at pos+1
       else
         FCode.PatchU32LE(jmpPos + 2, Cardinal(rel32)); // jcc rel32: opcode 0F xx at pos, rel32 at pos+2
      end
       else
         WriteLn('DEBUG: WARNING - target not found for jump patch ', i, ': ', labelName);
    end;

     // --- Instrumentation: dump generated code + data + label/handler info for debugging ---
    try
      FCode.SaveToFile('/tmp/emit_code.bin');
      FData.SaveToFile('/tmp/emit_data.bin');
      // write textual metadata
      metaFs := TFileStream.Create('/tmp/emit_metadata.txt', fmCreate);
      try
        // write small chunks to avoid very long source lines
        tmp := Format('Code size: %d bytes'#10, [FCode.Size]);
        metaFs.WriteBuffer(Pointer(tmp)^, Length(tmp));
        tmp := Format('Data size: %d bytes'#10, [FData.Size]);
        metaFs.WriteBuffer(Pointer(tmp)^, Length(tmp));
        tmp := 'Labels:'#10;
        metaFs.WriteBuffer(Pointer(tmp)^, Length(tmp));
        for i := 0 to High(FLabelPositions) do
        begin
          tmp := Format(' %s -> pos %d'#10, [FLabelPositions[i].Name, FLabelPositions[i].Pos]);
          metaFs.WriteBuffer(Pointer(tmp)^, Length(tmp));
        end;
        tmp := 'HandlerLabels:'#10;
        metaFs.WriteBuffer(Pointer(tmp)^, Length(tmp));
        for i := 0 to High(FHandlerLabels) do
        begin
          tmp := Format(' %d -> %s'#10, [i, FHandlerLabels[i]]);
          metaFs.WriteBuffer(Pointer(tmp)^, Length(tmp));
        end;
        tmp := 'HandlerHeadLoadPositions:'#10;
        metaFs.WriteBuffer(Pointer(tmp)^, Length(tmp));
        for i := 0 to High(FHandlerHeadLoadPositions) do
        begin
          tmp := Format(' pos %d'#10, [FHandlerHeadLoadPositions[i]]);
          metaFs.WriteBuffer(Pointer(tmp)^, Length(tmp));
        end;
        tmp := 'HandlerHeadStorePositions:'#10;
        metaFs.WriteBuffer(Pointer(tmp)^, Length(tmp));
        for i := 0 to High(FHandlerHeadStorePositions) do
        begin
          tmp := Format(' pos %d'#10, [FHandlerHeadStorePositions[i]]);
          metaFs.WriteBuffer(Pointer(tmp)^, Length(tmp));
        end;
        tmp := 'HandlerTableLeaPositions:'#10;
        metaFs.WriteBuffer(Pointer(tmp)^, Length(tmp));
        for i := 0 to High(FHandlerTableLeaPositions) do
        begin
          tmp := Format(' pos %d -> label %s'#10, [FHandlerTableLeaPositions[i], FHandlerLabels[i]]);
          metaFs.WriteBuffer(Pointer(tmp)^, Length(tmp));
        end;
      finally
        metaFs.Free;
      end;
    except
      // ignore any errors writing debug files
    end;

    // debug dump removed in release
 end;

function TX86_64Emitter.GetFunctionOffset(const name: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FLabelPositions) do
  begin
    if FLabelPositions[i].Name = name then
    begin
      Result := FLabelPositions[i].Pos;
      Exit;
    end;
  end;
end;

procedure TX86_64Emitter.AddExternalSymbol(const name, libName: string);
var
  i: Integer;
begin
  // Check if already exists
  for i := 0 to High(FExternalSymbols) do
  begin
    if FExternalSymbols[i].Name = name then
      Exit; // Already exists
  end;
  
  // Add new external symbol
  SetLength(FExternalSymbols, Length(FExternalSymbols) + 1);
  FExternalSymbols[High(FExternalSymbols)].Name := name;
  FExternalSymbols[High(FExternalSymbols)].LibraryName := libName;
end;

function TX86_64Emitter.IsExternalSymbol(const name: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  // Check if it's in our external symbols list
  for i := 0 to High(FExternalSymbols) do
  begin
    if FExternalSymbols[i].Name = name then
    begin
      Result := True;
      Exit;
    end;
  end;
  
  // Check if it's a function that doesn't have a local definition
  for i := 0 to High(FLabelPositions) do
  begin
    if FLabelPositions[i].Name = name then
      Exit; // Found local definition, so it's not external
  end;
  
  // Check if it's a builtin function (should not be treated as external)
  // Note: Filesystem functions (open, read, write, close) are NOT builtins
  // They are extern calls to libc
  if (name = 'print_str') or (name = 'print_int') or (name = 'exit') or (name = 'strlen') or
     (name = 'print_float') or (name = 'str_char_at') or (name = 'str_set_char') or
     (name = 'str_length') or (name = 'str_compare') or (name = 'str_copy_builtin') or
     (name = 'int_to_str') or (name = 'str_to_int') or
     // Math builtins (emitted inline or via runtime snippets)
     (name = 'abs') or (name = 'fabs') or (name = 'sin') or (name = 'cos') or
     (name = 'sqrt') or (name = 'sqr') or (name = 'exp') or (name = 'ln') or
     (name = 'arctan') or (name = 'round') or (name = 'trunc') or (name = 'int_part') or
     (name = 'frac') or (name = 'pi') or (name = 'random') or (name = 'randomize') or
     (name = 'odd') or (name = 'hi') or (name = 'lo') or (name = 'swap') then
  begin
    Result := False;  // These are builtins, not external symbols
    Exit;
  end;

  // v0.3.0: Filesystem functions are external (libc)
  if (name = 'open') or (name = 'read') or (name = 'write') or (name = 'close') or
     (name = 'unlink') or (name = 'rename') or (name = 'lseek') then
  begin
    Result := True;  // These are external libc functions
    Exit;
  end;
  
  // If we reach here, it's likely external (no local definition found)
  Result := True;
end;

function TX86_64Emitter.GetExternalSymbols: TExternalSymbolArray;
begin
  Result := Copy(FExternalSymbols, 0, Length(FExternalSymbols));
end;

function TX86_64Emitter.GetPLTGOTPatches: TPLTGOTPatchArray;
begin
  Result := Copy(FPLTGOTPatches, 0, Length(FPLTGOTPatches));
end;

procedure TX86_64Emitter.GeneratePLTStubs;
var
  i: Integer;
  pltBasePos: Integer;
  plt0Pos: Integer;
  pltGOTPatch: TPLTGOTPatch;
  pltStub: TLabelPos;
  rel32: Int64;
  curPos: Integer;
begin
  if Length(FExternalSymbols) = 0 then Exit;

  // Record base position for PLT
  pltBasePos := FCode.Size;

  // Emit PLT0 (resolver entry)
  plt0Pos := FCode.Size;
  // pushq QWORD PTR [rip + disp32] ; push link map or relocation index per platform
  FCode.WriteU8($FF); FCode.WriteU8($35);
  // record patch for PLT0 push target (will point into GOT)
  pltGOTPatch.Pos := FCode.Size;
  pltGOTPatch.SymbolName := '__plt0_push';
  pltGOTPatch.SymbolIndex := -1;
  SetLength(FPLTGOTPatches, Length(FPLTGOTPatches) + 1);
  FPLTGOTPatches[High(FPLTGOTPatches)] := pltGOTPatch;
  FCode.WriteU32LE(0);

  // jmpq *QWORD PTR [rip + disp32]
  FCode.WriteU8($FF); FCode.WriteU8($25);
  // record patch for PLT0 jmp target
  pltGOTPatch.Pos := FCode.Size;
  pltGOTPatch.SymbolName := '__plt0_jmp';
  pltGOTPatch.SymbolIndex := -1;
  SetLength(FPLTGOTPatches, Length(FPLTGOTPatches) + 1);
  FPLTGOTPatches[High(FPLTGOTPatches)] := pltGOTPatch;
  FCode.WriteU32LE(0);

  // padding/nop
  FCode.WriteU8($0F); FCode.WriteU8($1F); FCode.WriteU8($40); FCode.WriteU8($00); // nopl [rax]

  // Now emit one PLT entry per symbol
  for i := 0 to High(FExternalSymbols) do
  begin
    // mark PLT label
    curPos := FCode.Size;
    pltStub.Name := FExternalSymbols[i].Name + '@plt';
    pltStub.Pos := curPos;
    SetLength(FPLTStubs, Length(FPLTStubs) + 1);
    FPLTStubs[High(FPLTStubs)] := pltStub;

    // jmpq *QWORD PTR [rip + disp32]  ; jmp *GOT[3+i]
    FCode.WriteU8($FF); FCode.WriteU8($25);
    pltGOTPatch.Pos := FCode.Size;
    pltGOTPatch.SymbolName := FExternalSymbols[i].Name;
    pltGOTPatch.SymbolIndex := i;
    SetLength(FPLTGOTPatches, Length(FPLTGOTPatches) + 1);
    FPLTGOTPatches[High(FPLTGOTPatches)] := pltGOTPatch;
    FCode.WriteU32LE(0);

    // pushq imm32  ; relocation index (i+1)
    FCode.WriteU8($68);
    FCode.WriteU32LE(i + 1);

    // jmp rel32 -> PLT0 (relative to next instruction)
    rel32 := plt0Pos - (FCode.Size + 5);
    FCode.WriteU8($E9);
    FCode.WriteU32LE(Cardinal(rel32));
  end;
end;

end.
