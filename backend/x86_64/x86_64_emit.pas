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
  end;

implementation

uses
  Math;

const
  RAX = 0; RCX = 1; RDX = 2; RBX = 3; RSP = 4; RBP = 5; RSI = 6; RDI = 7;

procedure EmitU8(b: TByteBuffer; v: Byte); begin b.WriteU8(v); end;
procedure EmitU32(b: TByteBuffer; v: Cardinal); begin b.WriteU32LE(v); end;
procedure EmitU64(b: TByteBuffer; v: UInt64); begin b.WriteU64LE(v); end;

procedure WriteMovRegImm64(buf: TByteBuffer; reg: Byte; imm: UInt64);
begin
  EmitU8(buf, $48); EmitU8(buf, $B8 + reg); EmitU64(buf, imm);
end;
procedure WriteMovRegReg(buf: TByteBuffer; dst, src: Byte);
begin EmitU8(buf,$48); EmitU8(buf,$89); EmitU8(buf,$C0 or ((src shl 3) and $38) or (dst and $7)); end;
procedure WriteMovRegMem(buf: TByteBuffer; reg, base: Byte; disp: Cardinal);
begin EmitU8(buf,$48); EmitU8(buf,$8B); EmitU8(buf,$80 or ((reg shl 3) and $38) or (base and $7)); EmitU32(buf, disp); end;
procedure WriteMovMemReg(buf: TByteBuffer; base: Byte; disp: Cardinal; reg: Byte);
begin EmitU8(buf,$48); EmitU8(buf,$89); EmitU8(buf,$80 or ((reg shl 3) and $38) or (base and $7)); EmitU32(buf, disp); end;
procedure WriteAddRegReg(buf: TByteBuffer; dst, src: Byte);
begin EmitU8(buf,$48); EmitU8(buf,$01); EmitU8(buf,$C0 or ((src shl 3) and $38) or (dst and $7)); end;
procedure WriteSubRegReg(buf: TByteBuffer; dst, src: Byte);
begin EmitU8(buf,$48); EmitU8(buf,$29); EmitU8(buf,$C0 or ((src shl 3) and $38) or (dst and $7)); end;
procedure WriteImulRegReg(buf: TByteBuffer; dst, src: Byte);
begin EmitU8(buf,$48); EmitU8(buf,$0F); EmitU8(buf,$AF); EmitU8(buf,$C0 or ((dst shl 3) and $38) or (src and $7)); end;
procedure WriteCqo(buf: TByteBuffer); begin EmitU8(buf,$48); EmitU8(buf,$99); end;
procedure WriteIdivReg(buf: TByteBuffer; src: Byte); begin EmitU8(buf,$48); EmitU8(buf,$F7); EmitU8(buf,$F8 or (src and $7)); end;
procedure WriteTestRaxRax(buf: TByteBuffer); begin EmitU8(buf,$48); EmitU8(buf,$85); EmitU8(buf,$C0); end;
procedure WriteSyscall(buf: TByteBuffer); begin EmitU8(buf,$0F); EmitU8(buf,$05); end;
procedure WriteLeaRsiRipDisp(buf: TByteBuffer; disp32: Cardinal); begin EmitU8(buf,$48); EmitU8(buf,$8D); EmitU8(buf,$35); EmitU32(buf, disp32); end;

procedure WriteJeRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$84); EmitU32(buf, rel32); end;
procedure WriteJneRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$85); EmitU32(buf, rel32); end;
procedure WriteJgeRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$0F); EmitU8(buf,$8D); EmitU32(buf, rel32); end;
procedure WriteJmpRel32(buf: TByteBuffer; rel32: Cardinal);
begin EmitU8(buf,$E9); EmitU32(buf, rel32); end;

procedure WriteDecReg(buf: TByteBuffer; reg: Byte);
begin
  // dec r64: 48 FF C8+reg
  EmitU8(buf, $48); EmitU8(buf, $FF); EmitU8(buf, $C8 or (reg and $7));
end;

procedure WriteIncReg(buf: TByteBuffer; reg: Byte);
begin
  // inc r64: 48 FF C0+reg
  EmitU8(buf, $48); EmitU8(buf, $FF); EmitU8(buf, $C0 or (reg and $7));
end;

procedure WriteMovMemRegByte(buf: TByteBuffer; base: Byte; disp: Cardinal; reg8: Byte);
begin
  // mov byte ptr [base + disp32], r8 -> 88 /0 with mod=10 (disp32)
  EmitU8(buf, $88);
  EmitU8(buf, $80 or ((reg8 and $7) shl 3) or (base and $7));
  EmitU32(buf, disp);
end;

procedure WriteMovMemRegByteNoDisp(buf: TByteBuffer; base: Byte; reg8: Byte);
begin
  // mov byte ptr [base], r8 -> 88 /0 with mod=00 and rm=base
  EmitU8(buf, $88);
  EmitU8(buf, ((reg8 and $7) shl 3) or (base and $7));
end;

procedure WriteMovMemImm8(buf: TByteBuffer; base: Byte; disp: Cardinal; value: Byte);
begin
  // mov byte ptr [base+disp32], imm8 => C6 80 disp32 imm8
  EmitU8(buf, $C6);
  EmitU8(buf, $80 or (base and $7));
  EmitU32(buf, disp);
  EmitU8(buf, value);
end;

procedure WriteSetccMem8(buf: TByteBuffer; ccOpcode: Byte; baseReg: Byte; disp32: Cardinal);
begin
  // setcc r/m8 : opcode 0F ccOpcode modrm(mod=10) rm=base
  EmitU8(buf, $0F);
  EmitU8(buf, ccOpcode);
  EmitU8(buf, $80 or ((0 shl 3) and $38) or (baseReg and $7));
  EmitU32(buf, disp32);
end;

procedure WriteMovzxRegMem8(buf: TByteBuffer; dstReg: Byte; baseReg: Byte; disp32: Cardinal);
begin
  // movzx r64, r/m8 : rex.w 0F B6 /r with reg=dst, rm=mem
  EmitU8(buf, $48);
  EmitU8(buf, $0F);
  EmitU8(buf, $B6);
  EmitU8(buf, $80 or ((dstReg shl 3) and $38) or (baseReg and $7));
  EmitU32(buf, disp32);
end;

function SlotOffset(slot: Integer): Cardinal; begin Result := Cardinal(8*(slot+1)); end;

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
  leaPos: Integer;
  codeVA, instrVA, dataVA: UInt64;
  disp32, rel32: Int64;
  tempStrIndex: array of Integer;
  bufferAdded: Boolean;
  bufferOffset: UInt64;
  bufferLeaPositions: array of Integer;
  len: Integer;
  nonZeroPos, jmpDonePos, jgePos, loopStartPos, jneLoopPos, jeSignPos: Integer;
  targetPos, jmpPos: Integer;
begin
  // reset patch arrays
  SetLength(FLeaPositions, 0);
  SetLength(FLeaStrIndex, 0);
  SetLength(FLabelPositions, 0);
  SetLength(FJumpPatches, 0);

  // write interned strings
  SetLength(FStringOffsets, module.Strings.Count);
  totalDataOffset := 0;
  for i := 0 to module.Strings.Count - 1 do
  begin
    FStringOffsets[i] := totalDataOffset;
    for j := 1 to Length(module.Strings[i]) do
      FData.WriteU8(Byte(module.Strings[i][j]));
    FData.WriteU8(0);
    Inc(totalDataOffset, Length(module.Strings[i]) + 1);
  end;

  bufferAdded := False;
  bufferOffset := 0;
  SetLength(bufferLeaPositions, 0);

    for i := 0 to High(module.Functions) do
    begin
      // record function start label for calls
      SetLength(FLabelPositions, Length(FLabelPositions) + 1);
      FLabelPositions[High(FLabelPositions)].Name := module.Functions[i].Name;
      FLabelPositions[High(FLabelPositions)].Pos := FCode.Size;

      localCnt := module.Functions[i].LocalCount;
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

      // prologue
      EmitU8(FCode, $55); // push rbp
      EmitU8(FCode, $48); EmitU8(FCode, $89); EmitU8(FCode, $E5); // mov rbp,rsp
      if totalSlots > 0 then
      begin
        EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $EC);
        EmitU32(FCode, Cardinal(totalSlots * 8));
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
              // syscall write(1, rsi, len)
              WriteMovRegImm64(FCode, RAX, 1);
              WriteMovRegImm64(FCode, RDI, 1);
              len := 0;
              if (instr.Src1 >= 0) and (instr.Src1 < Length(tempStrIndex)) then
              begin
                sidx := tempStrIndex[instr.Src1];
                if (sidx >= 0) and (sidx < module.Strings.Count) then
                  len := Length(module.Strings[sidx]);
              end;
              WriteMovRegImm64(FCode, RDX, Cardinal(len));
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
            else if instr.ImmStr = 'exit' then
            begin
              WriteMovRegImm64(FCode, RAX, 60);
              WriteMovRegImm64(FCode, RDI, 0);
              WriteSyscall(FCode);
            end;
          end;
        irConstInt:
          begin
            // Load immediate integer into temp slot
            slotIdx := localCnt + instr.Dest;
            WriteMovRegImm64(FCode, RAX, UInt64(instr.ImmInt));
            WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), RAX);
          end;
        irLoadLocal:
          begin
            // Load local variable into temp: dest = locals[src1]
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(instr.Src1));
            slotIdx := localCnt + instr.Dest;
            WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), RAX);
          end;
        irStoreLocal:
          begin
            // Store temp into local variable: locals[dest] = src1
            WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));
            WriteMovMemReg(FCode, RBP, SlotOffset(instr.Dest), RAX);
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
        irReturn:
          begin
            // Move return value into RAX if provided
            if instr.Src1 >= 0 then
              WriteMovRegMem(FCode, RAX, RBP, SlotOffset(localCnt + instr.Src1));

            // Fix stack: add totalSlots*8 to RSP if we allocated
            if totalSlots > 0 then
            begin
              // add rsp, imm32  => 48 81 C4 imm32
              EmitU8(FCode, $48); EmitU8(FCode, $81); EmitU8(FCode, $C4);
              EmitU32(FCode, Cardinal(totalSlots * 8));
            end;

            // leave; ret
            EmitU8(FCode, $C9); // leave
            EmitU8(FCode, $C3); // ret
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
            // Call user-defined function (simple implementation)
            // Label name is in instr.ImmStr (function name)
            SetLength(FJumpPatches, Length(FJumpPatches) + 1);
            FJumpPatches[High(FJumpPatches)].Pos := FCode.Size;
            FJumpPatches[High(FJumpPatches)].LabelName := instr.ImmStr;
            FJumpPatches[High(FJumpPatches)].JmpSize := 5; // call rel32
            EmitU8(FCode, $E8); // call rel32
            EmitU32(FCode, 0);  // placeholder offset
            // Store result from RAX if there's a destination
            if instr.Dest >= 0 then
              WriteMovMemReg(FCode, RBP, SlotOffset(localCnt + instr.Dest), RAX);
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

  // patch jumps to labels
  for i := 0 to High(FJumpPatches) do
  begin
    // find target label position
    targetPos := -1;
    for j := 0 to High(FLabelPositions) do
    begin
      if FLabelPositions[j].Name = FJumpPatches[i].LabelName then
      begin
        targetPos := FLabelPositions[j].Pos;
        Break;
      end;
    end;
    if targetPos >= 0 then
    begin
      jmpPos := FJumpPatches[i].Pos;
      rel32 := Int64(targetPos) - Int64(jmpPos + FJumpPatches[i].JmpSize);
      if FJumpPatches[i].JmpSize = 5 then
        FCode.PatchU32LE(jmpPos + 1, Cardinal(rel32)) // jmp rel32: opcode at pos, rel32 at pos+1
      else
        FCode.PatchU32LE(jmpPos + 2, Cardinal(rel32)); // jcc rel32: opcode 0F xx at pos, rel32 at pos+2
    end;
  end;
end;

end.
