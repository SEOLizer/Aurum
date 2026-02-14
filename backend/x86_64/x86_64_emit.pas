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
  RAX = 0; RCX = 1; RDX = 2; RBX = 3; RSP = 4; RBP = 5; RSI = 6; RDI = 7; R8 = 8; R9 = 9;
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
  // for call/abi
  argCount: Integer;
  argTemps: array of Integer;
  sParse: string;
  ppos, ai: Integer;
  // for call extra
  extraCount: Integer;
  // function context
  isEntryFunction: Boolean;
  // debug IO
  fs: TFileStream;
  meta: TStringList;
  frameBytes: Integer;
  framePad: Integer;
  callPad: Integer;
  pushBytes: Integer;
  restoreBytes: Integer;
  // patch logging locals
  lf: TextFile;
  startOff, endOff, off: Integer;
  sBytes: string;
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
              // try static length from interned string
              len := 0;
              if (instr.Src1 >= 0) and (instr.Src1 < Length(tempStrIndex)) then
              begin
                sidx := tempStrIndex[instr.Src1];
                if (sidx >= 0) and (sidx < module.Strings.Count) then
                  len := Length(module.Strings[sidx]);
              end;
              if len > 0 then
              begin
                // static length known: use immediate
                WriteMovRegImm64(FCode, RDX, Cardinal(len));
              end
              else
              begin
                // runtime strlen: scan for \0 starting at RSI
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
              end;
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
            // Call user-defined function (simple SysV-ish implementation)
            // Args info: instr.ImmInt = argCount, instr.Src1/Src2 first two temp indices,
            // remaining temps serialized in instr.LabelName as CSV starting from index 2.
            argCount := instr.ImmInt;
            SetLength(argTemps, argCount);
            for k := 0 to argCount - 1 do
              argTemps[k] := -1;
            if argCount > 0 then argTemps[0] := instr.Src1;
            if argCount > 1 then argTemps[1] := instr.Src2;
            if (argCount > 2) and (instr.LabelName <> '') then
            begin
              // parse CSV from LabelName (temps starting from index 2)
              sParse := instr.LabelName;
              ppos := Pos(',', sParse);
              ai := 2;
              while (ppos > 0) and (ai < argCount) do
              begin
                argTemps[ai] := StrToIntDef(Copy(sParse, 1, ppos - 1), -1);
                Delete(sParse, 1, ppos);
                Inc(ai);
                ppos := Pos(',', sParse);
              end;
              if (sParse <> '') and (ai < argCount) then
                argTemps[ai] := StrToIntDef(sParse, -1);
            end;


            // Move args into registers (SysV: RDI, RSI, RDX, RCX, R8, R9)
            if argCount > 0 then
            begin
              // direct registers for up to 6 args
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

            // emit call and patch later
            SetLength(FJumpPatches, Length(FJumpPatches) + 1);
            FJumpPatches[High(FJumpPatches)].Pos := FCode.Size;
            FJumpPatches[High(FJumpPatches)].LabelName := instr.ImmStr;
            FJumpPatches[High(FJumpPatches)].JmpSize := 5; // call rel32
            EmitU8(FCode, $E8); // call rel32
            EmitU32(FCode, 0);  // placeholder offset

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

      // detailed patch logging for debugging
      try
        AssignFile(lf, '/tmp/emitter_patch_log.txt');
        if FileExists('/tmp/emitter_patch_log.txt') then Append(lf) else Rewrite(lf);
        try
          WriteLn(lf, Format('PATCH idx=%d jmpPos=%d label=%s targetPos=%d jmpSize=%d rel32=%d',
            [i, jmpPos, FJumpPatches[i].LabelName, targetPos, FJumpPatches[i].JmpSize, rel32]));
          // dump nearby bytes
          startOff := jmpPos - 8;
          if startOff < 0 then startOff := 0;
          endOff := jmpPos + 16;
          if endOff > FCode.Size - 1 then endOff := FCode.Size - 1;
          sBytes := '';
          for off := startOff to endOff do
            sBytes := sBytes + Format('%02x ', [FCode.ReadU8(off)]);
          WriteLn(lf, Format('BYTES @%d..%d: %s', [startOff, endOff, sBytes]));
        finally
          CloseFile(lf);
        end;
      except
        // ignore logging errors
      end;

      if FJumpPatches[i].JmpSize = 5 then
        FCode.PatchU32LE(jmpPos + 1, Cardinal(rel32)) // jmp rel32: opcode at pos, rel32 at pos+1
      else
        FCode.PatchU32LE(jmpPos + 2, Cardinal(rel32)); // jcc rel32: opcode 0F xx at pos, rel32 at pos+2
    end;
  end;

  // Dump code/data and metadata for debugging
  try
    // write raw code and data buffers
    fs := TFileStream.Create('/tmp/emitter_code.bin', fmCreate);
    try
      if FCode.Size > 0 then fs.WriteBuffer(FCode.GetBuffer^, FCode.Size);
    finally
      fs.Free;
    end;
    fs := TFileStream.Create('/tmp/emitter_data.bin', fmCreate);
    try
      if FData.Size > 0 then fs.WriteBuffer(FData.GetBuffer^, FData.Size);
    finally
      fs.Free;
    end;

    // write metadata
    meta := TStringList.Create;
    try
      meta.Add('CodeSize=' + IntToStr(FCode.Size));
      meta.Add('DataSize=' + IntToStr(FData.Size));
      meta.Add('Labels:');
      for i := 0 to High(FLabelPositions) do
        meta.Add(Format('  %s => %d', [FLabelPositions[i].Name, FLabelPositions[i].Pos]));
      meta.Add('JumpPatches:');
      for i := 0 to High(FJumpPatches) do
        meta.Add(Format('  pos=%d label=%s jmpSize=%d', [FJumpPatches[i].Pos, FJumpPatches[i].LabelName, FJumpPatches[i].JmpSize]));
      meta.Add('LeaPositions:');
      for i := 0 to High(FLeaPositions) do
        meta.Add(Format('  pos=%d strIndex=%d', [FLeaPositions[i], FLeaStrIndex[i]]));
      meta.SaveToFile('/tmp/emitter_meta.txt');
    finally
      meta.Free;
    end;
  except
    // ignore errors in debug dump
  end;
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

end.
