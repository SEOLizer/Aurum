{$mode objfpc}{$H+}
unit x86_64_emit;

interface

uses
  SysUtils, Classes, bytes, ir;

type
  TX86_64Emitter = class
  private
    FCode: TByteBuffer;
    FData: TByteBuffer;
    FStringOffsets: array of UInt64;
    FLeaPositions: array of Integer;
    FLeaStrIndex: array of Integer;
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

function SlotOffset(slot: Integer): Cardinal; begin Result := Cardinal(8*(slot+1)); end;

constructor TX86_64Emitter.Create;
begin
  inherited Create;
  FCode := TByteBuffer.Create;
  FData := TByteBuffer.Create;
  SetLength(FStringOffsets, 0);
  SetLength(FLeaPositions, 0);
  SetLength(FLeaStrIndex, 0);
end;

destructor TX86_64Emitter.Destroy;
begin
  FCode.Free; FData.Free; inherited Destroy;
end;

function TX86_64Emitter.GetCodeBuffer: TByteBuffer; begin Result := FCode; end;
function TX86_64Emitter.GetDataBuffer: TByteBuffer; begin Result := FData; end;

procedure TX86_64Emitter.EmitFromIR(module: TIRModule);
var
  i,j,sidx: Integer;
  totalDataOffset: UInt64;
  instr: TIRInstr;
  localCnt,tempCount,slotIdx: Integer;
  leaPos: Integer;
  codeVA,instrVA,dataVA: UInt64;
begin
  // strings
  SetLength(FStringOffsets, module.Strings.Count);
  totalDataOffset := 0;
  for i := 0 to module.Strings.Count-1 do
  begin
    FLeaStrIndex := FLeaStrIndex; // no-op to avoid unused warning
    FStringOffsets[i] := totalDataOffset;
    for j:=1 to Length(module.Strings[i]) do FData.WriteU8(Byte(module.Strings[i][j]));
    FData.WriteU8(0);
    Inc(totalDataOffset, Length(module.Strings[i]) + 1);
  end;

  // simple emitter: support constStr + call print_str + return via exit
  for i:=0 to High(module.Functions) do
  begin
    localCnt := module.Functions[i].LocalCount;
    tempCount := 0; // not tracking temps here
    // prologue
    FCode.WriteU8($55); // push rbp
    FCode.WriteU8($48); FCode.WriteU8($89); FCode.WriteU8($E5); // mov rbp,rsp
    // no local alloc for minimal emitter

    for j:=0 to High(module.Functions[i].Instructions) do
    begin
      instr := module.Functions[i].Instructions[j];
      case instr.Op of
        irConstStr:
          begin
            slotIdx := localCnt + instr.Dest;
            // emit lea rax,[rip+disp]
            leaPos := FCode.Size;
            FCode.WriteU8($48); FCode.WriteU8($8D); FCode.WriteU8($05); FCode.WriteU32LE(0);
            SetLength(FLeaPositions, Length(FLeaPositions)+1);
            SetLength(FLeaStrIndex, Length(FLeaStrIndex)+1);
            FLeaPositions[High(FLeaPositions)] := leaPos;
            FLeaStrIndex[High(FLeaStrIndex)] := StrToIntDef(instr.ImmStr, 0);
            // store rax to slot (mov [rbp - offset], rax)
            WriteMovMemReg(FCode, RBP, SlotOffset(slotIdx), RAX);
          end;
        irCallBuiltin:
          begin
            if instr.ImmStr = 'print_str' then
            begin
              // assume src1 is temp holding pointer in slot
              // mov rax,1; mov rdi,1
              WriteMovRegImm64(FCode, RAX, 1);
              WriteMovRegImm64(FCode, RDI, 1);
              // lea rsi placeholder recorded earlier
              // find next lea placeholder if any
              // for simplicity assume the last lea corresponds
              if Length(FLeaPositions)>0 then
              begin
                leaPos := FLeaPositions[High(FLeaPositions)];
                // we'll patch later
                FCode.WriteU8($48); FCode.WriteU8($8D); FCode.WriteU8($35); FCode.WriteU32LE(0);
              end else
              begin
                // no lea: set rsi=0
                WriteMovRegImm64(FCode, RSI, 0);
              end;
              // mov rdx, len
              var len := 0;
              if (instr.Src1>=0) and (instr.Src1 < Length(FLeaStrIndex)) then len := Length(module.Strings[FLeaStrIndex[instr.Src1]])
              else if (instr.ImmStr<>'') then len := Length(module.Strings[StrToIntDef(instr.ImmStr,0)]);
              WriteMovRegImm64(FCode, RDX, Cardinal(len));
              WriteSyscall(FCode);
            end
            else if instr.ImmStr = 'exit' then
            begin
              WriteMovRegImm64(FCode, RAX, 60);
              WriteMovRegImm64(FCode, RDI, 0);
              WriteSyscall(FCode);
            end;
          end;
        irReturn:
          begin
            WriteMovRegImm64(FCode, RAX, 60);
            WriteMovRegImm64(FCode, RDI, 0);
            WriteSyscall(FCode);
          end;
      end;
    end;

  end;

  // patch LEAs
  for i:=0 to High(FLeaPositions) do
  begin
    leaPos := FLeaPositions[i];
    var si := FLeaStrIndex[i];
    if (si>=0) and (si<Length(FStringOffsets)) then
    begin
      codeVA := $400000 + 4096;
      instrVA := codeVA + leaPos + 7;
      dataVA := $400000 + 4096 + AlignUp(FCode.Size, 4096) + FStringOffsets[si];
      var disp32 := Int64(dataVA) - Int64(instrVA);
      FCode.PatchU32LE(leaPos+3, Cardinal(disp32));
    end;
  end;
end;

end.
