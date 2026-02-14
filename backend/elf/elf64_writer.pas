{$mode objfpc}{$H+}
unit elf64_writer;

interface

uses
  SysUtils, Classes, bytes;

procedure WriteElf64(const filename: string; const codeBuf, dataBuf: TByteBuffer; entryVA: UInt64);

implementation

function AlignUp(v, a: UInt64): UInt64;
begin
  if a = 0 then Result := v else Result := (v + a - 1) and not (a - 1);
end;

procedure WriteElf64(const filename: string; const codeBuf, dataBuf: TByteBuffer; entryVA: UInt64);
var
  pageSize: UInt64 = 4096;
  codeOffset: UInt64;
  codeSize: UInt64;
  dataOffset: UInt64;
  dataSize: UInt64;
  fileBuf: TFileStream;
  elfHeader: TByteBuffer;
  phdr: TByteBuffer;
  filesz, memsz: UInt64;
  baseVA: UInt64 = $400000;
begin
  codeSize := codeBuf.Size;
  dataSize := dataBuf.Size;

  codeOffset := pageSize; // 0x1000
  dataOffset := codeOffset + AlignUp(codeSize, pageSize);

  // build ELF header
  elfHeader := TByteBuffer.Create;
  phdr := TByteBuffer.Create;
  try
    // e_ident
    elfHeader.WriteBytes([$7F, Ord('E'), Ord('L'), Ord('F')]);
    elfHeader.WriteU8(2); // EI_CLASS_64
    elfHeader.WriteU8(1); // EI_DATA_LE
    elfHeader.WriteU8(1); // EV_CURRENT
    elfHeader.WriteU8(0); // OSABI
    elfHeader.WriteU8(0); // ABI version
    elfHeader.WriteBytesFill(7, 0);

    // type, machine, version
    elfHeader.WriteU16LE(2); // ET_EXEC
    elfHeader.WriteU16LE(62); // EM_X86_64
    elfHeader.WriteU32LE(1);

    // entry, phoff, shoff
    elfHeader.WriteU64LE(entryVA);
    elfHeader.WriteU64LE(64);
    elfHeader.WriteU64LE(0);

    // flags, ehsize, phentsize, phnum, shentsize, shnum, shstrndx
    elfHeader.WriteU32LE(0);
    elfHeader.WriteU16LE(64);
    elfHeader.WriteU16LE(56);
    elfHeader.WriteU16LE(1);
    elfHeader.WriteU16LE(0);
    elfHeader.WriteU16LE(0);
    elfHeader.WriteU16LE(0);

    // program header
    phdr.WriteU32LE(1); // PT_LOAD
    phdr.WriteU32LE(4 or 2 or 1); // PF_R|PF_W|PF_X
    phdr.WriteU64LE(codeOffset);
    phdr.WriteU64LE(baseVA + codeOffset);
    phdr.WriteU64LE(baseVA + codeOffset);

    filesz := AlignUp(codeSize, pageSize) + dataSize;
    memsz := AlignUp(codeSize + dataSize, pageSize);
    if memsz < filesz then memsz := filesz;

    phdr.WriteU64LE(filesz);
    phdr.WriteU64LE(memsz);
    phdr.WriteU64LE(pageSize);

    // write file
    fileBuf := TFileStream.Create(filename, fmCreate);
    try
      if elfHeader.Size <> 64 then raise Exception.Create('Invalid ELF header size');
      fileBuf.WriteBuffer(elfHeader.GetBuffer^, elfHeader.Size);
      if phdr.Size <> 56 then raise Exception.Create('Invalid PHDR size');
      fileBuf.WriteBuffer(phdr.GetBuffer^, phdr.Size);
      while fileBuf.Position < Int64(codeOffset) do fileBuf.WriteByte(0);
      if codeSize > 0 then fileBuf.WriteBuffer(codeBuf.GetBuffer^, codeSize);
      while fileBuf.Position < Int64(dataOffset) do fileBuf.WriteByte(0);
      if dataSize > 0 then fileBuf.WriteBuffer(dataBuf.GetBuffer^, dataSize);
    finally
      fileBuf.Free;
    end;
  finally
    phdr.Free;
    elfHeader.Free;
  end;
end;

end.
