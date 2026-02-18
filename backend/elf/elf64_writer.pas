{$mode objfpc}{$H+}
unit elf64_writer;

interface

uses
  SysUtils, Classes, bytes, backend_types;

procedure WriteElf64(const filename: string; const codeBuf, dataBuf: TByteBuffer; entryVA: UInt64);
procedure WriteDynamicElf64(const filename: string; const codeBuf, dataBuf: TByteBuffer; 
  entryVA: UInt64; const externSymbols: TExternalSymbolArray; 
  const neededLibs: array of string);

implementation

// Forward declarations for helper procedures
procedure BuildDynamicElfHeader(elfHeader: TByteBuffer; entryVA: UInt64; phnum: Integer); forward;
procedure BuildProgramHeaders(phdrs: TByteBuffer; codeOffset, codeSize, 
  dataOffset, dataSize, dynstrOffset: UInt64; const interpPath: string; 
  dynamicOffset: UInt64); forward;
procedure BuildDynstrSection(dynstrBuf: TByteBuffer; symNames, libNames: TStringList); forward;
procedure BuildDynsymSection(dynsymBuf: TByteBuffer; symNames: TStringList); forward;
procedure BuildDynamicSection(dynamicBuf: TByteBuffer; libNames: TStringList; 
  dynstrOffset, dynsymOffset: UInt64); forward;

// Helper function to write string to buffer
procedure WriteStringToBuffer(buf: TByteBuffer; const s: string);
var
  i: Integer;
begin
  for i := 1 to Length(s) do
    buf.WriteU8(Ord(s[i]));
end;

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

procedure WriteDynamicElf64(const filename: string; const codeBuf, dataBuf: TByteBuffer; 
  entryVA: UInt64; const externSymbols: TExternalSymbolArray; 
  const neededLibs: array of string);
const
  pageSize: UInt64 = 4096;
  baseVA: UInt64 = $400000;
  interpPath: string = '/lib64/ld-linux-x86-64.so.2';
var
  // Layout offsets  
  codeOffset, dataOffset, dynstrOffset, dynsymOffset, hashOffset, 
  pltOffset, gotOffset, relapltOffset, dynamicOffset: UInt64;
  codeSize, dataSize, interpSize: UInt64;
  
  // ELF structures
  fileBuf: TFileStream;
  elfHeader, phdrs: TByteBuffer;
  dynstrBuf, dynsymBuf, hashBuf, pltBuf, gotBuf, relapltBuf, dynamicBuf: TByteBuffer;
  
  // Symbol management
  i, symCount, libCount: Integer;
  symNames: TStringList;
  libNames: TStringList;
  
begin
  codeSize := codeBuf.Size;
  dataSize := dataBuf.Size;
  interpSize := Length(interpPath) + 1; // +1 for null terminator
  
  // Collect unique library names
  libNames := TStringList.Create;
  libNames.Duplicates := dupIgnore;
  libNames.Sorted := True;
  
  symNames := TStringList.Create;
  try
    // Add libraries and symbols
    for i := 0 to High(externSymbols) do
    begin
      libNames.Add(externSymbols[i].LibraryName);
      symNames.Add(externSymbols[i].Name);
    end;
    
    symCount := Length(externSymbols);
    libCount := libNames.Count;
    
    // Calculate section offsets (simplified layout for now)
    codeOffset := pageSize; // 0x1000
    dataOffset := codeOffset + AlignUp(codeSize, pageSize);
    
    // Dynamic sections (will be refined)
    dynstrOffset := dataOffset + AlignUp(dataSize, 8);
    dynsymOffset := dynstrOffset + AlignUp(256, 8); // rough estimate  
    dynamicOffset := dynsymOffset + AlignUp((symCount + 1) * 24, 8);
    
    // Create buffers
    elfHeader := TByteBuffer.Create;
    phdrs := TByteBuffer.Create;
    dynstrBuf := TByteBuffer.Create;
    dynsymBuf := TByteBuffer.Create; 
    dynamicBuf := TByteBuffer.Create;
    
    try
      // Build ELF header for dynamic executable
      BuildDynamicElfHeader(elfHeader, entryVA, 3); // 3 program headers for now
      
      // Build program headers
      BuildProgramHeaders(phdrs, codeOffset, codeSize, dataOffset, dataSize, 
                         dynstrOffset, interpPath, dynamicOffset);
      
      // Build dynamic sections (minimal implementation)
      BuildDynstrSection(dynstrBuf, symNames, libNames);
      BuildDynsymSection(dynsymBuf, symNames);
      BuildDynamicSection(dynamicBuf, libNames, dynstrOffset, dynsymOffset);
      
      // Write ELF file
      fileBuf := TFileStream.Create(filename, fmCreate);
      try
        // Write header and program headers
        fileBuf.WriteBuffer(elfHeader.GetBuffer^, elfHeader.Size);
        fileBuf.WriteBuffer(phdrs.GetBuffer^, phdrs.Size);
        
        // Write interpreter path
        while fileBuf.Position < interpSize + 64 + phdrs.Size do 
          fileBuf.WriteByte(0);
        fileBuf.WriteBuffer(PChar(interpPath)^, Length(interpPath));
        fileBuf.WriteByte(0);
        
        // Write code section
        while fileBuf.Position < Int64(codeOffset) do 
          fileBuf.WriteByte(0);
        if codeSize > 0 then 
          fileBuf.WriteBuffer(codeBuf.GetBuffer^, codeSize);
          
        // Write data section
        while fileBuf.Position < Int64(dataOffset) do 
          fileBuf.WriteByte(0);
        if dataSize > 0 then 
          fileBuf.WriteBuffer(dataBuf.GetBuffer^, dataSize);
          
        // Write dynamic sections
        while fileBuf.Position < Int64(dynstrOffset) do 
          fileBuf.WriteByte(0);
        fileBuf.WriteBuffer(dynstrBuf.GetBuffer^, dynstrBuf.Size);
        
        while fileBuf.Position < Int64(dynsymOffset) do 
          fileBuf.WriteByte(0);
        fileBuf.WriteBuffer(dynsymBuf.GetBuffer^, dynsymBuf.Size);
        
        while fileBuf.Position < Int64(dynamicOffset) do 
          fileBuf.WriteByte(0);
        fileBuf.WriteBuffer(dynamicBuf.GetBuffer^, dynamicBuf.Size);
        
      finally
        fileBuf.Free;
      end;
    finally
      elfHeader.Free;
      phdrs.Free;
      dynstrBuf.Free;
      dynsymBuf.Free;
      dynamicBuf.Free;
    end;
  finally
    libNames.Free;
    symNames.Free;
  end;
end;

// Helper procedures (to be implemented)
procedure BuildDynamicElfHeader(elfHeader: TByteBuffer; entryVA: UInt64; phnum: Integer);
begin
  // Implementation will follow
  elfHeader.WriteBytes([$7F, Ord('E'), Ord('L'), Ord('F')]);
  elfHeader.WriteU8(2); // EI_CLASS_64
  elfHeader.WriteU8(1); // EI_DATA_LE  
  elfHeader.WriteU8(1); // EV_CURRENT
  elfHeader.WriteU8(0); // OSABI
  elfHeader.WriteU8(0); // ABI version
  elfHeader.WriteBytesFill(7, 0);
  
  elfHeader.WriteU16LE(2); // ET_EXEC (could be ET_DYN for PIE)
  elfHeader.WriteU16LE(62); // EM_X86_64
  elfHeader.WriteU32LE(1);
  
  elfHeader.WriteU64LE(entryVA);
  elfHeader.WriteU64LE(64); // phoff
  elfHeader.WriteU64LE(0);  // shoff (no sections for now)
  
  elfHeader.WriteU32LE(0);  // flags
  elfHeader.WriteU16LE(64); // ehsize
  elfHeader.WriteU16LE(56); // phentsize  
  elfHeader.WriteU16LE(phnum); // phnum
  elfHeader.WriteU16LE(0);  // shentsize
  elfHeader.WriteU16LE(0);  // shnum
  elfHeader.WriteU16LE(0);  // shstrndx
end;

procedure BuildProgramHeaders(phdrs: TByteBuffer; codeOffset, codeSize, 
  dataOffset, dataSize, dynstrOffset: UInt64; const interpPath: string; 
  dynamicOffset: UInt64);
const
  baseVA: UInt64 = $400000;
  pageSize: UInt64 = 4096;
begin
  // PT_INTERP header  
  phdrs.WriteU32LE(3); // PT_INTERP
  phdrs.WriteU32LE(4); // PF_R
  phdrs.WriteU64LE(64 + 3*56); // offset after headers
  phdrs.WriteU64LE(baseVA + 64 + 3*56); // vaddr
  phdrs.WriteU64LE(baseVA + 64 + 3*56); // paddr
  phdrs.WriteU64LE(Length(interpPath) + 1); // filesz
  phdrs.WriteU64LE(Length(interpPath) + 1); // memsz  
  phdrs.WriteU64LE(1); // align
  
  // PT_LOAD for code+data (combined for simplicity)
  phdrs.WriteU32LE(1); // PT_LOAD
  phdrs.WriteU32LE(4 or 2 or 1); // PF_R|PF_W|PF_X
  phdrs.WriteU64LE(codeOffset);
  phdrs.WriteU64LE(baseVA + codeOffset);
  phdrs.WriteU64LE(baseVA + codeOffset);
  phdrs.WriteU64LE(AlignUp(codeSize + dataSize, pageSize));
  phdrs.WriteU64LE(AlignUp(codeSize + dataSize, pageSize));
  phdrs.WriteU64LE(pageSize);
  
  // PT_DYNAMIC header
  phdrs.WriteU32LE(2); // PT_DYNAMIC
  phdrs.WriteU32LE(4 or 2); // PF_R|PF_W
  phdrs.WriteU64LE(dynamicOffset);
  phdrs.WriteU64LE(baseVA + dynamicOffset);
  phdrs.WriteU64LE(baseVA + dynamicOffset);
  phdrs.WriteU64LE(128); // rough estimate
  phdrs.WriteU64LE(128);
  phdrs.WriteU64LE(8); // align
end;

procedure BuildDynstrSection(dynstrBuf: TByteBuffer; symNames, libNames: TStringList);
var
  i: Integer;
begin
  dynstrBuf.WriteU8(0); // First entry is empty string
  
  // Add library names
  for i := 0 to libNames.Count - 1 do
  begin
    WriteStringToBuffer(dynstrBuf, libNames[i]);
    dynstrBuf.WriteU8(0);
  end;
  
  // Add symbol names  
  for i := 0 to symNames.Count - 1 do
  begin
    WriteStringToBuffer(dynstrBuf, symNames[i]);
    dynstrBuf.WriteU8(0);
  end;
end;

procedure BuildDynsymSection(dynsymBuf: TByteBuffer; symNames: TStringList);
var
  i: Integer;
begin
  // First symbol is always undefined
  dynsymBuf.WriteU32LE(0); // st_name
  dynsymBuf.WriteU8(0);    // st_info  
  dynsymBuf.WriteU8(0);    // st_other
  dynsymBuf.WriteU16LE(0); // st_shndx
  dynsymBuf.WriteU64LE(0); // st_value
  dynsymBuf.WriteU64LE(0); // st_size
  
  // Add external symbols (simplified)
  for i := 0 to symNames.Count - 1 do
  begin
    dynsymBuf.WriteU32LE(1 + i * 10); // rough st_name offset
    dynsymBuf.WriteU8($12); // STB_GLOBAL | STT_FUNC
    dynsymBuf.WriteU8(0);   // st_other
    dynsymBuf.WriteU16LE(0); // st_shndx (undefined)
    dynsymBuf.WriteU64LE(0); // st_value  
    dynsymBuf.WriteU64LE(0); // st_size
  end;
end;

procedure BuildDynamicSection(dynamicBuf: TByteBuffer; libNames: TStringList; 
  dynstrOffset, dynsymOffset: UInt64);
const
  baseVA: UInt64 = $400000;
var
  i: Integer;
begin
  // DT_NEEDED entries for required libraries
  for i := 0 to libNames.Count - 1 do
  begin
    dynamicBuf.WriteU64LE(1); // DT_NEEDED
    dynamicBuf.WriteU64LE(1 + i * 10); // offset in dynstr (rough)
  end;
  
  // DT_SYMTAB
  dynamicBuf.WriteU64LE(6); // DT_SYMTAB
  dynamicBuf.WriteU64LE(baseVA + dynsymOffset);
  
  // DT_STRTAB  
  dynamicBuf.WriteU64LE(5); // DT_STRTAB
  dynamicBuf.WriteU64LE(baseVA + dynstrOffset);
  
  // DT_NULL terminator
  dynamicBuf.WriteU64LE(0); // DT_NULL
  dynamicBuf.WriteU64LE(0);
end;

end.
