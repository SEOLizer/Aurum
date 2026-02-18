{$mode objfpc}{$H+}
unit backend_types;

interface

type
  TExternalSymbol = record
    Name: string;
    LibraryName: string;
  end;
  
  TExternalSymbolArray = array of TExternalSymbol;
  
  TPLTGOTPatch = record
    Pos: Integer;        // Position in code where GOT offset needs patching
    SymbolName: string;  // Symbol name for GOT lookup
    SymbolIndex: Integer; // Index in external symbols array
  end;
  
  TPLTGOTPatchArray = array of TPLTGOTPatch;

implementation

end.