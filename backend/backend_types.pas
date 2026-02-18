{$mode objfpc}{$H+}
unit backend_types;

interface

type
  TExternalSymbol = record
    Name: string;
    LibraryName: string;
  end;
  
  TExternalSymbolArray = array of TExternalSymbol;

implementation

end.