{$mode objfpc}{$H+}
unit ir;

interface

uses
  SysUtils, Classes;

type
  TIROpKind = (
    irInvalid,
    irConstInt,
    irConstStr,
    irAdd, irSub, irMul, irDiv, irMod, irNeg,
    irCmpEq, irCmpNeq, irCmpLt, irCmpLe, irCmpGt, irCmpGe,
    irNot, irAnd, irOr,
    irLoadLocal, irStoreLocal,
    // width/sign helpers
    irSExt,    // sign-extend Src1 to ImmInt bits -> Dest
    irZExt,    // zero-extend Src1 to ImmInt bits -> Dest
    irTrunc,   // truncate Src1 to ImmInt bits -> Dest
    irCallBuiltin, irCall,
    irJmp, irBrTrue, irBrFalse,
    irLabel,
    irReturn
  );

  TIRInstr = record
    Op: TIROpKind;
    Dest: Integer; // destination temp / local index
    Src1: Integer;
    Src2: Integer;
    ImmInt: Int64; // usage depends on Op: e.g., const int or width bits for ext/trunc
    ImmStr: string;
    LabelName: string;
  end;

  TIRInstructionList = array of TIRInstr;

  TIRFunction = class
  public
    Name: string;
    Instructions: TIRInstructionList;
    LocalCount: Integer; // number of local slots
    ParamCount: Integer;
    constructor Create(const AName: string);
    procedure Emit(const instr: TIRInstr);
  end;

  TIRModule = class
  public
    Functions: array of TIRFunction;
    Strings: TStringList; // deduplicated strings
    constructor Create;
    destructor Destroy; override;
    function AddFunction(const name: string): TIRFunction;
    function FindFunction(const name: string): TIRFunction;
    function InternString(const s: string): Integer;
  end;

implementation

{ TIRFunction }

constructor TIRFunction.Create(const AName: string);
begin
  inherited Create;
  Name := AName;
  Instructions := nil;
  LocalCount := 0;
  ParamCount := 0;
end;

procedure TIRFunction.Emit(const instr: TIRInstr);
begin
  SetLength(Instructions, Length(Instructions) + 1);
  Instructions[High(Instructions)] := instr;
end;

{ TIRModule }

constructor TIRModule.Create;
begin
  inherited Create;
  Functions := nil;
  Strings := TStringList.Create;
  Strings.Sorted := False;
  Strings.Duplicates := dupIgnore;
end;

destructor TIRModule.Destroy;
begin
  Strings.Free;
  inherited Destroy;
end;

function TIRModule.AddFunction(const name: string): TIRFunction;
begin
  SetLength(Functions, Length(Functions) + 1);
  Functions[High(Functions)] := TIRFunction.Create(name);
  Result := Functions[High(Functions)];
end;

function TIRModule.FindFunction(const name: string): TIRFunction;
var
  i: Integer;
begin
  for i := 0 to High(Functions) do
    if Functions[i].Name = name then
      Exit(Functions[i]);
  Result := nil;
end;

function TIRModule.InternString(const s: string): Integer;
begin
  Result := Strings.IndexOf(s);
  if Result >= 0 then Exit;
  Strings.Add(s);
  Result := Strings.Count - 1;
end;

end.
