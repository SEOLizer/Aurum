{$mode objfpc}{$H+}
program test_emit_main_args;

uses SysUtils, Classes, bytes, ir, x86_64_emit;

var
  m: TIRModule;
  fmain: TIRFunction;
  instr: TIRInstr;
  emit: TX86_64Emitter;
begin
  m := TIRModule.Create;
  try
    // function main(argc: int64, argv: pchar): int64
    fmain := m.AddFunction('main');
    fmain.ParamCount := 2; // argc, argv
    fmain.LocalCount := fmain.ParamCount + 1; // one extra local

    // For test: simply return argc (which should be in first param slot)
    // Load local param 0 into temp0
    instr.Op := irLoadLocal; instr.Dest := 0; instr.Src1 := 0; fmain.Emit(instr);
    // return temp0
    instr.Op := irReturn; instr.Src1 := 0; fmain.Emit(instr);

    emit := TX86_64Emitter.Create;
    try
      writeln('About to EmitFromIR');
      emit.EmitFromIR(m);
      writeln('EmitFromIR finished');
      writeln('CodeSize=', emit.GetCodeBuffer.Size, ' DataSize=', emit.GetDataBuffer.Size);
      // dump code/data for inspection
      emit.GetCodeBuffer.SaveToFile('/tmp/test_emit_main_args_code.bin');
      emit.GetDataBuffer.SaveToFile('/tmp/test_emit_main_args_data.bin');
    finally
      emit.Free;
    end;
  finally
    m.Free;
  end;
end.
