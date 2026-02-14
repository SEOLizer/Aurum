{$mode objfpc}{$H+}
program test_emit_run;

uses SysUtils, Classes, bytes, ir, x86_64_emit;

var
  m: TIRModule;
  fadd, fmain: TIRFunction;
  instr: TIRInstr;
  emit: TX86_64Emitter;
begin
  m := TIRModule.Create;
  try
    // function add(): int64 { return 2 + 3 }
    fadd := m.AddFunction('add');
    fadd.LocalCount := 0;
    // const 2 -> temp0
    instr.Op := irConstInt; instr.Dest := 0; instr.ImmInt := 2; fadd.Emit(instr);
    // const 3 -> temp1
    instr.Op := irConstInt; instr.Dest := 1; instr.ImmInt := 3; fadd.Emit(instr);
    // add temp2 = temp0 + temp1
    instr.Op := irAdd; instr.Dest := 2; instr.Src1 := 0; instr.Src2 := 1; fadd.Emit(instr);
    // return temp2
    instr.Op := irReturn; instr.Src1 := 2; fadd.Emit(instr);

    // function main(): int64 { var tmp := add(2,3); print_int(tmp); return 0 }
    fmain := m.AddFunction('main');
    fmain.LocalCount := 1; // one local slot for tmp
    // const 2 -> temp0
    instr.Op := irConstInt; instr.Dest := 0; instr.ImmInt := 2; fmain.Emit(instr);
    // const 3 -> temp1
    instr.Op := irConstInt; instr.Dest := 1; instr.ImmInt := 3; fmain.Emit(instr);
    // call add with temps 0,1 -> dest temp2
    instr.Op := irCall; instr.ImmStr := 'add'; instr.ImmInt := 2; instr.Src1 := 0; instr.Src2 := 1; instr.Dest := 2; instr.LabelName := ''; fmain.Emit(instr);
    // store result temp2 into local 0
    instr.Op := irStoreLocal; instr.Dest := 0; instr.Src1 := 2; fmain.Emit(instr);
    // load local0 into temp3
    instr.Op := irLoadLocal; instr.Dest := 3; instr.Src1 := 0; fmain.Emit(instr);
    // call builtin print_int with src1=temp3
    instr.Op := irCallBuiltin; instr.ImmStr := 'print_int'; instr.Src1 := 3; fmain.Emit(instr);
    // return 0 (call exit or return)
    instr.Op := irConstInt; instr.Dest := 4; instr.ImmInt := 0; fmain.Emit(instr);
    instr.Op := irReturn; instr.Src1 := 4; fmain.Emit(instr);

    // Now emit
    emit := TX86_64Emitter.Create;
    try
      writeln('About to EmitFromIR');
      emit.EmitFromIR(m);
      writeln('EmitFromIR finished');
      // dump buffers
      emit.GetCodeBuffer.SaveToFile('/tmp/test_emit_code.bin');
      emit.GetDataBuffer.SaveToFile('/tmp/test_emit_data.bin');
      writeln('Wrote /tmp/test_emit_code.bin and /tmp/test_emit_data.bin');
    finally
      emit.Free;
    end;
  finally
    m.Free;
  end;
end.
