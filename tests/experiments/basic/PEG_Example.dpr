program PEG_Example;

uses
  Forms,
  Unit_Main in 'Unit_Main.pas' {Form1},
  VCLPEG_Tree in '..\..\..\src\VCL\VCLPEG_Tree.pas',
  TeePEG_Grammar_Default in '..\..\..\src\TeePEG_Grammar_Default.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
