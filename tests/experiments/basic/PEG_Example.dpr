program PEG_Example;

uses
//  FastMM4,
  Forms,
  Unit_Main in 'Unit_Main.pas' {Form1},
  TeePEG in '..\TeePEG.pas',
  TeePEG_Rules in '..\TeePEG_Rules.pas',
  TeePEG_Grammar in '..\TeePEG_Grammar.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
