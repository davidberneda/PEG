unit Unit_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, 

  TeePeg, TeePeg_Grammar, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    PageControl1: TPageControl;
    TabPEG: TTabSheet;
    TabMath: TTabSheet;
    MathPEG: TMemo;
    PEGPEG: TMemo;
    CBTrace: TCheckBox;
    CBTokens: TCheckBox;
    PageControl2: TPageControl;
    TabRules: TTabSheet;
    MemoRules: TMemo;
    TabTrace: TTabSheet;
    MemoTrace: TMemo;
    TabSyntax: TTabSheet;
    TabEval: TTabSheet;
    Panel1: TPanel;
    MemoEval: TMemo;
    MemoOutput: TMemo;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBTraceClick(Sender: TObject);
    procedure MemoEvalChange(Sender: TObject);
  private
    { Private declarations }

    PEG : TPEG;
    Grammar : TGrammar;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  TeePEG_Rules;

procedure AddSyntax(const PEG:TPeg; const ATree:TTreeView);

  function NodeFrom(const AParent:TTreeNode; const AItem:PSyntaxItem):TTreeNode;
  var tmp : String;
  begin
    if AItem.Rule=nil then
       tmp:='?'
    else
    if AItem.Rule is TNamedRule then
       tmp:=TNamedRule(AItem.Rule).Name
    else
       tmp:=AItem.Rule.ClassName;

    tmp:=tmp+' -> '+IntToStr(AItem.Position);
    
    result:=ATree.Items.AddChildObject(AParent,tmp,AItem);
  end;
  
  procedure ShowStack(const AParent:TTreeNode; const AItem:PSyntaxItem);
  var t : Integer;
      tmp : TTreeNode;
  begin
    tmp:=NodeFrom(AParent,AItem);

    for t:=Low(AItem.Items) to High(AItem.Items) do
        ShowStack(tmp,AItem.Items[t]);
  end;

begin
  ATree.Items.BeginUpdate;
  try
    ATree.Items.Clear;

    ShowStack(nil,Peg.Grammar.Parser.Syntax);
  finally
    ATree.Items.EndUpdate;
  end;
end;

procedure TForm1.CBTraceClick(Sender: TObject);
begin
  if CBTrace.Checked then
     PEG_Log:=MemoTrace.Lines
  else
     PEG_Log:=nil;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PEG:=TPEG.Create;

  MemoRules.Lines.Text:=PEG.Grammar.AsString;
end;

procedure TForm1.Button1Click(Sender: TObject);

  function FirstLines(const Quantity:Integer):String;
  var t : Integer;
  begin
    result:='';

    for t:=0 to Quantity-1 do
    begin
      if t>0 then
         result:=result+#13#10;

      result:=result+PEGPEG.Lines[t];
    end;
  end;

var tmp : String;
begin
  if PEG_Log<>nil then
     PEG_Log.BeginUpdate;

  try
    if PEG_Log<>nil then
       PEG_Log.Clear;

    //tmp:=Memo1.Lines[21]; //FirstLines(22);

    // tmp:='11';

    if PageControl1.ActivePage=TabPEG then
       tmp:=PEGPEG.Text
    else
       tmp:=MathPEG.Text;

    //tmp:='\t';

    try
      Grammar:=PEG.Load(tmp{Memo1.Lines});

    finally
      if CBTokens.Checked then
         AddSyntax(PEG, TreeView1);
 
      MemoRules.Clear;
      MemoRules.Lines.Add('Parsed: '+tmp);
    end;

  finally
    if PEG_Log<>nil then
       PEG_Log.EndUpdate;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
//  Grammar.Free;
  PEG.Free;
end;

procedure TForm1.MemoEvalChange(Sender: TObject);
var tmp : TGrammar;
begin
  if Grammar<>nil then
  begin
    tmp:=Grammar.Load(MemoEval.Text);
    try
      MemoOutput.Text:=tmp.AsString;
    finally
      //tmp.Free;
    end;
  end;
end;

end.
