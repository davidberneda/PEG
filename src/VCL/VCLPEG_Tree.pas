unit VCLPEG_Tree;

interface

{
  Functions to fill VCL TreeView controls with data from PEG objects
}

uses
  TeePEG_Rules, ComCtrls;

type
  TPEGTree=record
  public
    class procedure AddSyntax(const AParser:TParser; const ATree:TTreeView); static;
  end;

implementation

uses
  SysUtils;

class procedure TPEGTree.AddSyntax(const AParser:TParser; const ATree:TTreeView);

  function ItemToString(const AItem:PSyntaxItem):String;
  begin
    if AItem.Rule=nil then
       result:='?'
    else
    if AItem.Rule is TNamedRule then
       result:=TNamedRule(AItem.Rule).Name
    else
       result:=AItem.Rule.ClassName;

    if AItem.Length>0 then
    begin
      result:=result+' -> '+
                     '('+IntToStr(AItem.Position)+','+IntToStr(AItem.Length)+')'+
                     ' '+
                     AParser.Extract(AItem);
    end;
  end;

  procedure ShowStack(const AParent:TTreeNode; const AItem:PSyntaxItem);
  var t : Integer;
      tmp : TTreeNode;
  begin
    tmp:=ATree.Items.AddChildObject(AParent,ItemToString(AItem),AItem);

    for t:=Low(AItem.Items) to High(AItem.Items) do
        ShowStack(tmp,AItem.Items[t]);
  end;

begin
  ATree.Items.BeginUpdate;
  try
    ATree.Items.Clear;

    ShowStack(nil,AParser.Syntax);
  finally
    ATree.Items.EndUpdate;
  end;
end;

end.
