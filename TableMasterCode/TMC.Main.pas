unit TMC.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Tabs, SynEditHighlighter,
  SynHighlighterPas, SynEdit, SynMemo, System.Generics.Collections, Vcl.Grids,
  TableDraw, Vcl.StdCtrls, Vcl.ExtCtrls, SQLLang, Vcl.Menus;

type
  TTMCField = record
   private
    FFieldName:string;
    function GetFieldName:string;
    function GetIsOtherUID: Boolean;
   public
    FieldType:TFieldType;
    FieldDataType:string;
    UIDName:Boolean;
    UID:Boolean;
    PrimaryKey:Boolean;
    NotNull:Boolean;
    Orded:Boolean;
    Comments:string;
    property IsOtherUID:Boolean read GetIsOtherUID;
    property FieldName:string read GetFieldName write FFieldName;
    property FieldNameSet:string read FFieldName;
  end;
  TTMCFields = class(TTableData<TTMCField>)
   function ExistsUID:Boolean;
   function GetUID:TTMCField;
   function GetUIDName:TTMCField;
  end;

  TTMCRecord = record
   TableName:string;
   ClassName:string;
   HumanName:string;
   DBName:string;
   UnitName:string;
   Fields:TTMCFields;
   CreateInsert:Boolean;
   CreateUpdate:Boolean;
   CreateUpdateField:Boolean;
   CreateDelete:Boolean;
   CreateGet:Boolean;
   CreateGetUID:Boolean;
   CreateFillList:Boolean;
   CreateFillListField:Boolean;
   CreateClear:Boolean;
  end;
  TTMCData = TTableData<TTMCRecord>;

  TFieldEditMode = (femAdd, femEdit);

  TFormMain = class(TForm)
    MemoData: TSynMemo;
    SynPasSyn2: TSynPasSyn;
    TabSetTables: TTabSet;
    ButtonAddTable: TButton;
    Panel2: TPanel;
    TableExFields: TTableEx;
    Splitter1: TSplitter;
    Panel3: TPanel;
    CheckBoxCreate: TCheckBox;
    CheckBoxCreateInsert: TCheckBox;
    CheckBoxCreateUpdate: TCheckBox;
    CheckBoxCreateDelete: TCheckBox;
    CheckBoxCreateGet: TCheckBox;
    CheckBoxCreateFillList: TCheckBox;
    CheckBoxCreateFillListField: TCheckBox;
    ButtonCreate: TButton;
    Panel4: TPanel;
    EditFieldName: TEdit;
    ComboBoxFieldDataType: TComboBox;
    ComboBoxFieldSQLType: TComboBox;
    CheckBoxFieldPK: TCheckBox;
    CheckBoxFieldNN: TCheckBox;
    ButtonAdd_Apply: TButton;
    ButtonChange_Cancel: TButton;
    ButtonDelete: TButton;
    EditTableName: TEdit;
    EditClass: TEdit;
    EditHumanName: TEdit;
    EditUnitName: TEdit;
    CheckBoxCreateClear: TCheckBox;
    EditDBName: TComboBox;
    EditFieldComments: TEdit;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemQuit: TMenuItem;
    MenuItemSave: TMenuItem;
    ButtonSave: TButton;
    FileSaveDialog: TFileSaveDialog;
    FileOpenDialog: TFileOpenDialog;
    ButtonOpen: TButton;
    CheckBoxFieldOrd: TCheckBox;
    CheckBoxFieldUIDN: TCheckBox;
    CheckBoxFieldUID: TCheckBox;
    CheckBoxCreateGetUID: TCheckBox;
    CheckBoxCreateUpdateField: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure TableExFieldsGetData(FCol, FRow: Integer; var Value: string);
    procedure ButtonAdd_ApplyClick(Sender: TObject);
    procedure ButtonChange_CancelClick(Sender: TObject);
    procedure TabSetTablesChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure ButtonAddTableClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonCreateClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
  private
    FData:TTMCData;
    FCurrentData:Integer;
    FMode:TFieldEditMode;
    FEditRec:Integer;
    procedure CancelEdit;
    procedure SetMode(const Value: TFieldEditMode);
    function SetTable(ID:Integer):Boolean;
  public
    procedure UpdateTabs;
    function Save(ID:Integer; FN:string):Boolean;
    function Open(FN:string):Boolean;
    function Add(Item:TTMCRecord):Integer;
    property Mode:TFieldEditMode read FMode write SetMode;
  end;

var
  FormMain: TFormMain;

implementation
 uses IniFiles;

{$R *.dfm}

function TFormMain.Add(Item:TTMCRecord):Integer;
begin
 Result:=FData.Add(Item);
 UpdateTabs;
 if TabSetTables.Tabs.Count > 0 then TabSetTables.TabIndex:=TabSetTables.Tabs.Count-1;
end;

procedure TFormMain.ButtonOpenClick(Sender: TObject);
begin
 if FileOpenDialog.Execute(Handle) then Open(FileOpenDialog.FileName);
end;

procedure TFormMain.ButtonAddTableClick(Sender: TObject);
var RecData:TTMCRecord;
begin
 RecData.Fields:=TTMCFields.Create(TableExFields);
 RecData.TableName:=EditTableName.Text; EditTableName.Clear;
 RecData.ClassName:=EditClass.Text; EditClass.Clear;
 RecData.HumanName:=EditHumanName.Text; EditHumanName.Clear;
 RecData.DBName:=EditDBName.Text; EditDBName.Text:='';
 RecData.UnitName:=EditUnitName.Text; EditUnitName.Clear;
 Add(RecData);
end;

procedure TFormMain.ButtonAdd_ApplyClick(Sender: TObject);
var FF:TTMCField;
begin
 if not IndexInList(FCurrentData, FData.Count) then Exit;
 FF.FieldName:=EditFieldName.Text;
 FF.FieldDataType:=ComboBoxFieldDataType.Text;
 FF.FieldType:=TFieldType(ComboBoxFieldSQLType.ItemIndex);
 FF.PrimaryKey:=CheckBoxFieldPK.Checked;
 FF.UIDName:=CheckBoxFieldUIDN.Checked;
 FF.UID:=CheckBoxFieldUID.Checked;
 FF.NotNull:=CheckBoxFieldNN.Checked;
 FF.Orded:=CheckBoxFieldOrd.Checked;
 FF.Comments:=EditFieldComments.Text;
 case Mode of
  femAdd:
   begin
    TableExFields.ItemIndex:=FData[FCurrentData].Fields.Add(FF);
   end;
  femEdit:
   begin
    if IndexInList(FEditRec, FData[FCurrentData].Fields.Count) then
     begin
      FData[FCurrentData].Fields[FEditRec]:=FF;
      FData[FCurrentData].Fields.UpdateTable;
     end;
   end;
 end;
 Mode:=femAdd;
end;

procedure TFormMain.ButtonChange_CancelClick(Sender: TObject);
var FF:TTMCField;
begin
 if not IndexInList(FCurrentData, FData.Count) then Exit;
 case Mode of
  femAdd:
   begin
    Mode:=femEdit;
    if not IndexInList(TableExFields.ItemIndex, FData[FCurrentData].Fields.Count) then Exit;
    FEditRec:=TableExFields.ItemIndex;
    FF:=FData[FCurrentData].Fields[FEditRec];
    EditFieldName.Text:=FF.FieldNameSet;
    ComboBoxFieldDataType.Text:=FF.FieldDataType;
    ComboBoxFieldSQLType.ItemIndex:=Ord(FF.FieldType);
    CheckBoxFieldPK.Checked:=FF.PrimaryKey;
    CheckBoxFieldNN.Checked:=FF.NotNull;
    CheckBoxFieldOrd.Checked:=FF.Orded;
    EditFieldComments.Text:=FF.Comments;
    EditFieldName.SetFocus;
   end;
  femEdit:
   begin
    Mode:=femAdd;
   end;
 end;
end;

procedure TFormMain.ButtonCreateClick(Sender: TObject);
var Buf:TStringList;
    RData:TTMCRecord;
    i:Integer;
begin
 if not IndexInList(FCurrentData, FData.Count) then Exit;
 if FData[FCurrentData].Fields.Count <= 0 then Exit;
 RData:=FData[FCurrentData];
 RData.CreateInsert:=CheckBoxCreateInsert.Checked;
 RData.CreateUpdate:=CheckBoxCreateUpdate.Checked;
 RData.CreateUpdateField:=CheckBoxCreateUpdateField.Checked;
 RData.CreateGet:=CheckBoxCreateGet.Checked;
 RData.CreateGetUID:=CheckBoxCreateGetUID.Checked;
 RData.CreateClear:=CheckBoxCreateClear.Checked;
 RData.CreateDelete:=CheckBoxCreateDelete.Checked;
 RData.CreateFillList:=CheckBoxCreateFillList.Checked;
 RData.CreateFillListField:=CheckBoxCreateFillListField.Checked;
 Buf:=TStringList.Create;
 //Uint
 Buf.Add(Format('unit %s;', [RData.UnitName]));
 Buf.Add(Format('', []));
 //interface
 Buf.Add(Format('interface', []));
 Buf.Add(Format(' uses', []));
 Buf.Add(Format('  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,', []));
 Buf.Add(Format('  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Menus, SQLite3, SQLiteTable3,', []));
 Buf.Add(Format('  PawnShop.Constants, SQLLang, System.Generics.Collections, PawnShop.Data, PawnShop.Types, TableDraw;', []));
 Buf.Add(Format('', []));
 //const
 Buf.Add(Format(' const', []));
 Buf.Add(Format('  /// <summary>', []));
 Buf.Add(Format('  /// %s', [RData.HumanName]));
 Buf.Add(Format('  /// </summary>', []));
 Buf.Add(Format('  tn%s = ''%s'';', [RData.HumanName, RData.TableName]));
 for i:= 0 to RData.Fields.Count-1 do
  begin
 Buf.Add(Format('   /// <summary>', []));
 Buf.Add(Format('   /// %s', [RData.Fields[i].Comments]));
 Buf.Add(Format('   /// </summary>', []));
 Buf.Add(Format('   fn%s = ''%s'';', [RData.Fields[i].FieldNameSet, RData.Fields[i].FieldNameSet]));
  end;
 Buf.Add(Format('', []));
 //type
 Buf.Add(Format(' type', []));
 Buf.Add(Format('  TData%s = record', [RData.HumanName]));
  for i:= 0 to RData.Fields.Count-1 do
   begin
 Buf.Add(Format('  /// <summary>', []));
 Buf.Add(Format('  /// %s', [RData.Fields[i].Comments]));
 Buf.Add(Format('  /// </summary>', []));
 Buf.Add(Format('   %s:%s;', [RData.Fields[i].FieldNameSet, RData.Fields[i].FieldDataType]));
   end;
 Buf.Add(Format('  end;', []));
 Buf.Add(Format('  T%s = TTableData<TData%s>;', [RData.ClassName, RData.HumanName]));
 Buf.Add(Format('', []));
 //Class
 Buf.Add(Format('  %s = class', [RData.ClassName]));
 Buf.Add(Format('   class function CreateTable:TTable; static;', []));
 if RData.CreateInsert then
 Buf.Add(Format('   class function Insert(DB:TDataBase; Data:TData%s):TUID;', [RData.HumanName]));
 if RData.CreateUpdate then
 Buf.Add(Format('   class function Update(DB:TDataBase; Data:TData%s):Boolean;', [RData.HumanName]));
 if RData.CreateDelete and (RData.Fields.Count > 0) then
 Buf.Add(Format('   class function Delete(DB:TDataBase; ID:string):Boolean;', []));
 if RData.CreateGet and (RData.Fields.Count > 0) then
 Buf.Add(Format('   class function Get(DB:TDataBase; ID:string; var Data:TData%s):Boolean;', [RData.HumanName]));
 if RData.CreateGetUID and (RData.Fields.Count > 0) and (RData.Fields.ExistsUID) then
 Buf.Add(Format('   class function GetUID(DB:TDataBase; ID:string):TUID;', [RData.HumanName]));
 if RData.CreateFillList then
 Buf.Add(Format('   class function FillList(DB:TDataBase; var List:T%s; Filter:string):Integer;', [RData.ClassName]));
 if RData.CreateFillListField then
 Buf.Add(Format('   class function FillListField(DB:TDataBase; Field:string; var List:TStringList; ADistinct:Boolean = False):Integer;', []));
 if RData.CreateClear then
 Buf.Add(Format('   class function Clear(DB:TDataBase):Integer;', []));
 if RData.CreateUpdateField and (RData.Fields.GetUID.FieldName <> '') then
  begin
   Buf.Add(Format('   class function UpdateField(DB:TDataBase; ID:string; FieldName:string; FieldValue:Integer):Boolean; overload;', []));
   Buf.Add(Format('   class function UpdateField(DB:TDataBase; ID:string; FieldName:string; FieldValue:string):Boolean; overload;', []));
   Buf.Add(Format('   class function UpdateField(DB:TDataBase; ID:string; FieldName:string; FieldValue:Double):Boolean; overload;', []));
   Buf.Add(Format('   class function UpdateField(DB:TDataBase; ID:string; FieldName:string; FieldValue:TDateTime):Boolean; overload;', []));
   Buf.Add(Format('   class function UpdateField(DB:TDataBase; ID:string; FieldName:string; FieldValue:Boolean):Boolean; overload;', []));
  end;
 Buf.Add(Format('  end;', []));
 Buf.Add(Format('', []));
 Buf.Add(Format('implementation', []));
 Buf.Add(Format('', []));
 Buf.Add(Format('{ %s }', [RData.ClassName]));
 Buf.Add(Format('', []));
 //Create
 Buf.Add(Format('class function %s.CreateTable:TTable;', [RData.ClassName]));
 Buf.Add(Format('begin', []));
 Buf.Add(Format(' Result:=TTable.Create(tn%s);', [RData.HumanName]));
 Buf.Add(Format(' with Result do', []));
 Buf.Add(Format('  begin', []));
 for i:= 0 to RData.Fields.Count-1 do
 Buf.Add(Format('   AddField(fn%s, %s, %s, %s);', [RData.Fields[i].FieldNameSet, FieldTypeToString(RData.Fields[i].FieldType), BoolToStr(RData.Fields[i].PrimaryKey ,True), BoolToStr(RData.Fields[i].NotNull ,True)]));
 Buf.Add(Format('  end;', []));
 Buf.Add(Format('end;', []));
 Buf.Add(Format('', []));
 //Update
 if RData.CreateInsert and (RData.Fields.Count > 0) then
  begin
   Buf.Add(Format('class function %s.Update(DB:TDataBase; Data:TData%s):Boolean;', [RData.ClassName, RData.HumanName]));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result:=False;', []));
   Buf.Add(Format(' with SQL.Update(tn%s) do', [RData.HumanName]));
   Buf.Add(Format('  begin', []));
   for i:= 0 to RData.Fields.Count-1 do
    if not RData.Fields[i].Orded then
     Buf.Add(Format('   AddValue(fn%s, Data.%s);', [RData.Fields[i].FieldNameSet, RData.Fields[i].FieldName]))
    else
     Buf.Add(Format('   AddValue(fn%s, Ord(Data.%s));', [RData.Fields[i].FieldNameSet, RData.Fields[i].FieldNameSet]));
   Buf.Add(Format('   WhereFieldEqual(fn%s, Data.%s);', [RData.Fields[0].FieldNameSet, RData.Fields[0].FieldName]));
   Buf.Add(Format('   try', []));
   Buf.Add(Format('    DB.%s.ExecSQL(GetSQL);', [RData.DBName]));
   Buf.Add(Format('    DB.HistoryAdd(haChange, tn%s, Data.%s);', [RData.HumanName, RData.Fields[0].FieldName]));
   Buf.Add(Format('    DB.InitUpdateTable(tn%s);', [RData.HumanName]));
   Buf.Add(Format('    Result:=DB.%s.GetLastChangedRows > 0;', [RData.DBName]));
   Buf.Add(Format('   except', []));
   Buf.Add(Format('    on E:Exception do DB.CreateException(DB.%s, E);', [RData.DBName]));
   Buf.Add(Format('   end;', []));
   Buf.Add(Format('   EndCreate;', []));
   Buf.Add(Format('  end;', []));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));
  end;
 //UpdateField
 if RData.CreateUpdateField and (RData.Fields.GetUID.FieldName <> '') then
  begin
   Buf.Add(Format('class function %s.UpdateField(DB:TDataBase; ID:string; FieldName:string; FieldValue:string):Boolean;', [RData.ClassName]));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result:=CommonSQLFunc.UpdateField(DB, DB.%s, ID, tn%s, FieldName, fn%s, FieldValue);', [RData.DBName, RData.HumanName, RData.Fields[0].FieldNameSet]));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));

   Buf.Add(Format('class function %s.UpdateField(DB:TDataBase; ID:string; FieldName:string; FieldValue:Integer):Boolean;', [RData.ClassName]));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result:=CommonSQLFunc.UpdateField(DB, DB.%s, ID, tn%s, FieldName, fn%s, FieldValue);', [RData.DBName, RData.HumanName, RData.Fields[0].FieldNameSet]));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));

   Buf.Add(Format('class function %s.UpdateField(DB:TDataBase; ID:string; FieldName:string; FieldValue:Double):Boolean;', [RData.ClassName]));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result:=CommonSQLFunc.UpdateField(DB, DB.%s, ID, tn%s, FieldName, fn%s, FieldValue);', [RData.DBName, RData.HumanName, RData.Fields[0].FieldNameSet]));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));

   Buf.Add(Format('class function %s.UpdateField(DB:TDataBase; ID:string; FieldName:string; FieldValue:Boolean):Boolean;', [RData.ClassName]));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result:=CommonSQLFunc.UpdateField(DB, DB.%s, ID, tn%s, FieldName, fn%s, FieldValue);', [RData.DBName, RData.HumanName, RData.Fields[0].FieldNameSet]));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));

   Buf.Add(Format('class function %s.UpdateField(DB:TDataBase; ID:string; FieldName:string; FieldValue:TDateTime):Boolean;', [RData.ClassName]));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result:=CommonSQLFunc.UpdateField(DB, DB.%s, ID, tn%s, FieldName, fn%s, FieldValue);', [RData.DBName, RData.HumanName, RData.Fields[0].FieldNameSet]));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));
  end;
 //Insert
 if RData.CreateUpdate and (RData.Fields.Count > 0) then
  begin
   Buf.Add(Format('class function %s.Insert(DB:TDataBase; Data:TData%s):TUID;', [RData.ClassName, RData.HumanName]));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result.Clear;', []));
   Buf.Add(Format(' with SQL.InsertInto(tn%s) do', [RData.HumanName]));
   Buf.Add(Format('  begin', []));
   Buf.Add(Format('   Data.%s:=DB.GetUID(tn%s);', [RData.Fields[0].FieldName, RData.HumanName]));
   for i:= 0 to RData.Fields.Count-1 do
    if not RData.Fields[i].Orded then
     Buf.Add(Format('   AddValue(fn%s, Data.%s);', [RData.Fields[i].FieldNameSet, RData.Fields[i].FieldName]))
    else
     Buf.Add(Format('   AddValue(fn%s, Ord(Data.%s));', [RData.Fields[i].FieldNameSet, RData.Fields[i].FieldNameSet]));
   Buf.Add(Format('   try', []));
   Buf.Add(Format('    DB.%s.ExecSQL(GetSQL);', [RData.DBName]));
   Buf.Add(Format('    DB.HistoryAdd(haAdd, tn%s, Data.%s);', [RData.HumanName, RData.Fields[0].FieldName]));
   Buf.Add(Format('    DB.InitUpdateTable(tn%s);', [RData.HumanName]));
   if RData.Fields.ExistsUID then
    begin
     Buf.Add(Format('    Data.%s.Name:=Data.%s;', [RData.Fields.GetUID.FieldNameSet, RData.Fields.GetUIDName.FieldName]));
    end;
   Buf.Add(Format('    Result:=Data.%s;', [RData.Fields[0].FieldNameSet]));
   Buf.Add(Format('   except', []));
   Buf.Add(Format('    on E:Exception do DB.CreateException(DB.%s, E);', [RData.DBName]));
   Buf.Add(Format('   end;', []));
   Buf.Add(Format('   EndCreate;', []));
   Buf.Add(Format('  end;', []));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));
  end;
 //Delete
 if RData.CreateDelete and (RData.Fields.Count > 0) then
  begin
   Buf.Add(Format('class function %s.Delete(DB:TDataBase; ID:string):Boolean;', [RData.ClassName]));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result:=False;', []));
   Buf.Add(Format(' with SQL.Delete(tn%s) do', [RData.HumanName]));
   Buf.Add(Format('  begin', []));
   Buf.Add(Format('   WhereFieldEqual(fn%s, ID);', [RData.Fields[0].FieldNameSet]));
   Buf.Add(Format('   try', []));
   Buf.Add(Format('    DB.%s.ExecSQL(GetSQL);', [RData.DBName]));
   Buf.Add(Format('    DB.HistoryAdd(haDelete, tn%s, ID);', [RData.HumanName]));
   Buf.Add(Format('    DB.InitUpdateTable(tn%s);', [RData.HumanName]));
   Buf.Add(Format('    Result:=True;', []));
   Buf.Add(Format('   except', []));
   Buf.Add(Format('    on E:Exception do DB.CreateException(DB.%s, E);', [RData.DBName]));
   Buf.Add(Format('   end;', []));
   Buf.Add(Format('   EndCreate;', []));
   Buf.Add(Format('  end;', []));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));
  end;
 //Get
 if RData.CreateGet and (RData.Fields.Count > 0) then
  begin
   Buf.Add(Format('class function %s.Get(DB:TDataBase; ID:string; var Data:TData%s):Boolean;', [RData.ClassName, RData.HumanName]));
   Buf.Add(Format('var Table:TSQLiteTable;', []));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result:=False;', []));
   Buf.Add(Format(' with SQL.Select(tn%s) do', [RData.HumanName]));
   Buf.Add(Format('  begin', []));
   for i:= 0 to RData.Fields.Count-1 do
   Buf.Add(Format('   AddField(fn%s);', [RData.Fields[i].FieldNameSet]));
   Buf.Add(Format('   WhereFieldEqual(fn%s, ID);', [RData.Fields[0].FieldNameSet]));
   Buf.Add(Format('   Limit:=1;', []));
   Buf.Add(Format('   try', []));
   Buf.Add(Format('    Table:=DB.%s.GetTable(GetSQL);', [RData.DBName]));
   Buf.Add(Format('    if Table.RowCount > 0 then', []));
   Buf.Add(Format('     begin', []));
   for i:= 0 to RData.Fields.Count-1 do
    begin
     case RData.Fields[i].FieldType of
      ftInteger:
       if not RData.Fields[i].Orded then
                  Buf.Add(Format('      Data.%s:=Table.FieldAsInteger(%d);',     [RData.Fields[i].FieldName, i]))
       else       Buf.Add(Format('      Data.%s:=%s(Table.FieldAsInteger(%d));', [RData.Fields[i].FieldName, RData.Fields[i].FieldDataType, i]));
      ftString:   Buf.Add(Format('      Data.%s:=Table.FieldAsString(%d);',      [RData.Fields[i].FieldName, i]));
      ftDateTime: Buf.Add(Format('      Data.%s:=Table.FieldAsDouble(%d);',      [RData.Fields[i].FieldName, i]));
      ftFloat:    Buf.Add(Format('      Data.%s:=Table.FieldAsDouble(%d);',      [RData.Fields[i].FieldName, i]));
      ftBoolean:  Buf.Add(Format('      Data.%s:=Table.FieldAsBoolean(%d);',     [RData.Fields[i].FieldName, i]));
      ftBlob:     Buf.Add(Format('      Data.%s:=Table.FieldAsBlob(%d);',        [RData.Fields[i].FieldName, i]));
     end;
     if RData.Fields[i].IsOtherUID then
                  Buf.Add(Format('      { Заполнить UID другой таблицы }', []));
    end;
   if RData.Fields.ExistsUID then
    begin
     Buf.Add(Format('      Data.%s.Name:=Data.%s;', [RData.Fields.GetUID.FieldNameSet, RData.Fields.GetUIDName.FieldNameSet]));
    end;
   Buf.Add(Format('     end;', []));
   Buf.Add(Format('    Result:=Table.RowCount > 0;', []));
   Buf.Add(Format('    Table.Free;', []));
   Buf.Add(Format('   except', []));
   Buf.Add(Format('    on E:Exception do DB.CreateException(DB.%s, E);', [RData.DBName]));
   Buf.Add(Format('   end;', []));
   Buf.Add(Format('   EndCreate;', []));
   Buf.Add(Format('  end;', []));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));
  end;
 //GetUID
 if RData.CreateGetUID and (RData.Fields.Count > 0) and (RData.Fields.ExistsUID) then
  begin
   Buf.Add(Format('class function %s.GetUID(DB:TDataBase; ID:string):TUID;', [RData.ClassName]));
   Buf.Add(Format('var Table:TSQLiteTable;', []));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result.Clear;', []));
   Buf.Add(Format(' with SQL.Select(tn%s) do', [RData.HumanName]));
   Buf.Add(Format('  begin', []));
   Buf.Add(Format('   AddField(fn%s);', [RData.Fields.GetUIDName.FieldNameSet]));
   Buf.Add(Format('   WhereFieldEqual(fn%s, ID);', [RData.Fields[0].FieldNameSet]));
   Buf.Add(Format('   Limit:=1;', []));
   Buf.Add(Format('   try', []));
   Buf.Add(Format('    Table:=DB.%s.GetTable(GetSQL);', [RData.DBName]));
   Buf.Add(Format('    if Table.RowCount > 0 then', []));
   Buf.Add(Format('     begin', []));
   Buf.Add(Format('      Result.Fill(ID, Table.FieldAsString(0));', []));
   Buf.Add(Format('     end;', []));
   Buf.Add(Format('    Table.Free;', []));
   Buf.Add(Format('   except', []));
   Buf.Add(Format('    on E:Exception do DB.CreateException(DB.%s, E);', [RData.DBName]));
   Buf.Add(Format('   end;', []));
   Buf.Add(Format('   EndCreate;', []));
   Buf.Add(Format('  end;', []));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));
  end;
 //FillList
 if RData.CreateFillList and (RData.Fields.Count > 0) then
  begin
   Buf.Add(Format('class function %s.FillList(DB:TDataBase; var List:T%s; Filter:string):Integer;', [RData.ClassName, RData.ClassName]));
   Buf.Add(Format('var Table:TSQLiteTable;', []));
   Buf.Add(Format('    Item:TData%s;', [RData.HumanName]));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result:=-1;', []));
   Buf.Add(Format(' with SQL.Select(tn%s) do', [RData.HumanName]));
   Buf.Add(Format('  begin', []));
   for i:= 0 to RData.Fields.Count-1 do
   Buf.Add(Format('   AddField(fn%s);', [RData.Fields[i].FieldNameSet]));
   Buf.Add(Format('   WhereStr(Filter);', []));
   Buf.Add(Format('   {OrderBy}', []));
   Buf.Add(Format('   try', []));
   Buf.Add(Format('    Table:=DB.%s.GetTable(GetSQL);', [RData.DBName]));
   Buf.Add(Format('    List.BeginUpdate;', []));
   Buf.Add(Format('    List.Clear;', []));
   Buf.Add(Format('    if Table.RowCount > 0 then', []));
   Buf.Add(Format('     begin', []));
   Buf.Add(Format('      Table.MoveFirst;', []));
   Buf.Add(Format('      while not Table.EOF do', []));
   Buf.Add(Format('       begin', []));
   for i:= 0 to RData.Fields.Count-1 do
    begin
     case RData.Fields[i].FieldType of
      ftInteger:
       if not RData.Fields[i].Orded then
                  Buf.Add(Format('        Item.%s:=Table.FieldAsInteger(%d);',     [RData.Fields[i].FieldName, i]))
       else
                  Buf.Add(Format('        Item.%s:=%s(Table.FieldAsInteger(%d));', [RData.Fields[i].FieldName, RData.Fields[i].FieldDataType, i]));
      ftString:   Buf.Add(Format('        Item.%s:=Table.FieldAsString(%d);',      [RData.Fields[i].FieldName, i]));
      ftDateTime: Buf.Add(Format('        Item.%s:=Table.FieldAsDouble(%d);',      [RData.Fields[i].FieldName, i]));
      ftFloat:    Buf.Add(Format('        Item.%s:=Table.FieldAsDouble(%d);',      [RData.Fields[i].FieldName, i]));
      ftBoolean:  Buf.Add(Format('        Item.%s:=Table.FieldAsBoolean(%d);',     [RData.Fields[i].FieldName, i]));
      ftBlob:     Buf.Add(Format('        Item.%s:=Table.FieldAsBlob(%d);',        [RData.Fields[i].FieldName, i]));
     end;
     if RData.Fields[i].IsOtherUID then
                  Buf.Add(Format('        { Заполнить UID другой таблицы }', []));
    end;
   if RData.Fields.ExistsUID then
    begin
     Buf.Add(Format('        Item.%s.Name:=Item.%s;', [RData.Fields.GetUID.FieldNameSet, RData.Fields.GetUIDName.FieldNameSet]));
    end;
   //Buf.Add(Format('        Item.%s:=Table.FieldAsString(%d);',      [RData.Fields[i].FieldName, i]));
   Buf.Add(Format('        List.Add(Item);', []));
   Buf.Add(Format('        Table.Next;', []));
   Buf.Add(Format('       end;', []));
   Buf.Add(Format('     end;', []));
   Buf.Add(Format('    List.EndUpdate;', []));
   Buf.Add(Format('    Result:=Table.RowCount;', []));
   Buf.Add(Format('    Table.Free;', []));
   Buf.Add(Format('   except', []));
   Buf.Add(Format('    on E:Exception do DB.CreateException(DB.%s, E);', [RData.DBName]));
   Buf.Add(Format('   end;', []));
   Buf.Add(Format('   EndCreate;', []));
   Buf.Add(Format('  end;', []));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));
  end;
 //FillListField
 if RData.CreateFillListField then
  begin
   Buf.Add(Format('class function %s.FillListField(DB:TDataBase; Field:string; var List:TStringList; ADistinct:Boolean):Integer;', [RData.ClassName]));
   Buf.Add(Format('var Table:TSQLiteTable;', []));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result:=-1;', []));
   Buf.Add(Format(' with SQL.Select(tn%s) do', [RData.HumanName]));
   Buf.Add(Format('  begin', []));
   Buf.Add(Format('   AddField(Field);', []));
   Buf.Add(Format('   Distinct:=ADistinct;', []));
   Buf.Add(Format('   OrderBy(Field);', []));
   Buf.Add(Format('   Limit:=1000;', []));
   Buf.Add(Format('   try', []));
   Buf.Add(Format('    Table:=DB.%s.GetTable(GetSQL);', [RData.DBName]));
   Buf.Add(Format('    if Table.RowCount > 0 then', []));
   Buf.Add(Format('     begin', []));
   Buf.Add(Format('      Table.MoveFirst;', []));
   Buf.Add(Format('      while not Table.EOF do', []));
   Buf.Add(Format('       begin', []));
   Buf.Add(Format('        List.Add(Table.FieldAsString(0));', []));
   Buf.Add(Format('        Table.Next;', []));
   Buf.Add(Format('       end;', []));
   Buf.Add(Format('     end;', []));
   Buf.Add(Format('    Result:=Table.RowCount;', []));
   Buf.Add(Format('    Table.Free;', []));
   Buf.Add(Format('   except', []));
   Buf.Add(Format('    on E:Exception do DB.CreateException(DB.%s, E);', [RData.DBName]));
   Buf.Add(Format('   end;', []));
   Buf.Add(Format('   EndCreate;', []));
   Buf.Add(Format('  end;', []));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));
  end;
 //Clear
 if RData.CreateClear then
  begin
   Buf.Add(Format('class function %s.Clear(DB:TDataBase):Integer;', [RData.ClassName]));
   Buf.Add(Format('begin', []));
   Buf.Add(Format(' Result:=-1;', []));
   Buf.Add(Format(' with SQL.Delete(tn%s) do', [RData.HumanName]));
   Buf.Add(Format('  begin', []));
   Buf.Add(Format('   try', []));
   Buf.Add(Format('    DB.%s.ExecSQL(GetSQL);', [RData.DBName]));
   Buf.Add(Format('    Result:=DB.%s.GetLastChangedRows;', [RData.DBName]));
   Buf.Add(Format('    DB.HistoryAdd(haDelete, tn%s, ''ALL'');', [RData.HumanName]));
   Buf.Add(Format('    DB.InitUpdateTable(tn%s);', [RData.HumanName]));
   Buf.Add(Format('   except', []));
   Buf.Add(Format('    on E:Exception do DB.CreateException(DB.%s, E);', [RData.DBName]));
   Buf.Add(Format('   end;', []));
   Buf.Add(Format('   EndCreate;', []));
   Buf.Add(Format('  end;', []));
   Buf.Add(Format('end;', []));
   Buf.Add(Format('', []));
  end;
 Buf.Add(Format('end.', []));
 ////
 MemoData.Clear;
 MemoData.Lines.AddStrings(Buf);
end;

procedure TFormMain.ButtonDeleteClick(Sender: TObject);
begin
 if not IndexInList(FCurrentData, FData.Count) then Exit;
 if not IndexInList(TableExFields.ItemIndex, FData[FCurrentData].Fields.Count) then Exit;
 FData[FCurrentData].Fields.Delete(TableExFields.ItemIndex);
end;

function TFormMain.Save(ID:Integer; FN:string):Boolean;
var Ini:TIniFile;
    Item:TTMCRecord;
    FieldItem:TTMCField;
    Count, i:Integer;
begin
 Result:=False;
 Ini:=TIniFile.Create(FN);
 Item:=FData[ID];
 Ini.WriteString('GENERAL', 'TableName', Item.TableName);
 Ini.WriteString('GENERAL', 'ClassName', Item.ClassName);
 Ini.WriteString('GENERAL', 'HumanName', Item.HumanName);
 Ini.WriteString('GENERAL', 'DBName', Item.DBName);
 Ini.WriteString('GENERAL', 'UnitName', Item.UnitName);
 Ini.WriteInteger('GENERAL', 'FieldCount', Item.Fields.Count);
 if Item.Fields.Count > 0 then
  begin
   for i:= 0 to Item.Fields.Count-1 do
    begin
     Ini.WriteString('FIELD_'+IntToStr(i+1), 'FieldName', Item.Fields[i].FieldNameSet);
     Ini.WriteInteger('FIELD_'+IntToStr(i+1), 'FieldType', Ord(Item.Fields[i].FieldType));
     Ini.WriteString('FIELD_'+IntToStr(i+1), 'FieldDataType', Item.Fields[i].FieldDataType);
     Ini.WriteString('FIELD_'+IntToStr(i+1), 'Comments', Item.Fields[i].Comments);
     Ini.WriteBool('FIELD_'+IntToStr(i+1), 'PrimaryKey', Item.Fields[i].PrimaryKey);
     Ini.WriteBool('FIELD_'+IntToStr(i+1), 'UIDName', Item.Fields[i].UIDName);
     Ini.WriteBool('FIELD_'+IntToStr(i+1), 'UID', Item.Fields[i].UID);
     Ini.WriteBool('FIELD_'+IntToStr(i+1), 'NotNull', Item.Fields[i].NotNull);
     Ini.WriteBool('FIELD_'+IntToStr(i+1), 'Orded', Item.Fields[i].Orded);
    end;
  end;
 Ini.Free;
 Result:=True;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
begin
 if not IndexInList(FCurrentData, FData.Count) then Exit;
 if FileSaveDialog.Execute(Handle) then Save(FCurrentData, FileSaveDialog.FileName);
end;

procedure TFormMain.CancelEdit;
var FF:TTMCField;
begin
 if not IndexInList(FCurrentData, FData.Count) then Exit;
 if not IndexInList(TableExFields.ItemIndex, FData[FCurrentData].Fields.Count) then Exit;
 //FF:=FData[FCurrentData].Fields[TableExFields.ItemIndex];
 EditFieldName.Text:='';
 ComboBoxFieldDataType.Text:='';
 CheckBoxFieldPK.Checked:=False;
 CheckBoxFieldNN.Checked:=False;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var FF:TTMCField;
begin
 FData:=TTMCData.Create(nil);
 with TableExFields do
  begin
   FirstColumn('№ п/п', 60);
   AddColumn('Имя поля', 200);
   AddColumn('Тип данных', 120);
   AddColumn('Orded', 60);
   AddColumn('Тип данных SQL', 120);
   AddColumn('PrimaryKey', 60);
   AddColumn('NotNull', 60);
   AddColumn('УИД', 60);
   AddColumn('УИД описание', 60);
   AddColumn('Описание/Комментарий', 200);
   AddColumn(' ', 60);
  end;

 ComboBoxFieldSQLType.Items.Clear;
 ComboBoxFieldSQLType.Items.Add('Integer');
 ComboBoxFieldSQLType.Items.Add('String');
 ComboBoxFieldSQLType.Items.Add('Float');
 ComboBoxFieldSQLType.Items.Add('DateTime');
 ComboBoxFieldSQLType.Items.Add('Blob');
 ComboBoxFieldSQLType.Items.Add('Boolean');
 ComboBoxFieldSQLType.ItemIndex:=0;

 ComboBoxFieldDataType.Items.Clear;
 ComboBoxFieldDataType.Items.Add('Integer');
 ComboBoxFieldDataType.Items.Add('String');
 ComboBoxFieldDataType.Items.Add('Double');
 ComboBoxFieldDataType.Items.Add('TDateTime');
 ComboBoxFieldDataType.Items.Add('Boolean');
 ComboBoxFieldDataType.Items.Add('TUID');
 Mode:=femAdd;
 UpdateTabs;

 ButtonAddTableClick(nil);

 FF.FieldName:='PM_UID';
 FF.FieldType:=ftString;
 FF.FieldDataType:='TUID';
 FF.PrimaryKey:=True;
 FF.UID:=True;
 FF.UIDName:=False;
 FF.NotNull:=True;
 FF.Orded:=False;
 FF.Comments:='ИД записи';
 FData[FCurrentData].Fields.Add(FF);

 FF.FieldName:='PM_NAME';
 FF.FieldType:=ftString;
 FF.FieldDataType:='String';
 FF.PrimaryKey:=False;
 FF.UID:=False;
 FF.UIDName:=True;
 FF.NotNull:=True;
 FF.Orded:=False;
 FF.Comments:='Наименование';
 FData[FCurrentData].Fields.Add(FF);

 FF.FieldName:='PM_CAT';
 FF.FieldType:=ftInteger;
 FF.FieldDataType:='TCat';
 FF.PrimaryKey:=False;
 FF.UID:=False;
 FF.UIDName:=False;
 FF.NotNull:=True;
 FF.Orded:=True;
 FF.Comments:='Категория';
 FData[FCurrentData].Fields.Add(FF);

 FF.FieldName:='PM_CUT';
 FF.FieldType:=ftString;
 FF.FieldDataType:='String';
 FF.PrimaryKey:=False;
 FF.UID:=False;
 FF.UIDName:=False;
 FF.NotNull:=True;
 FF.Orded:=False;
 FF.Comments:='Сокр';
 FData[FCurrentData].Fields.Add(FF);

 FF.FieldName:='PM_CLIENT';
 FF.FieldType:=ftString;
 FF.FieldDataType:='TUID';
 FF.PrimaryKey:=False;
 FF.UID:=False;
 FF.UIDName:=False;
 FF.NotNull:=True;
 FF.Orded:=False;
 FF.Comments:='ИД клиента';
 FData[FCurrentData].Fields.Add(FF);
end;

function TFormMain.Open(FN:string):Boolean;
var Ini:TIniFile;
    Item:TTMCRecord;
    FieldItem:TTMCField;
    Count, i:Integer;
begin
 Result:=False;
 Ini:=TIniFile.Create(FN);
 Item.TableName:=Ini.ReadString('GENERAL', 'TableName', '');
 Item.ClassName:=Ini.ReadString('GENERAL', 'ClassName', '');
 Item.HumanName:=Ini.ReadString('GENERAL', 'HumanName', '');
 Item.DBName:=Ini.ReadString('GENERAL', 'DBName', '');
 Item.UnitName:=Ini.ReadString('GENERAL', 'UnitName', '');
 Count:=Ini.ReadInteger('GENERAL', 'FieldCount', 0);
 Item.Fields:=TTMCFields.Create(TableExFields);
 if Count > 0 then
  begin
   for i:= 1 to Count do
    begin
     FieldItem.FieldName:=Ini.ReadString('FIELD_'+IntToStr(i), 'FieldName', '');
     FieldItem.FieldType:=TFieldType(Ini.ReadInteger('FIELD_'+IntToStr(i), 'FieldType', 0));
     FieldItem.FieldDataType:=Ini.ReadString('FIELD_'+IntToStr(i), 'FieldDataType', '');
     FieldItem.Comments:=Ini.ReadString('FIELD_'+IntToStr(i), 'Comments', '');
     FieldItem.PrimaryKey:=Ini.ReadBool('FIELD_'+IntToStr(i), 'PrimaryKey', False);
     FieldItem.UID:=Ini.ReadBool('FIELD_'+IntToStr(i), 'UID', False);
     FieldItem.UIDName:=Ini.ReadBool('FIELD_'+IntToStr(i), 'UIDName', False);
     FieldItem.NotNull:=Ini.ReadBool('FIELD_'+IntToStr(i), 'NotNull', False);
     FieldItem.Orded:=Ini.ReadBool('FIELD_'+IntToStr(i), 'Orded', False);
     Item.Fields.Add(FieldItem);
    end;
  end;
 Ini.Free;
 Result:=Add(Item) >= 0;
end;

procedure TFormMain.SetMode(const Value: TFieldEditMode);
begin
 CancelEdit;
 FMode:=Value;
 case FMode of
  femAdd:
   begin
    ButtonAdd_Apply.Caption:='Добавить';
    ButtonChange_Cancel.Caption:='Изменить';
    TableExFields.Enabled:=True;
   end;
  femEdit:
   begin
    TableExFields.Enabled:=False;
    ButtonAdd_Apply.Caption:='Применить';
    ButtonChange_Cancel.Caption:='Отменить';
   end;
 end;
end;

function TFormMain.SetTable(ID:Integer):Boolean;
begin
 if not IndexInList(ID, FData.Count) then Exit(False);
 FCurrentData:=ID;
 FData[FCurrentData].Fields.UpdateTable;
 Result:=True;
end;

procedure TFormMain.TableExFieldsGetData(FCol, FRow: Integer; var Value: string);
begin
 Value:='';
 if not IndexInList(FCurrentData, FData.Count) then Exit;
 if not IndexInList(FRow, FData[FCurrentData].Fields.Count) then Exit;
 case FCol of
  0:Value:=IntToStr(FRow+1);
  1:Value:=FData[FCurrentData].Fields[FRow].FieldNameSet;
  2:Value:=FData[FCurrentData].Fields[FRow].FieldDataType;
  3:Value:=BoolToStr(FData[FCurrentData].Fields[FRow].Orded, True);
  4:Value:=FieldTypeToStr(FData[FCurrentData].Fields[FRow].FieldType);
  5:Value:=BoolToStr(FData[FCurrentData].Fields[FRow].PrimaryKey, True);
  6:Value:=BoolToStr(FData[FCurrentData].Fields[FRow].NotNull, True);
  7:Value:=BoolToStr(FData[FCurrentData].Fields[FRow].UID, True);
  8:Value:=BoolToStr(FData[FCurrentData].Fields[FRow].UIDName, True);
  9:Value:=FData[FCurrentData].Fields[FRow].Comments;
 end;
end;

procedure TFormMain.TabSetTablesChange(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
begin
 AllowChange:=SetTable(NewTab);
end;

procedure TFormMain.UpdateTabs;
var i, ti:Integer;
begin
 ti:=TabSetTables.TabIndex;
 TabSetTables.Tabs.BeginUpdate;
 TabSetTables.Tabs.Clear;
 for i:= 0 to FData.Count-1 do TabSetTables.Tabs.Add(FData[i].TableName);
 TabSetTables.Tabs.EndUpdate;
 if not((ti < -1) or (ti >= TabSetTables.Tabs.Count)) then TabSetTables.TabIndex:=ti;
end;

{ TTMCField }

function TTMCField.GetFieldName: string;
begin
 Result:= FFieldName;
 if FieldDataType = 'TUID' then Result:=Result+'.ID';
end;

function TTMCField.GetIsOtherUID:Boolean;
begin
 Result:=(not UID) and (FieldDataType = 'TUID');
end;

{ TTMCFields }

function TTMCFields.ExistsUID:Boolean;
var i:Integer;
begin
 Result:=False;
 for i:= 0 to Count-1 do if Items[i].UID then begin Result:=True; Break; end;
 if not Result then Exit;
 Result:=False;
 for i:= 0 to Count-1 do if Items[i].UIDName then begin Result:=True; Break; end;
 if not Result then Exit;
end;

function TTMCFields.GetUID:TTMCField;
var i:Integer;
begin
 Result.FieldName:='';
 for i:= 0 to Count-1 do if Items[i].UID then Exit(Items[i]);
end;

function TTMCFields.GetUIDName:TTMCField;
var i:Integer;
begin
 Result.FieldName:='';
 for i:= 0 to Count-1 do if Items[i].UIDName then Exit(Items[i]);
end;

end.
