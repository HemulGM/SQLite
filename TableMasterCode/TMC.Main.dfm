object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = #1052#1072#1089#1090#1077#1088' '#1075#1077#1085#1077#1088#1072#1094#1080#1080' '#1082#1086#1076#1072
  ClientHeight = 652
  ClientWidth = 1115
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 272
    Width = 1115
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ResizeStyle = rsUpdate
    ExplicitTop = 288
    ExplicitWidth = 299
  end
  object MemoData: TSynMemo
    Left = 0
    Top = 310
    Width = 1115
    Height = 342
    Align = alClient
    Color = 3222569
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    BorderStyle = bsNone
    Gutter.Color = 2564897
    Gutter.BorderColor = 2827812
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = 15987699
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Highlighter = SynPasSyn2
    Lines.Strings = (
      'unit PawnShop.Data.Countries;'
      ''
      'interface'
      ' uses'
      
        '  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Varia' +
        'nts, System.Classes, Vcl.Graphics,'
      
        '  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWi' +
        'n, Vcl.Menus, SQLite3, SQLiteTable3,'
      
        '  PawnShop.Constants, SQLLang, System.Generics.Collections, Pawn' +
        'Shop.Data, PawnShop.Types, TableDraw;'
      ''
      ' const'
      '  //'#1057#1090#1088#1072#1085#1099
      '  tnCountries = '#39'countries'#39';'
      '   fnCO_ID = '#39'CO_ID'#39';'
      '   fnCO_Key = '#39'CO_Key'#39';'
      '   fnCO_NAME = '#39'CO_NAME'#39';'
      '   fnCO_NAMEENG = '#39'CO_NAMEENG'#39';'
      '   fnCO_DESC = '#39'CO_DESC'#39';'
      '   fnCO_A2 = '#39'CO_A2'#39';'
      '   fnCO_A3 = '#39'CO_A3'#39';'
      '   fnCO_LOCATION = '#39'CO_LOCATION'#39';'
      '   fnCO_LOCATION_PREC = '#39'CO_LOCATION_PREC'#39';'
      '   fnCO_ORDER = '#39'CO_ORDER'#39';'
      '   fnCO_GROUP = '#39'CO_GROUP'#39';'
      ''
      ' type'
      '  TDataCountry = record'
      '   CO_ID:TCO_ID;'
      '   CO_Key:TCO_Key;'
      '   CO_NAME:string;'
      '   CO_NAMEENG:string;'
      '   CO_DESC:string;'
      '   CO_A2:TCO_A2;'
      '   CO_A3:TCO_A3;'
      '   CO_LOCATION:string;'
      '   CO_LOCATION_PREC:string;'
      '   CO_ORDER:TRecOrder;'
      '   CO_GROUP:Integer;'
      '  end;'
      '  TCountries = TTableData<TDataCountry>;'
      ''
      '  TDataCitizenship = record'
      '   CO_ID:TCO_ID;'
      '   CO_Key:TCO_Key;'
      '   CO_NAME:string;'
      '   CO_NAMEENG:string;'
      '   CO_DESC:string;'
      '   CO_GROUP:Integer;'
      '  end;'
      '  TCitizenships = class(TList<TDataCitizenship>)'
      '   function Find(CO_ID:TCO_ID):Integer;'
      '  end;'
      ''
      '  Countries = class'
      '   class function CreateTable:TTable; static;'
      
        '   class function Insert(DB:TDataBase; Data:TDataCountry):Intege' +
        'r;'
      
        '   class function Update(DB:TDataBase; Data:TDataCountry):Intege' +
        'r;'
      '   class function Delete(DB:TDataBase; ID:TCO_ID):Boolean;'
      
        '   class function Get(DB:TDataBase; ID:TCO_ID; var Data:TDataCou' +
        'ntry):Boolean;'
      '   class function GetName(DB:TDataBase; ID:TCO_ID):string;'
      
        '   class function FillList(DB:TDataBase; var List:TCountries; Fi' +
        'lter:string):Integer;'
      
        '   class function FillListField(DB:TDataBase; Field:string; var ' +
        'List:TStringList; ADistinct:Boolean = False):Integer;'
      '  end;'
      ''
      '  Citizenship = class'
      
        '   class function Get(DB:TDataBase; ID:TCO_ID; var Data:TDataCit' +
        'izenship):Boolean;'
      
        '   class function FillList(DB:TDataBase; var List:TCitizenships;' +
        ' Filter:string = '#39#39'):Integer;'
      '  end;'
      ''
      'var'
      '  NoCitizenship:TDataCitizenship;'
      ''
      'implementation'
      ''
      '{ Countries }'
      ''
      'class function Countries.CreateTable: TTable;'
      'begin'
      ' Result:=TTable.Create(tnCountries);'
      ' with Result do'
      '  begin'
      '   AddField(fnCO_ID, ftInteger, True, True);'
      '   AddField(fnCO_Key, ftInteger, False, False);'
      '   AddField(fnCO_NAME, ftString, False, False);'
      '   AddField(fnCO_NAMEENG, ftString, False, False);'
      '   AddField(fnCO_DESC, ftString, False, False);'
      '   AddField(fnCO_A2, ftString, False, False);'
      '   AddField(fnCO_A3, ftString, False, False);'
      '   AddField(fnCO_LOCATION, ftString, False, False);'
      '   AddField(fnCO_LOCATION_PREC, ftString, False, False);'
      '   AddField(fnCO_ORDER, ftFloat, False, False);'
      '   AddField(fnCO_GROUP, ftInteger, False, False);'
      '  end;'
      'end;'
      ''
      
        'class function Countries.Delete(DB: TDataBase; ID: TCO_ID): Bool' +
        'ean;'
      'begin'
      ' Result:=False;'
      ' with SQL.Delete(tnCountries) do'
      '  begin'
      '   WhereFieldEqual(fnCO_ID, ID);'
      '   try'
      '    DB.DBCatalogs.ExecSQL(GetSQL);'
      '    Result:=True;'
      '    DB.InitUpdateTable(tnCountries);'
      '   except'
      '    on E:Exception do DB.CreateException(DB.DBCatalogs, E);'
      '   end;'
      '   EndCreate;'
      '  end;'
      'end;'
      ''
      
        'class function Countries.FillList(DB: TDataBase; var List: TCoun' +
        'tries; Filter: string): Integer;'
      'var Table:TSQLiteTable;'
      '    Item:TDataCountry;'
      'begin'
      ' Result:=-1;'
      ' with SQL.Select(tnCountries) do'
      '  begin'
      '   AddField(fnCO_ID);'
      '   AddField(fnCO_Key);'
      '   AddField(fnCO_NAME);'
      '   AddField(fnCO_NAMEENG);'
      '   AddField(fnCO_DESC);'
      '   AddField(fnCO_A2);'
      '   AddField(fnCO_A3);'
      '   AddField(fnCO_LOCATION);'
      '   AddField(fnCO_LOCATION_PREC);'
      '   AddField(fnCO_ORDER);'
      '   AddField(fnCO_GROUP);'
      '   WhereStr(Filter);'
      '   OrderBy(fnCO_ORDER);'
      '   OrderBy(fnCO_NAME);'
      '   try'
      '    Table:=DB.DBCatalogs.GetTable(GetSQL);'
      '    List.BeginUpdate;'
      '    List.Clear;'
      '    if Table.RowCount > 0 then'
      '     begin'
      '      Table.MoveFirst;'
      '      while not Table.EOF do'
      '       begin'
      '        Item.CO_ID:=Table.FieldAsInteger(0);'
      '        Item.CO_Key:=Table.FieldAsInteger(1);'
      '        Item.CO_NAME:=Table.FieldAsString(2);'
      '        Item.CO_NAMEENG:=Table.FieldAsString(3);'
      '        Item.CO_DESC:=Table.FieldAsString(4);'
      '        Item.CO_A2:=Table.FieldAsString(5);'
      '        Item.CO_A3:=Table.FieldAsString(6);'
      '        Item.CO_LOCATION:=Table.FieldAsString(7);'
      '        Item.CO_LOCATION_PREC:=Table.FieldAsString(8);'
      '        Item.CO_ORDER:=Table.FieldAsDouble(9);'
      '        Item.CO_GROUP:=Table.FieldAsInteger(10);'
      '        List.Add(Item);'
      '        Table.Next;'
      '       end;'
      '     end;'
      '    Result:=Table.RowCount;'
      '   except'
      '    on E:Exception do DB.CreateException(DB.DBCatalogs, E);'
      '   end;'
      '   List.EndUpdate;'
      '   EndCreate;'
      '  end;'
      'end;'
      ''
      
        'class function Countries.FillListField(DB: TDataBase; Field: str' +
        'ing; var List: TStringList; ADistinct: Boolean): Integer;'
      'var Table:TSQLiteTable;'
      'begin'
      ' Result:=-1;'
      ' with SQL.Select(tnCountries) do'
      '  begin'
      '   AddField(Field);'
      '   Distinct:=ADistinct;'
      '   OrderBy(fnCO_ORDER);'
      '   OrderBy(Field);'
      '   Limit:=1000;'
      '   try'
      '    Table:=DB.DBCatalogs.GetTable(GetSQL);'
      '    List.Clear;'
      '    if Table.RowCount > 0 then'
      '     begin'
      '      Table.MoveFirst;'
      '      while not Table.EOF do'
      '       begin'
      '        List.Add(Table.FieldAsString(0));'
      '        Table.Next;'
      '       end;'
      '     end;'
      '    Result:=Table.RowCount;'
      '   except'
      '    on E:Exception do DB.CreateException(DB.DBCatalogs, E);'
      '   end;'
      '   EndCreate;'
      '  end;'
      'end;'
      ''
      
        'class function Countries.Get(DB: TDataBase; ID: TCO_ID; var Data' +
        ': TDataCountry): Boolean;'
      'var Table:TSQLiteTable;'
      'begin'
      ' Result:=False;'
      ' with SQL.Select(tnCountries) do'
      '  begin'
      '   AddField(fnCO_ID);'
      '   AddField(fnCO_Key);'
      '   AddField(fnCO_NAME);'
      '   AddField(fnCO_NAMEENG);'
      '   AddField(fnCO_DESC);'
      '   AddField(fnCO_A2);'
      '   AddField(fnCO_A3);'
      '   AddField(fnCO_LOCATION);'
      '   AddField(fnCO_LOCATION_PREC);'
      '   AddField(fnCO_GROUP);'
      '   WhereFieldEqual(fnCO_ID, ID);'
      '   Limit:=1;'
      '   try'
      '    Table:=DB.DBCatalogs.GetTable(GetSQL);'
      '    if Table.RowCount > 0 then'
      '     begin'
      '      Data.CO_ID:=Table.FieldAsInteger(0);'
      '      Data.CO_Key:=Table.FieldAsInteger(1);'
      '      Data.CO_NAME:=Table.FieldAsString(2);'
      '      Data.CO_NAMEENG:=Table.FieldAsString(3);'
      '      Data.CO_DESC:=Table.FieldAsString(4);'
      '      Data.CO_A2:=Table.FieldAsString(5);'
      '      Data.CO_A3:=Table.FieldAsString(6);'
      '      Data.CO_LOCATION:=Table.FieldAsString(7);'
      '      Data.CO_LOCATION_PREC:=Table.FieldAsString(8);'
      '      Data.CO_GROUP:=Table.FieldAsInteger(9);'
      '     end;'
      '    Result:=Table.RowCount > 0;'
      '   except'
      '    on E:Exception do DB.CreateException(DB.DBCatalogs, E);'
      '   end;'
      '   EndCreate;'
      '  end;'
      'end;'
      ''
      
        'class function Countries.GetName(DB: TDataBase; ID: TCO_ID): str' +
        'ing;'
      'var Table:TSQLiteTable;'
      'begin'
      ' Result:='#39#39';'
      ' with SQL.Select(tnCountries) do'
      '  begin'
      '   AddField(fnCO_NAME);'
      '   WhereFieldEqual(fnCO_ID, ID);'
      '   Limit:=1;'
      '   try'
      '    Table:=DB.DBCatalogs.GetTable(GetSQL);'
      '    if Table.RowCount > 0 then Result:=Table.FieldAsString(0);'
      '   except'
      '    on E:Exception do DB.CreateException(DB.DBCatalogs, E);'
      '   end;'
      '   EndCreate;'
      '  end;'
      'end;'
      ''
      
        'class function Countries.Insert(DB: TDataBase; Data: TDataCountr' +
        'y): Integer;'
      'begin'
      ''
      'end;'
      ''
      
        'class function Countries.Update(DB: TDataBase; Data: TDataCountr' +
        'y): Integer;'
      'begin'
      ''
      'end;'
      ''
      '{ Citizenship }'
      ''
      
        'class function Citizenship.FillList(DB: TDataBase; var List: TCi' +
        'tizenships; Filter: string = '#39#39'): Integer;'
      'var FullList:TCountries;'
      '    Item:TDataCitizenship;'
      '    i:Integer;'
      'begin'
      ' FullList:=TCountries.Create(nil);'
      ' Countries.FillList(DB, FullList, Filter);'
      ' List.Clear;'
      ' List.Add(NoCitizenship);'
      ' for i:= 0 to FullList.Count - 1 do'
      '  begin'
      '   Item.CO_ID:=FullList[i].CO_ID;'
      '   Item.CO_Key:=FullList[i].CO_Key;'
      '   Item.CO_NAME:=FullList[i].CO_NAME;'
      '   Item.CO_NAMEENG:=FullList[i].CO_NAMEENG;'
      '   Item.CO_DESC:=FullList[i].CO_DESC;'
      '   Item.CO_GROUP:=FullList[i].CO_GROUP;'
      '   List.Add(Item);'
      '  end;'
      ' FullList.Free;'
      ' Result:=List.Count;'
      'end;'
      ''
      
        'class function Citizenship.Get(DB: TDataBase; ID: TCO_ID; var Da' +
        'ta: TDataCitizenship): Boolean;'
      'var CountItem:TDataCountry;'
      'begin'
      ' Result:=False;'
      ' if ID = NoCitizenship.CO_ID then'
      '  begin'
      '   Data:=NoCitizenship;'
      '   Exit(True);'
      '  end;'
      ' if not Countries.Get(DB, ID, CountItem) then Exit;'
      ' Data.CO_ID:=CountItem.CO_ID;'
      ' Data.CO_Key:=CountItem.CO_Key;'
      ' Data.CO_NAME:=CountItem.CO_NAME;'
      ' Data.CO_NAMEENG:=CountItem.CO_NAMEENG;'
      ' Data.CO_DESC:=CountItem.CO_DESC;'
      ' Data.CO_GROUP:=CountItem.CO_GROUP;'
      ' Result:=True;'
      'end;'
      ''
      ''
      '{ TCitizenships }'
      ''
      'function TCitizenships.Find(CO_ID: TCO_ID): Integer;'
      'var i:Integer;'
      'begin'
      ' Result:=-1;'
      ' for i:= 0 to Count-1 do'
      '  if Items[i].CO_ID = CO_ID then Exit(i);'
      'end;'
      ''
      'initialization'
      ' NoCitizenship.CO_ID:=-1;'
      ' NoCitizenship.CO_Key:=CitizenshipNoID;'
      ' NoCitizenship.CO_NAME:=CitizenshipNo;'
      ' NoCitizenship.CO_NAMEENG:=CitizenshipNoEng;'
      ' NoCitizenship.CO_DESC:=CitizenshipNo;'
      ' NoCitizenship.CO_GROUP:=0;'
      ''
      'end.')
    RightEdgeColor = 2630690
    SelectedColor.Background = 1709845
    FontSmoothing = fsmClearType
  end
  object TableExFields: TTableEx
    Left = 0
    Top = 109
    Width = 1115
    Height = 163
    ColumnsHeight = 30
    Align = alTop
    BorderStyle = bsNone
    Color = 3222569
    DefaultRowHeight = 25
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ItemIndex = -1
    GetData = TableExFieldsGetData
    VisibleEdit = True
    ItemCount = 2
    LineColor = 3222569
    LineColorXor = 3222569
    LineHotColor = 2499104
    LineSelColor = 4340791
    ColumnsColor = 2499104
    FontHotLine.Charset = DEFAULT_CHARSET
    FontHotLine.Color = clWhite
    FontHotLine.Height = -11
    FontHotLine.Name = 'Tahoma'
    FontHotLine.Style = []
    FontLine.Charset = DEFAULT_CHARSET
    FontLine.Color = clSilver
    FontLine.Height = -11
    FontLine.Name = 'Tahoma'
    FontLine.Style = []
    FontSelLine.Charset = DEFAULT_CHARSET
    FontSelLine.Color = clWhite
    FontSelLine.Height = -11
    FontSelLine.Name = 'Tahoma'
    FontSelLine.Style = [fsBold]
    ColumnsFont.Charset = DEFAULT_CHARSET
    ColumnsFont.Color = clSilver
    ColumnsFont.Height = -11
    ColumnsFont.Name = 'Tahoma'
    ColumnsFont.Style = []
    SetFocusOnEnter = False
    ColWidths = (
      1115)
    RowHeights = (
      30
      25
      25)
  end
  object TabSetTables: TTabSet
    Left = 0
    Top = 85
    Width = 1115
    Height = 24
    Align = alTop
    AutoScroll = False
    DitherBackground = False
    EndMargin = 60
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentShowHint = False
    ShowHint = False
    ShrinkToFit = True
    SoftTop = True
    Style = tsSoftTabs
    TabHeight = 25
    Tabs.Strings = (
      'price_metal'
      'price_probe'
      'pricelist')
    TabIndex = 0
    TabPosition = tpTop
    OnChange = TabSetTablesChange
  end
  object Panel3: TPanel
    Left = 0
    Top = 275
    Width = 1115
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object CheckBoxCreate: TCheckBox
      AlignWithMargins = True
      Left = 135
      Top = 3
      Width = 55
      Height = 29
      Align = alLeft
      Caption = 'Create'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 0
    end
    object CheckBoxCreateInsert: TCheckBox
      AlignWithMargins = True
      Left = 196
      Top = 3
      Width = 51
      Height = 29
      Align = alLeft
      Caption = 'Insert'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBoxCreateUpdate: TCheckBox
      AlignWithMargins = True
      Left = 253
      Top = 3
      Width = 55
      Height = 29
      Align = alLeft
      Caption = 'Update'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object CheckBoxCreateDelete: TCheckBox
      AlignWithMargins = True
      Left = 399
      Top = 3
      Width = 53
      Height = 29
      Align = alLeft
      Caption = 'Delete'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBoxCreateGet: TCheckBox
      AlignWithMargins = True
      Left = 458
      Top = 3
      Width = 39
      Height = 29
      Align = alLeft
      Caption = 'Get'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBoxCreateFillList: TCheckBox
      AlignWithMargins = True
      Left = 569
      Top = 3
      Width = 50
      Height = 29
      Align = alLeft
      Caption = 'FillList'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object CheckBoxCreateFillListField: TCheckBox
      AlignWithMargins = True
      Left = 625
      Top = 3
      Width = 75
      Height = 29
      Align = alLeft
      Caption = 'FillListField'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object ButtonCreate: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 126
      Height = 29
      Align = alLeft
      Caption = #1057#1075#1077#1085#1077#1088#1080#1088#1086#1074#1072#1090#1100' '#1082#1086#1076
      TabOrder = 7
      OnClick = ButtonCreateClick
    end
    object CheckBoxCreateClear: TCheckBox
      AlignWithMargins = True
      Left = 706
      Top = 3
      Width = 46
      Height = 29
      Align = alLeft
      Caption = 'Clear'
      Checked = True
      State = cbChecked
      TabOrder = 8
    end
    object ButtonSave: TButton
      AlignWithMargins = True
      Left = 1008
      Top = 3
      Width = 104
      Height = 29
      Align = alRight
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100'...'
      TabOrder = 9
      OnClick = ButtonSaveClick
    end
    object ButtonOpen: TButton
      AlignWithMargins = True
      Left = 898
      Top = 3
      Width = 104
      Height = 29
      Align = alRight
      Caption = #1054#1090#1082#1088#1099#1090#1100
      TabOrder = 10
      OnClick = ButtonOpenClick
    end
    object CheckBoxCreateGetUID: TCheckBox
      AlignWithMargins = True
      Left = 503
      Top = 3
      Width = 60
      Height = 29
      Align = alLeft
      Caption = 'GetUID'
      Checked = True
      State = cbChecked
      TabOrder = 11
    end
    object CheckBoxCreateUpdateField: TCheckBox
      AlignWithMargins = True
      Left = 314
      Top = 3
      Width = 79
      Height = 29
      Align = alLeft
      Caption = 'UpdateField'
      Checked = True
      State = cbChecked
      TabOrder = 12
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 1115
    Height = 29
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkSoft
    BevelOuter = bvNone
    TabOrder = 4
    object ButtonAddTable: TButton
      AlignWithMargins = True
      Left = 918
      Top = 2
      Width = 26
      Height = 23
      Margins.Top = 2
      Margins.Bottom = 2
      Align = alLeft
      Caption = '+'
      TabOrder = 0
      OnClick = ButtonAddTableClick
    end
    object EditTableName: TEdit
      AlignWithMargins = True
      Left = 552
      Top = 3
      Width = 177
      Height = 21
      Align = alLeft
      TabOrder = 1
      Text = 'price_metal'
      TextHint = #1048#1084#1103' '#1090#1072#1073#1083#1080#1094#1099
    end
    object EditClass: TEdit
      AlignWithMargins = True
      Left = 369
      Top = 3
      Width = 177
      Height = 21
      Align = alLeft
      TabOrder = 2
      Text = 'PriceMetals'
      TextHint = #1048#1084#1103' '#1082#1083#1072#1089#1089#1072
    end
    object EditHumanName: TEdit
      AlignWithMargins = True
      Left = 735
      Top = 3
      Width = 177
      Height = 21
      Align = alLeft
      TabOrder = 3
      Text = 'PriceMetal'
      TextHint = #1042#1085#1091#1090#1088#1077#1085#1085#1077#1077' '#1080#1084#1103' '#1090#1072#1073#1083#1080#1094#1099
    end
    object EditUnitName: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 177
      Height = 21
      Align = alLeft
      TabOrder = 4
      Text = 'PawnShop.Data.Price.Metal'
      TextHint = #1048#1084#1103' '#1084#1086#1076#1091#1083#1103
    end
    object EditDBName: TComboBox
      AlignWithMargins = True
      Left = 186
      Top = 3
      Width = 177
      Height = 21
      Align = alLeft
      ItemIndex = 0
      TabOrder = 5
      Text = 'DBGeneral'
      TextHint = #1041#1072#1079#1072' '#1076#1072#1085#1085#1099#1093
      Items.Strings = (
        'DBGeneral'
        'DBCatalogs'
        'DBFIAS')
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 29
    Width = 1115
    Height = 56
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 5
    object EditFieldName: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 177
      Height = 21
      TabOrder = 0
      TextHint = #1048#1084#1103' '#1087#1086#1083#1103
    end
    object ComboBoxFieldDataType: TComboBox
      AlignWithMargins = True
      Left = 3
      Top = 30
      Width = 177
      Height = 21
      TabOrder = 1
      TextHint = #1042#1085#1091#1090#1088#1077#1085#1085#1080#1081' '#1090#1080#1087
    end
    object ComboBoxFieldSQLType: TComboBox
      AlignWithMargins = True
      Left = 232
      Top = 30
      Width = 131
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      TextHint = #1058#1080#1087' '#1076#1072#1085#1085#1099#1093' SQL'
    end
    object CheckBoxFieldPK: TCheckBox
      AlignWithMargins = True
      Left = 369
      Top = 3
      Width = 109
      Height = 21
      Caption = #1055#1077#1088#1074#1080#1095#1085#1099#1081' ('#1040#1048')'
      TabOrder = 3
    end
    object CheckBoxFieldNN: TCheckBox
      AlignWithMargins = True
      Left = 369
      Top = 30
      Width = 109
      Height = 21
      Caption = #1053#1077' '#1085#1091#1083#1077#1074#1086#1081
      TabOrder = 4
    end
    object ButtonAdd_Apply: TButton
      AlignWithMargins = True
      Left = 605
      Top = 28
      Width = 75
      Height = 23
      Margins.Top = 2
      Margins.Bottom = 2
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      TabOrder = 5
      OnClick = ButtonAdd_ApplyClick
    end
    object ButtonChange_Cancel: TButton
      AlignWithMargins = True
      Left = 686
      Top = 28
      Width = 75
      Height = 23
      Margins.Top = 2
      Margins.Bottom = 2
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100
      TabOrder = 6
      OnClick = ButtonChange_CancelClick
    end
    object ButtonDelete: TButton
      AlignWithMargins = True
      Left = 767
      Top = 28
      Width = 75
      Height = 23
      Margins.Top = 2
      Margins.Bottom = 2
      Caption = #1059#1076#1072#1083#1080#1090#1100
      TabOrder = 7
      OnClick = ButtonDeleteClick
    end
    object EditFieldComments: TEdit
      AlignWithMargins = True
      Left = 186
      Top = 3
      Width = 177
      Height = 21
      TabOrder = 8
      TextHint = #1054#1087#1080#1089#1072#1085#1080#1077'/'#1050#1086#1084#1084#1077#1085#1090#1072#1088#1080#1081
    end
    object CheckBoxFieldOrd: TCheckBox
      AlignWithMargins = True
      Left = 186
      Top = 30
      Width = 40
      Height = 21
      Caption = 'Ord'
      TabOrder = 9
    end
    object CheckBoxFieldUIDN: TCheckBox
      AlignWithMargins = True
      Left = 484
      Top = 30
      Width = 93
      Height = 21
      Caption = 'UID '#1086#1087#1080#1089#1072#1085#1080#1077
      TabOrder = 10
    end
    object CheckBoxFieldUID: TCheckBox
      AlignWithMargins = True
      Left = 484
      Top = 3
      Width = 93
      Height = 21
      Caption = 'UID'
      TabOrder = 11
    end
  end
  object SynPasSyn2: TSynPasSyn
    Options.AutoDetectEnabled = True
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    IdentifierAttri.Style = [fsBold]
    KeyAttri.Foreground = 8421631
    NumberAttri.Foreground = clHighlight
    DelphiVersion = dvDelphi8
    Left = 824
    Top = 144
  end
  object MainMenu: TMainMenu
    Left = 904
    Top = 144
    object MenuItemFile: TMenuItem
      Caption = #1060#1072#1081#1083
      object MenuItemSave: TMenuItem
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      end
      object MenuItemQuit: TMenuItem
        Caption = #1042#1099#1093#1086#1076
      end
    end
  end
  object FileSaveDialog: TFileSaveDialog
    DefaultExtension = '*.tmc'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = #1060#1072#1081#1083' TMC'
        FileMask = '*.tmc'
      end>
    Options = []
    Left = 648
    Top = 136
  end
  object FileOpenDialog: TFileOpenDialog
    DefaultExtension = '*.tmc'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = #1060#1072#1081#1083' TMC'
        FileMask = '*.tmc'
      end>
    Options = []
    Left = 568
    Top = 136
  end
end
