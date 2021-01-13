unit HGM.SQLite.Wrapper;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$H+}            (* use AnsiString *)
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

uses
  System.SysUtils, Winapi.Windows;

const
  SQLiteDefaultDLL = 'sqlite3.dll';
  //sqlite3ex - библиотека с AES-256 шифрованием. ћожно использовать стандартную, изменив эту константу

//¬озвращаемые значени€ дл€ sqlite3_exec() и sqlite3_step()

const
  SQLITE_OK = 0; // Successful result
  (* beginning-of-error-codes *)
  SQLITE_ERROR = 1; // SQL error or missing database
  SQLITE_INTERNAL = 2; // An internal logic error in SQLite
  SQLITE_PERM = 3; // Access permission denied
  SQLITE_ABORT = 4; // Callback routine requested an abort
  SQLITE_BUSY = 5; // The database file is locked
  SQLITE_LOCKED = 6; // A table in the database is locked
  SQLITE_NOMEM = 7; // A malloc() failed
  SQLITE_READONLY = 8; // Attempt to write a readonly database
  SQLITE_INTERRUPT = 9; // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR = 10; // Some kind of disk I/O error occurred
  SQLITE_CORRUPT = 11; // The database disk image is malformed
  SQLITE_NOTFOUND = 12; // (Internal Only) Table or record not found
  SQLITE_FULL = 13; // Insertion failed because database is full
  SQLITE_CANTOPEN = 14; // Unable to open the database file
  SQLITE_PROTOCOL = 15; // Database lock protocol error
  SQLITE_EMPTY = 16; // Database is empty
  SQLITE_SCHEMA = 17; // The database schema changed
  SQLITE_TOOBIG = 18; // Too much data for one row of a table
  SQLITE_CONSTRAINT = 19; // Abort due to contraint violation
  SQLITE_MISMATCH = 20; // Data type mismatch
  SQLITE_MISUSE = 21; // Library used incorrectly
  SQLITE_NOLFS = 22; // Uses OS features not supported on host
  SQLITE_AUTH = 23; // Authorization denied
  SQLITE_FORMAT = 24; // Auxiliary database format error
  SQLITE_RANGE = 25; // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB = 26; // File opened that is not a database file
  SQLITE_ROW = 100; // sqlite3_step() has another row ready
  SQLITE_DONE = 101; // sqlite3_step() has finished executing

  //DATATYPES
  SQLITE_INTEGER = 1;
  SQLITE_FLOAT = 2;
  SQLITE_TEXT = 3;
  SQLITE_BLOB = 4;
  SQLITE_NULL = 5;
  //
  SQLITE_INTEGER_STR = 'INTEGER';
  SQLITE_BOOLEAN_STR = 'BOOLEAN';
  SQLITE_FLOAT_STR = 'FLOAT';
  SQLITE_DOUBLE_STR = 'DOUBLE';
  SQLITE_NUMERIC_STR = 'NUMERIC';
  SQLITE_REAL_STR = 'REAL';
  SQLITE_TEXT_STR = 'TEXT';
  SQLITE_BLOB_STR = 'BLOB';
  SQLITE_NULL_STR = '';
  //
  SQLITE_UTF8 = 1;
  SQLITE_UTF16 = 2;
  SQLITE_UTF16BE = 3;
  SQLITE_UTF16LE = 4;
  SQLITE_ANY = 5;
  SQLITE_STATIC = Pointer(0);
  SQLITE_TRANSIENT = Pointer(-1);

type
  ESQLiteInitException = class(Exception);

  TSQLiteBackup = Pointer;

  TSQLiteDB = Pointer;

  TSQLiteResult = ^PAnsiChar;

  TSQLiteStmt = Pointer;

  PPAnsiCharArray = ^TPAnsiCharArray;

  TPAnsiCharArray = array[0..(MaxInt div SizeOf(PAnsiChar)) - 1] of PAnsiChar;

type
  TSQLite3Destructor = procedure(Ptr: Pointer); cdecl;

  TSQLite3_Bind_Blob = function(hStmt: TSQLiteStmt; ParamNum: integer; ptrData: Pointer; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer; cdecl;

  TSQLite3_Bind_Double = function(hStmt: TSQLiteStmt; ParamNum: integer; Data: Double): integer; cdecl;

  TSQLite3_Bind_Int = function(hStmt: TSQLiteStmt; ParamNum: integer; Data: integer): integer; cdecl;

  TSQLite3_Bind_Int64 = function(hStmt: TSQLiteStmt; ParamNum: integer; Data: int64): integer; cdecl;

  TSQLite3_Bind_Null = function(hStmt: TSQLiteStmt; ParamNum: integer): integer; cdecl;

  TSQLite3_Bind_Parameter_Index = function(hStmt: TSQLiteStmt; zName: PAnsiChar): integer; cdecl;

  TSQLite3_Bind_Text = function(hStmt: TSQLiteStmt; ParamNum: integer; Text: PAnsiChar; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer; cdecl;

  TSQLite3_Enable_Shared_Cache = function(Value: integer): integer; cdecl;

  TxFunc = procedure(Context: Pointer; Arg: Integer; Args: PPointerArray); cdecl;

  TxStep = procedure(Context: Pointer; Arg: Integer; Args: PPointerArray); cdecl;

  TxFinal = procedure(Context: Pointer); cdecl;

  TSQLiteExecCallback = function(UserData: Pointer; NumCols: integer; ColValues: PPAnsiCharArray; ColNames: PPAnsiCharArray): integer; cdecl;

  TSQLiteBusyHandlerCallback = function(UserData: Pointer; P2: integer): integer; cdecl;

  TCollateCompare = function(UserData: Pointer; Buf1Len: integer; Buf1: Pointer; Buf2Len: integer; Buf2: Pointer): integer; cdecl;

  TSQLite3_Backup_Finish = function(hBackup: TSQLiteBackup): integer; cdecl;

  TSQLite3_Backup_Init = function(DestDB: TSQLiteDB; DestDbName: PAnsiChar; SourceDb: TSQLiteDB; SourceDbName: PAnsiChar): TSQLiteBackup; cdecl;

  TSQLite3_Backup_Pagecount = function(hBackup: TSQLiteBackup): integer; cdecl;

  TSQLite3_Backup_Remaining = function(hBackup: TSQLiteBackup): integer; cdecl;

  TSQLite3_Backup_Step = function(hBackup: TSQLiteBackup; nPage: integer): integer; cdecl;

  TSQLite3_Changes = function(DB: TSQLiteDB): integer; cdecl;

  TSQLite3_Close = function(DB: TSQLiteDB): integer; cdecl;

  TSQLite3_ColumnBlob = function(hStmt: TSQLiteStmt; ColNum: integer): Pointer; cdecl;

  TSQLite3_ColumnBytes = function(hStmt: TSQLiteStmt; ColNum: integer): integer; cdecl;

  TSQLite3_ColumnCount = function(hStmt: TSQLiteStmt): integer; cdecl;

  TSQLite3_ColumnDeclType = function(hStmt: TSQLiteStmt; ColNum: integer): PAnsiChar; cdecl;

  TSQLite3_ColumnDouble = function(hStmt: TSQLiteStmt; ColNum: integer): Double; cdecl;

  TSQLite3_ColumnInt = function(hStmt: TSQLiteStmt; ColNum: integer): integer; cdecl;

  TSQLite3_ColumnInt64 = function(hStmt: TSQLiteStmt; ColNum: integer): Int64; cdecl;

  TSQLite3_ColumnName = function(hStmt: TSQLiteStmt; ColNum: integer): PAnsiChar; cdecl;

  TSQLite3_ColumnText = function(hStmt: TSQLiteStmt; ColNum: integer): PAnsiChar; cdecl;

  TSQLite3_ColumnType = function(hStmt: TSQLiteStmt; ColNum: integer): Integer; cdecl;

  TSQLite3_Complete = function(P: PAnsiChar): Boolean; cdecl;

  TSQLite3_DataCount = function(hStmt: TSQLiteStmt): integer; cdecl;

  TSQLite3_ErrCode = function(DB: TSQLiteDB): integer; cdecl;

  TSQLite3_ErrMsg = function(DB: TSQLiteDB): PAnsiChar; cdecl;

  TSQLite3_Exec = function(DB: TSQLiteDB; SQLStatement: PAnsiChar; CallbackPtr: TSQLiteExecCallback; UserData: Pointer; var ErrMsg: PAnsiChar): integer; cdecl;

  TSQLite3_Finalize = function(hStmt: TSQLiteStmt): integer; cdecl;

  TSQLite3_GetTable = function(DB: TSQLiteDB; SQLStatement: PAnsiChar; var ResultPtr: TSQLiteResult; var RowCount: Cardinal; var ColCount: Cardinal; var ErrMsg: PAnsiChar): integer; cdecl;

  TSQLite3_LastInsertRowID = function(DB: TSQLiteDB): int64; cdecl;

  TSQLite3_Open = function(filename: PAnsiChar; var DB: TSQLiteDB): integer; cdecl;

  TSQLite3_Prepare = function(DB: TSQLiteDB; SQLStatement: PAnsiChar; nBytes: integer; var hStmt: TSQLiteStmt; var pzTail: PAnsiChar): integer; cdecl;

  TSQLite3_Prepare_v2 = function(DB: TSQLiteDB; SQLStatement: PAnsiChar; nBytes: integer; var hStmt: TSQLiteStmt; var pzTail: PAnsiChar): integer; cdecl;

  TSQLite3_Reset = function(hStmt: TSQLiteStmt): integer; cdecl;

  TSQLite3_Step = function(hStmt: TSQLiteStmt): integer; cdecl;

  TSQLite3_TotalChanges = function(DB: TSQLiteDB): integer; cdecl;

  TSQLite3_Version = function(): PAnsiChar; cdecl;

  TSQLite3_BusyHandler = procedure(DB: TSQLiteDB; CallbackPtr: TSQLiteBusyHandlerCallback; UserData: Pointer); cdecl;

  TSQLite3_BusyTimeout = procedure(DB: TSQLiteDB; TimeOut: integer); cdecl;

  TSQlite3_Free = procedure(P: PAnsiChar); cdecl;

  TSQLite3_FreeTable = procedure(Table: TSQLiteResult); cdecl;

  TSQLite3_Interrupt = procedure(DB: TSQLiteDB); cdecl;

  TSQLite3_Create_Function = function(DB: TSQLiteDB; zFunctionName: MarshaledString; nArg, eTextRep: Integer; pApp: Pointer; xFunc: TxFunc; xStep: TxStep; xFinal: TxFinal): Integer; cdecl;

  TSQLite3_Value_Text = function(Value: Pointer): MarshaledAString; cdecl;

  TSQLite3_Result_Text = procedure(Context: Pointer; Data: MarshaledAString; Size: Integer; Proc: TSQLite3Destructor); cdecl;

  TSQLite3_Result_Int = procedure(Context: Pointer; Data: Integer); cdecl;

  TSQLite3_Create_Collation = function(DB: TSQLiteDB; Name: PAnsiChar; eTextRep: integer; UserData: Pointer; xCompare: TCollateCompare): integer; cdecl;

//sqlite3_create_function(Db, 'UPPER', 1, SQLITE_ANY, nil, @SQLiteUpper, nil, nil);

////
//¬ запросах SQL, вводимых в sqlite3_prepare() и sqlite3_prepare_v2(),
//один или несколько литералов могут быть заменены шаблоном "?" »ли ":N:" где N - целое число.
//Ёти значени€ быть установлены с помощью методов sqlite3_bind_*.
//
//¬ каждом случае первым параметром €вл€етс€ указатель на структуру sqlite3_stmt, возвращенную из sqlite3_prepare().
//¬торой параметр - это индекс подстановочного знака.
//ѕервый "?" »меет индекс 1. ":N:" подстановочные знаки используют индекс N.
////
//ѕ€тый параметр дл€ sqlite3_bind_blob(), sqlite3_bind_text() €вл€етс€ деструктором,
//используемым дл€ удалени€ BLOB или text после завершени€ работы SQLite с ним.
//≈сли п€тый аргумент представл€ет собой специальное значение SQLITE_STATIC,
//библиотека предполагает, что информаци€ находитс€ в статическом,
//неуправл€емом пространстве и не нуждаетс€ в освобождении.
//≈сли п€тый аргумент имеет значение SQLITE_TRANSIENT, тогда SQLite создает
//свою собственную личную копию данных.
////
//ћетоды sqlite3_bind_* должны быть вызваны перед sqlite3_step(),
//после sqlite3_prepare() или sqlite3_reset().
//Ќесопоставленные подстановочные знаки интерпретируютс€ как NULL.
////

var
  SQLiteDLLInstance: THandle;
  SQLite3_Backup_Finish: TSQLite3_Backup_Finish;
  SQLite3_Backup_Init: TSQLite3_Backup_Init;
  SQLite3_Backup_Pagecount: TSQLite3_Backup_Pagecount;
  SQLite3_Backup_Remaining: TSQLite3_Backup_Remaining;
  SQLite3_Backup_Step: TSQLite3_Backup_Step;
  //
  SQLite3_BusyHandler: TSQLite3_BusyHandler;
  SQLite3_BusyTimeout: TSQLite3_BusyTimeout;
  SQLite3_Changes: TSQLite3_Changes;
  SQLite3_Close: TSQLite3_Close;
  //
  SQLite3_ColumnBlob: TSQLite3_ColumnBlob;
  SQLite3_ColumnBytes: TSQLite3_ColumnBytes;
  SQLite3_ColumnCount: TSQLite3_ColumnCount;
  SQLite3_ColumnDeclType: TSQLite3_ColumnDeclType;
  SQLite3_ColumnDouble: TSQLite3_ColumnDouble;
  SQLite3_ColumnInt64: TSQLite3_ColumnInt64;
  SQLite3_ColumnInt: TSQLite3_ColumnInt;
  SQLite3_ColumnName: TSQLite3_ColumnName;
  SQLite3_ColumnText: TSQLite3_ColumnText;
  SQLite3_ColumnType: TSQLite3_ColumnType;
  //
  SQLite3_Complete: TSQLite3_Complete;
  SQLite3_DataCount: TSQLite3_DataCount;
  SQLite3_ErrCode: TSQLite3_ErrCode;
  SQLite3_ErrMsg: TSQLite3_ErrMsg;
  SQLite3_Exec: TSQLite3_Exec;
  SQLite3_Finalize: TSQLite3_Finalize;
  SQlite3_Free: TSQlite3_Free;
  SQLite3_FreeTable: TSQLite3_FreeTable;
  SQLite3_GetTable: TSQLite3_GetTable;
  SQLite3_Interrupt: TSQLite3_Interrupt;
  SQLite3_LastInsertRowID: TSQLite3_LastInsertRowID;
  SQLite3_Open: TSQLite3_Open;
  SQLite3_Prepare: TSQLite3_Prepare;
  SQLite3_Prepare_v2: TSQLite3_Prepare_v2;
  SQLite3_Reset: TSQLite3_Reset;
  SQLite3_Step: TSQLite3_Step;
  SQLite3_TotalChanges: TSQLite3_TotalChanges;
  SQLite3_Version: TSQLite3_Version;
  //
  SQLite3_Bind_Blob: TSQLite3_Bind_Blob;
  SQLite3_Bind_Double: TSQLite3_Bind_Double;
  SQLite3_Bind_Int64: TSQLite3_Bind_Int64;
  SQLite3_Bind_Int: TSQLite3_Bind_Int;
  SQLite3_Bind_Null: TSQLite3_Bind_Null;
  SQLite3_Bind_Parameter_Index: TSQLite3_Bind_Parameter_index;
  SQLite3_Bind_Text: TSQLite3_Bind_Text;
  //
  SQLite3_Create_Collation: TSQLite3_Create_Collation;
  SQLite3_Enable_Shared_Cache: TSQLite3_Enable_Shared_Cache;
  SQLite3_Create_Function: TSQLite3_Create_Function;
  SQLite3_Value_Text: TSQLite3_Value_Text;
  SQLite3_Result_Text: TSQLite3_Result_Text;
  SQLite3_Result_Int: TSQLite3_Result_Int;

procedure InitSQL(SQLiteLib: string = '');

procedure UnInitSQL;

implementation

procedure UnInitSQL;
begin
  FreeLibrary(SQLiteDLLInstance);
end;

procedure InitSQL(SQLiteLib: string);
begin
  if SQLiteLib.IsEmpty then
    SQLiteLib := SQLiteDefaultDLL;

  SQLiteDLLInstance := LoadLibrary(PWideChar(SQLiteLib));
  if SQLiteDLLInstance <> 0 then
  begin
    @SQLite3_Open := GetProcAddress(SQLiteDLLInstance, 'sqlite3_open');
    @SQLite3_Close := GetProcAddress(SQLiteDLLInstance, 'sqlite3_close');
    @SQLite3_Exec := GetProcAddress(SQLiteDLLInstance, 'sqlite3_exec');
    @SQLite3_Version := GetProcAddress(SQLiteDLLInstance, 'sqlite3_libversion');
    @SQLite3_ErrMsg := GetProcAddress(SQLiteDLLInstance, 'sqlite3_errmsg');
    @SQLite3_ErrCode := GetProcAddress(SQLiteDLLInstance, 'sqlite3_errcode');
    @SQLite3_LastInsertRowID := GetProcAddress(SQLiteDLLInstance, 'sqlite3_last_insert_rowid');
    @SQLite3_Complete := GetProcAddress(SQLiteDLLInstance, 'sqlite3_complete');
    @SQLite3_GetTable := GetProcAddress(SQLiteDLLInstance, 'sqlite3_get_table');
    @SQLite3_Changes := GetProcAddress(SQLiteDLLInstance, 'sqlite3_changes');
    @SQLite3_TotalChanges := GetProcAddress(SQLiteDLLInstance, 'sqlite3_total_changes');
    @SQLite3_Prepare := GetProcAddress(SQLiteDLLInstance, 'sqlite3_prepare');
    @SQLite3_Prepare_v2 := GetProcAddress(SQLiteDLLInstance, 'sqlite3_prepare_v2');
    @SQLite3_ColumnCount := GetProcAddress(SQLiteDLLInstance, 'sqlite3_column_count');
    @SQLite3_ColumnName := GetProcAddress(SQLiteDLLInstance, 'sqlite3_column_name');
    @SQLite3_ColumnDeclType := GetProcAddress(SQLiteDLLInstance, 'sqlite3_column_decltype');
    @SQLite3_Step := GetProcAddress(SQLiteDLLInstance, 'sqlite3_step');
    @SQLite3_DataCount := GetProcAddress(SQLiteDLLInstance, 'sqlite3_data_count');
    @SQlite3_Free := GetProcAddress(SQLiteDLLInstance, 'sqlite3_free');
    @SQLite3_FreeTable := GetProcAddress(SQLiteDLLInstance, 'sqlite3_free_table');
    @SQLite3_Interrupt := GetProcAddress(SQLiteDLLInstance, 'sqlite3_interrupt');
    @SQLite3_BusyHandler := GetProcAddress(SQLiteDLLInstance, 'sqlite3_busy_handler');
    @SQLite3_BusyTimeout := GetProcAddress(SQLiteDLLInstance, 'sqlite3_busy_timeout');
    @SQLite3_ColumnBlob := GetProcAddress(SQLiteDLLInstance, 'sqlite3_column_blob');
    @SQLite3_ColumnBytes := GetProcAddress(SQLiteDLLInstance, 'sqlite3_column_bytes');
    @SQLite3_ColumnDouble := GetProcAddress(SQLiteDLLInstance, 'sqlite3_column_double');
    @SQLite3_ColumnInt := GetProcAddress(SQLiteDLLInstance, 'sqlite3_column_int');
    @SQLite3_ColumnText := GetProcAddress(SQLiteDLLInstance, 'sqlite3_column_text');
    @SQLite3_ColumnType := GetProcAddress(SQLiteDLLInstance, 'sqlite3_column_type');
    @SQLite3_ColumnInt64 := GetProcAddress(SQLiteDLLInstance, 'sqlite3_column_int64');
    @SQLite3_Finalize := GetProcAddress(SQLiteDLLInstance, 'sqlite3_finalize');
    @SQLite3_Reset := GetProcAddress(SQLiteDLLInstance, 'sqlite3_reset');
    @SQLite3_Backup_Init := GetProcAddress(SQLiteDLLInstance, 'sqlite3_backup_init');
    @SQLite3_Backup_Step := GetProcAddress(SQLiteDLLInstance, 'sqlite3_backup_step');
    @SQLite3_Backup_Finish := GetProcAddress(SQLiteDLLInstance, 'sqlite3_backup_finish');
    @SQLite3_Backup_Remaining := GetProcAddress(SQLiteDLLInstance, 'sqlite3_backup_remaining');
    @SQLite3_Backup_Pagecount := GetProcAddress(SQLiteDLLInstance, 'sqlite3_backup_pagecount');
    @SQLite3_Bind_Blob := GetProcAddress(SQLiteDLLInstance, 'sqlite3_bind_blob');
    @SQLite3_Bind_Text := GetProcAddress(SQLiteDLLInstance, 'sqlite3_bind_text');
    @SQLite3_Bind_Double := GetProcAddress(SQLiteDLLInstance, 'sqlite3_bind_double');
    @SQLite3_Bind_Int := GetProcAddress(SQLiteDLLInstance, 'sqlite3_bind_int');
    @SQLite3_Bind_Int64 := GetProcAddress(SQLiteDLLInstance, 'sqlite3_bind_int64');
    @SQLite3_Bind_Null := GetProcAddress(SQLiteDLLInstance, 'sqlite3_bind_null');
    @SQLite3_Bind_Parameter_index := GetProcAddress(SQLiteDLLInstance, 'sqlite3_bind_parameter_index');
    @SQLite3_Enable_Shared_Cache := GetProcAddress(SQLiteDLLInstance, 'sqlite3_enable_shared_cache');
    @SQLite3_Create_Collation := GetProcAddress(SQLiteDLLInstance, 'sqlite3_create_collation');
    @SQLite3_Create_Function := GetProcAddress(SQLiteDLLInstance, 'sqlite3_create_function16');
    @SQLite3_Value_Text := GetProcAddress(SQLiteDLLInstance, 'sqlite3_value_text');
    @SQLite3_Result_Text := GetProcAddress(SQLiteDLLInstance, 'sqlite3_result_text');
    @SQLite3_Result_Int := GetProcAddress(SQLiteDLLInstance, 'sqlite3_result_int');
  end
  else
    raise ESQLiteInitException.Create('Database library ' + SQLiteLib + ' not found');
end;

end.

