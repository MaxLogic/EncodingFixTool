unit EncodingFixToolCore;

{
  Delphi *.pas encoding fix tool
  --------------------------------
  - Gathers files by path / recursive / extensions
  - Parallel processing (TParallel.For)
  - For each file:
      * Try strict UTF-8 decode; if OK => (verbose: "OK"), skip save
      * Else fix per line (mixed encodings):
          - Try UTF-8 (strict), CP1250 (Polish/Central Europe), CP1252 (Western/German)
          - Score decodes and pick the best for each line
      * Save as UTF-8 with configurable BOM (default: y)
      * Optionally create a backup in bkp-dir preserving relative paths
  - Modes:
      dry, silent, verbose, help
  - Summary: time elapsed, files changed

  Notes:
  * Default UTF-8 BOM = yes (Delphi IDE/compiler behave best with BOM).
}

interface

uses
  System.Character, System.Classes, System.Diagnostics, System.Generics.Collections, System.IOUtils, System.StrUtils,
  System.SyncObjs, System.SysUtils, System.Threading;

type
  TEncodingFixTool = class
  public type
      TEndOfLineMode = (elmPreserve, elmCrlf);
      TOutputFormat = (ofText, ofJson);
      TScopeMode = (smAll, smGitChanged);
      TOptions = record
        DryRun: boolean;
        Silent: boolean;
        Verbose: boolean; // not compatible with Silent
        Path: string;
        Recursive: boolean; // default y
        Exts: TArray<string>; // normalized: ".pas", ".dpr", etc.
        Utf8Bom: boolean; // default y
        EolMode: TEndOfLineMode; // default preserve
        OutputFormat: TOutputFormat; // default text
        ScopeMode: TScopeMode; // default all files
        BackupDir: string; // if <> '', create backups preserving relative path
      end;
  private
    type
      TSafeConsole = class
      strict private
        class var FCrit: TCriticalSection;
      public
        class constructor Create;
        class destructor Destroy;
        class procedure WriteLine(const aMsg: string);
      end;

  private
    fWantedExts: TStringList;
    fEnc1250: TEncoding;
    fEnc1252: TEncoding;
    function ParseCommandLine(out aOptions: TOptions): integer;
    function ShowHelp: integer;
    function NormalizeExtList(const aCSV: string): TArray<string>;
    function CollectFiles(const aOptions: TOptions): TArray<string>;
    function CollectGitChangedFiles(const aOptions: TOptions): TArray<string>;
    function IsWantedExt(const aFile: string): boolean;
    function QuoteShellArg(const aValue: string): string;
    function RunCommand(const aCommandLine, aWorkingDirectory: string): integer;

    // Encoding helpers
    function HasUtf8Bom(const aBytes: TBytes): boolean;
    function HasUtf16BEBom(const aBytes: TBytes): boolean;
    function HasUtf16LEBom(const aBytes: TBytes): boolean;
    function IsUtf8Strict(const aBytes: TBytes): boolean;
    function GetWithoutUtf8Bom(const aBytes: TBytes): TBytes;
    function SplitLinesByBytes(const aBytes: TBytes): TArray<TBytes>;
    function IsAsciiBytes(const aBytes: TBytes): boolean;
    function IsBinaryDfmFile(const aFile: string; const aBytes: TBytes): boolean;
    function NormalizeTextEolToCrlf(const aText: string; out aChanged: boolean): string;
    function DecodeBestPerLine(const aLineBytes: TBytes; out aEncName: string): string;
    function ScoreDecoded(const aText: string): integer;
    function ContainsSpecials(const aText: string): integer;

    function FixFile(const aFile: string; const aOptions: TOptions; out aChanged: boolean; out aReason: string): boolean;
    function SaveTextUTF8(const aFile: string; const aText: string; aWithBOM: boolean; out aError: string): boolean;
    function MakeBackupPath(const aOptions: TOptions; const aRootPath, aFile: string): string;
    function MakeRelativeTo(const aRootPath, aFile: string): string;
    procedure PrepareExtIndex(const aExts: TArray<string>);

  public
    constructor Create;
    destructor Destroy; override;
    class function RunFromCommandLine: integer; static;
  end;

implementation

uses
  System.WideStrUtils,
  Winapi.Windows,
  AutoFree;

const
  cUtf8Bom0 = Byte($EF);
  cUtf8Bom1 = Byte($BB);
  cUtf8Bom2 = Byte($BF);
  cUtf16LEBom0 = Byte($FF);
  cUtf16LEBom1 = Byte($FE);
  cUtf16BEBom0 = Byte($FE);
  cUtf16BEBom1 = Byte($FF);

{ =====================  Utilities  ===================== }

class constructor TEncodingFixTool.TSafeConsole.Create;
begin
  FCrit := TCriticalSection.Create;
end;

class destructor TEncodingFixTool.TSafeConsole.Destroy;
begin
  FCrit.Free;
end;

class procedure TEncodingFixTool.TSafeConsole.WriteLine(const aMsg: string);
begin
  FCrit.Acquire;
  try
    writeln(aMsg);
  finally
    FCrit.release;
  end;
end;

{ =====================  TEncodingFixTool lifecycle  ===================== }

constructor TEncodingFixTool.Create;
begin
  inherited Create;
  try
    fWantedExts := TStringList.Create;
    fWantedExts.Sorted := True;
    fWantedExts.Duplicates := dupIgnore;
    fWantedExts.CaseSensitive := False;
    fEnc1250 := TEncoding.GetEncoding(1250);
    fEnc1252 := TEncoding.GetEncoding(1252);
  except
    fEnc1252.Free;
    fEnc1252 := nil;
    fEnc1250.Free;
    fEnc1250 := nil;
    fWantedExts.Free;
    fWantedExts := nil;
    raise;
  end;
end;

destructor TEncodingFixTool.Destroy;
begin
  fWantedExts.Free;
  fEnc1252.Free;
  fEnc1250.Free;
  inherited;
end;

procedure TEncodingFixTool.PrepareExtIndex(const aExts: TArray<string>);
var
  s: string;
begin
  fWantedExts.BeginUpdate;
  try
    fWantedExts.Clear;
    for s in aExts do
    begin
      if s <> '' then
      begin
        fWantedExts.Add(LowerCase(s));
      end;
    end;
  finally
    fWantedExts.EndUpdate;
  end;
end;

function TEncodingFixTool.NormalizeExtList(const aCSV: string): TArray<string>;
var
  lParts: TArray<string>;
  i: integer;
  s: string;
begin
  lParts := aCSV.Split([',', ';', ' '], TStringSplitOptions.ExcludeEmpty);
  SetLength(Result, length(lParts));
  for i := 0 to High(lParts) do
  begin
    s := lParts[i].ToLower
      .Trim([' ', '"', '''']);
    if s.StartsWith('*.') then
    begin
      s := s.Substring(1); // "*.pas" -> ".pas"
    end else if (s <> '') and (s[1] <> '.') then
    begin
      s := '.' + s; // "pas" -> ".pas"
    end;
    Result[i] := s;
  end;
end;

function TEncodingFixTool.CollectFiles(const aOptions: TOptions): TArray<string>;
var
  g: TGarbos;
  lFiles: TList<string>;
  lSearchOpt: TSearchOption;
  lExt, lPattern: string;
begin
  if aOptions.ScopeMode = TScopeMode.smGitChanged then
  begin
    Exit(CollectGitChangedFiles(aOptions));
  end;

  GC(lFiles, TList<string>.Create, g);

  if aOptions.Recursive then
    lSearchOpt := TSearchOption.soAllDirectories
  else
    lSearchOpt := TSearchOption.soTopDirectoryOnly;

  for lExt in fWantedExts do
  begin
    lPattern := '*' + lExt; // e.g. ".pas" -> "*.pas"
    lFiles.AddRange(TDirectory.GetFiles(aOptions.Path, lPattern, lSearchOpt));
  end;

  Result := lFiles.ToArray;
end;

function TEncodingFixTool.CollectGitChangedFiles(const aOptions: TOptions): TArray<string>;
var
  g: TGarbos;
  lCommand: string;
  lExitCode: integer;
  lFile: string;
  lFiles: TList<string>;
  lLine: string;
  lOutputFileName: string;
  lPath: string;
  lRootPath: string;
  lScanRootPath: string;
  lSepPos: integer;
  lStatus: string;
  lWorkTreeRootFileName: string;
begin
  GC(lFiles, TList<string>.Create, g);
  lOutputFileName := TPath.Combine(TPath.GetTempPath, 'EncodingFixTool-git-' + TGuid.NewGuid.ToString + '.txt');
  lWorkTreeRootFileName := TPath.Combine(TPath.GetTempPath, 'EncodingFixTool-git-root-' + TGuid.NewGuid.ToString + '.txt');
  try
    lCommand := 'cmd.exe /C git -C ' + QuoteShellArg(aOptions.Path) + ' rev-parse --show-toplevel > ' +
      QuoteShellArg(lWorkTreeRootFileName);
    lExitCode := RunCommand(lCommand, aOptions.Path);
    if lExitCode <> 0 then
    begin
      raise Exception.Create('git rev-parse failed');
    end;
    lRootPath := IncludeTrailingPathDelimiter(TFile.ReadAllText(lWorkTreeRootFileName, TEncoding.UTF8).Trim);
    lScanRootPath := IncludeTrailingPathDelimiter(ExpandFileName(aOptions.Path));

    lCommand := 'cmd.exe /C git -C ' + QuoteShellArg(aOptions.Path) +
      ' status --porcelain --untracked-files=all > ' + QuoteShellArg(lOutputFileName);
    lExitCode := RunCommand(lCommand, aOptions.Path);
    if lExitCode <> 0 then
    begin
      raise Exception.Create('git status failed');
    end;

    for lLine in TFile.ReadAllLines(lOutputFileName, TEncoding.UTF8) do
    begin
      if Length(lLine) < 4 then
      begin
        Continue;
      end;

      lStatus := Copy(lLine, 1, 2);
      if (lStatus = ' D') or (lStatus = 'D ') or (lStatus = 'DD') then
      begin
        Continue;
      end;

      lPath := Copy(lLine, 4, MaxInt).Trim([' ', '"']);
      lSepPos := Pos(' -> ', lPath);
      if lSepPos > 0 then
      begin
        lPath := Copy(lPath, lSepPos + 4, MaxInt).Trim([' ', '"']);
      end;
      lPath := StringReplace(lPath, '/', TPath.DirectorySeparatorChar, [rfReplaceAll]);
      lFile := TPath.GetFullPath(TPath.Combine(lRootPath, lPath));

      if TFile.Exists(lFile) and StartsText(lScanRootPath, lFile) and IsWantedExt(lFile) then
      begin
        lFiles.Add(lFile);
      end;
    end;
  finally
    if TFile.Exists(lOutputFileName) then
    begin
      TFile.Delete(lOutputFileName);
    end;
    if TFile.Exists(lWorkTreeRootFileName) then
    begin
      TFile.Delete(lWorkTreeRootFileName);
    end;
  end;

  Result := lFiles.ToArray;
end;

function TEncodingFixTool.IsWantedExt(const aFile: string): boolean;
begin
  Result := fWantedExts.IndexOf(LowerCase(ExtractFileExt(aFile))) >= 0;
end;

function TEncodingFixTool.QuoteShellArg(const aValue: string): string;
begin
  Result := '"' + StringReplace(aValue, '"', '\"', [rfReplaceAll]) + '"';
end;

function TEncodingFixTool.RunCommand(const aCommandLine, aWorkingDirectory: string): integer;
var
  lExitCode: DWORD;
  lCommandLineChars: TArray<Char>;
  lProcessInformation: TProcessInformation;
  lStartupInfo: TStartupInfo;
  lWaitResult: Cardinal;
begin
  lCommandLineChars := aCommandLine.ToCharArray;
  SetLength(lCommandLineChars, Length(lCommandLineChars) + 1);
  ZeroMemory(@lProcessInformation, SizeOf(lProcessInformation));
  ZeroMemory(@lStartupInfo, SizeOf(lStartupInfo));
  lStartupInfo.cb := SizeOf(lStartupInfo);

  if not CreateProcess(nil, PChar(lCommandLineChars), nil, nil, False, CREATE_NO_WINDOW, nil, PChar(aWorkingDirectory),
    lStartupInfo, lProcessInformation) then
  begin
    RaiseLastOSError;
  end;
  try
    lWaitResult := WaitForSingleObject(lProcessInformation.hProcess, 30000);
    if lWaitResult <> WAIT_OBJECT_0 then
    begin
      TerminateProcess(lProcessInformation.hProcess, 1);
      raise Exception.Create('process timed out');
    end;
    if not GetExitCodeProcess(lProcessInformation.hProcess, lExitCode) then
    begin
      RaiseLastOSError;
    end;
    Result := Integer(lExitCode);
  finally
    CloseHandle(lProcessInformation.hThread);
    CloseHandle(lProcessInformation.hProcess);
  end;
end;

function TEncodingFixTool.IsUtf8Strict(const aBytes: TBytes): boolean;
var
  lBytesNoBom: TBytes;
begin
  lBytesNoBom := GetWithoutUtf8Bom(aBytes);
  if length(lBytesNoBom) = 0 then
    exit(True);

  Result := TEncoding.Utf8.IsBufferValid(lBytesNoBom);
end;

function TEncodingFixTool.HasUtf8Bom(const aBytes: TBytes): boolean;
begin
  Result := (length(aBytes) >= 3) and (aBytes[0] = cUtf8Bom0) and (aBytes[1] = cUtf8Bom1) and
    (aBytes[2] = cUtf8Bom2);
end;

function TEncodingFixTool.HasUtf16BEBom(const aBytes: TBytes): boolean;
begin
  Result := (length(aBytes) >= 2) and (aBytes[0] = cUtf16BEBom0) and (aBytes[1] = cUtf16BEBom1);
end;

function TEncodingFixTool.HasUtf16LEBom(const aBytes: TBytes): boolean;
begin
  Result := (length(aBytes) >= 2) and (aBytes[0] = cUtf16LEBom0) and (aBytes[1] = cUtf16LEBom1);
end;

function TEncodingFixTool.SplitLinesByBytes(const aBytes: TBytes): TArray<TBytes>;
var
  g: TGarbos;
  i, lStart: integer;
  b: BYTE;
  lLine: TBytes;
  lList: TList<TBytes>;
begin
  GC(lList, TList<TBytes>.Create, g);

  lStart := 0;
  i := 0;
  while i < length(aBytes) do
  begin
    b := aBytes[i];
    if b = $0A then
    begin
      // LF: line ends before this char
      SetLength(lLine, i - lStart);
      if length(lLine) > 0 then
        move(aBytes[lStart], lLine[0], length(lLine));
      lList.Add(lLine);
      Inc(i);
      lStart := i;
    end else if b = $0D then
    begin
      // CR: line ends here; handle CRLF or standalone CR
      SetLength(lLine, i - lStart);
      if length(lLine) > 0 then
        move(aBytes[lStart], lLine[0], length(lLine));
      lList.Add(lLine);
      Inc(i);
      if (i < length(aBytes)) and (aBytes[i] = $0A) then
      begin
        Inc(i); // consume LF
      end;
      lStart := i;
    end else begin
      Inc(i);
    end;
  end;

  // last line (no trailing EOL)
  if lStart <= length(aBytes) - 1 then
  begin
    SetLength(lLine, length(aBytes) - lStart);
    if length(lLine) > 0 then
      move(aBytes[lStart], lLine[0], length(lLine));
    lList.Add(lLine);
  end;

  Result := lList.ToArray;
end;

function TEncodingFixTool.ContainsSpecials(const aText: string): integer;
const
  // Polish + German diacritics; count good matches positively.
  POL = 'ąćęłńóśźżĄĆĘŁŃÓŚŹŻ';
  GER = 'äöüÄÖÜßẞ';
var
  lScore: integer;
  ch: char;
begin
  lScore := 0;
  for ch in aText do
  begin
    if (pos(ch, POL) > 0) or (pos(ch, GER) > 0) then
    begin
      Inc(lScore);
    end;
  end;
  Result := lScore;
end;

function TEncodingFixTool.ScoreDecoded(const aText: string): integer;
var
  lScore: integer;
  ch: char;
begin
  // Base heuristic:
  // +2 for known PL/DE diacritics,
  // +1 for typical source-text ASCII range,
  // -2 for control chars (excluding tab),
  // -1 for � replacement if it appears (just in case)
  lScore := 0;

  Inc(lScore, 2 * ContainsSpecials(aText));

  for ch in aText do
  begin
    // U+FFFD REPLACEMENT CHARACTER: indicates decoding replacement for invalid/unknown bytes; penalize it.
    if ch = #$FFFD then
    begin
      Dec(lScore, 1);
    end else if (ch = #9) or (ch = #10) or (ch = #13) then
    begin
      // ignore tabs/newlines here
    end else if ch.IsControl then
    begin
      Dec(lScore, 2);
    end else if ch.IsLetterOrDigit or ch.IsWhiteSpace or CharInSet(ch, ['.', ',', ';', ':', '-', '_', '(', ')', '[', ']', '{', '}', '''', '"', '/', '\', '+', '*', '=', '<', '>', '!', '?', '@', '#', '$', '%', '^', '&', '|']) then
    begin
      Inc(lScore, 1);
    end;
  end;

  Result := lScore;
end;

function TEncodingFixTool.IsAsciiBytes(const aBytes: TBytes): boolean;
var
  b: Byte;
begin
  for b in aBytes do
  begin
    if b >= $80 then
      Exit(False);
  end;
  Exit(True);
end;

function TEncodingFixTool.IsBinaryDfmFile(const aFile: string; const aBytes: TBytes): boolean;
begin
  Result := SameText(ExtractFileExt(aFile), '.dfm') and (Length(aBytes) >= 4) and
    (aBytes[0] = Ord('T')) and (aBytes[1] = Ord('P')) and (aBytes[2] = Ord('F')) and (aBytes[3] = Ord('0'));
end;

function TEncodingFixTool.NormalizeTextEolToCrlf(const aText: string; out aChanged: boolean): string;
var
  g: TGarbos;
  i: integer;
  c: Char;
  lBuilder: TStringBuilder;
begin
  aChanged := False;
  GC(lBuilder, TStringBuilder.Create(Length(aText) + 16), g);

  i := 1;
  while i <= Length(aText) do
  begin
    c := aText[i];
    if c = #13 then
    begin
      lBuilder.Append(#13#10);
      if (i < Length(aText)) and (aText[i + 1] = #10) then
      begin
        Inc(i, 2);
      end else begin
        aChanged := True;
        Inc(i);
      end;
    end else if c = #10 then
    begin
      lBuilder.Append(#13#10);
      aChanged := True;
      Inc(i);
    end else begin
      lBuilder.Append(c);
      Inc(i);
    end;
  end;

  if aChanged then
    Result := lBuilder.ToString
  else
    Result := aText;
end;

function TEncodingFixTool.DecodeBestPerLine(const aLineBytes: TBytes; out aEncName: string): string;
var
  lANSI: TEncoding;
  s1250, s1252, sANSI: string;
  bestS: string;
  bestScore, sc: integer;
begin
  // 0) Pure ASCII? Treat as ASCII explicitly (safe UTF-8 subset)
  if IsAsciiBytes(aLineBytes) then
  begin
    aEncName := 'ASCII';
    Result := TEncoding.Utf8.GetString(aLineBytes);
    exit;
  end;

  // 1) Try strict UTF-8 first
  if IsUtf8Strict(aLineBytes) then
  begin
    aEncName := 'UTF-8';
    Result := TEncoding.Utf8.GetString(aLineBytes);
    exit;
  end;

  // 2) Try single-byte candidates; they never fail, so score them.
  lANSI := TEncoding.ANSI;

  s1250 := fEnc1250.GetString(aLineBytes);
  s1252 := fEnc1252.GetString(aLineBytes);
  sANSI := lANSI.GetString(aLineBytes);

  bestS := s1250;
  aEncName := 'Windows-1250';
  bestScore := ScoreDecoded(s1250);

  sc := ScoreDecoded(s1252);
  if sc > bestScore then
  begin
    bestScore := sc;
    bestS := s1252;
    aEncName := 'Windows-1252';
  end;

  sc := ScoreDecoded(sANSI);
  if sc > bestScore then
  begin
    bestScore := sc;
    bestS := sANSI;
    aEncName := 'ANSI';
  end;

  Result := bestS;
end;

function TEncodingFixTool.MakeBackupPath(const aOptions: TOptions; const aRootPath, aFile: string): string;
var
  lRel: string;
begin
  // Normalize root and compute relative path
  // Ensure trailing delimiter on root
  lRel := aFile;
  if aRootPath <> '' then
  begin
    // Make relative to root path if possible
    // We compare case-insensitively on Windows
    if SameText(copy(aFile, 1, length(IncludeTrailingPathDelimiter(aRootPath))), IncludeTrailingPathDelimiter(aRootPath)) then
    begin
      lRel := copy(aFile, length(IncludeTrailingPathDelimiter(aRootPath)) + 1, MaxInt);
    end;
  end;

  Result := TPath.Combine(IncludeTrailingPathDelimiter(aOptions.BackupDir), lRel);
end;

function TEncodingFixTool.MakeRelativeTo(const aRootPath, aFile: string): string;
var
  sRoot: string;
begin
  Result := aFile;
  if aRootPath <> '' then
  begin
    sRoot := IncludeTrailingPathDelimiter(aRootPath);
    if SameText(copy(aFile, 1, length(sRoot)), sRoot) then
      Result := copy(aFile, length(sRoot) + 1, MaxInt);
  end;
end;

function TEncodingFixTool.SaveTextUTF8(const aFile: string; const aText: string; aWithBOM: boolean; out aError: string): boolean;
var
  lContentBytes: TBytes;
  lBytes: TBytes;
  lTempFile: string;
begin
  Result := False;
  aError := '';
  lTempFile := '';
  try
    try
      lContentBytes := TEncoding.UTF8.GetBytes(aText);
      if aWithBOM then
      begin
        SetLength(lBytes, length(lContentBytes) + 3);
        lBytes[0] := cUtf8Bom0;
        lBytes[1] := cUtf8Bom1;
        lBytes[2] := cUtf8Bom2;
        if length(lContentBytes) > 0 then
        begin
          Move(lContentBytes[0], lBytes[3], length(lContentBytes));
        end;
      end else begin
        lBytes := lContentBytes;
      end;

      lTempFile := TPath.Combine(ExtractFileDir(aFile), TPath.GetRandomFileName);
      TFile.WriteAllBytes(lTempFile, lBytes);
      TFile.Copy(lTempFile, aFile, True);
      Result := True;
    except
      on e: Exception do
      begin
        aError := e.ClassName + ': ' + e.Message;
      end;
    end;
  finally
    if (lTempFile <> '') and TFile.Exists(lTempFile) then
    begin
      TFile.Delete(lTempFile);
    end;
  end;
end;

function TEncodingFixTool.FixFile(const aFile: string; const aOptions: TOptions; out aChanged: boolean; out aReason: string): boolean;
var
  lBytes: TBytes;
  lBytesNoBom: TBytes;
  lLinesBytes: TArray<TBytes>;
  lLine: TBytes;
  lFixedLines: TStringBuilder;
  lFirst: boolean;
  lRoot: string;
  lBackupPath: string;
  lHasBOM: boolean;
  lText: string;
  lCRLF, lLF, lCR: integer;
  i: integer;
  lEOL: string;
  lHadTrailingEOL: boolean;
  rb: RawByteString;
  lEncType: TEncodeType;
  lCntUtf8, lCnt1250, lCnt1252, lCntAnsi, lCntAscii: Integer;
  lEncName: string;
  lReasonEnc: string;
  lEolChanged: boolean;
  lKinds: Integer;
  lSaveError: string;
  g: TGarbos;
begin
  aChanged := False;
  aReason := '';

  lBytes := TFile.ReadAllBytes(aFile);

  if IsBinaryDfmFile(aFile, lBytes) then
  begin
    aChanged := False;
    aReason := 'Skipped binary DFM';
    Result := True;
    exit;
  end;

  if HasUtf16LEBom(lBytes) or HasUtf16BEBom(lBytes) then
  begin
    if HasUtf16LEBom(lBytes) then
    begin
      lText := TEncoding.Unicode.GetString(copy(lBytes, 2, length(lBytes) - 2));
      lReasonEnc := 'detected UTF-16 LE';
    end else begin
      lText := TEncoding.BigEndianUnicode.GetString(copy(lBytes, 2, length(lBytes) - 2));
      lReasonEnc := 'detected UTF-16 BE';
    end;

    if aOptions.DryRun then
    begin
      aChanged := True;
      aReason := Format('%s; would save UTF-8 (BOM=%s) (dry-run)',
        [lReasonEnc, IfThen(aOptions.Utf8Bom, 'Y', 'N')]);
      Result := True;
      exit;
    end;

    if aOptions.BackupDir <> '' then
    begin
      lRoot := IncludeTrailingPathDelimiter(ExpandFileName(aOptions.Path));
      lBackupPath := MakeBackupPath(aOptions, lRoot, aFile);
      TDirectory.CreateDirectory(ExtractFileDir(lBackupPath));
      TFile.copy(aFile, lBackupPath, True);
    end;

    if SaveTextUTF8(aFile, lText, aOptions.Utf8Bom, lSaveError) then
    begin
      aChanged := True;
      aReason := Format('%s; saved UTF-8 (BOM=%s)', [lReasonEnc, IfThen(aOptions.Utf8Bom, 'Y', 'N')]);
      Result := True;
    end else begin
      aChanged := False;
      aReason := 'Save failed: ' + lSaveError;
      Result := False;
    end;
    exit;
  end;

  // Detect encoding using System.WideStrUtils.DetectUTF8Encoding
  SetLength(rb, length(lBytes));
  if length(lBytes) > 0 then
    move(lBytes[0], pAnsiChar(rb)^, length(lBytes));
  lEncType := DetectUTF8Encoding(rb);

  if lEncType = etUSAscii then
  begin
    if aOptions.EolMode = TEndOfLineMode.elmCrlf then
    begin
      lText := TEncoding.ASCII.GetString(lBytes);
      lText := NormalizeTextEolToCrlf(lText, lEolChanged);
      if lEolChanged then
      begin
        if aOptions.DryRun then
        begin
          aChanged := True;
          aReason := 'Would normalize EOL to CRLF (dry-run)';
          Result := True;
          exit;
        end;

        if aOptions.BackupDir <> '' then
        begin
          lRoot := IncludeTrailingPathDelimiter(ExpandFileName(aOptions.Path));
          lBackupPath := MakeBackupPath(aOptions, lRoot, aFile);
          TDirectory.CreateDirectory(ExtractFileDir(lBackupPath));
          TFile.copy(aFile, lBackupPath, True);
        end;

        if SaveTextUTF8(aFile, lText, False, lSaveError) then
        begin
          aChanged := True;
          aReason := 'Normalized EOL to CRLF';
          Result := True;
        end else begin
          aChanged := False;
          aReason := 'Save failed: ' + lSaveError;
          Result := False;
        end;
        exit;
      end;
    end;

    // Leave as-is. Do not add BOM even if option requests it.
    if aOptions.DryRun then
    begin
      aChanged := False;
      aReason := 'US-ASCII OK';
      Result := True;
      exit;
    end;

    aChanged := False;
    aReason := 'US-ASCII OK';
    Result := True;
    exit;
  end else if lEncType = etUTF8 then
  begin
    // It's UTF-8 (ASCII subset or multibyte). Only ensure BOM matches option.
    lHasBOM := HasUtf8Bom(lBytes);

    // Decode text ignoring BOM if present
    if lHasBOM then
      lText := TEncoding.UTF8.GetString(copy(lBytes, 3, length(lBytes) - 3))
    else
      lText := TEncoding.UTF8.GetString(lBytes);

    lEolChanged := False;
    if aOptions.EolMode = TEndOfLineMode.elmCrlf then
    begin
      lText := NormalizeTextEolToCrlf(lText, lEolChanged);
    end;

    if aOptions.DryRun then
    begin
      if (aOptions.Utf8Bom and (not lHasBOM)) and lEolChanged then
      begin
        aChanged := True;
        aReason := 'Would add UTF-8 BOM and normalize EOL to CRLF (dry-run)';
      end else if ((not aOptions.Utf8Bom) and lHasBOM) and lEolChanged then
      begin
        aChanged := True;
        aReason := 'Would remove UTF-8 BOM and normalize EOL to CRLF (dry-run)';
      end else if (aOptions.Utf8Bom and (not lHasBOM)) then
      begin
        aChanged := True;
        aReason := 'Would add UTF-8 BOM (dry-run)';
      end else if ((not aOptions.Utf8Bom) and lHasBOM) then
      begin
        aChanged := True;
        aReason := 'Would remove UTF-8 BOM (dry-run)';
      end else if lEolChanged then
      begin
        aChanged := True;
        aReason := 'Would normalize EOL to CRLF (dry-run)';
      end else begin
        aChanged := False;
        aReason := 'UTF-8 OK';
      end;
      Result := True;
      exit;
    end;

    if (aOptions.Utf8Bom and (not lHasBOM)) or ((not aOptions.Utf8Bom) and lHasBOM) or lEolChanged then
    begin
      if aOptions.BackupDir <> '' then
      begin
        lRoot := IncludeTrailingPathDelimiter(ExpandFileName(aOptions.Path));
        lBackupPath := MakeBackupPath(aOptions, lRoot, aFile);
        TDirectory.CreateDirectory(ExtractFileDir(lBackupPath));
        TFile.copy(aFile, lBackupPath, True);
      end;

      if SaveTextUTF8(aFile, lText, aOptions.Utf8Bom, lSaveError) then
      begin
        aChanged := True;
        if (aOptions.Utf8Bom and (not lHasBOM)) and lEolChanged then
          aReason := 'Added UTF-8 BOM and normalized EOL to CRLF'
        else if ((not aOptions.Utf8Bom) and lHasBOM) and lEolChanged then
          aReason := 'Removed UTF-8 BOM and normalized EOL to CRLF'
        else if aOptions.Utf8Bom and (not lHasBOM) then
          aReason := 'Added UTF-8 BOM'
        else if (not aOptions.Utf8Bom) and lHasBOM then
          aReason := 'Removed UTF-8 BOM'
        else
          aReason := 'Normalized EOL to CRLF';
        Result := True;
      end else begin
        aChanged := False;
        aReason := 'Save failed: ' + lSaveError;
        Result := False;
      end;
      exit;
    end;

    // No change needed
    Result := True;
    aChanged := False;
    aReason := 'UTF-8 OK';
    exit;
  end;

  // Mixed encodings possible (ANSI detected): split by raw CR/LF bytes, decode per line.
  lBytesNoBom := GetWithoutUtf8Bom(lBytes);

  // Detect dominant EOL and whether the original had a trailing EOL
  lCRLF := 0;
  lLF := 0;
  lCR := 0;
  i := 0;
  while i < length(lBytesNoBom) do
  begin
    if lBytesNoBom[i] = $0D then
    begin
      if (i + 1 < length(lBytesNoBom)) and (lBytesNoBom[i + 1] = $0A) then
      begin
        Inc(lCRLF);
        Inc(i, 2);
      end else begin
        Inc(lCR);
        Inc(i);
      end;
    end else if lBytesNoBom[i] = $0A then
    begin
      Inc(lLF);
      Inc(i);
    end else begin
      Inc(i);
    end;
  end;

  if (lCRLF >= lLF) and (lCRLF >= lCR) then
    lEOL := #13#10
  else if (lLF >= lCR) then
    lEOL := #10
  else
    lEOL := #13;

  if aOptions.EolMode = TEndOfLineMode.elmCrlf then
  begin
    lEOL := #13#10;
  end;

  lHadTrailingEOL := False;
  if length(lBytesNoBom) > 0 then
  begin
    if (length(lBytesNoBom) >= 2) and (lBytesNoBom[length(lBytesNoBom) - 2] = $0D) and (lBytesNoBom[length(lBytesNoBom) - 1] = $0A) then
      lHadTrailingEOL := True
    else if (lBytesNoBom[length(lBytesNoBom) - 1] = $0A) or (lBytesNoBom[length(lBytesNoBom) - 1] = $0D) then
      lHadTrailingEOL := True;
  end;

  lLinesBytes := SplitLinesByBytes(lBytesNoBom);

  GC(lFixedLines, TStringBuilder.Create(length(lBytes) + 1024), g);

  lCntUtf8 := 0;
  lCnt1250 := 0;
  lCnt1252 := 0;
  lCntAnsi := 0;
  lCntAscii := 0;

  lFirst := True;
  for lLine in lLinesBytes do
  begin
    if not lFirst then
    begin
      lFixedLines.append(lEOL);
    end else begin
      lFirst := False;
    end;
    lFixedLines.append(DecodeBestPerLine(lLine, lEncName));
    if lEncName = 'UTF-8' then
      Inc(lCntUtf8)
    else if lEncName = 'Windows-1250' then
      Inc(lCnt1250)
    else if lEncName = 'Windows-1252' then
      Inc(lCnt1252)
    else if lEncName = 'ANSI' then
      Inc(lCntAnsi)
    else if lEncName = 'ASCII' then
      Inc(lCntAscii);
  end;

  if (length(lLinesBytes) > 0) and lHadTrailingEOL then
  begin
    lFixedLines.append(lEOL);
  end;

  // Summarize detected encoding(s)
  lKinds := 0;
  if lCntUtf8 > 0 then Inc(lKinds);
  if lCnt1250 > 0 then Inc(lKinds);
  if lCnt1252 > 0 then Inc(lKinds);
  if lCntAnsi > 0 then Inc(lKinds);

  if lKinds > 1 then
    lReasonEnc := 'mixed bytes'
  else if lCntUtf8 > 0 then
    lReasonEnc := 'detected UTF-8'
  else if lCnt1250 > 0 then
    lReasonEnc := 'detected Windows-1250'
  else if lCnt1252 > 0 then
    lReasonEnc := 'detected Windows-1252'
  else if lCntAnsi > 0 then
    lReasonEnc := 'detected ANSI'
  else
    lReasonEnc := 'detected ASCII';

  // If dry-run, do not write anything—just indicate change.
  if aOptions.DryRun then
  begin
    aChanged := True;
    aReason := Format('%s; would save UTF-8 (BOM=%s, EOL=%s) (dry-run)',
      [lReasonEnc,
       IfThen(aOptions.Utf8Bom, 'Y', 'N'),
       IfThen(lEOL = #13#10, 'CRLF', IfThen(lEOL = #10, 'LF', 'CR'))]);
    Result := True;
    exit;
  end;

  // Backup if requested
  if aOptions.BackupDir <> '' then
  begin
    lRoot := IncludeTrailingPathDelimiter(ExpandFileName(aOptions.Path));
    lBackupPath := MakeBackupPath(aOptions, lRoot, aFile);
    TDirectory.CreateDirectory(ExtractFileDir(lBackupPath));
    TFile.copy(aFile, lBackupPath, True);
  end;

  // Save fixed version
  if SaveTextUTF8(aFile, lFixedLines.ToString, aOptions.Utf8Bom, lSaveError) then
  begin
    aChanged := True;
    aReason := Format('%s; saved UTF-8 (BOM=%s, EOL=%s)',
      [lReasonEnc,
       IfThen(aOptions.Utf8Bom, 'Y', 'N'),
       IfThen(lEOL = #13#10, 'CRLF', IfThen(lEOL = #10, 'LF', 'CR'))]);
    Result := True;
  end else begin
    aChanged := False;
    aReason := 'Save failed: ' + lSaveError;
    Result := False;
  end;
end;

function TEncodingFixTool.GetWithoutUtf8Bom(const aBytes: TBytes): TBytes;
begin
  if HasUtf8Bom(aBytes) then
    Result := copy(aBytes, 3, length(aBytes) - 3)
  else
    Result := aBytes;
end;

function TEncodingFixTool.ShowHelp: integer;
const
  HELP_TEXT: PChar =
    'Delphi *.pas Encoding Fix Tool' + sLineBreak +
    sLineBreak +
    'Usage:' + sLineBreak +
    '  EncodingFixTool [params]' + sLineBreak +
    sLineBreak +
    'Params:' + sLineBreak +
    '  help                  : Show this help text.' + sLineBreak +
    '  dry                   : Dry run (no files are changed).' + sLineBreak +
    '  s | silent            : No console output.' + sLineBreak +
    '  v | verbose           : More output (not compatible with silent).' + sLineBreak +
    '  path=<dir>            : Directory to scan. Default: current working dir.' + sLineBreak +
    '  recursive=y|n         : Recurse into subfolders. Default: y.' + sLineBreak +
    '  ext=<csv>             : Extensions list. Default: pas,dpr. Accepts "pas", ".pas", "*.pas".' + sLineBreak +
    '  preset=delphi-ai      : AI cleanup preset for Delphi projects.' + sLineBreak +
    '  scope=all|git-changed : Scan all matching files or only Git modified/untracked files. Default: all.' + sLineBreak +
    '  format=text|json      : Human text output or compact JSON summary. Default: text.' + sLineBreak +
    '  utf8-bom=y|n          : Save with UTF-8 BOM (default: y).' + sLineBreak +
    '    Note: Pure US-ASCII files are left without BOM regardless of utf8-bom.' + sLineBreak +
    '  eol=preserve|crlf     : Preserve original line endings or normalize to CRLF. Default: preserve.' + sLineBreak +
    '  bkp-dir=<dir>         : If set, save a backup copy before overwriting.' + sLineBreak +
    sLineBreak +
    'How it works:' + sLineBreak +
    '- Files are processed in parallel (TParallel.For).' + sLineBreak +
    '- Each file is first tested for strict UTF-8.' + sLineBreak +
    '- If that fails, lines are split by raw CR/LF and decoded per line using heuristics' + sLineBreak +
    '  between UTF-8, Windows-1250 (PL/CE), and Windows-1252 (DE/Western).' + sLineBreak +
    '- Fixed text is saved as UTF-8 (with/without BOM per option).' + sLineBreak +
    '- eol=crlf also normalizes valid ASCII/UTF-8 files that need only line-ending repair.' + sLineBreak +
    sLineBreak +
    'Examples:' + sLineBreak +
    '  EncodingFixTool dry path=c:\src ext=pas,dpr recursive=n' + sLineBreak +
    '  EncodingFixTool path=./src ext="*.pas,*.dpr" utf8-bom=n eol=crlf v' + sLineBreak +
    '  EncodingFixTool path=c:\tmp bkp-dir=c:\bkp' + sLineBreak +
    sLineBreak;
begin
  writeln(HELP_TEXT);
  Result := 0;
end;

function TEncodingFixTool.ParseCommandLine(out aOptions: TOptions): integer;
var
  i: integer;
  p, Key, Val: string;
  eqPos: integer;
  lBool: boolean;

  function FailParse(const aMsg: string): integer;
  begin
    if not aOptions.Silent then
    begin
      TSafeConsole.WriteLine('ERROR: ' + aMsg);
    end;
    Result := 1;
  end;

  function TryAsYN(const s: string; const aDefault: boolean; out aValue: boolean): boolean;
  var
    l: string;
  begin
    l := LowerCase(s.Trim);
    if (l = '') then
    begin
      aValue := aDefault;
      exit(True);
    end else if (l = 'y') or (l = 'yes') or (l = '1') or (l = 'true') then
    begin
      aValue := True;
      exit(True);
    end else if (l = 'n') or (l = 'no') or (l = '0') or (l = 'false') then
    begin
      aValue := False;
      exit(True);
    end else begin
      aValue := aDefault;
      exit(False);
    end;
  end;

begin
  // Defaults
  aOptions.DryRun := False;
  aOptions.Silent := False;
  aOptions.Verbose := False;
  aOptions.Path := GetCurrentDir;
  aOptions.Recursive := True;
  aOptions.Exts := NormalizeExtList('pas,dpr');
  aOptions.Utf8Bom := True; // default y (Delphi-friendly)
  aOptions.EolMode := TEndOfLineMode.elmPreserve;
  aOptions.OutputFormat := TOutputFormat.ofText;
  aOptions.ScopeMode := TScopeMode.smAll;
  aOptions.BackupDir := '';

  // Parse
  for i := 1 to ParamCount do
  begin
    p := Trim(ParamStr(i));

    // allow for "-" or "/" param prefixes
    if startsStr('-', p) {$IFDEF MsWindows}or StartsStr('/', p){$ENDIF} then
      delete(p, 1, 1);
    if p = '' then
    begin
      Continue;
    end;


    // allow "key=value" or "key:value" or single flag
    eqPos := p.IndexOf('=');
    if eqPos < 0 then
    begin
      eqPos := p.IndexOf(':');
    end;

    if eqPos > 0 then
    begin
      Key := LowerCase(Trim(copy(p, 1, eqPos)));
      Val := Trim(copy(p, eqPos + 2, MaxInt));
    end else begin
      Key := LowerCase(p);
      Val := '';
    end;

    if (Key = 'help') or (Key = 'h') then
    begin
      exit(ShowHelp);
    end else if (Key = 'dry') then
    begin
      aOptions.DryRun := True;
    end else if (Key = 's') or (Key = 'silent') then
    begin
      aOptions.Silent := True;
      aOptions.Verbose := False;
    end else if (Key = 'v') or (Key = 'verbose') then
    begin
      if not aOptions.Silent then
      begin
        aOptions.Verbose := True;
      end;
    end else if (Key = 'path') then
    begin
      if Val <> '' then
      begin
        aOptions.Path := ExpandFileName(Val);
      end;
    end else if (Key = 'recursive') then
    begin
      if not TryAsYN(Val, True, lBool) then
      begin
        exit(FailParse('invalid recursive value: ' + Val));
      end;
      aOptions.Recursive := lBool;
    end else if (Key = 'ext') then
    begin
      if Val <> '' then
      begin
        Val := Val.Trim([' ', '"', '''']); // remove outer quotes
        aOptions.Exts := NormalizeExtList(Val);
      end;
    end else if (Key = 'preset') then
    begin
      Val := LowerCase(Val.Trim);
      if Val = 'delphi-ai' then
      begin
        aOptions.Recursive := True;
        aOptions.Exts := NormalizeExtList('pas,dpr,dpk,inc,dfm,dproj');
        aOptions.Utf8Bom := True;
        aOptions.EolMode := TEndOfLineMode.elmCrlf;
      end else begin
        exit(FailParse('invalid preset value: ' + Val));
      end;
    end else if (Key = 'scope') then
    begin
      Val := LowerCase(Val.Trim);
      if (Val = '') or (Val = 'all') then
      begin
        aOptions.ScopeMode := TScopeMode.smAll;
      end else if Val = 'git-changed' then
      begin
        aOptions.ScopeMode := TScopeMode.smGitChanged;
      end else begin
        exit(FailParse('invalid scope value: ' + Val));
      end;
    end else if (Key = 'format') then
    begin
      Val := LowerCase(Val.Trim);
      if (Val = '') or (Val = 'text') then
      begin
        aOptions.OutputFormat := TOutputFormat.ofText;
      end else if Val = 'json' then
      begin
        aOptions.OutputFormat := TOutputFormat.ofJson;
      end else begin
        exit(FailParse('invalid format value: ' + Val));
      end;
    end else if (Key = 'utf8-bom') then
    begin
      if not TryAsYN(Val, True, lBool) then
      begin
        exit(FailParse('invalid utf8-bom value: ' + Val));
      end;
      aOptions.Utf8Bom := lBool;
    end else if (Key = 'eol') then
    begin
      Val := LowerCase(Val.Trim);
      if (Val = '') or (Val = 'preserve') then
      begin
        aOptions.EolMode := TEndOfLineMode.elmPreserve;
      end else if Val = 'crlf' then
      begin
        aOptions.EolMode := TEndOfLineMode.elmCrlf;
      end else begin
        exit(FailParse('invalid eol value: ' + Val));
      end;
    end else if (Key = 'bkp-dir') then
    begin
      if Val <> '' then
      begin
        aOptions.BackupDir := ExpandFileName(Val);
      end;
    end else begin
      exit(FailParse('unknown parameter: ' + Key));
    end;
  end;

  Result := -1; // continue execution
end;

class function TEncodingFixTool.RunFromCommandLine: integer;
var
  g: TGarbos;
  lTool: TEncodingFixTool;
  lOptions: TOptions;
  lParseRes: integer;
  lFiles: TArray<string>;
  lChangedCount: integer;
  lFailureCount: integer;
  lScannedCount: integer;
  lSkippedCount: integer;
  lStopwatch: TStopWatch;
  lSilent: boolean;
  lVerbose: boolean;
  lLoopProc: TProc<integer>;
  lRoot: string;
begin

  GC(lTool, TEncodingFixTool.Create, g);

  lParseRes := lTool.ParseCommandLine(lOptions);
  if lParseRes >= 0 then
  begin
    exit(lParseRes); // help shown or early exit
  end;

  lSilent := lOptions.Silent;
  lVerbose := lOptions.Verbose and (not lSilent);

  if not TDirectory.Exists(lOptions.Path) then
  begin
    if not lSilent then
    begin
      TSafeConsole.WriteLine('ERROR: path not found: ' + lOptions.Path);
    end;
    exit(2);
  end;

  if (lOptions.BackupDir <> '') and (not TDirectory.Exists(lOptions.BackupDir)) then
  begin
    // create backup root if needed
    TDirectory.CreateDirectory(lOptions.BackupDir);
  end;

  lRoot := IncludeTrailingPathDelimiter(ExpandFileName(lOptions.Path));

  // Prepare fast extension lookup
  lTool.PrepareExtIndex(lOptions.Exts);

  lFiles := lTool.CollectFiles(lOptions);
  if (length(lFiles) = 0) and (not lSilent) and (lOptions.OutputFormat = TOutputFormat.ofText) then
  begin
    TSafeConsole.WriteLine('No files found.');
  end;

  lChangedCount := 0;
  lFailureCount := 0;
  lScannedCount := Length(lFiles);
  lSkippedCount := 0;
  lStopwatch := TStopWatch.startNew;

  lLoopProc :=

  procedure(idx: integer)
var
  lFile: string;
  lChanged: boolean;
  lReason: string;
  lMsg: string;
  lLocalChanged: integer;
  lLocalFailed: integer;
  lLocalSkipped: integer;
  lRelFile: string;
begin
  lFile := lFiles[idx];
  lLocalChanged := 0;
  lLocalFailed := 0;
  lLocalSkipped := 0;
  lRelFile := lTool.MakeRelativeTo(lRoot, lFile);

  try
    if lTool.FixFile(lFile, lOptions, lChanged, lReason) then
    begin
      if lChanged then
      begin
        if not lOptions.DryRun then
        begin
          lLocalChanged := 1;
        end;
        if (not lSilent) and (lOptions.OutputFormat = TOutputFormat.ofText) then
        begin
          TSafeConsole.WriteLine(Format('%s: %s (%s)',
            [IfThen(lOptions.DryRun, 'Would fix', 'Fixed'), lRelFile, lReason]));
        end;
      end else begin
        if StartsText('Skipped ', lReason) then
        begin
          lLocalSkipped := 1;
        end;
        if lVerbose and (not lSilent) and (lOptions.OutputFormat = TOutputFormat.ofText) then
        begin
          TSafeConsole.WriteLine('OK   : ' + lRelFile + ' (' + lReason + ')');
        end;
      end;
    end else begin
      lLocalFailed := 1;
      if (not lSilent) and (lOptions.OutputFormat = TOutputFormat.ofText) then
      begin
        TSafeConsole.WriteLine('FAIL : ' + lRelFile + ' (' + lReason + ')');
      end;
    end;
  except
    on e: Exception do
    begin
      lLocalFailed := 1;
      if (not lSilent) and (lOptions.OutputFormat = TOutputFormat.ofText) then
      begin
        lMsg := Format('ERROR: %s (%s)', [lRelFile, e.Message]);
        TSafeConsole.WriteLine(lMsg);
      end;
    end;
  end;

  if lLocalChanged <> 0 then
  begin
    TInterlocked.Add(lChangedCount, lLocalChanged);
  end;
  if lLocalFailed <> 0 then
  begin
    TInterlocked.Add(lFailureCount, lLocalFailed);
  end;
  if lLocalSkipped <> 0 then
  begin
    TInterlocked.Add(lSkippedCount, lLocalSkipped);
  end;
end;

if length(lFiles) > 0 then
  TParallel.&For(0, High(lFiles), lLoopProc);

lStopwatch.stop;

if (lOptions.OutputFormat = TOutputFormat.ofJson) and (not lSilent) then
begin
  TSafeConsole.WriteLine(Format('{"scanned":%d,"changed":%d,"skipped":%d,"failed":%d}',
    [lScannedCount, lChangedCount, lSkippedCount, lFailureCount]));
end else if not lSilent then
begin
  TSafeConsole.WriteLine('');
  TSafeConsole.WriteLine(Format('Done in %s. Files changed: %d. Failures: %d',
    [FormatDateTime('nn:ss.zzz', // pretty mm:ss.mmm
        EncodeTime(0, lStopwatch.Elapsed.Minutes, lStopwatch.Elapsed.Seconds, lStopwatch.Elapsed.Milliseconds)
        ),
      lChangedCount,
      lFailureCount]));
end;

if lFailureCount <> 0 then
  Result := 1
else
  Result := 0;
end;

end.
