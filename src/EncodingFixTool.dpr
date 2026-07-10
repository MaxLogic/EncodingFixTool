program EncodingFixTool;

{$APPTYPE CONSOLE}
{$IFDEF MSWINDOWS}
{$R *.res}
{$ENDIF}

uses
  System.SysUtils,
  EncodingFixToolCore in 'EncodingFixToolCore.pas';

begin
  ExitCode := TEncodingFixTool.RunFromCommandLine;
end.
