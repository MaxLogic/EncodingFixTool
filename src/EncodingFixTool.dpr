program EncodingFixTool;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  EncodingFixToolCore in 'EncodingFixToolCore.pas';

begin
  ExitCode := TEncodingFixTool.RunFromCommandLine;
end.
