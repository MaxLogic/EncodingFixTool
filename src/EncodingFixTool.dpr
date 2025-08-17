program EncodingFixTool;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  EncodingFixToolCore in 'EncodingFixToolCore.pas';

begin
  ExitCode := TEncodingFixTool.RunFromCommandLine;
end.
