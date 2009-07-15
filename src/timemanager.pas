{
    This unit pulls in the EpikTimer dependency. EpikTimer is platform
    independent and replaces Windows.QueryPerformanceCounter() calls.
}
unit TimeManager;

{$mode objfpc}{$H+}

interface

uses
  EpikTimer;

// Simple singleton to access the timer.
function gTimer: TEpikTimer;

implementation

var
  uTimer: TEpikTimer;

function gTimer: TEpikTimer;
begin
  if not Assigned(uTimer) then
    uTimer := TEpikTimer.Create(nil);
  Result := uTimer;
end;


initialization
  uTimer := nil;

finalization
  uTimer.Free;

end.

