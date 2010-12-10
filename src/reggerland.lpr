program Egger;

{ TODO : Hero should sail }
{ TODO : Stony should push }
{ TODO : Make raft swim }
uses
  EggerGame in 'EggerGame.pas';

begin
  Game := TGame.Create;
  try
    Game.Initialize;
    Game.Play;
  finally
    Game.Finalize;
  end;
end.
