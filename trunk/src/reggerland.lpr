program Egger;
{$mode objfpc}{$H+}
{ $apptype console}

uses
  EggerGame in 'EggerGame.pas';

{$R *.res}

begin
  Game := TGame.Create;
  try
    Game.Initialize;
    Game.Play;
  finally
    Game.Finalize;
  end;
end.
