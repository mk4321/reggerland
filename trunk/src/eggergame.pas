{ Implements game container

  @Author  Alexey Yarochkin
  @Version 07.12.2010 v0.2
  @History 11.02.2005 v0.1
}

unit EggerGame;

interface

uses
  SDL, SDLSprites, SDL_Mixer, EggerSprite, EggerData, IniFiles;

type
  TSpriteImages = array [TSpriteType] of PSDL_Surface;
  TSounds = array [TSoundType] of PMix_Chunk;
  TMap = array [0..MapHeight - 1, 0..MapWidth - 1] of TMapCell;
  TInfoMap = array [0..MapHeight - 1, MapWidth..MapWidth + MapInfoWidth - 1] of TMapCell;

  { TGame }

  TGame = class
  private
    Screen,
    Background,
    TileImage,
    FontImage: PSDL_Surface;
    Xorig, Yorig: TPixCoord;
    dXorig, dYorig: TDelta;
    fIniFile: TIniFile;
    fMap: TMap;
    fInfo: TInfoMap;
    fStartLevel: string;
    fLevel: T3DMapCoord;
    fMapLevelStatus: TMapLevelStatus;
    fLevelTimer: TLevelTimer;
    fSounds: TSounds;
    fGameState: TGameState;
    fMovePhase: Byte;
    fSpriteEngine: TSpriteEngine;
    fSpriteImage: TSpriteImages;
    function GetMap(aMapX, aMapY: TMapCoord): TMapCell;
    function GetPixMap(X, Y: TPixCoord): TMapCell;
    procedure LoadMap(var Map; Length: Word);
    function LoadImage(const Name: string): PSDL_Surface;
    procedure LoadGame;
    function LoadSound(const Name: String): PMix_Chunk;
    procedure SaveGame;
    procedure CreateSprites;
    procedure DeleteSprites(aDeleteAll: Boolean);
    procedure DrawMap;
    procedure OpenChest;
    procedure OpenDoors(aState: TGameState);
    procedure KillHero;
    procedure NextLevel;
    function GetLevelStatus: TLevelStatus;
    procedure SetGameState(const aValue: TGameState);
    procedure SetLevelStatus(aValue: TLevelStatus);
    procedure SetLevelTimer(Value: TLevelTimer);
    function ParseLevel(const S: string): Boolean;
  public
    Hero: THero;

    constructor Create;
    procedure Initialize;
    procedure Play;
    procedure Finalize;

    procedure DrawBackground;
    procedure DrawTile(aMapX, aMapY: TMapCoord; Update: Boolean);
    procedure DrawInfo;
    procedure PlaySound(Sound: TSoundType);
    procedure WriteText(const Text: string; aX, aY: TPixCoord);

    function TileCount(aTiles: TTiles; aCountWeight: Boolean = False): Word;
    function SpriteIn(aGridX1, aGridX2, aGridY1, aGridY2: TGridCoord;
      aExclude: TActiveSprite = nil): TActiveSprite;
    procedure ReplaceTiles(aSrcTile, aDestTile: TTileType; aLayer: TMapLayer = mlItem;
      aExclude: ShortInt = -1);
    procedure CheckHiddenItems(aUnhide: Boolean = False);

    property Map[MapX, MapY: TMapCoord]: TMapCell read GetMap;
    property PixMap[X, Y: TPixCoord]: TMapCell read GetPixMap;
    procedure SetMap(aMapX, aMapY: TMapCoord; aLayer: TMapLayer; aValue: TTileType);
    procedure SetNextLevel(dx, dy, dz: TDelta);
    function EndMovePhase(aMask: Byte = $3): Boolean;
    property Level: T3DMapCoord read fLevel;
    property LevelStatus: TLevelStatus read GetLevelStatus write SetLevelStatus;
    property LevelTimer: TLevelTimer read FLevelTimer write SetLevelTimer;
    property GameState: TGameState read fGameState write SetGameState;
    property MovePhase: Byte read fMovePhase;
    property SpriteEngine: TSpriteEngine read fSpriteEngine;
    property SpriteImage: TSpriteImages read fSpriteImage;
  end;

  function GameStateName: String;

var
  Game: TGame;

implementation

uses
  SysUtils, SDLUtils, SDL_Image, Logger;

type
  TKeyInfo = record
    // SDL key code
    SDLK: Word;
    // if key can be placed in stack only once while pressed
    PlaceOnce: Boolean;
    Name: String;
  end;
  TKeyRange = 0..10;
  TKeyState = record
    Index: TKeyRange;
    PrevIndex: TKeyRange;
    Down: Boolean;
  end;

const
  GameKeys: array [TKeyRange] of TKeyInfo = (
    (SDLK: 0;           PlaceOnce: False; Name: '(none)'),
    (SDLK: SDLK_SPACE;  PlaceOnce: True;  Name: 'Space'),
    (SDLK: SDLK_LEFT;   PlaceOnce: False; Name: 'Left'),
    (SDLK: SDLK_RIGHT;  PlaceOnce: False; Name: 'Right'),
    (SDLK: SDLK_UP;     PlaceOnce: False; Name: 'Up'),
    (SDLK: SDLK_DOWN;   PlaceOnce: False; Name: 'Down'),
    (SDLK: SDLK_F2;     PlaceOnce: True;  Name: 'F2'),
    (SDLK: SDLK_F3;     PlaceOnce: True;  Name: 'F3'),
    (SDLK: SDLK_F10;    PlaceOnce: True;  Name: 'F10'),
    (SDLK: SDLK_DELETE; PlaceOnce: True;  Name: 'Del'),
    (SDLK: SDLK_ESCAPE; PlaceOnce: True;  Name: 'Esc'));

  NormalDelay = 40;
  ShortDelay = 10;

function GameStateName: String;
begin
  if Game <> nil then
    Result := GameStateNames[Game.GameState]
  else
    Result := '(not created)';
end;

procedure FatalError(const S: string);
begin
//  MessageBox(0, PChar(S), '', MB_ICONERROR + MB_OK);
  Log.LogError(S, GameStateName);
  Halt;
end;

{ TGame }

constructor TGame.Create;
begin
  Randomize;
  GameState := gsNextLevel;
  LoadMap(FInfo, SizeOf(FInfo));
end;

procedure TGame.Initialize;

  function FindVideoMode: Integer;
  const
    CBPP: array [0..4] of Integer = (15, 16, 24, 32, 8);
  var
    Index: Byte;
  begin
    Index := low(CBPP);
    repeat
      Result := SDL_VideoModeOK(ScreenWidth, ScreenHeight, CBPP[Index], 0);
      Inc(Index);
    until (Result <> 0) or (Index > High(CBPP));
  end;

var
  BPP: Integer;
  spritetype: TSpriteType;
  soundtype: TSoundType;
begin
  // initializing SDL engine
  if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO) = -1 then
    FatalError(SSDLInitError);

  // setting window properties
  SDL_WM_SetCaption(PChar(SAppTitle), nil);
  SDL_WM_SetIcon(IMG_Load(PChar(SGFXDir + SIconFile)), 0);

  // initializing video mode
  BPP := FindVideoMode;
  Log.LogStatus(Format('Selected video mode: %d BPP', [BPP]), GameStateName);

  Screen := SDL_SetVideoMode(ScreenWidth, ScreenHeight, BPP,
    SDL_SWSURFACE or SDL_HWPALETTE {or SDL_DOUBLEBUF} {or SDL_FULLSCREEN});
  if Screen = nil then
    FatalError(SVideoModeError);

  // initializing background surface
  Background := SDL_CreateRGBSurface(
    SDL_SWSURFACE or SDL_HWPALETTE, ScreenWidth, ScreenHeight, BPP, 0, 0, 0, 0);
  if Background = nil then
    FatalError(SBackgroundError + ': ' + SDL_GetError);
  Background := SDL_DisplayFormat(Background);

  // loading images
  TileImage := LoadImage(STilesFile);
  FontImage := LoadImage(SFontFile);
  for spritetype := low(TSpriteType) to high(TSpriteType) do
    fSpriteImage[spritetype] := LoadImage(SSpriteFile[spritetype]);
  SDL_ShowCursor(SDL_DISABLE);

  // initializing sprite engine
  fSpriteEngine := TSpriteEngine.Create(Screen);
  fSpriteEngine.BackgroundSurface := Background;

  // initializing audio
  if Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT,
    MIX_DEFAULT_CHANNELS, 2048) < 0 then
      FatalError(SAudioInitError);
//Mix_HookMusicFinished(@RestartMusic);
  for soundtype := low(TSoundType) to high(TSoundType) do
    LoadSound(SSoundFile[soundtype]);
  fIniFile := TIniFile.Create(SMapDir + SIniFile);
  fStartLevel := fIniFile.ReadString(SGenSection, SStart, '');
//  fStartLevel := 'L0806';
  // setting initial level
  Log.LogStatus('Setting initial level ' + fStartLevel, GameStateName);
  if not ParseLevel(fStartLevel) then
    FatalError(SIniReadError);
  // creating Hero
  Hero := THero.Create(sHero, $7, MapToPix(7), MapToPix(3));
end;

procedure TGame.Play;
var
  Ticks, NextTicks: Cardinal;
  Event: TSDL_Event;
  Keys: PKeyStateArr;
  Key: TKeyState;

  procedure CheckKeys;
  var
    i: Byte;
  begin
    for i := Low(GameKeys) to High(GameKeys) do
      // if a pressed key is found
      if Keys^[GameKeys[i].SDLK] = 1 then
      begin
        // key can be placed in stack if
        // - it can be pressed more than once (continuously)
        // - it can be pressed once and the key was not down just before
        if not GameKeys[i].PlaceOnce or not Key.Down then
          if (Key.PrevIndex <> i) or EndMovePhase then
          begin
            Key.Index := i;
            Log.LogStatus(Format('Key %s registered in move phase %d (previous was %s)',
              [GameKeys[i].Name, MovePhase, GameKeys[Key.PrevIndex].Name]), GameStateName);
          end;
        Key.Down := True;
        Exit;
      end;
    // if no pressed keys are found isDown flag is reset to false
    Key.Down := False;
    Key.PrevIndex := 0;
  end;

  procedure MoveSprites;
  begin
    // process case with a set level timer
    if (LevelTimer > 0) and EndMovePhase($1F) then
    begin
      LevelTimer := LevelTimer - 1;
      if LevelTimer = 0 then
        KillHero;
    end;
    SpriteEngine.Move;
    Inc(fMovePhase);
  end;

  procedure StartLevel;
  begin
    if (LevelStatus = lsDone) or (TileCount([tHeart]) = 0) then
      GameState := gsCatchKey
    else
      GameState := gsPlayLevel;
    DrawBackground;
    fMovePhase := 0;
  end;

  procedure HackLevel;
  begin
    OpenDoors(gsPlayLevel);
    Key.Index := 0;
    Hero.Ghostly := True;
  end;

  procedure ProcessDebugKeys;
  begin
    if (Keys^[SDLK_LCTRL] + Keys^[SDLK_LSHIFT] = 2) then
      case GameKeys[Key.Index].SDLK of
        SDLK_DELETE: HackLevel;
      end;
  end;

  procedure ProcessUserkeys;
  begin
    case GameKeys[Key.Index].SDLK of
      SDLK_SPACE:  Hero.Attack(Hero.dX, Hero.dY);
      SDLK_LEFT:   Hero.Pushed(-1, 0);
      SDLK_RIGHT:  Hero.Pushed(1, 0);
      SDLK_UP:     Hero.Pushed(0, -1);
      SDLK_DOWN:   Hero.Pushed(0, 1);
      SDLK_ESCAPE: GameState := gsQuit;
      SDLK_F2:     SaveGame;
      SDLK_F3:     LoadGame;
      SDLK_F10:    GameState := gsKillHero;
    end;
  end;

begin
  NextTicks := 0;
  Key.Index := 0;
  Key.PrevIndex := 0;
  Key.Down := False;

  // game loop start
  repeat
    while SDL_PollEvent(@Event) > 0 do;

    Keys := PKeyStateArr(SDL_GetKeyState(nil));
    CheckKeys;
    // keys are processed from stack, in persistent modes only
    // hero activities are processed when hero is on the grid edge
    if (Key.Index <> 0) and (GameState in PersistentStates) and EndMovePhase then
    begin
      ProcessDebugKeys;
      ProcessUserKeys;
      Key.PrevIndex := Key.Index;
      Key.Index := 0;
    end;

    // level starts moving if a key is pressed
    if Key.Down and (GameState = gsStartLevel) then
      StartLevel;

    case GameState of
      gsOpenChest:
        OpenChest;
      gsOpenDoors:
        OpenDoors(gsPlayLevel);
      gsNextLevel:
        NextLevel;
      gsKillHero:
        KillHero;
      gsPlayLevel, gsCatchKey:
        MoveSprites;
    end;

    // updating sprites on the screen
    SpriteEngine.Draw;
    SDL_UpdateRect(Screen, 0, 0, 0, 0);

    // remaining time delay at the end of loop
    Ticks := SDL_GetTicks;
    if NextTicks > Ticks then
      SDL_Delay(NextTicks - Ticks);
    if Keys^[SDLK_LALT] = 1 then
      NextTicks := SDL_GetTicks + ShortDelay
    else
      NextTicks := SDL_GetTicks + NormalDelay;

  until GameState = gsQuit;
  // game loop end
end;

procedure TGame.Finalize;
var
  spritetype: TSpriteType;
  soundtype: TSoundType;
begin
  fIniFile.Free;
  // audio
  for soundtype := Low(TSoundType) to High(TSoundType)do
    if FSounds[soundtype] <> nil then
      Mix_FreeChunk(FSounds[soundtype]);
  Mix_CloseAudio;

  // sprite engine
  SpriteEngine.Free;

  // images
  for spritetype := Low(TSpriteType) to High(TSpriteType) do
    if fSpriteImage[spritetype] <> nil then
      SDL_FreeSurface(fSpriteImage[spritetype]);
  if FontImage <> nil then
    SDL_FreeSurface(FontImage);
  if TileImage <> nil then
    SDL_FreeSurface(TileImage);

  // background surface
  if Background <> nil then
    SDL_FreeSurface(Background);

  // closing video mode
  if Screen <> nil then
    SDL_FreeSurface(Screen);

  // SDL engine
  SDL_Quit;

  Destroy;
end;

function TGame.LoadImage(const Name: string): PSDL_Surface;
begin
  Log.LogStatus('Loading image ' + Name, GameStateName);
  Result := IMG_Load(PChar(SGFXDir + Name));
  if Result = nil then
    FatalError('Error loading image: ' + Name);
  // converting image in the current color mode
  Result := SDL_DisplayFormat(Result);
  // setting image transparency
  SDL_SetColorKey(Result, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
    SDL_MapRGB(Result^.Format, $FF, 0, $FF));
end;

function TGame.LoadSound(const Name: String): PMix_Chunk;
begin
  Log.LogStatus('Loading sound ' + Name, GameStateName);
  Result := Mix_LoadWav(PChar(SSndDir + Name));
  if Result = nil then
    Log.LogWarning('Error loading sound: ' + Name, GameStateName);
end;

procedure TGame.DrawBackground;
var
  MapX, MapY: TMapCoord;
begin
  for MapY := 0 to MapHeight - 1 do
    for MapX := 0 to MapWidth + MapInfoWidth - 1 do
      with fMap[MapY, MapX] do
      begin
        if (LevelStatus = lsDone) and (Item in CollectedItems) then
          Item := tEmpty;
        DrawTile(MapX, MapY, False);
      end;
  DrawMap;
  DrawInfo;
end;

procedure TGame.DrawTile(aMapX, aMapY: TMapCoord; Update: Boolean);
var
  Layer: TMapLayer;
  srcRect, destRect: TSDL_Rect;
  TileType: TTileType;
begin
  srcRect.y := 0;
  srcRect.w := MapCellSize;
  srcRect.h := MapCellSize;
  destRect.x := MapToPix(aMapX);
  destRect.y := MapToPix(aMapY);

  with Map[aMapX, aMapY] do
    // drawing from the "back" up to the "item" layer
    for Layer := mlBack to mlItem do
    begin
      // getting tile from the coresponding map table
      if aMapX < MapWidth then
        TileType := Z[Layer]
      else
        TileType := FInfo[aMapY, aMapX].Z[Layer];
      if Layer = mlItem then
        // hidden items are not shown while having some weight
        // or shown as a "diamond"
        if TileType in HiddenItems then
          if Weight > 0 then
            TileType := tEmpty
          else
            TileType := tDiamond
        // chest items are shown as a "chest"
        // until catching key starts or level is done
        else if TileType in ChestItems then
          if (GameState in (PersistentStates - [gsCatchKey])) and (LevelStatus <> lsDone) then
            TileType := tChest
          else if Hero.InBasket(TileType) then
            TileType := tEmpty;

      srcRect.x := MapToPix(Ord(TileType));

      if TileType <> tEmpty then
        SDL_BlitSurface(TileImage, @srcRect, Background, @destRect);
    end;

  if Update then
    SDL_UpperBlit(Background, nil, Screen, nil);
end;

procedure TGame.DrawInfo;
var
  Index: Byte;
  MapX, MapY: TMapCoord;
begin
  // drawing Hero's basket items
  if Hero <> nil then
  begin
    for Index := low(Hero.Basket) to high(Hero.Basket) do
      FInfo[5, MapWidth + 1 + Index].Mid := Hero.Basket[Index];
    FInfo[3, MapWidth + 1].Item := ifop(Hero.Bullets > 0, tBullet, tNothing);
  end;

  for MapY := 1 to 8 do
    for MapX := MapWidth to MapWidth + 4 do
      DrawTile(MapX, MapY, False);

  WriteText('STUFF', MapToPix(MapWidth + 2) + GridCellSize, MapToPix(2) + 9);
//  WriteText('HINT', (MapWidth + 1) * MapCellSize + 16, 6 * MapCellSize + 9);

  // writing Hero's bullets amount
  if Hero.Bullets > 0 then
    WriteText(IntToStr(Hero.Bullets), MapToPix(MapWidth + 1) + GridCellSize, MaptoPix(4));

  if LevelTimer > 0 then
  begin
    WriteText('TIME', MapToPix(MapWidth + 2) + GridCellSize, MapToPix(6));
    WriteText(IntToStr(LevelTimer), MapToPix(MapWidth + 2) + GridCellSize, MapToPix(7));
  end;

  DrawTile(0, 0, True);
end;

procedure TGame.DrawMap;
const
  CMapR: array [Boolean] of Uint8 = ($7F, $FF);
  CMapG: array [Boolean] of Uint8 = ($FF, $FF);
  CMapB: array [Boolean] of Uint8 = ($7F, $7F);
var
  mapX, mapY: TMapCoord;
  destRect: TSDL_Rect;
  isCurrent: Boolean;
begin
  destRect.w := 8;
  destRect.h := 8;
  for mapY := 0 to LevelMapHeight - 1 do
    for mapX := 0 to LevelMapWidth - 1 do
    begin
      destRect.x := MapToPix(MapWidth + 1) + 3 + mapX * 9;
      destRect.y := MapToPix(10) + 3 + 81 - mapY * 9;
      isCurrent := (mapX = Level.mapX) and (mapY = Level.MapY);
      // drawing all passed levels and the current level
      if (fMapLevelStatus[Level.mapZ, mapY, mapX] = lsDone) or isCurrent then
        SDL_FillRectAdd(Background, @destRect, SDL_MapRGB(
          Background^.format, CMapR[isCurrent], CMapG[isCurrent], CMapB[isCurrent]));
  end;
end;

function TGame.GetMap(aMapX, aMapY: TMapCoord): TMapCell;
begin
  Result := fMap[aMapY, aMapX];
end;

function TGame.GetPixMap(X, Y: TPixCoord): TMapCell;
begin
  Result := fMap[PixToMap(Y), PixToMap(X)];
end;

procedure TGame.SetMap(aMapX, aMapY: TMapCoord; aLayer: TMapLayer; aValue: TTileType);
begin
  fMap[aMapY, aMapX].Z[aLayer] := aValue;
  DrawTile(aMapX, aMapY, True);
end;

function TGame.TileCount(aTiles: TTiles; aCountWeight: Boolean = False): Word;
var
  MapX, MapY: Byte;
  Layer: TMapLayer;
begin
  Result := 0;
  for Layer := mlBack to mlItem do
    for MapY := 0 to MapHeight - 1 do
      for MapX := 0 to MapWidth - 1 do
        with Map[MapX, MapY] do
          if Z[Layer] in aTiles then
            Inc(Result, ifop(aCountWeight, Weight, 1));
end;

procedure TGame.ReplaceTiles(aSrcTile, aDestTile: TTileType; aLayer: TMapLayer;
  aExclude: ShortInt);
var
  MapX, MapY: Byte;
  Index: ShortInt;
begin
  Index := aExclude;
  for MapY := 0 to MapHeight - 1 do
    for MapX := 0 to MapWidth - 1 do
      with fMap[MapY, MapX] do
        if Z[aLayer] = aSrcTile then
        begin
          if (aExclude = -1) or (Index <> 0) then
          begin
            Z[aLayer] := aDestTile;
            DrawTile(MapX, MapY, False);
          end;
          Dec(Index);
        end;
  DrawTile(0, 0, True);
end;

function TGame.SpriteIn(aGridX1, aGridX2, aGridY1, aGridY2: TGridCoord;
  aExclude: TActiveSprite = nil): TActiveSprite;
var
  Index: Word;
begin
  for Index := 0 to SpriteEngine.Sprites.Count - 1 do
  begin
    Result := TActiveSprite(SpriteEngine.Sprites[Index]);
    { TODO : optimize intersection check }
    if ((aExclude = nil) or (Result <> aExclude) {and (Result <> Hero)}) and
      (Result.State <> ssRecovering) and not (Result.SpriteType in Ammunition) and
      RectsIntersect(
        PixToGrid(Result.X), PixToGrid(Result.X + GridCellSize),
        PixToGrid(Result.Y), PixToGrid(Result.Y + GridCellSize),
        aGridX1, aGridX2, aGridY1, aGridY2) then Exit;
  end;
  Result := nil;
end;

procedure TGame.CreateSprites;
var
  X, Y: TPixCoord;
  MapX, MapY: TMapCoord;
  dX, dY: TDelta;
  SpriteType: TSpriteType;
  LevelHint: Boolean;
begin
  Log.LogStatus('Creating sprites', GameStateName);
  for MapY := 0 to MapHeight - 1 do
    for MapX := 0 to MapWidth - 1 do
      with Map[MapX, MapY] do
        // sprite can stand on an empty item only
        if (Item in SpriteHolders) and (Sprite and $8 = 0) then
        begin
          // screen coordinates
          X := MapToPix(MapX);
          Y := MapToPix(MapY);
          // initial turn
          dX := ItoDX[Sprite and $3];
          dY := ItoDY[Sprite and $3];
          if Sprite and $F0 = $F0 then
            Hero.Reset(X, Y, dX, dY)
          else
          begin
            // sprite type
            SpriteType := TSpriteType(Sprite shr 4);
            // flag whether sprite is a level hint
            LevelHint := Boolean(Sprite and $4);
            if SpriteType = sBox then
              TBox.Create(sBox, $FF, X, Y, 0, 0, LevelHint)
            else
            // enemy sprite can be created only if level is not done yet
            if LevelStatus <> lsDone then
              case SpriteType of
                sDragon:
                  TDragon.Create(sDragon, 0, X, Y, 0, 0, LevelHint);
                sSkull:
                  TSkull.Create(sSkull, $FF, X, Y, dX, dY, LevelHint);
                sRoller:
                  TRoller.Create(sRoller, 3, X, Y, dX, dY, LevelHint);
                sFireBall:
                  TFireball.Create(sFireball, 0, X, Y, dX, dY, LevelHint);
                sSleepy:
                  TSleepy.Create(X, Y, dX, dY, LevelHint);
                sMedusa:
                  TMedusa.Create(sMedusa, $FF, X, Y, 0, 0);
                sStony:
                  TStony.Create(sStony, 3, X, Y, dX, dY, LevelHint);
                sCuckold:
                  TCuckold.Create(sCuckold, $FF, X, Y, dX, dY, LevelHint);
              end
            else if LevelHint then
              case SpriteType of
                sSleepy:
                  if dX = 0 then
                    TSleepy.DoHint;
                sFireball:
                  TFireball.DoHint;
              end;
          end;
        end;
end;

procedure TGame.DeleteSprites(aDeleteAll: Boolean);
var
  Index: Word;
begin
  // delete all sprites BUT
  // - do not delete Hero
  // - delete boxes only if state is gsNextLevel or gsKillHero
  for Index := SpriteEngine.Sprites.Count - 1 downto 0 do
    with TActiveSprite(SpriteEngine.Sprites[Index]) do
      if (SpriteType <> sHero) and (aDeleteAll or (SpriteType <> sBox)) then
        SpriteEngine.RemoveSprite(SpriteEngine.Sprites[Index]);
end;

procedure TGame.OpenChest;
begin
  Log.LogStatus('Opening chest', GameStateName);
  GameState := gsCatchKey;
  DrawBackground;
end;

procedure TGame.OpenDoors(aState: TGameState);
begin
  Log.LogStatus('Opening doors', GameStateName);
  ReplaceTiles(tDoorTop, tDoorOpenTop);
  ReplaceTiles(tDoorLeft, tDoorOpenLeft);
  ReplaceTiles(tDoorRight, tDoorOpenRight);
  ReplaceTiles(tDoorBottom, tDoorOpenBottom);
  ReplaceTiles(tDoorDown, tDoorOpenDown);
  ReplaceTiles(tDoorUp, tDoorOpenUp);
  // delete all enemy sprites in the level
  DeleteSprites(False);
  // mark level as done
  LevelStatus := lsDone;
  GameState := aState;
end;

procedure TGame.SetNextLevel(dx, dy, dz: TDelta);
begin
  // calculate the next level number
  Inc(Level.mapX, dx);
  Inc(Level.mapY, -dy);
  Inc(Level.mapZ, dz);
  with Level do
    Log.LogStatus(Format('Next level map is %d:%d:%d', [mapX, mapY, mapZ]), GameStateName);
end;

function TGame.EndMovePhase(aMask: Byte): Boolean;
begin
  Result := MovePhase and aMask = 0;
end;

procedure TGame.NextLevel;
var
  Count: Word;
  MapX, MapY: TMapCoord;
begin
  Log.LogStatus(Format('Proceeding to level %d:%d:%d',
    [Level.mapX, Level.mapY, Level.mapZ]), GameStateName);
  // delete all remaining sprites in the level
  GameState := gsNextLevel;
  DeleteSprites(True);
  // form the new level
  LoadMap(fMap, SizeOf(fMap));
  Count := TileCount([tDoorDown]);
  if Count > 1 then
    ReplaceTiles(tDoorDown, tEmpty, mlItem, Random(Count));
  LevelTimer := TileCount([tMasterKey], True);
  GameState := gsStartLevel;
  DrawBackground;
  if TileCount(ChestItems) = 0 then
    LevelStatus := lsDone;
  if LevelStatus = lsDone then
    OpenDoors(gsStartLevel);
  // adjust the hero position
  with Hero do
  begin
    Xorig := X;
    Yorig := Y;
    dXorig := dX;
    dYorig := dY;

    Reset(
      ifop(dX <> 0, MapToPix(MapWidth - 1 - PixToMap(X)), X),
      ifop(dY <> 0, MapToPix(MapHeight - 1 - PixToMap(Y)), Y), dX, dY);

    MapX := PixToMap(X);
    MapY := PixToMap(Y);
    with Map[MapX, MapY] do
    begin
      // if Hero appears on the ground
      // - drawing an open door
      if Back in [tNothing, tEmpty] then
      begin
        SetMap(MapX, MapY, mlItem, OffsetTileType(tDoorOpenTop, -dX, -dY));
        // make one more grid cell step
        Inc(X, GridToPix(dX));
        Inc(Y, GridToPix(dY));
      end
      // if Hero appears on the water
      // - recreating a swimming raft for Hero
      else if Back = tWater then
      begin
        if Item <> tEmpty then
        begin
          // make one more map cell step
          Inc(X, MapToPix(dX));
          Inc(Y, MapToPix(dY));
        end;
        Sail := TShip.Create(Hero, X, Y);
      end;
    end;
  end;
  CreateSprites;
end;

procedure TGame.KillHero;
begin
  Log.LogStatus('Killing Hero', GameStateName);
  PlaySound(sndKill);
  Hero.Reset(Xorig, Yorig, dXorig, dYorig);
  //SetNextLevel(0, 0, 0);
  NextLevel;
end;

procedure TGame.PlaySound(Sound: TSoundType);
begin
  if FSounds[Sound] <> nil then
    Mix_PlayChannel(-1, FSounds[Sound], 0);
end;

procedure TGame.CheckHiddenItems(aUnhide: Boolean = False);
var
  MapX, MapY: Byte;
begin
  for MapY := 0 to MapHeight - 1 do
    for MapX := 0 to MapWidth - 1 do
      with fMap[MapY, MapX] do
        if Item in HiddenItems then
        begin
          if Weight > 0 then
            Dec(Weight, ifop(aUnhide, Weight, 1));
          if Weight > 0 then Continue;
          PlaySound(sndUnhide);
          DrawTile(MapX, MapY, True);
        end;
end;

procedure TGame.WriteText(const Text: string; aX, aY: TPixCoord);
var
  Index: Word;
  srcRect, destRect: TSDL_Rect;
begin
  srcRect.y := 0;
  srcRect.w := 14;
  srcRect.h := 16;
  destRect.y := aY;
  for Index := 1 to Length(Text) do
  begin
    srcRect.x := (Ord(Text[Index]) - Ord(' ')) * 16 + 1;
    destRect.x := aX - Length(Text) * 14 div 2 + (Index - 1) * 14;
    SDL_BlitSurface(FontImage, @srcRect, Background, @destRect);
  end;
end;

procedure TGame.LoadMap(var Map; Length: Word);
var
  MapFile: File;

function GetOffset: Int64;
var
  S: string;
  Code: Longint;
begin
  Result := FileSize(MapFile);
  Str(Level.mapX * 100 + Level.mapY + 10101, S);
  S[1] := Chr(Ord(FStartLevel[1]) + Level.mapZ);
  S := FIniFile.ReadString(SGenSection, S, '');
  if S = '' then Exit;

  Val(S, Result, Code);
  if Code = 0 then Exit;
  if ParseLevel(S) then
    Result := GetOffset;
end;

begin
  AssignFile(MapFile, SMapDir + SMapFile);
  try
    Reset(MapFile, 1);
  except
    FatalError(SMapOpenError);
  end;
  try
    try
      if @Map = @FInfo then
        Log.LogStatus('Loading info map', GameStateName)
      else
      begin
        Log.LogStatus(Format('Loading level map %d:%d:%d',
          [Level.mapX, Level.mapY, Level.mapZ]), GameStateName);
        Seek(MapFile, SizeOf(FInfo) + GetOffset);
      end;
      BlockRead(MapFile, Map, Length);
    finally
      CloseFile(MapFile);
    end;
  except
    FatalError(SMapReadError);
  end;
end;

procedure TGame.LoadGame;
var
  GameFile: File of TGameRecord;
  Rec: TGameRecord;
begin
  Log.LogStatus('Loading game...', GameStateName);
  AssignFile(GameFile, SGameStateFile);
  Reset(GameFile);
  try
    try
      Read(GameFile, Rec);
      Log.LogStatus('Game loaded successfully', GameStateName);
      fMapLevelStatus := Rec.MapLevelStatus;
      fLevel := Rec.Level;
      Xorig := Rec.x;
      Yorig := Rec.y;
      dXorig := Rec.dx;
      dYorig := Rec.dy;
      GameState := gsKillHero;
    finally
      CloseFile(GameFile);
    end;
  except
    on E: Exception do
      Log.LogError(E.Message, GameStateName);
  end;
end;

procedure TGame.SaveGame;
var
  GameFile: File of TGameRecord;
  Rec: TGameRecord;
begin
  Log.LogStatus('Saving game...', GameStateName);
  AssignFile(GameFile, SGameStateFile);
  Rewrite(GameFile);
  try
    try
      Rec.MapLevelStatus := fMapLevelStatus;
      Rec.Level := Level;
      Rec.X := Xorig;
      Rec.Y := Yorig;
      Rec.dX := dXorig;
      Rec.dY := dYorig;
      Write(GameFile, Rec);
      Log.LogStatus('Game saved successfully', GameStateName);
    finally
      CloseFile(GameFile);
    end;
  except
    on E: Exception do
      Log.LogError(E.Message, GameStateName);
  end;
end;

function TGame.GetLevelStatus: TLevelStatus;
begin
  Result := fMapLevelStatus[Level.mapZ, Level.mapY, Level.mapX];
end;

procedure TGame.SetGameState(const aValue: TGameState);
begin
  if fGameState = aValue then Exit;
  Log.LogStatus('Game state set to ' + GameStateNames[aValue], GameStateName);
  fGameState := aValue;
end;

procedure TGame.SetLevelStatus(aValue: TLevelStatus);
begin
  fMapLevelStatus[Level.mapZ, Level.mapY, Level.mapX] := aValue;
  Log.LogStatus('LevelStatus set to ' + LevelStatusNames[aValue], GameStateName);
end;

function TGame.ParseLevel(const S: string): Boolean;
var
  Code: Longint;
  mapX, mapY: TMapCoord;
begin
  Val(S[2] + S[3], mapX, Code);
  if Code <> 0 then Exit(False);
  Val(S[4] + S[5], mapY, Code);
  if Code <> 0 then Exit(False);
  fLevel.mapX := mapX - 1;
  fLevel.mapY := mapY - 1;
  fLevel.mapZ := Ord(s[1]) - Ord(FStartLevel[1]);
  Result := True;
end;

procedure TGame.SetLevelTimer(Value: TLevelTimer);
begin
  FLevelTimer := Value;
  DrawInfo;
end;

end.

