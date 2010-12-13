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
    procedure LoadMap(var Map; Length: Word);
    function LoadImage(const Name: string): PSDL_Surface;
    procedure LoadGame;
    procedure SaveGame;
    procedure CreateSprites;
    procedure DeleteSprites(All: Boolean);
    procedure DrawMap;
    procedure OpenChest;
    procedure OpenDoors(aState: TGameState);
    procedure KillHero;
    procedure NextLevel;
    function GetLevelStatus: TLevelStatus;
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

    function TileCount(Tiles: TTiles; CountWeight: Boolean = False): Word;
    function SpriteIn(aGridX1, aGridX2, aGridY1, aGridY2: TGridCoord;
      Exclude: TActiveSprite = nil): TActiveSprite;
    procedure ReplaceTiles(STile, DTile: TTileType; Layer: TMapLayer = mlItem;
      Exclude: ShortInt = -1);
    procedure CheckHidden(Unhide: Boolean = False);

    property Map[MapX, MapY: TMapCoord]: TMapCell read GetMap;
    procedure SetMap(aMapX, aMapY: TMapCoord; aLayer: TMapLayer; aValue: TTileType);
    procedure SetNextLevel(dx, dy, dz: TDelta);
    property Level: T3DMapCoord read fLevel;
    property LevelStatus: TLevelStatus read GetLevelStatus write SetLevelStatus;
    property LevelTimer: TLevelTimer read FLevelTimer write SetLevelTimer;
    property GameState: TGameState read fGameState write fGameState;
    property MovePhase: Byte read fMovePhase;
    property SpriteEngine: TSpriteEngine read fSpriteEngine;
    property SpriteImage: TSpriteImages read fSpriteImage;
  end;

var
  Game: TGame;

implementation

uses
  SDLUtils, SDL_Image, Logger;

procedure FatalError(const S: string);
begin
//  MessageBox(0, PChar(S), '', MB_ICONERROR + MB_OK);
  Log.LogError(S, SInit);
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
const
  cbpp: array [0..4] of Byte = (15, 16, 24, 32, 8);
var
  i, bpp: Byte;
  st: TSpriteType;
  snd: TSoundType;
  s: string;
begin
  // initializing SDL engine
  if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO) = -1 then
    FatalError(SSDLInitError);

  // setting window properties
  SDL_WM_SetCaption(PChar(SAppTitle), nil);
  SDL_WM_SetIcon(IMG_Load(PChar(SGFXDir + SIconFile)), 0);

  // initializing video mode
  for i := Low(cbpp) to High(cbpp) do
  begin
    bpp := SDL_VideoModeOK(ScreenWidth, ScreenHeight, cbpp[i], 0);
    if bpp <> 0 then Break;
  end;
  Str(bpp, S);
  log.LogStatus(SVideoMode + S + ' bpp', SInit);

  Screen := SDL_SetVideoMode(ScreenWidth, ScreenHeight, bpp,
    SDL_SWSURFACE or SDL_HWPALETTE {or SDL_DOUBLEBUF} {or SDL_FULLSCREEN});
  if Screen = nil then
    FatalError(SVideoModeError);

  // initializing background surface
  Background := SDL_CreateRGBSurface(
    SDL_SWSURFACE or SDL_HWPALETTE, ScreenWidth, ScreenHeight, bpp, 0, 0, 0, 0);
  if Background = nil then
    FatalError(SBackgroundError + ': ' + SDL_GetError);
  Background := SDL_DisplayFormat(Background);

  // loading images
  TileImage := LoadImage(STilesFile);
  FontImage := LoadImage(SFontFile);
  for st := Low(TSpriteType) to High(TSpriteType) do
    fSpriteImage[st] := LoadImage(SSpriteFile[st]);
  SDL_ShowCursor(SDL_DISABLE);

  // initializing sprite engine
  fSpriteEngine := TSpriteEngine.Create(Screen);
  fSpriteEngine.BackgroundSurface := Background;

  // initializing audio
  if Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT,
    MIX_DEFAULT_CHANNELS, 2048) < 0 then
      FatalError(SAudioInitError);
//Mix_HookMusicFinished(@RestartMusic);
  for snd := Low(TSoundType) to High(TSoundType)do
  begin
    FSounds[snd] := Mix_LoadWav(PChar(SSndDir + SSoundFile[snd]));
    if FSounds[snd] = nil then
      log.LogWarning(SSoundLoadError + SSoundFile[snd], SInit);
  end;
  FIniFile := TIniFile.Create(SMapDir + SIniFile);
  FStartLevel := FIniFile.ReadString(SGenSection, SStart, '');
//  FStartLevel := 'L0806';
  // setting initial level
  if not ParseLevel(FStartLevel) then
    FatalError(SIniReadError);
  // creating Hero
  Hero := THero.Create(sHero, $7, MapToPix(7), MapToPix(3));
end;

type
  TKeyInfo = record
    // SDL key code
    SDLK: Word;
    // if key can be placed in stack only once while pressed
    Once: Boolean;
  end;
  TKeyRange = 0..10;
  TKeyState = record
    Index: TKeyRange;
    PrevIndex: TKeyRange;
    Down: Boolean;
  end;

const
  KeyInfo: array [TKeyRange] of TKeyInfo = (
    (SDLK: 0; Once: False),
    (SDLK: SDLK_SPACE; Once: True),
    (SDLK: SDLK_LEFT; Once: False),
    (SDLK: SDLK_RIGHT; Once: False),
    (SDLK: SDLK_UP; Once: False),
    (SDLK: SDLK_DOWN; Once: False),
    (SDLK: SDLK_F2; Once: True),
    (SDLK: SDLK_F3; Once: True),
    (SDLK: SDLK_F10; Once: True),
    (SDLK: SDLK_DELETE; Once: True),
    (SDLK: SDLK_ESCAPE; Once: True)
  );
  NormalDelay = 40;
  ShortDelay = 10;

procedure TGame.Play;
var
  Ticks, NextTicks: Cardinal;
  Event: TSDL_Event;
  Keys: PKeyStateArr;
  Key: TKeyState;
  //isDown: Boolean;
  //si, osi: Byte;

procedure CheckKeys;
var
  i: Byte;
begin
  for i := Low(KeyInfo) to High(KeyInfo) do
    // if a pressed key is found
    if Keys[KeyInfo[i].SDLK] = 1 then
    begin
      // key can be placed in stack if
      // - it can be pressed more than once (continuously)
      // - it can be pressed once and the key was not down just before
      if not KeyInfo[i].Once or not Key.Down then
        if (Key.PrevIndex <> i) or (MovePhase and $3 = 0) then
          Key.Index := i;
      Key.Down := True;
      Exit;
    end;
  // if no pressed keys are found isDown flag is reset to false
  Key.Down := False;
  Key.PrevIndex := 0;
end;

procedure MoveSprites;
begin
  if (LevelTimer > 0) and (MovePhase and $1F = 0) then
  begin
    LevelTimer := LevelTimer - 1;
    if LevelTimer = 0 then
      KillHero;
  end;
  SpriteEngine.Move;
  Inc(fMovePhase);
end;

procedure ProcessDebugKeys;
begin
  if (Keys[SDLK_LCTRL] + Keys[SDLK_LSHIFT] = 2) then
    case KeyInfo[Key.Index].SDLK of
      SDLK_DELETE: OpenDoors(gsPlayLevel);
    end
    else Exit;
  Key.Index := 0;
  Hero.Ghostly := True;
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
    if (Key.Index <> 0) and (GameState in PersistentStates) then
      with Hero do
      begin
        // hero activities are processed when hero is on the grid edge
        if MovePhase and $3 = 0 then
        begin
          ProcessDebugKeys;
          case KeyInfo[Key.Index].SDLK of
            SDLK_SPACE:
              Attack(dx, dy);
            SDLK_LEFT:
              Pushed(-1, 0);
            SDLK_RIGHT:
              Pushed(1, 0);
            SDLK_UP:
              Pushed(0, -1);
            SDLK_DOWN:
              Pushed(0, 1);
            SDLK_ESCAPE:
              GameState := gsQuit;
            SDLK_F2:
              SaveGame;
            SDLK_F3:
              LoadGame;
            SDLK_F10:
              GameState := gsKillHero;
          end;
          Key.PrevIndex := Key.Index;
          Key.Index := 0;
        end;
      end;

    // level starts moving if a key is pressed
    if Key.Down and (GameState = gsStartLevel) then
    begin
      if (LevelStatus = lsDone) or (TileCount([tHeart]) = 0) then
        GameState := gsCatchKey
      else
        GameState := gsPlayLevel;
      DrawBackground;
      fMovePhase := 0;
    end;

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
    if Keys[SDLK_LALT] = 1 then
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
  FIniFile.Free;
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
  Result := IMG_Load(PChar(SGFXDir + Name));
  if Result = nil then
    FatalError(SImageError + Name);
  // converting image in the current color mode
  Result := SDL_DisplayFormat(Result);
  // setting image transparency
  SDL_SetColorKey(Result, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
    SDL_MapRGB(Result.Format, $FF, 0, $FF));
end;

procedure TGame.DrawBackground;
var
  MapX, MapY: TMapCoord;
begin
  for MapY := 0 to MapHeight - 1 do
    for MapX := 0 to MapWidth + MapInfoWidth - 1 do
      with FMap[MapY, MapX] do
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
  S: String;
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
  begin
    Str(Hero.Bullets, S);
    WriteText(S, MapToPix(MapWidth + 1) + GridCellSize, MaptoPix(4));
  end;

  if LevelTimer > 0 then
  begin
    WriteText('TIME', MapToPix(MapWidth + 2) + GridCellSize, MapToPix(6));
    Str(LevelTimer, S);
    WriteText(S, MapToPix(MapWidth + 2) + GridCellSize, MapToPix(7));
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
          Background.format, CMapR[isCurrent], CMapG[isCurrent], CMapB[isCurrent]));
  end;
end;

function TGame.GetMap(aMapX, aMapY: TMapCoord): TMapCell;
begin
  Result := FMap[aMapY, aMapX];
end;

procedure TGame.SetMap(aMapX, aMapY: TMapCoord; aLayer: TMapLayer; aValue: TTileType);
begin
  FMap[aMapY, aMapX].Z[aLayer] := aValue;
  DrawTile(aMapX, aMapY, True);
end;

function TGame.TileCount(Tiles: TTiles; CountWeight: Boolean = False): Word;
var
  mx, my: Byte;
  ml: TMapLayer;
begin
  Result := 0;
  for ml := mlBack to mlItem do
    for my := 0 to MapHeight - 1 do
      for mx := 0 to MapWidth - 1 do
        with Map[mx, my] do
          if Z[ml] in Tiles then
            Inc(Result, ifop(CountWeight, Weight, 1));
end;

procedure TGame.ReplaceTiles;
var
  mx, my: Byte;
  i: ShortInt;
begin
  i := Exclude;
  for my := 0 to MapHeight - 1 do
    for mx := 0 to MapWidth - 1 do
      with FMap[my, mx] do
        if z[Layer] = STile then
        begin
          if (Exclude = -1) or (i <> 0) then
          begin
            z[Layer] := DTile;
            DrawTile(mx, my, False);
          end;
          Dec(i);
        end;
  DrawTile(0, 0, True);
end;

function TGame.SpriteIn(aGridX1, aGridX2, aGridY1, aGridY2: TGridCoord;
  Exclude: TActiveSprite = nil): TActiveSprite;
var
  Index: Word;
begin
  for Index := 0 to SpriteEngine.Sprites.Count - 1 do
  begin
    Result := TActiveSprite(SpriteEngine.Sprites[Index]);
    { TODO : optimize intersection check }
    if ((Exclude = nil) or (Result <> Exclude) {and (Result <> Hero)}) and
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

procedure TGame.DeleteSprites;
var
  i: Word;
begin
  // delete all sprites BUT
  // - do not delete Hero
  // - delete boxes only if state is gsNextLevel or gsKillHero
  for i := SpriteEngine.Sprites.Count - 1 downto 0 do
    with SpriteEngine, TActiveSprite(Sprites[i]) do
      if (SpriteType <> sHero) and (All or (SpriteType <> sBox)) then
        RemoveSprite(Sprites[i]);
end;

procedure TGame.OpenChest;
begin
  GameState := gsCatchKey;
  DrawBackground;
end;

procedure TGame.OpenDoors(aState: TGameState);
begin
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
end;

procedure TGame.NextLevel;
var
  Count: Word;
  MapX, MapY: TMapCoord;
begin
  // delete all remaining sprites in the level
  GameState := gsNextLevel;
  DeleteSprites(True);
  // form the new level
  LoadMap(FMap, SizeOf(FMap));
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
        SetMap(MapX, MapY, mlItem, TTileType(Ord(tDoorOpenTop) + DeltaToIndex(-dX, -dY)));
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
  PlaySound(sndKill);
  Hero.Reset(Xorig, Yorig, dXorig, dYorig);
  SetNextLevel(0, 0, 0);
  NextLevel;
end;

procedure TGame.PlaySound;
begin
  if FSounds[Sound] <> nil then
    Mix_PlayChannel(-1, FSounds[Sound], 0);
end;

procedure TGame.CheckHidden;
var
  mx, my: Byte;
begin
  for my := 0 to MapHeight - 1 do
    for mx := 0 to MapWidth - 1 do
      with FMap[my, mx] do
        if Item in HiddenItems then
        begin
          if Weight > 0 then
            Dec(Weight, ifop(Unhide, Weight, 1));
          if Weight = 0 then
          begin
            PlaySound(sndUnhide);
            DrawTile(mx, my, True);
          end;
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
  f: File;

function GetOffset: Int64;
var
  s: string;
  c: Longint;
begin
  Result := FileSize(f);
  Str(Level.mapX * 100 + Level.mapY + 10101, s);
  s[1] := Chr(Ord(FStartLevel[1]) + Level.mapZ);
  s := FIniFile.ReadString(SGenSection, s, '');
  if s = '' then Exit;

  Val(s, Result, c);
  if c = 0 then Exit;
  if ParseLevel(s) then
    Result := GetOffset;
end;

begin
  AssignFile(f, SMapDir + SMapFile);
  try
    Reset(f, 1);
  except
    FatalError(SMapOpenError);
  end;
  try
    try
      if @Map <> @FInfo then
        Seek(f, SizeOf(FInfo) + GetOffset);
      BlockRead(f, Map, Length);
    except
      FatalError(SMapReadError);
    end;
  finally
    CloseFile(f);
  end;
end;

procedure TGame.LoadGame;
var
  GameFile: File of TGameRecord;
  Rec: TGameRecord;
begin
  AssignFile(GameFile, SGameStateFile);
  Reset(GameFile);
  try
    Read(GameFile, Rec);
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
end;

procedure TGame.SaveGame;
var
  GameFile: File of TGameRecord;
  Rec: TGameRecord;
begin
  AssignFile(GameFile, SGameStateFile);
  Rewrite(GameFile);
  try
    Rec.MapLevelStatus := fMapLevelStatus;
    Rec.Level := Level;
    Rec.X := Xorig;
    Rec.Y := Yorig;
    Rec.dX := dXorig;
    Rec.dY := dYorig;
    Write(GameFile, Rec);
  finally
    CloseFile(GameFile);
  end;
end;

function TGame.GetLevelStatus: TLevelStatus;
begin
  Result := fMapLevelStatus[Level.mapZ, Level.mapY, Level.mapX];
end;

procedure TGame.SetLevelStatus(aValue: TLevelStatus);
begin
  fMapLevelStatus[Level.mapZ, Level.mapY, Level.mapX] := aValue;
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

